; patches loaded into a remote common lisp after starting swank

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT

(flet ((inspect-value (value)
          ; replaces swank::inspect-in-emacs
          (declare (ftype (function (t) t) swank::mclide-inspect))
          (swank::mclide-inspect (list :value value))))

#+allegro
(push `(inspect::*inspector-hook* . ,#'inspect-value) 
          swank:*default-worker-thread-bindings*)

#+clozure        
(push `(inspector::*default-inspector-ui-creation-function* .  ,#'inspect-value)
      swank::*default-worker-thread-bindings*)

) ; end flet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ED

#+allegro
(push '(scm::*ed-hook* . swank::ed-in-emacs)  
      swank::*default-worker-thread-bindings*)

#+clozure
(push '(ccl::*resident-editor-hook* . swank::ed-in-emacs)  
      swank::*default-worker-thread-bindings*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :swank-backend)

(definterface all-logical-hosts ()
   "Return a list of the names of all logical hosts"
   nil)

#+clozure
(defimplementation all-logical-hosts ()
  (mapcar #'car ccl::%logical-host-translations%))  

#+sbcl ; from Nikodemus Siivola
(defimplementation all-logical-hosts ()
  (loop for host being each hash-key in sb-impl::*logical-hosts*
        collect host))

#+lispworks ;; swank-lispworks.lisp
(defimplementation all-logical-hosts ()
  (loop for host being each hash-key in system::*logical-pathname-translations*
        collect host))

#+allegro
(defimplementation all-logical-hosts ()
  (loop for host being each hash-key in excl::*logical-pathname-translations*
        collect host))

#+mcl
(defimplementation all-logical-hosts ()
  (mapcar #'car ccl::%logical-host-translations%))

(in-package :swank)

(defslimefun logical-hosts ()
  "A list of the names of all logical hosts"
  (all-logical-hosts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT THE FRAME FUNCTION

;; like inspect-frame-var
;; may replace disassemble-frame.
;; note that inspect may differ if presentations are loaded/active. e.g. there may not be a disassemble.

;; See also swank:frame-call function added to Slime in January 2010.

(in-package :swank-backend)

(definterface frame-function (frame-number)
   "Return the function of the frame associated with FRAME-NUMBER."
   (declare (ignore frame-number))
   nil)

(in-package :swank-backend)

#+(and openmcl ccl-1.4) ; swank-ccl.lisp
(defimplementation frame-function (frame-number)
    (with-frame (p context) frame-number
      (multiple-value-bind (lfun pc) (ccl:frame-function p context)
        (declare (ignore pc))
        lfun)))

#+(and openmcl (not ccl-1.4)) ; swank-openmcl.lisp
(defimplementation frame-function (frame-number)
    (with-frame (p context lfun pc) frame-number
      (declare (ignore p context pc))
      lfun))

(in-package :swank)

(defslimefun inspect-frame-function (frame)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-function frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINITION TYPES

#|
Posted on slime-devel December 2009.

The find-definitions-for-emacs function returns a list of definition  
specifiers (dspecs) associated with their source locations. The first  
item in a dspec denotes its type.

Swank clients like SLIME and MCLIDE can benefit from reliably  
determining the canonical type of a definition. However, the lisp  
implementations differ in the number of definition types and how they  
are named, and may also let users define custom definition types.

This ambiguity can be resolved by swank exposing information about  
the semantics of the implementation specific definition types and the  
relationship between them, such as:
|#

(in-package :swank-backend)

(definterface definition-types ()
"Association list of definition type symbol, supertype(s), and  
optional attributes inherited by subtypes, where an attribute can be  
one of (:variable :class :method :function :macro) denoting the  
semantics of the definition type."
    '((variable nil :variable)
      (constant (variable))
      (function nil :function)
      (macro (function) :macro)
      (generic (function))
      (method nil :method)
      (class nil :class)
      (condition (class))))

#+clozure
(defimplementation definition-types ()
   (loop
     with semantics = '((variable :variable)(class :class)
                        (method :method)(function :function) (macro :macro))
     for (type . def) in ccl::*definition-types*
     collect
      (list*
       type
       (loop
         for super in (ccl::class-direct-superclasses (class-of def))
         for type = (car (rassoc (ccl::class-prototype super) ccl::*definition-types*))
         when (and type (not (eq 't type)))
         collect type)
       (cdr (assoc type semantics)))))

#+sbcl
(defimplementation definition-types ()
   (loop
     with semantics = '((defvar :variable)(defclass :class) 
                        (defmethod :method) (defun :function)(defmacro :macro))
     with super = '((defconstant defvar)(define-condition defclass) (defgeneric defun)
                    (define-compiler-macro defmacro)(defmacro defun))
     for type in (cdr *definition-types*) by #'cddr
     collect (list* type (cdr (assoc type super)) (cdr (assoc type semantics)))))

#+lispworks
(defimplementation definition-types ()
  (loop
    with semantics = '((defvar :variable)(defclass :class) 
                       (method :method)
                       (defun :function)(defmacro :macro))
    with super = '((defconstant defvar) (define-condition defclass)
                   (defgeneric defun) (compiler-macro defmacro) 
                   (defmacro defun))
    for type in dspec:*dspec-classes*
    collect (list* type (cdr (assoc type super)) (cdr (assoc type semantics)))))

#+allegro
(defimplementation definition-types ()
   (loop
     with semantics = '((:type nil) (deftype nil) (defstruct nil) (defclass :class)
                        (:setf-method :method)(defmethod :method) (defun :function)(defmacro :macro)
                        (:compiler-macro :macro)(:special-declaration :variable) (:operator nil)
                        (generic-function :function))
     for type in excl::*definition-types*
     collect (list (aref type 1) NIL (or (second (assoc (aref type 2) semantics))
                                                       (second (assoc (aref type 1) semantics))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allegro swank backend shouldn't send a background message when it encounters an inconsistent frame.
;; Maybe same issue as mentioned in http://lists.common-lisp.net/pipermail/slime-devel/2008-October/015629.html
;; Where is http://www.franz.com/support/documentation/current/doc/debugger-api.htm#frame-reference-p ???

(in-package :swank-backend)

#+allegro
(defvar *debug-frame-restartable-p* nil
   "Set to true to debug frame-restartable-p")

#+allegro
(defimplementation frame-restartable-p (frame)
  (handler-case 
    (debugger:frame-retryable-p frame)
    (serious-condition (c)
      (when *debug-frame-restartable-p*
       (funcall (read-from-string "swank::background-message")
                    "~a ~a" frame (princ-to-string c)))
      nil)))




    
    
  


  