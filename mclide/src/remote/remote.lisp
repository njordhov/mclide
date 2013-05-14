;;; -*- Mode: LISP; Package: (remote); Syntax: Common-lisp; Base: 10.; -*-

;; REMOTE.LISP
;;
;; Layer between the IDE and the swank-client.
;;
;; Copyright (c) 2007-2011 Terje Norderhaug <terje@in-progress.com>
;;
;; You are hereby granted the rights to distribute and use this
;; software as governed by the terms of the lisp lesser gnu public
;; license (http://opensource.franz.com/preamble.html), known as the
;; llgpl. Other free licenses available upon request.
;;
;; This software is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.

(defpackage remote
   (:import-from :swank-client
    #:destructure-case
    #:client-connection
    #:dispatch-available-input))

(in-package :remote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE

(defparameter *remote-lisp* nil
  "Bind to the remote-repl to use in a dynamic context")

(defvar *auto-remote-lisp-function* NIL
  "Set to an optional function returning the repl if currently in a remote lisp context")

(defun check-remote-lisp (&optional (lisp *remote-lisp*))
  (check-type lisp (or null remote-repl swank-client::swank-client)))

(defun call-using-remote (thunk &optional (lisp *remote-lisp*))
  (check-type lisp (or null remote-application remote-repl swank-client::swank-client)) ; ## too liberal but all are used
  (let ((*remote-lisp* lisp))
    (funcall thunk)))

(defmacro using-remote-lisp (repl &body body)
  `(call-using-remote (lambda () ,@body) ,repl))

(defun remote-lisp ()
  "The remote lisp to use in the context"
  *remote-lisp*)

(defparameter *remote-lisp-mode* :auto
  "whether forms should be evaluated on the remote LISP (T), MCL (NIL), or automatically determined based on the top listener (:auto)")

(defun remote-lisp-mode ()
   "True if a remote lisp should be used in the context"
  (case *remote-lisp-mode*
    (:auto
     (or *remote-lisp*
         (if *auto-remote-lisp-function*
           (funcall *auto-remote-lisp-function*))))
    ((nil) nil)
    (otherwise t)))

(defmacro using-normal-lisp (&body body)
  "Evaluate body without remote lisp mode, typically to bypass remote lisp contextual overrides"
  ; also bind *remote-lisp* ??
  `(let ((*remote-lisp-mode* nil))
     (assert (not (remote-lisp-mode)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-string-list-contents (string stream)
  "Write a string except eventual start and end ()"
  ;; extracted from ccl::arglist-to-stream
  (check-type string string)
  (let ((start 0)
        (end (length string)))
    (declare (fixnum start end))
    (when (and (> end 0)
               (eql #\( (char string 0))
               (eql #\) (char string (1- end)))
               (not (eql end 2)))
      (setq start 1 end (1- end)))
    (stream-write-string stream string start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES FOR REMOTE LISP FORMS

#+ignore
(defun swank (symbol)
  "Encodes a remote symbol in the swank package"
  (let ((sym (make-symbol (string symbol))))
    (setf (get sym 'symbol-p) t
          (get sym 'package) "swank"
          (get sym 'lisp) t)
    sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE EVAL

(define-condition no-connection (error) ())  ;; not used!!

(define-condition not-connected (error)
 ((error-message :initform "not connected" :initarg :message :reader error-message))
  (:report (lambda (c stream)
             (write-string (error-message c) stream))))

(define-condition failed-connecting (not-connected)())

(defparameter *remote-eval-timeout* (* 20 60))

(defun event-processor-p (&optional (p ccl::*current-process*))
   #-mcl (declare (ignore p))
   #+mcl (eq p ccl::*event-processor*))

#+ignore
(defun in-listener-p (&optional (p *current-process*))
   #+mcl
   (ccl::process-is-listener-p p)
   #+clozure
   (typep p 'gui::cocoa-listener-process))

(defvar *remote-eval-dispatch-swank*
   #+mcl #'ccl::process-is-listener-p
   #+clozure NIL ; (lambda (p)(typep p 'gui::cocoa-listener-process))
  "Bind to a true value or a process function returning true when the current process should dispatch swank events")

(defun dispatch-swank-p (&optional (p ccl::*current-process*))
   "True if process should dispatch swank events for the current remote repl"
   (or (typecase *remote-eval-dispatch-swank*
           (function (funcall *remote-eval-dispatch-swank* p))
           (otherwise *remote-eval-dispatch-swank*))
         #+ignore
         (in-listener-p)))

(defun remote-eval-1 (form &optional (callback nil callback-p))
  (labels ((remote-eval-simple (form &optional post-processing package)
             (let ((remote (remote-lisp)))
               (assert remote () "No remote lisp for evaluating:~&~A" form)
               (handler-case
                 (if callback-p
                   (flet ((post-callback (&rest values)
                            "Post-processing the result before completing the callback"
                            ;; ## need to fully support multiple values from swank!
                            (when callback
                              (if post-processing 
                                (multiple-value-call callback 
                                                     (apply post-processing values))
                                (apply callback values)))))                              
                     (swank-client::swank-remote-eval-async remote form #'post-callback package))
                   (multiple-value-call (or post-processing #'values)
                                        (swank-client::swank-remote-eval remote form package))) ; process swank input while waiting
                 #+mcl
                 (ccl::type-error ()
                  ; may happen if stream closes while reading (with "nil not being a macptr" error)
                  (error 'not-connected :message "Failed eval on remote lisp"))                  
                 #+mcl
                 (ccl::opentransport-peer-closed (c)
                  (error 'not-connected :message (ccl::opentransport-error-message c)) ))))
           (complex-form-p (form)
             "Test whether form is a property list with context for evaluation" 
             (and (listp form)(keywordp (car form))))
           (remote-eval-in-context (&rest rest
                                    &key form 
                                         post-processing
                                         ; Do not use swank-client::slime-current-thread as the default thread!
                                         (thread t) 
                                         (connection swank-client::slime-dispatching-connection) ;; ## not used?
                                         package
                                         recover
                                         remote)
             "Setup a remote eval based on the potentially nested context, using defaults"
             (declare (dynamic-extent rest))
             (if (complex-form-p form) 
               ; resolve nested complex form, giving outer key values precedence
               (multiple-value-call #'remote-eval-in-context
                                    (values :form (getf form :form))
                                    (apply #'values rest)
                                    (apply #'values form))                                   
               (let ((swank-client::slime-dispatching-connection connection)
                     (swank-client::slime-current-thread thread))
                (using-remote-lisp (or remote (remote-lisp))
                 (if recover
                   (restart-case
                     (remote-eval-simple form post-processing package)
                     (abort ()
                            ; swank failed to complete evaluation, returning :abort (# recover from errors/disconnect too?)
                            (funcall recover)))
                   (remote-eval-simple form post-processing package)))))))
    (if (complex-form-p form) 
      (apply #'remote-eval-in-context form)
      (remote-eval-simple form))))

(defgeneric remote-listener-process (repl)
   (:documentation "Specialize to provide the process of the listener associated with the repl"))

(defparameter *eval-wait-queue* NIL
  "Bound to a values-queue in the context of an eval wait.")

(defmethod call-in-eval-wait (repl fun &rest args)
   "Call function while the listener process is waiting for a remote eval to complete"
  ; function is not guaranteed to be called (like if the remote-eval completes)
  (let* ((process (remote-listener-process repl))
         (queue (or (symbol-value-in-process '*eval-wait-queue* process)
                    (warn "Not in the context of a wait queue"))))
    (when queue
      (swank-client::values-queue-enqueue queue :apply fun args))))

(defun remote-eval-wait (form &optional (timeout (when (event-processor-p) *remote-eval-timeout*)))
   ;; waits for other process to dispatch incoming events
   ;; ## move to the swank-client?
   (let* ((queue (or *eval-wait-queue* ; avoid nesting as it may cause lost events
                     (swank-client::make-values-queue)))
          (*eval-wait-queue* queue))
     (flet ((%remote-eval-result (&rest values)
                 (swank-client::values-queue-enqueue queue :result values))
             (%remote-eval-abort (&optional reason)
                 (swank-client::values-queue-enqueue queue :cancel reason)))
        (declare (dynamic-extent #'%remote-eval-result))
        (let ((swank-client::*abort-continuation* #'%remote-eval-abort))
          (loop
            initially (remote-eval-1 form #'%remote-eval-result)
            for event = (multiple-value-list (swank-client::values-queue-dequeue queue timeout))
            do (ecase (first event)
                    (:result
                     (assert (swank-client::values-queue-empty-p queue)() 
                             "The result should be the last event in a *eval-wait-queue*")
                     (return (apply #'values (second event))))
                    (:apply 
                     (apply #'apply (rest event)))
                    ((nil)
                     (warn "Remote evaluation timed out before completion.")
                     (return))
                    (:cancel
                     (let ((result (second event)))
                       #+mcl
                       (if result ;; ## change from warnings to conditions?
                         (warn  "Evaluation aborted by remote lisp (~A)" result)
                         (warn "Evaluation aborted by remote lisp.")))
                     (return nil))
                    (:not-connected
                     (error 'not-connected))))))))

(defun remote-eval (form &optional (callback nil callback-p))
  "Evaluates the form (possibly a property list) in the remote lisp, providing the result as remote object values"
   (cond
    (callback-p
     (remote-eval-1 form callback))
    ((dispatch-swank-p)
     (remote-eval-1 form))
    (T
     (remote-eval-wait form))))

(defun remote-send (form &optional ignore-response)
  (remote-eval form (when ignore-response (lambda (result)(declare (ignore result)) nil))))

(defmacro remote-eval-bind (vars form &body body)
  "Evaluate a form or string in the current remote lisp, bind the result, then evaluate the body in the current process"
  `(multiple-value-bind ,(if (listp vars) vars `(,vars))
                        (remote-eval ,form)
     ,@body))

(defmacro remote-eval-enqueue-bind! (vars form &body body)
  "Without waiting for completion to continue, evaluate a form or string in the current remote lisp, bind the result, then evaluate the body efficiently (likely in the listener process, so keep it brief and safe)"
  `(let ((form ,form)
         (callback 
          (lambda ,(if (listp vars) vars `(,vars)) ;; capture *remote-lisp* ?
            ,@body)))
     (remote-eval form callback)))

(defparameter $remote-eval-mode$ nil
  "Facilitates experimentation with various swank request/response arrangements")

(defun init-remote-eval-mode (&optional (keyword :vanilla))
  (setf $remote-eval-mode$
        (ecase keyword
          (:vanilla '(call call))
          ;; Vanilla - the request is sent by the event handler (priority 1), with the response run by the 
          ;; swank dispatcher. This is fast, but a lengthy request may hold up event processing
          ;; and a lengthy response will likely hold up the repl and the swank dispatcher.
          ;; Does not change the minibuffer.
          ;; Staus: works, no crashes
          #+mcl
          (:enqueue '(eval-enqueue funcall-enqueue))
          ;; It reliably works to enqueue both directions on the listener, but listener will be notably busy
          ;; and repl evaluation may get in the way of swank messaging (or the other way around)...
          ;; Alerts about busy/idle in the minibuffer.
          ;; Staus: works, no crashes
          (:fork/fork '(process-run process-run))
          ;; Forking both during eval request and response should have given nice concurrency abilities.
          ;; Often results in RMCL crashes, such as always when using the apropos dialog. Mysteriously,
          ;; having an MCLIDE listener present seems to make crashes like likely.
          ;; Staus: usually crashes in RMCL, works fine in MCL5.2.
          (:fork/call '(process-run call))
          ;; Forking the request but not the response].
          ;; Benefit (minor) is that the event handling can continue without waiting for the request.
          ;; Still holds up the repl if the response handling is lengthy.
          (:call/fork '(call process-run))
          ;; Forking the response only (to handle lengthy response processing). 
          ;; This would be my preference!
          ;; Status: leads to a hard crash of Apropos in RMCL.
)))

(init-remote-eval-mode)

(defun complete-remote-eval-request (thunk listener)
  "Completes the sending of a form for remote evaluation using the swank client of the listener"
  (ecase (first $remote-eval-mode$)
    (call
     (funcall thunk listener)) ; vanilla version
    #+mcl
    (eval-enqueue 
     (eval-enqueue `(funcall ,thunk ,listener)))
    (process-run
     (process-run-function "Remote Send" thunk listener))))

(defun complete-remote-eval-response (fun &rest args)
  "Completes handling a response from swank to a remote eval request."
  ;; ### PROBLEM: The process-run-function here results in crash in RMCL 
  ;;     unless there is at least one non-remote listener presenent (weird...)
  (ecase (second $remote-eval-mode$)
    (process-run
     (apply #'process-run-function "Remote Response" fun args))
    (call
     (apply fun args))
    #+mcl
    (eval-enqueue
     (eval-enqueue `(apply ,fun ',args)))
    #+mcl
    (funcall-enqueue
     (apply #'funcall-enqueue fun args)) ;; this works OK, but isn't optimal!
    #+mcl
    (enqueue-process-run
     (apply #'funcall-enqueue #'process-run-function "Remote Response" fun args))
))

(defmacro remote-eval-enqueue-bind (vars form &body body &aux (temp (gensym)))
  "Without waiting for completion to continue, evaluate a form or string in the current remote lisp, bind the result, then fork the body in the context of the original remote-listener."
  ;; This is usually the preferable way to perform remote evaluations.
  ;; ## optimize by starting the process while we wait for the reponse?
  ;; Ensures that *remote-lisp* is explicitely passed on even if processing is forked.
  `(flet ((thunk (*remote-lisp* &aux (,temp *remote-lisp*))
            (assert *remote-lisp*) 
              (remote-eval-enqueue-bind! ,vars ,form
                (complete-remote-eval-response
                 (lambda ,(cons '*remote-lisp* (if (listp vars) vars `(,vars)))
                   ,@body)
                 ,temp ,@(if (listp vars) vars `(,vars))))))
     (complete-remote-eval-request #'thunk (remote-lisp))))

(defmethod query-form (form &rest rest &key post-processing package thread recover remote)
   "Build a composite remote query form" 
   (declare (ignore post-processing package thread recover remote))
   (list* :form form rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP INFO

(defclass lisp-info ()
  ((lisp-implementation :reader lisp-implementation :initarg :lisp-implementation :initform nil)
   (machine :reader remote-machine :initform "Unknown" :initarg :machine)
   (features :reader remote-features :initform NIL :initarg :features)
   (modules :reader remote-modules :initform nil :initarg :modules)
   (package :reader remote-package :initform NIL :initarg :package)
   (version :reader remote-version :initform NIL :initarg :version)))

(defmethod initialize-instance ((info lisp-info) &key &allow-other-keys)
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SWANK INIT FORMS

(defparameter *swank-init-forms* NIL
  "Forms to execute on the swank server at connection time, based on the features provided by the server.")

;; (pprint *swank-init-forms*)

(defun register-swank-init-form (name feature form)
  "Register a form to execute on swank servers that matches the feature" 
  (let ((item (or (assoc name *swank-init-forms*)
                  (let ((item (list name)))
                    (push item *swank-init-forms*)
                    item))))
    (values 
     name
     (setf (cdr item)
           (cons
            (list feature form)
            (remove feature (cdr item) :key #'car))))))

(defun swank-init-forms (features)
  "List of forms and form-generators to init a remote lisp with the features (a list of strings, symbols and keywords, like *features*)."
  ; Only returns a single form for each named init form: the first that has a matching feature!
  (nreverse
   (loop
     for spec in *swank-init-forms*
     for form = (assoc-if (lambda (feature)(member feature features :test #'string-equal)) 
                          (cdr spec))
     when form
     collect (second form))))

(defun string-for-swank (form)
  ;; ## better use a function in the swank-rpc package, but not ready yet...
  (let ((*package* swank-client::+null-package+) ; ad-hoc to make print-swank-symbol do the right thing
        (*explicit-swank-package* NIL) ; explicit package prefix not required when form is encoded in a string
        (*print-case* :downcase))
    (prin1-to-string form)))

(define-condition incomplete-swank-initforms (error)
  ((forms :initarg :forms :type hash-table))
  (:report (lambda (c out)
             (with-slots (forms) c
               (format out "~&Init of swank did not complete ~A forms:" (hash-table-count forms))
               (maphash (lambda (key value)
                          (declare (ignore value))
                          (format out "~& ~A..." 
                                  (subseq (string key) 0 (min (length (string key)) 180))))
                        forms)))))

(defun init-swank-from-features (client features)
  (let ((forms (swank-init-forms features))
        (incomplete (make-hash-table)))
    (assert client)
    (flet ((execute (string)
             (setf (gethash string incomplete) T)
             (swank-client::swank-remote-eval-async client
              (interactive-eval-region-form string)
              (lambda (result)
                (declare (ignore result))
                (remhash string incomplete))
              :swank))
           (wait-for-completion ()
             (loop
               with time = (get-universal-time)
               until (= 0 (hash-table-count incomplete))
               until (>= (get-universal-time) (+ time 20)) 
               unless (swank-client::swank-client-dispatch-next client :timeout 0)
               do (sleep 0.1)
               finally (return (= 0 (hash-table-count incomplete))))))
      (dolist (form forms)
        (etypecase form
          (null nil)
          (pathname
           (with-open-file (in form :if-does-not-exist nil)
             (when in
               (with-output-to-string (out)
                 (loop for line = (read-line in nil)
                       while line
                       do (write-string line out)
                       ; use linefeed for swank!
                       do (write-char #\linefeed out))
                 (execute (get-output-stream-string out))))))
          (string
           (execute form))
          (cons
           (execute (string-for-swank form)))))
      (unless (wait-for-completion)
        (cerror "Continue" 'incomplete-swank-initforms  :forms incomplete)))))

; (swank-init-forms '(:common-lisp))

(defun current-implementation ()
  "Return the remote application active in the context"
  (let ((lisp (remote-lisp)))
    (etypecase lisp
      (remote-application lisp)
      (remote-repl (remote-repl-application lisp)))))

(defun remote-feature-p (&rest names)
  "Return the first of the names that either is a feature of the remote lisp or the name of the implementation"
  (declare (dynamic-extent names))
  (let* ((imp (current-implementation))
         (feature (find-if (lambda (f)(member f names :test #'string-equal))
                           (list*
                            (remote-implementation-type imp)
                            (remote-implementation-type-name imp)
                            (remote-features imp)))))
    (when feature
      (find feature names :test #'string-equal))))

(defmacro define-swank-function (name (&rest arglist) &rest body)
  "Slimefun to be defined on the remote swank servers at connection time, with a function to generate a form for the call"
  ; Facilitates custom slimefuns on different lisp implementations.
  ; # perhaps this should also define the function interactively on all remote lisps??
  (let ((description (when (stringp (car body)) (pop body)))
        (features (remove-if-not #'keywordp (mapcar #'car body))))
    `(eval-when (:load-toplevel :execute)
       (define-swank-call ,name ,arglist 
         :description ,description
         :slimefun (if (remote-feature-p ,@features)
                     nil
                     (progn ,@(or (cdr (assoc 'T body))
                                  (let ((missed (set-difference '(:common-lisp :clojure :gambit) features)))
                                     (when missed
                                       (warn "No default swank function name for ~A, or missing forms for ~A" name missed))
                                     nil)))))
       ,@(mapcar
          (lambda (def)
            (if (eql T (car def))
             nil
            `(register-swank-init-form ',name ',(car def)
              ;; avoid converting to string here
              ;; - may not be properly converted due swank-compatible format until this module is loaded!
              ,(typecase (second def)
                ((or symbol string)
                 (string (second def)))
                (otherwise
                  `'(defslimefun ,name ; ,(make-symbol (string-downcase name))
                    ,arglist
                    ,@(cdr def)))))))            
          (reverse body))
       ',name)))

(eval-when (:load-toplevel :execute) ;; ## move to top of resources?
  (register-swank-init-form :initfile :common-lisp #p"resources:src;init-swank.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIMEFUN

(defmacro define-swank-call (name (&rest arglist) &key slimefun description)
  "Function NAME generate a form to execute a slimefun on swank (the slimefun symbol can be generated at calling time)"
  (let ((foreign-name (make-symbol (format nil "swank:~A" (string-downcase name)))))
    (multiple-value-bind (required optional restp rest keyp keys allowp aux)
                         (ccl::parse-lambda-list arglist)
      (declare (ignore rest aux allowp keyp restp))
      `(defun ,name ,arglist
         ,(or description "Build a form to evaluate slimefun on the swank server")  
         (list
           (or ,slimefun ',foreign-name)
           ,@(append required optional
                     (mapcan (lambda (key)
                               (list (intern (string key) :keyword) key))
                             keys)))))))

(defmacro declare-slimefun (name (&rest arglist) &key alias description)
  "Declare a known swank slimefun, defining a function that generates a form to execute it on swank"
  `(define-swank-call ,(or alias name) ,arglist
      :description ,(or description 
                        (format nil "Build a form to evaluate the ~A slimefun on the swank server" 
                                (string-downcase name)))       
      :slimefun ',(make-symbol (format nil "swank:~A" (string-downcase name)))))

;; # make more like def for foreign functions?

(declare-slimefun interactive-eval-region (string) :alias interactive-eval-region-form)
(declare-slimefun create-repl (target) :alias create-repl-form)
(declare-slimefun connection-info () :alias connection-info-form)
(declare-slimefun listener-eval (string) :alias listener-eval-form)
(declare-slimefun invoke-nth-restart (level n) :alias invoke-nth-restart-form)
(declare-slimefun sldb-continue () :alias sldb-continue-form) 
(declare-slimefun sldb-abort () :alias sldb-abort-form) 
(declare-slimefun throw-to-toplevel () :alias throw-to-toplevel-form) 
(declare-slimefun inspect-in-emacs (what) :alias inspect-in-emacs-form)
(declare-slimefun inspect-current-condition () :alias inspect-current-condition-form-1)
(declare-slimefun frame-source-location (id) :alias frame-source-location-form-1)
(declare-slimefun inspect-frame-function (id) :alias inspect-frame-function-form-1)
(declare-slimefun frame-locals-and-catch-tags (frame-number) :alias frame-locals-and-catch-tags-form-1)
(declare-slimefun inspect-frame-var (frame index) :alias inspect-frame-var-1)
(declare-slimefun sldb-return-from-frame (frame index) :alias sldb-return-from-frame-form)
(declare-slimefun restart-frame (frame-number) :alias restart-frame-form-1)
(declare-slimefun operator-arglist (symbol-name package) :alias operator-arglist-form)
(declare-slimefun inspect-nth-part (index) :alias inspect-nth-part-form)
(declare-slimefun untrace-all () :alias untrace-all-form)
(declare-slimefun list-threads () :alias list-threads-form)
(declare-slimefun simple-completions (prefix package) :alias simple-completions-form)
(declare-slimefun describe-definition-for-emacs (symbol type) :alias describe-definition-for-emacs-form)
(declare-slimefun swank-macroexpand-1 (string) :alias swank-macroexpand-1-form)
(declare-slimefun swank-macroexpand (string) :alias swank-macroexpand-form)
(declare-slimefun swank-macroexpand-all (string) :alias swank-macroexpand-all-form)
(declare-slimefun find-definitions-for-emacs (string) :alias find-definitions-for-emacs-form)
(declare-slimefun xref (type name) :alias xref-form)
(declare-slimefun undefine-function (fname-string) :alias undefine-function-form)
(declare-slimefun inspector-pop () :alias inspector-pop-form)
(declare-slimefun inspector-next () :alias inspector-next-form)
(declare-slimefun inspector-reinspect () :alias inspector-reinspect-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE APPLICATION

;; rename to remote-implementation?

(defvar *remote-applications* nil "list of all active remote lisp applications")

(defparameter *remote-applications-count* 0)

(defparameter *remote-application-class* 'remote-application)

(define-condition address-error (simple-error) ())

(defun signal-address-error (format-string &rest args)
   (error 'address-error #+mcl :format-string #-mcl :format-control format-string :format-arguments args))

(defun valid-ip (host)
   (etypecase host
      (integer host)
      (string
       (handler-case
          (let* ((host (string-trim ccl::wsp&cr host))
                   (host (if (position-if-not #'whitespacep host) host "127.0.0.1")))
            #+ignore (ccl::tcp-host-address host) ;; # can be eliminated in place of usocket?
            (usocket:ip-from-octet-buffer 
             (usocket::host-to-vector-quad host)))
          (tcp-bad-ip-address ()
           (signal-address-error "Invalid host IP address: ~S" host))
          (error () ; shouldn't happen!
           (signal-address-error  "Failed to get IP address from ~S" host))))))

(defun valid-portnum (port)
   (etypecase port
      (integer port)
      (string 
       (handler-case 
          (multiple-value-bind (num len)
                                             (read-from-string (string-trim ccl::wsp&cr  port))
              (unless (= len (length port))
                 (signal-address-error "The port number should only contain digits, not ~S" 
                                                     (subseq port len)))
              (check-type num integer)
              num)
          (type-error ()
                             (signal-address-error "The port should be a whole number, not ~S" port))
          (error ()
                    (signal-address-error "Invalid port number: ~S" port))))))

(defun valid-secret (password)
   (etypecase password
      (null NIL)
      (string
       (let ((password (string-trim ccl::wsp&cr password)))
         (unless (equal password "")
            password)))))

(defun remote-application (host port &optional secret)
   "Creates a representation of a remote implementation without connecting to it - Can be used with gui input directly as arguments are flexible and validated"
   (let ((host (valid-ip host))
          (port (valid-portnum port))
          (secret (valid-secret secret)))
     (let ((app (make-instance *remote-application-class* 
                  :host host
                  :port port
                  :secret secret)))
       app)))

(defclass remote-application ()
  ((id :reader remote-application-id :type string :initarg :id :initform (write-to-string (incf *remote-applications-count*)))
   (clients :initform nil :type list :accessor remote-clients)
   (host :type (or null integer) :reader remote-host :initarg :host :initform nil)
   (port :type (or null integer) :reader remote-port :initarg :port :initform nil)
   (secret :type (or null string) 
                      :reader remote-secret :initarg :secret :initform (swank-client::slime-secret) 
           :documentation "An optional password for the connection")
   (info :type (or lisp-info null) :initform nil)
   (indentation :type list :initform nil :accessor remote-indentation :documentation "Indentation for the editor, based on the values provided by the :indentation-update event"))
  (:documentation "Represents a remote LISP system"))

(defmethod initialize-instance :after ((app remote-application) &rest rest)
   ;; ## should this be done after connecting instead?
   (push app *remote-applications*))

(defmethod remote-hostname ((app remote-application))
   ;; ## should use usocket functions for portability - see ip-to-octet-buffer
   (let ((host (remote-host app)))
     (unless (member host `(nil 0 ,(valid-ip "127.0.0.1")))
       #+ignore (ccl::tcp-addr-to-str host)
       (usocket::host-to-hostname host))))

(define-condition connect-timed-out (error) ())

(defmethod open-remote-client ((application remote-application))
  (with-slots (host port secret) application
    (assert port)
    (handler-case
       (let* ((client (swank-client::open-swank-client (or host 0) port secret)))
         (push client (remote-clients application))
         client)
       (usocket:timeout-error ()
        (error 'connect-timed-out)))))

(defmethod update-indentation ((app remote-application) update)
  "Non-destructively update the indentation for the editor based on an association list of changes"
  (let ((indent (remote-indentation app)))
    (dolist (item indent)
      (pushnew item update :test #'string-equal :key #'car))
    (setf (remote-indentation app) update)))

(define-swank-function mclide-eval-region (string)
  "Eval a form read from a string, bypassing interactive error handling"
  ; # better using swank:interactive-eval-region?
  (:common-lisp
   (eval-region string)))

;; maybe this should be in swank-client module? on a target-lisp class subclassed by remote-application?
(defmethod init-swank ((application remote-application))
  ; has to use only built-in slimefuns here as custom ones may not yet have been defined!
  (let ((client (first (remote-clients application)))
        (features (or (remote-features application)
                      (remove-if #'null
                                 (list (remote-implementation-type application)
                                       (remote-implementation-type-name application))))))    
    (init-swank-from-features client features)))

#| delete
(defmethod connected-p ((client swank-client::swank-client))
  (open-stream-p (swank-client::swank-connection client))
  #+ignore ;; perhaps better:
  (usocket::connected-p (swank-client::swank-connection client)))

(defmethod connected-p ((application remote-application))
  (let ((client (first (remote-clients application))))
    (connected-p client)))

(defmethod connected-p ((object null))
  nil)
|#

(define-condition notify-connected ()
   ((implementation-type :initarg :type)
    (implementation-version :initarg :version)))

(defun notify-connected (type version)
   "Notifies connected to an implementation of the given type and version"
   (signal 'notify-connected :type type :version version))

(defmethod attach ((application remote-application) remote-repl &key (initial (not (remote-clients application))))
   (flet ((thunk ()
              (let* ((client (open-remote-client application))
                       (id :repl-thread))
                (swank-client::set-swank-client remote-repl client)
                (update-package remote-repl)
                (notify-connected 
                 (remote::remote-implementation-type application)
                 (remote::remote-implementation-version application)) 
                (when initial
                    (init-swank application))
                ; swank2 starts repl by default, but not latest versions 
                ;; need to be after the client/listener is ready!
                ;; ## is it a chance that another remote-eval in the repl happens before this is completed??
                (using-remote-lisp remote-repl
                   (remote-send (create-repl-form nil)))
                ;; Order matters... The remote-eval above cannot use the repl it creates...
                (setf (swank-client::thread-id remote-repl) id))))
      (handler-case
         (thunk)
         (swank-client::reconnect (c) 
          (setf (slot-value application 'port)
                  (swank-client::reconnect-port c))
          (thunk))
         (usocket:connection-refused-error ()
          #+ignore
          (swank-client::close-connection (first (remote-clients application)))
          (error 'failed-connecting)))))

 (defmethod remote-info ((app remote-application))
  (or (slot-value app 'info)
      ;; ## improve!!! Use a promise???
      (let ((client (first (remote-clients app))))
        (when client
          (let (#+ignore (*remote-lisp* client)) ;; ## should be bound to a remote-repl, not client! Or bind :connection below...
            (remote-eval-bind (info)
                              (query-form (connection-info-form)
                                      :package "SWANK" ; to eliminate the swank:: prefix in transmission - may not be needed?
                                      ; maybe this isn't required as the client isn't a thread???
                                      :remote client ;; ## should be bound to a remote-repl, not client?
                                      :thread t)
              (setf (slot-value app 'info) 
                    (apply #'make-instance 'lisp-info info))))))))

(defmethod remote-implementation-type ((app remote-application))
  (let ((info (remote-info app)))
    (when info
      (getf (lisp-implementation info) :type)))) 

(defmethod remote-implementation-type-name ((app remote-application))
  (let ((info (remote-info app)))
    (when info
      (getf (lisp-implementation info) :name))))

(defmethod remote-features ((app remote-application))
  (let ((info (remote-info app)))
    (when info
      (remote-features info))))

(defmethod remote-default-package ((app remote-application))
  "The name of the default package suggested by the swank server (with prompt/nickname as second value)"
  (let ((info (remote-info app)))
    (when info
      (destructuring-bind (&key name prompt &allow-other-keys)
                          (remote-package info)
        (values name prompt)))))

(defmethod remote-implementation-version ((app remote-application))
  (let ((info (remote-info app)))
    (when info
      (getf (lisp-implementation info) :version))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE THREAD

(defclass remote-repl (swank-client::remote-repl)
  ())

(defmethod input-available-p ((thread remote-repl))
  "True if there is an incoming event to dispatch"
  (swank-client::next-message-available-p (swank-client::swank-client thread)) 
  #+ignore
  (let ((connection (client-connection thread)))
    ;; calls process-lock which may result in recursive process-wait error!
    (stream-listen connection)))

(defmethod update-package ((repl remote-repl))
  (multiple-value-bind (pname prompt)
                                      (remote-default-package 
                                       (remote-repl-application repl))
       (declare (ignore prompt))
       (check-type pname string)
       (setf (thread-package repl) pname)))

(defmethod remote-implementation-version ((thread remote-repl))
   (remote-implementation-version (remote-repl-application thread)))

#+ignore
(define-swank-function mclide-interrupt-abort () 
  ;; should be avoided as it isn't supported for thread-less lisps
  (:common-lisp
     (interrupt-thread (find-repl-thread (default-connection))
                       (lambda () (abort)))))

(define-swank-function mclide-interrupt-abort () 
  ;; Also works when server doesn't have threads.
  ;; Perhaps :emacs-interrupt can be used instead?
  (:common-lisp
   #+ignore ; signal-interrupt eliminated from swank past march 2010
    (signal-interrupt (find-worker-thread :repl-thread) 'abort)
    (cond ((use-threads-p)
           (interrupt-thread (find-repl-thread *emacs-connection*)
                             (lambda ()
                               ;; safely interrupt THREAD
                               (invoke-or-queue-interrupt #'abort))))
          (t (abort)))))

(defun swank-encode (string)
  "Encode a string to be compatible with remote lisps"
  (substitute #\lf #\cr string))

(defmethod remote-repl-call ((lisp remote-repl) command-string &aux ok)
  "Evaluates the expression string as in the repl of the listener"
  ;; Slime3 uses swank:interactive-eval to evaluate last expression in editor.
  ;; or async swank:eval-and-grab-output.
  ;; Slime does not call listener-eval, at least not directly.
  ;; Yet the listener-eval seems to be right as it maintains history etc. 
  ;; Returns the result from swank, which is nil or (soon) an integer indicating characters consumed.
  (check-type command-string string)
  (using-remote-lisp lisp
    (unwind-protect
      (let* ((form (query-form (listener-eval-form (swank-encode command-string))
                                             :thread (thread-id lisp))))
        ;; Note: swank uses :write-string to present the result.
        ;; hence don't use async mode here even for swank3...
        (prog1
          (remote-eval form)
          (setf ok t)))
      (unless ok ; user-initiated abort, was mclide1 command-. in the listener
        #+ignore ; no longer needed per february 2012 (messes up multiple level debug)
        (remote-eval-enqueue-bind (v) ;  sent to control thread
                                  (mclide-interrupt-abort)
          (declare (ignore v)))))))

(defmethod remote-repl-send ((lisp remote-repl) form)
  "Evaluate a form on the swank repl, ignoring the response"
  ; same as swank-remote-eval-async but without reporting the :abort or processing result
  (using-remote-lisp lisp
    (remote-send (query-form form 
                                               :thread (thread-id lisp) 
                                               :package (swank-client::swank-package lisp))
                 t)))

(defmethod swank-return-string ((thread remote-repl) string tag)
  (when string
      (let ((thread-id (thread-id thread)))
        (check-type thread-id number "Only numbers allowed for thread-id when returning a string (even to the repl)") 
      (swank-client::dispatch-return-string thread thread-id string tag))))

(define-swank-function mclide-invoke-nth-restart (level n)
  (:common-lisp
   (let ((cl:*error-output* (cl:make-broadcast-stream))) ; avoid swank reporting restart
    (invoke-nth-restart-for-emacs level n)))
  ; # should make it possible to eliminate these:
  (:gambit ; # can be eliminated with the default!
   #:|(define (swank:mclide-invoke-nth-restart level n)
       (swank:invoke-nth-restart-for-emacs level n)) 
     |)
  (:clojure #:|
    (in-ns 'swank.commands.basic)   
    (swank.commands/defslimefn mclide-invoke-nth-restart [level n]
         (swank.commands.basic/invoke-nth-restart-for-emacs level n))
       nil |)
   (T '#:|swank:invoke-nth-restart-for-emacs|))

(defmethod repl-invoke-nth-restart ((lisp remote-repl) thread level n)
   ; thread is from activate-debug and may differ from the repl thread!
   (using-remote-lisp lisp
    (remote-eval ;; don't use async as it leads to warning
      (query-form (mclide-invoke-nth-restart level n)
        :package :swank
        :thread thread))))

(defmethod swank-continue ((lisp remote-repl))
  (remote-repl-send lisp (sldb-continue-form)))

(defmethod swank-abort ((lisp remote-repl))
  (remote-repl-send lisp (sldb-abort-form)))

(defmethod swank-throw-to-toplevel ((lisp remote-repl))
  (remote-repl-send lisp (throw-to-toplevel-form)))

(defmethod thread-id ((lisp remote-repl)) ;; # alternatively just import it
   (swank-client::thread-id lisp))

(defmethod thread-package ((lisp remote-repl))
   (swank-client::swank-package lisp))

(defmethod (setf thread-package) (value (lisp remote-repl))
   (setf (swank-client::swank-package lisp) value))

(defmethod remote-repl-application ((thread remote-repl))
  (find-if 
   (lambda (app)
        (member (swank-client::swank-client thread)
                    (remote-clients app)))
     *remote-applications*))

(defmethod remote-implementation-type ((thread remote-repl))
   (remote-implementation-type (remote-repl-application thread)))

(defmethod remote-implementation-type-name ((thread remote-repl))
   (remote-implementation-type-name (remote-repl-application thread)))

(define-swank-function mclide-kill-thread (id)
  (:common-lisp
     ; # can this be eliminated by using id directly?
     ; The thread may no longer exist on the server even if listed in the IDE!
    (let ((thread (find-thread id)))
      (when thread
        (kill-thread thread)))))

(defmethod close-remote-repl ((repl remote-repl))
  (let ((connection (client-connection repl)))
    (unless (or (not connection)
                 #+mcl (ccl::stream-closed-p connection))
      (let ((id (thread-id repl)))
        (if (eq id :repl-thread)
          (let (#+mcl (ccl::*tcp-close-timeout* 1/60))
            (swank-client::slime-net-close connection))
          (using-remote-lisp repl
            (remote-send (mclide-kill-thread id))))))))

(defgeneric edit-file-handler (repl file &key line column position))

(defgeneric edit-definition-handler (repl symbol type))

(swank-client::define-swank-dispatch :ed remote-repl (what)
  "WHAT can be:
  A filename (string), -- ????? really
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil."
  (if what
    (swank-client::destructure-case what 
     ((:filename file &key line column position)
      (edit-file-handler remote-repl file :line line :column column :position position)) 
     ((:function-name name)
      (edit-definition-handler remote-repl (remote::make-remote-symbol name) 'function)))
    (edit-file-handler remote-repl nil))
  t)

(defgeneric inspect-handler (repl object))

(defmethod inspect-handler ((repl remote-repl) object)
   "Usually override with custom handler to inspect the object in a dialog"
   (inspect object))

(swank-client::define-swank-dispatch :inspect remote-repl (value &optional thread tag)
  ; thread and tag are only set when swank expects a value back and may not be included by server.
  (assert (and thread tag))
  (unwind-protect
      (let* ((session (make-instance 'remote::remote-inspection-session
                        :thread thread :connection (client-connection remote-repl)))
             (object (parse-remote-inspected-object value session)))
        (inspect-handler remote-repl object))
    ; alternatively call this before the inspect handler!
    (swank-client::swank-client-dispatch (swank-client::swank-client remote-repl)
                                         :emacs-return thread tag NIL))
  t)

(defgeneric new-package-handler (repl package-name))

(swank-client::define-swank-dispatch :new-package remote-repl (package prompt-string)
  ; # A flaw is that this dispatch doesn't provide the thread with the changed package, so we have to guess...
  (declare (ignore prompt-string))
  (new-package-handler remote-repl package)
  t)

(defgeneric debug-setup-handler (repl thread level condition restarts frames conts))

(swank-client::define-swank-dispatch :debug remote-repl (thread level condition restarts frames conts)
  ; Note: remote-eval should never be called before binding debug context, as it calls swank-client-dispatch-next!
  (debug-setup-handler remote-repl thread level condition restarts frames conts)
  t)

(defmethod debug-activate-handler ((repl remote-repl) thread level)
  (declare (ignore thread level))
  nil)

(swank-client::define-swank-dispatch :debug-activate remote-repl (thread level dummy)
  ; Note: :debug-activate can be dispatched before :debug has completed if making any calls to swank!
  (declare (ignore dummy))
  ;(setf *break-level* level)
  (debug-activate-handler remote-repl thread level)
  t)

(defgeneric debug-return-handler (repl thread level stepping))

(swank-client::define-swank-dispatch :debug-return remote-repl (thread level stepping)
  (debug-return-handler remote-repl thread level stepping)
  t)

(defgeneric background-message-handler (repl string))

(swank-client::define-swank-dispatch :background-message remote-repl (output)
  (background-message-handler remote-repl output)
  t)

(defgeneric write-string-handler (repl string))

(swank-client::define-swank-dispatch :write-string remote-repl (output &optional mode)
  (declare (ignore mode))
  (write-string-handler remote-repl output)
  t)

(swank-client::define-swank-dispatch :indentation-update remote-repl (info)
  ; non-destructive in case the indent is in use
  (let ((app (remote-repl-application remote-repl)))
    (update-indentation app info))
  t)

(swank-client::define-swank-dispatch :not-connected remote-repl ()
  ; not part of swank 
  (error 'not-connected :message "Not connected")
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ FROM LISTENER

(defgeneric read-string-handler (repl))

(swank-client::define-swank-dispatch :read-string remote-repl (thread tag)
  "Reads an expression and return it to swank as a string (while dispatching)"          
   ; (eq thread (thread-id remote-repl))
   (let (p)
    (flet ((abort-read ()
             (process-abort p))
           (redirect-listener-input ()
             (ccl::with-standard-abort-handling "Read aborted"
              (let ((string (read-string-handler remote-repl)))
               #+ignore ; fails as long as thread-id is :repl-thread rather than a number!
               (swank-return-string remote-repl string tag)
               (swank-client::dispatch-return-string remote-repl
                                       thread ; may be different that thread-id property for a repl
                                       string
                                       tag)))
             (unregister-read-abort-action remote-repl thread tag)))
      (register-read-abort-action remote-repl thread tag #'abort-read)
      (setf p
       (ccl::process-run-function "input for remote" #'redirect-listener-input))))
    t)

(defvar *read-abort-actions* (make-hash-table :test 'equal)) ;; ## better if slot on remote-repl to ensure they're not hanging around after closing listener!

(defmethod register-read-abort-action (remote-repl thread tag action)
   (setf (gethash (list remote-repl thread tag) *read-abort-actions*) action))

(defmethod unregister-read-abort-action (remote-repl thread tag)
  (remhash (list remote-repl thread tag) *read-abort-actions*))

(defmethod activate-read-abort-action (remote-repl thread tag)
  (let ((action (gethash (list remote-repl thread tag) *read-abort-actions*)))
    (unregister-read-abort-action remote-repl thread tag)
    (when action
      (funcall action))))

(swank-client::define-swank-dispatch :read-aborted remote-repl (thread tag)
   "abort reading initiated by the :read-string swank event"
   (activate-read-abort-action remote-repl thread tag)
   T)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RPC

(defun remote-call (command-string)
  "Calls with a string, returns results as remote objects"
  ;; maybe better if this can format the string!
  (check-type command-string string)
  (let ((lisp (remote-lisp)))
    (swank-client::swank-remote-call lisp command-string)))

#+mcl
(defun funcall-enqueue (&rest rest)
  "Enqueues the function for evaluation in the current listener process"
  ;; doesn't use eval-enqueue as it will use the topmost listener rather than the current process.
  ;; enqueing async remote eval may lead to conflicts if a form is remote eval'ed in the listener!
  ;; The repl will be on hold while the enqueued function runs, so use this with caution!
  (assert (ccl::process-is-listener-p *current-process*))
  (setq *eval-queue* 
        (nconc *eval-queue* 
                 (cons (cons #'funcall rest) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROMISE

(defclass promise ()
  ((ticks :initform (ccl::get-tick-count))
   (ready :documentation "semaphore to flag when it has been filed with valid content")))

(defmethod initialize-instance :after ((p promise) &key form (deliver #'deliver))
   (remote-eval-enqueue-bind (result) form
     (when deliver
       (funcall deliver p result))))

(defmethod deliver ((p promise) result)
  (setf (slot-value p 'ready) result))

;; # needs error message etc...

(define-condition promise-not-fulfilled (error) ())

;; delete wait-until-ready methosd!!
; maybe fullfill shouldn't trigger an error but rather return T if fulfilled and NIL if not?

(defmethod fulfill (object &key timeout)
  (declare (ignore timeout))
  object)

(defmethod fulfill ((p promise) &key timeout)
  (let ((timeout (when timeout (- timeout (- (ccl::get-tick-count) (slot-value p 'ticks))))))
    (if (process-wait-with-timeout "fulfill" timeout #'slot-boundp p 'ready)
      (slot-value p 'ready)
      (error 'promise-not-fulfilled)))) 

(defun promise (form)
  "Returns a promise after enqueueing the form for remote evaluation to later provide its result"
  (make-instance 'promise :form form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REFORMAT TEXT

#-mcl
(defun char-eolp (c)
  (member c '(#\newline #\linefeed)))

#+mcl
(import 'ccl::char-eolp )

(defun oneline (string)
  "Return a copy of the string without newlines or linefeeds."
  (substitute-if #\space #'char-eolp string))

(defun fix-foreign-string (foreign-string)
  "Return a copy of a foreign string with characters like newlines updated for compatibility with this version of MCL"
  (substitute #\newline #\linefeed foreign-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE S-EXPRESSIONS

#|
S-expressions for a remote lisp may contain symbols in packages that does not exist within MCLIDE. It may also contain constructs not understood by the MCL reader. Hence remote s-expressions are not read but encapsulated.

Remote s-expressions are encoded as uninterned symbols. Much of the relevant functionality in MCL won't balk on symbols but pass them through. For practical reasons, A practical benefit compared to other potential representations is that much of MCLs IDE implementation therefore can be reused for our purposes with little or no modification.
|#

(in-package :remote)

(defun make-remote-sexp (string &optional package-name (lisp (remote-lisp)))
   "Makes a representation of a remote s-exp based on a string from which it can be read in the package"
   (check-type package-name (or null string)) ;; # consider require package?
   (check-remote-lisp lisp)
   (let ((sym (make-symbol string)))
     (setf (get sym 'lisp) lisp
           (get sym 'package) package-name)
     sym))

(defun remote-sexp-p (object)
   "True if the object represents a sexp on a remote lisp"
   (and (symbolp object)
         (get object 'lisp)))

; (remote-sexp-p (make-remote-sexp "princ" "cl"))

(defun remote-reader-package (sexp)
   "The name of the package in which the sexp should be read"
   (get sexp 'package))

(defun check-remote-sexp-lisp (sexp)
   "Santity checking asserting consistent remote lisp"
   (assert (remote-sexp-p sexp))
   (assert *remote-lisp*)
   (assert (eq (get sexp 'lisp) *remote-lisp*) () "Inconsistent remote lisp")) 

(defun remote-sexp-form (sexp)
   "generates a form which when evaluated on the remote lisp reads and returns the expression as read in its reader package, for use in other remote expressions"
   (assert (remote-sexp-p sexp))
  (if (remote-reader-package sexp)
    `(parse-string ,(string sexp) ,(remote-reader-package sexp))
    `(from-string ,(remote-symbol-string sexp) )))

(defparameter *reveal-remote-sexp* nil
    "For debugging purposes - true to print remote expressions as such")

; ## should specify whether it is symbol or sexp!!

(defmethod print-object ((symbol symbol) stream)
  (cond
   ((remote-sexp-p symbol)
    (if *reveal-remote-sexp*
      (if (remote-symbol-p symbol)
        (format stream "#<REMOTE-SYMBOL ~A in ~S>" 
                 (remote-symbol-string symbol)
                 (remote-symbol-package symbol))
        (format stream "#<REMOTE-SEXP ~A in ~S>"
                (remote-sexp-form symbol)
                (remote-reader-package symbol)))
        ;; ## ad-hoc, should properly handle case, like write-pname in mcl!
      (if (remote-symbol-p symbol)
        (write-string (remote-symbol-string symbol) stream)
        (write-string (symbol-name symbol) stream))))
   ; Symbols in the remote swank package are represented as uninterned symbols:
   ((ignore-errors ; its a mess if symbol printing fails!
     (and
      (eq *package* swank-client::+null-package+) ; ad-hoc - only true in swank-client::slime-net-send
      #+ignore (not (symbol-package symbol))
      (progn
        (print-swank-symbol symbol stream)
        t))))
   (t
    (call-next-method))))

(defparameter *explicit-swank-package* "SWANK"
  "Default package qualifier for all symbols to be read by swank (has to be uppercase)")

(defun print-swank-symbol (symbol stream)
  "Print a symbol in the default swank package"
  ; Interned symbols are printed as in *explicit-swank-package*.
  ; Uninterned symbols prints as the raw name without escape.
  ; Use uninterned symbols when situation demands complete control over the printed representation.
  ; For even more control and convenience, use a 'remote-symbol or 'remote-sexp'.
  ; Don't change without careful consideration!
  (let ((name (symbol-name symbol))
        (case *print-case*)
        (package *explicit-swank-package*))
    (cond
     ((symbol-package symbol) ;; interned symbols are considered internal to the swank package:
      (cond
       ((keywordp symbol)
        (write-char #\: stream))
       ((and package 
             (not (eql symbol t))
             (not (null symbol)))
        (ccl::write-pname package case stream)
        (write-string "::" stream)))
      (ccl::write-pname name *print-case* stream))
     (T 
      (write-string name stream)))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE SYMBOLS

#|
Remote symbols are pragmatically represented using the same representation as for remote s-expressions. Not interned, so multiple remote symbols can represent the same symbol on the remote lisp. 
|#

(in-package :remote)

(defparameter *remote-package* NIL
  "The defult remote package when making remote symbols")

(defun make-remote-symbol (symbol-name &optional (package-name *remote-package*))
  "A representation of a symbol in the remote LISP environment (a subtype of remote-sexp)"
  (let ((sym (make-remote-sexp symbol-name package-name)))
    (setf (get sym 'symbol-p) t) 
    sym))

(defun remote-symbols (items &optional default-package)
  "Convert a list of names into remote symbols" 
  (check-type items list)
  (mapcar
   (lambda (name)
     (if default-package
       (remote::make-remote-symbol name default-package)
       (remote::make-remote-symbol name)))
   items))

(defun remote-symbol-p (symbol)
   (and (remote-sexp-p symbol)
         (get symbol'symbol-p)))

(deftype remote-symbol () `(satisfies remote-symbol-p))

(defun remote-symbol-package (remote-symbol)
   ;; ##careful, this should consider the prefix too!!
    (remote-reader-package remote-symbol))

(defun remote-symbol-name (remote-symbol)
   ;; ## returns package prefix too...
  (symbol-name remote-symbol))

(defun remote-symbol-string (remote-symbol)
  (etypecase remote-symbol
    (symbol
     (let ((pk (remote-symbol-package remote-symbol))
           (nm (symbol-name remote-symbol)))
       (if pk
         (format nil "~A::~A" pk nm)
         nm)))
    (string ;; forgiving for ease of use
     remote-symbol)))

; (make-remote-symbol "foo" "bar")

(defun string-name-package (name)
  "Split a symbol encoded as a string into name and package strings"
  ; like ccl::string-package-and-tail
  (let ((pos (position #\: name))
        (pkg))
    (when pos
      (setq pkg
            (if (eq pos 0) 
              "KEYWORD"
              (string-upcase (subseq name 0 pos)))))
    (when pkg
      (setq name
            (subseq name (1+ (if (eq #\: (schar name (1+ pos))) (1+ pos) pos)))))
    (values name pkg)))

; (string-name-package "CCL::test")
; (string-name-package "test")

(defun make-symbol-string (name &optional package)
  "Combines a symbol name and package into a symbol string"
  (check-type name string)
  (if package
    (format nil "~A::~A" package name)
    name))

(defparameter swank-rpc::*intern-foreign-symbol* ; # should rather be bound around calls to swank-rpc
      (lambda (string)
              (cond
               ((and (not (equal "" string))
                     (eql #\: (schar string 0)))
                ;; used for swank keywords too 
                ;; ## wrongly upcases symbols with escapes for lowercase chars
                (intern (string-upcase (subseq string 1)) :keyword)
                #+ignore
                (read-from-string  string :preserve-whitespace T))
               (T
                (make-remote-symbol string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE DEFINITION

;; ## consolidate all uses of remote definitions into a shared representation!!
;; ## rename to not be 'remote' - similar concept used elsewhere.

(defclass remote-definition ()
  ((name :type (or null symbol) :initform nil)
   (package 
    :reader remote-definition-package 
    :initarg :package
    :type (or null string)
    :initform NIL
    :documentation "The package to intern unqualified symbols in the definition")
   (content :reader remote-definition-spec :initarg :spec :initform nil)) ;; ## rename to spec?
  (:documentation "The definition of an object on the remote lisp, possibly kepts as a proxy or lazy"))

(defun normalize-dspec (dspec package)
  "Provides the dspec in a form expected by a remote definition"
  ;; could do a better job in checking a dspec!
  ; may just as well deconstruct into the remote definition, so this is a transition!
  ; Note: swank encodes a dspec as a string in a way that may not be readable and may not always conform to the swank-rpc::simple-read format!
  ; If parsing fails, return the original dspec string.
  ; ## It would be beneficial if swank had a much more strict and predictable encoding of dpecs!!!
  (etypecase dspec
    (list dspec)
    (string
     (with-input-from-string (*standard-input* dspec)
       (labels ((stringify (d)
                  (typecase d
                    (list
                     (mapcar #'stringify d))
                    (symbol
                     (let* ((s (string d))
                            (n (length s)))  
                         (if (and (> n 2) ; swank may print 'function as #'
                                  (eql #\# (schar s 0))
                                  (eql #\' (schar s 1)))
                           (list 'function (subseq s 2))
                           s)))
                    (otherwise
                     (string d)))))        
         (stringify 
          (let ((*remote-package* (or package *remote-package*)))
            (handler-case
              (swank-rpc::simple-read)
              (error (#+ignore c)
               #+ignore
               (cerror "Ignore" 'reader-error
                      :stream *standard-input*
                      #+mcl :format-string #-mcl :format-control
                      (format nil "Invalid swank DSPEC (~A)~&~S" c dspec))
                #+ignore
               `(:reader-error ,dspec ,c)
                dspec)))))))))

(defun make-remote-definition-from-spec (spec &optional package)
  "Create a remote-definition from a list ((DSPEC LOCATION) ...) for NAME's definitions.
DSPEC is a \"definition specifier\" describing the
definition, e.g., FOO or (METHOD FOO (STRING NUMBER)) or
\(DEFVAR FOO). LOCATION is the source location for the definition." 
  (make-instance 'remote-definition 
    :spec (cons (normalize-dspec (first spec) package) (rest spec))
    :package package))

(defun symbol->definition (symbol &key (if-does-not-exist :error) &aux (notfound '#.(cons nil nil)))
  "Get the remote definition for a remote symbol if it is proxied"
  (check-type symbol remote-symbol)
  (let ((def (get symbol 'definition notfound)))
    (when (eq def notfound)
      (ecase if-does-not-exist
        (:error
         (error "Expected a definition in the proxy for ~A" symbol))
        ((nil)
                  (return-from symbol->definition))))
    (values def t)))

(defun deconstruct-location (definition)
  ;; ## maybe use the official deconstruct instead? or eliminate as it is only used maybe once?
  "Deconstructs the location part of a definition spec"
  (let* (;(type (caar definition))
         ;(name (second (car definition)))
         (location (cdr (assoc :location definition)))
         (file (second (assoc :file location)))
         (pos (second (assoc :position location))))
    (values
        #+ignore
     (etypecase type 
       (string (make-remote-symbol type))
       (symbol type))
        #+ignore
        (etypecase name
        (string name)
        (cons 
         ; e.g. Clozure provide ("ARRAY" ":SIMPLE-INTERSECTION") as the name of a caller to mapcar
         ; ad-hoc, but it is not clear what a cons for the name really means:
             (write-to-string name)))
     file 
     pos)))

(defmethod remote-definition-source ((definition remote-definition))
  "The filename and position of the source"
  (multiple-value-bind (file pos)
                       (deconstruct-location (remote-definition-spec definition))
     (values file pos)))

(defmethod remote-definition-type ((definition remote-definition))
  "The type of the remote definition, as a remote symbol"
  (let ((dspec (car (remote-definition-spec definition))))
    (typecase dspec
      (string 
       nil)
      (list
       (let ((type (car dspec)))
         (etypecase type 
           (string (make-remote-symbol type))
           (symbol type)))))))

(defmethod remote-definition-name ((definition remote-definition))
  "A remote symbol naming the defined object and used as unique identifier for this representation"
  ; Examples of specs:
  ; ("IDE.FORM::FINDER-FUNCTION-IN-BIL-FILE" (:error "Unknown source location for IDE.FORM::FINDER-FUNCTION-IN-BIL-FILE"))
  (flet ((name-symbol-from-dspec (dspec)
           "A remote symbol from the name part of the dspec"
           (let* ((name (typecase dspec
                         (string
                          dspec)
                         (list
                          (let ((name (second dspec)))
                            (typecase name
                              (string 
                               name)
                              (cons
                               (cond
                                ((equalp ":internal" (first name))
                                 (let ((name (second name)))
                                   (typecase name
                                     (string name)
                                     (t (princ-to-string (second name))))))
                                ((equalp "setf" (first name))
                                 (second name))
                                (t ; graceful
                                 (princ-to-string name)
                                 #+ignore
                                 (error "Unrecognized remote definition name specification ~A" name))))
                              (t ; graceful
                               (princ-to-string name)))))))
                  (package (remote-definition-package definition)) 
                  (sym (make-remote-symbol name package)))
             (setf (get sym 'definition) definition) ; = put 
             sym)))
    (with-slots (name) definition
      (or name
          (setf name
                (name-symbol-from-dspec (remote-definition-dspec definition)))))))

(defmethod remote-definition-dspec ((def remote-definition))
  (car (remote-definition-spec def)))

(defmethod remote-definition-qualifiers ((def remote-definition))
  (let ((dspec (remote-definition-dspec def)))
    (typecase dspec
      (cons
       (case (car dspec)
         ((or cl::method cl::reader-method)
          (butlast (cddr dspec)))))
      (string NIL))))

(defmethod remote-definition-specializer ((def remote-definition)) ;; ## should be plural?
  (let ((dspec (remote-definition-dspec def)))
    (typecase dspec
      (cons
       (case (car dspec)
         ((or cl::method cl::reader-method)
          (first (last dspec)))))
      (string NIL))))

(defmethod remote-definition-setf-p ((def remote-definition))
   (let ((dspec (car (remote-definition-spec def))))
     (and
      (consp dspec)
      (listp (cadr dspec))
      (equalp "setf" (caadar dspec)))))

#+ignore ;; eliminated
(defun deconstruct-definitions (defs)
  (loop for d in defs
        collect (multiple-value-bind (type name file pos)
                                     (deconstruct-location d) ; should be definition-info instead?
                  (declare (ignore name pos))
                  (cons type file))))

(defparameter %defintion-types%  ;; fallback until swank always implements #'definition-types
  '(; Clozure
    (METHOD-COMBINATION-EVALUATOR NIL)
    (PPC-LAP NIL)
    (PPC32-VINSN NIL)
    (PACKAGE NIL)
    (CONSTANT (VARIABLE))
    (VARIABLE NIL :VARIABLE)
    (METHOD-COMBINATION NIL)
    (DEFINITION-TYPE NIL)
    (STRUCTURE NIL)
    (CONDITION (CLASS))
    (CLASS NIL :CLASS)
    (TYPE NIL)
    (STRUCTURE-ACCESSOR (FUNCTION))
    (CALLBACK (FUNCTION))
    (WRITER-METHOD (METHOD))
    (READER-METHOD (METHOD))
    (METHOD NIL :METHOD)
    (GENERIC-FUNCTION (FUNCTION))
    (SETF-EXPANDER (MACRO))
    (SYMBOL-MACRO (MACRO))
    (COMPILER-MACRO (MACRO))
    (MACRO (FUNCTION) :MACRO)
    (FUNCTION NIL :FUNCTION)
    (T NIL)
    ; SBCL
    (DEFVAR NIL :VARIABLE)
    (DEFCONSTANT (DEFVAR))
    (DEFTYPE NIL)
    (DEFINE-SYMBOL-MACRO NIL)
    (DEFMACRO (DEFUN) :MACRO)
    (DEFINE-COMPILER-MACRO (DEFMACRO))
    (DEFUN NIL :FUNCTION)
    (DEFGENERIC (DEFUN))
    (DEFMETHOD NIL :METHOD)
    (DEFINE-SETF-EXPANDER NIL)
    (DEFSTRUCT NIL)
    (DEFINE-CONDITION (DEFCLASS))
    (DEFCLASS NIL :CLASS)
    (DEFINE-METHOD-COMBINATION NIL)
    (DEFPACKAGE NIL)
    (:DEFTRANSFORM NIL)
    (:DEFOPTIMIZER NIL)
    (:DEFINE-VOP NIL)
    (:DEFINE-SOURCE-TRANSFORM NIL)
    ; LispWorks
    ;(CAPI:DEFINE-INTERFACE NIL)
    ;(CAPI:DEFINE-LAYOUT NIL)
    ;(CAPI-INTERNALS:DEFINE-ELEMENT NIL)
    ;(CAPI-INTERNALS:DEFINE-AUTO-MENU NIL)
    ;(CAPI:DEFINE-MENU NIL)
    ;(EDITOR:DEFINE-TOP-LEVEL-FORM-PARSER NIL)
    ;(EDITOR:DEFCOMMAND NIL)
    (DEFSYSTEM NIL)
    ;(EXTERNAL-FORMAT::EF-FUNCTION NIL)
    ;(EXTERNAL-FORMAT:DEFINE-EXTERNAL-FORMAT-SYNONYM NIL)
    ;(EXTERNAL-FORMAT:DEFINE-EXTERNAL-FORMAT NIL)
    ;(FLI:DEFINE-FOREIGN-CALLABLE NIL)
    ;(FLI:DEFINE-FOREIGN-VARIABLE NIL)
    ;(FLI:DEFINE-FOREIGN-FUNCTION NIL)
    ;(FLI:DEFINE-FOREIGN-TYPE NIL)
    ;(FLI::FOREIGN-SYMBOL NIL)
    ;(FLI::DEFRAW-IMMEDIATE-FOREIGN-TYPE NIL)
    ;(SCM:DEFINE-PATCH-SEQUENCE NIL)
    ;(COMPILER::ADVICE-DEFINITION NIL)
    ;(COMPILER::ADVISED-FUNCTION-NAME NIL)
    ;(SYSTEM:DEFASM NIL)
    (METHOD-COMBINATION NIL)
    (METHOD NIL :METHOD)
    (DEFGENERIC (DEFUN))
    (DEFINE-CONDITION (DEFCLASS))
    (DEFCLASS NIL :CLASS)
    (DEFRESOURCE NIL)
    (PACKAGE NIL)
    (DEFSETF NIL)
    (STRUCTURE NIL)
    (STRUCTURE-CLASS NIL)
    ;(TYPE:TYPE-PREDICATE NIL)
    (DEFTYPE NIL)
    (TYPE NIL)
    (COMPILER-MACRO (DEFMACRO))
    (DEFMACRO (DEFUN) :MACRO)
    (DEFUN NIL :FUNCTION)
    (FUNCTION NIL)
    (DEFINE-SYMBOL-MACRO NIL)
    (DEFCONSTANT (DEFVAR))
    (DEFVAR NIL :VARIABLE)
    (VARIABLE NIL)
    ;(DSPEC:DEFINE-FORM-PARSER NIL)
    ;(DSPEC:DEFINE-DSPEC-ALIAS NIL)
    ;(DSPEC:DEFINE-DSPEC-CLASS NIL)
    ; CLOJURE:
    (DEFN NIL :function)
    (DEF NIL :variable)
    ; default:
    (variable nil :variable)
    (constant (variable))
    (function nil :function)
    (macro (function) :macro)
    (ccl::macro (function) :macro)
    (generic (function))
    (method nil :method)
    (class nil :class)
    (condition (class))
    (T NIL)))

; (mapcar #'canonical-definition-types ccl::*apropos-types*)

(defun canonical-definition-types (type)
  "A list with the most specific canonical type first"
  ;; note the lax matching - for now...
  (check-type type symbol)
  (let ((spec (assoc type %defintion-types% :test #'string-equal)))
    (append
     (cddr spec)
     (loop for super in (second spec)
           when (canonical-definition-types super)
           append it))))

; (canonical-definition-types 'defmacro)
; (canonical-definition-types 'macro)

(defmethod remote-definition-typep (definition canonical)
  ; a future version may allow other types than canonical ones
  (check-type canonical keyword)
  (let ((rtype (remote-definition-type definition)))
    ; (unless rtype (inspect definition))
    (when rtype
      (member canonical (canonical-definition-types rtype)))))

(define-swank-function mclide-xref>lisp (xref)
  "modelled after xref>elisp but retains the list structure"
  ;; only needed as used when finding definitions
  (:common-lisp
   (when xref
     (destructuring-bind (dspec loc) xref
       (labels ((stringify (d)
                  (typecase d
                    (list
                     (mapcar #'stringify d))
                    #+ignore
                    (keyword 
                     d)
                    (otherwise
                     (to-string d)))))
         (list 
          (cons
           (if (or (eq (symbol-package (car dspec)) 
                       (find-package :common-lisp))
                   (keywordp (car dspec)))
             (car dspec)
             (to-string (car dspec)))
           (stringify (cdr dspec)))
          loc))))))

(define-swank-function mclide-find-definitions (symbol-string)
  "Finds all definitions for the named symbol, like find-definitions-for-emacs"
  (:common-lisp ; # can be eliminated with the default...
   (find-definitions-for-emacs symbol-string))
  (:clojure #:|
    (in-ns 'swank.commands.basic) 
    (swank.commands/defslimefn mclide-find-definitions [name] 
      (try 
        (swank.commands.basic/find-definitions-for-emacs name)
        (catch java.lang.NullPointerException e ; "no message"
          `((~name (:error ~(str e)))))))
    nil |)
  (T '#:|swank:find-definitions-for-emacs|))

(define-swank-function mclide-xref-definitions (name type)
  "Finds the definitions for the xrefs of the named symbol, like the xref slimefun"
  (:common-lisp
   (check-type type keyword) ;; :specializes etc
   (assert (not (eql type :function))) ; sanity check!          
   (#:|swank::xref| type name))
  (:clojure
    #:|(in-ns 'swank.commands.basic)

       (defslimefn mclide-xref-definitions [name type]
          (xref type name))

        nil |))

(defun remote-definition-list (specs type package)
  "Makes remote definitions for specs matching the requested type"
  (etypecase specs
    (null nil)
    (list
     (flet ((remote-definition-using-package (spec)
              (make-remote-definition-from-spec spec package)))
       (let ((defs (mapcar #'remote-definition-using-package specs)))
         (etypecase type
           ((eql t)
            defs)
           (keyword
            defs)
           (symbol 
            (let ((canonical (first (canonical-definition-types type))))
              (if canonical
                (remove-if-not  
                 (lambda (def)
                   (remote-definition-typep def canonical))
                 defs)
                defs)))))))
    ((eql :not-implemented) 
     NIL)))

(defun remote-definitions-form (thing type)
  "Form to evaluate in Swank to produce a list of definitions"
  ; Overloaded to provide definitions both for xrefs and symbol:
  ;   For xrefs, type is :calls etc.
  ;   For symbol definitions, type is 'function, 'variable etc (using the values from MCL)
  ; # Reconsider this overloading, its messy - split out the xref variation? Move filtering to mclide module?
  ; Use the slimefun #'find-definitions-for-emacs even if it returns dspecs as strings?
 (let ((name (string-trim ccl::wsp&cr (string thing))))
  (unless (equal name "")
    (let ((package (remote-reader-package thing)))
      `(:post-processing
        ,(lambda (specs)
           (remote-definition-list specs type package))
        :form ,(etypecase type
                 (keyword
                  (assert (not (eql type :function))) ; sanity check!
                  (mclide-xref-definitions name type))
                 ((or symbol (eql t))
                  (mclide-find-definitions name)))
        :package ,package)))))

(define-swank-function mclide-xrefs-exists-p (name type)
  (:common-lisp
    (declare (ftype (function (string keyword) t) mclide-xref-definitions))
    (when (ignore-errors (mclide-xref-definitions name type))
       t))
  (:clojure #:|
    (in-ns 'swank.commands.basic)
    (declare mclide-xref-definitions)   
    (swank.commands/defslimefn mclide-xrefs-exists-p [name type]
      (when (seq (mclide-xref-definitions name type))
         true))
    nil  |))

(defun if-xref-definitions-form (thing type)
   (assert (remote-symbol-p thing))
  (check-type type keyword)
  (query-form (mclide-xrefs-exists-p (string thing) type)
                       :package (remote-reader-package thing)))

;;;;;;

(defmethod remote-definition-error ((def remote-definition))
   (let ((spec (remote-definition-spec def)))
     (second (assoc :error (cdr spec)))))

(defun extract-remote-definition (object &optional name)
  "Returns a remote definition object from the object, typically a representation in dialogs tables"
  ; Allows various representations of a definition for flexibility in implementation.
  (etypecase object
    (cons ; like original 
     (typecase (cdr object)
       (remote-definition
        (cdr object))
       (otherwise
        (symbol->definition name))))
    (symbol
     (symbol->definition object))
    (remote-definition
     object)))

(defun definition-info (def)
  "Return the essential values of a definition in a normalized form"
  (let ((type (remote-definition-type def))
        (name (remote-definition-name def)))
    (multiple-value-bind (file pos)
                         (remote-definition-source def)
      (check-type type symbol)
      (check-type name remote-symbol)
      (check-type file (or null string))
      (check-type pos (or null integer))
      (values type name file pos))))

(defun extract-definition-info (object &optional name)
  "Returns (values type name pathname pos) from the object, typically a representation in dialogs tables"
  ; Allows various representations of a definition for flexibility in implementation.
  (let ((def (extract-remote-definition object name)))
    (multiple-value-bind (type name2 file pos)
                         (definition-info def)
      (let ((name (or name name2))) 
        (check-type type symbol)
        (check-type name remote-symbol)
        (values type name file pos)))))

(defgeneric foreign-object-definition (object)
   (:documentation "The remote definition of the foreign object, whether cached or taken from server"))

(defmethod foreign-object-definition ((name symbol))
   (check-type name remote-symbol)
   (symbol->definition name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VIEW FILES IN A ZIP JAR

#|
(swank.commands.basic/find-definitions-for-emacs "swank.commands.basic/find-definitions-for-emacs")
=>
(("(defn find-definitions-for-emacs)"
 (:location (:file "/Users/terje/Applications/clojure-1.1.0/swank-clojure/src/swank/commands/basic.clj")
            (:line 321) nil)))

(swank.commands.basic/find-definitions-for-emacs "find-doc")
=>
(("(defn find-doc)"
 (:location (:zip "/Users/terje/Applications/clojure-1.1.0/clojure.jar" "clojure/core.clj")
            (:line 3249) nil)))

|#

(in-package :remote)

(defclass source-location ()
  ((position :reader source-location-position :initarg :position :type list :initform NIL)
   (hints :initarg :hints :initform nil))
  (:documentation "Abstract class for a definition source location"))

(defclass file-source-location (source-location)
  ((file :reader source-location-file :initarg :file)))

(defclass zip-source-location (source-location)
  ((file :reader source-location-file :initarg :file)
   (zip :reader source-location-zip-entry :initarg :entry)))

(defun extract-source-location (location-spec)
  "Extracts a source location object from a location spec, if available"
  #| See the Slime implementation (slime-goto-source-location).
     Several kinds of locations are supported: 

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>) 
             | (:method <name string> <specializer strings> . <qualifiers strings>)
  |#
  (destructure-case location-spec
    ((:location buffer position hints)
     (destructure-case buffer
      ((:file file)
       (make-instance 'file-source-location :file file :position position :hints hints))
      ((:buffer buffer-name)
       )
      ((:source-form string)
       )
      ((:zip file entry)
       (make-instance 'zip-source-location :file file :entry entry :position position :hints hints))))
    ((:error message)
     (warn message)
     NIL)))

; # (can this replace remote-definition-source?)

(defmethod remote-definition-location ((definition remote-definition))
  "A location object representing where to find the definition source."
  (let* ((spec (remote-definition-spec definition))
         (location-spec (assoc :location (cdr spec))))
    (when location-spec
      (extract-source-location location-spec))))

; (extract-source-location '((:ZIP "/Users/terje/Applications/clojure-1.1.0/clojure.jar" "clojure/core.clj") (:LINE 3249) NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPERATOR ARGLIST

(in-package :remote)

(defun remote-symbol-arglist-form (symbol &optional default-package)
  (etypecase symbol
    (string
     (operator-arglist-form symbol default-package))
    (remote-symbol
     (operator-arglist-form 
      (remote-symbol-name symbol)
      (or (remote-symbol-package symbol) default-package)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOCUMENTATION

(in-package :remote)

#+ignore
(define-swank-function mclide-symbol-documentation (symbol type)
  "use instead of mclide-documentation-symbol when the type is known (combine?)"
  (:common-lisp
   (cl:documentation symbol type)))

;; ## temporary, fix in the mclide module instead! 
(defun swank-apropos-type-from-mcl (type)
  "Ad hoc to convert from mcl apropos type to swank"
  ;; Compare to #'canonical-definition-types - very similar!
  (if (keywordp type)
    type
    (ecase type
      (cl:function :function)
      (cl:variable :variable)
      (cl:class :class)
      (ccl::macro :macro)
      ((eql T) T))))

(define-swank-function mclide-describe-symbol (symbol-string)
  "Like describe-symbol-for-emacs but takes a string argument"
  ; extracts the symbol using the buffer package!
  (:common-lisp
     (describe-symbol-for-emacs ; not a slimefun! 
      (from-string symbol-string)))
  ; (:gambit)
  (:clojure ; incomplete!!! derived from briefly-describe-symbol-for-emacs. with-emacs-package still needed?
    #:|
    (in-ns 'swank.commands.basic)
    (swank.commands/defslimefn mclide-describe-symbol [name]
     "Describe the symbol as a property list"
     (with-emacs-package
      (try
       (let [var (ns-resolve (maybe-ns *current-package*) (symbol name))
             lines (fn [s] (seq (.split #^String s (System/getProperty "line.separator"))))
             [_ symbol-name arglists d1 d2 & __] (lines (describe-to-string var))
             macro? (= d1 "Macro")]
            (list :designator symbol-name
                  (cond
                   macro? :macro
                   (:arglists (meta var)) :function
                   :else :variable)
                  (apply str (concat arglists (if macro? d2 d1)))))
       (catch java.lang.NullPointerException e ; "no message"
          nil))))
      nil |))

(defun remote-symbol-documentation-form (remote-symbol type)
  "A form that when evaluated on swank returns a plist describing the symbol"
  (setf type (swank-apropos-type-from-mcl type)) ;; ## should be handled in mclide module
  (etypecase type
    ((eql T)
     (query-form (mclide-describe-symbol (string remote-symbol))
                          :package (remote-reader-package remote-symbol)))
    (keyword ; ## but is this correct???
     (query-form (mclide-describe-symbol (string remote-symbol))
                          :post-processing (lambda (result) (list type (getf result type)))
                          :package (remote-reader-package remote-symbol)))))

(defun map-documentation (fun doc-plist) 
   "Map a function over the type and docstring in a symbol documentation from the remote swank:describe-symbol-for-emacs, collecting the result in a list"
  (loop
    while doc-plist
    for type = (pop doc-plist)
    for docstring = (pop doc-plist)
    unless (eq :not-documented docstring) ; lispworks per february 2010
    collect (funcall fun type docstring)))

#+ignore
(define-swank-function mclide-symbol-documentation-p (symbol type)
  "Determines whether documentation exists for the symbol"
  ; not much saved by using this - consider to consolidate!
  (:common-lisp
   (when (cl:documentation symbol type) type)))

(defun if-remote-symbol-documentation-form (thing type)
  ; keep in case it can be optimized!
  (remote-symbol-documentation-form thing type))

#+ignore
(define-swank-function mclide-documentation-symbol (symbol)
  "Like swank:documentation-symbol but more descriptive" ;  suggest as replacement?
  (:common-lisp
   (list :function (cl:documentation symbol '#:|cl:function|)
         :variable (cl:documentation symbol '#:|cl:variable|))))

#+ignore
(defun remote-documentation-symbol-form (remote-symbol)
  (mclide-documentation-symbol (remote-sexp-form remote-symbol)))


#+ignore
(defun remote-documentation-symbol-form (remote-symbol)
   "property list documenting the definitions of the symbol"
   ;; Like swank:documentation-symbol but more descriptive (suggest as replacement?)
   ;; ## add a written representation of symbol as first item for use in output?
   `(let ((#:sym ,(remote-sexp-form remote-symbol)))
      `(:function ,(cl:documentation #:sym 'cl:function)
        :variable ,(cl:documentation #:sym 'cl:variable))))

;; consider eliminating...
#+ignore
(defun remote-symbol-documentation (remote-symbol &optional (type 'cl:function))
   (oneline (remote-eval (remote-symbol-documentation-form remote-symbol type))))  
     
; (swank::describe-definition 'cl:documentation :function)
; (remote-symbol-documentation 'cl:documentation)
; (remote-symbol-documentation (make-remote-symbol "DOCUMENTATION" "CL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATHNAMES

(define-swank-function mclide-translate-pathname (device host directory name type version)
  (:common-lisp
   (let* ((pathname (cl:make-pathname 
                     :device device 
                     :host host 
                     :directory directory 
                     :name name 
                     :type type 
                     :version version))
          (path (if host (cl:translate-logical-pathname pathname) pathname)))
      (list
       :device (cl:pathname-device path)
       :host NIL  ; note that SBCL provides value for cl:pathname-host
       :directory (cl:pathname-directory path)
       :name (cl:pathname-name path)
       :type (cl:pathname-type path)
       :version (cl:pathname-version path))))
  (:gambit #:| ;; ## does not yet work properly! 
    (let ()
      (define (swank:mclide-translate-pathname device host directory name type version)
        (list :device device :host 'nil :directory directory :name name :type type))
      (table-set! swank-op-table 'swank:mmclide-translate-pathname swank:mclide-translate-pathname)) |)
  (:clojure #:|
     (in-ns 'swank.commands.basic)  
     (swank.commands/defslimefn mclide-translate-pathname [device host directory name type version]
        (list :device device :host 'nil :directory directory :name name :type type))
     nil |))

(defun remote-translate-logical-pathname-form (pathname)
  "A form to translate the pathname using the logical hosts of the remote lisp, returning an argument list suited for make-pathname"
  (mclide-translate-pathname 
   `',(cl:pathname-device pathname)
   `',(unless (eq :unspecific (cl:pathname-host pathname))
       ; unspecific is not allowed by SBCL (is it standard?)
       (cl:pathname-host pathname))
   `',(cl:pathname-directory pathname)
   `',(cl:pathname-name pathname)
   `',(cl:pathname-type pathname)
   `',(cl:pathname-version pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-as-string (pathname)
  (with-open-file (in pathname :if-does-not-exist nil)
    (when in
      (with-output-to-string (out)
        (loop for line = (read-line in nil)
          while line
          do (write-line line out))))))

(defun remote-load (repl pathname)
  "Load a file into a remote lisp based on a local pathname"
  (remote-repl-call repl (file-as-string pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APROPOS

(in-package :remote)

(define-swank-function mclide-apropos (op str1 str2 package type external)
  "Apropos on symbols matching two optional non-empty strings (similar to apropos-list-for-emacs)"
  (:common-lisp
   (when (or str1 str2)
     (let ((result))
       (labels ((test1 (name &optional default)
                      (if str1 (search str1 name :test #'char-equal) default))
                    (test2 (name &optional default) 
                       (if str2 (search str2 name :test #'char-equal) default))
                    (consider (sym)
                       (when (and
                                   (if external (symbol-external-p sym) t) 
                                   (case type
                                      (:function (fboundp sym))
                                      (:variable (boundp sym))
                                      (:macro (macro-function sym))
                                      (:class (find-class sym nil))
                                      (t t)))
                           (let ((name (symbol-name sym)))
                             (when (ecase op
                                          (:and (and (test1 name t) (test2 name t)))
                                          (:or (or (test1 name) (test2 name)))
                                          (:not (and (test1 name t) (not (test2 name)))))
                                 (if package
                                   (push name result)
                                   (push (prin1-to-string sym) result)))))))
         (if package
           (do-symbols (sym package result)
             (consider sym))
           (do-all-symbols (sym result)
             (consider sym)))))))

  (:clojure ;; ## can likely be speeded up considerably!
   ; uses public interface:
   ;#:|(swank.commands/defslimefn mclide-apropos [op str1 str2 package type external]
   ;     (map second (swank.commands.basic/apropos-list-for-emacs str1 external false package))) | 
   #:| (in-ns 'swank.commands.basic)
        (require 'clojure.set)
       (defn- mclide-apropos-symbols [string external-only? case-sensitive? package]
         (let [packages (or (when package [package]) (all-ns))
               matcher (make-apropos-matcher string case-sensitive?)
               lister (if external-only? ns-publics ns-map)]
           (filter matcher
              (apply concat (map (comp (partial map second) lister)
                                 packages)))))
       (swank.commands/defslimefn mclide-apropos [op str1 str2 package type external]
         (let [package (when package
                         (or (find-ns (symbol package))
                           'user))
               set1 (if str1 (set (mclide-apropos-symbols str1 external false package)))
               set2 (if str2 (set (mclide-apropos-symbols str2 external false package))) ]
          (map #(cond
                  (class? %1) (pr-str %1) 
                  :else (let [m (meta %1)] 
                          (str (or (if m (:name m)) %1))))
            (filter (condp = type
                       :function #(:arglists (meta %1))
                       :variable #(var? %1)
                       :macro #(:macro (meta %1))
                       :class #(class? %1)
                       #'identity) 
             (if (or (= str1 nil) (= str2 nil))
               (or set1 set2)
               (condp = op
                 :or (clojure.set/union set1 set2)
                 :and (clojure.set/intersection set1 set2)
                 :not (clojure.set/difference set1 set2)
                 nil))))))
        nil |)

  #+ignore ;; for clojure 1.2, fails in 1.3 due to missing (require 'clojure.set)
  (:clojure ;; ## can likely be speeded up considerably!
   ; uses public interface:
   ;#:|(swank.commands/defslimefn mclide-apropos [op str1 str2 package type external]
   ;     (map second (swank.commands.basic/apropos-list-for-emacs str1 external false package))) | 
   #:| (in-ns 'swank.commands.basic)
       (defn- mclide-apropos-symbols [string external-only? case-sensitive? package]
         (let [packages (or (when package [package]) (all-ns))
               matcher (make-apropos-matcher string case-sensitive?)
               lister (if external-only? ns-publics ns-map)]
           (filter matcher
              (apply concat (map (comp (partial map second) lister)
                                 packages)))))
       (swank.commands/defslimefn mclide-apropos [op str1 str2 package type external]
         (let [package (when package
                         (or (find-ns (symbol package))
                           'user))
               set1 (if str1 (set (mclide-apropos-symbols str1 external false package)))
               set2 (if str2 (set (mclide-apropos-symbols str2 external false package))) ]
          (map #(cond
                  (class? %1) (pr-str %1) 
                  :else (let [m (meta %1)] 
                          (str (or (if m (:name m)) %1))))
            (filter (condp = type
                       :function #(:arglists (meta %1))
                       :variable #(var? %1)
                       :macro #(:macro (meta %1))
                       :class #(class? %1)
                       #'identity) 
             (if (or (= str1 nil) (= str2 nil))
               (or set1 set2)
               (condp = op
                 :or (clojure.set/union set1 set2)
                 :and (clojure.set/intersection set1 set2)
                 :not (clojure.set/difference set1 set2)
                 nil))))))
        nil |))

(defun remote-apropos-form (op str1 str2 &key package type external)
  (query-form 
   (mclide-apropos op
                   (when (find-if-not #'whitespacep str1) str1)
                    (when (find-if-not #'whitespacep str2) str2)
                    package 
                    (swank-apropos-type-from-mcl type)
                    external)
    :post-processing
    (lambda (result)
      (remote-symbols result package))))
 
; (pprint (remote-apropos-form :and "a" "b" :package "ccl" :type 'function))
; (pprint (remote-apropos-form :or "a" "b" :package "ccl" :type 'function))
; (pprint (remote-apropos-form :not "a" "b" :package nil :type 'function :external T))
; (pprint (remote-apropos-form :and "" "" :package "ccl" :type 'function))

#|
(let ((*remote-lisp-mode* T))
  (remote-initargs-form (make-remote-sexp "window" "ccl" (remote-lisp))))
|#

;; also see .getConstructors .getFields of a class!
;; (count (.getFields (class {}))) => 1 !!!
;; See swank.completions.clj for how to get classes!

(define-swank-function mclide-class-initargs (symbol-string)
  "Return a list of initargs for the class SYMBOL"
  ;; ## Consider changing this to instead present the class!
  (:common-lisp
   (ignore-errors 
     (let* ((s (#:|swank::from-string| symbol-string))
            (class (when s (cl:find-class s nil))))
       #+ignore ;; ## only works for clz!!!
       (when (typep class 'ccl::std-class)
         (ccl::class-make-instance-initargs class))
       ; See #'extra-keywords/slots in swank-arglist.lisp
       (when class
         (mapcan 
          (lambda (slot)
            (#:|swank-mop::slot-definition-initargs| slot))
          (if (#:|swank-mop::class-finalized-p| class)
            (#:|swank-mop::class-slots| class)
            (#:|swank-mop::class-direct-slots| class)))))))
  (:clojure #:|
    (in-ns 'swank.commands.basic)   
    (swank.commands/defslimefn mclide-class-initargs [name]
       (let [c (ns-resolve (maybe-ns *current-package*) (symbol name)) ] 
         (when (class? c)
           (map #(.getName %1)
                 (concat (.getFields c) (.getConstructors c)))))) 
     nil |))

(defun remote-initargs-form (symbol)
  (query-form (mclide-class-initargs (string symbol))
                       :package (remote-reader-package symbol)))

#+ignore
(defun remote-initargs-form (symbol)
  `(ignore-errors 
    (let* ((#:s ,(remote-sexp-form symbol))
           (#:class (when #:s (cl:find-class #:s nil))))
      #+ignore ;; ## only works for clz!!!
      (when (typep #:class 'ccl::std-class)
         (ccl::class-make-instance-initargs #:class))
      ; See #'extra-keywords/slots in swank-arglist.lisp
      (when #:class
        (mapcan 
         (lambda (#:slot)
           (,(make-remote-symbol "slot-definition-initargs" "swank-mop") #:slot))
         (if (,(make-remote-symbol "class-finalized-p" "swank-mop") #:class)
           (,(make-remote-symbol "class-slots" "swank-mop") #:class)
           (,(make-remote-symbol "class-direct-slots" "swank-mop") #:class)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRACE

(in-package :remote)

(declare-slimefun swank-toggle-trace (spec-string))

(define-swank-function mclide-toggle-trace (spec-string)
  (:common-lisp
   (ignore-errors ; don't err on undefined symbols
    (swank-toggle-trace spec-string)))
  (T '#:|swank:swank-toggle-trace|))

(defun decode-tracing-response (status)
  (cond
   ((search " untraced" status) :untrace)
   ((search " traced" status) :trace)
   (T :undefined)))

(defun swank-toggle-trace-form (spec-string package-name)
  (query-form (mclide-toggle-trace spec-string)
                       :post-processing #'decode-tracing-response                        
                       :package package-name))

(defun remote-trace (spec-string package-name &key callback)
  (remote-eval-bind (status)
                    (swank-toggle-trace-form spec-string package-name)
    (when (eq status :untrace)
      (remote-eval (swank-toggle-trace-form spec-string package-name)))   
    (when callback
      (funcall callback status))))

(defun remote-untrace (spec-string package-name &key callback)
  (remote-eval-bind (status)
                    (swank-toggle-trace-form spec-string package-name)
    (when (eq status :trace)
      (remote-eval (swank-toggle-trace-form spec-string package-name)))
    (when callback
      (funcall callback status))))

(define-swank-function mclide-traced ()
  "Generate a list of traced functions"
  (:common-lisp
   (cl:mapcar 'cl:prin1-to-string (cl:trace)))
  (:clojure
   #:|
     (in-ns 'swank.commands.basic)
      (swank.commands/defslimefn mclide-traced []
        (seq (map #(str (.name (.ns %1)) "/" (.sym %1)) ; better way?
                  (keys traced-fn-map))))
      nil
     |))

(defun remote-traced ()
  "List of remotely traced symbols (as strings)"
  (remote-eval (mclide-traced)))

(defun remote-untrace-all ()
  (remote-send (untrace-all-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKTRACE

(in-package :remote)

(defclass remote-backtrace ()
  ((repl :initarg :repl :reader backtrace-repl)
   (thread :initarg :thread :reader backtrace-thread)
   (all-frames 
    :reader all-backtrace-frames
    :initarg :frames
    :type list
    :documentation "all frames in swank's raw encoding")
   (condition :initarg :condition :reader backtrace-condition))
  (:documentation "Represents a backtrace in a remote lisp"))

(defun make-remote-backtrace (&rest rest)
   "A remote backtrace from frames in swank's encoding"
  (apply #'make-instance 'remote-backtrace rest))

;; ever used??
(defmethod backtrace-locals ((bt remote-backtrace) index)
  "A property list ((&key NAME ID VALUE) ...) describing the local variables in the frame INDEX."
  (with-slots (thread) bt
    (swank-client::swank-remote-call thread ;; can use remote-call instead! 
     (format nil "(cl:ignore-errors (apply #'cl:values (frame-locals-for-emacs ~A)))" index))))

;; # Should this contain or be independent of a backtrace?

(defclass remote-frame ()
  ((remote :reader frame-remote :initarg :remote :initform NIL)
   (frame-thread :reader frame-thread :initarg :frame-thread :type integer)
   (frame-number :reader frame-number :initarg :frame-number :type integer)))

(defmethod backtrace-remote-frame ((backtrace remote-backtrace) frame-number &optional (class 'remote-frame) &rest initargs)
  (apply #'make-instance class 
    :frame-number frame-number
    :frame-thread (remote::backtrace-thread backtrace)
    :remote (remote::backtrace-repl backtrace)
    initargs))

(defmethod frame-locals-form ((frame remote-frame))
  (let ((thread (remote::frame-thread frame))
        (frame-number (remote::frame-number frame)))
    (frame-locals-and-catch-tags-form frame-number thread)))

(defclass remote-frame-var ()
  ((frame :initarg :frame :type remote-frame)
   (index :initarg :index :type integer)))

(defmethod frame-var-inspected-object-form ((frame-var remote-frame-var))
   ;; deprecated - better to request swank to start an inspector
  (with-slots (frame index) frame-var
    (let* ((thread (frame-thread frame))
           (frame-number (frame-number frame))
           (session (make-instance 'remote-inspection-session
                      :thread thread
                      :connection nil)))
      (query-form 
       (inspect-frame-var-form frame-number index thread)
       :post-processing (lambda (object)
                          (parse-remote-inspected-object object session))
       :remote (frame-remote frame)))))

(defmethod show-frame-var-inspector-form ((frame-var remote-frame-var))
  "Creates query to show an inspector containing the frame var"
    (with-slots (frame index) frame-var
      (let* ((thread (frame-thread frame))
             (frame-number (frame-number frame)))
        (query-form
         (mclide-inspect `'(:frame-var ,frame-number ,index))
         :thread thread
         :remote (frame-remote frame)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES

(in-package :remote)

(defparameter *remote-package-names-proxy* (make-hash-table :test 'eq :weak :key))

(defun get-remote-package-names (name)
  "A list of the canonical name and nicknames of the package"
  (check-type name string)
  (let* ((all (gethash (remote-lisp) *remote-package-names-proxy*))
         (names (assoc name all :test #'string=)))
    (assert names () "Package ~A is not in the proxy" name)
    (copy-list names)))

(define-swank-function mclide-list-all-package-names ()
  ;; swank:list-all-package-names still collapses nicknames with packages!!
  (:common-lisp
   (cl:mapcar 'package-names (cl:list-all-packages)))
  (:gambit ; incorrect, but then again, gambit doesn't have packages and swank:list-all-package-names isn't defined
   #:|(let ()
        (define (swank:mclide-list-all-package-names)
          '(("Gambit")))
        (table-set! swank-op-table 'swank:mclide-list-all-package-names swank:mclide-list-all-package-names))        
     |)  ; (swank-define-op swank:mclide-list-all-package-names)
  (:clojure #:|
    (in-ns 'swank.commands.basic)  
    (swank.commands/defslimefn mclide-list-all-package-names [] 
        (map list (swank.commands.basic/list-all-package-names)))
    nil 
     |))

(defun remote-package-names (&optional immediate)
  "A list of cached canonical names of all packages in the active remote LISP, updating the cache as side effect"
  ; avoid calling with immediate from the event handler as it may cause hung-ups!
    ;; # consider to cache packages for each application!
  ;; ## change this into a with-construct?
  ;; may need better locking?
  ;; ## consider a refresh time to avoid updating more than once in a while.
  ;; ### This could be much better if swank:list-all-package-names did not collapse nicknames with packages!!
  (let ((owner (remote-lisp)) ;; repl, but better if remote application!
        (form (query-form (mclide-list-all-package-names)
                                      :package :swank)))
    (if immediate
      (mapcar #'car
                (setf (gethash owner *remote-package-names-proxy*)
                      (remote-eval form)))
      (let ((pnames (or (gethash owner *remote-package-names-proxy*)
                        ; proxy should be set in advance, this is a bogus default just in case:
                        (let ((package (thread-package owner)))
                          (if package 
                            `((,package))
                            '(("COMMON-LISP" "CL") ("COMMON-LISP-USER" "CL-USER")))))))
          (prog1
             (mapcar #'car pnames)
             (remote::remote-eval-enqueue-bind (pnames) form
               (without-interrupts ; poor mans lock
                (unless (equal pnames (gethash owner *remote-package-names-proxy*)) 
                   (setf (gethash owner *remote-package-names-proxy*)
                         pnames)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCESSES

(in-package :remote)

(defclass remote-process ()
  ((id :reader remote-process-id :initarg :id)
   (name :reader remote-process-name :initarg :name)
   (state :reader remote-process-whostate :initarg :state)
   (description :reader remote-process-description :initarg :description)
   (priority :reader remote-process-priority :initarg :priority)
   ; (created :reader remote-process-created :initarg :created)
   ;(allocation-quantum)
   ; (runtime :reader remote-process-runtime :initarg :runtime)
   (labels :reader remote-process-labels :initarg :labels)
   (properties :reader remote-process-properties :initarg :properties)))

(defun remote-process-p (p)
  (typep p 'remote::remote-process))

(defun make-remote-process (id name status description &rest rest)
  (apply #'make-instance 'remote-process :id id :name name :state status :description description rest))

(defun nthprop (name names values)
  (let ((n (position name names :test #'string=)))
    (when n
      (nth n values))))

(defun build-remote-processes (table)
  "Builds list of remote processes from the swank data"
  (loop  
    with labels = (car table)
    for p in (cdr table)
    collect 
    (make-remote-process
     (nthprop :id labels p)
     (nthprop :name labels p) 
     (or (nthprop :status labels p)
         (nthprop :state labels p))
     (nthprop :description labels p)
     :priority (nthprop :priority labels p)
     :labels labels
     :properties p)))

(defun remote-processes-form ()
   (query-form
    (list-threads-form)
    :post-processing
    #'build-remote-processes))

(defun remote-processes () 
   (remote-eval (remote-processes-form)))

(defmethod show-process-inspector-form ((p remote-process))
  (let ((id (remote-process-id p)))
    (mclide-inspect (format nil "#.(swank::find-thread ~A)" id)) ))




















