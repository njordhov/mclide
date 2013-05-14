 ;; MCLIDE 2 FOR CLOZURE

#-clozure 
(error "For Clozure CL only")

(defpackage mclide
   (:export #:register-implementation-type))

(in-package :mclide)

;; Same as in autostart from mclide1, so create shared module?

(defparameter *implementation-types* nil)

(defun register-implementation-type (name command &rest args)
  (push (list* name command args) *implementation-types*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ccl)

(advise lisp-implementation-type
            (concatenate 'string "MCLIDE on " (:do-it))
            :when :around :name custom-lisp-implementation-type)

(defmethod application-error :around ((a gui::cocoa-application) condition error-pointer)
  ; avoid that internal errors outside a connection to mclide ends up in the *debugger-hook* with potentially disasterous consequences...
  (declare (ignore error-pointer)
                (special swank::*emacs-connection*))
  (if (and (boundp 'swank::*emacs-connection*)
              swank::*emacs-connection*)
    (call-next-method)
    (let ((swank::*debugger-hook* NIL))
      (gui::log-debug "~%Internal error: ~A" condition)
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOG

(in-package :mclide)

(defparameter *logfile* ;; ## should use findfolder equivalent
   "home:Library;Logs;com.in-progress.mclide;mclide2.log")

(defparameter *log* NIL)

;; ## rename to enable-logging

(defun init-logging ()
   "Return the logging stream after opening it"
   (handler-case
    (progn
     (when *log*
      (unwind-protect
       (close *log*)
       (setf *log* NIL)))
     (setf *log* (open *logfile* :direction :output :if-exists :append :if-does-not-exist :create :sharing :lock)))
    (error (c)
      (#_NSLog #@"\nERROR: Failed to initialize error logging.\n")
      (setf *log* NIL))))

(advise gui::log-debug
    (if *log*
      (progn
         (apply #'format *log* ccl::arglist)
         (force-output *log*))
      (:do-it))
    :when :around :name mclide-log-debug)

(ccl::defcontextual hemlock::editor-error (text &rest args)
  (remote::remote-lisp-mode)
  ; mclide may call it out of the context of a view, which causes failure
  (handler-case
      (ccl::call-next)
    (error ()
      ;; ## should provide a better place for feedback
      (apply #'gui::log-debug text args))))

(advise gui::maybe-start-swank-listener 
             (let* ((log (init-logging))
                     (*error-output* (or log *error-output*)))
               (:do-it))
            :when :around :name start-swank-listener-no-altconsole)

(advise gui::load-and-start-swank
            (let ((*error-output* (or *log* *error-output*))
                   (*standard-output* (or *log* *standard-output*)))
               (:do-it))
             :when :around :name start-swank-no-altconsole)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION

(in-package :gui)

(defun maybe-load-init-file ()
   ; startup-ccl  is usually called from new-cocoa-listener-process, but we need it when the application starts 
   ; yet cannot use *lisp-startup-functions* as these are run before start-cocoa-application.
   (assert *standalone-cocoa-ide* () "Attempted to load init file before cocoa is started")
   (when (prog1 *first-listener* (setq *first-listener* nil))
       (ccl::startup-ccl (ccl::application-init-file ccl::*application*))))

(objc:defmethod (#/ensureListener: :void) ((self lisp-application-delegate)
					   sender)
   (declare (ignore sender))
  (let ((top-listener-document (#/topListener hemlock-listener-document)))
    (when (eql top-listener-document +null-ptr+)
               (easygui::new-remote-lisp-from-user))))

;; avoiding default listener and ad-hoc load init file before anything else:

(objc:defmethod (#/applicationOpenUntitledFile: :<BOOL>)
    ((self lisp-application-delegate) app)
   (maybe-load-init-file)
   (when (zerop *cocoa-listener-count*)
       #+ignore
       (#/ensureListener: self app)
       t))

(eval-when (:load-toplevel)
(flet ((load-initfile-after-startup ()
          ; ensure that the initfile is loaded also when starting the app by dropping a file .
          ; alternatively create a application:openFile: method
          ; see https://developer.apple.com/library/mac/#documentation/cocoa/Reference/NSApplicationDelegate_Protocol/Reference/Reference.html
          (process-run-function "load initfile"
              (lambda ()
                (wait-on-semaphore *cocoa-application-finished-launching*)
                (maybe-load-init-file)
                (gui::log-debug "Loaded Initfile~&")))))
  (push #'load-initfile-after-startup  ccl::*lisp-startup-functions*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mclide)

;; ## should be more discrete...

(defmethod remote::background-message-handler ((repl remote::remote-repl) string)
  (easygui::message-dialog string))

(defmethod edit (pathname &key position line)
   (ed pathname))

(defmethod remote::edit-file-handler ((repl remote::remote-repl) file &key line column position)
  (declare (ignore column))
  ;; ## use a source note to generalize?
  (if file    
      (let ((pathname (probe-file file)))
        (if pathname
          (edit pathname :position position :line line)
          (warn "File does not exist: ~A" file)))
      (ed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD FILE

(in-package "GUI")  ;; mclide

;; ## *.lisp-pathname* should depend on the lisp type.

(ccl::defcontextual listener-load-file ((doc hemlock-listener-document))
  (remote::remote-lisp-mode) ;; ## better test for repl associated with the listener
  (let ((filename (cocoa-choose-file-dialog
		   :button-string "Load"
		   :file-types (list (pathname-type *.lisp-pathname*)
				     (pathname-type *.fasl-pathname*)))))
    (when (and filename (probe-file filename))
      ; inspired by #/evalAll
      (let ((process (hemlock-document-process doc))
             (file-string (remote::file-as-string filename)))
        (eval-in-listener-process process file-string :path filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCH FILES

(in-package :mclide)

(ccl::defcontextual gui::set-search-files-default-dir (wc) T
  (declare (ignore wc))
  (let ((ccl::%logical-host-translations% nil)
        (gui::*search-files-default-path*  ;; ## perhaps this should be the folder of the current app instead?
         (directory-namestring (user-homedir-pathname))))
    (ccl::call-next)))

