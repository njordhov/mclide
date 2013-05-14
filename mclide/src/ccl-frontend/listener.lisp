;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mclide)

(defun top-listener-remote-lisp ()
   "The remote for the top listener, if it is a remote listener"
  (let ((process (gui::hemlock-document-process (#/topListener gui::hemlock-listener-document))))
    (and process (symbol-value-in-process 'remote::*remote-lisp* process))))

(defparameter remote::*auto-remote-lisp-function* #'top-listener-remote-lisp)

;; ## move functionality to remote?

(advise remote::remote-lisp
        (or (:do-it)
            (when remote::*auto-remote-lisp-function*
              (funcall remote::*auto-remote-lisp-function*)))
        :when :around :name 'mclide-remote-lisp)

(defun listener-process (window)
  (when (and window (#/isVisible window))
    (let ((wc (#/windowController window)))
      (when (typep wc 'gui::hemlock-listener-window-controller)
        (let* ((doc (#/document wc)))
          (unless (%null-ptr-p doc)
            (let ((process (gui::hemlock-document-process doc)))
              process)))))))

(defun listener-remote (window)
  "The value of the *remote-lisp* if the window is a remote listener"
  (let ((process (listener-process window)))
    (when process
      (symbol-value-in-process 'remote::*remote-lisp* process))))

(defun listener-document (&optional (listener (process-listener)))
   "The window document of the (current) listener"
  (#/document (#/windowController listener)))

(defun process-listener (&optional (p *current-process*))
   (flet ((matching-process-p (window)
              (eq (listener-process window) p)))
     (gui::first-window-satisfying-predicate #'matching-process-p)))

(defun front-remote-listener ()
   "The frontmost remote listener (even if a regular listener is on top)"
  (gui::first-window-satisfying-predicate #'listener-remote))

(defmacro front-remote-eval (&body body)
  "Evaluate the body using the remote of the frontmost remote listener, for testing purposes"
  `(remote::using-remote-lisp (listener-remote (front-remote-listener))
                              ,@body))

(defmethod remote-repl-listener ((repl remote::remote-repl))
   (flet ((same-repl-p (window)
              (eq (listener-remote window) repl)))
          (or (gui::first-window-satisfying-predicate #'same-repl-p)
                ;; ## fallback, should be eliminated asap!
                (mclide::front-remote-listener))))  ; ## incorrect, should be found from repl!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifies the CCL listener to be connected to a remote implementation.

(in-package :mclide)

(defun new-remote-lisp (host port &optional secret)
   "Returns immediately after validating the arguments, while connecting to a remote implementation with user feedback"
   ;; corresponds to function in mclide1
   (let ((app (remote::remote-application host port secret)))
      (process-run-function "New Remote Listener" #'new-remote-listener app)))

(defparameter *remote-app* NIL "Used to pass the remote-app during initialization of the listener")

(defparameter *listener-wrapper* #'funcall 
   "A function taking a thunk to call as argument - used to bind the context of the listener process")

(defun new-remote-listener (app &optional (wrapper *listener-wrapper*))
   (flet ((thunk ()
              (let ((*remote-app* app)
                     (*listener-wrapper* wrapper))
                 ;; ## Deprecated in Mac OS X v10.4!!!
                 (#/openUntitledDocumentOfType:display:   
                  (#/sharedDocumentController ns:ns-document-controller) #@"Listener" t))))
          (let ((listener (gui::execute-in-gui #'thunk)))
            listener)))

(defun connect-app (app)
   (handler-bind
      ((remote::notify-connected
        (lambda (c)
          (with-slots (remote::implementation-type remote::implementation-version) c
              (format T "Welcome to ~A~@[ ~A~]!" 
                      remote::implementation-type remote::implementation-version)))))
      (let ((repl (make-instance 'remote::remote-repl))
             (remote::*remote-eval-dispatch-swank* T))
        (remote::attach app repl)
        repl)))

#+ignore
(defun connect (port &optional (out (make-broadcast-stream)))
  (let ((app (remote::remote-application #-mcl 2130706433 #+mcl (ccl::tcp-host-address "127.0.0.1") port nil)))
    (values app (connect-app app))))

(defun connect-call (fun app)
   "Connect application and evaluate the function in a remote context"
     (let* ((repl (connect-app app)))
       (remote::using-remote-lisp repl 
         (remote::remote-package-names) ;; init package names - ## handle this better, perhaps during attach? for mclide1 too...
         (funcall fun))))

(defmethod application-error ((a gui::cocoa-application) (condition remote::failed-connecting) error-pointer)
  (declare (ignore error-pointer))
  (labels ((associated-listener-document ()
             (or (listener-document)
                 (gui::top-listener-document)))
           (close-listener ()
             ; # alternatively just do process-reset on the listener process
             (#/performClose: (associated-listener-document) ccl:+null-ptr+))
           #+ignore
           (alert-action (result)
             (case result
               (:default :continue)
               (:alternate :terminate))))
    (let ((continue (find-restart 'continue)))
      (ecase (gui::alert "Possibly a remote implementation isn't available at the host or port." 
                :title "Failed to Connect" 
                :owner (associated-listener-document)
               ;  :action #'alert-action
                :default-button  (if continue (format nil "~A" continue) "OK")
                :alternate-button "Exit")
        (:default (continue condition))
        #+ignore (:close (close-listener))
        (:alternate (process-reset *current-process*)))))
  (toplevel))

(defmethod application-error ((a gui::cocoa-application) (condition remote::not-connected) error-pointer)
  (declare (ignore error-pointer))
  (labels ((associated-listener-document ()
             (or (listener-document)
                 (gui::top-listener-document)))
           (close-listener ()
             ; # alternatively just do process-reset on the listener process
             (#/performClose: (associated-listener-document) ccl:+null-ptr+))
           #+ignore
           (alert-action (result)
             (case result
               (:default :continue)
               (:alternate :terminate))))
    (let ((continue (find-restart 'continue)))
      (ecase (gui::alert (format nil "The listener is not connected to a remote implementation.") 
                      :title "Not Connected" 
                      :owner (or (listener-document)
                                 (gui::top-listener-document))
                      ; :action #'alert-action
                      :default-button (if continue (format nil "~A" continue) "OK")
                      :alternate-button "Exit")
        (:default (continue condition))
        #+ignore (:close (close-listener))
        (:alternate (process-reset *current-process*)))))
  (toplevel))

(ccl::defcontextual ccl::listener-function ()
                               *remote-app*
      (flet ((do-it ()
               (let ((ccl::*inhibit-greeting* T))
                 (:do-it))))
        (connect-call #'do-it *remote-app*)))

(advise ccl::make-mcl-listener-process
  ; Hairy hack to pass *remote-app* to the listener process and bind the context for a remote connection.
            (let ((initial-function (getf (nthcdr 4 ccl::arglist) :initial-function ccl::arglist))
                   (app *remote-app*)
                   (wrapper *listener-wrapper*))
              (setf (getf (nthcdr 4 ccl::arglist) :initial-function ccl::arglist)
                      (lambda (&aux (*remote-app* app))
                            (funcall wrapper initial-function)))                       
              (:do-it))
      :when :around :name remember-listener-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODELINE PACKAGE

(in-package :mclide)

(defmethod remote-buffer-p (buffer)
  "True if the buffer is for a remote listener"
  ;; ## not correct, should use buffer!
  (remote::remote-lisp-mode))

;; Destructively changes the package modeline field to avoid ?? for remote packages not declared in internal lisp.
;; Cannot use make-modeline-field as it doesn't update existing instance!

(flet ((mclide-modeline-package-field (buffer)
         "Returns the value of buffer's \"Current Package\" followed
  by a colon and two spaces, or a string with one space."
         (if (hi:hemlock-bound-p 'hemlock::current-package :buffer buffer)
           (let ((val (hi:variable-value 'hemlock::current-package
                                         :buffer buffer)))
             (if (stringp val)
               (if (or (remote-buffer-p buffer) (find-package val))
                 (format nil "~A:  " val)
                 (format nil "?~A?:  " val))
               " "))
           " ")))
  (let ((field (gethash :package hi::*modeline-field-names*)))
    (setf (hi::modeline-field-%function field) #'mclide-modeline-package-field)))

(defmethod remote::new-package-handler ((repl remote::remote-repl) package-name)
  (setf (remote::thread-package repl) package-name)
  (let ((buffer (hi::hemlock-view-buffer (gui::hemlock-view (remote-repl-listener repl)))))
    (setf (hi::variable-value 'hemlock::current-package :buffer buffer) package-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGLIST ON SPACE

(in-package :hemlock)

(ccl::defcontextual string-to-arglist (string buffer &optional quiet-if-unknown)
   (remote::remote-lisp-mode) ;; ## better testing the remote of the buffer!!
  (declare (ignore quiet-if-unknown))
  (let ((package (variable-value 'current-package :buffer buffer)))
    ;; #'message need a context not available if enqueued, but will this cause hickups?
     (remote::remote-eval-bind (arglist-exp)
                                      (remote::query-form 
                                       (remote::operator-arglist-form string package)
                                       :recover (lambda () (return-from string-to-arglist))
                                       :thread t)
                                                                        
       (when arglist-exp
         (message "~a" (subseq arglist-exp 1 (1- (length arglist-exp))))))
    NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERRUPT MENU ITEM (FORCE BREAK)

(in-package :mclide)

(ccl::defcontextual ccl::force-break-in-listener (process)
   (and (process-listener process)
        (listener-remote (process-listener process)))
   ;; called from #/interrupt on the menu
  (let* ((listener (process-listener process))
         (remote (when listener (listener-remote listener))))
    (when remote
      (remote::remote-eval-enqueue-bind (result) 
                                        (remote::query-form
                                         (remote::mclide-interrupt-abort)
                                         :remote remote
                                         :thread t)
         (declare (ignore result))
         NIL))))





