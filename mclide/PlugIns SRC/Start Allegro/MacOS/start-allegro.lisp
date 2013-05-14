(in-package :mclide) ;; make new package for allegro plugin?

(labels ((implementation-from-user ()
           "Returns the portfile of a running implementation"
           (let* ((alias (full-pathname "Resources:allegro-executable"))
                  (executable (if (probe-file alias) (truename alias)))
                  (pathname 
                   (catch-cancel
                    (gui::cocoa-choose-file-dialog
                            :file (if executable (file-namestring executable) "alisp")
                            :directory executable
                            :button-string "Select" 
                            :title "Start Allegro" 
                            :message "Locate the Allegro executable file (e.g. 'alisp' or 'mlisp'):"))))
             (when pathname
               ; always create alias to keep the latest:
               (asdf:run-shell-command "ln -s ~S ~S" (namestring pathname) (namestring alias))
               (autostart::activate-implementation :allegro pathname))))
         (start-implementation (&optional (implementation (implementation-from-user)))
            (when implementation
              (let ((app (autostart::remote-application-from-portfile implementation))
                     (restarted))
                (gui::with-modal-progress-dialog "Starting Allegro" "Connecting..."
                     (handler-case
                      (labels ((restart-implementation (app)
                                 "restart implementation as needed, handling gracefully if multiple listeners compete on starting the implementation"
                                 ; # alternative to the restarted bool is if app only contains open clients
                                 (unless restarted 
                                   (let ((implementation (implementation-from-user)))
                                     (when (and implementation (not restarted))
                                       (setf restarted T)
                                       (autostart::reset-from-portfile app implementation)
                                       (setf *new-listener-menu-action* #'show-implementation-listener)))))
                               (listener-wrapper (fn)
                                     "Context for the listener process function"
                                     (handler-case
                                      (funcall fn)
                                      (remote::failed-connecting (c)
                                       (assert (eq app *remote-app*))
                                       (setf *new-listener-menu-action* #'start-implementation)
                                       (setf restarted nil)
                                       (cerror "Start Allegro" c)
                                       (restart-implementation app)
                                       (listener-wrapper fn))
                                      (remote::not-connected (c)
                                       (cerror "Reconnect" c)
                                       (listener-wrapper fn))))
                               (show-implementation-listener ()
                                 (process-run-function "show listener" #'new-remote-listener app #'listener-wrapper)))
                        (setf *new-listener-menu-action* #'show-implementation-listener)
                        (show-implementation-listener))
                      (error (c)
                       (format mclide::*log* "~%Failed when starting Allegro (~A)" c)
                       (easygui::message-dialog 
                        (format nil "Unable to start Allegro. See ~A for details." (file-namestring mclide::*log*)))))
                     (sleep 1/2))))))

       (setf *new-listener-menu-action* #'start-implementation)
       
       (process-run-function "start allegro" #'start-implementation))
