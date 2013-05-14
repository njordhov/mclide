;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't print "> Type :? for other options" for ui applications.

(in-package :ccl)

(let ((*warn-if-redefine-kernel* NIL))

(defun break-loop (condition &optional (frame-pointer *top-error-frame*))
  "Never returns"
  (let* ((%handlers% (last %handlers%)) ; firewall
         (*break-frame* frame-pointer)
         (*break-condition* condition)
         (*compiling-file* nil)
         (*backquote-stack* nil)
         (continue (find-restart 'continue))
         (*continuablep* (unless (eq %last-continue% continue) continue))
         (%last-continue% continue)
         (*standard-input* *debug-io*)
         (*standard-output* *debug-io*)
         (*signal-printing-errors* nil)
         (*read-suppress* nil)
         (*print-readably* nil)
	 (*default-integer-command* `(:c 0 ,(1- (length (compute-restarts condition)))))
         (context (new-backtrace-info nil
                                      frame-pointer
                                      (if *backtrace-contexts*
                                        (or (child-frame
                                             (bt.youngest (car *backtrace-contexts*))
                                             nil)
                                            (last-frame-ptr))
                                        (last-frame-ptr))
                                      (%current-tcr)
                                      condition
                                      (%current-frame-ptr)
                                      #+ppc-target *fake-stack-frames*
                                      #+x86-target (%current-frame-ptr)
                                      #+arm-target (or (current-fake-stack-frame) (%current-frame-ptr))
                                      (db-link)
                                      (1+ *break-level*)))
         (*backtrace-contexts* (cons context *backtrace-contexts*)))
    (with-terminal-input
      (with-toplevel-commands :break
        (if *continuablep*
          (let* ((*print-circle* *error-print-circle*)
                 (*print-level* *error-print-level*)
                 (*print-length* *error-print-length*)
					;(*print-pretty* nil)
                 (*print-array* nil))
            (format t (or (application-ui-operation *application* :break-options-string t)
                          "~&> Type :GO to continue, :POP to abort, :R for a list of available restarts."))
            (format t "~&> If continued: ~A~%" continue))
          (format t (or (application-ui-operation *application* :break-options-string nil)
                        "~&> Type :POP to abort, :R for a list of available restarts.~%")))
        (unless (application-ui-object *application*) ;; <<----- TN
          (format t "~&> Type :? for other options."))
        (terpri)
        (force-output)

        (clear-input *debug-io*)
        (setq *error-reentry-count* 0)  ; succesfully reported error
        (ignoring-without-interrupts
          (unwind-protect
               (progn
                 (application-ui-operation *application*
                                           :enter-backtrace-context context)
                 (read-loop :break-level (1+ *break-level*)
                            :input-stream *debug-io*
                            :output-stream *debug-io*))
            (application-ui-operation *application* :exit-backtrace-context
                                      context)))))))

) ; end redefine
