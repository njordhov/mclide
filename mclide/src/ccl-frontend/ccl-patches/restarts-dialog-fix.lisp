;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOSING RESTARTS DIALOG WITHOUT BACKTRACE  FAIL

#|
In Clozure 1.7, returning from backtrace fails if the Restarts dialog has been closed. 

This is related to the issues discussed in:
http://permalink.gmane.org/gmane.lisp.openmcl.devel/2339
http://comments.gmane.org/gmane.lisp.openmcl.devel/2332

The reason is that ui-object-exit-backtrace-context attempts to #/close the (car (ccl::bt.restarts context)) even if that is a closed restarts sequence-window-controller.

The patch below fixes the problem by setting (car (ccl::bt.restarts context)) to NIL when closing the restarts dialog.

The functionality has been considerably changed in the trunk since Clozure 1.7, but the problem seems to remain.

Note: I attempted to use an observer instead of a #/windowWillClose method on restarts-window-controller class, but couldn't get it to take effect.

  (#/addObserver:selector:name:object: (#/defaultCenter ns:ns-notification-center)
				       controller
				       (@selector #/NSWindowWillCloseNotification)
				       #&NSWindowWillCloseNotification
				       (#/standardUserDefaults ns:ns-user-defaults)))

Perhaps Clozure could benefit from a utility to register such observers?

Also, the Restarts dialog is not immediately closed for restarts that requests user input. This is also fixed below by making the :result-callback close the window.

|#

(in-package :gui)

(defclass restarts-window-controller (sequence-window-controller)
  ((cleanup :initform nil :initarg :cleanup))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/windowWillClose: :void) ((self restarts-window-controller)
					    notification)
  (declare (ignore notification))
  (funcall (or (slot-value self 'cleanup) #'false))
  (call-next-method self))

(defun restarts-controller-for-context (context)
  (or (car (ccl::bt.restarts context))
      (setf (car (ccl::bt.restarts context))
            (let* ((tcr (ccl::bt.tcr context))
                   (tsp-range (inspector::make-tsp-stack-range tcr context))
                   (vsp-range (inspector::make-vsp-stack-range tcr context))
                   (csp-range (inspector::make-csp-stack-range tcr context))
                   (process (ccl::tcr->process tcr))
                   (controller
                     (make-instance 'restarts-window-controller
                             :cleanup (lambda ()
                                        (setf (car (ccl::bt.restarts context)) nil))
                             :sequence (cdr (ccl::bt.restarts context))
                             :result-callback #'(lambda (r)
                                                  (#/performSelectorOnMainThread:withObject:waitUntilDone: 
                                                   (car (ccl::bt.restarts context)) (@selector #/close)  +null-ptr+ t)
                                                  (process-interrupt
                                                   process
                                                   #'invoke-restart-interactively
                                                   r))
                             :display #'(lambda (item stream)
                                          (let* ((ccl::*aux-vsp-ranges* vsp-range)
                                                 (ccl::*aux-tsp-ranges* tsp-range)
                                                 (ccl::*aux-csp-ranges* csp-range))
                                          (princ item stream)))
                             :title (format nil "Restarts for ~a(~d), break level ~d"
                                            (process-name process)
                                            (process-serial-number process)
                                            (ccl::bt.break-level context)))))
              controller))))




