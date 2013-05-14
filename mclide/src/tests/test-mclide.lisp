(in-package :mclide)

(defun enable-debug (&optional (mode T))
  (setq swank-client::slime-log-events (when mode *log*)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :remote)

(ccl::using-remote-listener (front-remote-listener)
  (remote-eval-bind (specs) 
                    (remote-definitions-form  (make-remote-symbol "MAPCAR" "SWANK") :callers)
      (mapcar #'remote-definition-name
            (mapcar #'make-remote-definition-from-spec specs))))


