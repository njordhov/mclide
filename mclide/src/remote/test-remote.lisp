; Test harness for MCLIDE REMOTE

(eval-when (:execute-toplevel)
   (eval-enqueue `(load ,*loading-file-source-file*))
   (abort))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :remote)

(defVar *remote-host* 0)
(defVar *remote-port* 57359)
(defVar *remote-secret* (swank-client::slime-secret))

; (defparameter *remote-port* 57359)

(defParameter *app* (remote-application *remote-host* *remote-port* *remote-secret*))

(defParameter *repl* (make-instance 'remote-repl))

(attach *app* *repl*)

(using-remote-lisp *repl*
  (remote-eval-bind (result) 
                              `(+ 1 2)
        result))

(using-remote-lisp *repl*
  (list
    (list (cl:lisp-implementation-type)
        *current-process*)
   (remote-eval-bind (result) 
                               `(list (cl:lisp-implementation-type) (write-to-string *current-process*))
         result)))

(using-remote-lisp *repl*
   (remote-eval-bind (result) 
                               (remote-symbol-documentation-form (make-remote-symbol "mapcar"))
         result))




                    (remote-definitions-form  (make-remote-symbol "MAPCAR" "SWANK") :callers)
      (mapcar #'remote-definition-name
            (mapcar #'make-remote-definition-from-spec specs))))
         
      
   
    
