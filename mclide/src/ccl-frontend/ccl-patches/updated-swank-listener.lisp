;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clozure's swank-listener updated to recent swank, and no longer requires patching slime:

;; Consider making path arg of load-and-start-swank optional with default, skipping loading if nil


(in-package :gui)

; workaround for swank bug where port in *LISTENER-SOCKETS* property list can be NIL:

(defun swank-server-running? ()
  (and (find-package :swank)
       (let ((active-listeners (symbol-value (intern "*LISTENER-SOCKETS*" :swank))))
         (if (not (null active-listeners))
              (or (first active-listeners)
                    (funcall (intern "LOCAL-PORT" :swank) 
                                 (cadadr active-listeners)))))))

#+ignore (defvar swank::*LISTENER-SOCKETS* NIL) ; because swank-server-running? fails without it

;; load-and-start-swank is changed to use loadup instead of load-swank to load all contribs (required to call swank:create-repl)

(defun load-and-start-swank (path requested-port) 
  (handler-case (let* ((active-swank-port (swank-server-running?))
                       (msg (format nil "A swank server is already running on port ~A" active-swank-port)))
                  (if active-swank-port
                      (progn
                        (log-debug msg)
                        (make-swank-status :active? t :message msg :requested-loader path :requested-port requested-port))
                      (progn
                        (load path)
                        (let ((swank-loader-package (find-package :swank-loader)))
                          (if swank-loader-package
                              ;; swank loaded. start the server
                              (progn
                                #+ignore (funcall (intern "LOAD-SWANK" swank-loader-package))
                                (funcall (intern "LOADUP" swank-loader-package)) ;; loads contribs too
                                (let ((port (funcall (intern "CREATE-SERVER" (find-package :swank)) :port requested-port :dont-close t)))
                                  (make-swank-status :active? t :requested-loader path :requested-port port)))
                              ;; swank failed to load. return failure status
                              (make-swank-status :active? nil :message "swank load failed" :requested-loader path :requested-port requested-port))))))
    (ccl::socket-creation-error (e) (log-debug "Unable to start a swank server on port: ~A; ~A"
                                               requested-port e)
                                (make-swank-status :active? nil :message "socket-creation error"
                                                   :requested-loader path :requested-port requested-port))
    (serious-condition (e) (log-debug "There was a problem creating the swank server on port ~A: ~A"
                                      requested-port e)
                       (make-swank-status :active? nil :message "error loading or starting swank"
                                          :requested-loader path :requested-port requested-port))))

;; no longer fails if connecting using unmodified swank client:

(defun read-swank-ping (tcp-stream) 
   (if (digit-char-p (peek-char nil tcp-stream) 16)
     nil
     (read-line tcp-stream nil nil nil)))

;; ## should send-swank-response also use swank rpc format with length?
