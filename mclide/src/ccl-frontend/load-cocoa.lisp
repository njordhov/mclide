(in-package :ccl)

(eval-when (:compile-toplevel :load-toplevel)
 (require :cocoa)
 (require :easygui)
 #+ignore
 (load "ccl:examples;cocoa;easygui.lisp"))

; (load "ccl:examples;cocoa;easygui;example;tiny.lisp")
; (make-instance 'easygui-demo:tiny-demo-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clozure's swank-listener also works when connected from MCLIDE

;; Suggest upgrading Clozure to implement this functionality
;; suggest clozure make path arg of load-and-start-swank optional with default, skipping loading if nil


(in-package :gui)

(defvar swank::*LISTENER-SOCKETS* NIL) ; because swank-server-running? fails without it

(defvar *swank-loader-pathname* "resources:Slime;swank-loader")

#+ignore
(advise load-and-start-swank 
    ; return active port in status
    (progn
     (unless (first ccl::arglist) 
                  (setf (first ccl::arglist)
                          *swank-loader-pathname*))
     (let ((status (:do-it))
            (sockets swank::*LISTENER-SOCKETS*))
        (make-swank-status :active? (swank-active? status)
                                           :message (swank-message status)
                                          :requested-loader (swank-requested-loader status)
                                          :requested-port (or (swank-requested-port status)
                                                                          (swank-server-running?)
                                                                          (when (second sockets) ;; sometimes the first on socket plist is nil for port
                                                                             (local-port (second (second sockets))))))))
    :when :around :name load-and-start-swank_optional-path)


(advise load-and-start-swank 
    ; return active port in status
  (progn
     (unless (first ccl::arglist) 
                  (setf (first ccl::arglist)
                          *swank-loader-pathname*))
     (let ((status (:do-it)))
        ;; ## NO LONGER NEEDED?
        (make-swank-status :active? (swank-active? status)
                           :message (swank-message status)
                           :requested-loader (swank-requested-loader status)
                           :requested-port (or (swank-requested-port status)
                                               (swank-server-running?)))))
    :when :around :name load-and-start-swank_optional-path)

