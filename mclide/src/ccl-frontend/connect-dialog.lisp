;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONNECT DIALOG

(in-package :easygui)   ;; ## should be mclide package

(defclass connect-window (window)
   ()
   (:default-initargs :size (point 378 175)
     :position (point 125 513)
     :title "Connect to Swank Server"
     :resizable-p nil
     :minimizable-p t))

(defun message-dialog (message)
   (gui::alert-window :message message))

(defmethod initialize-view :after ((cw connect-window))
  (let* ((width (point-x (view-size cw)))
           (left-margin 10)
           (div 70)
           (right-margin 10)
           (label-size (point (- div left-margin) 20))
           (field-size (point (- width div right-margin) 20))
         (host-label (make-instance 'static-text-view
                      :position (point left-margin 130)
                      :size label-size
                      :dialog-item-text "Host:"))
        (host-field (make-instance 'text-input-view
                      :position (point div 130)
                      :size field-size))
        (port-label (make-instance 'static-text-view
                      :position (point left-margin 100)
                      :size label-size
                      :dialog-item-text "Port:"))
        (port-field (make-instance 'text-input-view
                      :position (point div 100)
                      :size field-size))
        (pwd-label  (make-instance 'static-text-view
                      :position (point left-margin 70)
                      :size label-size
                      :dialog-item-text "Secret:"))
        (pwd-field  (make-instance 'password-input-view
                      :position (point div 70)
                      :size field-size))
        (line (make-instance 'box-view
                 :position (point 15 59)
                 :size (point 353 2)))
        (cancel-button (make-instance 'push-button-view
                           :default-button-p nil
                           :text "Cancel"
                           :position (point 200 15)))
        (connect-button (make-instance 'push-button-view
                           :default-button-p t
                           :text "Connect"
                           :position (point 280 15))))
    (labels ((listener-wrapper (fn)
               "Context for the listener process function"
               (handler-case
                   (funcall fn)
                 (remote::failed-connecting (c)
                   (cerror "Try Again" c)
                   (listener-wrapper fn))
                 (remote::not-connected (c)
                   (cerror "Reconnect" c)
                   (listener-wrapper fn))))
             (show-listener ()
               (handler-case
                   ;; ## similar code in remote-application-from-portfile...
                   (let* ((host (view-text host-field) )
                          (port (view-text port-field))
                          (secret (view-text pwd-field))
                          (app (remote::remote-application host port secret)))
                     (ccl::process-run-function "show listener" #'mclide::new-remote-listener app #'listener-wrapper)
                     (perform-close cw))
                 (remote::address-error (c)
                   (message-dialog (format nil "~A" c)))
                 (error (c)
                   (message-dialog (format NIL "Error: ~A" c)))))
             (cancel-action ()
               (perform-close cw)))
      (setf (action connect-button) #'show-listener)
      (setf (action cancel-button) #'cancel-action))
    (setf (view-text host-field) "localhost")
    (setf (view-text pwd-field) (or (swank-client::slime-secret) ""))
    (setf (view-text port-field)
            (cond
             ((gui::swank-server-running?)
              (write-to-string (gui::swank-server-running?)))
            ((gui::swank-listener-active?)
              (write-to-string gui::*active-gui-swank-listener-port*))
            (T "4005")))
    (add-subviews cw host-label host-field port-label port-field pwd-label pwd-field line cancel-button connect-button)
    (window-show cw)))

(defun new-remote-lisp-from-user ()
   ;; similar to mclide1
   (make-instance 'connect-window))