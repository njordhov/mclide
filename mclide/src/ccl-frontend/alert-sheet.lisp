;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FEEDBACK SHEET

(in-package :gui)

;; see the sequence-window-controller for similar

(defclass alert-sheet-controller (ns:ns-window-controller)
   ((title :initarg :title :initform nil)
    (message :initarg :message :initform "<empty>")
    (default-button :initarg :default-button :initform "OK")
    (alternate-button :initarg :alternate-button :initform nil :type (or null string))
    (owner :initarg :owner :initform NIL)
    (action :initarg :action :initform NIL :type (or function null)))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/runAlertSheet: :void) ((controller alert-sheet-controller) owner title message default-button alternate-button)
  (#_NSBeginAlertSheet title
                       default-button
                       (or alternate-button +null-ptr+)
                       +null-ptr+
                       owner
                       controller
                       (@selector #/didEndSelector:)
                       +null-ptr+ ;  (@selector #/didDismissSelector:)
                       +null-ptr+ ; context info
                       message))

(defmethod initialize-instance :after ((controller alert-sheet-controller) &key)
     (execute-in-gui ;; possibly not needed
      (lambda ()
       (#/showAlertSheet: controller)))
     #+ignore
     (#/performSelectorOnMainThread:withObject:waitUntilDone:
     controller
     (@selector #/showAlertSheet:)
     controller
     t))

(objc:defmethod (#/showAlertSheet: :void) ((controller alert-sheet-controller))
   (with-slots (title message default-button alternate-button owner) controller
       (let* ((nstitle (if title (%make-nsstring title ) +null-ptr+))
              (nsmessage (%make-nsstring message))
              (ns-default-button (%make-nsstring default-button))
              (ns-alternate-button (if alternate-button (%make-nsstring alternate-button) +null-ptr+))
              (nswindow (etypecase owner
                          ((eql :listener) 
                           (#/windowForSheet (#/topListener gui::hemlock-listener-document)))
                          (hemlock-listener-document
                           (#/windowForSheet owner)))))
          (#/runAlertSheet: controller nswindow nstitle nsmessage ns-default-button ns-alternate-button))))

; ;; performSelectorOnMainThread

(objc:defmethod (#/didEndSelector: :void) ((controller alert-sheet-controller) panel return-code contextinfo)
  ; WTF -  the return-code should be an integer but is encoded as an NSPanel
   (declare (ignore panel contextinfo))
   (let ((result (case (%ptr-to-int return-code)
                   (#.#$NSAlertDefaultReturn :default)
                   (#.#$NSAlertAlternateReturn :alternate)
                   (#.#$NSAlertOtherReturn :other)
                   (#.#$NSAlertErrorReturn :error)
                   (otherwise return-code))))
        (with-slots (action) controller
            (when action
               (funcall action result)))))

(defloadvar *alert-sheet-controller* NIL)

; merge with the one in alert.lisp!

(defun alert (message &rest rest &key owner action (title "Alert") (default-button "OK") alternate-button )
   ;; # consider merging with #'alert-window or make public
   (declare (dynamic-extent rest))
   (if owner
     (let ((done (make-semaphore))
           (value nil))
       (flet ((alert-action (mode)
                (setf value
                  (if action
                    (funcall action mode)
                    mode))
                (signal-semaphore done)))
        (let ((controller (make-instance 'alert-sheet-controller
                            :message message 
                            :title title 
                            :default-button default-button
                            :alternate-button alternate-button
                            :owner owner 
                            :action #'alert-action)))
          (declare (ignore controller))
          (wait-on-semaphore done nil "Alert Shown")
          value)))
     ;; ## should also return a result
     (let ((args (remprop :owner (remprop :action rest))))
       (apply #'alert-window  :message message args))))