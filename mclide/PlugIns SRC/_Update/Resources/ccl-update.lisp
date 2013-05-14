
(in-package :ccl)

#+clozure ;; mostly borrowed from builder-utilities.lisp in Clozure 1.7
(defun get-ide-bundle-info-plist ()
  (let* ((ide-bundle (#/mainBundle ns:ns-bundle))
         (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
         (ide-bundle-path (ensure-directory-pathname 
                           (lisp-string-from-nsstring ide-bundle-path-nsstring)))
         (ide-plist-path-str (namestring (merge-pathnames ";Contents;Info.plist" ide-bundle-path)))
         (info-path-str (#/autorelease (%make-nsstring (namestring ide-plist-path-str)))))
    (#/dictionaryWithContentsOfFile: ns:ns-mutable-dictionary 
                                     info-path-str)))

#+clozure
(defun get-bundle-property (key)
  (let ((nskey (#/autorelease (%make-nsstring key)))
        (dict (get-ide-bundle-info-plist)))
    (iu::ns-to-lisp-object (#/objectForKey: dict nskey))))   

; (get-bundle-property "CFBundleVersion")

#+clozure
(defun telnet-read-line (stream)
   (read-line stream nil))

#+clozure
(defun telnet-write-line (stream control &rest args)
   (apply #'format stream control args)
   (write-char #\cr stream)
   (write-char #\lf stream)
   T)

#+clozure
(defvar *update-connect-timeout* 10)

#+clozure
(defun open-tcp-stream (host port)
   (let* ((sock (usocket:socket-connect host port :timeout *update-connect-timeout*))
            (connection (usocket:socket-stream sock)))
     connection))

#+clozure
(iu:make-and-install-menuitems-after "Clozure CL" "Preferences..." ;; bug in clozure, should be epsilon!
    `(,(format nil "Update~A" mclide::%epsilon%) "installUpdate" "" ,mclide::*controller*))

#+clozure
(objc:defmethod (#/installUpdate :void)
                ((self mclide::lisp-controller))
   (process-run-function "install update"
     (lambda ()
       (when (easygui::y-or-n-dialog "Download and install the latest MCLIDE update?") 
         (let ((update ( progn ; gui::with-modal-progress-dialog "Updating MCLIDE" "Downloading update file..."
                                (handler-case
                                 (download-update)
                                 (error (c)
                                           c)))))
           (typecase update
             (condition
              (easygui::message-dialog "Update failed. Try again later."))
             ((or pathname string)
              (enable-update update))))))))
 