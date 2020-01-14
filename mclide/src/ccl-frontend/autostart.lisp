(defpackage autostart)

(in-package :autostart)

(defun logfile (name)
  ;; similar as in mclide1
  (merge-pathnames
   (make-pathname :directory ";com.in-progress.mclide"
                  :name (format nil "~A-swank" (string-downcase name))
                  :type "log")
   ;; # should lookup path for logs from the OS! 
   (full-pathname "home:Library;Logs;")))

(defun activate-implementation (name path)
  ;; # generalize to other implementations!
  (let* ((loader (namestring (full-pathname "resources:Slime;swank-loader.lisp")))
         (portfile (ccl::gen-temp-name))
         (initform (remote-lisp-initform name
                      :logfile (logfile name)
                      :portfile (full-pathname portfile)))
         #+ignore (asdf::*VERBOSE-OUT* mclide::*log*))
    (uiop:run-program (format nil "(~S -L ~S -e ~S)&" path loader initform))
    portfile))

(defmethod reset-from-portfile ((application remote::remote-application) portfile)
   (reinitialize-instance application
      :secret (remote::valid-secret (swank-client::slime-secret))
      :host (remote::valid-ip "localhost")
      :port (remote::valid-portnum (ccl::retrieve-swank-file-data portfile)))
   (setf (remote::remote-clients application) NIL)
    application)

(defun remote-application-from-portfile (portfile)
  (let* ((secret (swank-client::slime-secret))
         (host "localhost")
         (port (ccl::retrieve-swank-file-data portfile)))
    (remote::remote-application host port secret)))
