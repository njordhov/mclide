
(in-package :ccl)

#+mcl
(load (merge-pathnames "mcl-update" *loading-file-source-file*))

#-mcl
(load (merge-pathnames "ccl-update" *loading-file-source-file*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-net-resource (url &optional out)
  (let* ((start (+ (search "://" url) 3))
         (split (position #\/ url :start start))
         (host (subseq url start split))
         (path (subseq url split)))
    (with-open-stream (stream (open-tcp-stream host 80))
      (telnet-write-line stream "GET ~A HTTP/1.0" path)
      (telnet-write-line stream "User-Agent: MCLIDE")
      (telnet-write-line stream "Host: ~A" host)
      (telnet-write-line stream "")
      (force-output stream)
      (let ((response (telnet-read-line stream))
            (headers (loop for h = (telnet-read-line stream)
                           while (and h (find-if-not #'whitespacep h)) 
                           collect h)))
        (when out
          (loop 
            for line = (telnet-read-line stream)
            while line
            do (write-line line out)))
           (when (or response headers)
          (cons response  headers))))))

(defun find-http-header (label headers)
   (let ((len (length (string label))))
      (flet ((match (h)
                  (and (string-equal label h :end2 len)
                         (char= #\: (aref h len)))))
         (find-if #'match headers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATES

(defun absolute-time-from-string (string)
  ; http://www.unicode.org/reports/tr35/#Date_Format_Patterns
  #+mcl
  (with-cfstrs ((timestr string)
                (RFC_822 "EEE, d MMM yyyy HH:mm:ss z"))
    (let* ((locale (#_CFLocaleCopyCurrent))
           (formatter (#_CFDateFormatterCreate 
                       (%null-ptr) 
                       locale 
                       #$kCFDateFormatterNoStyle
                       #$kCFDateFormatterNoStyle)))
      (#_CFDateFormatterSetFormat formatter RFC_822)
      #+ignore
      (#_FDateFormatterSetProperty formatter #$kCFDateFormatterIsLenient #$kCFBooleanTrue)
      (rlet ((&absolute :CFABSOLUTETIME))
        (let ((ok? (#_CFDateFormatterGetAbsoluteTimeFromString 
                    formatter
                    timestr
                    (%null-ptr)
                    &absolute)))
          (#_cfrelease locale)
          (#_cfrelease formatter)
          (when ok?
            (pref &absolute :double-float))))))
  #+clozure ;; ## ad-hoc for now
  (prog2 string (- (get-universal-time) 
                            #.(encode-universal-time 0 0 0 1 1 2001))))

(defun universal-time-from-string (string)
  (let ((absolute (absolute-time-from-string string)))
    (when absolute
      (+ (floor absolute) 
         #.(encode-universal-time 0 0 0 1 1 2001)))))

#+ignore
(universal-time-from-string "Wed, 16 Dec 2009 20:32:20 GMT")

(defvar *update-plugin-contents* 
  (merge-pathnames #+mcl ";;" #+clozure ";..;" 
   (directory-namestring (full-pathname *loading-file-source-file*))))

; *update-plugin-contents*

(defun disabled-updates-directory ()
   "Pathname to the folder with inactive updates"
   (merge-pathnames ";PlugIns Disabled;" *update-plugin-contents*))

; (disabled-updates-directory)

(defun enabled-updates-directory ()
   "Pathname to the folder with inactive updates"
   (merge-pathnames ";PlugIns;" *update-plugin-contents*))

; (enabled-updates-directory)

(defun disable-all-updates ()
  (dolist (file (directory (merge-pathnames "*.lisp" (enabled-updates-directory))))
    (rename-file file (disabled-updates-directory) :if-exists :overwrite)))

(defun enable-update (file)
  (disable-all-updates)
  #+mcl
  (create-directory (enabled-updates-directory) :if-exists nil)
  #+clozure
  (create-directory (enabled-updates-directory))
  (rename-file (merge-pathnames (disabled-updates-directory) file)
               (merge-pathnames (enabled-updates-directory) file)))

(defun current-update ()
  (first (directory (merge-pathnames ";*.lisp" (enabled-updates-directory)))))

(defun all-updates ()
  (sort (nconc (directory (merge-pathnames ";*.lisp" (enabled-updates-directory)))
               (directory (merge-pathnames ";*.lisp" (disabled-updates-directory))))
        #'string<
        :key #'pathname-name))

; (all-updates)

(defun download-update ()
   "Download the latest update, eventually adding it to the collection of updates and returning its pathname"
  (let* ((version (get-bundle-property "CFBundleVersion"))
         (url (format nil "http://mclide.in-progress.com/download/update/~A.lisp" 
                            (substitute #\_ #\space version)))
         (tempfile (gen-temp-name))
         (headers (with-open-file (out tempfile :direction :output :if-does-not-exist :create)
                           (get-net-resource url out)))
         (header (find-http-header :last-modified headers)))
    (when header
      (let* ((modified (subseq header (+ 1 (length (string :last-modified )))))
             (modtime (universal-time-from-string modified))
             (name (update-name-from-time modtime)))
        (unless (member name (all-updates) :key #'pathname-name :test #'equalp)
          (copy-file tempfile (merge-pathnames (make-pathname :name name :type "lisp")
                                               (disabled-updates-directory))))))))

; (download-update)

(defun user-revert-update (&optional message)
  (flet (#+mcl 
         (ask-download? ()
           (y-or-n-dialog
            (format nil "~AWould you like to disable the update or if available download a more recent replacement?" 
                    (or message "")) 
            :yes-text "Download"
            :no-text "Disable"))
         #+clozure
         (ask-download? () 
           (easygui::y-or-n-dialog 
            (format nil "~AWould you like to disable the update? If no, downloading a more recent replacement." 
                    (or message ""))))
         #+clozure
         (message-dialog (message &key title ok-text)
           (declare (ignore title ok-text))
           (easygui::message-dialog message)))

  (catch-cancel
    (if (ask-download?)
      (or (ignore-errors (download-update))
          (disable-all-updates))
      (disable-all-updates))
    (message-dialog "The application has to be restarted for the change to take effect."
                    :title "Notice"
                    :ok-text "Quit")
    (quit))))

(defun update-name-from-time (universal-time)
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time universal-time)
      (declare (ignore day second))
    (format nil "Update ~d-~2,'0d-~2,'0d (~2,'0d~2,'0d)" year month date hour minute)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD UPDATE (keep last so a revised update can be loaded if needed)

(eval-when (:load-toplevel :execute)
  (let ((*warn-if-redefine* nil)
        (*warn-if-redefine-kernel* nil))
    (let ((update (current-update)))
      (when update
        (handler-case
          (load update)
          (error ()
            (user-revert-update (format nil "The update failed to load.~&")))))))) 
               
 