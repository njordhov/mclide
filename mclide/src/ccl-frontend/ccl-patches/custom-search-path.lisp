;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom default search path

(in-package :gui)

(defparameter *search-files-default-path* "ccl:")

(defun set-search-files-default-dir (wc)
  (let* ((w (first-window-satisfying-predicate #'window-pathname))
         (path (and w (window-pathname w)))
         (dir (if path
                (namestring (ccl::back-translate-pathname (directory-namestring path)))
                *search-files-default-path*))
         (nsdir (#/autorelease (%make-nsstring dir))))
    (with-slots (folder-combo-box) wc
      (#/setStringValue: folder-combo-box nsdir)
      (#/updateFolderString: wc folder-combo-box))))
