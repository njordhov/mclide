;; The search dialog of Clozure 1.7 will fail if the "in folder" field is empty.
;; The fix is to explicitely test whether the length of the text is positive.

(in-package :gui)

#+ignore ; original
(defmethod folder-valid-p ((wc search-files-window-controller))
  (let* ((nsstr (folder-string-value wc)))
    (when (and (typep nsstr ns:ns-string) (#/length nsstr))
      (let ((lstr (lisp-string-from-nsstring nsstr)))
        (when (valid-host-p lstr)
          (probe-file (get-full-dir-string lstr)))))))

(defmethod folder-valid-p ((wc search-files-window-controller))
  (let* ((nsstr (folder-string-value wc)))
    (when (and (typep nsstr ns:ns-string) 
               (plusp (#/length nsstr)))
      (let ((lstr (lisp-string-from-nsstring nsstr)))
        (when (valid-host-p lstr)
          (probe-file (get-full-dir-string lstr)))))))
