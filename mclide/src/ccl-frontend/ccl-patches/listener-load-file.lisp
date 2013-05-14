;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  #/loadFile can be specialized for different listeners

(in-package :gui)

(defmethod listener-load-file ((doc hemlock-listener-document))
  (let ((filename (cocoa-choose-file-dialog
		   :button-string "Load"
		   :file-types (list (pathname-type *.lisp-pathname*)
				     (pathname-type *.fasl-pathname*)))))
    (when filename
      (let ((process (hemlock-document-process doc)))
	(process-interrupt process #'(lambda ()
				       (load filename)
				       (fresh-line)))))))

(objc:defmethod (#/loadFile: :void) ((self lisp-application-delegate) sender)
  (declare (ignore sender))
  (#/ensureListener: self nil)
  (let ((doc (#/topListener hemlock-listener-document)))
    (listener-load-file doc)))
