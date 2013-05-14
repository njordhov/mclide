;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gui)

; decreasing *cocoa-listener-count* causes listener name conflicts when creating new listener after closing another.
; Fix: Determine whether there are no listeners by checking for toplisteners instead of using *cocoa-listener-count*

(objc:defmethod (#/dealloc :void) ((self hemlock-listener-document))
  (if (not (#/topListener hemlock-listener-document)) #+ignore (zerop (decf *cocoa-listener-count*))
    (setq *next-listener-x-pos* nil
          *next-listener-y-pos* nil))
  (let* ((p (shiftf (hemlock-document-process self) nil)))
    (when p
      (process-kill p)))
  (call-next-method))
