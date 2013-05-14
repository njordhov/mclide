;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECTOR

(in-package :gui)

;; method to customize the inspector title and label depending on the inspected object

(defmethod (setf inspector-item) ((ii inspector-item) (wc ninspector-window-controller))
  (setf @@@ @@
        @@ @
        @ (inspector-object ii))
  (setf (slot-value wc 'inspector-item) ii)
  (let ((w (#/window wc)))
    (multiple-value-bind (title label)
                         (inspector-title (lisp-inspector ii))
     (#/setTitle: w (%make-nsstring (concatenate 'string  "Inspect: " title)))
     (#/setStringValue: (object-label wc) (%make-nsstring (or label title)))
     (#/reloadData (table-view wc)))))

(defmethod inspector-title (li)
   (let ((ob (inspector::inspector-object li))
          (*print-readably* nil)
          (*signal-printing-errors* nil)
          (*print-circle* t)
          (*print-length* 20)
          (*print-pretty* nil))
         (prin1-to-string ob)))

;; refresh replaces current inspected object rather than pushing to the history:

(objc:defmethod (#/doRefresh: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender))
  (let ((inspector::*inspector-disassembly* t))
    (set-enabled wc)
    (setf (inspector-item wc)
          (make-inspector-item (inspector::refresh-inspector (lisp-inspector wc))))))
