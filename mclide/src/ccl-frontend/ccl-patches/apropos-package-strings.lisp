;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apropos manages packages as strings instead of package objects

(in-package :gui) ; apropos-window.lisp

;; reset:
;; (setf gui::*apropos-window-controller* NIL)

;; # new

(defun all-package-names ()
  (mapcar #'package-name (list-all-packages)))

(objc:defmethod (#/becomeFirstResponder :<BOOL>) ((self package-combo-box))
  (with-slots (packages) self
    (setf packages (coerce (all-package-names) 'vector))
    (setf packages (sort packages #'string-lessp)))
  (call-next-method))

#+ignore
(objc:defmethod (#/setPackage: :void) ((self apropos-window-controller)
				       sender)
  (with-slots (combo-box package) self
    (assert (eql sender combo-box))
    (with-slots (packages) sender
      (let ((index (#/indexOfSelectedItem sender)))
	(if (minusp index)
	  (setf package nil)		;search all packages
	  (setf package (svref packages index))))))
  (update-symbol-list self)
  (update-apropos-array self))

(objc:defmethod #/comboBox:objectValueForItemAtIndex: ((self apropos-window-controller)
						       combo-box
						       (index :<NSI>nteger))
  (with-slots (packages) combo-box
    (let* ((pkg-name (svref packages index)))
      (if pkg-name
	(#/autorelease (%make-nsstring pkg-name))
	+null-ptr+))))

(objc:defmethod #/comboBox:completedString: ((self apropos-window-controller)
					     combo-box
					     partial-string)
  (flet ((string-prefix-p (s1 s2)
	   "Is s1 a prefix of s2?"
	   (string-equal s1 s2 :end2 (min (length s1) (length s2)))))
    (with-slots (packages) combo-box
      (let* ((s (lisp-string-from-nsstring partial-string)))
	(dotimes (i (length packages) +null-ptr+)
	  (let ((name (svref packages i)))
	    (when (string-prefix-p s name)
	      (return (#/autorelease (%make-nsstring name))))))))))

(objc:defmethod (#/comboBox:indexOfItemWithStringValue: :<NSUI>nteger)
    ((self apropos-window-controller)
     combo-box
     string)
  (with-slots (packages) combo-box
    (let* ((s (lisp-string-from-nsstring string)))
      (or (position s packages :test #'string-equal)
	  #$NSNotFound))))

;; ## Potential problem if the number of packages changes?

(objc:defmethod (#/numberOfItemsInComboBox: :<NSI>nteger) ((self apropos-window-controller)
						   combo-box)
  (declare (ignore combo-box))
  (length (all-package-names)))
