
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split out finding symbol documentation

(in-package :gui)

;; new # rename to call-with-documentation

(defun call-with-documentation (fun sym)
   (let ((info (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t)))
	  (with-output-to-string (s info)
	    (dolist (doctype '(compiler-macro function method-combination
			       setf structure t type variable))
	      (let ((docstring (documentation sym doctype)))
		(when docstring
		  (format s "~&~a" docstring))
		(when (eq doctype 'function)
		  (format s "~&arglist: ~s" (arglist sym))))))
	  (funcall fun info)))

(objc:defmethod (#/tableViewSelectionDidChange: :void) ((self apropos-window-controller) notification)
  (let* ((tv (#/object notification))
         (row (#/selectedRow tv)))
    (unless (minusp row)
     (with-slots (array-controller symbol-list text-view) self
       (flet ((selected ()
                (let* ((number (#/valueForKeyPath:
                                array-controller #@"selection.index"))
                       (i (#/intValue number))
                       (sym (elt symbol-list i)))
                  sym)))
          (let ((sym (selected)))
            (flet ((set-documentation-field (info)
                       (check-type info string)
                       (execute-in-gui ;; ## possibly not required?
                        (lambda ()
                          (when (eq sym (selected))
                              (if (plusp (length info))
                                (#/setString: text-view (#/autorelease (%make-nsstring info)))
                                (#/setString: text-view #@"")))))))
               (#/setString: text-view #@"")
               (call-with-documentation #'set-documentation-field sym))))))))
