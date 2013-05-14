;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT MENU ITEM

;; Can inspect selected text too.

(in-package :gui)

;; new, similar to objc:defmethod #/evalSelection
;; See also #/inspectSelection: in cocoa-editor.lisp

(objc:defmethod (#/inspect: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (let* ((view (hemlock-view self)))
    (when view
      (hi::handle-hemlock-event view #'(lambda ()
                                         (hemlock::editor-inspect-selection-command nil))))))

;; new, mostly copied from defcommand "Editor Execute Expression"

(in-package :hemlock) ; listener.lisp

(defcommand "Editor Inspect Selection" (p)
  "Inspects the current region in the editor Lisp."
  (declare (ignore p))
  (let* ((region (copy-region (current-form-region)))
         (form (when *echo-expression-to-listener* (region-to-string region)))
         (doc (gui::top-listener-document))
         (buf (when doc (gui::hemlock-buffer doc))))
    (when buf
      (let ((HI::*CURRENT-BUFFER* buf))
        (move-mark (current-point) (region-end (buffer-region buf)))))
    (when form (format (HEMLOCK-EXT:TOP-LISTENER-OUTPUT-STREAM) "~A~&" form))
    (inspect-region region)))

;; new, mostly copied from eval-region


(defun inspect-region (region
		    &key
		    (package (variable-value 'current-package :buffer (current-buffer)))
		    (path (buffer-pathname (current-buffer))))
  (ccl::application-ui-operation ccl:*application*
                                 :inspect-selection
                                 (list package
                                       path
                                       (region-to-string region)
                                       (mark-absolute-position (region-start region)))))

;; new, mostly copied from :eval-selection method

(in-package :gui)

(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        (operation (eql :inspect-selection))
                                        &rest args)
  (ui-object-inspect-selection o (car args)))

; new, mostly copied from ui-object-eval-selection :

(defmethod ui-object-inspect-selection ((app ns:ns-application)
				     selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection
			   app selection)))
    (when target-listener
      (destructuring-bind (package path string &optional offset) selection
        (let ((string (format nil "(ccl::inspect ~A)" string)))
          (eval-in-listener-process target-listener string :package package :path path :offset offset))))))


(defgeneric validate-menu-item (self action item)
  (:documentation "Return true if the menu item should be validated, possibly changing the menu item"))

(defmethod validate-menu-item ((self hemlock-text-view) (action (eql (@selector #/inspect:))) item)
  (let* ((doc (#/document (#/windowController (#/window self)))))
    (case (type-of doc) 
      (hemlock-listener-document
       ;; ## determines whether the listener has text in cursor position - can likely be done much better:
       (let* ((buffer (hemlock-buffer self)))
         (when buffer 
           (let* ((hi::*current-buffer* buffer)
                  (region (hemlock::current-form-region)))
             (when region
               T)))))
      (otherwise T))))

;; also handles the #/inspect: selector:

  (objc:defmethod (#/validateMenuItem: :<BOOL>)
    ((self hemlock-text-view) item)
  (let* ((action (#/action item)))
    #+debug (#_NSLog #@"action = %s" :address action)
    (cond ((eql action (@selector #/inspect:))
               (let* ((doc (#/document (#/windowController (#/window self)))))
                (case (type-of doc) 
                 (hemlock-listener-document
                  ;; ## determines whether the listener has text in cursor position - can likely be done much better:
                  (let* ((buffer (hemlock-buffer self)))
                   (when buffer 
                    (let* ((hi::*current-buffer* buffer)
                             (region (hemlock::current-form-region)))
                     (when region
                                T)))))
                 (otherwise T))))
          ((eql action (@selector #/hyperSpecLookUp:))
           ;; For now, demand a selection.
           (and *hyperspec-lookup-enabled*
		(hyperspec-root-url)
                (not (eql 0 (ns:ns-range-length (#/selectedRange self))))))
          ((eql action (@selector #/cut:))
           (let* ((selection (#/selectedRange self)))
             (and (> (ns:ns-range-length selection))
                  (#/shouldChangeTextInRange:replacementString: self selection #@""))))
          ((eql action (@selector #/evalSelection:))
           (when (hemlock-view self)
             (if (eql 0 (ns:ns-range-length (#/selectedRange self)))
               ;; Should check whether there is a current form
               (#/setTitle: item #@"Execute Expression")
               (#/setTitle: item #@"Execute Selection"))
             t))
          ((eql action (@selector #/evalAll:))
           (let* ((doc (#/document (#/windowController (#/window self)))))
             (and (not (%null-ptr-p doc))
                  (eq (type-of doc) 'hemlock-editor-document))))
          ;; if this hemlock-text-view is in an editor window and its buffer has
          ;; an associated pathname, then activate the Load Buffer item
          ((or (eql action (@selector #/loadBuffer:))
               (eql action (@selector #/compileBuffer:))
               (eql action (@selector #/compileAndLoadBuffer:))) 
           (let* ((buffer (hemlock-buffer self))
                  (pathname (hi::buffer-pathname buffer)))
             (not (null pathname))))
          ((eql action (@selector #/openSelection:))
           (let* ((text (#/string self))
                  (selection (#/substringWithRange: text (#/selectedRange self))))
             (pathname-for-namestring-fragment (lisp-string-from-nsstring selection))))
	  (t (call-next-method item)))))


