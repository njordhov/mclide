;;; -*- Mode: LISP; Package: CCL; -*-
;;;
;;; Modifies the Clozure inspector to target remote implementations.
;;; 
;;; Copyright (C) 2011 Terje Norderhaug <terje@in-progress.com>
;;;
;;; You are hereby granted the rights to distribute and use this
;;; software as governed by the terms of the Lisp Lesser GNU Public
;;; License (http://opensource.franz.com/preamble.html), known as the
;;; LLGPL. Other free licenses available upon request.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT

;; Requests to inspect an expression is passed to the remote implementation, which 
;; passes back a swank event to activate the inspector for the decoded value.

(in-package :gui)

(ccl::defcontextual ui-object-inspect-selection ((app ns:ns-application)
				     selection)
  (remote::remote-lisp-mode)
  (let* ((target-listener (ui-object-choose-listener-for-selection
			   app selection)))
    (when target-listener
      (destructuring-bind (package path string &optional offset) selection
        (declare (ignore path))
        (remote::remote-eval-enqueue-bind (result)
                                          (remote::open-sexp-inspector-form ;; ## consider new function for remote!
                                           (remote::make-remote-sexp (subseq string offset) package))
          (when result
            (destructuring-bind (key &optional reason) result
              (assert (eq key :error))
              ;; ## better if this opens an empty inspect dialog!
              (easygui::message-dialog (format nil "Nothing to inspect (~A)" reason))))
          (values))))))

(ccl::defcontextual inspect (symbol)
  (remote::remote-symbol-p symbol)
  ; intercepts call in #/inspectSelectedSymbol on the apropos-window-controller
  (remote::remote-eval-enqueue-bind (response)
                                    (remote::open-sexp-inspector-form symbol)
    (declare (ignore response))
    ;; ## check for error response?
     nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECTOR

;; Entry point for gui inspect: function ninspect (object)
;; passes control to make-inspector (object)

(in-package :inspector) ;; should be mclide?

(defparameter *bypass-mclide-inspector* NIL)

(defclass remote-inspector (inspector::usual-inspector)
   ((lines)))

(defmethod inspector::inspector-class ((object remote::remote-inspected-object))
   "Default inspector class to use for remote inspections"
  (if *bypass-mclide-inspector*
    (call-next-method)
    'remote-inspector))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRESENTATION

;; ## much is from mclide1 and should be updated/migrated

(defmethod prin1-line ((i remote-inspector) stream (value inspector::remote-inspected-item) 
                       &optional presentation type dummy)
  (declare (ignore dummy))
  (assert (eq type :presentation))
  (multiple-value-bind (type-sym)
                       (inspector::parse-type i type)
    (declare (ignore type-sym))
    (dolist (token presentation)
      (typecase token
        (string
         (write-string token stream))
        (list
         (case (first token)
           (:value
            (write-string (second token) stream))
           (:action NIL)))))))

(defmethod compute-line-count ((i remote-inspector) &aux (object (inspector::inspector-object i)))
  (with-slots (lines) i
     (setf lines (remote::remote-inspected-items object))
     (length lines))) 

(defmethod line-n ((i remote-inspector) n)
  "Return three values: the value, label, and type of the nth line of the object"
  (with-slots (lines) i
    (presentation (nth n lines))))

(defmethod (setf line-n) (value (i remote-inspector) n)
  (easygui::message-dialog "Cannot yet modify a remotely inspected value."))

(defmethod line-n-inspector ((i remote-inspector) n value label type)
   ; Called when building the items for the inspector (rather than when inspecting a line in the inspector as in mclide1)
  (declare (ignore label value type))
  (with-slots (lines) i
    (let ((selected (nth n lines)))
      (make-inspector selected))))

(defun presentation-token-type (token)
  (typecase token
    (cons (car token))
    (string
     (cond
      ((not (find-if-not #'ccl::whitespacep token))
       NIL)
      ((and (eql #\- (schar token 0))
            (not (find #\- token :test-not #'eql)))
       :separator)
      ((let ((end (position-if (lambda (c)(member c '(#\. #\: #\=))) token
                               :from-end T))
             (start (position-if-not #'ccl::whitespacep token)))
         (and end 
              (not (find-if-not #'ccl::whitespacep token :start (1+ end)))
              ; not a sentence (period but only one word)
              (or (not (eql (schar token end) #\.))
                  (not (find-if #'ccl::whitespacep token :start start :end end)))))
       :label)
      (T :text)))))

;; # candidate for remote module?

(defun presentation-values (tokens)
  "Extract common patterns out of the presentation tokens"
  (flet ((value (token)
           (case (first token)
             (:value
              (second token))
             (:action "")))
         (ignorable-p (token)
           (typecase token
              (string (not (find-if-not #'ccl::whitespacep token))) 
              (null T)
              (cons (eq :action (first token))))))

    (let* ((tokens (remove-if #'ignorable-p tokens))
           (pattern (mapcar #'presentation-token-type tokens)))
      (cond
       ((equal '(:text) pattern)
        (values :comment (first tokens)))
       ((equal '(:value) pattern)
        (values :value (value (first tokens))))
       ((equal '(:separator) pattern)
        (values :separator))
       ((equal '(:label) pattern)
        (values :header (first tokens)))
       ((equal '(:label :value) pattern)
        (let ((label (string-left-trim '(#\space) 
                       (string-right-trim '(#\. #\: #\= #\space) (first tokens))))
              (value (value (second tokens))))
          (if (find-if-not #'ccl::whitespacep label) 
            (values :property label value)
            (values :value value))))
       (T
        (apply #'values :presentation tokens))))))

(defmethod presentation ((line remote-inspected-item))
  "Returns values for call to prin1-line or custom inspector-print-function: value label type"
  ;; #  reconsider name of method
  ; Valid types per MCL are:
  ;  :NORMAL or NIL  - a normal constituent line with label and value: changeable
  ;  :COLON          - a normal line with ": " between the label and the value
  ;  :COMMENT        - a commentary line - Print only the label
  ;  :STATIC         - a commentary line with an inspectable value: not changeable
  ; Or a list whose first element is one of the types above and whose
  ; second and third elements are font-specs for the label & value, respectively.
  ; If there is no third element, then the second element is the font-spec.
  ; for both the label and value.
  ; These types can be extended with specialized inspector-print-function methods!
    (destructuring-bind (type &rest content)
                        (multiple-value-list (presentation-values (remote-inspected-item-tokens line))) 
      (ecase type
        (:separator
         (values nil nil :comment))
        (:header
         (destructuring-bind (value) content
           (values nil value :comment)))
        (:comment
         (destructuring-bind (value) content
           (values nil value '(:comment (:bold)))))
        (:value
         (destructuring-bind (value) content
           (values (swank-client::make-remote-object :value value) nil :normal)))
        (:property
         (destructuring-bind (label value) content
           (values (swank-client::make-remote-object :value value) label :colon)))
        (:presentation
         (values line content :presentation))
        (otherwise
           (values (swank-client::make-remote-object :value (or (inspection-value line) ""))
                   (inspection-title line)
                   (inspection-mode line))))))

(defmethod gui::inspector-title ((i inspector::remote-inspector))
  (let ((object (inspector::inspector-object i)))
    (check-type object remote::remote-inspected-object)
    (values
     (or (inspection-title object)
           "Remote Object")
     (or (inspection-label object) 
           (inspection-title object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HISTORY/NAVIGATION 

(defmethod remote-inspection-session ((wc gui::ninspector-window-controller))
  (let ((current (gui::inspector-item wc)))
    (remote::inspection-session 
     (inspector::inspector-object 
      (gui::lisp-inspector current))))) 

(defmethod (setf gui::inspector-item) :around ((ii gui::inspector-item) (wc gui::ninspector-window-controller))
   ;; intercepting call from push-inspector-item
  (let ((inspector (gui::lisp-inspector ii)))
      (typecase inspector
       (remote-inspector
         (let* ((selected (inspector-object inspector)))
          (typecase selected
            (remote-inspected-item
             (if inspector::*inspector-disassembly* 
               ;; in push-inspector-item of #/doRefresh:
               (remote::remote-reinspect-current
                 (remote-inspection-session wc)
                 (lambda (object)
                   (setf (slot-value object 'label) (inspection-label selected)) ;; ## cover in remote?
                   (setf (inspector-object inspector) object)
                   (call-next-method)))
               ;; in push-inspector-item of #/inspectSelectionInPlace:
               (remote::remote-inspect-part 
                 (remote-inspection-session wc)
                ; (remote::inspection-session selected)
                (inspection-id selected)
                (lambda (object)
                  (setf (slot-value object 'label) (inspection-label selected)) ;; ## cover in remote?
                  (setf (inspector-object inspector) object)
                  (call-next-method)))))
            (otherwise
             (call-next-method)))))
       (otherwise
        (call-next-method)))))

(defmethod gui::set-current-inspector-item :around ((wc gui::ninspector-window-controller) index)
  (flet ((change-history (direction)
          (with-slots (gui::viewed-inspector-items) wc
            (let* ((ii (aref gui::viewed-inspector-items index))
                   (inspector (gui::lisp-inspector ii)))
              (funcall
               (ecase direction
                 (:backward #'remote::remote-inspection-pop)
                 (:forward #'remote::remote-inspection-next))
               (remote-inspection-session wc)
               (lambda (object)
                 (setf (inspector::inspector-object inspector) object)
                 (call-next-method)))))))
    (let ((current (1- (gui::next-index wc))))
      (cond
       ((= index (1- current)) ; back button
        (change-history :backward))
       ((= index (1+ current)) ; forward button
        (change-history :forward))
       (T
        (call-next-method))))))

;; ## migrate to remote:

(defmethod inspection-label ((item remote-inspected-item))
  (or (call-next-method)
        (inspection-value item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT IN NEW WINDOW

(ccl::defcontextual gui::make-inspector-window (inspector)
  ; bypassing creating window on #/inspectSelectionInNewWindow: event
  (ignore-errors ; ## should be eliminated
   (and (typep inspector 'remote-inspector)
        (typep (inspector-object inspector) 'remote-inspected-item)))
  (let* ((item (inspector-object inspector))
         (session (remote::inspection-session item)))
    (remote::show-part-inspector session (inspection-id item))))



