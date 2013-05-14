
(in-package :mclide)

(ccl::defcontextual gui::all-package-names ()
                               (remote::remote-lisp-mode)
  (remote::remote-package-names))

(in-package :gui)

(defmethod window-controller-remote ((self apropos-window-controller))
   ;; ## can be better, should not change arbitrarily!
   (remote::remote-lisp))

(ccl::defcontextual update-symbol-list ((self apropos-window-controller))
                               (remote::remote-lisp-mode)
     (with-slots (input package shows-external-symbols symbol-list) self
         (when (plusp (length input))
             (setf symbol-list nil)
             (let ((str1 input)
                    (str2 "")
                    (pname package)
                    (external shows-external-symbols)
                    (type T)
                    (op :or))
               (remote::using-remote-lisp (window-controller-remote self) ;; ## better if added as :remote to form
                 (remote::remote-eval-enqueue-bind (result)
                                                                            (remote::remote-apropos-form op str1 str2 
                                                                                                                                 :package pname 
                                                                                                                                 :type type 
                                                                                                                                 :external external)
                             (setf symbol-list 
                                   (sort result #'string-lessp :key #'remote::remote-symbol-name))
                             ;; from objc:defmethod #/apropos: which should be redesigned to avoid conflicts
                             (execute-in-gui ;; ## possibly not required?
                              (lambda()
                               (update-apropos-array self)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIND SYMBOL DEFINITION

(in-package :mclide)

(defmethod location-source-note ((location remote::source-location))
   "return a clozure source note for the remote source location"
   (flet ((source-note (pathname &key position line)
              (declare (ignore line))
              (ccl::make-source-note :filename pathname 
                                                     :start-pos position 
                                                     :end-pos position 
                                                     :source NIL)))
     (let* ((file (remote::source-location-file location))
            (pos (remote::source-location-position location))
            (pathname (probe-file file)))
       (apply #'source-note pathname 
              (ecase (first pos)
                (:line `(:line ,(second pos) :position ,(third pos)))
                (:position pos))))))

(defmethod remote-definition-source-note (definition)
   "return a clozure source note for the remote definition"
  (let ((location (remote::remote-definition-location definition)))
    (when location
      (location-source-note location))))

(defmethod remote-definition-edit (definition &optional msg)
   (let ((type (remote::remote-definition-type definition))
          (name (remote::remote-definition-name definition))
          (source (remote-definition-source-note definition)))
     (if source
       (hemlock-ext:execute-in-file-view
        (ccl:source-note-filename source)
        (lambda ()
          (when msg (hemlock::loud-message msg))
          (hemlock::find-definition-in-buffer type name source)))
       (hemlock::editor-error "No known source for ~s" name))))

;; ## incomplete, note there is a method with the same name elsewhere...

(defmethod remote-edit-definition (name)
   (flet ((defn-print (def out)
                      ;; copied from m1, consider sharing...
                      (labels ((output (item &optional (space t))
                                 (when space
                                   (write-char #\space out))
                                 (typecase item
                                   (list (write-char #\( out)
                                         (output (car item) nil)
                                         (mapc #'output (cdr item))
                                         (write-char #\) out))
                                   (string (write-string item out))
                                   (otherwise (prin1 item out)))))
                        (output (first (remote::remote-definition-spec def)) nil))
            #+ignore
            (write def :stream out)
            #+ignore ;; like in clozure
            (format out "~s ~s" (remote::remote-definition-type def) (remote::remote-definition-name def)))
          (defn-action (definition &optional msg)
              (remote-definition-edit definition msg)))
      (let ((rsym (if (remote::remote-symbol-p name)
                         name
                         (remote::make-remote-symbol name)))
             (rpackage T)
             (msg nil))
        (remote::remote-eval-enqueue-bind (definitions)
                                          (remote::remote-definitions-form rsym rpackage)
           (cond
            ((null definitions)
             (hemlock::editor-error "No known definitions for ~s" name))
            ((not (cdr definitions))
             (defn-action (car definitions) msg))
            (T
             (when msg (hemlock::loud-message msg))
             (hemlock-ext:open-sequence-dialog
              :title (format nil "Definitions of ~s" name)
              :sequence definitions
              :action #'defn-action
              :printer #'defn-print)))))))

(ccl::defcontextual (hemlock::edit-definition remote-lisp-edit-definition) (name)
  (remote::remote-lisp-mode)
  (remote-edit-definition name))

(ccl::defcontextual (hemlock::edit-definition remote-symbol-edit-definition) (symbol)
  (remote::remote-symbol-p symbol)
  ;; intercepts call in #/definitionForSelectedSymbol on apropos-window-controller
  ;; # redundant? 
  (remote-edit-definition symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHOW ARGLIST AND DOCUMENTATION

(in-package :mclide)

(defun build-arglist-string (arglist)
  ;; code from mclide1
  (with-output-to-string (stream)
    (typecase arglist
      (string
       (let* ((end (1- (length arglist)))
              (start (and (> end 0)
                          (eql #\( (char arglist 0))
                          0 #+ignore (position-if #'whitespacep arglist :end end))))
         (if start
           (write-string arglist stream :start (1+ start) :end end)
           (write-string "()" stream))))
      (null
       (write-string "" stream))
      (otherwise (format stream "[~A]" arglist)))))

(defun build-documentation-string (documentation)
  (with-output-to-string (out)
    (flet ((output-doc (type docstring)
             (format out "~&~A:~%~A" type docstring)))
      (remote::map-documentation #'output-doc documentation))))

(defun call-with-remote-symbol-documentation (fun symbol)
   (remote::using-remote-lisp (remote::remote-lisp) ;; better if used the one in symbol... 
      #+ignore ; # for now, but this should be asserted!
      (remote::check-remote-sexp-lisp symbol)
      (let ((argstring)
            (docstring)
            (package (remote::remote-symbol-package symbol)))
        (flet ((finalize ()
                 (when (and argstring docstring)
                   (funcall fun
                     (with-output-to-string (out)
                       (when (plusp (length argstring))
                                  (write-line argstring out))
                       (write-string docstring out))))))
          (remote::remote-eval-enqueue-bind! (result) ;; ## why the !
                                             (remote::remote-symbol-arglist-form symbol package) 
                                             (setf argstring (build-arglist-string result))
                                             (check-type argstring string)
                                             (finalize))
          (remote::remote-eval-enqueue-bind (documentation)
                                            (remote::remote-symbol-documentation-form symbol t)
                                            (setf docstring (build-documentation-string documentation))
                                            (check-type docstring string)
                                            (finalize))))))

(ccl::defcontextual gui::call-with-documentation (fun symbol)
                               (remote::remote-lisp-mode)
   (when symbol
    (call-with-remote-symbol-documentation fun symbol)))      
                               
 
                               
                               

