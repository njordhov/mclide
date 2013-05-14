(in-package :ccl)

(compile-load "ccl:examples;anticipat-symbol-complete")

(defparameter *Wait-Time-for-Anticipatory-Symbol-Complete* 0.1)

(defparameter *completion-promise* NIL)

(defparameter *Zero-Completion-Hook* 
  (lambda () 
    (when  *completion-request*
      (set-mini-buffer (fred-instance *completion-request*) "No matching symbols..."))))

(defparameter *completion-request* NIL)

(defparameter *use-partial-completion* t)

(defparameter *symbol-completed* NIL)

(defmethod INITIALIZE-INSTANCE :around ((Self completion-request) &rest Args)
  (declare (ignore args))
  (handler-case
    (call-next-method)
    (error (c) ; like if the package of the modeline doesn't exist
     (ccl::log-condition c "Failed to create completion request"))))

(defcontextual ANTICIPATORY-SYMBOL-COMPLETE ((Self completion-request))
  (remote::remote-lisp-mode)
  (handler-case
    (let* ((*completion-request* self)
           (string 
            (completion-string self)
            #+ignore ;; messes with case
            (completion-name Self))
           (package
            (remote-package-name)
            #+ignore ; fails on dialogs
            (view-package-string (view-window (fred-instance self)))
            #+ignore
            (package-name (completion-package Self)))
           (*completion-promise* 
            (remote::promise 
             (remote::simple-completions-form string package)))
           (*symbol-completed* NIL))
      (call-next)
      (when (and *use-partial-completion*
               *symbol-completed*)
        (continue-completion self)))
    (error (c)
     (ccl::log-condition c "Failed to complete symbol"))))

(defcontextual apropos-list (string &optional package)
 (and *completion-promise*
      (remote::remote-lisp-mode))
 (let* ((all (handler-case
                   (mapcar #'remote::make-remote-symbol 
                           (mapcar #'string-upcase
                                   (car (remote::fulfill *completion-promise* :timeout (* 5 60)))))
                   (remote::promise-not-fulfilled ()
                    (check-type package string) ;; ??  perhaps package should be (remote-package-name)
                    (list (remote::make-remote-symbol string package)))))
        (editor (fred-instance *completion-request*))
       (mini (view-mini-buffer editor))
       (symbols (sort (matching-prefix-symbols string all)
                      #'< :key (lambda (s)(length (string s)))))
       (symbols- (delete string symbols :test #'string-equal :key #'string)))
      (when (and mini
                 symbols-)
       ; (stream-fresh-line mini)
       (set-mini-buffer editor "~&~{~a ~}" symbols)
       (mini-buffer-show-cursor mini) 
       (force-output mini))
      (if *use-partial-completion*
            (if symbols-
               (list (remote::make-remote-symbol (LONGEST-PREFIX symbols-)))
               symbols)
          symbols)))

(defcontextual buffer-current-string (buffer position)
           T
       ; compensate for bug in buffer-current-string - it includes the '(' when at zero position in editor:
  (declare (ignore buffer position))
  (multiple-value-bind (string start end) (call-next)
    (when (zerop start)
      (incf start)
      (setf string (subseq string 1)))
    (values string start end)))

; rename or integrate
(defun continue-completion (self)
  ; alternatively delete char then insert it...
  (let ((editor (fred-instance self)))
    (multiple-value-bind (String Start End)
                         (buffer-current-string (fred-buffer editor) 
                                                (- (buffer-position (fred-buffer editor)) 1))
      (when (> (length String) 1)
       (flet ((%restart ()
                (start-symbol-complete-process 
                 (make-instance 'completion-request 
                                :completion-string String
                                :fred-instance editor
                                :fred-buffer-start Start
                                :fred-buffer-end End))))
         (process-run-function "restart" #'%restart))))))

(defcontextual SHOW-IN-OVERLAY-WINDOW (text position)
           (remote::remote-lisp-mode)
  ; hack to catch when user completes a symbol
  (let ((char (call-next)))
    (when (eql char #\tab)
      (setf *symbol-completed* T))
    char))
          



