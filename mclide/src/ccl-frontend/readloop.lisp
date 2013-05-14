;; Low level modifications to use a remote lisp as implementation for the readloop of a listener.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISTENER STREAM

(in-package "GUI")

(defmethod pop-cur-string ((stream cocoa-listener-input-stream))
  (with-slots (queue queue-lock read-lock queue-semaphore text-semaphore cur-string cur-string-pos cur-sstream
               cur-env source-map cur-offset) stream           
    (with-lock-grabbed (read-lock)
      (when (and cur-string
                 (< cur-string-pos (length cur-string)))
        (let ((expression cur-string))
          (setq cur-string "" 
                cur-string-pos 0 
                cur-offset 0)
          expression)))))

(defun remote-dispatcher (repl &optional (*standard-output* *standard-output*))
 (let ((remote::*remote-eval-dispatch-swank* T)))
  (remote::dispatch-available-input repl))

; like stream-read on the listener in MCLIDE1

(ccl::defcontextual ccl::read-toplevel-form ((stream cocoa-listener-input-stream) &rest keys)
                               (remote::remote-lisp-mode)
  (declare (ignore keys))
  ;; Make sure grabbing lock doesn't conflict with the read-string-handler callback!
  (flet ((string-trim-trailing-newline (string)
             "trim exactly one newline trailing the string"
             (let ((index (length string)))
                   (if (eql (aref string (1- index)) #\newline)
                        (subseq string 0 (1- index))
                        string))))
  (with-slots (queue queue-lock read-lock queue-semaphore text-semaphore cur-string cur-string-pos cur-sstream
               cur-env source-map cur-offset) stream
     (loop
       (let ((expression+enter (pop-cur-string stream)))
         (when expression+enter
           (let ((expression (string-trim-trailing-newline expression+enter))
                 (remote::*remote-eval-dispatch-swank* T))
             (remote::remote-repl-call (remote::remote-lisp) expression))
           (return (values nil nil nil))))
      #+ignore (wait-on-semaphore queue-semaphore nil "Toplevel Read")
       ;; # consider avoiding polling by combining semaphores
       (loop
         until (timed-wait-on-semaphore queue-semaphore 1/20)
         do (remote-dispatcher (remote::remote-lisp))
         finally
         (let ((val (with-lock-grabbed (queue-lock) (pop queue))))
           (with-lock-grabbed (read-lock)
             (cond ((stringp val)
                    (assert (timed-wait-on-semaphore text-semaphore 0) () "text/queue mismatch!")
                    (setq cur-string val cur-string-pos 0))
                   (t 
                    (destructuring-bind (string package-name pathname offset) val
                      ;; # environment should be passed to remote lisp, but pathnames need to be proper
                      (let ((env (cons '(*loading-file-source-file* *load-pathname* *load-truename*)
                                       (list pathname pathname (and pathname (or (probe-file pathname) pathname))))))
                        (when package-name
                          (push '*package* (car env))
                          (push (ccl::pkg-arg package-name) (cdr env)))
                        (setq cur-string string cur-string-pos 0 cur-env env))))))))))))

;; # consider moving these to the listener and rename without the repl part

(defmethod repl-listener-output-stream ((repl remote::remote-repl))
  (let* ((listener (mclide::remote-repl-listener repl))
         (process (mclide::listener-process listener)))
    (cond
     (process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-output-stream process))
     (T ;; really an error
      (hemlock-ext:top-listener-output-stream)))))

(defmethod repl-listener-input-stream ((repl remote::remote-repl))
  (let* ((listener (mclide::remote-repl-listener repl))
         (process (mclide::listener-process listener)))
    (cond
     (process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-input-stream process))
     (T ; really an error
      (hemlock-ext:top-listener-input-stream)))))
   
(defmethod remote::write-string-handler ((repl remote::remote-repl) string)
  (let ((stream (repl-listener-output-stream repl))) ;; # should ensure it's the correct one for the repl
       (write-string string #+ignore (substitute #\newline #\linefeed string) stream)
       (force-output stream)))

(defmethod remote::read-string-handler ((repl remote::remote-repl))
    (let ((stream (repl-listener-input-stream repl)))
      ; cocoa-listener-input-stream
      ; see pop-cur-string for details
      (read-line stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BREAK/DEBUG

(in-package :ccl)

(defmethod repl (stream #+ignore(stream ui::cocoa-listener-output-stream)) 
   (let ((listener (gui::first-window-satisfying-predicate  ; ## what a mess...
                         (lambda (listener)
                           (let ((process (mclide::listener-process listener)))
                             (and process (eq (gui::cocoa-listener-process-output-stream process) stream)))))))
     (mclide::listener-remote listener))
  #+ignore ; ad-hoc version...
  (remote::remote-lisp))

(defparameter *remote-backtrace* nil)

;; ## call-in-debug and bind-in-debug can be made portable by turning the latter into a method!
;; very similar to the one in mclide1 - # extract and put in remote

(defun call-in-debug (fun listener thread level condition restarts frames)
  (let* (#+ignore (label (remote::remote-implementation-type-name (repl listener)))
         (%restarts%
          (flet ((build-restart (restart n)
                   (destructuring-bind (name description) restart
                     (make-restart nil
                                   (cond
                                    ((equalp name "continue")
                                     'continue)
                                    (t
                                     ; consider gensym...
                                     (intern name :keyword)))
                                   (lambda (&rest rest)
                                     (declare (ignore rest))
                                     (restart-case
                                      (remote::repl-invoke-nth-restart (repl listener) thread level n)
                                      (abort ()
                                         ; # perhaps better if slime-eval supported the abort-continuation instead of calling (abort) ?
                                         (return-from call-in-debug)))
                                     #+ignore ; doesn't make a difference
                                     (return-from call-in-debug))
                                   :report-function 
                                   (lambda (out)(format out "~A" description #+ignore label))))))
            (cons (loop for n from 0
                        for restart in restarts
                        collect (build-restart restart n))
                  %restarts%)))
         (*remote-backtrace*
            (remote::make-remote-backtrace 
                              :repl (repl listener)
                              :thread thread
                              :frames frames 
                              :condition condition))
         ; set in break-loop. so possibly redundant...
         (*continuablep* (find-restart 'continue)))
      (funcall fun)))

;; from mclide1, should be moved to :remote module with call-in-debug as a method, perhaps a var to hold %restarts% for portability?

(defmacro bind-debug ((listener thread level condition restarts frames) &body body) 
  `(flet ((body ()
            ,@body))
     (call-in-debug #'body ,listener ,thread ,level ,condition ,restarts ,frames)))

;; almost same as in MCLIDE1...

(defun ride-break-loop (listener level condition) 
  (labels ((feedback (format &rest args)
             (apply #'format listener format args)))
    (handler-case
      ; using the MCL break loop...
      (let ((*break-level* (1- level)))
        (break-loop (make-condition 'swank-client::swank-condition 
                                    :format-control "~A" :format-arguments (cdr condition))))
      (swank-client::swank-condition (c)
                       (format *terminal-io* "~&BREAK (~A)~%" c)
                       (remote::swank-abort listener)
                       ))))

(defun debug-setup (listener thread level condition restarts frames conts)
  (declare (ignore conts))
  (when (= *break-level* (1- level))
   (let ((repl (remote::remote-lisp));; # should ensure it's the correct one for the repl
          (%restarts% (if swank-client::*debug* %restarts% NIL))) ; eliminate top level restarts
     (restart-bind ;; ## maybe can do like this also for mcl?
         (#+ignore
          (abort-break
           (lambda ()
             #+ignore (format listener "~&[ABORT BREAK IN DEBUG SETUP]~&")
             ; abort on toplevel
             ;(swank-throw-to-toplevel listener)
             ; (swank-abort listener)
             (return-from debug-setup t)))
          ; Used by the exit break menu item:
          (abort 
              (lambda ()
                ; abort on level 1 will kill thread, so throw-to-toplevel instead!
                (if (eq level 1)
                  (remote::swank-throw-to-toplevel repl)
                  (remote::swank-abort repl))
                (return-from debug-setup t))
            :report-function (lambda (stream)
                               (format stream "Exit break level ~A." level)))

          (toplevel 
           (lambda ()
             (remote::swank-throw-to-toplevel repl))
           :report-function (lambda (stream)
                              (format stream "Exit debug."))))

        (bind-debug (listener thread level condition restarts frames)
           (format listener "Error in ~A process ~A: ~A~%"
                        (remote::remote-implementation-type repl) 
                        thread 
                        (remote::fix-foreign-string (car condition)))     
           (ride-break-loop listener level condition)))
     T)))

(defun debug-return (thread level)
   (setf *break-level* (1- level)))

(defmethod remote::remote-listener-process ((repl remote::remote-repl))
  (gui::hemlock-document-process (#/topListener gui::hemlock-listener-document))) ;; ### not correct, find from repl!

(defmethod remote::debug-setup-handler ((repl remote::remote-repl) thread level condition restarts frames conts)
   ; Note: remote-eval should never be called before binding debug context, as it calls swank-client-dispatch-next!
   ;; ## sometimes fails with unbound *eval-wait-queue* - maybe we cannot be sure the repl is in eval-wait like during inspect? 
  (let ((remote::*remote-lisp* repl)) ;; ## see better way to do this - but likely in process anyway so assert!
    (debug-setup (gui::repl-listener-output-stream repl) thread level condition restarts frames conts)))

(defmethod remote::debug-return-handler ((repl remote::remote-repl) thread level stepping)
  (declare (ignore stepping))
  (let ((remote::*remote-lisp* repl)) ;; ## see better way to do this - but likely in process anyway so assert!
    (debug-return thread level)))



