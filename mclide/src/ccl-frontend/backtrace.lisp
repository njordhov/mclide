(in-package :gui)

(defparameter *use-internal-backtrace* nil
  "Set to a non-nil value during debug to show the internal backtrace")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| in cocoa-listener.lisp

(objc:defmethod (#/backtrace: :void) ((self hemlock-listener-document) sender)
  (let* ((process (hemlock-document-process self)))
    (when process
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/makeKeyAndOrderFront: (#/windowForSheet self) nil)
          (#/showWindow: (backtrace-controller-for-context context) sender))))))
|#

(defparameter *context->remote-backtrace* (make-hash-table :test 'eq :weak T))

(defun context-remote-backtrace (context)
  (gethash context *context->remote-backtrace*))

(defmethod listener-backtrace-context :around ((proc cocoa-listener-process))
  (let ((context (call-next-method)))
   (when (remote::remote-lisp-mode)
     (let ((rbt (symbol-value-in-process 'ccl::*remote-backtrace* proc)))
       #+ignore (setf (ccl::bt.dialog context) rbt)
       (setf (gethash context *context->remote-backtrace*) rbt)))
    context))

;; new ## (but isn't this a slot for remote-stack-descriptor, so why do we need this here?):

(defmethod stack-descriptor-remote-backtrace ((stack-descriptor stack-descriptor))
  (context-remote-backtrace (stack-descriptor-context stack-descriptor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gui)

(defclass remote-frame-descriptor (frame-descriptor remote::remote-frame)
  ())

;; consider splitting into two - one that gets the remote locals and another to eventually update the display. 
;; This may lead to less latency as the query can be queued before display.

(defmethod enqueue-remote-update ((fd remote-frame-descriptor) &key ov-item)
  ; the frame descriptor should be in the cache before this is called
    ;; ## consider using promise instead!
    ;; ideally this should be delayed until dialog is displayed!
  (remote::remote-eval-enqueue-bind (frame) 
                                    (remote::frame-locals-form fd)
        (flet ((convert-remote-frame-local (item)
                 (destructuring-bind (&key name id value &allow-other-keys) item 
                   (declare (ignore id))
                   (list* (format nil "~A ~A" name value) name value))))
          (with-slots (values) fd
            (setf values
                  (apply #'vector (mapcar #'convert-remote-frame-local (car frame)))))
          (update-ov-item ov-item fd))))

(defmethod make-ov-item ((fd remote-frame-descriptor) &key container)
  (check-type container remote-stack-descriptor)
  (let* ((ov (call-next-method)))
   (enqueue-remote-update fd :ov-item ov)
   ov))

(defmethod remote-edit-definition ((frame remote-frame-descriptor)) 
   (with-slots (label) frame
     (let ((id (remote::frame-number frame)))
      (remote::remote-eval-enqueue-bind (location)
                                        (remote::query-form
                                         (remote::frame-source-location-form id)
                                         :post-processing #'remote::extract-source-location)
        (when location
          (let ((source (mclide::location-source-note location)))
            ; from gui::show-frame-source
            (if (source-note-p source)
              (hemlock-ext:execute-in-file-view
                 (ccl:source-note-filename source)
                 (lambda ()
                    (hemlock::move-to-source-note source))))))))))

(ccl::defcontextual show-frame-source (frame)
                               (typep frame 'remote-frame-descriptor)
 (remote-edit-definition frame))

(defclass inspected-remote-frame-var (remote::remote-frame-var)
  ()
  (:documentation "A remote frame var passed to be inspected"))

(ccl::defcontextual frame-descriptor-value (frame index)
                                           (typep frame 'remote-frame-descriptor)
  (make-instance 'inspected-remote-frame-var :frame frame :index index))

(ccl::defcontextual (inspect :remote-frame-var) (remote-frame-var)
  (typep remote-frame-var 'inspected-remote-frame-var)
  ;; expected to be called from objc:defmethod #/inspect: backtrace-window-controller
  (remote::remote-eval-enqueue-bind (object)
                                    (remote::show-frame-var-inspector-form remote-frame-var)
    (declare (ignore object))
     NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gui)

(defclass remote-stack-descriptor (stack-descriptor)
   ((remote-backtrace :reader stack-descriptor-remote-backtrace :initarg :remote-backtrace)))

(defmethod count-stack-descriptor-frames ((sd remote-stack-descriptor))
  (if *use-internal-backtrace*
    (call-next-method)
    (length (remote::all-backtrace-frames (stack-descriptor-remote-backtrace sd)))))

; consider patching make-frame-descriptor instead! But then collect-stack-frames also need patching...

(defmethod stack-descriptor-frame ((sd remote-stack-descriptor) index)
  (if *use-internal-backtrace*
    (call-next-method)
    (let ((cache (stack-descriptor-frame-cache sd)))
      (or (gethash index cache)
          (let* ((backtrace (stack-descriptor-remote-backtrace sd))
                 (item (nth index (remote::all-backtrace-frames backtrace)))
                 (frame (remote::backtrace-remote-frame backtrace (first item)
                              'remote-frame-descriptor :data item :label (second item) :values (vector))))
            (setf (gethash index cache) frame)
            frame)))))
    
;; ### ccl would be better if make-stack-descriptor din't require that the context is a vector!!!

(advise make-stack-descriptor 
  (destructuring-bind (context &rest keys) ccl::arglist
    (let ((rbt (context-remote-backtrace context)))
      (if rbt
        (apply #'make-instance 'remote-stack-descriptor
         ;; For some reason backtrace context is an anonymous vector
         :context (require-type context 'simple-vector)
         :remote-backtrace rbt
         keys)
        (:do-it))))
  :when :around :name mclide-make-stack-descriptor)


