;; Clozure 1.7 improvements
;; Author: Terje Norderhaug
;; Parts of the code Copyright Clozure. 
;; LLGPL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKTRACE

(in-package :gui)

(defmethod reload-ov-item ((item backtrace-ov-item))
   "Refresh the visualization represented by the ov-item, returning a true value if it is displayed"
   (let* ((stack-descriptor (container item))
            (controller (ccl::bt.dialog (stack-descriptor-context stack-descriptor)))
            (outline (when controller (slot-value controller 'outline-view))))
      (when outline
        (gui::execute-in-gui
         (lambda ()
           (#/reloadItem: outline item)))
        T)))

;; # consider adding a value slot to backtrace-ov-item that can contain the object it represents, 
;     eliminating the need for a frame descriptor argument.
;     Consider an additional optional argument to control whether the ov-item should be reloaded.

(defmethod update-ov-item ((item backtrace-ov-item) (fd frame-descriptor))
  "Update the ov-item to reflect the object, refreshing the gui if it is displayed" 
  (let ((nvalues (frame-descriptor-value-count fd))
        (children nil))
    (unless (zerop nvalues) ;; ## redundant!
      (dotimes (j nvalues)
        (let ((item (make-instance 'backtrace-ov-item :index j
                      :container fd)))
          (push item children))))
    (setf (children item) (nreverse children))
    (reload-ov-item item)))

(defmethod make-ov-item ((fd frame-descriptor) &key index container)
  (let ((item (make-instance 'backtrace-ov-item :index index :container container)))
    (update-ov-item item fd)
    item))

;; to replace the one in cocoa-backtrace:
(defun generate-ov-items (stack-descriptor)
  (let ((items '())
        (nframes (count-stack-descriptor-frames stack-descriptor)))
    (dotimes (i nframes)
      (let* ((frame (stack-descriptor-frame stack-descriptor i))
             (item (make-ov-item frame :index i :container stack-descriptor)))
        (push item items)))
    (coerce (nreverse items) 'vector)))