;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCESSES

(in-package :gui)

(defun enqueue-with-processes (fun)
   "Call function with the list of processes (possibly returning immediately after enqueing the call)"
    (progn (funcall fun (all-processes)) NIL))

(objc:defmethod (#/refresh: :void) ((self processes-window-controller) sender)
  (declare (ignore sender))
  (enqueue-with-processes
   (lambda (processes)
    (execute-in-gui
     (lambda ()
      (setf (slot-value self 'processes)
       (coerce processes 'vector))
      (#/reloadData (processes-window-table-view self)))))))

(objc:defmethod #/init ((self processes-window-controller))
   (setf (slot-value self 'processes) NIL)
   (enqueue-with-processes
    (lambda (processes)
     (execute-in-gui
     (lambda ()
      (setf (slot-value self 'processes)
       (coerce processes 'vector))
      (#/reloadData (processes-window-table-view self))))))
   (#/initWithWindowNibName: self #@"processes"))
