(in-package :gui)

;; ## consider binding remote-lisp around processes-window-controller to ensure consistency

;; # not good to advise all-processes as it may be used elsewhere! Better move towards using continuation.

#+ignore
(advise all-processes
  (if (not (remote::remote-lisp-mode))
       (:do-it)
       (remote::using-remote-lisp (remote::remote-lisp)
          (remote::remote-processes)))
  :when :around :name mclide-all-processes)

(ccl::defcontextual enqueue-with-processes (fun)
                               (remote::remote-lisp-mode)
 (remote::using-remote-lisp (remote::remote-lisp) ;; perhaps not required?
  (remote::remote-eval-enqueue-bind (processes)
                                                             (remote::remote-processes-form)
   (funcall fun processes))))

#+ignore
(objc:defmethod #/init ((self processes-window-controller))
   (let ((processes (all-processes)))
      (setf (slot-value self 'processes) (coerce processes 'vector))
      (#/initWithWindowNibName: self #@"processes")))


(defmethod process-name ((process remote::remote-process))
                   (remote::remote-process-name process))

#+ignore ; not generic, need advise?
(defmethod process-whostate ((process remote::remote-process))
                   "state")

(ccl::defcontextual process-whostate (p)
                               (typep p 'remote::remote-process)
 (remote::remote-process-whostate p))

(defmethod process-thread ((process remote::remote-process))
   (remote::remote-process-id process))

#+ignore ; not generic, need advise?
(defmethod process-suspend-count ((process remote::remote-process))
                   0)

(ccl::defcontextual process-suspend-count  (p)
                               (remote::remote-lisp-mode)
 (declare (ignore p))
 0)

(defmethod process-kill ((process remote::remote-process))
   ; called in #/killSelectedProcess
  ; # ask user to conform first like in mclide1?
  (let ((id (remote::remote-process-id process)))
    (remote::remote-eval-enqueue-bind (response)
                              (remote::kill-thread-form id) 
      (declare (ignore response))
      nil)))

(ccl::defcontextual (inspect inspect-remote-process) (p)
  (remote::remote-process-p p)
  (remote::remote-eval-enqueue-bind (response)
                                    (remote::show-process-inspector-form p)
    (declare (ignore response))
    ;; ## check for error response?
     nil))





