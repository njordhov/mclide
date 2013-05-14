
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mclide)

(defconstant %epsilon% #\U+2026)

(eval-when (:load-toplevel :compile-toplevel :execute)
 (require :menu-utils))

;; ## consider using lisp-application-delegate instead...

(defclass lisp-controller (ns:ns-object)
   ()
   (:metaclass ns:+ns-object))

(defvar *controller* NIL)

(defparameter *new-listener-menu-action* #'easygui::new-remote-lisp-from-user)

(objc:defmethod (#/newConnection :void)
                ((self lisp-controller))
   (funcall *new-listener-menu-action*))

(objc:defmethod (#/newConnection: :void)
                ((self gui::lisp-application-delegate))
   (funcall *new-listener-menu-action*))

(def-load-pointers init-controller ()
   (defclass lisp-controller (ns:ns-object)
      ()
      (:metaclass ns:+ns-object))
   (setf *controller* (make-instance 'lisp-controller)))

(defun init-connection-menu ()
   (let ((item (iu:find-menu-item-with-name "New Listener")))
         (#/setAction: item (gui::@selector #/newConnection:)))
   #+ignore
   (iu:make-and-install-menuitems-after "File" "Open Recent"
    `("-" nil) 
    `(,(format nil "Connect~A" %epsilon%) "newConnection" "" ,*controller* )))

;; ## see *cocoa-application-finished-launching* for alternative solution?

(defvar ccl::*mclide-startup-functions* '(init-connection-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mclide)

(defvar *experiments-menu* NIL)

(defun init-mclide-menu (&aux (epsilon (string %epsilon%)))

  (flet ((permanent-disable-menu-item (name)        
           (let ((item (iu:find-menu-item-with-name name)))
             (assert item () "No item for ~A" name)
             (#/setAction: item +null-ptr+)
             (#/setEnabled: item nil)
             item))

         (remove-menu-item (name)
           (let ((item (iu:find-menu-item-with-name name)))
             (when item
               (#/removeItem: (#/menu item) item)))))
   
   (iu::insert-menu-after "Lisp" "Debug" 
                                  (iu:find-menu-item-with-name "Exit Break Loop")  ; abort
                                  (iu:find-menu-item-with-name "Interrupt")  ; break
                                  (iu:find-menu-item-with-name "Continue")
                                  (iu:find-menu-item-with-name (concatenate 'string "Restarts"  epsilon))
                                  "-"
                                  (iu:find-menu-item-with-name (concatenate 'string "Backtrace" epsilon)))
   
   (remove-menu-item "Check for Updates...")  ;; note that this incorrectly uses tripple dot, not epsilon!
   
   (iu::trim-menu "Lisp")
   
   (let ((menu (iu::find-menu "Experiments")))
     (when menu
         (setf *experiments-menu* menu)
         (#/removeItem: (#/mainMenu #&NSApp) menu)))
   
   (remove-menu-item "Clozure CL Help")
   (remove-menu-item "Compile Buffer")
   (remove-menu-item "Compile and Load Buffer")
#+ignore
   (permanent-disable-menu-item "Load File...") ;; ## should use epsilon!!!
   (permanent-disable-menu-item "Compile File..."))) ;; ## should use epsilon!!! 

(eval-when (:load-toplevel :execute)
 (pushnew 'init-mclide-menu ccl::*mclide-startup-functions*))

(defun extend-lisp-menu ()
  (iu:make-and-install-menuitems-after "Lisp" "Inspect"
    `("Documentation" nil "" ,*controller*)
    `("Definition" nil "")
    `("Callers" nil "" )
    `("-" nil)
    `("Macroexpand" nil "" ,*controller* )))

(eval-when (:load-toplevel :execute)
 (pushnew 'extend-lisp-menu ccl::*mclide-startup-functions*))

(defun extend-tools-menu ()
  (iu:make-and-install-menuitems-after "Tools" (format nil "Processes~A" %epsilon%)
    `(,(format nil "Trace~A" %epsilon%) nil "")
    `("Logical Hosts" nil "")
    `("Packages" nil "" )))

(eval-when (:load-toplevel :execute)
 (pushnew 'extend-tools-menu ccl::*mclide-startup-functions*))
                
  




