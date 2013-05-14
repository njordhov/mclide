;; improvements to ccl
;; Author: Terje Norderhaug
;; create menu separators using the title "-"

(eval-when (:load-toplevel :compile-toplevel :execute)
 (require :menu-utils))

(in-package :iu)

; new, similar code is in make-and-install-menu, make-and-install-menuitems-after, add-to-main-menu and likely elsewhere!

(defun insert-menu-item (menu item &optional index)
   (flet ((insert (mi)
             (if index
                  (#/insertItem:atIndex: menu mi index) 
                  (#/addItem: menu mi))
             mi))
          (etypecase item
                            (null NIL)
                            (string
                             (if (string-equal item "-")
                                  (insert (#/separatorItem ns:ns-menu-item))
                                  (insert-menu-item menu `(,item ,item))))
                            (cons
                             ;; ## more flexible to use keywords in the menu item definition!
                             (destructuring-bind (mi-title mi-selector &optional (mi-key "") mi-target) item
                                                              (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
                                                                       (action-selector (get-selector (string mi-selector)))
                                                                       (ns-key (ccl::%make-nsstring (string mi-key)))
                                                                       (men-item 
                                                                        (if index
                                                                             (#/insertItemWithTitle:action:keyEquivalent:atIndex: menu 
                                                                              ns-title 
                                                                              action-selector
                                                                              ns-key
                                                                              index)
                                                                             (#/addItemWithTitle:action:keyEquivalent: menu 
                                                                              ns-title 
                                                                              action-selector
                                                                              ns-key))))
                                                                      (when mi-target
                                                                                 (#/setTarget: men-item mi-target))
                                                                      (#/release ns-title)
                                                                      (#/release ns-key)
                                                                      men-item)))
                            (ns:ns-menu-item 
                             (#/retain item)
                             (let ((current-menu (#/menu item)))
                                   (when (not (eql (%null-ptr) current-menu))
                                              (#/removeItem: current-menu item)))
                             (insert item)
                             (#/release item)
                             item))))

;; see e.g. make-and-install-menu


(let ((*warn-if-redefine-kernel* NIL)
       (*warn-if-redefine* NIL))

(defun make-and-install-menuitems-after (menu-name menu-item-name &rest menu-item-specs)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menuitem (or (#/itemWithTitle: main-menu ns-menu-name) 
                       (error "~s is not a valid menu title" menu-name)))
         (sub-menu (#/submenu menuitem))
         (ns-menu-item-name (ccl::%make-nsstring menu-item-name))
         (insert-index (#/indexOfItemWithTitle: sub-menu ns-menu-item-name))
         (installed-items nil))
    (dolist (mi menu-item-specs)
      (destructuring-bind (mi-title &optional mi-selector (mi-key "") mi-target) mi
        (if (string-equal mi-title "-")
          (let ((men-item (#/insertItem:atIndex: sub-menu (#/separatorItem ns:ns-menu-item) (incf insert-index))))
            (push men-item installed-items))
          (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
                 (action-selector (get-selector (string mi-selector)))
                 (ns-key (ccl::%make-nsstring (string mi-key)))
                 (men-item (#/insertItemWithTitle:action:keyEquivalent:atIndex: 
                            sub-menu 
                            ns-title 
                            action-selector
                            ns-key
                            (incf insert-index))))
            (push men-item installed-items)
            (when mi-target
              (#/setTarget: men-item mi-target))
            (#/release ns-title)
            (#/release ns-key)))))
    (#/release ns-menu-item-name)
    (#/release ns-menu-name)
    ;; returned value is suitable as argument for remove-menuitems
    (cons sub-menu installed-items)))

) ; end redefine

; http://developer.apple.com/library/mac/#documentation/Cocoa/Reference/ApplicationKit/Classes/NSMenuItem_Class/Reference/Reference.html%23//apple_ref/occ/instm/NSMenuItem/menu

;; functionality from make-and-install-menuitems-after and menu-item-action-selector [consider use abstraction]

(defun find-menu (menu-name)
  (let ((ns-menu-name (ccl::%make-nsstring menu-name))
        (main-menu (#/mainMenu #&NSApp)))
    (unwind-protect
        (let ((item (#/itemWithTitle: main-menu ns-menu-name)))
          (unless (eql (%null-ptr) item)
             item))
      (#/release ns-menu-name))))

;; ## consider split into a functiont hat only add a single item, with optional index (?)
;; ## Should use keywords for the list variation!
;; ## should support separator bar! "-"
;; ## consider to allow just a string as menu item definition, with selector the same as title

(defun insert-menu-after (existing-menu-name menu-name &rest menu-items)   
  (let* ((main-menu (#/mainMenu #&NSApp))
         (ns-existing-name (ccl::%make-nsstring existing-menu-name))
         (index (#/indexOfItemWithTitle: main-menu ns-existing-name))
         (ns-menu-name (ccl::%make-nsstring menu-name))
         (new-menuitem (make-instance ns:ns-menu-item))
         (menu (#/initWithTitle: (#/allocWithZone: 
                                  ns:ns-menu (#/menuZone ns:ns-menu))
                                 ns-menu-name)))
    (#/setSubmenu: new-menuitem menu)
    (dolist (item menu-items)
      (insert-menu-item menu item))
    (unwind-protect
        (#/insertItem:atIndex: main-menu new-menuitem (1+ index))
      (#/release ns-existing-name)
      (#/release ns-menu-name))))

(defun find-menu-item (menu title)
  (let ((menu (typecase menu
                (string (find-menu menu))
                (otherwise menu)))
        (ns-title (ccl::%make-nsstring title)))
    (unwind-protect
       (when menu
        (let ((item (#/itemWithTitle: (#/submenu menu) ns-title)))
          (unless (eql (%null-ptr) item)
            item)))
      (#/release ns-title))))

(defun trim-menu (title)
  "Eliminate redundant separators"
  (let* ((menu (or (find-menu title)
                           (error "No menu ~A" title)))
         (separator nil)
         (items-to-delete)
         (item-count (#/numberOfItems (#/submenu menu))))
    (dotimes (index item-count)
      (let ((item (#/itemAtIndex: (#/submenu menu) index)))
        (if (#/isSeparatorItem item)
          (if (or separator (= index (1- item-count)))
            (push item items-to-delete)
            (setf separator T))
          (setf separator NIL))))
    (dolist (item items-to-delete)
      (#/removeItem: (#/submenu menu) item))
    (when items-to-delete
      (length items-to-delete))))
