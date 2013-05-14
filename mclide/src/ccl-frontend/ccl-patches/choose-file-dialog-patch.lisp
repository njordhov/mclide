(in-package "GUI") ; file-dialogs.lisp of cocoa-ide

;; Adds :title and :message to choose-file-dialog of ccl 1.7.
;; http://developer.apple.com/library/mac/#documentation/Cocoa/Reference/ApplicationKit/Classes/NSOpenPanel_Class/Reference/Reference.html

; Note that #/runModalForDirectory:file:types: has been deprecated with osx 10.6 (use runModal instead):
; http://developer.apple.com/library/mac/#documentation/Cocoa/Reference/ApplicationKit/Classes/NSOpenPanel_Class/DeprecationAppendix/AppendixADeprecatedAPI.html#//apple_ref/occ/instm/NSOpenPanel/runModalForDirectory:file:types:

(defun %cocoa-choose-file-dialog (directory file-types file button-string title message)
  (assume-cocoa-thread)
  (let* ((open-panel (#/openPanel ns:ns-open-panel))
         (types-array +null-ptr+))
    ;; Maybe support multiple file selection later.
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (when directory
      (setq directory (#/autorelease (%make-nsstring directory))))
    (when file
      (setq file (#/autorelease (%make-nsstring file))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    (when button-string
      (#/setPrompt: open-panel (#/autorelease (%make-nsstring button-string))))
    (when title
      (#/setTitle: open-panel (#/autorelease (%make-nsstring title))))
    (when message
      (#/setMessage: open-panel (#/autorelease (%make-nsstring message))))
    (let ((result (#/runModalForDirectory:file:types: open-panel directory
						      file types-array)))
      (cond ((= result #$NSOKButton)
	     (lisp-string-from-nsstring (#/filename open-panel)))
	    ((= result #$NSCancelButton)
	     nil)
	    (t
	     (error "couldn't run the open panel: error code ~d" result))))))

(defun cocoa-choose-file-dialog (&key directory file-types file button-string title message)
  (when directory
    (setq directory (directory-namestring directory)))
  (when file-types
    (unless (and (listp file-types)
		 (every #'stringp file-types))
      (error "~s is not a list of strings." file-types)))
  (when file
    (setq file (file-namestring file)))
  (check-type button-string (or null string))
  (execute-in-gui #'(lambda () (%cocoa-choose-file-dialog directory file-types file button-string title message))))

; (cocoa-choose-file-dialog :title "Hello" :Message "Choose a file")

 
