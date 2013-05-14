;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For patches (issue 34 on MCL googlecode) - fixes error on incoming event:

(in-package :ccl)

(install-appleevent-handler #$kTextService #$kUnicodeNotFromInputMethod NIL ;; eliminates the error but still signals CCL::UNDEFINED-FUNCTION-CALL.
 ;; A proper event handler will stop key handling!
                            #+ignore
                            (lambda (app event reply refcon)
                              (declare (ignore app event reply refcon))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use same typeface as xcode
;; Don't add to b4, instead provide in an update.
;; Should only be  

(in-package :ccl)
#+ignore
(progn ; def-ccl-pointers developer-font ()
  (let ((typeface "Menlo Regular"))
    (when (and (>= (system-version) #x1060)
               (find-if (lambda (font)(eql 0 (search typeface (first font)))) *font-list*))
      (setq *fred-default-font-spec* `(,typeface 11 :plain))
      (setq *mini-buffer-font-spec* `(,typeface 9 :plain))
      (setq *listener-default-font-spec* *fred-default-font-spec*))))

; *fred-default-font-spec*

;; add a shift-command-t menu item to use plain text (revert to default)?

;; (ccl::get-osx-font-list)
;; (mapcar #'car *font-list*)
;; (find-if (lambda (font)(eql 0 (search "Menlo" (first font)))) *font-list*)


;; ###################################################################################
;; #########################################################################
;; #########################################################################

#+ignore
(defun default-toplevel-result-function (listener expression)
  (flet ((aborting (message &rest args)
           "Throw to read-loop-internal after reporting a message to user"
           (apply #'set-mini-buffer listener message args)
           (sleep 3)
           (cancel)))
        (handler-case
         (with-logged-warnings "Warning when sending a message to remote lisp"      
           (remote::remote-repl-call (repl listener) expression))
         (remote::not-connected ()
                                (aborting "~&Not connected to lisp."))
         (error (c)
                (format *log* "~&Failed to send message to remote lisp (~A)" c)
                (aborting "~&~A" c)))
        ; no values to avoid printing any result in the listener for swank3 at is is covered by callbacks!
        (values)));; ###################################################################################
;; #########################################################################
;; #########################################################################


