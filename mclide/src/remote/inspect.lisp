;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECT

(in-package :remote)

(define-swank-function inspection (id slimefun &rest args)
  "Call slimefun in context associated with id (to allow concurrent inspectors) returning a new id if the context changed"
  ; # would it make sense to automatically release previous *istate* if it has changed?
  ; # should also bind the istate history?
  ;; ## alternatively use the make-tag id generator of swank!
 (:common-lisp
   (declare (special *id->istate* *last-istate-id*))
  (defvar *id->istate* (make-hash-table))
  (defvar *last-istate-id* 1111) 
  (let* ((istate (gethash id *id->istate*))
           (*istate* istate)
           (result (multiple-value-list
                       (case slimefun
                          (:release
                           (setf (gethash id *id->istate*) nil
                                   *istate* nil))
                          (otherwise
                           (apply slimefun args))))))
   (list*
    (unless (or (null *istate*)
                      (eq istate *istate*))
       (let ((new-id (incf *last-istate-id*)))
         (setf (gethash new-id *id->istate*) *istate*)
         new-id))
    result)))
 ;; ## need to be properly implemented?
 (:clojure #:|
  (in-ns 'swank.commands.basic)
  (swank.commands/defslimefn inspection [istate-id slimefun & args]
   (apply slimefun args)) 
  |))

(define-swank-function mclide-inspect-method (name specializers qualifiers)
  "Open an inspector for the specified method"
  (:common-lisp
   (inspect-in-emacs
    (cl:find-method (cl:symbol-function (from-string name))
                    (unless (eql t qualifiers) qualifiers)
                    (from-string specializers)))))

(defun open-inspector-form (type name specializers qualifiers)
  (case type
    (cl:method
     (mclide-inspect-method name (string (car specializers)) qualifiers))
    (t
     (open-sexp-inspector-form name))))

(define-swank-function mclide-inspect (encoding)
  "Similar to swank:inspect-in-emacs but with its own inspect state context - takes an unparsed string or part id"
  ; Note: The slime inspect does an eval before inspect, while mcl doesn't!
  ; Assumes the sexp to be read in the current buffer/transport package.
  ; Returns NIL, or a list (:error "reason"?)
  ; can be extended with various encodings.
  ; suggest something like it for swank?
  ; ## need to implement all encodings for clojure and gambit!
  (:common-lisp
   (handler-case
     (let ((value (etypecase encoding
                    (string (from-string encoding))
                    (integer (inspector-nth-part encoding))
                    (list
                     (destructure-case encoding
                       ((:value value) value)
                       ((:frame-var frame-number index) 
                        (ignore-errors ; requently fails...
                         (frame-var-value frame-number index)))))))
           (connection *emacs-connection*)
           (package *buffer-package*)
           (readtable *buffer-readtable*))
       (#:|swank::spawn|
        (lambda (&aux *istate* *inspector-history* 
                      (*inspector-verbose* T) ; # better if :label was the verbose and :title not
                      #+ignore (*emacs-connection* connection)
                      (connection connection)
                      (*buffer-package* package) 
                      (*buffer-readtable* readtable))
          (handler-case
              (with-connection (connection) ; with-bindings *default-worker-thread-bindings* ;; required?
                (inspect-in-emacs value :wait T)
                    ;; limited to this process only, not the the connection:
                 (handle-requests connection)
                    ) ;)
            (error (c) ;; for debugging purposes, should be substituted with proper logging
              (background-message "Error during inspect (~A)" c))))
        :name "Remote Inspect")
       NIL)
     (error (c)
      `(:error ,(princ-to-string c)))))
  (:gambit #:|
   (define (swank:mclide-inspect sexp-string)
        (let ((thread (object->serial-number (current-thread)))
              (tag (current-seqnum))) 
          (swank-write `(:inspect ,(inspect-object (with-input-from-string sexp-string read)) ,thread ,tag))
          (swank-write `(:write-string ,"\n" :repl-result)))
        #f)
      (table-set! swank-op-table 'swank:mclide-inspect swank:mclide-inspect)
     |)
  (:clojure 
    #:|
    (in-ns 'swank.commands.basic)   
    (swank.commands/defslimefn mclide-inspect [string]
     ; (with-emacs-package
        (try 
          (do
            (swank.commands.inspector/inspect-in-emacs (eval (read-string string)))
            nil)
          (catch Exception e
            `(:error ~(str e)))))  ;)
    nil
    |))

(defun open-sexp-inspector-form (sexp)
  (assert (remote-sexp-p sexp))
  (let ((package (remote-reader-package sexp)))
    (query-form (mclide-inspect (string sexp))
                :thread :repl-thread
                :package package)))

(define-swank-function mclide-inspect-current-condition ()
  (:common-lisp
   (ignore-errors
    (inspect-current-condition)))
  (T '#:|swank:inspect-current-condition|))

(defun inspect-current-condition-form ()
  (query-form (mclide-inspect-current-condition)
                       :thread :repl-thread))

(define-swank-function mclide-frame-source-location (id)
  (:common-lisp
   (ignore-errors
     (frame-source-location id)))
  (T '#:|swank:frame-source-location|))

(defun frame-source-location-form (id)
  (query-form (mclide-frame-source-location id)
                       :thread :repl-thread))

(define-swank-function mclide-inspect-frame-function (id)
  (:common-lisp
   (declare (ftype (function (integer) t) inspect-frame-function))
   (ignore-errors
    (inspect-frame-function id)))
  (T '#:|swank:inspect-frame-function|))

(defun inspect-frame-function-form (id thread-id)
  (query-form (mclide-inspect-frame-function id)
                       :thread thread-id))

(defun frame-locals-and-catch-tags-form (frame-number thread-id)
  (query-form (frame-locals-and-catch-tags-form-1 frame-number)
                       ;; the thread for which we are backtracing:
                       :thread thread-id))

(define-swank-function mclide-inspect-frame-var (frame index)
  ;; avoid! Use mclide-inspect instead to ensure private inspect context!
  (:common-lisp
   (ignore-errors
    (inspect-frame-var frame index)))    
  (T '#:|swank:inspect-frame-var|))

(defun inspect-frame-var-form (frame index thread-id)
  (query-form (mclide-inspect-frame-var frame index)
          #+ignore ; requires swank-repsentations (but this is what AquaMacs uses): 
          (#:inspect-presentation `(:frame-var ,thread-id ,frame ,index) t)
          ;; the backtraced thread 
          :thread  thread-id))

(defun return-from-frame-form (id value)
  `(handler-case
      ,(sldb-return-from-frame-form id value)
      (error (#:c)
             (format nil "Failed to return value from selected frame: ~A"
                      #:c))))


(define-swank-function mclide-restart-frame (id)
  (:common-lisp
   (handler-case
      (restart-frame id)
      (error (c)
             (format nil "Failed to restart selected frame: ~A" c))))
  (T '#:|swank:restart-frame|))

(defun restart-frame-form (id)
  (mclide-restart-frame id))

(defun kill-thread-form (id)
  "form to kill the remote thread associated with the id"
  (mclide-kill-thread id))

(define-swank-function mclide-interrupt-thread (id)
  (:common-lisp
   (#:interrupt-thread
    (#:find-thread id)
    'cl:break)))

(defun interrupt-thread-form (id)
  "form to interrupts the remote thread associated with the id"
  (mclide-interrupt-thread id))

(define-swank-function mclide-logical-host-mapping ()
  (:common-lisp
   (declare (ftype (function () t) logical-hosts))
   (mapcar (lambda (host)
                      (cons host
                         (mapcar (lambda (translation)
                                           "Eliminate implementation-defined elements"
                                           (subseq translation 0 2))
                                       (cl:logical-pathname-translations host))))
              (logical-hosts)))
  ; Clojure doesn't have any logical pathname concept, so this is a stretch!
  ; Perhaps introduce a new item for 'search paths' for all lisps?
  (:clojure #:|
    (in-ns 'swank.commands.basic)  
    (swank.commands/defslimefn mclide-logical-host-mapping []
      (map #(list (str %1) (.getFile %1)) (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))
    nil |))

(defun logical-host-mapping-form () ;; should be eliminated!
  (mclide-logical-host-mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspection results

(in-package :remote)

;; ## eliminate all references to the inspector package!

(defclass remote-inspected-object ()
  ((session :reader inspection-session :initarg :session :initform nil)
   (inspector::id :reader inspector::inspection-id :initarg :id :initform nil)
   (inspector::title :reader inspector::inspection-title :initarg :title :initform nil)
   (inspector::label :reader inspector::inspection-label :initarg :label :initform nil)
   (inspector::content :reader inspector::inspection-content :initarg :content :initform nil))
  (:documentation "Encapsulates the response of inspecting a remote object"))

(defun parse-remote-inspected-object (value &optional session)
  "Makes a remote-inspected-object from the swank representation of an object"
  ;; ## rename to build-remote-inspected-object?
  (destructuring-bind (&key id title label content) value
      (make-instance 'remote-inspected-object
        :session session 
        :id id
        :title title
        :label label
        :content content)))

(defmethod remote-inspected-items ((object remote-inspected-object))
  "Generate a list of items representing the inspected content"
   (let ((content (inspector::inspection-content object))
         (session (inspection-session object)))
      (inspector::parse-remote-inspected-content (first content) session)))

(in-package :inspector) ;; # move all to remote

#|
(parse-remote-inspected-object
'(:title "#<BIT #x1>" :id 0 :content (("Fixnum:      " " " (:value "#0=1" 1) "
" "Scientific:  " " " (:value "1.0" 2) "
" "Log base 2:  " " " (:value "0.0" 3) "
" "Binary:      " " " (:value "#0=1" 4) "
" "Octal:       " " " (:value "#0=1" 5) "
" "Decimal:     " " " (:value "#0=1" 6) "
" "Hex:         " " " (:value "#0=1" 7) "
" "Roman:       " " " (:value "\"I\"" 8) "
" "Character:   " " " (:value "#\\^A" 9) "
" "Abbreviated: " " " (:value "\"1\"" 10) "
" "As time:     " " " (:value "\"16:00:01 Sunday, 31 December, 1899\"" 11) "
" "MOST-POSITIVE-FIXNUM" " " (:value "536870911" 12) "
") 48 0 500)))
|#

;; ## careful using the remote-inspection promise as it may change the inspector state on swank!
;; ## consider phasing ut and use remote-inspection-session to represent the state.

(defclass remote-inspection (remote::remote-inspected-object remote::promise)
   ((thread :initarg :thread :initform swank-client::slime-current-thread))
   (:documentation "Promised result of  inspected object that may still be in progress performing asynchronous remote evaluation"))

(defmethod remote::deliver ((object remote-inspection) result)
  (destructuring-bind (&key id title label content) result
    (setf (slot-value object 'id) id
          (slot-value object 'title) title
          (slot-value object 'label) label
          (slot-value object 'content) content))
  (call-next-method))

;; should ultimately be eliminated, handy for testing but not called
#+ignore
(defun remote-inspection-x (form)
   "Encapsulate the result of evaluating the inspect on the swank server using the current thread (possibly returning before the result of the inspect is complete)"
     (remote::remote-eval-bind (result) form
       (remote::parse-remote-inspected-object result)))

(defun remote-inspection (form)
   "Encapsulate the result of evaluating the inspect on the swank server using the current thread (possibly returning before the result of the inspect is complete)"
   (make-instance 'remote-inspection :form form))

#+test ; eval in MCLIDE listener during a break:
(let ((swank-client::slime-current-thread)) ;; the thread for which we are backtracing
  (inspect (remote-inspection `(progn (sleep 5)(#:inspect-current-condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SESSION

(in-package :remote) 

(defclass remote-inspection-session ()
  ((thread :reader inspection-thread :initarg :thread :type integer)
   (connection :initarg :connection))
  (:documentation "Mirror of the inspection state on the swank server"))

(defmethod remote-inspect-part ((session remote-inspection-session) id &optional callback)
  "Inspect the identified part of the current remote object, calling back with the new current remote object" 
  (flet ((build-inspected-object (value)
           (parse-remote-inspected-object value session)))
    (remote-eval-bind (object) ; ## should enqueue to avoid beachballs... and/or have a timeout
                 (query-form (inspect-nth-part-form id)
                                     :thread (inspection-thread session)
                                     :post-processing #'build-inspected-object
                                     :recover (lambda () NIL))
      (when callback
        (funcall callback object)))))

(defmethod remote-inspection-pop ((session remote-inspection-session) &optional callback)
  (flet ((build-inspected-object (value)
           (parse-remote-inspected-object value session)))
    (remote-eval-bind (object) ; ## should enqueue to avoid beachballs... and/or have a timeout
                 (remote::query-form (remote::inspector-pop-form)
                                     :post-processing #'build-inspected-object
                                     :thread (inspection-thread session))
      (when callback
        (funcall callback object)))))

(defmethod remote-inspection-next ((session remote-inspection-session) &optional callback)
  (flet ((build-inspected-object (value)
           (parse-remote-inspected-object value session)))
    (remote-eval-bind (object) ; ## should enqueue to avoid beachballs... and/or have a timeout
                 (remote::query-form (remote::inspector-next-form)
                                     :post-processing #'build-inspected-object
                                     :thread (inspection-thread session))
      (when callback
        (funcall callback object)))))

(defmethod remote-reinspect-current ((session remote-inspection-session) &optional callback)
  "Inspect the identified part of the current remote object, calling back with the new current remote object" 
  (flet ((build-inspected-object (value)
           (parse-remote-inspected-object value session)))
    (remote-eval-bind (object) ; ## should enqueue to avoid beachballs... and/or have a timeout
                 (query-form (inspector-reinspect-form)
                             :thread (inspection-thread session)
                             :post-processing #'build-inspected-object
                             :recover (lambda () NIL))
      (when callback
        (funcall callback object)))))

(defmethod show-part-inspector ((session remote-inspection-session) id)
   "Send event to client to open a new inspector with the identified part of the current remote object" 
  (check-type id number)
  (remote-eval-enqueue-bind (result)
                                    (query-form (mclide-inspect id)
                                                :thread (inspection-thread session))
     (declare (ignore result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTENT

(in-package :inspector)

(defclass remote-inspected-item (remote::remote-inspected-object)
  ((tokens :reader remote-inspected-item-tokens
           :initarg :tokens
           :type list
           :initform NIL
           :documentation "Presentation tokens for this line as provided by swank")
   (mode :reader inspection-mode :initarg :mode :initform :normal)
   (type :reader inspection-type :initarg type :initform nil)
   (value :reader inspection-value :initarg :value :initform nil)
   (format :reader inspection-format :initarg :format :initform nil)))

(defmethod initialize-instance :after ((line remote-inspected-item) &key tokens)
  ; for now (march 2009) preparse the tokens into values - consider making sense of it on demand instead!
  (with-output-to-string (out)
    (loop
      for item in tokens
      do (typecase item
            (string
               (write-string item out)
               (unless tokens
                 (let* ((title (string-trim ccl::wsp&cr (ccl::get-output-stream-string out)))
                        (len (length title)))
                   (when (plusp len)                     
                     (when (eql #\: (schar title (1- len)))
                       (setf title (subseq title 0 (1- len)))
                       (setf (slot-value line 'mode) :colon))
                     (setf (slot-value line 'title) title)))))
            (list
             (case (car item)
               (:action nil) ; fancy inspector
               (:value
                (setf (slot-value line 'value)
                      (second item))
                (setf (slot-value line 'id)
                      (third item)))
               (otherwise NIL)))))))

(defun parse-remote-inspected-content (content &optional session)
  "Decodes the aspects of a remote inspection into a list of remote inspected items"
  ;; Assumes that each line including the last is terminated with a linefeed.
  (loop
    with result
    with tokens
    while content
    for item = (pop content) ;; to allow push back
    do (flet ((collect-string (str)
               "normalize by combining strings and ignoring empty strings"
               ; should it instead keep tokens unchanged in case it has meaning?
               (when (plusp (length str))
                (if (stringp (first tokens))
                  (setf (first tokens)
                        (concatenate 'string (first tokens) str))
                  (push str tokens)))))
         (typecase item
           (string
            (let ((end (position-if #'remote::char-eolp item)))
              (cond
               (end
                (unless (zerop end)
                  (collect-string (subseq item 0 end)))
                (unless (= end (length item))
                  (push (subseq item (1+ end)) content))
                (push (make-instance 'remote-inspected-item
                        :session session
                        :tokens (nreverse tokens)) result)
                (setf tokens nil))
               (T
                (collect-string item)))))
           (list
            (push item tokens))))
    finally (return (nreverse result))))

#+ignore
(inspector::parse-remote-inspected-content 
 '("" (:value "#0=#<Compiled-function (:INTERNAL SWANK:INSPECT-FRAME-FUNCTION) (Non-Global)  .." 1) "
" 
   "Name" ": " (:value "(:INTERNAL SWANK:INSPECT-FRAME-FUNCTION)" 2) "
" 
   "Arglist (analysis)" ": " (:value "NIL" 3) "
" 
   "(0 TWNEI NARGS 4)" " " (:value "4" 4) "
" 
   "(4 MFLR LOC-PC)" "
" 
   "(8 BLA .SPSAVECONTEXTVSP)" "
" 
   "(12 VPUSH ARG_Z)" "
" 
   "(16 SET-NARGS 0)" " " (:value "0" 5) "
" 
   "(20 LWZ TEMP3 'RESET-INSPECTOR FN)" " " (:value "SWANK::RESET-INSPECTOR" 6) "
" 
   "(24 BLA .SPJMPSYM)" "
" 
   "(28 LWZ ARG_Z 0 VSP)" " " (:value "0" 7) "
" 
   "(32 SET-NARGS 1)" " " (:value "1" 8) "
" 
   "(36 LWZ TEMP3 'FRAME-FUNCTION FN)" " " (:value "SWANK-BACKEND:FRAME-FUNCTION" 9) "
" 
   "(40 BLA .SPJMPSYM)" "
" 
   "(44 SET-NARGS 1)" "
" ))

#+ignore 
(parse-remote-inspected-content 
 '("Fixnum:      " " " (:value "#0=5" 1) "
"
 "Scientific:  " " " (:value "5.0" 2) "
"
 "Log base 2:  " " " (:value "2.321928" 3) "
"
 "Binary:      " " " (:value "#0=5" 4) "
" 
"Octal:       " " " (:value "#0=5" 5) "
"
 "Decimal:     " " " (:value "#0=5" 6) "
" 
"Hex:         " " " (:value "#0=5" 7) "
"
 "Roman:       " " " (:value "\"V\"" 8) "
"
 "Character:   " " " (:value "#\\^E" 9) "
"
 "Abbreviated: " " " (:value "\"5\"" 10) "
"
 "As time:     " " " (:value "\"16:00:05 Sunday, 31 December, 1899\"" 11) "
"
 "MOST-POSITIVE-FIXNUM" " " (:value "536870911" 12)  "
"))


#+ignore ; keep for reference
(defun parse-remote-inspected-content (content &aux result)
  "Generates a list of (value label type) based on a list of content that may contain linefeeds"
  (with-output-to-string (out)
    (loop
      while content
      for item = (pop content) ;; to allow push back
      do (typecase item
           (string
            (let ((end (position #\linefeed item)))
              (write-string item out :end end)
              (when end
                (push (ccl::get-output-stream-string out) result) 
                (push (subseq item (1+ end)) content))))
           (list
            (case (car item)
              (:value
               (format out "~A" (second item))
               #+ignore
               (setf id (third item))))))))
  (nreverse result))

#+ignore
(parse-remote-inspected-content 
 '("Fixnum:      " " " (:value "#0=5" 1) "
" "Scientific:  " " " (:value "5.0" 2) "
" "Log base 2:  " " " (:value "2.321928" 3) "
" "Binary:      " " " (:value "#0=5" 4) "
" "Octal:       " " " (:value "#0=5" 5) "
" "Decimal:     " " " (:value "#0=5" 6) "
" "Hex:         " " " (:value "#0=5" 7) "
" "Roman:       " " " (:value "\"V\"" 8) "
" "Character:   " " " (:value "#\\^E" 9) "
" "Abbreviated: " " " (:value "\"5\"" 10) "
" "As time:     " " " (:value "\"16:00:05 Sunday, 31 December, 1899\"" 11) "
" "MOST-POSITIVE-FIXNUM" " " (:value "536870911" 12) "
"))
