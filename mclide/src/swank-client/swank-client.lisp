;;; -*- Mode: LISP; Package: :swank-client; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Swank Client
;;;; 
;;; Copyright (C) 2007-2012 Terje Norderhaug <terje@in-progress.com>
;;;
;;;   Bugs-To: <terje@in-progress.com>
;;;
;;; You are hereby granted the rights to distribute and use this
;;; software as governed by the terms of the Lisp Lesser GNU Public
;;; License (http://opensource.franz.com/preamble.html), known as the
;;; LLGPL. Other free licenses available upon request.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :swank-client)

#+(or mcl clozure) ;; temporary hack, consider just loading package definition from swank instead!
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'swank)
    (defpackage :swank
      (:use :cl)
      (:import-from :swank-backend
                             #:quit-lisp)
      (:export 
       #:connection-info
       #:quit-lisp
       #:throw-to-toplevel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar slime-dispatching-connection nil)

(defun slime-connection ()
   slime-dispatching-connection)

(defparameter *abort-continuation* (lambda (&optional reason) 
                                                           (when *debug* 
                                                               (warn (or reason "Evaluation aborted.")))))

(defvar slime-current-thread t)

(defun slime-current-thread (connection)
   (declare (ignore connection))
   slime-current-thread)

(defvar slime-connection nil)

(defvar slime-log-events nil)

(defmacro log-swank-event (direction control &rest args)
  `(when slime-log-events 
     (let ((message (concatenate 'string ,(ecase direction (:in "<-- ")(:out "--> "))
                                  (format nil ,control ,@args))))
       (format slime-log-events "~A~%" message))))

(defparameter *stream->socket* (make-hash-table :test #'eq :weak :key))

(defun stream-socket (stream)
  (gethash stream *stream->socket*))

(defvar *swank-connect-timeout* 5)

(defvar +null-package+
  (let ((package (make-package :null-package :use nil)))
    package))

(defparameter *debug* nil)


;;;;;;;;;;;;

(defmacro destructure-case (value &rest patterns)
  ;; copied from from swank-rpc per february 2010 as it has since been moved from that module
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect 
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands 
                               ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

(defun swank-net-connect (host port &optional secret)
  ;; :timeout should be :deadline?
   (let* ((sock (usocket:socket-connect host port :timeout *swank-connect-timeout* :element-type '(unsigned-byte 8)))
          (connection (usocket:socket-stream sock)))
      (setf (gethash connection *stream->socket*) sock)
      (let ((secret (or secret (slime-secret))))
        (when secret
          (slime-net-send secret connection)))
      connection))

(defun slime-net-send (sexp proc)
  (log-swank-event :out "~S~%" sexp)
  (swank-rpc:write-message sexp +null-package+ proc))

(defun slime-send (sexp)
  (slime-net-send sexp (slime-connection)))

(defun slime-net-close (process)
  (close process :abort t))

(defparameter %shared-remote-packages%
  (list 
   (find-package :common-lisp)
   (find-package :keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECRET PASSWORD

(defun user-homedir ()
  ;; ## asdf:user-homedir can soon be used instead for portable version!
  #+mcl
 (ccl::findfolder -32767 ; #$konappropriatedisk 
                  :|cusr| ; #$kcurrentuserfoldertype
                  )
 #-mcl
 (user-homedir-pathname))

(defun slime-secret-pathname ()
  (merge-pathnames  #p".slime-secret" (user-homedir)))

(defvar *slime-secret* :default "Bind to string or NIL to override getting the slime secret from file")

(defun slime-secret (&optional (file #p"home:.slime-secret") )
   (case *slime-secret*
      (:default
        (with-open-file (stream (or file (slime-secret-pathname)) :if-does-not-exist nil)
            (when stream
                (read-line stream nil nil))))
      (otherwise
       *slime-secret*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE OBJECTS

(defstruct remote-object
  (value))

(defmethod print-object ((object remote-object) out)
  (let ((value (remote-object-value object)))
    (typecase value
      (string
       (write-string value out))
      (otherwise ;; really error
       (princ value out)))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SWANK CLIENT

(defun open-swank-client (host port &optional secret)
  (make-instance 'swank-client :host host :port port :secret secret))

(defgeneric swank-remote-eval (client form &optional package)
  (:documentation "Remote evaluate the LISP form, returning the result values"))

(defclass swank-client ()
  ((connection :reader swank-connection :initform nil)
   (listeners :type list :initform nil)
   (inbound :reader inbound :initform (make-values-queue))
   (outbound :reader outbound :initform (make-values-queue))
   (processes :initform nil)
   (lock :initform (ccl::make-lock 'swank-client))
   (continuations :initform nil)
   (continuation-counter :initform 0))
  (:default-initargs
    :host nil))

(defvar *stream->swank-client* (make-hash-table :test 'eq :weak :key))

(defun stream->swank-client (stream)
  (when stream (gethash stream *stream->swank-client*)))

(defmethod initialize-instance :after ((client swank-client) &key host port secret)
  (assert port)
  (swank-client-connect client host port secret) ;; ## move to open-swank-client?
)

(defmethod register-continuation ((client swank-client) continuation)
  (with-slots (continuations continuation-counter lock) client
    (ccl::with-lock-grabbed (lock)
      (let ((id (incf continuation-counter)))
        (push (cons id continuation) continuations)
        id))))

(defmethod retrieve-continuation ((client swank-client) id)
  (with-slots (continuations lock) client
    (ccl::with-lock-grabbed (lock)
      (let ((rec (assoc id continuations :test #'eq)))
        (when rec 
          (setf continuations
                (remove rec continuations))
          (values (cdr rec) (null continuations)))))))


(defmethod swank-client-connect ((client swank-client) host port &optional secret)
  (with-slots (connection) client
    (when (functionp port)
      (setf port (funcall port)))
    (setf connection
          (swank-net-connect host port secret))
    (setf (gethash connection *stream->swank-client*) client)
    (push (ccl:process-run-function "Inbound" #'swank-client-inbound-daemon client)
          (slot-value client 'processes))
    (push (ccl:process-run-function "Outbound" #'swank-client-outbound-daemon client)
          (slot-value client 'processes))
    connection))

(defmethod swank-start-thread ((client swank-client))
  "starts a new thread on the remote LISP, returning its id"
  (read-from-string
   (remote-object-value
    (swank-remote-call client
     "(swank::thread-id (swank::spawn-repl-thread swank::*emacs-connection* \"Swank REPL\"))"))))

(defmethod swank-client-next-message ((client swank-client) &key timeout)
  "Pop the next message from the mailbox when available (optional timeout is in 60th seconds)"
  (values-queue-dequeue (inbound client) timeout))

(defmethod next-message-available-p ((client swank-client))
  (not (values-queue-empty-p (inbound client))))

(defvar swank::*swank-io-package*
   ;; should be identical with definition in swank.lisp of slime!
  (let ((package (make-package :swank-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defun swank-io-package ()
  ; Don't bind as the package might be redefined by swank server!
  (or (find-package "SWANK-IO-PACKAGE")
        swank::*swank-io-package*))

(defun read-clozure-redirect (stream &optional prefix)
  ;; hack... ## clozure should change to use rpc chunk format for the initial response!
  #+ignore ; old version for character stream
  (let ((*read-eval* nil))(read stream))
  (let ((*read-eval* nil)
        (str (with-output-to-string (out)
               (when prefix
                 (write-string prefix out)
                 (loop 
                    for c = (character (swank-rpc::asciify (swank-rpc::read-chunk stream 1)))
                    do (write-char c out)
                    until (char= c #\)))))))
    (read-from-string str)))

(defun handle-clozure-redirect (queue &key active loader message port)
   ; see the swank-listener of the clozure cocoa ide.
   ; should be obsoleted by improving/generalizing the swank-listener
  (declare (ignore loader))
  (unless active
    (error "Failed conncting to swank server: ~A" message))
  (let ((match "A swank server is already running on port "))
    (when (and message (search match message))
      (setf port
            (parse-integer message :start (length match)))))
  (values-queue-enqueue queue 
                        `(:reconnect :port ,port)))

(defmethod swank-client-daemon-1 ((client swank-client))
  "Called from the process of the client to read the next message from connection and queue it for processing"
  (let* ((stream (or (swank-connection client)
                             (error "~A not connected." client)))
         (socket (stream-socket stream))
         (queue (inbound client))
         (message 
            (if (usocket:wait-for-input socket)
              (handler-case
                  (swank-rpc:read-message stream (swank-io-package))
                (; swank-rpc:swank-reader-error ()
                 #+clozure ccl::parse-integer-not-integer-string
                 #-clozure error ()
                   ;; hack - pretend that this is the only possible protocol error.
                   ;; note that the first six bytes of redirect message is skipped!
                 (let ((args (read-clozure-redirect stream "(:acti")))
                   (close stream)
                   (apply #'handle-clozure-redirect queue args)
                   (return-from swank-client-daemon-1))))
              (if (open-stream-p stream)
                (return-from swank-client-daemon-1 T)
                (return-from swank-client-daemon-1)))))
    (when message
      (values-queue-enqueue queue message))
    (or message T)))

(defmethod swank-client-inbound-daemon ((client swank-client))
  (handler-case
      (loop while (swank-client-daemon-1 client))
    (end-of-file ()
      (values-queue-enqueue (inbound client) `(:not-connected)) 
      NIL)
    (error (c)
           (values-queue-enqueue (inbound client) `(:internal-error ,c)))))

(defmethod swank-client-dispatch-next ((client swank-client) &key timeout)
  "Dispatches the next message when available"
  (let* ((event (swank-client-next-message client :timeout timeout))
         ;; ## eliminate asap:
         (slime-dispatching-connection (swank-connection client)))
    (when event
      (log-swank-event :in "~S~%" event)
      (values (apply #'swank-client-dispatch client event) t))))

(defmethod swank-client-outbound-daemon ((client swank-client))
  (handler-case
      (loop
        with queue = (outbound client)
        with connection = (swank-connection client)
        do (slime-net-send (multiple-value-list (values-queue-dequeue queue)) connection))
    (stream-error ()
      (values-queue-enqueue (inbound client) `(:not-connected)))      
    (error (c)
           (values-queue-enqueue (inbound client) `(:internal-error ,c)))))

(defmethod swank-remote-call ((client swank-client) command-string &optional (package (swank-client-package client)))
  "Remote evaluate the LISP form in the string, returning a printable version of the result values"
  ;; listener-eval calls eval-region
  #+ignore
  (let ((form  `(swank::listener-eval ,command-string)))
    (destructuring-bind (mode values)
                        (swank-remote-eval client form package)
      (ecase mode
        (:present
         (apply #'values (mapcar (lambda (result)
                                   (make-remote-object :value (car result))) values))))))
  #+ignore  ; obviously not optimal... maybe using interactive-eval-region is better?
  (when command-string
    (let ((form `(mapcar #'write-to-string 
                         (multiple-value-list (eval (read-from-string ,command-string))))))
      (let ((values (swank-remote-eval client form package)))
        (apply #'values (mapcar (lambda (result)
                                  (make-remote-object :value result)) values)))))
  (when command-string
    (let* ((form `(mapcar #'write-to-string (swank::eval-region ,command-string)))
           (values (swank-remote-eval client form package)))
        (apply #'values (mapcar (lambda (result)
                                  (make-remote-object :value result)) values)))))

(defvar slime-stack-eval-tags nil)

(defmethod swank-remote-eval ((client swank-client) sexp &optional package)
   "Evaluate SEXP on the superior Lisp and return the result."
  (assert package)
  (let* ((tag (gensym "slime-result-"))
         (slime-stack-eval-tags (cons tag slime-stack-eval-tags))
         (result
          (catch tag
                (let ((*abort-continuation*
                             (lambda (&optional condition)
                                           (declare (ignore condition))
                                           (when (member tag slime-stack-eval-tags)
                                                      (throw tag
                                                                 (list #'abort))))))
                      (swank-remote-eval-async client sexp
                                                                        (lambda (value)
                                                                                      (if (not (member tag slime-stack-eval-tags))
                                                                                           ;; may happen if inner eval aborts!
                                                                                           (warn "~S should be one of ~S when swank has evaluated ~S returning ~S"
                                                                                                     tag slime-stack-eval-tags sexp value)
                                                                                           (throw tag (list #'identity value))))
                                                                        package)
                           (loop
                            (swank-client-dispatch-next client))))))
    (assert result)
    (apply #'funcall result)))

(defparameter *shortcut-swank-client-outbound* #+mcl T #-mcl NIL
  "Whether to bypass enqueing outgoing swank messages and output directly to the connection for speed. Can be eliminated as it is legacy support")

(defmethod swank-remote-eval-async ((client swank-client) form &optional cont package)
  "Remote evaluate the LISP form, returning immediately but later calling the continuation with the result value"
  (swank-client-dispatch client :emacs-rex form package slime-current-thread cont))

(defmethod swank-client-dispatch ((client swank-client) command &rest args)
  (declare (ignore command args))
  nil)
 
(defmethod swank-client-dispatch :around ((client swank-client) command &rest args)
  ; Give the listeners the first chance to handle the dispatch
  (let ((use-default t))
    (dolist (listener (slot-value client 'listeners))
      (when (apply #'ccl::method-exists-p #'swank-client-dispatch listener command args) 
        (setf use-default
              (and use-default 
                   (not (apply #'swank-client-dispatch listener command args))))))
    (or (not use-default)
        (call-next-method))))

(defmethod swank-client-package ((client swank-client))
  (slime-current-package (swank-connection client)))

(defmethod swank-enqueue-event ((client swank-client) &rest event)
  (apply #'values-queue-enqueue (outbound client) event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-swank-dispatch (event target (&rest arglist) &body body &aux (command (gensym))(args (gensym)))
  `(defmethod swank-client-dispatch ((,target ,target) (,command (eql ,event)) &rest ,args)
     (declare (ignore ,command) 
              (dynamic-extent ,args))
     (destructuring-bind ,arglist ,args
       ,@(if (stringp (car body)) (cdr body) body))))

(define-swank-dispatch :internal-error swank-client (condition)
  ; not part of swank, use to pass conditions
  (error condition))

(define-swank-dispatch :emacs-rex swank-client (sexp package thread cont)
  (let ((abort-cont *abort-continuation*))
   (flet ((swank-continuation (result)
           (destructure-case result
                             ((:ok result)
                              (when cont
                                (funcall cont result)))
                             ((:abort &optional reason)
                              (when abort-cont
                                (funcall abort-cont (unless (and reason (string-equal reason "NIL")) reason)))))))
    (let ((id (register-continuation swank-client #'swank-continuation)))
      (if *shortcut-swank-client-outbound*
        (let ((slime-dispatching-connection (swank-connection swank-client)))
          (slime-send `(:emacs-rex ,sexp ,package ,thread ,id)))
        (swank-enqueue-event swank-client :emacs-rex sexp package thread id))))
  t))

(define-swank-dispatch :return swank-client (value id)
  (let ((continuation (retrieve-continuation swank-client id)))
    (if continuation
      (funcall continuation value)
      (error "Unexpected reply id: ~S" id)))
  t)

(define-swank-dispatch :emacs-return swank-client (thread tag value)
  (swank-enqueue-event swank-client :emacs-return thread tag value))

(define-swank-dispatch :not-connected swank-client ()
  ; not part of swank
  (warn "Not connected")
  T)

(define-swank-dispatch :invalid-rpc swank-client (id message)
  (retrieve-continuation swank-client id)
  (warn "Invalid RPC (~A) ~S" id message)
  t)

(define-swank-dispatch :ping swank-client (thread tag)
  ; per slime:
  (slime-send `(:emacs-pong ,thread ,tag))
  t)

(define-swank-dispatch :inspect swank-client (value thread tag)
  (declare (ignore thread tag))
  (destructuring-bind (&key id title content) value
    (declare (ignore id title))
    (cl:inspect content)
  t))

 ;; ## should be redirect rather than reconnect:

(define-condition reconnect ()
   ((port :reader reconnect-port :initarg :port)))

(define-swank-dispatch :reconnect swank-client (&key port)
   ; internal addition to swank protocol
   (signal 'reconnect :port port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOTE THREAD

(defclass remote-repl ()
  ((swank-client :initform nil :accessor swank-client)
   (thread-id 
    :accessor thread-id
    :initform nil
    :documentation "Identifies the thread on the swank server")
   (swank-package :accessor swank-package :initform "COMMON-LISP-USER"))
  (:documentation "Represents a repl on the remote LISP interfaced by the swank-client"))

(defmethod client-connection ((thread remote-repl))
  (let ((swank-client (swank-client thread)))
    (when swank-client
       (swank-connection swank-client))))

(defmethod dispatch-available-input ((thread remote-repl))
  "Dispatch available input from swank server, returning a true value unless no input was available"
  ;; ## simplify by eliminating the connection!
  (let ((connection (client-connection thread)))
    (handler-case
      (when connection
         (swank-client-dispatch-next (stream->swank-client connection) :timeout 0))
      #+mcl
      (ccl::opentransport-peer-closed (c)
                                      (error 'not-connected :message (ccl::opentransport-error-message c)) ))
    t))

(defmethod set-swank-client ((thread remote-repl) client)
  (setf (swank-client thread) client)
  (push thread (slot-value client 'listeners)))

(defmethod swank-remote-call ((thread remote-repl) command-string &optional (package (swank-package thread)))
   (assert (swank-client thread))
   (swank-remote-call (swank-client thread) command-string package))

(defmethod swank-remote-eval ((thread remote-repl) form &optional package)
   (assert (swank-client thread))
   (swank-remote-eval (swank-client thread) form (or package (swank-package thread))))

(defmethod swank-remote-eval-async ((thread remote-repl) form &optional cont package)
   (assert (swank-client thread))
   (swank-remote-eval-async (swank-client thread) form cont (or package (swank-package thread))))

(defmethod dispatch-return-string ((thread remote-repl) thread-id string tag)
   (check-type tag number)
  (let ((client (swank-client thread))
        (string (concatenate 'string  string #.(string #\linefeed)))) ; ## eliminate eol? 
    (swank-enqueue-event client :emacs-return-string thread-id tag string)))

;; All communication both to and from the Swank server goes through the dispatch below.
;; Should return NIL for default event handling in swank-client.lisp, or non-nil if the event is fully processed.

(define-swank-dispatch :write-string remote-repl (output &optional mode)
  (declare (ignore mode))
  (write-string (substitute #\newline #\linefeed output) remote-repl)
  t)

(define-swank-dispatch :background-message remote-repl (output)
  (write-string (substitute #\newline #\linefeed output) remote-repl)
  t)

(define-swank-dispatch :read-output remote-repl (output)
  (declare (ignore output))
  t)

(define-swank-dispatch :presentation-start remote-repl (id #-swank2 thread)
  (declare (ignore id thread))
  t)

(define-swank-dispatch :presentation-end remote-repl (id #-swank2 thread)
  (declare (ignore id thread))
  t)

(define-swank-dispatch :emacs-rex remote-repl (form package thread continuation)
  (declare (ignore form package thread continuation))
  nil)

(define-swank-dispatch :return remote-repl (value id)
  (declare (ignore value id))
  nil)

(define-swank-dispatch :debug-activate remote-repl (thread level dummy)
  (declare (ignore thread dummy))
  (setf ccl::*break-level* level)
  t)

(define-condition swank-condition (simple-condition) ())

(define-swank-dispatch :debug remote-repl (thread level condition restarts frames conts)
  (declare (ignore thread conts level restarts))
  (format remote-repl "ENTER DEBUG - ~A~%~S~%" (car condition) (cdr condition))
  (format remote-repl "BACKTRACE:~%")
  (dolist (item frames)
    (format remote-repl "~A~%" item))
  t)

(define-swank-dispatch :debug-return remote-repl (thread level stepping)
  (declare (ignore thread stepping))
  (setf ccl::*break-level* level)
  t)

(define-swank-dispatch :emacs-interrupt remote-repl (thread)
  (declare (ignore thread))
  t)

(define-swank-dispatch :read-string remote-repl (thread tag)
   (declare (ignore thread tag))
  t)

(define-swank-dispatch :evaluate-in-emacs remote-repl (thread tag)
  (declare (ignore thread tag))
  t)

(define-swank-dispatch :read-aborted remote-repl (thread tag)
  (declare (ignore thread tag))
  t)

(define-swank-dispatch :emacs-return-string remote-repl (thread tag string)
  (slime-send `(:emacs-return-string ,thread ,tag ,string))
  t)

(define-swank-dispatch :new-package remote-repl (package prompt-string)
  (declare (ignore package prompt-string))
  t)

(define-swank-dispatch :new-features remote-repl (features)
  (declare (ignore features))
  nil)

(define-swank-dispatch :indentation-update remote-repl (info)
  (declare (ignore info))
  t)

(define-swank-dispatch :open-dedicated-output-stream remote-repl (port)
  (declare (ignore port))
  t)

(define-swank-dispatch :eval-no-wait remote-repl (fun args)
  (declare (ignore fun args))
  t)

(define-swank-dispatch :eval remote-repl (thread tag fun args)
  (declare (ignore thread tag fun args))
  t)

(define-swank-dispatch :emacs-return remote-repl (thread tag value)
  (declare (ignore thread tag value))
  nil)

(define-swank-dispatch :ed remote-repl (what)
  "WHAT can be:
  A filename (string),
  A list (FILENAME LINE [COLUMN]),
  A list (FILENAME :charpos CHARPOS),
  A function name (symbol or cons),
  nil."
  ;; ## incomplete:
  (handler-case
    (cl:ed what)
    (file-error (c)
       (warn "~A" c)))
  t)

(define-swank-dispatch :debug-condition remote-repl (thread message)
  (declare (ignore thread message))
  t)

(define-condition swank-reader-error (ccl::simple-reader-error) ())

(define-swank-dispatch :reader-error remote-repl (expression condition-string)
  #+ignore (error "~A~&~%~A" condition-string expression)
  (cerror "Ignore" 'swank-reader-error 
          :stream (swank-client::slime-connection)
          #+mcl :format-string #-mcl :format-control "Error when reading from swank (~A)~&~A"
          :format-arguments (list condition-string expression))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :swank-client-clos)


    




