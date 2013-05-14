;;; -*- Mode: LISP; Package: :swank-client; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Values Queue
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

(in-package :swank-client) ;; ## should be its own package

(defstruct values-queue 
  head
  tail
  (lock (ccl::make-lock 'queue))
  #+clozure
  (semaphore (ccl::make-semaphore))
)

(defun values-queue-enqueue (queue &rest values &aux (lock (values-queue-lock queue)))
  "Appends the item at the end of the queue"
  (declare (dynamic-extent values))
  (ccl::with-lock-grabbed (lock) 
   (let ((values (ccl::cheap-cons (ccl::cheap-copy-list values) NIL))) 
     (if (values-queue-tail queue)
       (setf (values-queue-tail queue)
             (setf (cdr (values-queue-tail queue))
                   values))
       (setf (values-queue-head queue)
             (setf (values-queue-tail queue)
                   values)))))
  #+clozure
  (ccl::signal-semaphore (values-queue-semaphore queue))
  t)

(defun values-queue-dequeue (queue &optional timeout &aux (lock (values-queue-lock queue)))
  "Pop the next values from the queue, optionally timing out within the given number of seconds"
  (labels ((queue-listen (queue)
             (ccl::with-lock-grabbed (lock) 
               (let* ((head (values-queue-head queue)))
                 (when head
                   (unless (cdr head)
                     (setf (values-queue-tail queue) NIL))
                   (let ((values (ccl::pop-and-free head)))  ;; can only be used on symbols per MCL 4.3.1
                     (setf (values-queue-head queue) head)
                     values)))))
           #-clozure
           (queue-pop (queue &aux item)
             (ccl::process-wait-with-timeout "dequeue" timeout
                                             (lambda (queue)
                                               (setf item (queue-listen queue)))
                                             queue)
             item)
           #+clozure
           (queue-pop (queue)
             (if timeout
               (ccl::timed-wait-on-semaphore (values-queue-semaphore queue) timeout) 
               (ccl::wait-on-semaphore (values-queue-semaphore queue) nil "Values Queue"))
             (queue-listen queue)))
    (let ((values (queue-pop queue)))
      (multiple-value-prog1
          (apply #'values values)
        (ccl::with-lock-grabbed (lock) 
          (ccl::cheap-free-list values))))))

(defun values-queue-empty-p (queue &aux (lock (values-queue-lock queue)))
  (ccl::with-lock-grabbed (lock)
   (not (values-queue-head queue))))
    




