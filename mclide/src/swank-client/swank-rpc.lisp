;;  Load and patch Slime:swank-rpc with improvements
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
 
;; Parts should be added to the SLIME project! 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'swank-backend)
               ; temporary to be able to load Slime;swank-rpc until it has been updated with patches to eliminate use oif swank-backend
    (defpackage :swank-backend
      (:use :cl)
       (:export
        #:utf8-to-string
        #:string-to-utf8
        #:quit-lisp))))

(eval-when (:load-toplevel :compile-toplevel :execute)
   (unless (ignore-errors (find-package :swank-rpc))
      (load "Resources:Slime;swank-rpc")))

#+clozure
(ccl::advise gui::load-and-start-swank
            (load "resources:src;swank-client;swank-rpc")
            :when :after :name reload-swank-rpc)
          
(in-package :swank-rpc)

(defvar *utf8-to-string-function*
   #-clozure nil
   #+clozure
     (lambda (octets)
      (ccl:decode-string-from-octets octets :external-format :utf-8)))

(defun utf8-to-string (octets)
   (if *utf8-to-string-function*
     (funcall *utf8-to-string-function* octets)
     (let* ((size (length octets))
              (string (make-string size)))
       (dotimes (index size string)
          (setf (schar string index)
                  (code-char (aref octets index)))))))

(defvar *string-to-utf8-function*
   #-clozure nil
   #+clozure
   (lambda (string)
      (ccl:encode-string-to-octets string :external-format :utf-8)))

(defun string-to-utf8  (string)
   (if *string-to-utf8-function*
     (funcall *string-to-utf8-function* string)
     (let* ((size (length string))
              (octets (make-array size)))
       (dotimes (index size octets)
          (setf (aref octets index)
                  (char-code (schar string index)))))))

(defun read-packet (stream)
  (let* ((length (parse-header stream))
         (octets (read-chunk stream length)))
    (handler-case (utf8-to-string octets)
      (error (c) 
        (error (make-condition 'swank-reader-error 
                               :packet (asciify octets)
                               :cause c))))))

(defun write-message (message package stream)
  (let* ((string (prin1-to-string-for-emacs message package))
         (octets (handler-case (string-to-utf8 string)
                   (error (c) (encoding-error c string))))
         (length (length octets)))
    (write-header stream length)
    (write-sequence octets stream)
    (finish-output stream)))

(defun encoding-error (condition string)
  (string-to-utf8
   (prin1-to-string-for-emacs
    `(:reader-error
      ,(asciify string)
      ,(format nil "Error during string-to-utf8: ~a"
               (or (ignore-errors (asciify (princ-to-string condition)))
                   (asciify (princ-to-string (type-of condition))))))
    (find-package :cl))))

(defvar *intern-foreign-symbol* NIL) ;; ## better name??

(defun intern-foreign-symbol (string)
  (funcall (or *intern-foreign-symbol* #'intern) string))

(defun simple-read ()
   "Reads a form that conforms to the protocol, otherwise signalling an error."
   (let ((c (read-char)))
     (case c
       (#\" (with-output-to-string (*standard-output*)
              (loop for c = (read-char) do
                    (case c
                      (#\" (return))
                      (#\\ (write-char (read-char)))
                      (t (write-char c))))))
       (#\( (loop collect (simple-read)
                  while (ecase (read-char)
                          (#\) nil)
                          (#\space t))))
       (#\' `(quote ,(simple-read)))
       (t (let ((string (with-output-to-string (*standard-output*)
                          (loop for ch = c then (read-char nil nil) do
                                (case ch
                                  ((nil) (return))
                                  (#\\ (write-char (read-char)))
                                  ((#\space #\)) (unread-char ch)(return))
        ;; ## only for mclide until swank distribution has a writer for symbols to never serialize them as :|symbol|
                                  #+(or clozure mcl)
                                  (#\| (loop for c = (read-char nil nil) until (eql c #\|) do (write-char c)))
                                  (t (write-char ch)))))))
            (cond ((digit-char-p c) (parse-integer string))
                  ((intern-foreign-symbol string))))))))

;; don't install, just for now to handle remote symbols not in existing package!

(ccl::advise read-form
   (handler-case
     (:do-it)
      (CCL::NO-SUCH-PACKAGE (c)
       (declare (ignore c))
       (destructuring-bind (string package) ccl::arglist
        (with-standard-io-syntax
            (let ((*package* package))
                  (with-input-from-string (*standard-input* string)
                      (simple-read)))))))
   :when :around :name custom-read-form)

#+ignore
(defun read-form (string package)
  (with-standard-io-syntax
    (let ((*package* package))
      (if *validate-input*
        (with-input-from-string (*standard-input* string)
          (simple-read))
        (handler-case
          (read-from-string string)
          (CCL::NO-SUCH-PACKAGE (c)
           (declare (ignore c))
           (with-input-from-string (*standard-input* string)
             (simple-read))))))))

;; This fails coming from Allegro Express:
;; (swank-rpc::read-form "(:|YACC Release 0.98|) "*package*)
;; swank-rpc need a compliant printer as this is invalid syntax!

#+clozure ;; still needed?
(when (and (find-package :swank)
                  (find-symbol (string :read-message) :SWANK))
    (import (read-from-string  "SWANK::READ-MESSAGE") :swank-rpc)
    (export (read-from-string "SWANK::READ-MESSAGE") :swank-rpc))









