;;; -*- Mode: LISP; Package: autostart; Syntax: Common-lisp; Base: 10.;  -*-
;;
;; Start a local lisp system with a swank server
;;; 
;;; Copyright (C) 2009-2012 Terje Norderhaug <terje@in-progress.com>
;;;
;;; You are hereby granted the rights to distribute and use this
;;; software as governed by the terms of the Lisp Lesser GNU Public
;;; License (http://opensource.franz.com/preamble.html), known as the
;;; LLGPL. Other free licenses available upon request.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage autostart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PORT NUMBER EXCHANGE

(in-package :ccl) ;; should be swank-client!

(defun tempdir ()
  #+mcl
  (ccl::findfolder -32767  ; #$konappropriatedisk 
                   :|temp| ; #$ktemporaryfoldertype
                   t)
  #-mcl
  (ccl::temp-pathname))

(defun gen-temp-name (&optional (folder (tempdir)))
  "Returns the pathname to a unique temporary file"
  ;; see also gen-file-name function!
  (let ((path (merge-pathnames
               (make-pathname :name (write-to-string 
                                     (mod (+ (get-universal-time) (random most-positive-fixnum))
                                          most-positive-fixnum))
                              :type "tem") 
               (or folder #+mcl (get-app-directory) #-mcl #p"home:"))))
    (without-interrupts ; ad-hoc to avoid reentry
     (if (probe-file path)
       (gen-temp-name)
       path
       #+ignore
       (create-file path)))))

;; MCL could perhaps benefit from using this, like in open-internal??

(defun file-open-p (path)
  "True if the file exists and is open"
  #+mcl
  (multiple-value-bind (exists bits) (probe-file-x path)
    (when
      (and exists
           (logbitp 3 ; #$kfsnodedataopenbit 
                    bits))
      bits))
  ; http://developer.apple.com/library/mac/#documentation/Cocoa/Reference/Foundation/Classes/nsfilemanager_Class/Reference/Reference.html#//apple_ref/c/data/NSFileBusy
  ; better to use attributesOfItemAtPath:error:   -> NSFileBusy which indicates whether the file is busy.
  ; Incorrect ad-hoc implementation until the above is in place: 
  #+clozure
  (and (probe-file path)
       (ignore-errors 
        (with-open-file (stream path)
          (zerop (file-length stream))))))

(defun retrieve-swank-file-data (temp-file)
  "Wait until swank has completed writing to the file, then read the data"
  (process-wait "Get Swank Port" 
                (lambda (file)(and (probe-file file)
                                   #+mcl
                                   (not (file-locked-p file))
                                   (not (file-open-p file))
                                   #+mcl
                                   (plusp (file-data-size file))))
                temp-file)
  (with-open-file (in temp-file :if-does-not-exist :error)
    (read in)))       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :autostart)

(defun remote-lisp-initform (name &key initfile logfile portfile &aux (*print-case* :downcase))
  ; make sure all symbols are downcase for compatibility with case sensitive lisps like Allegro mlisp!
  (declare (ignore name))
  (check-type initfile (or pathname null))
  (check-type logfile pathname)
  (check-type portfile pathname)
  (flet ((remote-pathname (pathname)
           (let ((pathname (full-pathname pathname)))
             `(make-pathname :directory ',(list* :absolute #+mcl "Volumes" (cdr (pathname-directory pathname))) 
                             :name ,(pathname-name pathname) 
                             :type ,(pathname-type pathname)))))
    (concatenate 'string
                   "(progn "
                   " (swank-loader::load-swank)"
                   " (set (swank-loader::q \"swank::*global-debugger*\") nil)"
                   " (funcall (swank-loader::q \"swank-loader:init\") :load-contribs t)"
                   (format nil (concatenate 'string
                                              " (set (swank-loader::q \"swank::*log-output*\")"
                                              "  (open ~S :direction :output :if-exists :append :if-does-not-exist :create #+clozure :sharing #+clozure :lock))")
                            (remote-pathname logfile))
                   (when initfile (format nil " (load ~S)" (remote-pathname initfile)))
                   (format nil "(funcall (swank-loader::q \"swank::start-server\") ~S :dont-close t)"
                            (remote-pathname portfile) #+ignore (posix-namestring portfile))
                   ")"
                   )))

; (remote-lisp-initform :mcl :initfile #p"Mirror:foo" :logfile #p"Mirror;bar" :portfile #p"ccl;foobar")
