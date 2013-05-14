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

(in-package :asdf)

(asdf:defsystem swank-client
   :author ("Terje Norderhaug")
   :description "MCLIDE"
   :long-description "Swank client for Common Lisp"
   :license "LLGPL"
   :serial t
   :components 
   ((:file "swank-rpc")
    (:file "symbols")
    (:file "values-queue")
    (:file "swank-client"))
   :depends-on (usocket))
