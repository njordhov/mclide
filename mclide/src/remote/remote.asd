(in-package :asdf)

(asdf:defsystem remote
   :author ("Terje Norderhaug")
   :description "MCLIDE"
   :long-description "Remote swank client for MCLIDE"
   :license "LLGPL"
   :serial t
   :components 
   ((:file "remote")
    (:file "inspect"))
   :depends-on (swank-client))

(asdf:defsystem contextual
   :author ("Terje Norderhaug")
   :description "quasi-contextual programming"
   :long-description "Helpers to use quasi-contextual programming when coercing a lisp into mclide"
   :license "LLGPL"
   :serial t
   :components 
   ((:file "contextual")))

(asdf:defsystem autostart
   :author ("Terje Norderhaug")
   :description "Start a Lisp with a swank server"
   :license "LLGPL"
   :serial t
   :components 
   ((:file "autostart")))

