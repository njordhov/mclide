(in-package :asdf)

(defsystem frontend
   :author "Terje Norderhaug"
   :description "MCLIDE2 frontend using CCL"
   :license "LLGPL"
   :serial t
   :components 
   ((:file "load-cocoa")
    (:file "mclide")
    (:file "connect-dialog")
    (:file "listener")
    (:file "alert-sheet")
    (:file "readloop")
    (:file "backtrace")
    (:file "inspector")
    (:file "apropos")
    (:file "processes")
    (:file "autostart")
    (:file "mclide-menu"))
  :depends-on (ccl-patches contextual remote autostart))