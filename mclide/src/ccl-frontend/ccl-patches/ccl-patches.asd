(in-package :asdf)

(defsystem ccl-improvements
   :author "Terje Norderhaug"
   :description "General improvements for Clozure"
   :license "LLGPL - same as Clozure"
   :serial t
   :components
   ((:file "choose-file-dialog-patch")
    (:file "menu-separator-patch")
    (:file "custom-inspector-title")
    (:file "inspect-selection")
    (:file "updated-swank-listener")))

(defsystem ccl-fixes
   :author "Terje Norderhaug"
   :description "Fixes flaws in Clozure"
   :license "LLGPL - same as Clozure"
   :serial t
   :components
   ((:file "search-files-dialog-patch")
    (:file "listener-count-patch")
    (:file "restarts-dialog-fix")))

(defsystem ccl-mods
   :author "Terje Norderhaug"
   :description "Modifies Clozure for MCLIDE"
   :license "LLGPL - same as Clozure"
   :serial t
   :components
   ((:file "apropos-package-strings")
    (:file "call-with-documentation")
    (:file "enqueue-with-processes")
    (:file "ov-item-patch")
    (:file "listener-load-file")
    (:file "custom-search-path")))

(defsystem ccl-patches
   :author "Terje Norderhaug"
   :description "Patches Clozure for MCLIDE2"
   :license "LLGPL - same as Clozure"
   :serial t
   :components
   ((:file "ccl-patches"))
  :depends-on (ccl-improvements ccl-fixes ccl-mods))
