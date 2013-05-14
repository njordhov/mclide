#| Start swank by loading into MCL or one of the Common Lisp implementations supported by Slime/Swank.
   Then connect from MCLIDE using the Connect Lisp item of the New Listener submenu.
   Terje Norderhaug, November 2009. LLGPL.

   Examples of use:
   #+mcl
   (load (probe-file (or *loading-file-source-file* "home:start-swank.lisp")) :print T :verbose nil)
   #-mcl
   (load "start-swank.lisp" :external-format :utf8)

   Implementation note: No on-line comments to get around end-of-line issues with different lisps.
|#

(in-package :cl-user)

(defvar *swank-loader*
  (or #+mcl
      (probe-file (merge-pathnames "mcl-swank-loader.lisp" 
                                   (or *loading-file-source-file* *load-pathname* *loading-file-source-file*)))
      #+mcl
      (probe-file (merge-pathnames "MCLIDE.app;Contents;Resources;lib;mcl-swank-loader.lisp"
                                   (or *loading-file-source-file* *load-pathname* "home:")))
      #-mcl
      (probe-file
       (merge-pathnames (make-pathname :directory '(:relative :up :up "Slime") :name "swank-loader" :type "lisp")
                       (or *load-truename* *load-pathname*)))
      #-mcl
      (probe-file (merge-pathnames
                   (make-pathname :directory '(:relative "MCLIDE.app" "Contents" "Resources" "Slime") 
                                  :name "swank-loader" 
                                  :type "lisp")
                   (or *load-truename* *load-pathname*)))
      (error "Cannot locate the swank loader"))
  "Path to 'MCLIDE.app/Contents/Resources/Slime/swank-loader.lisp' or equivalent")

(load *swank-loader*)

(swank-loader::init :reload T)

(setq swank::*global-debugger* NIL)

(defParameter *swank-port* (swank::create-server :port 0 :dont-close t))

*swank-port*
