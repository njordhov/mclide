(in-package :ccl)

(eval-when (:load-toplevel :execute)
  (let ((update-path (full-pathname (loading-file-source-file))))
  #+mcl (compile-load (merge-pathnames ";;Resources;init-update.lisp" update-path)
                :ignore-compiler-warnings T)
  #-mcl (load (merge-pathnames ";..;Resources;init-update.lisp" update-path))))