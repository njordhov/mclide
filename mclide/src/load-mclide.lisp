;; Load the main modules of MCLIDE
;; 2009-2012 Terje Norderhaug <terje@in-progress.com>
;; LLGPL

(in-package :ccl)

(require :asdf "resources:lib;asdf;asdf")

;; hack to bypass that asdf::apply-output-translations doesn't translate logical pathnames
;; to avoid that compiled files is kept with sources:

(advise asdf::apply-output-translations
   (let ((ccl::arglist (mapcar #'full-pathname ccl::arglist)))
     (:do-it))
   :when :around :name :logical-apply-output-translations)

(eval-when (:compile-toplevel :load-toplevel)
   (asdf:enable-asdf-binary-locations-compatibility 
      :centralize-lisp-binaries t ))

(load "resources:lib;usocket;usocket.asd")

#+mcl ;; still needed with new usocket?
(load "resources:lib;split-sequence;split-sequence.asd")

(load "resources:src;swank-client;swank-client.asd")
(load "resources:src;remote;remote.asd")

#+clozure
(load "resources:src;ccl-frontend;ccl-patches;ccl-patches.asd")

(load #+mcl "resources:src;mcl-frontend;frontend.asd"
      #+clozure "resources:src;ccl-frontend;frontend.asd")

(defParameter *swank-loader* 
  (probe-file "resources:Slime;swank-loader.lisp"))

(asdf:defsystem mclide
   :author ("Terje Norderhaug")
   :description "MCLIDE"
   :long-description "Macintosh IDE for lisp implementations on any platform"
   :license "LLGPL"
   :serial t
   :components  ()
   :depends-on (remote frontend))

(asdf:load-system :mclide)