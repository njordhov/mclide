; Builds the bundle distribution of mclide.
;
; Terje Norderhaug <terje@in-progress.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(require :asdf)

(ccl::add-feature :clz)

(defvar *top-pathname* (merge-pathnames ";..;" (directory-namestring (ccl::loading-file-source-file)))) 

(defvar *destination-root* *top-pathname*)

(defparameter *mclide-repo-pathname* (directory-namestring (ccl::loading-file-source-file)))

(setf (logical-pathname-translations "home")
      `(("**;*.*" ,(merge-pathnames ";**;*.*" *mclide-repo-pathname*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POSIX COMMANDS 

(in-package :ccl)

(export '(probe-posix relative-pathname posix-namestring posix-relative-namestring execute-shell-command create-alias copy-files)) 

;; ## All of these should be eliminated!!

#+ccl
(defun probe-posix (posix-namestring)
  "If a file exists for the posix namestring, return the pathname"
  (if (probe-file posix-namestring)
    posix-namestring))

#+ccl
(defun relative-pathname (source destination)
  "Creates a relative path from the source to the destination"
  (do ((name (pathname-name destination))
       (type (pathname-type destination))
       (source (pathname-directory (full-pathname source)) (cdr source))
       (destination (pathname-directory (full-pathname (merge-pathnames destination source))) (cdr destination)))
      ((or (not source) (not destination) (not (equal (car source)(car destination))))
       (values
        (make-pathname 
         :directory (append
                     '(:relative)
                     (substitute-if :up #'true (if name source (cdr source)))
                     destination)
         :name name
         :type type)
        #+ignore
        source
        #+ignore
        destination))))

(defun posix-namestring (ns)
  (namestring ns))

#+ccl
(defun posix-relative-namestring (pathname)
  "Same as posix-namestring but also handles relative pathnames (ad-hoc, incomplete, unsafe)"
  ; should really be covered by posix-namestring and properly implemented there!!
  (let ((d (pathname-directory pathname)))
    (cond
     ((eq :relative (first d))
      (with-output-to-string (out)
        (dolist (item (cdr d))
          (case item 
            (:up 
             (write-string "../" out))
            (otherwise
             (write-string item out)
             (write-char #\/ out))))
        (when (pathname-name pathname)
          (write-string (pathname-name pathname) out))
        (when (and (pathname-type pathname)
                   (neq (pathname-type pathname) :unspecific))
          (write-char #\. out)
          (write-string (pathname-type pathname) out))))
     (t
      (posix-namestring pathname)))))

#+ccl
(defun execute-shell-command (command &key output-stream)
  #+ignore
  (let ((asdf::*verbose-out* (or output-stream asdf::*verbose-out*)))
    (asdf:run-shell-command "~A" command))
  (when output-stream
    (format output-stream "~&$ ~A~%" command))
  (nth-value 1
             (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output (or output-stream (make-broadcast-stream))
                                 :wait t))))

#+ccl
(defun create-alias (pathname target &key relative debug)
  "Creates a symbolic link to a target file; The pathname can be a file- or directory path"
  (execute-shell-command
   (format nil "ln -s ~S ~S" 
           (if relative
             (posix-relative-namestring (relative-pathname pathname target))
             (posix-namestring (full-pathname target)))
           (if (directory-pathname-p pathname)
             (posix-namestring (dirpath-to-filepath pathname))
             (posix-namestring (full-pathname pathname))))
   :output-stream (when debug *debug-io*))
  pathname)

#+ccl
(defun copy-files (source destination &key verbose shell-p)
  "Copies the content of a source directory to the destination directory"
  (if shell-p
    (let* ((src (ccl::posix-namestring (ccl::dirpath-to-filepath source)))
           (des (ccl::posix-namestring (ccl::dirpath-to-filepath destination)))
           (command (format nil "cp -pR ~S ~S" src des)))  ; -pR
      (let ((result (ccl::execute-shell-command command)))
        (assert (= result 0)() "Failed to copy files with ~S" command)))
    (let ((source (full-pathname source))
          (files (directory (merge-pathnames ";**;*.*" source))))
      (unless files
        (warn "No files to copy from ~S" source))
      (dolist (file files)
        (unless nil
          (let* ((path (enough-namestring file source))
                 (dest (merge-pathnames path destination)))
            (when verbose
              (format T "COPY ~S ~S~%" path dest))
            (copy-file file dest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIG

(in-package :cl-user)

; Whether to pre-load modules and patches:
(ccl::add-feature :prebuild)

;; Whether to use Swank 2 or the latest version of Swank (soon to be swank 3):
(ccl::add-feature :swank3)

(defun ask-directory (&key prompt directory)
  (if (find-package "GUI")
    (funcall (intern "GUI::COCOA-CHOOSE-DIRECTORY-DIALOG")
           :directory directory)
    (progn
      (write-line prompt)
      (pathname (read-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERSION

(defParameter *appname* "MCLIDE")

(defparameter *app-version* "2.0a1")

(defparameter *app-revision* "0")

(defparameter *increment-bundle* T)

(defvar *bundle-version* (with-open-file (stream "home:version.txt" :direction :io :if-exists :overwrite :if-does-not-exist nil)
                           (when stream
                             (let ((version  (read stream NIL)))
                               (ccl::stream-position stream 0)
                               (write (if *increment-bundle* (1+ version) version) :stream stream)
                               version)))
  "CFBundleVersion (String) specifies the build version number of the bundle, which identifies an iteration (released or unreleased) of the bundle. This is a monotonically increased string, comprised of one or more period-separated integers.")

(defparameter *bundle-release* 555)  ; Set to value of *bundle-version* for a new release!

(defparameter *short-version-string* (format nil "~A~@[-~A~]" *app-version* (if *bundle-version* 
                                                                              (when (> *bundle-version* *bundle-release*) (- *bundle-version* *bundle-release*))
                                                                              *app-revision*))                                                                                
  "CFBundleShortVersionString (String) specifies the release version number of the bundle, which identifies a released iteration of the application. The release version number is a string comprised of three period-separated integers. The first integer represents major revisions to the application, such as revisions that implement new features or major changes. The second integer denotes revisions that implement less prominent features. The third integer represents maintenance releases.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defMacro generate-lisp (filename &rest lisp)
  `(with-open-file (*standard-output* ,filename
                                     :direction :output :if-does-not-exist :create :if-exists :append)
    (dolist (item ',lisp)
      (typecase item
        (string 
         (fresh-line)
         (write-string item))
        (otherwise 
         (pprint item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUNDLE STRUCTURE

(defparameter *distribution*
  (merge-pathnames
    (format nil ";~A ~A;" *appname* *short-version-string*) 
    (or *destination-root*
        (ask-directory-dialog
           :prompt "Where would you like to build MCLIDE?"
           :directory (directory-namestring (ccl::loading-file-source-file))))))

(assert (not (probe-file *distribution*)) ()
  "An existing MCLIDE distribution should be removed before a new build: ~S" 
  *distribution*)
(create-directory *distribution*)
(assert (probe-file *distribution*))

(copy-files (merge-pathnames ";aux;"  *mclide-repo-pathname*) *distribution*)

(defParameter *root* (merge-pathnames 
                      (make-pathname :name *appname* :type "app")
                      *distribution*))

(unless (directory-pathname-p *root*)
  (setf *root*
        (make-pathname :directory (namestring *root*) :defaults nil)))

(assert (not (probe-file *root*)))
(create-directory *root*)

(defParameter *contents* (merge-pathnames ";Contents;" (directory-namestring *root*)))

(assert (not (probe-file *contents*)))
(create-file *contents*)

(copy-files (directory-namestring (merge-pathnames ";Contents;" *mclide-repo-pathname*)) *contents*)

(defParameter *plugins-folder* (merge-pathnames ";PlugIns;" *contents*))

(create-file *plugins-folder*)

(defParameter *plugins-disabled-folder* (merge-pathnames ";PlugIns Disabled;" *contents*))

(assert (not (probe-file *plugins-disabled-folder*)))
(create-file *plugins-disabled-folder*)

(defParameter *resources-path* (merge-pathnames ";Resources;" *contents*))

(create-file *resources-path*)

(create-file (merge-pathnames ";Resources Disabled;" *contents*))

(defParameter *mclide-app* (merge-pathnames (make-pathname :type :unspecific)
                                                           (merge-pathnames *appname* *resources-path*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; copy repository to resources and create links

(defparameter *resources-repository-path* 
  (let ((dirname  (first (last (pathname-directory *mclide-repo-pathname*)))))
    (merge-pathnames (make-pathname :directory (list :relative dirname)) *resources-path*)))

; cannot use copy-files as it changes the modification times, conflicting with hg.
#+ignore
(ccl::execute-shell-command (format nil "cp -pLR ~S ~S" 
                                    (ccl::posix-namestring (ccl::dirpath-to-filepath (directory-namestring *mclide-repo-pathname*)))
                                    (ccl::posix-namestring (ccl::dirpath-to-filepath *resources-repository-path*))))

(copy-files (directory-namestring *mclide-repo-pathname*) *resources-repository-path* :shell-p T)

(dolist (path '(";src;"))
  (create-alias (merge-pathnames path *resources-path*) 
                (merge-pathnames path *resources-repository-path*) :relative T))

(defparameter *library-pathname* (merge-pathnames ";lib;" *top-pathname*))

(copy-files *library-pathname* (merge-pathnames ";lib;" *resources-path*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME

(defParameter *slime-orig*
  (let ((org  (merge-pathnames ";slime;" *top-pathname*)))
    (if (probe-file org)
      org
      (ask-directory 
         :prompt "Locate Slime Directory" 
         :directory *top-pathname*))))

(assert (probe-file *slime-orig*))

(defparameter *slime-target* (merge-pathnames ";slime-xxx;" *resources-path*))


;(defparameter *slime-target* (merge-pathnames (make-pathname :directory (list :relative (first (last (pathname-directory *slime-orig*)))))
;                                              *resources-path*))

(defParameter *swank-path* (merge-pathnames ";Slime;" *resources-path*))

(copy-files *slime-orig* *slime-target* :shell-p T)  ;; Error: Is a directory : #P"/Users/terje/Desktop/WORKSPACE MCLIDE GIT/TMP/MCLIDE 2.0a0-x99x/MCLIDE.app/Contents/Resources/mclide-repo/src/"

(assert (probe-file *slime-target*))

; ???
(create-alias *swank-path* *slime-target* :relative T)

(create-alias (merge-pathnames "start-swank.lisp" *distribution*) (merge-pathnames ";src;start-swank.lisp" *resources-path*) :relative T)

; ???

(create-alias *swank-path* (merge-pathnames ";src;swank-mcl.lisp" *resources-path*) :relative T)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MCLIDE2 BUILD & INIT

;; Clozure command line options: http://ccl.clozure.com/manual/chapter2.5.html

(in-package :cl-user)

(defparameter *build2-pathname* (merge-pathnames "build2.lisp" *resources-path*))

(defparameter *mclide2-pathname* (merge-pathnames ";ccl;MCLIDE2.app" *resources-path*))

(defparameter *init2-filename* "init2.lisp")

(defparameter *init2-pathname* (merge-pathnames *init2-filename* *resources-path*))

(defParameter *build2-forms*
  `((in-package :ccl)
    (setf *lisp-startup-parameters* NIL) ;; required to avoid loading this file also when starting saved app!

    #+ignore (require :cocoa-application)
    " ;all below modified from cocoa-appplication.lisp "

    (defvar *cocoa-application-path* 
       "ccl:MCLIDE2.app;"
       #+ignore (full-pathname (merge-pathnames "../../../../MCLIDE3.app"  (directory-namestring (loading-file-source-file))))) ;; ## better if this could be generated!
    (defvar *cocoa-application-copy-headers-p* nil)
    (defvar *cocoa-application-install-altconsole* t)
    (defvar *cocoa-application-bundle-suffix*
      (multiple-value-bind (os bits cpu) (host-platform)
        (declare (ignore os))
        (format nil "MCLIDE-~a~a" (string-downcase cpu) bits)))
    (defvar *cocoa-application-frameworks* #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;") #-cocotron nil)
    (defvar *cocoa-application-libraries* #+cocotron '("ccl:cocotron;Foundation>.1>.0.dll" "ccl:cocotron;AppKit>.1>.0.dll") #-cocotron nil)      
    (defvar *cocoa-ide-force-compile* nil)

    (let ((*load-verbose* T))
      (load "ccl:cocoa-ide;defsystem.lisp" :verbose T)
      (load-ide *cocoa-ide-force-compile*))

    (format T "~% ---- Completed loading cocoa ----~%")

    ;; used in mclide sources so required for load (but move it to load-mclide.lisp or eliminate?):
    (add-LOGICAL-PATHNAME-TRANSLATION "resources"
                                                                           (LIST* "resources:**;*.*.*"
                                                                                     (LIST (MERGE-PATHNAMES
                                                                                               ";**;*.*.*"
                                                                                               (let ((source-file (CCL::LOADING-FILE-SOURCE-FILE)))
                                                                                                     (if source-file
                                                                                                          (directory-namestring (full-pathname source-file) )
                                                                                                          (merge-pathnames
                                                                                                           "MCLIDE.app/Contents/Resources/"
                                                                                                           (funcall (read-from-string "gui::cocoa-choose-directory-dialog")))))))))

    ; consider to just add an advise for load-ide and make this happen after to use (require :cocoa-application) instead...
    (let ((*load-verbose* T))
      (load (merge-pathnames ";src;load-mclide.lisp" 
                                              (directory-namestring (ccl::loading-file-source-file))) :verbose T))

    (format T "~% ---- Completed loading mclide libraries ----~%")

    (require :xref) ;; needed by the swank-ccl backend

    (defparameter *init-file* ,*init2-filename*) 

    (defmethod application-init-file :around (a)
       ;; cannot use resources logical pathname as it isn't yet initialized
       ;; should use a better way to find the resources folder!
       ;; probe-file resolves an eventual alias, which benefits loading-file-source-file.
       (declare (ignore a))
       (probe-file (merge-pathnames *init-file* (merge-pathnames "../Resources/*.*" (ccl::%realpath (ccl::heap-image-name))))))

    (build-ide *cocoa-application-path*)))

(in-package :cl-user)

(with-open-file (*standard-output* *build2-pathname* :direction :output)
  (dolist (form *build2-forms*)
    (pprint form)))

(defvar *clozure-directory* 
  (if (probe-file "ccl:")
    (probe-file "ccl:")
    (ask-directory
     :directory "home:"
     :prompt "Locate the Clozure folder:")))


;; ## DOES THIS COPY TO ccl subfolder or to Resources toplevel????

#+clz ; has to keep permissions so cannot just copy-files
(ccl::execute-shell-command (format nil "cp -pLR ~S ~S" 
                                    (ccl::posix-namestring (ccl::dirpath-to-filepath *clozure-directory*))
                                    (ccl::posix-namestring (ccl::dirpath-to-filepath (merge-pathnames ";ccl;" *resources-path*)))))


(defparameter *clozure-exec* (merge-pathnames ";ccl;dx86cl64" *resources-path*))
; (warn "Should also handle 32 bit Clozure!")

(assert (probe-file *clozure-exec*))

#+clz
(ccl::execute-shell-command 
 (format nil "exec ~S -l ~S" 
         (ccl::posix-namestring (full-pathname *clozure-exec*))
         (ccl::posix-namestring (full-pathname *build2-pathname*)))
 :output-stream *debug-io*)

#+clz
(create-alias (merge-pathnames 
                      (merge-pathnames *init2-filename* ";ccl;MCLIDE2.app;Contents;Resources; *.*")
                      *resources-path*)
                *init2-pathname* :relative T)

(ccl::add-logical-pathname-translation "resources" '("resources:**;*.*.*" ";**;*.*.*"))  ;; just so the init2-forms is read without missing resource: host
(defpackage :mclide) ;; just so the init2-forms is read without missing package
(defpackage :swank-client) ;; just so the init2-forms is read without missing package


#+clz
(defParameter cl-user::*init2-forms*
  `((in-package :ccl)
    (add-logical-pathname-translation "resources"
                                                                           (LIST* "resources:**;*.*.*"
                                                                                     (LIST (merge-pathnames
                                                                                               ";**;*.*.*"
                                                                                               (let ((source-file (loading-file-source-file)))
                                                                                                     (if source-file
                                                                                                          (directory-namestring (full-pathname source-file))
                                                                                                          (merge-pathnames
                                                                                                           "MCLIDE.app/Contents/Resources/"
                                                                                                           (funcall (read-from-string "gui::cocoa-choose-directory-dialog")))))))))

(flet ((replace-pathname-translation (host key value)
           (let ((translations (logical-pathname-translations host)))
                 (setf (logical-pathname-translations "ccl")
                         (substitute (list key value)
                                           (assoc key translations :test #'equal)
                                           translations)))))
       (replace-pathname-translation "ccl" #4P"ccl:**;*.*" #4P"resources:ccl;**;*.*")
       (replace-pathname-translation  "ccl" "ccl:ccl;*.*" "resources:ccl;*.*"))

#+ignore
    (add-logical-pathname-translation "ccl" `(#P"ccl:**;*.*" #p"resources:ccl;**;*.*"))
#+ignore
    (add-logical-pathname-translation "ccl" `("ccl:ccl;*.*" "resources:ccl;*.*"))
 
    (defparameter *original-error-output* *error-output*)
  
    (let ((log-output (mclide::init-logging )))
          (setf *error-output* log-output))
    
    (defparameter swank-client::slime-log-events NIL)
    (defparameter *load-verbose* NIL)

    (require :swank-loader "resources:Slime;swank-loader") ; clozure ready for connection
    (dolist (fun (reverse *mclide-startup-functions*))  ;; ## add condition handling! 
       (funcall fun))

    ;; # move to library?
    (defun load-plugin (name)
        (flet ((visible-p (file)
                 (or (plusp (length (pathname-name file)))
                     #+ignore (not (is-path-invisible file))))
               (plugins (name)
                 (directory (full-pathname (merge-pathnames (make-pathname :name name :type :wild) "resources:..;PlugIns;"))
                            :files T :directories T)))
                 
          (dolist (pathname (plugins name))
            (cond
             ((directoryp pathname)
              (dolist (file (or (directory (merge-pathnames ";Contents;MacOS;*.*" pathname) :test #'visible-p)
                                (directory (merge-pathnames ";MacOS;*.*" pathname) :test #'visible-p)  ; # eliminate asap...
                                (warn "No executable for plugin ~A" (pathname-name pathname))))
                (load file)))
           
             ((visible-p pathname)
              (load pathname))))))

     (let ((plugins (directory "resources:..;PlugIns;*.*" :files T :directories T))
            (*break-on-warnings* nil))
       (dolist (plugin plugins)
          (load-plugin 
           (if (directoryp plugin)
             (pathname-name (car (last (pathname-directory (translate-logical-pathname plugin)))))
             (pathname-name plugin)))))))

(in-package :cl-user)

#+clz
(with-open-file (*standard-output* *init2-pathname* :direction :output)
  (dolist (form *init2-forms*)
    (pprint form)))

#+clz
(ccl::execute-shell-command
 (concatenate 'string
  (format nil "build=~S; original=\"MCLIDE.app\"; contents=~S; target=~S; " 
               "mclide2.app"
               (ccl::posix-namestring (full-pathname *contents*))
               (ccl::posix-namestring (full-pathname *distribution*)))
  "mv \"$contents\"/Resources/ccl/$build \"$target\"; "
  "mv \"$contents\"/Resources/*   \"$target\"/$build/Contents/Resources ;"
  "rm \"$contents\"/Resources/  ;"
  "mv \"$contents\"/PlugIns  \"$target\"/$build/Contents ;"
  "mv \"$contents/PlugIns Disabled\"  \"$target/$build/Contents/PlugIns Disabled\"  ;"
  "mv \"$target/$original\"/Contents/Resources/English.lproj/* \"$target\"/$build/Contents/Resources/English.lproj/ ;"
  "rmdir \"$target/$original/Contents/Resources/English.lproj/\" ;"
  "rmdir \"$target/$original/Contents/Resources/\" ;"
  "rmdir \"$target/$original/Contents/\" ;"
;  "rmdir \"$target/$original/\" ;"
  "mv \"$target/$original\"  \"$target/OLD\" ;"
  ;; The rest of the build depends on this:
  "mv \"$target/$build\"  \"$target/$original\" ;"
  ;; explicit removal of everything in OLD instead of taking our changes with a recursive:
  "rm \"$target/OLD/Contents/Resources/.DS_Store\" ;"
  "rmdir \"$target/OLD/Contents/Resources\" ;"
  "rmdir \"$target/OLD/Contents/Resources Disabled\" ;"
  "rm \"$target/OLD/Contents/.DS_Store\" ;"
  "rmdir \"$target/OLD/Contents\" ;"
  "rmdir \"$target/OLD\" ;"
  ;; FIX ICON
  "cp \"$contents\"/Resources/application.icns \"$contents\"/Resources/openmcl-icon.icns")
 :output-stream *debug-io*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTUP SCRIPT

(defParameter *script* 
    (merge-pathnames (make-pathname :directory ":MacOS:" :name *appname* :type "sh") 
                                    *contents*))

(defmacro generate-script (&rest lines)
  `(progn
     ,@(mapcar
        (lambda (line)
          `(progn
             ,(etypecase line
               (string `(write-string ,line))
               (list `(format t ,@line)))
             (write-char #\linefeed)))
        lines)))

#-clz
(with-open-file (*standard-output* *script* :direction :output :external-format :unix)
  (generate-script
   "#!/bin/sh"
   "here=\"${0%/*}\""
   ("cmd='/~A'" (ccl::posix-relative-namestring (ccl::relative-pathname *script* *mclide-app*)))  ; e.g. /../Resources/MCLIDE
   ("exec \"/usr/bin/open\" -a \"$here$cmd\" $2")))


#+ignore
(with-open-file (*standard-output* *script* :direction :output :external-format :unix)
  (generate-script
   "#!/bin/sh"
   "here=\"${0%/*}\""
   ("cmd='~A'" (ccl::posix-relative-namestring (ccl::relative-pathname *script* *mclide2-pathname*)))  ; e.g. /../Resources/CCL/MCLIDE2
   ("open \"$here/$cmd\" --args -l \"$here\"/~S" (ccl::posix-relative-namestring (ccl::relative-pathname *script* *init2-pathname*)))))

(sleep 1)

(ccl::execute-shell-command (format nil "chmod u+x ~S" (ccl::posix-namestring *script*)) :output-stream *debug-io*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUNDLE PLIST

(defParameter *bundle-plist* (merge-pathnames ";Info.plist" *contents*))

(defconstant %copyright% (concatenate 'string  (string #\Latin_Capital_Letter_A_With_Circumflex #+ignore (character #xC2))
                                               (string #\Copyright_Sign #+ignore (character 169))))  ; UTF-8 unicode: (#\Â 194 (#\© 169)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLUG-INS

;; ## Consider representing plugins as a loader file (load "ccl:examples;mouse-copy") instead of using a symbolic link.

(defun declare-plugin (name path)
  "Declares a file or bundle in the distribution to be a plugin"
  (declare (special *mcl-path*))
  (let ((file (merge-pathnames (string path) (merge-pathnames ":Examples:.lisp" *mcl-path*)))
        (link (merge-pathnames name *plugins-disabled-folder*)))
    (assert (probe-file file) () "Missing plugin file: ~A" file)
    (create-alias file link :relative T :debug T)
    #+ignore
    (ccl::execute-shell-command
     (format nil "ln -s ~S ~S"
             (ccl::posix-relative-namestring (ccl::relative-pathname link file)) 
             (ccl::posix-namestring link)) 
     :output-stream *debug-io*)))

(defun create-plugin-from-folder (pathname &optional (destination *plugins-folder*))
  ;(assert (directoryp pathname))
  (let* ((name (concatenate 'string (first (last (pathname-directory pathname))) ".plugin"))
         (destination (merge-pathnames (make-pathname :directory `(:relative ,name "Contents")) destination )))
    (copy-files pathname destination)))

(defmacro define-plugin (name (&key disabled) &rest body)
   ; (declare (ignore args))
   `(generate-lisp (merge-pathnames ,name ,(if disabled '*plugins-disabled-folder* '*plugins-folder*))
      ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possibly better alternative is to use:
;; (advise ccl::make-mcl-listener-process (progn) :when :around :name 'ccl::disable-default-listener)

(define-plugin "Load Init File" ()
 "; Loads an eventual init.lisp file in the home folder"   ; should be loaded after all other plug-ins
 ""
 (in-package :ccl)
 (let ((file #-clz (probe-file "home:init.lisp")
               #+clz (probe-file
                          (merge-pathnames "../../../init.lisp" 
                                                         (full-pathname "resources:")))))
   (when file
     (load file))))

; mainly used for development, should be loaded after the Update but before other plug-ins.
#-clz
(define-plugin "_~Load Patches" ()
 "; Loads files from a top level Patches folder"
 ""
 (in-package :ccl)
 (let ((*warn-if-redefine* NIL)
       (*warn-if-redefine-kernel* NIL))
   (dolist (file (or (directory "home:Patches;**;*.*") 
                     (directory "home:Patches.lisp")))
     (unless (is-path-invisible file)
       (load file)))))

#-clz
(define-plugin "Debug MCLIDE" (:disabled T) 
  ; similar name as the initialization-output window!
 "; Put MCLIDE in debug mode."
 (in-package :ccl)
 (setf *debug-mclide* :debug)
 (when *initialization-output*
   (window-show *initialization-output*)
   (setf *standard-output* *initialization-output*) 
   (setf *error-output* *initialization-output*)
   (setf *debug-io* *initialization-output*)))

(defPackage :swank
   (:use :common-lisp))

(defPackage :swank-backend
   (:use :common-lisp))

#+ignore
(generate-lisp (merge-pathnames (format nil "Debug ~A" *appname*) *plugins-disabled-folder*)
 "; Activate to debug this application"
 ";"
 (in-package :ccl)
 (defParameter *debug-mclide* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATE

(create-plugin-from-folder "home:PlugIns SRC;_Update;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLOR CODED

#+ignore
(defParameter *color-coded-version-folder-name*
    "color-coded-20b10")

#+ignore
(ccl::execute-shell-command
     (format nil "ln -s ~S ~S"
            *color-coded-version-folder-name*
             (ccl::posix-namestring (merge-pathnames "color-coded" *resources-path*))))

#+ignore
(defPackage :cc)

#-clz
(create-plugin-from-folder "home:PlugIns SRC;Color Coded Styling;" *plugins-disabled-folder*)

#+ignore
(rename-file (merge-pathnames ";Color Coded Styling.plugin;" *plugins-folder*)
             (merge-pathnames ";Color Coded Styling.plugin;" *plugins-disabled-folder*))

#+ignore
(define-plugin "Color Coded Styling" (:disabled T)
  "; Activate the Color Coded lisp syntax styling utility by Glen Foy - www.clairvaux.org"
  (in-package :ccl)
  (let* ((edit-menu *edit-menu*)
            (*edit-menu* (make-instance 'menu 
                                      :menu-title "Dummy" :menu-items nil)))
    (compile-load "resources:color-coded;load-color-coded"))

  (load "resources:color-coded+")
  (cc::install-styles-menu)
  ) ; end styling 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOL COMPLETION

#-clz
(create-plugin-from-folder "home:PlugIns SRC;Symbol Completion;")

#+ignore
(rename-file (merge-pathnames ";Symbol Completion.plugin;" *plugins-folder*)
             (merge-pathnames ";Symbol Completion.plugin;" *plugins-disabled-folder*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT MENU

#-clz
(create-plugin-from-folder "home:PlugIns SRC;Project Menu;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STANDARD WINDOW PATH

#-clz
(define-plugin "Standard Window Path" ()
                        ; Window Path is what Apple calls it
                        "; Modernize the window path action to open selected folders in the Finder"
                        (in-package :ccl)
                        (compile-load "resources:src;application-window-title")
                        (setf *use-development-window-filepath-menu* nil))

#-clz
(define-plugin "Internal Listener" ()
 "; rename the default listener"
 (in-package :ccl)
 (defmethod set-window-title ((w listener) title)
   (if (and (eq (type-of w) 'listener)
            (not (search "Internal " title)))
     (call-next-method w (concatenate 'string "Internal " title))
     (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREFERENCES

(define-plugin "Backtrace on Break" (:disabled T)
  ";open the backtrace window on break"
  (in-package :ccl)
  ; maybe this should instead set the default in *env-categories* ?
  (setf *backtrace-on-break* T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure Listener

#-clz
(define-plugin "Ensure Listener" ()
 "; Ensures a lisp listener"
 (in-package :ccl)
 (flet ((ensure-listener ()
          ; startup is completed before creating listener
          ; avoids prompt written to the initialization output 
          (process-wait "until started" (lambda ()(null *initialization-output*)))
          (if (probe-file (projects-definitions-file))
            (user-start-project-dialog :startup T)
            (catch-cancel
              (modal-dialog (new-remote-lisp-from-user))))
          (unless (windows :class *default-listener-class*)
            (make-new-listener))))
   ; startup-ccl runs queued functions after init has completed but before restoring streams...
   ; cannot use eval-enqueue though, as it immediately evaluates the form at this stage...
   (process-run-function "ensure listener" #'ensure-listener)))

#+clz ;; ## should be loaded last?
(define-plugin "Ensure Listener" ()
   (in-package :gui)
   "(when (zerop *cocoa-listener-count*)"
   "    (#/ensureListener: (#/delegate *NSApp*) nil))"  ; lisp-application-delegate 
   )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP TYPES

(defpackage autostart) ; just to avoid missing package in form

(define-plugin "Allegro" ()
 (in-package :mclide)
 (register-implementation-type :allegro
  '("\"$exec\" -l \"$loader\" -e \"$initform\"") 
  :init NIL
  :default "alisp"
  :homepage "http://www.clozure.com/clozurecl.html"
  :install
  (lambda (&key alias destination &allow-other-keys)
   (let ((exec "alisp"))
     (list 
      (format nil "path=~S" destination)
      "mkdir -p \"$path\""
      "cd \"$path\""
      (format nil "curl -Lsf ~S | tar xz -v --strip 1"
                   "ftp://ftp.franz.com/pub/acl82express/macosx86/acl82express-macosx-x86.bz2")
      (format nil "mkdir -p ~S" (autostart::alias-posix))
      (format nil "ln -sf \"$path/~A\" ~S" exec alias))))))

#+clz
(create-plugin-from-folder "home:PlugIns SRC;Start Allegro;" *plugins-disabled-folder*)

#-clz
(define-plugin "Clozure" ()
 (in-package :autostart)
 (register-implementation-type :clozure
  '("\"$exec\" -l \"$loader\" -e \"$initform\"") 
  :init NIL
  :default (case (cpu) ; see http://ccl.clozure.com/manual/chapter3.1.html
             (:x86-64 "dx86cl64")
             (:x86   "dx86cl")
             (:ppc-64 "dppccl64")
             (:ppc   "dppccl"))
  :help "Clozure needs the swank starting forms as a command argument. It cannot be piped to the application input."
  :homepage "http://www.clozure.com/clozurecl.html"
  :install
  (lambda (&key alias destination &allow-other-keys)
   (let ((exec (case (cpu) ; see http://ccl.clozure.com/manual/chapter3.1.html
                 (:x86-64 "dx86cl64")
                 (:x86   "dx86cl")
                 (:ppc-64 "dppccl64")
                 (:ppc   "dppccl"))))
     (list 
      (format nil "path=~S" destination)
      "mkdir -p \"$path\""
      "cd \"$path\""
      (format nil "curl -Lsf ~S | tar xz -v --strip 1"
              (ecase (cpu) ; see http://ccl.clozure.com/manual/chapter3.1.html
                ((:x86-64 :x86)
                 "ftp://ftp.clozure.com/pub/release/1.7/ccl-1.7-darwinx86.tar.gz")
                ((:ppc-64 :ppc)
                 "ftp://ftp.clozure.com/pub/release/1.7/ccl-1.7-darwinppc.tar.gz")))
      (format nil "mkdir -p ~S" (alias-posix))
      (format nil "ln -sf \"$path/~A\" ~S" exec alias))))))

#-clz
(define-plugin "SBCL" ()
 (in-package :autostart)
  (register-implementation-type :SBCL
   '("\"$exec\" --load \"$loader\" --eval \"$initform\"") 
   :init NIL
   :default "run-sbcl.sh"
   :help "Can start the sbcl binary or run-sbcl.sh (assuming the latter is set to be executable)"
   :homepage "http://www.sbcl.org/"
   :install
   (lambda (&key alias destination &allow-other-keys) 
     (let ((variation (case (cpu)
                        (:x86 "1.0.47-x86")
                        (:x86-64 "1.0.47-x86-64")
                        ((or :ppc :ppc-64) "1.0.47-powerpc"))))
       (list
        (format nil "path=~S" destination)
        (format nil "dist=http://downloads.sourceforge.net/project/sbcl/sbcl/1.0.47/sbcl-~A-darwin-binary.tar.bz2"  variation)
        "mkdir -p \"$path\""
        "cd \"$path\""
        (format nil "curl -Lsf \"$dist\" | tar xz -v --strip 1")
        (format nil "INSTALL_ROOT=\"$path\" sh install.sh")
        "chmod u+x run-sbcl.sh"
        ; work around install bug in SBCL where it fails to find core for pathnames with space:
        (format nil "ln -s \"$path\"/lib/sbcl/sbcl.core \"$path\"/contrib/sbcl.core")
        (format nil "mkdir -p ~S" (alias-posix))
        (format nil "ln -sf \"$path/run-sbcl.sh\" ~S" alias))))))

#-clz
(define-plugin "Clojure" ()
 (in-package :autostart)
  (register-implementation-type :Clojure
   '("(echo $init; echo; cat - ; ) | java -cp \"$exec\":\"$resources\"/clojure/swank/src clojure.main")
   :init (lambda (port-file _)
           (declare (ignore _))
           (format nil "(do (require 'swank.swank) (swank.swank/start-server :port-file ~S :encoding \"utf-8\" :dont-close true))"
                   (ccl::posix-namestring port-file))) 
   :default "clojure-1.3.0.jar"
   :homepage "http://clojure.org/"
   :install
   (lambda (&key alias destination &allow-other-keys) 
     (list 
      (format nil "path=~S" destination)
      "mkdir -p \"$path\""
      "cd \"$path\""
      "curl -Lsf http://github.com/downloads/clojure/clojure/clojure-1.3.0.zip | tar xz -v --strip 1"
      (format nil "mkdir -p ~S" (alias-posix))
      (format nil "ln -sf \"$path/clojure-1.3.0.jar\" ~S" alias)))))

#-clz
(define-plugin "CLISP" ()
 (in-package :autostart)
 ; ("\"$exec\" -i \"$loader\" -x \"$initform\" -repl") ; works, but *standard-output* isn't bound to slime stream
 (register-implementation-type :clisp
  '("tmp=$(mktemp -t 'mclide') ;"
         "echo \"(cl:load \\\"$loader\\\")\" > $tmp ;"
         "echo \"$initform\" >> $tmp ;"
         "lisp=(\"$exec\") ;"
         "\"$lisp\" -i \"$tmp\" ;")
  :init nil
  :homepage "http://clisp.cons.org/"
  :default "clisp"
  :install
  (lambda (&key alias destination &allow-other-keys)
    (list
     (format nil "path=~S" destination)
     "temp=$(mktemp -d '/tmp/clisp.XXXXXX')"        ; homebrew fail when pathnames contains spaces!
     "log=\"$path/clisp-install.log\""
     "mkdir -p \"$path\""
     "touch \"$log\""
     "cd \"$temp\""
     "curl -Lsf http://github.com/mxcl/homebrew/tarball/master | tar xz -v --strip 1"
     "open -aConsole \"$log\""
     "./bin/brew install -vd clisp 3>&2 2>&1 >> \"$log\""
     "cp -R \"$temp/\" \"$path\""  ;; to keep the symlinks (unlike mv)
     (format nil "mkdir -p ~S" (alias-posix))
     (format nil "ln -sf \"$path/bin/clisp\" ~S" alias)))))

#-clz
(define-plugin "MCL" ()
  (in-package :autostart)
  (register-implementation-type :mcl
   '("load=$(mktemp \"$exec.swank-loader.cfsl.XXXXXX\"); mv \"$init\" \"$load\"; open -a \"$exec\" \"$load\"; (sleep 60; mv \"$load\" \"$init\")& ")
   :init (lambda (port-file _)
           (declare (ignore _))
           (ccl::remote-mcl-init port-file (logfile :mcl)))
   :homepage "http://mcl.googlecode.com/"
   :install
   (lambda (&key alias destination &allow-other-keys)
     (let ((source (posix-namestring
                    (full-pathname "Resources:MCL;"))))
       (list 
        (format nil "path=~S" destination)
        "mkdir -p \"$path/../\""
        "rmdir \"$path\""
        (format nil "ln -sf ~S \"$path\"" source) ; consider copy instead, eliminating mclide executable! 
        (format nil "mkdir -p ~S" (alias-posix))
        (format nil "ln -sf \"$path/MCL\" ~S" alias))))))

#-clz
(define-plugin "LispWorks" ()
 (in-package :autostart)
  (register-implementation-type :LispWorks 
   '("tmp=$(mktemp -t 'mclide') ;"
    "echo \"(cl:load \\\"$loader\\\")\" > $tmp ;"
    "echo \"$initform\" >> $tmp ;"
    "lisp=(\"$exec\") ;"
    "\"$lisp\" -init \"$tmp\" ;")
   :init nil
   :help "Assumes that the selected executable is the LispWorks application bundle (not the Personal edition)"
   :homepage "http://www.lispworks.com/"))


;; #############################
;; SANITY CHECK

(defpackage :autostart)

(defun ensure-proper-fileformat (pathname)
  (dolist (file (directory pathname))
    (with-open-file (in file)
      (loop for c = (read-char in nil)
            while (and c (not (find c '(#\cr))))
            finally (when c (error "File ~A had an illegal character ~S" file c))))))

(ensure-proper-fileformat (merge-pathnames ";src;init-swank.lisp" *resources-path*))
(ensure-proper-fileformat (merge-pathnames ";src;start-swank.lisp" *resources-path*))

;;;;;
*short-version-string*
;;;;;
; END
