(in-package :ccl)

#+clozure
(defmacro defcontextual (name (&rest args) context &body body &environment env)
  "Intercept  a function or method with custom functionality when the context evaluates to true. The provided call-next function calls the original function, even  if enqueued or forked."
  ; allows multiple contextual variations of the same function or method by allowing name to be a list with second item the name of the variation.
  ; # improve to support declarations for the arguments
  ; # should warn if it replaces an existing contextual
 (let* ((label (if (listp name) (first name) name))
        (variation (if (listp name) (second name) (intern (concatenate 'string "contextual_" (string name)) :keyword)))
        (method? (method-exists-p label))
        (spec (if method?
               `(:method ,label ,(mapcar (lambda (arg)(if (listp arg) (second arg) t)) args))
               label))
        (argvars (mapcar (lambda (arg)(if (listp arg) (first arg) arg)) args)))
  (multiple-value-bind (forms decls doc) (parse-body body env t)
    (declare (ignore doc))
    `(eval-when (:load-toplevel :execute :compile-toplevel)
       (when (advisedp ,spec :when :around :name ,variation)
         (warn "Replacing existing contextual function for ~A." 
               ',name))
       (advise ,spec
                (destructuring-bind (,@argvars) ccl::arglist
                   ,@decls
                   (if ,context
                     (let ((arglist (copy-list arglist))) ; arglist is dynamic extent in do-apropos
                       (declare (ignore-if-unused arglist))
                       (flet ((call-next ()
                                  (:do-it)))
                        (block ,label ,@forms)))
                     (:do-it)))
               :when :around :name ,variation)))))

#+mcl
(defmacro defcontextual (name (&rest args) context &body body &environment env)
  "Intercept  a function or method with custom functionality when the context evaluates to true. The provided call-next function calls the original function, even  if enqueued or forked."
  ; # improve to support declarations for the arguments
  (multiple-value-bind (forms decls doc) (parse-body body env t)
    (declare (ignore doc))
      (if (method-exists-p name)
        `(defmethod ,name :around (,@args)
           ,@decls
           (if ,context
             (flet ((call-next ()
                      (call-next-method)))
               ,@forms)
             ;(when (call-next-method-p ,(function name)) ;; can have a confun with no default! 
             (call-next-method)))
        `(advise ,name
                (destructuring-bind (,@args) ccl::arglist
                   ,@decls
                   (if ,context
                     (let ((arglist (copy-list arglist))) ; arglist is dynamic extent in do-apropos
                       (declare (ignore-if-unused arglist))
                       (flet ((call-next ()
                                  (:do-it)))
                        (block ,name ,@forms)))
                     (:do-it)))
               :when :around :name ,(intern (concatenate 'string "contextual_" (string name)) :keyword)))))

(defmacro with-unchanged-let* ((name &rest bindings) &body body &aux (capture (gensym "capture")) (test (gensym "test")))
   "Capture the value of evaluating the forms, providing a function with the name that results T if evaluating the same forms returns values that outputs the same. Can be used to verify that a dialog hasn't changed in the time between a query goes to the swank server until response is back to be presented in a dialog, to avoid that the presented result is out of sync with the dialog."
   `(flet ((,test (,capture) ; before to avoid conflicting bindings
            (ignore-errors ; some of the capture functions may fail the second time eg if dialog is closed 
             (let* (,@bindings)
               (let ((capture (write-to-string (list ,@(mapcar #'car bindings)))))
                 (values (equal ,capture capture) ,capture capture))))))
      (let* (,@bindings)
        (let ((,capture (write-to-string (list ,@(mapcar #'car bindings)))))
          (flet ((,name ()
                  (,test ,capture)))
            ,@body)))))
