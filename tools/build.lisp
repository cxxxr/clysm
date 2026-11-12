;;;; tools/build.lisp — command line driver of the SBCL-hosted compiler
;;;; usage: sbcl --script tools/build.lisp OUT.wasm FILE...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(let* ((args (cdr sb-ext:*posix-argv*))
       (out (first args))
       (files (rest args)))
  (handler-case
      (let ((n (compile-program files :output out
                                      :runtime-dir (merge-pathnames "../runtime/" *load-truename*))))
        (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
    (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1))))
