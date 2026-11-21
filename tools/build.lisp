;;;; tools/build.lisp — command line driver of the SBCL-hosted compiler
;;;; usage: sbcl --script tools/build.lisp [--shake] [--shake-why REPORT] OUT.wasm FILE...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(let* ((args (cdr sb-ext:*posix-argv*))
       (shake (when (equal (first args) "--shake") (pop args) t))
       (why (when (equal (first args) "--shake-why") (pop args) (pop args)))
       (out (first args))
       (files (rest args)))
  (handler-case
      (let ((n (compile-program files :output out :shake (or shake (and why t)) :shake-why why
                                      :runtime-dir (merge-pathnames "../runtime/" *load-truename*))))
        (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
    (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1))))
