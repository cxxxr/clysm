;;;; tools/build.lisp — command line driver of the SBCL-hosted compiler
;;;; usage: sbcl --script tools/build.lisp [--shake] [--shake-symbols] [--shake-why REPORT] OUT.wasm FILE...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(let* ((args (cdr sb-ext:*posix-argv*)) (shake nil) (symbols nil) (why nil))
  (loop (cond ((equal (first args) "--shake") (pop args) (setf shake t))
              ((equal (first args) "--shake-symbols") (pop args) (setf symbols t))
              ((equal (first args) "--shake-why") (pop args) (setf why (pop args)))
              (t (return))))
  (let ((out (first args))
        (files (rest args)))
    (handler-case
        (let ((n (compile-program files :output out :shake (or shake symbols (and why t)) :shake-symbols symbols
                                        :shake-why why
                                        :runtime-dir (merge-pathnames "../runtime/" *load-truename*))))
          (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
      (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1)))))
