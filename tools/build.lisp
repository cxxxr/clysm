;;;; tools/build.lisp — command line driver of the SBCL-hosted compiler
;;;; usage: sbcl --script tools/build.lisp [--shake] [--shake-symbols] [--shake-why REPORT] [--eval-string] [--thrown-exits] OUT.wasm FILE...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
;; (CLYSM_SHAKE_WHY_LARGEST=N: --shake-why lists the N nodes that dominate the most)
(let ((n (sb-ext:posix-getenv "CLYSM_SHAKE_WHY_LARGEST")))
  (when n (setf clysm-wasm::*shake-why-largest* (parse-integer n))))
(let* ((args (cdr sb-ext:*posix-argv*)) (shake nil) (symbols nil) (why nil) (eval-string nil) (preinit-through nil))
  (loop (cond ((equal (first args) "--shake") (pop args) (setf shake t))
              ((equal (first args) "--shake-symbols") (pop args) (setf symbols t))
              ((equal (first args) "--shake-why") (pop args) (setf why (pop args)))
              ((equal (first args) "--eval-string") (pop args) (setf eval-string t))
              ;; every non-local exit a Wasm exception (*THROWN-EXITS*)
              ((equal (first args) "--thrown-exits") (pop args) (setf *thrown-exits* t))
              ((equal (first args) "--preinit-through")
               (pop args)
               (unless (and args (not (eql 0 (search "--" (first args)))))
                 (error "--preinit-through needs a source file"))
               (setf preinit-through (pop args)))
              (t (return))))
  (let ((out (first args))
        (files (rest args)))
    (handler-case
        (let ((n (compile-program files :output out :shake (or shake symbols (and why t)) :shake-symbols symbols
                                        :shake-why why :eval-string eval-string :preinit-through preinit-through
                                        :runtime-dir (merge-pathnames "../runtime/" *load-truename*))))
          (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
      (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1)))))
