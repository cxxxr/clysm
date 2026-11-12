;;;; tools/build.lisp — command line driver
;;;; usage: sbcl --script tools/build.lisp OUT.wasm FILE...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(defparameter *runtime-files*
  (mapcar (lambda (f) (merge-pathnames (concatenate 'string "../runtime/" f ".lisp") *load-truename*))
          (with-open-file (s (merge-pathnames "runtime-files.lisp" *load-truename*)) (read s))))
(let* ((args (cdr sb-ext:*posix-argv*))
       (out (first args))
       (files (rest args)))
  (handler-case
      (progn
        (reset-compiler)
        (let ((*read-package* (find-target-package "SYSTEM")))
          (dolist (f *runtime-files*) (compile-source-file f))
          (finish-runtime))
        (dolist (f files) (compile-source-file f))
        (let ((bytes (build-module)))
          (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (write-sequence bytes o))
          (format *error-output* "~&; wrote ~A (~D bytes)~%" out (length bytes))))
    (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1))))
