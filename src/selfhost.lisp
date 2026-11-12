;;;; selfhost.lisp — entry point of the compiler when it is itself compiled
;;;; to WebAssembly by Clysm.
;;;;   node host/run.mjs build/clysmc.wasm OUT.wasm FILE... [--runtime DIR]

(in-package :clysm)

(defun selfhost-runtime-files (dir)
  (with-open-file (s (concatenate 'string dir "/../tools/runtime-files.lisp"))
    (mapcar (lambda (f) (concatenate 'string dir "/" f ".lisp")) (read s))))

(defun selfhost-main ()
  (let* ((args (sys::%command-line-arguments))
         (runtime-dir "runtime")
         (files nil))
    (loop while args
          do (let ((a (pop args)))
               (if (string= a "--runtime")
                   (setf runtime-dir (pop args))
                   (push a files))))
    (setf files (nreverse files))
    (when (< (length files) 1)
      (format *error-output* "usage: clysmc OUT.wasm FILE... [--runtime DIR]~%")
      (sys::%exit 2))
    (let ((out (first files)) (sources (rest files)))
      (reset-compiler)
      (let ((*read-package* (find-target-package "SYSTEM")))
        (dolist (f (selfhost-runtime-files runtime-dir)) (compile-source-file f))
        (finish-runtime))
      (dolist (f sources) (compile-source-file f))
      (let ((bytes (build-module)))
        (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence bytes o))
        (format *error-output* "~&; wrote ~A (~D bytes)~%" out (length bytes))))))

(selfhost-main)
