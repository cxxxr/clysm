;;;; selfhost.lisp — entry point of the compiler when it is itself compiled
;;;; to WebAssembly by Clysm.
;;;;   node host/run.mjs build/clysmc.wasm [--shake] OUT.wasm FILE... [--runtime DIR]

(in-package :clysm)

(defun selfhost-runtime-files (dir)
  (with-open-file (s (concatenate 'string dir "/../tools/runtime-files.lisp"))
    (mapcar (lambda (f) (concatenate 'string dir "/" f ".lisp")) (read s))))

(defun selfhost-main ()
  (let* ((args (sys::%command-line-arguments))
         (runtime-dir "runtime")
         (shake nil)
         (files nil))
    (loop while args
          do (let ((a (pop args)))
               (cond ((string= a "--runtime") (setf runtime-dir (pop args)))
                     ((string= a "--shake") (setf shake t))
                     (t (push a files)))))
    (setf files (nreverse files))
    (when (< (length files) 1)
      (format *error-output* "usage: clysmc [--shake] OUT.wasm FILE... [--runtime DIR]~%")
      (sys::%exit 2))
    (let ((out (first files)) (sources (rest files))
          (*shake* shake)
          (*source-root* (ignore-errors (truename (concatenate 'string runtime-dir "/../")))))
      ;; macro expanders run by the image's evaluator receive the compiler's
      ;; environment objects (e.g. GET-SETF-EXPANSION inside a SETF expander)
      (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
      (reset-compiler)
      (let ((*read-package* (find-target-package "SYSTEM")))
        (dolist (f (selfhost-runtime-files runtime-dir)) (compile-source-file f))
        ;; the version of the package (VERSION, written by tools/build-npm.sh)
        (let ((version (ignore-errors
                        (with-open-file (in (concatenate 'string runtime-dir "/../VERSION"))
                          (read-line in)))))
          (when version
            (process-toplevel `(setq clysm-sys::*clysm-version* ,version) (make-cenv))))
        (finish-runtime))
      (let ((*host-load-defuns* t))
        (dolist (f sources) (compile-source-file f)))
      (let ((bytes (build-module)))
        (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence bytes o))
        (format *error-output* "~&; wrote ~A (~D bytes)~%" out (length bytes))))))

(selfhost-main)
