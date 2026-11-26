;;;; selfhost.lisp — entry point of the compiler when it is itself compiled
;;;; to WebAssembly by Clysm.
;;;;   node host/run.mjs build/clysmc.wasm [--shake] OUT.wasm FILE... [--runtime DIR]

(in-package :clysm)

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
    ;; macro expanders run by the image's evaluator receive the compiler's
    ;; environment objects (e.g. GET-SETF-EXPANSION inside a SETF expander)
    (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
    (setq sys::*foreign-env-macro-function-hook* #'macro-function-in)
    ;; the driver of the SBCL-hosted compiler (tools/build.lisp), so that
    ;; both compile a program alike.  There is no heap of src/preinit.lisp
    ;; here (*PREINIT-FUNCTION* is NIL): a shaken module runs the runtime's
    ;; toplevel forms when it starts.
    (let* ((out (first files))
           (n (compile-program (rest files) :output out :shake shake
                               :runtime-dir (concatenate 'string runtime-dir "/"))))
      (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))))

(selfhost-main)
