;;;; selfhost.lisp — entry point of the compiler when it is itself compiled
;;;; to WebAssembly by Clysm: the options of tools/build.lisp, and --runtime
;;;;   node host/run.mjs build/clysmc.wasm [--shake] [--shake-symbols] [--shake-why REPORT]
;;;;     [--eval-string] [--thrown-exits] [--preinit-through FILE] OUT.wasm FILE... [--runtime DIR]
;;;;   node host/run.mjs build/clysmc.wasm --repl-image OUT.wasm [MAIN.lisp...] [--runtime DIR]
;;;;     (the REPL image, as tools/build-repl.lisp builds it)

(in-package :clysm)

(defun selfhost-main ()
  (let ((args (sys::%command-line-arguments))
        (runtime-dir "runtime")
        (shake nil) (symbols nil) (why nil) (eval-string nil) (preinit-through nil) (repl-image nil)
        (files nil))
    (flet ((usage ()
             (format *error-output* "usage: clysmc [--shake] [--shake-symbols] [--shake-why REPORT] [--eval-string] [--thrown-exits] [--preinit-through FILE] OUT.wasm FILE... [--runtime DIR]~%       clysmc --repl-image OUT.wasm [MAIN...] [--runtime DIR]~%")
             (sys::%exit 2)))
      (loop while args
            do (let ((a (pop args)))
                 (cond ((string= a "--runtime") (setf runtime-dir (or (pop args) (usage))))
                       ((string= a "--shake") (setf shake t))
                       ((string= a "--repl-image") (setf repl-image t))
                       ((string= a "--shake-symbols") (setf symbols t))
                       ((string= a "--shake-why") (setf why (or (pop args) (usage))))
                       ((string= a "--eval-string") (setf eval-string t))
                       ((string= a "--thrown-exits") (setf *thrown-exits* t))
                       ((string= a "--preinit-through")
                        (setf preinit-through (pop args))
                        (unless (and preinit-through (not (eql 0 (search "--" preinit-through))))
                          (format *error-output* "--preinit-through needs a source file~%")
                          (sys::%exit 2)))
                       (t (push a files)))))
      (setf files (nreverse files))
      (when (< (length files) 1) (usage)))
    ;; relative names are of the working directory, as on SBCL (and so are
    ;; the names of the sources in stack traces: SOURCE-FILE-NAME)
    (sys::%init-default-pathname)
    ;; macro expanders run by the image's evaluator receive the compiler's
    ;; environment objects (e.g. GET-SETF-EXPANSION inside a SETF expander)
    (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
    (setq sys::*foreign-env-macro-function-hook* #'macro-function-in)
    ;; a shaken module starts from the heap of its runtime made here, as on
    ;; SBCL (src/preinit.lisp, which runs a module on Node.js for it)
    (unless (let ((v (sys::%getenv "CLYSM_NO_PREINIT"))) (and v (plusp (length v))))
      (setf *preinit-function* 'preinit-shake))
    ;; the driver of the SBCL-hosted compiler (tools/build.lisp), so that
    ;; both compile a program alike
    (let ((out (first files))
          (runtime-dir (concatenate 'string (string-right-trim "/" runtime-dir) "/")))
      (handler-case
          (let ((n (if repl-image
                       (build-repl-image out (or (rest files) '("lib/repl-main.lisp"))
                                         (concatenate 'string runtime-dir "../")
                                         :redefinable (let ((v (sys::%getenv "CLYSM_REDEFINABLE")))
                                                        (and v (plusp (length v)))))
                       (compile-program (rest files) :output out :shake (or shake symbols (and why t))
                                    :shake-symbols symbols :shake-why why :eval-string eval-string
                                    :preinit-through preinit-through
                                    :runtime-dir runtime-dir))))
            (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
        (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sys::%exit 1))))))

(selfhost-main)
