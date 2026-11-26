;;;; tools/build-repl.lisp — build the REPL image with the compiler embedded
;;;; (COMPILE and the REPL then generate WebAssembly at run time).
;;;; usage: sbcl --script tools/build-repl.lisp OUT.wasm [MAIN.lisp...]
;;;; The MAIN files (by default lib/repl-main.lisp) are compiled into the image
;;;; after the compiler.  With the environment variable CLYSM_REDEFINABLE set,
;;;; calls of their functions are not bound to the definitions in the files.
;;;; The self-hosted compiler builds the same image (BUILD-REPL-IMAGE):
;;;;   node host/run.mjs build/clysmc.wasm --repl-image OUT.wasm [MAIN.lisp...]
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(let ((out (second sb-ext:*posix-argv*))
      (mains (or (cddr sb-ext:*posix-argv*) '("lib/repl-main.lisp"))))
  (handler-case
      (let ((n (build-repl-image out mains (merge-pathnames "../" *load-truename*)
                                 :redefinable (and (sb-ext:posix-getenv "CLYSM_REDEFINABLE") t))))
        (format *error-output* "~&; wrote ~A (~D bytes)~%" out n))
    (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1))))
