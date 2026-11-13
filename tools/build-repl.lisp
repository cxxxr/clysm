;;;; tools/build-repl.lisp — build the REPL image with the compiler embedded
;;;; (COMPILE and the REPL then generate WebAssembly at run time).
;;;; usage: sbcl --script tools/build-repl.lisp OUT.wasm [MAIN.lisp]
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(defparameter *root* (merge-pathnames "../" *load-truename*))
(defun rpath (f) (namestring (merge-pathnames f *root*)))
(let ((out (second sb-ext:*posix-argv*))
      (main (or (third sb-ext:*posix-argv*) "lib/repl-main.lisp")))
  (handler-case
      (progn
        (reset-compiler)
        (let ((*read-package* (find-target-package "SYSTEM")))
          (dolist (f (with-open-file (s (rpath "tools/runtime-files.lisp")) (read s)))
            (compile-source-file (rpath (format nil "runtime/~A.lisp" f))))
          (finish-runtime))
        ;; the compiler's own CLYSM-SYS package is the image's SYSTEM package
        (setf (gethash "CLYSM-SYS" *target-packages*) (find-package :clysm-sys))
        (push "CLYSM-SYS" (gethash "SYSTEM" *target-nicknames*))
        (dolist (f '("src/package" "src/wasm" "src/reader" "src/layout" "src/frontend" "src/types" "src/inline" "src/codegen"
                     "src/prims" "src/typed" "src/driver" "src/hostenv" "src/live" "lib/live-image"))
          (compile-source-file (rpath (concatenate 'string f ".lisp"))))
        (compile-source-file (if (probe-file main) main (rpath main)))
        (let ((bytes (build-module)))
          (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (write-sequence bytes o))
          (format *error-output* "~&; wrote ~A (~D bytes)~%" out (length bytes))))
    (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e) (sb-ext:exit :code 1))))
