;;;; tests/lem/load-lem.lisp — load Lem's core (lem/core, lem-lisp-syntax),
;;;; its Lisp grammar and fake display, and tests/lem/buffer-bench.lisp, in
;;;; SBCL or in Clysm, the same way: with ASDF and Quicklisp (already
;;;; loaded), from the checkout of cxxxr/lem named by the environment
;;;; variable LEM_DIR, with the stubs of its frontends/clysm/ (log4cl,
;;;; dexador, deploy, tree-sitter, subprocesses) in both, so that both run
;;;; the same code.  micros and lem-mailbox are those that frontends/clysm/
;;;; build.sh put in LEM_DIR/build/clysm/deps/.  Used by tools/bench-lem.sh.

(in-package :cl-user)

(defvar *lem-dir* (uiop:ensure-directory-pathname (uiop:getenv "LEM_DIR")))
(defvar *bench-dir* (uiop:pathname-directory-pathname *load-truename*))

(flet ((dir (path) (merge-pathnames path *lem-dir*)))
  (push *lem-dir* asdf:*central-registry*)
  (push (dir "extensions/lisp-syntax/") asdf:*central-registry*)
  (push (dir "build/clysm/deps/micros/") asdf:*central-registry*)
  (push (dir "build/clysm/deps/lem-mailbox/") asdf:*central-registry*))

(defun lem-bench-stub-search (name)
  (when (member name '("log4cl" "dexador" "deploy" "lem-tree-sitter" "lem-process") :test #'string=)
    (probe-file (merge-pathnames (format nil "frontends/clysm/stubs/~A/~A.system" name name) *lem-dir*))))
(pushnew 'lem-bench-stub-search asdf:*system-definition-search-functions*)

(let ((*compile-verbose* nil) (*load-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    ;; (LEM_BENCH_DEFINE_ONLY: the systems are only defined, for
    ;; `clysm build`, which loads them itself: tools/bench-lem.sh, MODE=static)
    (unless (uiop:getenv "LEM_BENCH_DEFINE_ONLY")
      (funcall (intern "QUICKLOAD" "QL") '("lem/core" "lem-lisp-syntax") :silent t))
    ;; the Lisp grammar (without the rest of the Lisp mode), the fake display
    ;; of Lem's tests, and the benchmark: compiled to fasls like the rest
    (eval `(asdf:defsystem "lem-bench-support"
             :depends-on ("lem/core" "lem-lisp-syntax")
             :pathname ,*lem-dir*
             :serial t
             :components ((:file "extensions/lisp-mode/grammar")
                          (:file "frontends/fake-interface/fake-interface"))))
    (eval `(asdf:defsystem "lem-buffer-bench"
             :depends-on ("lem-bench-support")
             :pathname ,*bench-dir*
             :components ((:file "buffer-bench"))))
    (unless (uiop:getenv "LEM_BENCH_DEFINE_ONLY")
      (asdf:load-system "lem-buffer-bench"))))
