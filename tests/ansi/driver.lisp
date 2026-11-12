;;;; tests/ansi/driver.lisp — run part of the ANSI test suite (ansi-test) in Clysm.
;;;;   clysm script tests/ansi/driver.lisp CATEGORY [START]
;;;; Loads the rt framework and CATEGORY/load.lsp, then runs the tests from
;;;; index START, printing one line per test:  "@@ <index> <PASS|FAIL> <name>".
;;;; A crash (Wasm trap) kills the process; the outer script then resumes.

(in-package :cl-user)

(defvar *ansi-root* (merge-pathnames "third_party/ansi-test/"))
(defvar *aux-dir* (merge-pathnames "auxiliary/" *ansi-root*))

;;; compile-and-load.lsp needs file dates and fasls; Clysm just loads sources
(defvar *compiled-and-loaded-files* nil)
(defun compile-and-load (pathspec &key force)
  (let ((p (merge-pathnames pathspec (or *load-pathname* *default-pathname-defaults*))))
    (unless (and (not force) (member (namestring p) *compiled-and-loaded-files* :test #'string=))
      (push (namestring p) *compiled-and-loaded-files*)
      (load p))))
(defun compile-and-load* (pathspec &key force)
  (compile-and-load (merge-pathnames pathspec *aux-dir*) :force force))

(defvar *args* (cdr sys::*posix-argv*))
(defvar *category* (first *args*))
(defvar *start* (if (second *args*) (parse-integer (second *args*)) 0))

(let ((*default-pathname-defaults* *ansi-root*))
  (load "rt-package.lsp")
  (load "rt.lsp")
  (load "cl-test-package.lsp")
  (in-package :cl-test)
  (cl-user::compile-and-load* "ansi-aux-macros.lsp")
  (load "universe.lsp")
  (cl-user::compile-and-load* "random-aux.lsp")
  (cl-user::compile-and-load* "ansi-aux.lsp")
  (load "cl-symbol-names.lsp")
  (load "notes.lsp"))

(in-package :cl-test)

(let ((*default-pathname-defaults* cl-user::*ansi-root*))
  (load (concatenate 'string cl-user::*category* "/load.lsp")))

(format t "~&@@TOTAL ~D~%" (length (cdr rt::*entries*)))
(let ((i 0))
  (dolist (entry (cdr rt::*entries*))
    (when (>= i cl-user::*start*)
      (format t "~&@@START ~D ~S~%" i (rt::name entry))
      (finish-output)
      (let ((ok (let ((*default-pathname-defaults* (merge-pathnames "sandbox/" cl-user::*ansi-root*)))
                  (rt::do-entry entry *standard-output*))))
        (format t "~&@@ ~D ~A ~S~%" i (if ok "PASS" "FAIL") (rt::name entry))
        (finish-output)))
    (incf i)))
(format t "~&@@DONE~%")
