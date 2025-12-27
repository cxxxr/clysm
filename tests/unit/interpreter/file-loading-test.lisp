;;;; file-loading-test.lisp - TDD tests for interpreter file loading
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Task: T080

(defpackage #:clysm/tests/interpreter/file-loading-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:interpret-file
                #:load-compiler-modules
                #:make-interpreter-env
                #:read-file-forms
                #:filter-body-declarations))

(in-package #:clysm/tests/interpreter/file-loading-test)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defvar *test-temp-dir* (merge-pathnames "interpreter-test-tmp/"
                                          (uiop:temporary-directory)))

(defun ensure-test-dir ()
  "Ensure test temp directory exists."
  (ensure-directories-exist *test-temp-dir*))

(defun temp-file (name)
  "Create path for a temp test file."
  (merge-pathnames name *test-temp-dir*))

(defun write-test-file (name content)
  "Write CONTENT to test file NAME."
  (ensure-test-dir)
  (let ((path (temp-file name)))
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-string content s))
    path))

(defun cleanup-test-files ()
  "Remove test temp files."
  (when (probe-file *test-temp-dir*)
    (uiop:delete-directory-tree *test-temp-dir* :validate t)))

;;; ============================================================
;;; read-file-forms Tests
;;; ============================================================

(deftest test-read-file-forms-simple
  "Test reading simple forms from file."
  (let ((path (write-test-file "simple.lisp"
                               "(defun foo () 42)
                                (defun bar () 43)")))
    (unwind-protect
        (let ((forms (read-file-forms path)))
          (ok (= 2 (length forms)))
          (ok (eq 'defun (first (first forms))))
          (ok (eq 'defun (first (second forms)))))
      (cleanup-test-files))))

(deftest test-read-file-forms-with-comments
  "Test reading forms with comments."
  (let ((path (write-test-file "comments.lisp"
                               ";; Comment line
                                (defun foo () 42)
                                #| block comment |#
                                (+ 1 2)")))
    (unwind-protect
        (let ((forms (read-file-forms path)))
          (ok (= 2 (length forms))))
      (cleanup-test-files))))

(deftest test-read-file-forms-empty
  "Test reading empty file."
  (let ((path (write-test-file "empty.lisp" "")))
    (unwind-protect
        (let ((forms (read-file-forms path)))
          (ok (null forms)))
      (cleanup-test-files))))

;;; ============================================================
;;; filter-body-declarations Tests
;;; ============================================================

(deftest test-filter-body-declarations
  "Test filtering declare forms from body."
  (multiple-value-bind (filtered decls)
      (filter-body-declarations '((declare (type integer x))
                                  (+ x 1)
                                  (declare (special y))
                                  (* y 2)))
    (ok (= 2 (length filtered)))
    (ok (= 2 (length decls)))
    (ok (equal '(+ x 1) (first filtered)))))

(deftest test-filter-body-declarations-none
  "Test filtering when no declarations present."
  (multiple-value-bind (filtered decls)
      (filter-body-declarations '((+ 1 2) (- 3 4)))
    (ok (= 2 (length filtered)))
    (ok (null decls))))

;;; ============================================================
;;; interpret-file Tests
;;; ============================================================

(deftest test-interpret-file-simple
  "Test interpreting simple file."
  (let ((path (write-test-file "interp.lisp"
                               "(defun test-fn () 42)")))
    (unwind-protect
        (let ((env (make-interpreter-env)))
          (interpret-file path :env env)
          ;; Function should be defined
          (ok (= 42 (interpret '(test-fn) env))))
      (cleanup-test-files))))

(deftest test-interpret-file-multiple-forms
  "Test interpreting file with multiple forms."
  (let ((path (write-test-file "multi.lisp"
                               "(defvar *counter* 0)
                                (defun inc-counter () (setf *counter* (1+ *counter*)))
                                (inc-counter)
                                (inc-counter)
                                *counter*")))
    (unwind-protect
        (let ((result (interpret-file path)))
          (ok (= 2 result)))
      (cleanup-test-files))))

(deftest test-interpret-file-in-package
  "Test in-package handling during file loading."
  (let ((path (write-test-file "pkg.lisp"
                               "(in-package #:cl-user)
                                (defun pkg-test-fn () :from-cl-user)")))
    (unwind-protect
        (let ((env (make-interpreter-env)))
          (interpret-file path :env env)
          ;; Should complete without error
          (ok t))
      (cleanup-test-files))))

(deftest test-interpret-file-eval-when
  "Test eval-when handling during file loading."
  (let ((path (write-test-file "evalwhen.lisp"
                               "(eval-when (:compile-toplevel :load-toplevel :execute)
                                  (defvar *eval-when-test* :executed))
                                *eval-when-test*")))
    (unwind-protect
        (let ((result (interpret-file path)))
          (ok (eq :executed result)))
      (cleanup-test-files))))

(deftest test-interpret-file-skip-declarations
  "Test that declarations are skipped."
  (let ((path (write-test-file "decl.lisp"
                               "(declaim (optimize (speed 3)))
                                (proclaim '(special *some-var*))
                                42")))
    (unwind-protect
        (let ((result (interpret-file path)))
          (ok (= 42 result)))
      (cleanup-test-files))))

;;; ============================================================
;;; load-compiler-modules Tests
;;; ============================================================

(deftest test-load-compiler-modules-count
  "Test that load-compiler-modules counts loaded modules."
  (let ((path1 (write-test-file "mod1.lisp" "(defun mod1-fn () 1)"))
        (path2 (write-test-file "mod2.lisp" "(defun mod2-fn () 2)")))
    (unwind-protect
        (let* ((env (make-interpreter-env))
               (count (load-compiler-modules (list path1 path2) :env env)))
          (ok (= 2 count)))
      (cleanup-test-files))))

(deftest test-load-compiler-modules-continues-on-error
  "Test that loading continues when one module fails."
  (let ((path1 (write-test-file "good.lisp" "(defun good-fn () :good)"))
        (path2 "/nonexistent/path.lisp")
        (path3 (write-test-file "also-good.lisp" "(defun also-good () :also)")))
    (unwind-protect
        (let* ((env (make-interpreter-env))
               (count (load-compiler-modules (list path1 path2 path3) :env env)))
          ;; Should load 2 of 3 (skip nonexistent)
          (ok (= 2 count)))
      (cleanup-test-files))))
