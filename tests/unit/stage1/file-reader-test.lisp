;;;; file-reader-test.lisp - Unit tests for Stage 1 file reading
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for source file reading and module enumeration

(in-package #:clysm/tests/unit/stage1-reader)

;;; ==========================================================================
;;; Module Path Tests
;;; ==========================================================================

(deftest test-get-module-paths-returns-list
  "get-module-paths should return a non-empty list."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (listp paths) "returns a list")
    (ok (> (length paths) 0) "list is non-empty")))

(deftest test-get-module-paths-count
  "get-module-paths should return at least 40 modules."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (>= (length paths) 40)
        (format nil "expected >= 40 modules, got ~D" (length paths)))))

(deftest test-get-module-paths-are-strings
  "get-module-paths should return strings."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (every #'stringp paths)
        "all paths are strings")))

(deftest test-get-module-paths-lisp-extension
  "get-module-paths should all end with .lisp."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (every (lambda (p) (search ".lisp" p)) paths)
        "all paths end with .lisp")))

;;; ==========================================================================
;;; Source Form Reading Tests
;;; ==========================================================================

(deftest test-read-source-forms-from-string
  "read-source-forms should parse S-expressions from string."
  (let ((forms (clysm/stage1:read-source-forms "(+ 1 2) (defun foo () 42)")))
    (ok (listp forms) "returns a list")
    (ok (= (length forms) 2) "parsed 2 forms")
    (ok (every #'clysm/stage1::source-form-p forms)
        "all forms are source-form structs")))

(deftest test-read-source-forms-empty-string
  "read-source-forms should return empty list for empty string."
  (let ((forms (clysm/stage1:read-source-forms "")))
    (ok (null forms) "returns empty list for empty input")))

(deftest test-read-source-forms-comment-only
  "read-source-forms should skip comments."
  (let ((forms (clysm/stage1:read-source-forms "; comment\n(+ 1 2)")))
    (ok (= (length forms) 1) "comments are skipped")))

(deftest test-source-form-has-id
  "source-form should have unique ID."
  (let* ((forms (clysm/stage1:read-source-forms "(a) (b) (c)"))
         (ids (mapcar #'clysm/stage1::source-form-id forms)))
    (ok (every #'stringp ids) "all IDs are strings")
    (ok (= (length (remove-duplicates ids :test #'string=)) 3)
        "IDs are unique")))

(deftest test-source-form-has-operator
  "source-form should extract top-level operator."
  (let ((forms (clysm/stage1:read-source-forms "(defun foo () 42)")))
    (ok (eq (clysm/stage1::source-form-operator (first forms)) 'defun)
        "operator is DEFUN")))

;;; ==========================================================================
;;; Compilable Form Predicate Tests
;;; ==========================================================================

(deftest test-compilable-form-p-defun
  "compilable-form-p should return T for defun."
  (ok (clysm/stage1::compilable-form-p '(defun foo () 42))
      "defun is compilable"))

(deftest test-compilable-form-p-lambda
  "compilable-form-p should return T for lambda."
  (ok (clysm/stage1::compilable-form-p '(lambda (x) x))
      "lambda is compilable"))

(deftest test-compilable-form-p-arithmetic
  "compilable-form-p should return T for arithmetic."
  (ok (clysm/stage1::compilable-form-p '(+ 1 2))
      "arithmetic is compilable"))

(deftest test-compilable-form-p-in-package
  "compilable-form-p should return NIL for in-package."
  (ok (null (clysm/stage1::compilable-form-p '(in-package :foo)))
      "in-package is not compilable"))

(deftest test-compilable-form-p-declare
  "compilable-form-p should return NIL for declare."
  (ok (null (clysm/stage1::compilable-form-p '(declare (optimize speed))))
      "declare is not compilable"))

;;; ==========================================================================
;;; File Reading Tests (with mock)
;;; ==========================================================================

(deftest test-read-forms-from-file-not-found
  "read-forms-from-file should signal on missing file."
  (ok (signals 'clysm/stage1:stage1-file-not-found
               (clysm/stage1::read-forms-from-file "/nonexistent/file.lisp"))
      "signals file-not-found for missing file"))

