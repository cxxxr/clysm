;;;; fs-read-test.lisp - Unit tests for Stage 0 file reading
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US5: FFI Filesystem Access - File reading

(defpackage #:clysm/tests/unit/stage0/fs-read-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:read-source-file
                #:read-source-string))

(in-package #:clysm/tests/unit/stage0/fs-read-test)

;;; ============================================================
;;; T015: Unit test for file reading
;;; ============================================================

(deftest test-read-source-string-parses-simple-expr
  "Verify read-source-string can parse simple expressions"
  (let ((forms (read-source-string "(+ 1 2)")))
    (ok (listp forms) "Should return a list")
    (ok (= 1 (length forms)) "Should have one form")
    (ok (equal '(+ 1 2) (first forms)) "Should parse correctly")))

(deftest test-read-source-string-parses-multiple-forms
  "Verify read-source-string handles multiple forms"
  (let ((forms (read-source-string "(defun foo () 1) (defun bar () 2)")))
    (ok (= 2 (length forms)) "Should have two forms")))

(deftest test-read-source-string-handles-comments
  "Verify read-source-string skips comments"
  (let ((forms (read-source-string ";; comment
(+ 1 2)")))
    (ok (= 1 (length forms)) "Should have one form after comment")))

(deftest test-read-source-string-handles-empty-input
  "Verify read-source-string handles empty input"
  (let ((forms (read-source-string "")))
    (ok (null forms) "Empty input should return NIL")))

(deftest test-read-source-string-handles-whitespace
  "Verify read-source-string handles whitespace-only input"
  (let ((forms (read-source-string "

  ")))
    (ok (null forms) "Whitespace-only should return NIL")))

;;; ============================================================
;;; Source File Reading Tests (placeholder - requires FFI)
;;; ============================================================

(deftest test-read-source-file-signature
  "Verify read-source-file function exists"
  (skip "Requires FFI host shim")
  (ok (fboundp 'read-source-file) "read-source-file should be defined"))
