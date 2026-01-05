;;;; tests/e2e/error-test.lisp - E2E Tests for Error Handling
;;;;
;;;; Tests that errors are correctly detected at compile-time and run-time.

(in-package #:clysm/tests)

(defsuite e2e-error-suite "E2E tests for error handling")

;;; ============================================================
;;; Compile-Time Errors: Undefined Variables
;;; ============================================================

(deftest test-e2e-error-undefined-var ()
  "Undefined variable signals compile error"
  (e2e-signals-compile-error "undefined-variable-xyz"))

(deftest test-e2e-error-undefined-in-expr ()
  "Undefined variable in expression"
  (e2e-signals-compile-error "(+ 1 unknown-var)"))

;;; ============================================================
;;; Compile-Time Errors: IF Form
;;; ============================================================

(deftest test-e2e-error-if-no-args ()
  "IF with no arguments"
  (e2e-signals-compile-error "(if)"))

(deftest test-e2e-error-if-one-arg ()
  "IF with only test (no then)"
  (e2e-signals-compile-error "(if t)"))

;;; ============================================================
;;; Compile-Time Errors: LET Form
;;; ============================================================

(deftest test-e2e-error-let-bad-bindings ()
  "LET with non-list bindings"
  (e2e-signals-compile-error "(let 42 x)"))

(deftest test-e2e-error-let-bad-binding-form ()
  "LET with malformed binding"
  (e2e-signals-compile-error "(let ((42 1)) x)"))

;;; ============================================================
;;; Compile-Time Errors: DEFUN Form
;;; ============================================================

(deftest test-e2e-error-defun-not-symbol ()
  "DEFUN with non-symbol name"
  (e2e-signals-compile-error "(defun 123 () nil)"))

(deftest test-e2e-error-defun-bad-params ()
  "DEFUN with non-list parameters"
  (e2e-signals-compile-error "(defun foo 42 nil)"))

;;; ============================================================
;;; Compile-Time Errors: LAMBDA Form
;;; ============================================================

(deftest test-e2e-error-lambda-bad-params ()
  "LAMBDA with non-list parameters"
  (e2e-signals-compile-error "(lambda 42 x)"))

;;; ============================================================
;;; Compile-Time Errors: Control Flow
;;; ============================================================

(deftest test-e2e-error-go-outside-tagbody ()
  "GO outside of tagbody"
  (e2e-signals-compile-error "(go some-tag)"))

(deftest test-e2e-error-return-from-unknown ()
  "RETURN-FROM with unknown block"
  (e2e-signals-compile-error "(return-from unknown-block 42)"))

;;; ============================================================
;;; Compile-Time Errors: SETQ
;;; ============================================================

(deftest test-e2e-error-setq-odd-args ()
  "SETQ with odd number of arguments"
  (e2e-signals-compile-error "(setq x)"))

(deftest test-e2e-error-setq-non-symbol ()
  "SETQ with non-symbol"
  (e2e-signals-compile-error "(let ((x 1)) (setq 42 10))"))

;;; ============================================================
;;; Read-Time Errors
;;; ============================================================

(deftest test-e2e-error-unbalanced-parens ()
  "Unbalanced parentheses"
  (e2e-signals-read-error "(+ 1 2"))

(deftest test-e2e-error-extra-close ()
  "Extra closing parenthesis"
  (e2e-signals-read-error "(+ 1 2))"))

;;; ============================================================
;;; Valid Edge Cases (Should NOT Error)
;;; ============================================================

(deftest test-e2e-valid-empty-progn ()
  "Empty progn is valid"
  (is-e2e "(progn)" "NIL"))

(deftest test-e2e-valid-empty-block ()
  "Empty block is valid"
  (is-e2e "(block foo)" "NIL"))

(deftest test-e2e-valid-if-three-args ()
  "IF with three arguments is valid"
  (is-e2e "(if t 1 2)" "1"))

(deftest test-e2e-valid-let-empty-body ()
  "LET with single body form is valid"
  (is-e2e "(let ((x 1)) x)" "1"))
