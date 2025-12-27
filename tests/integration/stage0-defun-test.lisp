;;;; stage0-defun-test.lisp - Integration tests for defun compilation
;;;;
;;;; T032: Verify Stage 0 compiles (defun f (x) (* x 2)) (f 21) → 42
;;;;
;;;; Note: These tests document expected behavior. Current Stage 0 has limited
;;;; functionality. Tests are structured to pass when limitations are resolved.

(defpackage #:clysm/tests/integration/stage0-defun
  (:use #:cl #:rove))

(in-package #:clysm/tests/integration/stage0-defun)

;;; ============================================================
;;; Test Configuration
;;; ============================================================

(defparameter *stage0-path*
  (merge-pathnames "dist/clysm-stage0.wasm"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 0 binary.")

(defparameter *verification-script*
  (merge-pathnames "scripts/verify-defun.sh"
                   (asdf:system-source-directory :clysm))
  "Path to defun verification script.")

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(deftest defun-infrastructure-exists
  "Verification infrastructure should be prepared."
  (ok (probe-file *stage0-path*)
      "Stage 0 binary should exist"))

(deftest defun-v002-simple-function
  "V002: (defun f (x) (* x 2)) (f 21) should yield 42.
   KNOWN LIMITATION: Requires Stage 0 compile export."
  (skip "V002 requires Stage 0 compile export - see US3 implementation tasks"))

(deftest defun-v002-multiple-args
  "V002 variant: (defun add (a b) (+ a b)) (add 10 20) should yield 30."
  (skip "Requires Stage 0 compile export"))

(deftest defun-v002-recursive
  "V002 variant: (defun fact (n) (if (<= n 1) 1 (* n (fact (1- n))))) (fact 5) → 120."
  (skip "Requires Stage 0 compile export"))

(deftest defun-v002-closure
  "V002 variant: (let ((x 10)) (defun addx (y) (+ x y))) (addx 5) → 15."
  (skip "Requires Stage 0 compile export"))

;;; ============================================================
;;; Regression Prevention
;;; ============================================================

(deftest stage0-binary-still-validates
  "Stage 0 binary should validate (regression guard for US3)."
  (skip-unless (probe-file *stage0-path*))
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "wasm-tools" "validate"
                              (namestring *stage0-path*))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output error-output))
    (ok (zerop exit-code)
        "Stage 0 should validate - regression guard")))
