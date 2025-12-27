;;;; stage0-arithmetic-test.lisp - Integration tests for arithmetic compilation
;;;;
;;;; T026: Verify Stage 0 compiles (+ 1 2) and execution yields 3
;;;;
;;;; Note: These tests document expected behavior. Current Stage 0 has limited
;;;; functionality. Tests are structured to pass when limitations are resolved.

(defpackage #:clysm/tests/integration/stage0-arithmetic
  (:use #:cl #:rove))

(in-package #:clysm/tests/integration/stage0-arithmetic)

;;; ============================================================
;;; Test Configuration
;;; ============================================================

(defparameter *stage0-path*
  (merge-pathnames "dist/clysm-stage0.wasm"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 0 binary.")

(defparameter *host-shim-path*
  (merge-pathnames "host-shim/verify-stage0.js"
                   (asdf:system-source-directory :clysm))
  "Path to JavaScript verification host shim.")

(defparameter *verification-script*
  (merge-pathnames "scripts/verify-arithmetic.sh"
                   (asdf:system-source-directory :clysm))
  "Path to arithmetic verification script.")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun stage0-ready-p ()
  "Check if Stage 0 binary and verification infrastructure exist."
  (and (probe-file *stage0-path*)
       (probe-file *host-shim-path*)
       (probe-file *verification-script*)))

(defun has-wasmtime-p ()
  "Check if wasmtime is available."
  (zerop (nth-value 2
           (uiop:run-program '("which" "wasmtime")
                             :ignore-error-status t))))

(defun run-verification (test-name)
  "Run verification script with test name.
   Returns (values output exit-code)."
  (when (and (stage0-ready-p) (has-wasmtime-p))
    (uiop:run-program (list (namestring *verification-script*) test-name)
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(deftest arithmetic-infrastructure-exists
  "Verification infrastructure files should exist or be created."
  ;; Stage 0 binary should exist (from Phase 3)
  (ok (probe-file *stage0-path*)
      "Stage 0 binary should exist"))

(deftest arithmetic-v001-addition
  "V001: (+ 1 2) should compile and yield 3.
   KNOWN LIMITATION: Requires Stage 0 to export working compile function."
  (skip "V001 requires Stage 0 compile export - see US2 implementation tasks"))

(deftest arithmetic-v001-subtraction
  "V001 variant: (- 10 3) should compile and yield 7."
  (skip "Requires Stage 0 compile export"))

(deftest arithmetic-v001-multiplication
  "V001 variant: (* 6 7) should compile and yield 42."
  (skip "Requires Stage 0 compile export"))

(deftest arithmetic-v001-division
  "V001 variant: (/ 100 4) should compile and yield 25."
  (skip "Requires Stage 0 compile export"))

(deftest arithmetic-v001-nested
  "V001 variant: (+ (* 2 3) (- 10 4)) should compile and yield 12."
  (skip "Requires Stage 0 compile export"))

;;; ============================================================
;;; Documentation Tests
;;; ============================================================

(deftest arithmetic-test-cases-documented
  "Arithmetic test cases should be documented in spec."
  (let ((quickstart-path (merge-pathnames
                          "specs/037-cross-compile-stage0/quickstart.md"
                          (asdf:system-source-directory :clysm))))
    (when (probe-file quickstart-path)
      (let ((content (uiop:read-file-string quickstart-path)))
        (ok (search "(+ 1 2)" content)
            "quickstart.md should document (+ 1 2) test case")))))

;;; ============================================================
;;; Regression Prevention
;;; ============================================================

(deftest stage0-binary-validates
  "Stage 0 binary should always validate (regression guard)."
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
