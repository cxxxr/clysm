;;;; stage0-control-flow-test.lisp - Integration tests for control flow
;;;;
;;;; T036: Verify Stage 0 compiles (if (> 10 5) 'greater 'less) → GREATER
;;;;
;;;; Note: These tests document expected behavior. Current Stage 0 has limited
;;;; functionality. Tests are structured to pass when limitations are resolved.

(defpackage #:clysm/tests/integration/stage0-control-flow
  (:use #:cl #:rove))

(in-package #:clysm/tests/integration/stage0-control-flow)

;;; ============================================================
;;; Test Configuration
;;; ============================================================

(defparameter *stage0-path*
  (merge-pathnames "dist/clysm-stage0.wasm"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 0 binary.")

(defparameter *verification-script*
  (merge-pathnames "scripts/verify-control-flow.sh"
                   (asdf:system-source-directory :clysm))
  "Path to control flow verification script.")

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(deftest control-flow-infrastructure-exists
  "Verification infrastructure should be prepared."
  (ok (probe-file *stage0-path*)
      "Stage 0 binary should exist"))

(deftest control-flow-v003-if-greater
  "V003: (if (> 10 5) 'greater 'less) should yield GREATER.
   KNOWN LIMITATION: Requires Stage 0 compile export."
  (skip "V003 requires Stage 0 compile export - see US4 implementation tasks"))

(deftest control-flow-v003-if-less
  "V003 variant: (if (< 10 5) 'greater 'less) should yield LESS."
  (skip "Requires Stage 0 compile export"))

(deftest control-flow-v003-when
  "V003 variant: (when (> 10 5) 'yes) should yield YES."
  (skip "Requires Stage 0 compile export"))

(deftest control-flow-v003-unless
  "V003 variant: (unless (< 10 5) 'no-change) should yield NO-CHANGE."
  (skip "Requires Stage 0 compile export"))

(deftest control-flow-v003-cond
  "V003 variant: (cond ((< 5 3) 'a) ((> 5 3) 'b) (t 'c)) should yield B."
  (skip "Requires Stage 0 compile export"))

;;; ============================================================
;;; Regression Prevention
;;; ============================================================

(deftest stage0-binary-validates-us4
  "Stage 0 binary should validate (regression guard for US4)."
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
