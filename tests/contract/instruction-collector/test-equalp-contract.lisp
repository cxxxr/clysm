;;;; Contract Test: Equalp Bytecode Verification
;;;; Verifies that compile-equalp produces byte-identical Wasm output after migration

(defpackage #:clysm/tests/contract/instruction-collector/test-equalp-contract
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:compile-to-wasm
                #:compile-to-instructions)
  (:import-from #:clysm/tests/contract/instruction-collector/bytecode-compare
                #:bytecode-identical-p
                #:verify-against-baseline
                #:format-diff-report))

(in-package #:clysm/tests/contract/instruction-collector/test-equalp-contract)

;;; Test forms that exercise compile-equalp

(defparameter *equalp-test-form*
  '(defun test-equalp (x y) (equalp x y))
  "Basic equalp test form")

(defparameter *equal-test-form*
  '(defun test-equal (x y) (equal x y))
  "Basic equal test form")

(defparameter *complex-equalp-form*
  '(defun test-complex-equalp (a b c d)
     (and (equalp a b)
          (equalp c d)
          (equalp (cons a b) (cons c d))))
  "Complex equalp test form with multiple calls")

;;; Contract verification tests

(deftest equalp-bytecode-contract
  (testing "equalp compilation produces consistent Wasm output"
    ;; Skip if baseline does not exist
    (let ((baseline-path (merge-pathnames
                          "baselines/equalp-baseline.wasm"
                          (asdf:system-relative-pathname
                           :clysm "tests/contract/instruction-collector/"))))
      (if (probe-file baseline-path)
          (multiple-value-bind (success-p report)
              (verify-against-baseline "equalp-baseline" *equalp-test-form*)
            (ok success-p (format nil "equalp bytecode: ~A" report)))
          (skip "Baseline not captured - run capture-all-baselines first")))))

(deftest equal-bytecode-contract
  (testing "equal compilation produces consistent Wasm output"
    (let ((baseline-path (merge-pathnames
                          "baselines/equal-baseline.wasm"
                          (asdf:system-relative-pathname
                           :clysm "tests/contract/instruction-collector/"))))
      (if (probe-file baseline-path)
          (multiple-value-bind (success-p report)
              (verify-against-baseline "equal-baseline" *equal-test-form*)
            (ok success-p (format nil "equal bytecode: ~A" report)))
          (skip "Baseline not captured - run capture-all-baselines first")))))

;;; Analysis documentation: compile-equalp migration assessment

#|
ANALYSIS: compile-equalp Migration Assessment
=============================================
Date: 2026-01-03
Branch: 001-instruction-collector-refactor

Function Location: func-section.lisp lines 4809-5181 (374 lines)

Pattern Count: 2 ,@ patterns
  - Line 4837: ,@(compile-to-instructions (first args) env)
  - Line 4839: ,@(compile-to-instructions (second args) env)

Structure:
  - Uses single large quasiquote expression
  - Only 2 ,@ calls at top for argument compilation
  - Rest is inline instruction list with ,var substitutions

Migration Effort: LOW
  - Minimal ,@ patterns to convert
  - Adjacent function (compile-atom) already uses with-instruction-collector

Migration Benefit: LOW
  - Only 2 append operations eliminated
  - 126 remaining patterns in other functions provide more impact

Recommendation:
  If migrating compile-equalp, wrap with with-instruction-collector and use:
  (emit* (compile-to-instructions (first args) env))
  (emit* (compile-to-instructions (second args) env))
  (emit* '(...rest of instructions...))

  However, focus on functions with higher ,@ pattern counts for better ROI.
|#
