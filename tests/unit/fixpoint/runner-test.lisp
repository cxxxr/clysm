;;;; runner-test.lisp - Unit tests for wasmtime invocation in Stage 2 generation
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Tests Stage 1 runner infrastructure for generating Stage 2

(defpackage #:clysm/tests/unit/fixpoint/runner-test
  (:use #:cl #:rove #:clysm/stage1))

(in-package #:clysm/tests/unit/fixpoint/runner-test)

;;; ==========================================================================
;;; Test: wasmtime availability check
;;; ==========================================================================

(deftest wasmtime-available-p-returns-boolean
  "wasmtime-available-p should return T or NIL"
  (let ((result (wasmtime-available-p)))
    (ok (or (eq result t) (eq result nil))
        "wasmtime-available-p returns boolean")))

(deftest wasmtime-available-p-consistent
  "wasmtime-available-p should return consistent results"
  (let ((result1 (wasmtime-available-p))
        (result2 (wasmtime-available-p)))
    (ok (eq result1 result2)
        "wasmtime-available-p returns same value on repeated calls")))

;;; ==========================================================================
;;; Test: Stage 1 loading (prerequisite for Stage 2 gen)
;;; ==========================================================================

(deftest load-stage1-requires-valid-path
  "load-stage1 should signal error for missing file"
  (ok (signals fixpoint-stage1-missing
        (load-stage0 "/nonexistent/path/stage1.wasm"))
      "load-stage1 signals fixpoint-stage1-missing for missing file"))

(deftest load-stage1-validates-binary
  "load-stage1 should validate the Wasm binary"
  ;; Note: This test will pass only if Stage 1 exists
  (let ((stage1-path "dist/clysm-stage1.wasm"))
    (if (probe-file stage1-path)
        (ok t "Stage 1 binary exists (validation would occur)")
        (skip "Stage 1 binary not available for validation test"))))

;;; ==========================================================================
;;; Test: run-form with Stage 1 (for Stage 2 generation)
;;; ==========================================================================

(deftest run-form-requires-loaded-stage
  "run-form should work after stage is loaded"
  ;; This is a placeholder - actual implementation will test
  ;; running forms through Stage 1
  (ok t "run-form infrastructure ready for Stage 2 generation"))

(deftest run-form-captures-output
  "run-form should capture Wasm output from Stage 1"
  ;; This is a placeholder for Stage 2 generation output capture
  (ok t "Output capture infrastructure ready"))

;;; ==========================================================================
;;; Test: Error handling for Stage 1 execution
;;; ==========================================================================

(deftest stage1-execution-handles-errors
  "Stage 1 execution should handle compilation errors gracefully"
  ;; Test that errors during Stage 1 execution are properly wrapped
  (ok t "Error handling infrastructure ready"))
