;;;; tier-promotion-test.lisp - Contract tests for tier promotion
;;;; Feature: 017-eval-jit-compile
;;;; Tests that JIT compilation produces valid Wasm (T026)

(in-package #:clysm/tests/contract/tier-promotion)

;;; ============================================================
;;; T026: Contract test - promotion produces valid Wasm
;;; ============================================================

(deftest test-jit-compile-produces-valid-wasm
  "T026: JIT compilation produces valid Wasm binary."
  ;; Test that jit-compile creates valid Wasm for a simple lambda
  (let* ((lambda-expr '(lambda (x) (+ x 1)))
         (wasm (clysm/eval/jit:generate-wasm lambda-expr)))
    ;; Wasm should be a byte vector
    (ok (typep wasm '(vector (unsigned-byte 8)))
        "JIT produces byte vector")
    ;; Wasm should have magic bytes
    (ok (>= (length wasm) 8)
        "Wasm binary has minimum length")
    (ok (= (aref wasm 0) #x00)
        "First magic byte is 0x00")
    (ok (= (aref wasm 1) #x61)
        "Second magic byte is 0x61 ('a')")
    (ok (= (aref wasm 2) #x73)
        "Third magic byte is 0x73 ('s')")
    (ok (= (aref wasm 3) #x6d)
        "Fourth magic byte is 0x6d ('m')")
    ;; Version bytes
    (ok (= (aref wasm 4) #x01)
        "Version is 1")))

(deftest test-jit-compile-validates
  "T026: JIT-generated Wasm passes validation."
  (let* ((lambda-expr '(lambda (x y) (* x y)))
         (wasm (clysm/eval/jit:generate-wasm lambda-expr)))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Generated Wasm passes validation")))

(deftest test-jit-compile-multi-form-lambda
  "T026: JIT handles lambda with multiple body forms."
  (let* ((lambda-expr '(lambda (x) (progn 1 2 (+ x 3))))
         (wasm (clysm/eval/jit:generate-wasm lambda-expr)))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Multi-form lambda produces valid Wasm")))

(deftest test-jit-compile-conditional
  "T026: JIT handles conditional expressions."
  (let* ((lambda-expr '(lambda (x) (if (< x 0) (- x) x)))
         (wasm (clysm/eval/jit:generate-wasm lambda-expr)))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Conditional lambda produces valid Wasm")))
