;;;; 1-minus-validation-test.lisp - Contract tests for 1- Wasm output
;;;; Phase 13D-1b: User Story 1 - Validate 1- produces valid Wasm
(in-package #:clysm/tests/contract/arithmetic-primitives)

;;; ============================================================
;;; T005: Contract test - (1- 5) produces valid Wasm that equals 4
;;; ============================================================

(deftest test-1-minus-wasm-validation
  "Verify (1- 5) generates Wasm that passes validation"
  ;; Compile (1- 5) to binary Wasm
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(1- 5))))
    (ok (arrayp wasm-bytes)
        "(1- 5) should produce Wasm byte array")
    (ok (plusp (length wasm-bytes))
        "Wasm output should have content")
    ;; Validate using wasm-tools (via helper)
    (ok (validate-wasm-silent wasm-bytes)
        "(1- 5) Wasm should pass wasm-tools validate")))

(deftest test-1-minus-function-wasm-validation
  "Verify function using 1- generates valid Wasm"
  ;; Compile a complete function using 1-
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun decrement (n) (1- n)))))
    (ok (arrayp wasm-bytes)
        "decrement function should produce Wasm byte array")
    (ok (validate-wasm-silent wasm-bytes)
        "decrement function Wasm should pass validation")))

(deftest test-1-minus-in-expression-wasm-validation
  "Verify (1- x) in larger expression generates valid Wasm"
  ;; Test 1- used in arithmetic expression
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun prev-double (n) (* 2 (1- n))))))
    (ok (arrayp wasm-bytes)
        "prev-double function should compile")
    (ok (validate-wasm-silent wasm-bytes)
        "prev-double Wasm should pass validation")))
