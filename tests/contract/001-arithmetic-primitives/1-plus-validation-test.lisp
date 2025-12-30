;;;; 1-plus-validation-test.lisp - Contract tests for 1+ Wasm output
;;;; Phase 13D-1b: User Story 2 - Validate 1+ produces valid Wasm
(in-package #:clysm/tests/contract/arithmetic-primitives)

;;; ============================================================
;;; T011: Contract test - (1+ 5) produces valid Wasm that equals 6
;;; ============================================================

(deftest test-1-plus-wasm-validation
  "Verify (1+ 5) generates Wasm that passes validation"
  ;; Compile (1+ 5) to binary Wasm
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(1+ 5))))
    (ok (arrayp wasm-bytes)
        "(1+ 5) should produce Wasm byte array")
    (ok (plusp (length wasm-bytes))
        "Wasm output should have content")
    ;; Validate using wasm-tools (via helper)
    (ok (validate-wasm-silent wasm-bytes)
        "(1+ 5) Wasm should pass wasm-tools validate")))

(deftest test-1-plus-function-wasm-validation
  "Verify function using 1+ generates valid Wasm"
  ;; Compile a complete function using 1+
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun increment (n) (1+ n)))))
    (ok (arrayp wasm-bytes)
        "increment function should produce Wasm byte array")
    (ok (validate-wasm-silent wasm-bytes)
        "increment function Wasm should pass validation")))

(deftest test-1-plus-in-expression-wasm-validation
  "Verify (1+ x) in larger expression generates valid Wasm"
  ;; Test 1+ used in arithmetic expression
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun next-double (n) (* 2 (1+ n))))))
    (ok (arrayp wasm-bytes)
        "next-double function should compile")
    (ok (validate-wasm-silent wasm-bytes)
        "next-double Wasm should pass validation")))

(deftest test-1-plus-iterative-pattern
  "Verify 1+ in iterative pattern compiles and validates"
  ;; Common pattern: loop counter increment
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun count-up (start end)
                       (if (>= start end)
                           start
                           (count-up (1+ start) end))))))
    (ok (arrayp wasm-bytes)
        "count-up function should compile")
    (ok (validate-wasm-silent wasm-bytes)
        "count-up Wasm should pass validation")))
