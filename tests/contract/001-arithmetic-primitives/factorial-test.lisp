;;;; factorial-test.lisp - Contract test for factorial using 1-
;;;; Phase 13D-1b: User Story 1 - Key validation: factorial compiles
(in-package #:clysm/tests/contract/arithmetic-primitives)

;;; ============================================================
;;; T006: Contract test - Factorial function using 1- compiles and validates
;;; This is the PRIMARY acceptance test for User Story 1
;;; ============================================================

(deftest test-factorial-compiles
  "Verify factorial function using 1- compiles successfully"
  ;; This is the exact test case from the spec:
  ;; (defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun fact (n)
                       (if (<= n 1)
                           1
                           (* n (fact (1- n))))))))
    (ok (arrayp wasm-bytes)
        "Factorial function should compile to Wasm byte array")
    (ok (plusp (length wasm-bytes))
        "Factorial Wasm should have content")))

(deftest test-factorial-wasm-validates
  "Verify factorial Wasm passes wasm-tools validate"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun fact (n)
                       (if (<= n 1)
                           1
                           (* n (fact (1- n))))))))
    (ok (validate-wasm-silent wasm-bytes)
        "Factorial Wasm MUST pass wasm-tools validate (SC-001)")))

(deftest test-factorial-wat-structure
  "Verify factorial WAT has expected structure"
  (let ((wat (clysm/compiler:compile-to-wat
              '(defun fact (n)
                (if (<= n 1)
                    1
                    (* n (fact (1- n))))))))
    (ok (stringp wat)
        "Factorial should compile to WAT string")
    ;; Check for key instructions
    (ok (search "i32.sub" wat)
        "Factorial should use i32.sub for 1-")
    (ok (search "i32.mul" wat)
        "Factorial should use i32.mul for *")
    (ok (search "i32.le_s" wat)
        "Factorial should use i32.le_s for <=")))
