;;;; compile-1-test.lisp - Unit tests for compile-1- (001-arithmetic-primitives)
;;;; Phase 13D-1b: User Story 1 - Compile 1- Primitive
(in-package #:clysm/tests/unit/arithmetic-primitives)

;;; ============================================================
;;; T004: Unit test for compile-1- returns correct Wasm instructions
;;; ============================================================

;; Test that (1- x) generates correct Wasm instructions:
;; 1. Compile argument
;; 2. ref.cast i31
;; 3. i31.get_s
;; 4. i32.const 1
;; 5. i32.sub
;; 6. ref.i31

(deftest test-compile-1-minus-basic
  "Verify (1- x) generates correct Wasm instructions"
  ;; Test basic compilation of (1- 5)
  (let ((wat (clysm/compiler:compile-to-wat '(1- 5))))
    (ok (stringp wat)
        "(1- 5) should compile to WAT string")
    (ok (search "i32.sub" wat)
        "(1- 5) should generate i32.sub instruction")
    (ok (search "i32.const 1" wat)
        "(1- 5) should push constant 1")))

(deftest test-compile-1-minus-with-variable
  "Verify (1- n) generates correct structure"
  ;; Test with local variable reference
  (let ((wat (clysm/compiler:compile-to-wat
              '(lambda (n) (1- n)))))
    (ok (stringp wat)
        "(lambda (n) (1- n)) should compile")
    (ok (search "i32.sub" wat)
        "1- should use i32.sub instruction")))

(deftest test-compile-1-minus-nested
  "Verify nested (1- (1- x)) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat
              '(lambda (n) (1- (1- n))))))
    (ok (stringp wat)
        "Nested 1- should compile")
    ;; Should have two i32.sub instructions
    (let ((pos1 (search "i32.sub" wat)))
      (ok pos1 "Should have at least one i32.sub")
      (when pos1
        (ok (search "i32.sub" wat :start2 (1+ pos1))
            "Should have second i32.sub for nested call")))))

(deftest test-compile-1-minus-arity-error
  "Verify wrong arity signals error"
  ;; (1-) with no arguments should error
  (ok (signals (clysm/compiler:compile-to-wat '(1-)))
      "(1-) with no args should signal error")
  ;; (1- 1 2) with two arguments should error
  (ok (signals (clysm/compiler:compile-to-wat '(1- 1 2)))
      "(1- 1 2) with two args should signal error"))
