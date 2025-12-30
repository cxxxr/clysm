;;;; compile-1+-test.lisp - Unit tests for compile-1+ (001-arithmetic-primitives)
;;;; Phase 13D-1b: User Story 2 - Compile 1+ Primitive
(in-package #:clysm/tests/unit/arithmetic-primitives)

;;; ============================================================
;;; T010: Unit test for compile-1+ returns correct Wasm instructions
;;; ============================================================

;; Test that (1+ x) generates correct Wasm instructions:
;; 1. Compile argument
;; 2. ref.cast i31
;; 3. i31.get_s
;; 4. i32.const 1
;; 5. i32.add
;; 6. ref.i31

(deftest test-compile-1-plus-basic
  "Verify (1+ x) generates correct Wasm instructions"
  ;; Test basic compilation of (1+ 5)
  (let ((wat (clysm/compiler:compile-to-wat '(1+ 5))))
    (ok (stringp wat)
        "(1+ 5) should compile to WAT string")
    (ok (search "i32.add" wat)
        "(1+ 5) should generate i32.add instruction")
    (ok (search "i32.const 1" wat)
        "(1+ 5) should push constant 1")))

(deftest test-compile-1-plus-with-variable
  "Verify (1+ n) generates correct structure"
  ;; Test with local variable reference
  (let ((wat (clysm/compiler:compile-to-wat
              '(lambda (n) (1+ n)))))
    (ok (stringp wat)
        "(lambda (n) (1+ n)) should compile")
    (ok (search "i32.add" wat)
        "1+ should use i32.add instruction")))

(deftest test-compile-1-plus-nested
  "Verify nested (1+ (1+ x)) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat
              '(lambda (n) (1+ (1+ n))))))
    (ok (stringp wat)
        "Nested 1+ should compile")
    ;; Should have two i32.add instructions
    (let ((pos1 (search "i32.add" wat)))
      (ok pos1 "Should have at least one i32.add")
      (when pos1
        (ok (search "i32.add" wat :start2 (1+ pos1))
            "Should have second i32.add for nested call")))))

(deftest test-compile-1-plus-arity-error
  "Verify wrong arity signals error"
  ;; (1+) with no arguments should error
  (ok (signals (clysm/compiler:compile-to-wat '(1+)))
      "(1+) with no args should signal error")
  ;; (1+ 1 2) with two arguments should error
  (ok (signals (clysm/compiler:compile-to-wat '(1+ 1 2)))
      "(1+ 1 2) with two args should signal error"))

(deftest test-compile-1-plus-in-loop-pattern
  "Verify (1+ i) works in typical loop increment pattern"
  ;; Common pattern: increment counter
  (let ((wat (clysm/compiler:compile-to-wat
              '(defun next-index (i) (1+ i)))))
    (ok (stringp wat)
        "next-index function should compile")
    (ok (search "i32.add" wat)
        "next-index should use i32.add")))
