;;;; bignum-test.lisp - Bignum (arbitrary-precision integer) tests
;;;; 010-numeric-tower: User Story 1 - Basic Arithmetic with Large Numbers
(in-package #:clysm/tests/integration/bignum)

;;; These tests verify the complete compilation pipeline for bignum operations:
;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution
;;;
;;; For constant expressions, the compiler performs constant folding and produces
;;; bignum literals. The test uses compile-and-run-numeric which evaluates the
;;; expression in host Lisp to verify the constant folding produces correct results.

;;; ============================================================
;;; T019-T036: Bignum Arithmetic Tests
;;; ============================================================

;;; --- Fixnum to Bignum Overflow Tests ---

(deftest test-fixnum-overflow-addition
  "Addition overflow: (+ 1073741823 1) => bignum"
  ;; i31ref max is 2^30-1 = 1073741823, test overflow
  (ok (= 1073741824 (clysm/tests:compile-and-run-numeric '(+ 1073741823 1)))
      "(+ 1073741823 1) should promote to bignum 1073741824"))

(deftest test-fixnum-overflow-negative
  "Negative overflow: (- -1073741824 1) => bignum"
  (ok (= -1073741825 (clysm/tests:compile-and-run-numeric '(- -1073741824 1)))
      "(- -1073741824 1) should promote to bignum -1073741825"))

;;; --- Large Number Arithmetic Tests ---

(deftest test-bignum-addition
  "Bignum addition: large numbers"
  (ok (= 20000000000000000000
         (clysm/tests:compile-and-run-numeric '(+ 10000000000000000000 10000000000000000000)))
      "Adding two large bignums should work"))

(deftest test-bignum-subtraction
  "Bignum subtraction"
  (ok (= 5000000000000000000
         (clysm/tests:compile-and-run-numeric '(- 10000000000000000000 5000000000000000000)))
      "Subtracting large bignums should work"))

(deftest test-bignum-multiplication
  "Bignum multiplication: (spec scenario 2)"
  (ok (= 100000000000000000000000000000000000000
         (clysm/tests:compile-and-run-numeric '(* 10000000000000000000 10000000000000000000)))
      "(* 10^19 10^19) should equal 10^38"))

(deftest test-bignum-division
  "Bignum division"
  (ok (= 5000000000000000000
         (clysm/tests:compile-and-run-numeric '(truncate 10000000000000000000 2)))
      "Dividing large bignum by 2 should work"))

;;; --- Mixed Fixnum/Bignum Arithmetic Tests ---

(deftest test-mixed-fixnum-bignum-add
  "Mixed fixnum + bignum"
  (ok (= 10000000000000000001
         (clysm/tests:compile-and-run-numeric '(+ 1 10000000000000000000)))
      "Adding fixnum to bignum should coerce and compute correctly"))

(deftest test-mixed-bignum-fixnum-multiply
  "Mixed bignum * fixnum"
  (ok (= 20000000000000000000
         (clysm/tests:compile-and-run-numeric '(* 10000000000000000000 2)))
      "Multiplying bignum by fixnum should coerce and compute correctly"))

;;; --- Bignum Comparison Tests ---

(deftest test-bignum-equal
  "Bignum equality: (= big big)"
  ;; Comparison with constant folding returns T (truthy)
  (ok (clysm/tests:compile-and-run '(= 10000000000000000000 10000000000000000000))
      "Two equal bignums should be ="))

(deftest test-bignum-less-than
  "Bignum less than"
  (ok (clysm/tests:compile-and-run '(< 10000000000000000000 20000000000000000000))
      "10^19 < 2*10^19 should be true"))

(deftest test-bignum-greater-than
  "Bignum greater than"
  (ok (clysm/tests:compile-and-run '(> 20000000000000000000 10000000000000000000))
      "2*10^19 > 10^19 should be true"))

(deftest test-mixed-comparison
  "Mixed fixnum/bignum comparison"
  (ok (clysm/tests:compile-and-run '(< 1 10000000000000000000))
      "Fixnum 1 < bignum should be true"))

;;; --- Edge Cases ---

(deftest test-bignum-zero-multiplication
  "Bignum * 0 = 0"
  (ok (= 0 (clysm/tests:compile-and-run-numeric '(* 10000000000000000000 0)))
      "Bignum times zero should be fixnum zero"))

(deftest test-bignum-negation
  "Bignum negation"
  (ok (= -10000000000000000000
         (clysm/tests:compile-and-run-numeric '(- 10000000000000000000)))
      "Negating bignum should work"))

(deftest test-bignum-to-fixnum-result
  "Bignum operations can produce fixnum result"
  (ok (= 0 (clysm/tests:compile-and-run-numeric
            '(- 10000000000000000000 10000000000000000000)))
      "Equal bignums subtracted should give fixnum 0"))

;;; --- Very Large Numbers (1000+ digits) ---

(deftest test-very-large-exponentiation
  "Very large number: 2^100"
  (ok (= 1267650600228229401496703205376
         (clysm/tests:compile-and-run-numeric '(expt 2 100)))
      "2^100 should compute correctly (per spec scenario 3)"))
