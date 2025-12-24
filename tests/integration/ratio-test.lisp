;;;; ratio-test.lisp - Ratio (exact rational) tests
;;;; 010-numeric-tower: User Story 2 - Rational Number Arithmetic
(in-package #:clysm/tests/integration/ratio)

;;; These tests verify the complete compilation pipeline for ratio operations:
;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution

;;; ============================================================
;;; T037-T051: Ratio Arithmetic Tests
;;; ============================================================

;;; --- Basic Ratio Creation Tests ---

(deftest test-ratio-creation
  "Division creates ratio: (/ 1 3) => 1/3 (spec scenario 1)"
  (let ((result (clysm/tests:compile-and-run-numeric '(/ 1 3))))
    (ok (rationalp result) "(/ 1 3) should return a rational")
    (ok (= 1/3 result) "(/ 1 3) should equal 1/3")))

(deftest test-ratio-simplification
  "Ratio simplifies to integer: (/ 6 3) => 2 (spec scenario 3)"
  (let ((result (clysm/tests:compile-and-run-numeric '(/ 6 3))))
    (ok (integerp result) "(/ 6 3) should simplify to integer")
    (ok (= 2 result) "(/ 6 3) should equal 2")))

(deftest test-ratio-auto-reduce
  "Ratios auto-reduce to lowest terms: (/ 4 8) => 1/2"
  (ok (= 1/2 (clysm/tests:compile-and-run-numeric '(/ 4 8)))
      "(/ 4 8) should reduce to 1/2"))

;;; --- Ratio Arithmetic Tests ---

(deftest test-ratio-addition
  "Ratio addition: (+ 1/2 1/4) => 3/4 (spec scenario 2)"
  (ok (= 3/4 (clysm/tests:compile-and-run-numeric '(+ (/ 1 2) (/ 1 4))))
      "(+ 1/2 1/4) should equal 3/4"))

(deftest test-ratio-addition-exact
  "Ratio exactness: (+ 1/3 1/3 1/3) => 1"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ (/ 1 3) (/ 1 3) (/ 1 3)))))
    (ok (= 1 result) "(+ 1/3 1/3 1/3) should equal exactly 1")
    (ok (integerp result) "Result should be an integer (simplified)")))

(deftest test-ratio-subtraction
  "Ratio subtraction: (- 3/4 1/4) => 1/2"
  (ok (= 1/2 (clysm/tests:compile-and-run-numeric '(- (/ 3 4) (/ 1 4))))
      "(- 3/4 1/4) should equal 1/2"))

(deftest test-ratio-multiplication
  "Ratio multiplication: (* 2/3 3/4) => 1/2"
  (ok (= 1/2 (clysm/tests:compile-and-run-numeric '(* (/ 2 3) (/ 3 4))))
      "(* 2/3 3/4) should equal 1/2"))

(deftest test-ratio-division
  "Ratio division: (/ 1/2 1/4) => 2"
  (let ((result (clysm/tests:compile-and-run-numeric '(/ (/ 1 2) (/ 1 4)))))
    (ok (= 2 result) "(/ 1/2 1/4) should equal 2")
    (ok (integerp result) "Result should simplify to integer")))

;;; --- Mixed Type Ratio Tests ---

(deftest test-ratio-plus-integer
  "Ratio + integer: (+ 1/2 1) => 3/2"
  (ok (= 3/2 (clysm/tests:compile-and-run-numeric '(+ (/ 1 2) 1)))
      "(+ 1/2 1) should equal 3/2"))

(deftest test-integer-minus-ratio
  "Integer - ratio: (- 1 1/4) => 3/4"
  (ok (= 3/4 (clysm/tests:compile-and-run-numeric '(- 1 (/ 1 4))))
      "(- 1 1/4) should equal 3/4"))

(deftest test-ratio-times-integer
  "Ratio * integer: (* 1/3 6) => 2"
  (let ((result (clysm/tests:compile-and-run-numeric '(* (/ 1 3) 6))))
    (ok (= 2 result) "(* 1/3 6) should equal 2")
    (ok (integerp result) "Result should simplify to integer")))

;;; --- Ratio Comparison Tests ---

(deftest test-ratio-equal
  "Ratio equality"
  (ok (clysm/tests:compile-and-run-numeric '(= (/ 1 2) (/ 2 4)))
      "1/2 = 2/4 should be true"))

(deftest test-ratio-less-than
  "Ratio less than"
  (ok (clysm/tests:compile-and-run-numeric '(< (/ 1 3) (/ 1 2)))
      "1/3 < 1/2 should be true"))

(deftest test-ratio-greater-than
  "Ratio greater than"
  (ok (clysm/tests:compile-and-run-numeric '(> (/ 2 3) (/ 1 2)))
      "2/3 > 1/2 should be true"))

;;; --- Bignum Ratio Tests (spec scenario 4) ---

(deftest test-ratio-with-bignum-components
  "Ratio with bignum numerator/denominator"
  (let ((result (clysm/tests:compile-and-run-numeric
                 '(/ 10000000000000000000 30000000000000000000))))
    (ok (= 1/3 result)
        "Ratio with bignums should reduce correctly")))

;;; --- Edge Cases ---

(deftest test-negative-ratio
  "Negative ratio: (/ -1 2) => -1/2"
  (ok (= -1/2 (clysm/tests:compile-and-run-numeric '(/ -1 2)))
      "(/ -1 2) should equal -1/2"))

(deftest test-double-negative-ratio
  "Double negative ratio: (/ -1 -2) => 1/2"
  (ok (= 1/2 (clysm/tests:compile-and-run-numeric '(/ -1 -2)))
      "(/ -1 -2) should equal 1/2"))

(deftest test-ratio-numerator
  "Numerator accessor"
  (ok (= 1 (clysm/tests:compile-and-run-numeric '(numerator (/ 1 3))))
      "(numerator 1/3) should be 1"))

(deftest test-ratio-denominator
  "Denominator accessor"
  (ok (= 3 (clysm/tests:compile-and-run-numeric '(denominator (/ 1 3))))
      "(denominator 1/3) should be 3"))
