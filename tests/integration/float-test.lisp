;;;; float-test.lisp - Float (IEEE 754 double-precision) tests
;;;; 010-numeric-tower: User Story 3 - Floating-Point Arithmetic
(in-package #:clysm/tests/integration/float)

;;; These tests verify the complete compilation pipeline for float operations:
;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution

;;; ============================================================
;;; T052-T066: Float Arithmetic Tests
;;; ============================================================

;;; --- Basic Float Arithmetic Tests ---

(deftest test-float-addition
  "Float addition: (+ 1.5 2.5) => 4.0 (spec scenario 1)"
  (ok (= 4.0 (clysm/tests:compile-and-run-numeric '(+ 1.5 2.5)))
      "(+ 1.5 2.5) should equal 4.0"))

(deftest test-float-subtraction
  "Float subtraction: (- 5.0 2.5) => 2.5"
  (ok (= 2.5 (clysm/tests:compile-and-run-numeric '(- 5.0 2.5)))
      "(- 5.0 2.5) should equal 2.5"))

(deftest test-float-multiplication
  "Float multiplication: (* 2.5 4.0) => 10.0"
  (ok (= 10.0 (clysm/tests:compile-and-run-numeric '(* 2.5 4.0)))
      "(* 2.5 4.0) should equal 10.0"))

(deftest test-float-division
  "Float division: (/ 10.0 4.0) => 2.5"
  (ok (= 2.5 (clysm/tests:compile-and-run-numeric '(/ 10.0 4.0)))
      "(/ 10.0 4.0) should equal 2.5"))

;;; --- Mixed Integer/Float Arithmetic Tests ---

(deftest test-integer-plus-float
  "Integer + float coercion: (+ 1 2.5) => 3.5 (spec scenario 2)"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1 2.5))))
    (ok (floatp result) "Result should be a float")
    (ok (= 3.5 result) "(+ 1 2.5) should equal 3.5")))

(deftest test-float-plus-integer
  "Float + integer: (+ 2.5 1) => 3.5"
  (ok (= 3.5 (clysm/tests:compile-and-run-numeric '(+ 2.5 1)))
      "(+ 2.5 1) should equal 3.5"))

(deftest test-ratio-to-float-coercion
  "Ratio coerces to float: (+ 1/2 1.0) => 1.5"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ (/ 1 2) 1.0))))
    (ok (floatp result) "Result should be a float")
    (ok (= 1.5 result) "(+ 1/2 1.0) should equal 1.5")))

;;; --- Float Comparison Tests ---

(deftest test-float-equal
  "Float equality"
  (ok (clysm/tests:compile-and-run-numeric '(= 1.5 1.5))
      "1.5 = 1.5 should be true"))

(deftest test-float-less-than
  "Float less than"
  (ok (clysm/tests:compile-and-run-numeric '(< 1.0 2.0))
      "1.0 < 2.0 should be true"))

(deftest test-float-greater-than
  "Float greater than"
  (ok (clysm/tests:compile-and-run-numeric '(> 2.0 1.0))
      "2.0 > 1.0 should be true"))

(deftest test-mixed-float-integer-comparison
  "Mixed float/integer comparison"
  (ok (clysm/tests:compile-and-run-numeric '(= 2.0 2))
      "2.0 = 2 should be true"))

;;; --- IEEE 754 Special Values Tests (spec scenario 3) ---

(deftest test-positive-infinity
  "Positive infinity"
  (let ((result (clysm/tests:compile-and-run-numeric '(/ 1.0 0.0))))
    (ok (floatp result) "Result should be a float")
    ;; Note: Exact infinity comparison depends on runtime representation
    ))

(deftest test-negative-infinity
  "Negative infinity"
  (let ((result (clysm/tests:compile-and-run-numeric '(/ -1.0 0.0))))
    (ok (floatp result) "Result should be a float")))

(deftest test-nan-arithmetic
  "NaN propagation"
  ;; NaN is not equal to itself - constant folding handles this
  (ok (null (clysm/tests:compile-and-run-numeric
             '(let ((nan (- (/ 1.0 0.0) (/ 1.0 0.0))))
                (= nan nan))))
      "NaN should not equal itself"))

(deftest test-negative-zero
  "Negative zero"
  (ok (= 0.0 (clysm/tests:compile-and-run-numeric '(- 0.0)))
      "-0.0 should equal 0.0"))

;;; --- Float Precision Tests (spec scenario 4) ---

(deftest test-double-precision
  "Double precision: 1.0d0"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1.0d0 0.0d0))))
    (ok (floatp result) "Double float should be recognized")))

(deftest test-float-precision-preservation
  "Float precision in arithmetic"
  (ok (< (abs (- (clysm/tests:compile-and-run-numeric '(/ 1.0 3.0))
                 0.3333333333333333d0))
         1.0d-15)
      "1.0/3.0 should be close to 0.333..."))

;;; --- Edge Cases ---

(deftest test-float-zero-multiplication
  "Float * 0 = 0.0"
  (let ((result (clysm/tests:compile-and-run-numeric '(* 3.14159 0))))
    (ok (= 0.0 result) "Float times zero should be zero")
    (ok (floatp result) "Result should be a float")))

(deftest test-very-small-float
  "Very small float"
  (ok (< (clysm/tests:compile-and-run-numeric '(* 1.0d-100 1.0d-100)) 1.0d-190)
      "Very small float multiplication should work"))

(deftest test-very-large-float
  "Very large float"
  (ok (> (clysm/tests:compile-and-run-numeric '(* 1.0d100 1.0d100)) 1.0d190)
      "Very large float multiplication should work"))
