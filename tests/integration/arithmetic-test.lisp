;;;; arithmetic-test.lisp - Arithmetic operation tests
(in-package #:clysm/tests/integration/arithmetic)

;;; T028-T029: Fixnum arithmetic and comparison tests
;;; These tests verify the complete compilation pipeline:
;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution

;;; Helper function (requires implementation in tests/helpers.lisp)
;;; (compile-and-run expr) -> runs expr through full pipeline

;;; ============================================================
;;; T028: Fixnum Arithmetic Tests (10+ cases)
;;; ============================================================

(deftest test-add-two-fixnums
  "Basic addition: (+ 1 2) => 3"
  (ok (= 3 (clysm/tests:compile-and-run '(+ 1 2)))
      "(+ 1 2) should equal 3"))

(deftest test-add-multiple-fixnums
  "Multiple addition: (+ 1 2 3 4) => 10"
  (ok (= 10 (clysm/tests:compile-and-run '(+ 1 2 3 4)))
      "(+ 1 2 3 4) should equal 10"))

(deftest test-add-negative-fixnums
  "Negative addition: (+ -5 3) => -2"
  (ok (= -2 (clysm/tests:compile-and-run '(+ -5 3)))
      "(+ -5 3) should equal -2"))

(deftest test-subtract-two-fixnums
  "Basic subtraction: (- 5 3) => 2"
  (ok (= 2 (clysm/tests:compile-and-run '(- 5 3)))
      "(- 5 3) should equal 2"))

(deftest test-subtract-negative-result
  "Subtraction with negative result: (- 3 5) => -2"
  (ok (= -2 (clysm/tests:compile-and-run '(- 3 5)))
      "(- 3 5) should equal -2"))

(deftest test-unary-minus
  "Unary minus: (- 5) => -5"
  (ok (= -5 (clysm/tests:compile-and-run '(- 5)))
      "(- 5) should equal -5"))

(deftest test-multiply-two-fixnums
  "Basic multiplication: (* 3 4) => 12"
  (ok (= 12 (clysm/tests:compile-and-run '(* 3 4)))
      "(* 3 4) should equal 12"))

(deftest test-multiply-by-zero
  "Multiply by zero: (* 5 0) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(* 5 0)))
      "(* 5 0) should equal 0"))

(deftest test-multiply-negatives
  "Multiply negatives: (* -3 -4) => 12"
  (ok (= 12 (clysm/tests:compile-and-run '(* -3 -4)))
      "(* -3 -4) should equal 12"))

(deftest test-divide-fixnums
  "Integer division: (/ 10 2) => 5"
  (ok (= 5 (clysm/tests:compile-and-run '(/ 10 2)))
      "(/ 10 2) should equal 5"))

(deftest test-divide-with-truncation
  "Division with truncation: (/ 10 3) => 3 (truncated)"
  (ok (= 3 (clysm/tests:compile-and-run '(truncate 10 3)))
      "(truncate 10 3) should equal 3"))

(deftest test-nested-arithmetic
  "Nested arithmetic: (* (+ 1 2) (- 5 3)) => 6"
  (ok (= 6 (clysm/tests:compile-and-run '(* (+ 1 2) (- 5 3))))
      "(* (+ 1 2) (- 5 3)) should equal 6"))

(deftest test-add-zero-args
  "Empty addition: (+) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(+)))
      "(+) should equal 0 (identity)"))

(deftest test-multiply-zero-args
  "Empty multiplication: (*) => 1"
  (ok (= 1 (clysm/tests:compile-and-run '(*)))
      "(*) should equal 1 (identity)"))

;;; ============================================================
;;; T029: Comparison Operator Tests
;;; ============================================================

(deftest test-less-than-true
  "Less than true: (< 1 2) => T"
  (ok (clysm/tests:compile-and-run '(< 1 2))
      "(< 1 2) should be true"))

(deftest test-less-than-false
  "Less than false: (< 2 1) => NIL"
  (ok (null (clysm/tests:compile-and-run '(< 2 1)))
      "(< 2 1) should be false"))

(deftest test-greater-than-true
  "Greater than true: (> 2 1) => T"
  (ok (clysm/tests:compile-and-run '(> 2 1))
      "(> 2 1) should be true"))

(deftest test-less-or-equal-true
  "Less or equal true: (<= 2 2) => T"
  (ok (clysm/tests:compile-and-run '(<= 2 2))
      "(<= 2 2) should be true"))

(deftest test-greater-or-equal-true
  "Greater or equal true: (>= 2 2) => T"
  (ok (clysm/tests:compile-and-run '(>= 2 2))
      "(>= 2 2) should be true"))

(deftest test-numeric-equal-true
  "Numeric equal true: (= 5 5) => T"
  (ok (clysm/tests:compile-and-run '(= 5 5))
      "(= 5 5) should be true"))

(deftest test-numeric-equal-false
  "Numeric equal false: (= 5 6) => NIL"
  (ok (null (clysm/tests:compile-and-run '(= 5 6)))
      "(= 5 6) should be false"))

(deftest test-not-equal
  "Not equal: (/= 5 6) => T"
  (ok (clysm/tests:compile-and-run '(/= 5 6))
      "(/= 5 6) should be true"))

(deftest test-chained-comparison
  "Chained comparison: (< 1 2 3) => T"
  (ok (clysm/tests:compile-and-run '(< 1 2 3))
      "(< 1 2 3) should be true"))
