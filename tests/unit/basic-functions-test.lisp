;;;; tests/unit/basic-functions-test.lisp
;;;; Unit tests for basic arithmetic functions (001-numeric-functions US1)
;;;;
;;;; Tests for signum, max, min
;;;; Note: abs, gcd, lcm already have tests in math-functions-test.lisp

(in-package #:clysm/tests/unit/basic-functions)

;;; ============================================================
;;; Signum Tests (FR-002)
;;; ============================================================

(deftest test-signum-negative
  "signum of negative: (signum -5) => -1"
  (ok (= -1 (compile-and-run-numeric '(signum -5)))
      "(signum -5) should equal -1"))

(deftest test-signum-zero
  "signum of zero: (signum 0) => 0"
  (ok (= 0 (compile-and-run-numeric '(signum 0)))
      "(signum 0) should equal 0"))

(deftest test-signum-positive
  "signum of positive: (signum 5) => 1"
  (ok (= 1 (compile-and-run-numeric '(signum 5)))
      "(signum 5) should equal 1"))

(deftest test-signum-negative-float
  "signum of negative float: (signum -3.14) => -1.0"
  (ok (= -1.0 (compile-and-run-numeric '(signum -3.14)))
      "(signum -3.14) should equal -1.0"))

(deftest test-signum-positive-float
  "signum of positive float: (signum 3.14) => 1.0"
  (ok (= 1.0 (compile-and-run-numeric '(signum 3.14)))
      "(signum 3.14) should equal 1.0"))

(deftest test-signum-negative-ratio
  "signum of negative ratio: (signum -3/4) => -1"
  (ok (= -1 (compile-and-run-numeric '(signum -3/4)))
      "(signum -3/4) should equal -1"))

;;; ============================================================
;;; Max Tests (FR-003)
;;; ============================================================

(deftest test-max-two-args
  "max of two: (max 3 5) => 5"
  (ok (= 5 (compile-and-run-numeric '(max 3 5)))
      "(max 3 5) should equal 5"))

(deftest test-max-multiple-args
  "max of multiple: (max 1 2 3) => 3"
  (ok (= 3 (compile-and-run-numeric '(max 1 2 3)))
      "(max 1 2 3) should equal 3"))

(deftest test-max-with-negative
  "max with negatives: (max -5 -2 -10) => -2"
  (ok (= -2 (compile-and-run-numeric '(max -5 -2 -10)))
      "(max -5 -2 -10) should equal -2"))

(deftest test-max-single-arg
  "max single arg: (max 42) => 42"
  (ok (= 42 (compile-and-run-numeric '(max 42)))
      "(max 42) should equal 42"))

(deftest test-max-floats
  "max of floats: (max 1.5 2.5 0.5) => 2.5"
  (ok (= 2.5 (compile-and-run-numeric '(max 1.5 2.5 0.5)))
      "(max 1.5 2.5 0.5) should equal 2.5"))

(deftest test-max-mixed-types
  "max of mixed types: (max 1 2.5 3/2) => 2.5"
  (ok (= 2.5 (compile-and-run-numeric '(max 1 2.5 3/2)))
      "(max 1 2.5 3/2) should equal 2.5"))

;;; ============================================================
;;; Min Tests (FR-004)
;;; ============================================================

(deftest test-min-two-args
  "min of two: (min 3 5) => 3"
  (ok (= 3 (compile-and-run-numeric '(min 3 5)))
      "(min 3 5) should equal 3"))

(deftest test-min-multiple-args
  "min of multiple: (min 1 2 3) => 1"
  (ok (= 1 (compile-and-run-numeric '(min 1 2 3)))
      "(min 1 2 3) should equal 1"))

(deftest test-min-with-negative
  "min with negatives: (min -5 -2 -10) => -10"
  (ok (= -10 (compile-and-run-numeric '(min -5 -2 -10)))
      "(min -5 -2 -10) should equal -10"))

(deftest test-min-single-arg
  "min single arg: (min 42) => 42"
  (ok (= 42 (compile-and-run-numeric '(min 42)))
      "(min 42) should equal 42"))

(deftest test-min-floats
  "min of floats: (min 1.5 2.5 0.5) => 0.5"
  (ok (= 0.5 (compile-and-run-numeric '(min 1.5 2.5 0.5)))
      "(min 1.5 2.5 0.5) should equal 0.5"))

(deftest test-min-mixed-types
  "min of mixed types: (min 1 2.5 3/2) => 1"
  (ok (= 1 (compile-and-run-numeric '(min 1 2.5 3/2)))
      "(min 1 2.5 3/2) should equal 1"))
