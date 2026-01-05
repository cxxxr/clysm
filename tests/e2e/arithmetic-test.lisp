;;;; tests/e2e/arithmetic-test.lisp - E2E Tests for Arithmetic Operations
;;;;
;;;; Tests that arithmetic primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-arithmetic-suite "E2E tests for arithmetic operations")

;;; ============================================================
;;; Addition Tests
;;; ============================================================

(deftest test-e2e-add-simple ()
  "Simple addition"
  (is-e2e "(+ 1 2)" "3")
  (is-e2e "(+ 0 0)" "0")
  (is-e2e "(+ 10 20)" "30"))

(deftest test-e2e-add-with-zero ()
  "Addition with zero (identity)"
  (is-e2e "(+ 42 0)" "42")
  (is-e2e "(+ 0 42)" "42"))

(deftest test-e2e-add-negatives ()
  "Addition with negative numbers"
  (is-e2e "(+ -1 -2)" "-3")
  (is-e2e "(+ 5 -3)" "2")
  (is-e2e "(+ -5 3)" "-2")
  (is-e2e "(+ -10 10)" "0"))

(deftest test-e2e-add-nested ()
  "Nested addition"
  (is-e2e "(+ (+ 1 2) (+ 3 4))" "10")
  (is-e2e "(+ (+ (+ 1 2) 3) 4)" "10")
  (is-e2e "(+ 1 (+ 2 (+ 3 4)))" "10"))

;;; ============================================================
;;; Subtraction Tests
;;; ============================================================

(deftest test-e2e-sub-simple ()
  "Simple subtraction"
  (is-e2e "(- 5 3)" "2")
  (is-e2e "(- 10 4)" "6")
  (is-e2e "(- 100 50)" "50"))

(deftest test-e2e-sub-negative-result ()
  "Subtraction resulting in negative"
  (is-e2e "(- 3 5)" "-2")
  (is-e2e "(- 0 10)" "-10"))

(deftest test-e2e-sub-with-zero ()
  "Subtraction with zero"
  (is-e2e "(- 42 0)" "42")
  (is-e2e "(- 0 42)" "-42"))

(deftest test-e2e-sub-same ()
  "Subtracting same number"
  (is-e2e "(- 42 42)" "0")
  (is-e2e "(- -5 -5)" "0"))

(deftest test-e2e-sub-negatives ()
  "Subtraction with negatives"
  (is-e2e "(- -3 -5)" "2")
  (is-e2e "(- -5 -3)" "-2")
  (is-e2e "(- 5 -3)" "8"))

;;; ============================================================
;;; Multiplication Tests
;;; ============================================================

(deftest test-e2e-mul-simple ()
  "Simple multiplication"
  (is-e2e "(* 3 4)" "12")
  (is-e2e "(* 2 5)" "10")
  (is-e2e "(* 7 8)" "56"))

(deftest test-e2e-mul-with-zero ()
  "Multiplication with zero"
  (is-e2e "(* 0 100)" "0")
  (is-e2e "(* 100 0)" "0")
  (is-e2e "(* 0 0)" "0"))

(deftest test-e2e-mul-with-one ()
  "Multiplication with one (identity)"
  (is-e2e "(* 1 42)" "42")
  (is-e2e "(* 42 1)" "42"))

(deftest test-e2e-mul-negatives ()
  "Multiplication with negatives"
  (is-e2e "(* -3 4)" "-12")
  (is-e2e "(* 3 -4)" "-12")
  (is-e2e "(* -3 -4)" "12"))

(deftest test-e2e-mul-nested ()
  "Nested multiplication"
  (is-e2e "(* (* 2 3) (* 4 5))" "120")
  (is-e2e "(* 2 (* 3 (* 4 5)))" "120"))

;;; ============================================================
;;; Division Tests
;;; ============================================================

(deftest test-e2e-truncate-simple ()
  "Simple truncating division"
  (is-e2e "(truncate 10 2)" "5")
  (is-e2e "(truncate 9 3)" "3")
  (is-e2e "(truncate 100 10)" "10"))

(deftest test-e2e-truncate-remainder ()
  "Division with remainder (truncates toward zero)"
  (is-e2e "(truncate 10 3)" "3")
  (is-e2e "(truncate 7 2)" "3")
  (is-e2e "(truncate 11 4)" "2"))

(deftest test-e2e-truncate-negative ()
  "Division with negative numbers"
  (is-e2e "(truncate -10 3)" "-3")
  (is-e2e "(truncate 10 -3)" "-3")
  (is-e2e "(truncate -10 -3)" "3"))

(deftest test-e2e-truncate-by-one ()
  "Division by one"
  (is-e2e "(truncate 42 1)" "42")
  (is-e2e "(truncate -42 1)" "-42"))

;;; ============================================================
;;; Remainder Tests
;;; ============================================================

(deftest test-e2e-rem-simple ()
  "Simple remainder"
  (is-e2e "(rem 10 3)" "1")
  (is-e2e "(rem 7 2)" "1")
  (is-e2e "(rem 9 3)" "0"))

(deftest test-e2e-rem-zero-result ()
  "Remainder is zero"
  (is-e2e "(rem 10 5)" "0")
  (is-e2e "(rem 12 4)" "0"))

(deftest test-e2e-rem-negative ()
  "Remainder with negatives"
  (is-e2e "(rem -10 3)" "-1")
  (is-e2e "(rem 10 -3)" "1"))

;;; ============================================================
;;; Complex Expressions
;;; ============================================================

(deftest test-e2e-complex-arithmetic ()
  "Complex arithmetic expressions"
  (is-e2e "(+ (* 2 3) (* 4 5))" "26")
  (is-e2e "(- (* 10 5) (+ 20 10))" "20")
  (is-e2e "(* (+ 1 2) (- 10 5))" "15"))

(deftest test-e2e-deeply-nested ()
  "Deeply nested arithmetic"
  (is-e2e "(+ 1 (+ 2 (+ 3 (+ 4 5))))" "15")
  (is-e2e "(* 2 (* 2 (* 2 (* 2 2))))" "32"))
