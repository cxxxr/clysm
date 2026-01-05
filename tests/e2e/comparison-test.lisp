;;;; tests/e2e/comparison-test.lisp - E2E Tests for Comparison Operations
;;;;
;;;; Tests that comparison primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-comparison-suite "E2E tests for comparison operations")

;;; ============================================================
;;; Less Than Tests
;;; ============================================================

(deftest test-e2e-lt-true ()
  "Less than - true cases"
  (is-e2e "(< 1 2)" "T")
  (is-e2e "(< 0 1)" "T")
  (is-e2e "(< -1 0)" "T")
  (is-e2e "(< -10 10)" "T"))

(deftest test-e2e-lt-false ()
  "Less than - false cases"
  (is-e2e "(< 2 1)" "NIL")
  (is-e2e "(< 1 1)" "NIL")
  (is-e2e "(< 0 -1)" "NIL"))

;;; ============================================================
;;; Less Than or Equal Tests
;;; ============================================================

(deftest test-e2e-le-true ()
  "Less than or equal - true cases"
  (is-e2e "(<= 1 2)" "T")
  (is-e2e "(<= 2 2)" "T")
  (is-e2e "(<= -1 0)" "T")
  (is-e2e "(<= 0 0)" "T"))

(deftest test-e2e-le-false ()
  "Less than or equal - false cases"
  (is-e2e "(<= 3 2)" "NIL")
  (is-e2e "(<= 1 0)" "NIL"))

;;; ============================================================
;;; Greater Than Tests
;;; ============================================================

(deftest test-e2e-gt-true ()
  "Greater than - true cases"
  (is-e2e "(> 2 1)" "T")
  (is-e2e "(> 1 0)" "T")
  (is-e2e "(> 0 -1)" "T")
  (is-e2e "(> 10 -10)" "T"))

(deftest test-e2e-gt-false ()
  "Greater than - false cases"
  (is-e2e "(> 1 2)" "NIL")
  (is-e2e "(> 1 1)" "NIL")
  (is-e2e "(> -1 0)" "NIL"))

;;; ============================================================
;;; Greater Than or Equal Tests
;;; ============================================================

(deftest test-e2e-ge-true ()
  "Greater than or equal - true cases"
  (is-e2e "(>= 2 1)" "T")
  (is-e2e "(>= 2 2)" "T")
  (is-e2e "(>= 0 -1)" "T")
  (is-e2e "(>= 0 0)" "T"))

(deftest test-e2e-ge-false ()
  "Greater than or equal - false cases"
  (is-e2e "(>= 2 3)" "NIL")
  (is-e2e "(>= -1 0)" "NIL"))

;;; ============================================================
;;; Numeric Equality Tests
;;; ============================================================

(deftest test-e2e-num-eq-true ()
  "Numeric equality - true cases"
  (is-e2e "(= 5 5)" "T")
  (is-e2e "(= 0 0)" "T")
  (is-e2e "(= -42 -42)" "T"))

(deftest test-e2e-num-eq-false ()
  "Numeric equality - false cases"
  (is-e2e "(= 5 6)" "NIL")
  (is-e2e "(= 0 1)" "NIL")
  (is-e2e "(= -1 1)" "NIL"))

;;; ============================================================
;;; Numeric Inequality Tests
;;; ============================================================

(deftest test-e2e-num-neq-true ()
  "Numeric inequality - true cases"
  (is-e2e "(/= 5 6)" "T")
  (is-e2e "(/= 0 1)" "T")
  (is-e2e "(/= -1 1)" "T"))

(deftest test-e2e-num-neq-false ()
  "Numeric inequality - false cases"
  (is-e2e "(/= 5 5)" "NIL")
  (is-e2e "(/= 0 0)" "NIL"))

;;; ============================================================
;;; Zero/Positive/Negative Predicates
;;; ============================================================

(deftest test-e2e-zerop-true ()
  "zerop - true case"
  (is-e2e "(zerop 0)" "T"))

(deftest test-e2e-zerop-false ()
  "zerop - false cases"
  (is-e2e "(zerop 1)" "NIL")
  (is-e2e "(zerop -1)" "NIL")
  (is-e2e "(zerop 42)" "NIL"))

(deftest test-e2e-plusp-true ()
  "plusp - true cases"
  (is-e2e "(plusp 1)" "T")
  (is-e2e "(plusp 42)" "T")
  (is-e2e "(plusp 100)" "T"))

(deftest test-e2e-plusp-false ()
  "plusp - false cases"
  (is-e2e "(plusp 0)" "NIL")
  (is-e2e "(plusp -1)" "NIL")
  (is-e2e "(plusp -42)" "NIL"))

(deftest test-e2e-minusp-true ()
  "minusp - true cases"
  (is-e2e "(minusp -1)" "T")
  (is-e2e "(minusp -42)" "T")
  (is-e2e "(minusp -100)" "T"))

(deftest test-e2e-minusp-false ()
  "minusp - false cases"
  (is-e2e "(minusp 0)" "NIL")
  (is-e2e "(minusp 1)" "NIL")
  (is-e2e "(minusp 42)" "NIL"))

;;; ============================================================
;;; Comparison with Computed Values
;;; ============================================================

(deftest test-e2e-compare-computed ()
  "Comparisons with computed operands"
  (is-e2e "(< (+ 1 2) (+ 3 4))" "T")
  (is-e2e "(= (* 2 3) (+ 4 2))" "T")
  (is-e2e "(> (* 5 5) (* 4 4))" "T"))
