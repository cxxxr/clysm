;;;; tests/e2e/if-test.lisp - E2E Tests for Conditional Expressions
;;;;
;;;; Tests that IF special form works correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-if-suite "E2E tests for conditional expressions")

;;; ============================================================
;;; Basic IF Tests
;;; ============================================================

(deftest test-e2e-if-true-branch ()
  "IF with true condition takes then-branch"
  (is-e2e "(if t 1 2)" "1")
  (is-e2e "(if 'foo 'yes 'no)" "YES")
  (is-e2e "(if 42 'truthy 'falsy)" "TRUTHY"))

(deftest test-e2e-if-false-branch ()
  "IF with false condition takes else-branch"
  (is-e2e "(if nil 1 2)" "2")
  (is-e2e "(if '() 'yes 'no)" "NO"))

(deftest test-e2e-if-no-else ()
  "IF without else returns NIL when false"
  (is-e2e "(if nil 42)" "NIL")
  (is-e2e "(if '() 'yes)" "NIL"))

(deftest test-e2e-if-no-else-true ()
  "IF without else returns then-value when true"
  (is-e2e "(if t 42)" "42")
  (is-e2e "(if 1 'yes)" "YES"))

;;; ============================================================
;;; IF with Computed Conditions
;;; ============================================================

(deftest test-e2e-if-comparison ()
  "IF with comparison as condition"
  (is-e2e "(if (< 1 2) 'less 'greater)" "LESS")
  (is-e2e "(if (> 1 2) 'less 'greater)" "GREATER")
  (is-e2e "(if (= 5 5) 'equal 'not-equal)" "EQUAL"))

(deftest test-e2e-if-zerop ()
  "IF with zerop condition"
  (is-e2e "(if (zerop 0) 'zero 'non-zero)" "ZERO")
  (is-e2e "(if (zerop 1) 'zero 'non-zero)" "NON-ZERO"))

(deftest test-e2e-if-null ()
  "IF with null condition"
  (is-e2e "(if (null nil) 'empty 'not-empty)" "EMPTY")
  (is-e2e "(if (null '(a)) 'empty 'not-empty)" "NOT-EMPTY"))

;;; ============================================================
;;; Nested IF Tests
;;; ============================================================

(deftest test-e2e-if-nested-then ()
  "Nested IF in then-branch"
  (is-e2e "(if t (if t 1 2) 3)" "1")
  (is-e2e "(if t (if nil 1 2) 3)" "2"))

(deftest test-e2e-if-nested-else ()
  "Nested IF in else-branch"
  (is-e2e "(if nil 1 (if t 2 3))" "2")
  (is-e2e "(if nil 1 (if nil 2 3))" "3"))

(deftest test-e2e-if-deeply-nested ()
  "Deeply nested IF"
  (is-e2e "(if t (if t (if t 'deep 'c) 'b) 'a)" "DEEP"))

;;; ============================================================
;;; IF with Complex Branches
;;; ============================================================

(deftest test-e2e-if-arithmetic-branches ()
  "IF with arithmetic in branches"
  (is-e2e "(if (zerop 0) (+ 1 2) (* 3 4))" "3")
  (is-e2e "(if (zerop 1) (+ 1 2) (* 3 4))" "12"))

(deftest test-e2e-if-cons-branches ()
  "IF with cons operations in branches"
  (is-e2e "(if t (cons 1 2) (cons 3 4))" "(1 . 2)")
  (is-e2e "(if nil (cons 1 2) (cons 3 4))" "(3 . 4)"))

;;; ============================================================
;;; IF with PROGN
;;; ============================================================

(deftest test-e2e-if-progn-then ()
  "IF with progn in then-branch"
  (is-e2e "(if t (progn 1 2 3) 4)" "3"))

(deftest test-e2e-if-progn-else ()
  "IF with progn in else-branch"
  (is-e2e "(if nil 1 (progn 2 3 4))" "4"))

;;; ============================================================
;;; PROGN Tests
;;; ============================================================

(deftest test-e2e-progn-single ()
  "PROGN with single form"
  (is-e2e "(progn 42)" "42"))

(deftest test-e2e-progn-multiple ()
  "PROGN with multiple forms (returns last)"
  (is-e2e "(progn 1 2 3)" "3")
  (is-e2e "(progn 'a 'b 'c)" "C"))

(deftest test-e2e-progn-side-effects ()
  "PROGN evaluates all forms"
  (is-e2e "(let ((x 0)) (progn (setq x 1) (setq x 2) x))" "2"))

(deftest test-e2e-progn-empty ()
  "Empty PROGN returns NIL"
  (is-e2e "(progn)" "NIL"))
