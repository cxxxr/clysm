;;;; tests/e2e/closure-test.lisp - E2E Tests for Closures
;;;;
;;;; Tests that closures capture variables correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-closure-suite "E2E tests for closures")

;;; ============================================================
;;; Basic Closure Tests
;;; ============================================================

(deftest test-e2e-closure-capture-single ()
  "Closure captures single variable"
  (is-e2e-forms "(defun make-adder (n)
                   (lambda (x) (+ x n)))
                 ((make-adder 10) 5)"
                "15"))

(deftest test-e2e-closure-capture-multiple ()
  "Closure captures multiple variables"
  (is-e2e-forms "(defun make-linear (a b)
                   (lambda (x) (+ (* a x) b)))
                 ((make-linear 2 3) 5)"
                "13"))

(deftest test-e2e-closure-from-let ()
  "Closure from let binding"
  (is-e2e "(let ((n 100))
            ((lambda (x) (+ x n)) 5))"
          "105"))

;;; ============================================================
;;; Mutable Closure Tests
;;; ============================================================

(deftest test-e2e-closure-counter ()
  "Counter closure with mutation"
  (is-e2e-forms "(defun make-counter ()
                   (let ((count 0))
                     (lambda ()
                       (setq count (+ count 1))
                       count)))
                 (let ((c (make-counter)))
                   (c) (c) (c))"
                "3"))

(deftest test-e2e-closure-accumulator ()
  "Accumulator closure"
  (is-e2e-forms "(defun make-accumulator (init)
                   (let ((sum init))
                     (lambda (n)
                       (setq sum (+ sum n))
                       sum)))
                 (let ((acc (make-accumulator 0)))
                   (acc 10) (acc 20) (acc 30))"
                "60"))

;;; ============================================================
;;; Multiple Closures Sharing State
;;; ============================================================

(deftest test-e2e-closure-shared-env ()
  "Multiple closures sharing environment"
  (is-e2e "(let ((x 0))
            (let ((inc (lambda () (setq x (+ x 1)) x))
                  (get (lambda () x)))
              (inc) (inc) (get)))"
          "2"))

;;; ============================================================
;;; Nested Closures
;;; ============================================================

(deftest test-e2e-closure-nested ()
  "Nested closures"
  (is-e2e-forms "(defun make-multiplier-maker (base)
                   (lambda (factor)
                     (lambda (x) (* x (* base factor)))))
                 ((((make-multiplier-maker 2)) 3) 5)"
                "30"))

(deftest test-e2e-closure-deep-capture ()
  "Closure capturing from multiple levels"
  (is-e2e "(let ((a 1))
            (let ((b 2))
              (let ((c 3))
                ((lambda () (+ a (+ b c)))))))"
          "6"))

;;; ============================================================
;;; Closure with Control Flow
;;; ============================================================

(deftest test-e2e-closure-conditional ()
  "Closure with conditional"
  (is-e2e-forms "(defun make-threshold-checker (threshold)
                   (lambda (x)
                     (if (> x threshold) 'above 'below)))
                 ((make-threshold-checker 10) 15)"
                "ABOVE"))

(deftest test-e2e-closure-in-recursion ()
  "Closure used in recursive context"
  (is-e2e-forms "(defun make-factorial ()
                   (let ((fact nil))
                     (setq fact
                           (lambda (n)
                             (if (zerop n)
                                 1
                                 (* n (fact (- n 1))))))
                     fact))
                 ((make-factorial) 5)"
                "120"))

;;; ============================================================
;;; Closure Edge Cases
;;; ============================================================

(deftest test-e2e-closure-no-capture ()
  "Lambda that doesn't capture (not really a closure)"
  (is-e2e-forms "(defun make-const ()
                   (lambda (x) x))
                 ((make-const) 42)"
                "42"))

(deftest test-e2e-closure-parameter-shadow ()
  "Closure parameter shadows captured variable"
  (is-e2e "(let ((x 100))
            ((lambda (x) x) 42))"
          "42"))
