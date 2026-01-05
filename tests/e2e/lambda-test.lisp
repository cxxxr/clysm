;;;; tests/e2e/lambda-test.lisp - E2E Tests for Lambda Expressions
;;;;
;;;; Tests that LAMBDA works correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-lambda-suite "E2E tests for lambda expressions")

;;; ============================================================
;;; Immediate Lambda Application
;;; ============================================================

(deftest test-e2e-lambda-identity ()
  "Identity lambda"
  (is-e2e "((lambda (x) x) 42)" "42"))

(deftest test-e2e-lambda-constant ()
  "Lambda returning constant"
  (is-e2e "((lambda () 99))" "99"))

(deftest test-e2e-lambda-two-args ()
  "Lambda with two arguments"
  (is-e2e "((lambda (x y) (+ x y)) 3 4)" "7"))

(deftest test-e2e-lambda-three-args ()
  "Lambda with three arguments"
  (is-e2e "((lambda (a b c) (+ a (+ b c))) 1 2 3)" "6"))

;;; ============================================================
;;; Lambda in Let
;;; ============================================================

(deftest test-e2e-lambda-in-let ()
  "Lambda bound to variable"
  (is-e2e "(let ((f (lambda (x) (* x 2))))
            (f 5))"
          "10"))

(deftest test-e2e-lambda-multiple-in-let ()
  "Multiple lambdas in let"
  (is-e2e "(let ((inc (lambda (x) (+ x 1)))
                (dec (lambda (x) (- x 1))))
            (inc (dec 10)))"
          "10"))

;;; ============================================================
;;; Lambda as Arguments
;;; ============================================================

(deftest test-e2e-lambda-as-arg ()
  "Lambda passed as argument"
  (is-e2e-forms "(defun apply1 (f x) (f x))
                 (apply1 (lambda (n) (* n 3)) 7)"
                "21"))

(deftest test-e2e-lambda-to-lambda ()
  "Lambda passed to lambda"
  (is-e2e "((lambda (f) (f 5)) (lambda (x) (* x 2)))" "10"))

;;; ============================================================
;;; Nested Lambda
;;; ============================================================

(deftest test-e2e-lambda-nested ()
  "Nested lambda expressions"
  (is-e2e "((lambda (x) ((lambda (y) (+ x y)) 10)) 5)" "15"))

(deftest test-e2e-lambda-curried ()
  "Curried function pattern"
  (is-e2e "(((lambda (x) (lambda (y) (+ x y))) 3) 4)" "7"))

;;; ============================================================
;;; Lambda with Complex Body
;;; ============================================================

(deftest test-e2e-lambda-with-if ()
  "Lambda with conditional"
  (is-e2e "((lambda (x) (if (> x 0) 'positive 'non-positive)) 5)" "POSITIVE")
  (is-e2e "((lambda (x) (if (> x 0) 'positive 'non-positive)) -5)" "NON-POSITIVE"))

(deftest test-e2e-lambda-with-let ()
  "Lambda with let"
  (is-e2e "((lambda (x) (let ((y (* x 2))) (+ x y))) 3)" "9"))

(deftest test-e2e-lambda-multi-body ()
  "Lambda with multiple body forms"
  (is-e2e "((lambda (x) 1 2 x) 42)" "42"))

;;; ============================================================
;;; Lambda Returning Lambda
;;; ============================================================

(deftest test-e2e-lambda-returns-lambda ()
  "Lambda returning lambda"
  (is-e2e "(let ((make-adder (lambda (n) (lambda (x) (+ x n)))))
            ((make-adder 10) 5))"
          "15"))
