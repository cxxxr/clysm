;;;; tests/e2e/defun-test.lisp - E2E Tests for Function Definitions
;;;;
;;;; Tests that DEFUN works correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-defun-suite "E2E tests for function definitions")

;;; ============================================================
;;; Basic DEFUN Tests
;;; ============================================================

(deftest test-e2e-defun-identity ()
  "Identity function"
  (is-e2e-forms "(defun identity (x) x)
                 (identity 42)"
                "42"))

(deftest test-e2e-defun-constant ()
  "Function returning constant"
  (is-e2e-forms "(defun always-42 () 42)
                 (always-42)"
                "42"))

(deftest test-e2e-defun-two-args ()
  "Function with two arguments"
  (is-e2e-forms "(defun add (x y) (+ x y))
                 (add 3 4)"
                "7"))

(deftest test-e2e-defun-three-args ()
  "Function with three arguments"
  (is-e2e-forms "(defun sum3 (a b c) (+ a (+ b c)))
                 (sum3 1 2 3)"
                "6"))

;;; ============================================================
;;; DEFUN with Body Forms
;;; ============================================================

(deftest test-e2e-defun-multi-body ()
  "Function with multiple body forms"
  (is-e2e-forms "(defun multi-body (x)
                   (setq x (+ x 1))
                   (setq x (+ x 1))
                   x)
                 (multi-body 0)"
                "2"))

(deftest test-e2e-defun-with-let ()
  "Function using let"
  (is-e2e-forms "(defun with-let (x)
                   (let ((y (* x 2)))
                     (+ x y)))
                 (with-let 5)"
                "15"))

;;; ============================================================
;;; Recursive Functions
;;; ============================================================

(deftest test-e2e-defun-factorial ()
  "Factorial function"
  (is-e2e-forms "(defun fact (n)
                   (if (zerop n)
                       1
                       (* n (fact (- n 1)))))
                 (fact 5)"
                "120"))

(deftest test-e2e-defun-fibonacci ()
  "Fibonacci function"
  (is-e2e-forms "(defun fib (n)
                   (if (< n 2)
                       n
                       (+ (fib (- n 1)) (fib (- n 2)))))
                 (fib 10)"
                "55"))

(deftest test-e2e-defun-countdown ()
  "Countdown recursion"
  (is-e2e-forms "(defun countdown (n)
                   (if (zerop n)
                       0
                       (countdown (- n 1))))
                 (countdown 100)"
                "0"))

;;; ============================================================
;;; Multiple Function Definitions
;;; ============================================================

(deftest test-e2e-defun-multiple ()
  "Multiple function definitions"
  (is-e2e-forms "(defun double (x) (* x 2))
                 (defun triple (x) (* x 3))
                 (+ (double 5) (triple 5))"
                "25"))

(deftest test-e2e-defun-calling-each-other ()
  "Functions calling each other"
  (is-e2e-forms "(defun inc (x) (+ x 1))
                 (defun double-inc (x) (inc (inc x)))
                 (double-inc 5)"
                "7"))

;;; ============================================================
;;; Higher-Order Function Patterns
;;; ============================================================

(deftest test-e2e-defun-apply-fn ()
  "Function taking function as argument"
  (is-e2e-forms "(defun apply1 (f x) (f x))
                 (defun double (n) (* n 2))
                 (apply1 double 5)"
                "10"))

(deftest test-e2e-defun-compose ()
  "Function composition pattern"
  (is-e2e-forms "(defun inc (x) (+ x 1))
                 (defun double (x) (* x 2))
                 (defun compose (f g x) (f (g x)))
                 (compose inc double 5)"
                "11"))
