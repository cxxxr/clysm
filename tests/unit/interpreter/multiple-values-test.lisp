;;;; multiple-values-test.lisp - TDD tests for interpreter multiple values
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T028

(defpackage #:clysm/tests/interpreter/multiple-values-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/multiple-values-test)

;;; ============================================================
;;; US4: Multiple values support
;;; ============================================================

(deftest test-values-single
  "Test values with single value."
  (let ((env (make-interpreter-env)))
    (ok (= 42 (interpret '(values 42) env)))))

(deftest test-values-multiple
  "Test values with multiple values."
  (let ((env (make-interpreter-env)))
    ;; Primary value should be 1
    (ok (= 1 (interpret '(values 1 2 3) env)))))

(deftest test-values-empty
  "Test values with no values."
  (let ((env (make-interpreter-env)))
    (ok (eq nil (interpret '(values) env)))))

(deftest test-multiple-value-bind
  "Test multiple-value-bind."
  (let ((env (make-interpreter-env)))
    (ok (= 6 (interpret
              '(multiple-value-bind (a b c) (values 1 2 3)
                 (+ a b c))
              env)))))

(deftest test-multiple-value-bind-fewer-values
  "Test multiple-value-bind with fewer values than vars."
  (let ((env (make-interpreter-env)))
    ;; c should be nil when fewer values returned
    (ok (eq nil (interpret
                 '(multiple-value-bind (a b c) (values 1 2)
                    c)
                 env)))))

(deftest test-multiple-value-list
  "Test multiple-value-list."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 2 3)
               (interpret '(multiple-value-list (values 1 2 3)) env)))
    (ok (equal '()
               (interpret '(multiple-value-list (values)) env)))))

(deftest test-multiple-value-call
  "Test multiple-value-call."
  (let ((env (make-interpreter-env)))
    (ok (= 6 (interpret
              '(multiple-value-call #'+ (values 1 2 3))
              env)))))

(deftest test-multiple-value-prog1
  "Test multiple-value-prog1."
  (let ((env (make-interpreter-env)))
    ;; Returns primary value of first form
    (ok (= 1 (interpret
              '(multiple-value-prog1
                   (values 1 2 3)
                 999)
              env)))))

(deftest test-values-list
  "Test values-list."
  (let ((env (make-interpreter-env)))
    ;; Primary value
    (ok (= 1 (interpret '(values-list '(1 2 3)) env)))))

(deftest test-nth-value
  "Test nth-value."
  (let ((env (make-interpreter-env)))
    (ok (= 1 (interpret '(nth-value 0 (values 1 2 3)) env)))
    (ok (= 2 (interpret '(nth-value 1 (values 1 2 3)) env)))
    (ok (= 3 (interpret '(nth-value 2 (values 1 2 3)) env)))
    ;; Out of range returns nil
    (ok (eq nil (interpret '(nth-value 5 (values 1 2 3)) env)))))

(deftest test-floor-multiple-values
  "Test that floor returns multiple values."
  (let ((env (make-interpreter-env)))
    ;; floor returns (quotient, remainder)
    (ok (equal '(3 2)
               (interpret '(multiple-value-list (floor 11 3)) env)))))
