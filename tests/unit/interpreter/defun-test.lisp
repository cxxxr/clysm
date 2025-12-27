;;;; defun-test.lisp - TDD tests for interpreter defun with full lambda-list
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T021

(defpackage #:clysm/tests/interpreter/defun-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:get-default-env
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/defun-test)

;;; ============================================================
;;; US4: defun with full lambda-list support
;;; ============================================================

(deftest test-defun-simple
  "Test simple defun with required parameters."
  (let ((env (make-interpreter-env)))
    ;; Define a simple function
    (interpret '(defun add (a b) (+ a b)) env)
    ;; Call it
    (ok (= 5 (interpret '(add 2 3) env)))))

(deftest test-defun-no-params
  "Test defun with no parameters."
  (let ((env (make-interpreter-env)))
    (interpret '(defun answer () 42) env)
    (ok (= 42 (interpret '(answer) env)))))

(deftest test-defun-optional
  "Test defun with &optional parameters."
  (let ((env (make-interpreter-env)))
    (interpret '(defun greet (name &optional (greeting "Hello"))
                  (list greeting name))
               env)
    ;; With optional provided
    (ok (equal '("Hi" "World")
               (interpret '(greet "World" "Hi") env)))
    ;; Without optional
    (ok (equal '("Hello" "Alice")
               (interpret '(greet "Alice") env)))))

(deftest test-defun-optional-supplied-p
  "Test defun with &optional and supplied-p parameter."
  (let ((env (make-interpreter-env)))
    (interpret '(defun check-opt (x &optional (y 10 y-supplied-p))
                  (list x y y-supplied-p))
               env)
    ;; Supplied
    (ok (equal '(1 20 t)
               (interpret '(check-opt 1 20) env)))
    ;; Not supplied
    (ok (equal '(1 10 nil)
               (interpret '(check-opt 1) env)))))

(deftest test-defun-rest
  "Test defun with &rest parameter."
  (let ((env (make-interpreter-env)))
    (interpret '(defun sum-all (first &rest rest)
                  (apply #'+ first rest))
               env)
    (ok (= 15 (interpret '(sum-all 1 2 3 4 5) env)))
    (ok (= 1 (interpret '(sum-all 1) env)))))

(deftest test-defun-key
  "Test defun with &key parameters."
  (let ((env (make-interpreter-env)))
    (interpret '(defun make-point (&key (x 0) (y 0))
                  (list x y))
               env)
    (ok (equal '(0 0) (interpret '(make-point) env)))
    (ok (equal '(10 0) (interpret '(make-point :x 10) env)))
    (ok (equal '(10 20) (interpret '(make-point :x 10 :y 20) env)))
    (ok (equal '(5 15) (interpret '(make-point :y 15 :x 5) env)))))

(deftest test-defun-key-supplied-p
  "Test defun with &key and supplied-p parameter."
  (let ((env (make-interpreter-env)))
    (interpret '(defun check-key (&key (value 0 value-p))
                  (list value value-p))
               env)
    (ok (equal '(0 nil) (interpret '(check-key) env)))
    (ok (equal '(42 t) (interpret '(check-key :value 42) env)))))

(deftest test-defun-aux
  "Test defun with &aux parameters."
  (let ((env (make-interpreter-env)))
    (interpret '(defun with-aux (x &aux (y (* x 2)))
                  (list x y))
               env)
    (ok (equal '(5 10) (interpret '(with-aux 5) env)))))

(deftest test-defun-full-lambda-list
  "Test defun with all lambda-list features."
  (let ((env (make-interpreter-env)))
    (interpret '(defun complex-fn (req1 req2
                                   &optional (opt1 "default")
                                   &rest rest
                                   &key (key1 :default)
                                   &aux (aux1 42))
                  (list req1 req2 opt1 rest key1 aux1))
               env)
    ;; Minimal call
    (ok (equal '(1 2 "default" nil :default 42)
               (interpret '(complex-fn 1 2) env)))
    ;; With optional
    (ok (equal '(1 2 "custom" nil :default 42)
               (interpret '(complex-fn 1 2 "custom") env)))))

(deftest test-defun-returns-name
  "Test that defun returns the function name."
  (let ((env (make-interpreter-env)))
    (ok (eq 'my-func (interpret '(defun my-func () nil) env)))))

(deftest test-defun-recursive
  "Test recursive defun."
  (let ((env (make-interpreter-env)))
    (interpret '(defun factorial (n)
                  (if (<= n 1)
                      1
                      (* n (factorial (- n 1)))))
               env)
    (ok (= 1 (interpret '(factorial 0) env)))
    (ok (= 1 (interpret '(factorial 1) env)))
    (ok (= 120 (interpret '(factorial 5) env)))))
