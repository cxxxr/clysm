;;;; defmacro-test.lisp - TDD tests for interpreter defmacro
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T022

(defpackage #:clysm/tests/interpreter/defmacro-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/defmacro-test)

;;; ============================================================
;;; US4: defmacro with &whole, &environment, &body
;;; ============================================================

(deftest test-defmacro-simple
  "Test simple defmacro."
  (let ((env (make-interpreter-env)))
    (interpret '(defmacro my-when (test &body body)
                  (list 'if test (cons 'progn body)))
               env)
    ;; Use the macro
    (interpret '(defun test-it (x)
                  (my-when (> x 0)
                    (+ x 1)))
               env)
    (ok (= 6 (interpret '(test-it 5) env)))
    (ok (eq nil (interpret '(test-it -1) env)))))

(deftest test-defmacro-returns-name
  "Test that defmacro returns the macro name."
  (let ((env (make-interpreter-env)))
    (ok (eq 'my-macro (interpret '(defmacro my-macro () nil) env)))))

(deftest test-defmacro-whole
  "Test defmacro with &whole parameter."
  (let ((env (make-interpreter-env)))
    (interpret '(defmacro show-form (&whole form arg)
                  (list 'quote form))
               env)
    (ok (equal '(show-form 42)
               (interpret '(show-form 42) env)))))

(deftest test-defmacro-body
  "Test defmacro with &body parameter."
  (let ((env (make-interpreter-env)))
    (interpret '(defmacro my-progn (&body body)
                  (cons 'progn body))
               env)
    (ok (= 3 (interpret '(my-progn 1 2 3) env)))))

(deftest test-defmacro-backquote
  "Test defmacro with backquote expansion."
  (let ((env (make-interpreter-env)))
    (interpret '(defmacro double (x)
                  `(+ ,x ,x))
               env)
    (ok (= 10 (interpret '(double 5) env)))))

(deftest test-defmacro-nested-expansion
  "Test that macros expand recursively."
  (let ((env (make-interpreter-env)))
    (interpret '(defmacro inc (x) `(+ ,x 1)) env)
    (interpret '(defmacro double-inc (x) `(inc (inc ,x))) env)
    (ok (= 7 (interpret '(double-inc 5) env)))))
