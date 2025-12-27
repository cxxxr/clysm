;;;; defstruct-test.lisp - TDD tests for interpreter defstruct
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T023

(defpackage #:clysm/tests/interpreter/defstruct-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/defstruct-test)

;;; ============================================================
;;; US4: defstruct with slot options
;;; ============================================================

(deftest test-defstruct-simple
  "Test simple defstruct with named slots."
  (let ((env (make-interpreter-env)))
    (interpret '(defstruct point x y) env)
    ;; Constructor should exist
    (let ((pt (interpret '(make-point :x 3 :y 4) env)))
      (ok pt)
      ;; Accessors should work
      (ok (= 3 (interpret `(point-x ',pt) env)))
      (ok (= 4 (interpret `(point-y ',pt) env))))))

(deftest test-defstruct-predicate
  "Test defstruct predicate."
  (let ((env (make-interpreter-env)))
    (interpret '(defstruct box width height) env)
    (let ((b (interpret '(make-box :width 10 :height 20) env)))
      (ok (interpret `(box-p ',b) env))
      (ok (not (interpret '(box-p 42) env))))))

(deftest test-defstruct-defaults
  "Test defstruct with default values."
  (let ((env (make-interpreter-env)))
    (interpret '(defstruct counter (value 0)) env)
    (let ((c (interpret '(make-counter) env)))
      (ok (= 0 (interpret `(counter-value ',c) env))))
    ;; Override default
    (let ((c2 (interpret '(make-counter :value 10) env)))
      (ok (= 10 (interpret `(counter-value ',c2) env))))))

(deftest test-defstruct-returns-name
  "Test that defstruct returns the structure name."
  (let ((env (make-interpreter-env)))
    (ok (eq 'my-struct (interpret '(defstruct my-struct slot1) env)))))

(deftest test-defstruct-multiple-slots
  "Test defstruct with multiple slots."
  (let ((env (make-interpreter-env)))
    (interpret '(defstruct person name age city) env)
    (let ((p (interpret '(make-person :name "Alice" :age 30 :city "NYC") env)))
      (ok (equal "Alice" (interpret `(person-name ',p) env)))
      (ok (= 30 (interpret `(person-age ',p) env)))
      (ok (equal "NYC" (interpret `(person-city ',p) env))))))
