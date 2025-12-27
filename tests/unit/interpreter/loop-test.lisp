;;;; loop-test.lisp - TDD tests for interpreter LOOP macro
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T024

(defpackage #:clysm/tests/interpreter/loop-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/loop-test)

;;; ============================================================
;;; US4: LOOP macro with for/collect/do/when/while
;;; ============================================================

(deftest test-loop-for-from-to
  "Test LOOP for ... from ... to ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 2 3 4 5)
               (interpret '(loop for i from 1 to 5 collect i) env)))))

(deftest test-loop-for-below
  "Test LOOP for ... from ... below ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '(0 1 2 3 4)
               (interpret '(loop for i from 0 below 5 collect i) env)))))

(deftest test-loop-for-downto
  "Test LOOP for ... from ... downto ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '(5 4 3 2 1)
               (interpret '(loop for i from 5 downto 1 collect i) env)))))

(deftest test-loop-for-by
  "Test LOOP for ... by ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '(0 2 4 6 8)
               (interpret '(loop for i from 0 to 8 by 2 collect i) env)))))

(deftest test-loop-for-in
  "Test LOOP for ... in ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '(2 4 6)
               (interpret '(loop for x in '(1 2 3) collect (* x 2)) env)))))

(deftest test-loop-for-on
  "Test LOOP for ... on ..."
  (let ((env (make-interpreter-env)))
    (ok (equal '((1 2 3) (2 3) (3))
               (interpret '(loop for x on '(1 2 3) collect x) env)))))

(deftest test-loop-collect
  "Test LOOP collect."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 4 9 16 25)
               (interpret '(loop for i from 1 to 5 collect (* i i)) env)))))

(deftest test-loop-sum
  "Test LOOP sum."
  (let ((env (make-interpreter-env)))
    (ok (= 55 (interpret '(loop for i from 1 to 10 sum i) env)))))

(deftest test-loop-count
  "Test LOOP count."
  (let ((env (make-interpreter-env)))
    (ok (= 5 (interpret '(loop for i from 1 to 10 count (evenp i)) env)))))

(deftest test-loop-do
  "Test LOOP do."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *sum* 0) env)
    (interpret '(loop for i from 1 to 5 do (setq *sum* (+ *sum* i))) env)
    (ok (= 15 (interpret '*sum* env)))))

(deftest test-loop-when
  "Test LOOP when."
  (let ((env (make-interpreter-env)))
    (ok (equal '(2 4 6 8 10)
               (interpret '(loop for i from 1 to 10
                                 when (evenp i) collect i)
                          env)))))

(deftest test-loop-unless
  "Test LOOP unless."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 3 5 7 9)
               (interpret '(loop for i from 1 to 10
                                 unless (evenp i) collect i)
                          env)))))

(deftest test-loop-repeat
  "Test LOOP repeat."
  (let ((env (make-interpreter-env)))
    (ok (= 5 (interpret '(loop repeat 5 sum 1) env)))))

(deftest test-loop-while
  "Test LOOP while."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *items* '(1 2 3 0 4 5)) env)
    (ok (equal '(1 2 3)
               (interpret '(loop for x in *items*
                                 while (> x 0)
                                 collect x)
                          env)))))

(deftest test-loop-until
  "Test LOOP until."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 2 3)
               (interpret '(loop for i from 1
                                 until (> i 3)
                                 collect i)
                          env)))))

(deftest test-loop-return
  "Test LOOP return."
  (let ((env (make-interpreter-env)))
    (ok (= 3 (interpret '(loop for i from 1 to 10
                               when (= i 3) return i)
                        env)))))

(deftest test-loop-initially-finally
  "Test LOOP initially and finally."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *result* nil) env)
    (interpret '(loop initially (setq *result* :started)
                      for i from 1 to 3
                      collect i
                      finally (setq *result* :finished))
               env)
    (ok (eq :finished (interpret '*result* env)))))
