;;;; special-forms-test.lisp - TDD tests for interpreter special forms
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T027

(defpackage #:clysm/tests/interpreter/special-forms-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/special-forms-test)

;;; ============================================================
;;; US4: cond/case/typecase/unwind-protect
;;; ============================================================

(deftest test-cond-basic
  "Test basic cond form."
  (let ((env (make-interpreter-env)))
    (ok (eq :a (interpret '(cond ((= 1 1) :a) (t :b)) env)))
    (ok (eq :b (interpret '(cond ((= 1 2) :a) (t :b)) env)))
    (ok (eq nil (interpret '(cond ((= 1 2) :a)) env)))))

(deftest test-cond-multiple-clauses
  "Test cond with multiple clauses."
  (let ((env (make-interpreter-env)))
    (interpret '(defun classify (n)
                  (cond ((< n 0) :negative)
                        ((= n 0) :zero)
                        ((< n 10) :small)
                        (t :large)))
               env)
    (ok (eq :negative (interpret '(classify -5) env)))
    (ok (eq :zero (interpret '(classify 0) env)))
    (ok (eq :small (interpret '(classify 5) env)))
    (ok (eq :large (interpret '(classify 100) env)))))

(deftest test-cond-implicit-progn
  "Test cond with multiple forms per clause."
  (let ((env (make-interpreter-env)))
    (ok (= 3 (interpret '(cond (t 1 2 3)) env)))))

(deftest test-case-basic
  "Test basic case form."
  (let ((env (make-interpreter-env)))
    (interpret '(defun day-type (day)
                  (case day
                    ((sat sun) :weekend)
                    ((mon tue wed thu fri) :weekday)
                    (otherwise :unknown)))
               env)
    (ok (eq :weekend (interpret '(day-type 'sat) env)))
    (ok (eq :weekday (interpret '(day-type 'mon) env)))
    (ok (eq :unknown (interpret '(day-type 'foo) env)))))

(deftest test-case-single-key
  "Test case with single keys."
  (let ((env (make-interpreter-env)))
    (ok (eq :one (interpret '(case 1 (1 :one) (2 :two) (t :other)) env)))
    (ok (eq :two (interpret '(case 2 (1 :one) (2 :two) (t :other)) env)))
    (ok (eq :other (interpret '(case 3 (1 :one) (2 :two) (t :other)) env)))))

(deftest test-ecase-error
  "Test that ecase signals error for unmatched key."
  (let ((env (make-interpreter-env)))
    (ok (eq :yes (interpret '(ecase 'a ((a b) :yes)) env)))
    (ok (signals (interpret '(ecase 'z ((a b) :yes)) env)))))

(deftest test-typecase-basic
  "Test basic typecase form."
  (let ((env (make-interpreter-env)))
    (interpret '(defun what-type (x)
                  (typecase x
                    (integer :int)
                    (string :str)
                    (symbol :sym)
                    (t :other)))
               env)
    (ok (eq :int (interpret '(what-type 42) env)))
    (ok (eq :str (interpret '(what-type "hello") env)))
    (ok (eq :sym (interpret '(what-type 'foo) env)))
    (ok (eq :other (interpret '(what-type #(1 2 3)) env)))))

(deftest test-etypecase-error
  "Test that etypecase signals error for unmatched type."
  (let ((env (make-interpreter-env)))
    (ok (eq :int (interpret '(etypecase 42 (integer :int)) env)))
    (ok (signals (interpret '(etypecase "str" (integer :int)) env)))))

(deftest test-unwind-protect-normal
  "Test unwind-protect with normal execution."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *cleanup-ran* nil) env)
    (interpret '(unwind-protect
                    (progn 42)
                  (setq *cleanup-ran* t))
               env)
    (ok (interpret '*cleanup-ran* env))))

(deftest test-unwind-protect-error
  "Test unwind-protect cleanup runs on error."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *cleanup-ran* nil) env)
    (handler-case
        (interpret '(unwind-protect
                        (error "test error")
                      (setq *cleanup-ran* t))
                   env)
      (error () nil))  ; Ignore the error
    (ok (interpret '*cleanup-ran* env))))

(deftest test-the-form
  "Test that THE form is accepted (declarations ignored)."
  (let ((env (make-interpreter-env)))
    (ok (= 42 (interpret '(the integer 42) env)))
    (ok (equal "str" (interpret '(the string "str") env)))))

(deftest test-locally-form
  "Test locally form with declarations."
  (let ((env (make-interpreter-env)))
    (ok (= 5 (interpret '(locally
                          (declare (type integer x))
                          (+ 2 3))
                        env)))))
