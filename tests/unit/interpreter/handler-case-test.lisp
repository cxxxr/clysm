;;;; handler-case-test.lisp - TDD tests for interpreter condition handling
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T025

(defpackage #:clysm/tests/interpreter/handler-case-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/handler-case-test)

;;; ============================================================
;;; US4: handler-case/handler-bind/restart-case/restart-bind
;;; ============================================================

(deftest test-handler-case-basic
  "Test basic handler-case."
  (let ((env (make-interpreter-env)))
    (ok (eq :caught
            (interpret '(handler-case
                            (error "test")
                          (error (c) :caught))
                       env)))))

(deftest test-handler-case-no-error
  "Test handler-case when no error occurs."
  (let ((env (make-interpreter-env)))
    (ok (= 42
           (interpret '(handler-case
                           42
                         (error (c) :caught))
                      env)))))

(deftest test-handler-case-multiple-handlers
  "Test handler-case with multiple handlers."
  (let ((env (make-interpreter-env)))
    (ok (eq :type-error
            (interpret '(handler-case
                            (+ "not" "numbers")
                          (type-error (c) :type-error)
                          (error (c) :other-error))
                       env)))))

(deftest test-handler-case-access-condition
  "Test accessing condition object in handler."
  (let ((env (make-interpreter-env)))
    (let ((result (interpret '(handler-case
                                  (error "my message")
                                (error (c)
                                  (format nil "Got: ~A" c)))
                             env)))
      (ok (stringp result))
      (ok (search "my message" result)))))

(deftest test-handler-case-no-binding
  "Test handler-case without condition variable."
  (let ((env (make-interpreter-env)))
    (ok (eq :handled
            (interpret '(handler-case
                            (error "test")
                          (error () :handled))
                       env)))))

(deftest test-handler-bind-basic
  "Test basic handler-bind."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *handled* nil) env)
    (interpret '(handler-bind ((warning
                                (lambda (c)
                                  (setq *handled* t)
                                  (muffle-warning))))
                  (warn "test warning"))
               env)
    (ok (interpret '*handled* env))))

(deftest test-restart-case-basic
  "Test basic restart-case."
  (let ((env (make-interpreter-env)))
    (ok (eq :restarted
            (interpret '(restart-case
                            (progn
                              (invoke-restart 'my-restart)
                              :not-restarted)
                          (my-restart () :restarted))
                       env)))))

(deftest test-restart-case-with-args
  "Test restart-case with arguments."
  (let ((env (make-interpreter-env)))
    (ok (= 42
           (interpret '(restart-case
                           (invoke-restart 'use-value 42)
                         (use-value (val) val))
                      env)))))

(deftest test-restart-bind-basic
  "Test basic restart-bind."
  (let ((env (make-interpreter-env)))
    (ok (eq :bound
            (interpret '(restart-bind ((my-restart
                                        (lambda () :bound)))
                          (invoke-restart 'my-restart))
                       env)))))

(deftest test-cerror-and-continue
  "Test cerror with continue restart."
  (let ((env (make-interpreter-env)))
    (ok (eq :continued
            (interpret '(handler-bind ((simple-error
                                        (lambda (c)
                                          (invoke-restart 'continue))))
                          (cerror "Continue anyway" "Error")
                          :continued)
                       env)))))

(deftest test-invoke-restart-interactively
  "Test invoke-restart-interactively."
  (skip "Interactive restart invocation requires more setup"))

(deftest test-with-simple-restart
  "Test with-simple-restart macro."
  (let ((env (make-interpreter-env)))
    (ok (equal '(nil t)
               (interpret '(multiple-value-list
                            (with-simple-restart (abort "Abort")
                              (invoke-restart 'abort)))
                          env)))))
