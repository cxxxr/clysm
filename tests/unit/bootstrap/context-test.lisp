;;;; context-test.lisp - Unit tests for bootstrap-context struct
;;;; TDD: Tests written first per Constitution VII

(defpackage :clysm/tests/bootstrap/context
  (:use :cl :rove))

(in-package :clysm/tests/bootstrap/context)

;;; Test bootstrap-context struct

(deftest bootstrap-context-struct-exists
  "bootstrap-context struct should be defined"
  (testing "struct type exists"
    (ok (find-class 'clysm/bootstrap:bootstrap-context nil)
        "bootstrap-context class should exist")))

(deftest make-bootstrap-context-works
  "make-bootstrap-context should create a context"
  (testing "constructor works"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context)))
      (ok ctx "Should create a context"))))

(deftest bootstrap-context-has-source-files
  "bootstrap-context should have source-files slot"
  (testing "source-files accessor exists"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context :source-files '("/a" "/b"))))
      (ok (listp (clysm/bootstrap:bootstrap-context-source-files ctx))
          "source-files should return a list"))))

(deftest bootstrap-context-has-all-forms
  "bootstrap-context should have all-forms slot"
  (testing "all-forms accessor exists"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context :all-forms '((defun a () 1)))))
      (ok (listp (clysm/bootstrap:bootstrap-context-all-forms ctx))
          "all-forms should return a list"))))

(deftest bootstrap-context-has-current-module
  "bootstrap-context should have current-module slot"
  (testing "current-module accessor exists"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context :current-module "/path/to/mod.lisp")))
      (ok (clysm/bootstrap:bootstrap-context-current-module ctx)
          "current-module should be accessible"))))

(deftest bootstrap-context-has-error-info
  "bootstrap-context should have error-info slot"
  (testing "error-info accessor exists"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context :error-info nil)))
      (ok (null (clysm/bootstrap:bootstrap-context-error-info ctx))
          "error-info should be nil by default"))))

(deftest bootstrap-context-has-output-path
  "bootstrap-context should have output-path slot"
  (testing "output-path accessor exists"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context :output-path "dist/clysm-stage0.wasm")))
      (ok (clysm/bootstrap:bootstrap-context-output-path ctx)
          "output-path should be accessible"))))
