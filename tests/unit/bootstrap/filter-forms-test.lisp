;;;; filter-forms-test.lisp - Unit tests for filter-compilable-forms
;;;; TDD: Tests written first per Constitution VII

(defpackage :clysm/tests/bootstrap/filter-forms
  (:use :cl :rove))

(in-package :clysm/tests/bootstrap/filter-forms)

;;; Test filter-compilable-forms function

(deftest filter-removes-in-package
  "filter-compilable-forms should remove in-package forms"
  (testing "removes in-package"
    (let* ((forms '((in-package :foo) (defun bar () 1)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should have 1 form after filtering")
      (ok (eq 'defun (caar filtered)) "Remaining form should be defun"))))

(deftest filter-removes-defpackage
  "filter-compilable-forms should remove defpackage forms"
  (testing "removes defpackage"
    (let* ((forms '((defpackage :foo (:use :cl)) (defvar *x* 1)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should have 1 form after filtering"))))

(deftest filter-removes-declare
  "filter-compilable-forms should remove declare forms"
  (testing "removes declare"
    (let* ((forms '((declare (optimize speed)) (defun foo () 1)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should have 1 form after filtering"))))

(deftest filter-keeps-defun
  "filter-compilable-forms should keep defun forms"
  (testing "keeps defun"
    (let* ((forms '((defun foo () 1) (defun bar () 2)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 2 (length filtered)) "Should keep both defuns"))))

(deftest filter-keeps-defvar
  "filter-compilable-forms should keep defvar/defparameter forms"
  (testing "keeps defvar and defparameter"
    (let* ((forms '((defvar *x* 1) (defparameter *y* 2)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 2 (length filtered)) "Should keep both forms"))))

(deftest filter-keeps-defmacro
  "filter-compilable-forms should keep defmacro forms"
  (testing "keeps defmacro"
    (let* ((forms '((defmacro foo () 1)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should keep defmacro"))))

(deftest filter-keeps-defclass
  "filter-compilable-forms should keep defclass forms"
  (testing "keeps defclass"
    (let* ((forms '((defclass foo () ())))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should keep defclass"))))

(deftest filter-keeps-defmethod
  "filter-compilable-forms should keep defmethod forms"
  (testing "keeps defmethod"
    (let* ((forms '((defmethod foo ((x t)) x)))
           (filtered (clysm/bootstrap:filter-compilable-forms forms)))
      (ok (= 1 (length filtered)) "Should keep defmethod"))))
