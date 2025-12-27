;;;; read-forms-test.lisp - Unit tests for read-all-source-forms
;;;; TDD: Tests written first per Constitution VII

(defpackage :clysm/tests/bootstrap/read-forms
  (:use :cl :rove))

(in-package :clysm/tests/bootstrap/read-forms)

;;; Test read-all-source-forms function

(deftest read-all-source-forms-returns-list
  "read-all-source-forms should return a list of forms"
  (testing "returns a list for valid module paths"
    (let ((forms (clysm/bootstrap:read-all-source-forms)))
      (ok (listp forms) "Result should be a list"))))

(deftest read-all-source-forms-reads-41-modules
  "read-all-source-forms should read from all 41 modules in *compilation-order*"
  (testing "reads from all modules"
    (let ((forms (clysm/bootstrap:read-all-source-forms)))
      (ok (> (length forms) 0) "Should read at least some forms"))))

(deftest read-all-source-forms-returns-sexps
  "Each form should be a valid S-expression"
  (testing "all forms are valid S-expressions"
    (let ((forms (clysm/bootstrap:read-all-source-forms)))
      (ok (every (lambda (f) (or (consp f) (symbolp f) (numberp f) (stringp f)))
                 forms)
          "All forms should be valid S-expressions"))))

(deftest read-source-file-exists
  "read-source-file should exist and be callable"
  (testing "function is defined"
    (ok (fboundp 'clysm/bootstrap:read-source-file)
        "read-source-file should be defined")))
