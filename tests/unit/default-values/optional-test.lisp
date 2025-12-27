;;;; optional-test.lisp - Unit tests for &optional default values
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US3 - Default Values

(in-package #:clysm/tests)

(deftest optional-default-ast-test
  "Test parsing of &optional with default values."
  (testing "&optional (x 10) parses correctly"
    (let ((ast (clysm/compiler::parse-form '(defun foo (&optional (x 10)) x))))
      (ok (clysm/compiler::ast-defun-p ast))
      (let ((params (clysm/compiler::ast-defun-params ast)))
        (ok (clysm/compiler::ast-lambda-list-optional params))))))

(deftest optional-default-wasm-test
  "Test Wasm generation for &optional with defaults."
  (testing "defun with &optional (x 10) compiles"
    (let ((wasm (clysm:compile-to-wasm '(defun foo (&optional (x 10)) x))))
      (ok wasm "Should compile defun with optional default"))))

(deftest optional-supplied-p-test
  "Test &optional with supplied-p parameter."
  (testing "defun with (x 10 x-p) compiles"
    (let ((wasm (clysm:compile-to-wasm '(defun foo (&optional (x 10 x-p)) (if x-p x 0)))))
      (ok wasm "Should compile defun with supplied-p"))))
