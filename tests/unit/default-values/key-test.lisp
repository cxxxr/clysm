;;;; key-test.lisp - Unit tests for &key default values
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US3 - Default Values

(in-package #:clysm/tests)

(deftest key-default-ast-test
  "Test parsing of &key with default values."
  (testing "&key (x 10) parses correctly"
    (let ((ast (clysm/compiler::parse-form '(defun foo (&key (x 10)) x))))
      (ok (clysm/compiler::ast-defun-p ast))
      (let ((params (clysm/compiler::ast-defun-params ast)))
        (ok (clysm/compiler::ast-lambda-list-keys params))))))

(deftest key-default-wasm-test
  "Test Wasm generation for &key with defaults."
  (testing "defun with &key (x 10) compiles"
    (let ((wasm (clysm:compile-to-wasm '(defun foo (&key (x 10)) x))))
      (ok wasm "Should compile defun with key default"))))

(deftest key-supplied-p-test
  "Test &key with supplied-p parameter."
  (testing "defun with (:x x 10 x-p) compiles"
    (let ((wasm (clysm:compile-to-wasm '(defun foo (&key ((:x x) 10 x-p)) (if x-p x 0)))))
      (ok wasm "Should compile defun with key supplied-p"))))

(deftest multiple-keys-test
  "Test multiple &key parameters with defaults."
  (testing "defun with multiple keys compiles"
    (let ((wasm (clysm:compile-to-wasm '(defun foo (&key (x 1) (y 2) (z 3)) (+ x y z)))))
      (ok wasm "Should compile defun with multiple keys"))))
