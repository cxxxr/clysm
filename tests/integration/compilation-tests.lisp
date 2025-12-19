;;;; compilation-tests.lisp - End-to-end compilation tests

(in-package #:cl-wasm/tests)

(in-suite :integration)

(test compile-simple-expression
  "Test compiling a simple arithmetic expression to a complete module."
  (let ((module (compile-module '((+ 1 2)))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      ;; Should produce valid WASM
      (is (> (length bytes) 8))
      ;; Check magic number
      (is (= #x00 (aref bytes 0)))
      (is (= #x61 (aref bytes 1)))
      (is (= #x73 (aref bytes 2)))
      (is (= #x6d (aref bytes 3))))))

(test compile-defun
  "Test compiling a function definition."
  (let ((module (compile-module '((defun square (x) (* x x))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-factorial-like
  "Test compiling a function with conditionals."
  ;; Note: This is a simplified version until we have full recursion
  (let ((module (compile-module
                 '((defun abs-val (x)
                     (if (< x 0)
                         (- 0 x)
                         x))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))
