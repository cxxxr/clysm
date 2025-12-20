;;;; compilation-tests.lisp - End-to-end compilation tests

(in-package #:clysm/tests)

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

(test compile-recursive-function
  "Test compiling a recursive function."
  (let ((module (compile-module
                 '((defun sum-to-n (n acc)
                     (if (<= n 0)
                         acc
                         (sum-to-n (- n 1) (+ acc n))))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-let*-in-defun
  "Test compiling let* inside a function."
  (let ((module (compile-module
                 '((defun compute ()
                     (let* ((a 1)
                            (b (+ a 2))
                            (c (+ a b)))
                       (* c 2)))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-list-operations
  "Test compiling list operations."
  (let ((module (compile-module
                 '((defun sum-list (lst acc)
                     (if (consp lst)
                         (sum-list (cdr lst) (+ acc (car lst)))
                         acc))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-multiple-functions
  "Test compiling multiple functions that call each other."
  (let ((module (compile-module
                 '((defun double (x) (* x 2))
                   (defun quadruple (x) (double (double x)))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

;;; Closure Tests

(test compile-lambda-no-capture
  "Test compiling a lambda that doesn't capture variables."
  (let ((module (compile-module
                 '((defun test-lambda ()
                     (funcall (lambda (x) (+ x 1)) 5))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-lambda-with-capture
  "Test compiling a lambda that captures a variable."
  (let ((module (compile-module
                 '((defun make-adder (n)
                     (lambda (x) (+ x n)))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test compile-closure-funcall
  "Test compiling funcall with a closure."
  (let ((module (compile-module
                 '((defun test-closure ()
                     (let ((f (lambda (x) (* x 2))))
                       (funcall f 21)))))))
    (is (not (null module)))
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))
