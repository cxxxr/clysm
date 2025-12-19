;;;; list-tests.lisp - Tests for cons, car, cdr

(in-package #:cl-wasm/tests)

(in-suite :compiler)

(test compile-cons
  "Test compiling cons."
  (let* ((module (cl-wasm/compiler:compile-module '((cons 1 2))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Verify memory section exists
    (is (not (null (cl-wasm/wasm:wasm-module-memories module))))))

(test compile-car
  "Test compiling car of a cons."
  (let* ((module (cl-wasm/compiler:compile-module '((car (cons 42 0)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-cdr
  "Test compiling cdr of a cons."
  (let* ((module (cl-wasm/compiler:compile-module '((cdr (cons 0 99)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nested-cons
  "Test compiling nested cons."
  (let* ((module (cl-wasm/compiler:compile-module
                  '((car (cdr (cons 1 (cons 2 (cons 3 0))))))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-consp
  "Test compiling consp predicate."
  (let* ((module (cl-wasm/compiler:compile-module '((consp (cons 1 2)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list-function
  "Test compiling a function that uses lists."
  (let* ((module (cl-wasm/compiler:compile-module
                  '((defun first-elem (lst)
                      (car lst))
                    (defun second-elem (lst)
                      (car (cdr lst))))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list-empty
  "Test compiling empty list."
  (let* ((module (cl-wasm/compiler:compile-module '((list))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list-single
  "Test compiling list with single element."
  (let* ((module (cl-wasm/compiler:compile-module '((list 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list-multiple
  "Test compiling list with multiple elements."
  (let* ((module (cl-wasm/compiler:compile-module '((list 1 2 3))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list-car-access
  "Test accessing car of a list."
  (let* ((module (cl-wasm/compiler:compile-module '((car (list 1 2 3)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list*-single
  "Test compiling list* with single element."
  (let* ((module (cl-wasm/compiler:compile-module '((list* 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list*-two
  "Test compiling list* with two elements (same as cons)."
  (let* ((module (cl-wasm/compiler:compile-module '((list* 1 2))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-list*-multiple
  "Test compiling list* with multiple elements."
  (let* ((module (cl-wasm/compiler:compile-module '((list* 1 2 3))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))
