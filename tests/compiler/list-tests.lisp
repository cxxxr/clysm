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

;;; List accessor tests

(test compile-first
  "Test compiling first."
  (let* ((module (cl-wasm/compiler:compile-module '((first (cons 1 2)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-rest
  "Test compiling rest."
  (let* ((module (cl-wasm/compiler:compile-module '((rest (cons 1 2)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-second
  "Test compiling second."
  (let* ((module (cl-wasm/compiler:compile-module '((second (list 1 2 3)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nth
  "Test compiling nth with constant index."
  (let* ((module (cl-wasm/compiler:compile-module '((nth 2 (list 10 20 30)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nthcdr
  "Test compiling nthcdr with constant index."
  (let* ((module (cl-wasm/compiler:compile-module '((nthcdr 1 (list 1 2 3)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Number predicate tests

(test compile-zerop
  "Test compiling zerop."
  (let* ((module (cl-wasm/compiler:compile-module '((zerop 0))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-plusp
  "Test compiling plusp."
  (let* ((module (cl-wasm/compiler:compile-module '((plusp 5))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-minusp
  "Test compiling minusp."
  (let* ((module (cl-wasm/compiler:compile-module '((minusp -5))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-1+
  "Test compiling 1+."
  (let* ((module (cl-wasm/compiler:compile-module '((1+ 5))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-1-
  "Test compiling 1-."
  (let* ((module (cl-wasm/compiler:compile-module '((1- 5))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Equality tests

(test compile-eq
  "Test compiling eq."
  (let* ((module (cl-wasm/compiler:compile-module '((eq 1 1))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-eql
  "Test compiling eql."
  (let* ((module (cl-wasm/compiler:compile-module '((eql 42 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))
