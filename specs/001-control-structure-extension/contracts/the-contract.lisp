;;;; the-contract.lisp - Test contracts for the type declaration
;;;; HyperSpec: resources/HyperSpec/Body/s_the.htm

(in-package #:clysm-test)

;;; ============================================================
;;; Contract Tests: the Special Form
;;; ============================================================

;;; The `the` special form is a type declaration that passes through
;;; its value without runtime type checking in this implementation.

(deftest the-simple-type-compiles
  "Contract: (the fixnum x) compiles to just evaluating x"
  (let ((wasm (compile-form-to-wasm '(lambda (x) (the fixnum x)))))
    ;; Should compile successfully
    (ok wasm "the with simple type should compile")
    ;; Should just return the value
    (ok (wasm-returns-local wasm 0))))

(deftest the-complex-type-compiles
  "Contract: (the (or null cons) x) compiles successfully"
  (let ((wasm (compile-form-to-wasm '(lambda (x) (the (or null cons) x)))))
    (ok wasm "the with complex type should compile")
    (ok (wasm-validates wasm))))

(deftest the-nested-compiles
  "Contract: Nested the declarations compile"
  (let ((wasm (compile-form-to-wasm '(lambda (x) (the fixnum (the integer x))))))
    (ok wasm "nested the should compile")
    (ok (wasm-validates wasm))))

(deftest the-with-expression-compiles
  "Contract: (the t (+ 1 2)) compiles the expression"
  (let ((wasm (compile-form-to-wasm '(the fixnum (+ 1 2)))))
    ;; Should contain addition operation
    (ok (wasm-contains-instruction wasm '(call $+)))
    (ok (wasm-validates wasm))))

(deftest the-values-type-compiles
  "Contract: (the (values fixnum string) (values 1 \"x\")) compiles"
  (let ((wasm (compile-form-to-wasm '(the (values fixnum string) (values 1 "x")))))
    (ok wasm "the with values type should compile")
    (ok (wasm-validates wasm))))

(deftest the-all-standard-types
  "Contract: the accepts all standard CL type specifiers"
  (dolist (type '(t nil fixnum integer float string symbol cons list
                  array vector hash-table function))
    (let ((wasm (compile-form-to-wasm `(lambda (x) (the ,type x)))))
      (ok (wasm-validates wasm)
          (format nil "(the ~S x) must compile" type)))))
