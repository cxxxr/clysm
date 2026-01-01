;;; ast-export-test.lisp - Unit tests for AST function export system
;;; Feature: 001-ast-function-export

(in-package #:clysm/tests/unit/ast-export)

(deftest ast-functions-exported
  "Test that AST functions are exported from clysm package."
  (testing "All 8 AST manipulation functions should be external in clysm"
    (dolist (sym '(compile-to-instructions
                   make-wasm-struct-type
                   wasm-struct-type-p
                   wasm-struct-type-fields
                   make-ast-literal
                   ast-literal-value
                   ast-literal-p
                   get-numeric-value))
      (multiple-value-bind (symbol status)
          (find-symbol (symbol-name sym) :clysm)
        (ok symbol (format nil "~A should exist in clysm package" sym))
        (ok (eq status :external) (format nil "~A should be external" sym))))))

(deftest ast-functions-registered
  "Test that AST functions are registered in runtime function table with correct arities."
  (testing "All 8 AST functions should be in *runtime-function-table*"
    (dolist (entry '((clysm:compile-to-instructions . 2)
                     (clysm:make-wasm-struct-type . nil)
                     (clysm:wasm-struct-type-p . 1)
                     (clysm:wasm-struct-type-fields . 1)
                     (clysm:make-ast-literal . nil)
                     (clysm:ast-literal-value . 1)
                     (clysm:ast-literal-p . 1)
                     (clysm:get-numeric-value . 1)))
      (let* ((sym (car entry))
             (expected-arity (cdr entry))
             (table-entry (clysm/compiler/codegen/func-section::runtime-function-p sym)))
        (ok table-entry (format nil "~A should be in runtime table" sym))
        (when table-entry
          (ok (eql (cdr table-entry) expected-arity)
              (format nil "~A arity should be ~A, got ~A"
                      sym expected-arity (cdr table-entry))))))))
