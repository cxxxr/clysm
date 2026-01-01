;;;; test-unknown-types.lisp - Unit tests for unknown type error handling
;;;; Task: T042 [US4]
;;;;
;;;; TDD: Tests written FIRST before implementation.
;;;; These tests must FAIL initially (T043).

(in-package #:clysm/tests)

(deftest test-unknown-type-signals-error ()
  "Test that unknown type name signals an error."
  (ok (signals error (clysm::type-index-for-name 'nonexistent-type-xyz))
      "Unknown type should signal an error"))

(deftest test-make-wasm-struct-type-known ()
  "Test make-wasm-struct-type* for known type name."
  (let ((result (clysm::make-wasm-struct-type* :name 'cons)))
    (ok result
        "make-wasm-struct-type* should return a value for known type")
    (ok (= (clysm::wasm-struct-type-index result) 0)
        "cons type should have index 0")))

(deftest test-make-wasm-struct-type-unknown ()
  "Test make-wasm-struct-type* for unknown type name."
  (ok (signals error (clysm::make-wasm-struct-type* :name 'unknown-type-abc))
      "make-wasm-struct-type* should error for unknown type"))

(deftest test-nil-type-name ()
  "Test that nil type name signals an error."
  (ok (signals error (clysm::type-index-for-name nil))
      "nil type name should signal an error"))

(deftest test-compilation ()
  "Test that type construction forms compile to Wasm."
  (let ((form '(defun test-type () (clysm::type-index-for-name 'cons))))
    (let ((result (compile-form-to-wasm form)))
      (ok result
          "Form using type-index-for-name should compile"))))
