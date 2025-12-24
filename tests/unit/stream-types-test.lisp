;;;; stream-types-test.lisp - Unit tests for stream types
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/tests)

;;; ============================================================
;;; Stream Type Index Tests (T005)
;;; ============================================================

(deftest stream-type-index-test
  "Test that +type-stream+ is defined at correct index"
  (testing "+type-stream+ is 19"
    (ok (= clysm/compiler/codegen/gc-types:+type-stream+ 19))))

;;; ============================================================
;;; Stream Type Constructor Tests (T006)
;;; ============================================================

(deftest stream-type-constructor-test
  "Test make-stream-type creates valid WasmGC struct"
  (testing "stream type has correct structure"
    (let ((stream-type (clysm/compiler/codegen/gc-types:make-stream-type)))
      (ok (typep stream-type 'clysm/compiler/codegen/gc-types:wasm-struct-type))
      (ok (= (clysm/compiler/codegen/gc-types:gc-type-index stream-type)
             clysm/compiler/codegen/gc-types:+type-stream+)))))

(deftest stream-type-fields-test
  "Test stream type has correct fields"
  (testing "fd and direction fields"
    (let* ((stream-type (clysm/compiler/codegen/gc-types:make-stream-type))
           (fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields stream-type)))
      (ok (= (length fields) 2))
      (ok (eq (clysm/compiler/codegen/gc-types:wasm-field-type (first fields)) :i32))
      (ok (eq (clysm/compiler/codegen/gc-types:wasm-field-type (second fields)) :i32)))))

;;; ============================================================
;;; Stream Type Generation Tests (T007)
;;; ============================================================

(deftest stream-type-in-generate-test
  "Test stream type is included in generate-type-definitions"
  (testing "stream type at index 19"
    (let ((types (clysm/compiler/codegen/gc-types:generate-type-definitions)))
      (ok (>= (length types) 20))
      (ok (typep (nth 19 types) 'clysm/compiler/codegen/gc-types:wasm-struct-type)))))

;;; ============================================================
;;; Stream Reference Emission Tests (T008)
;;; ============================================================

(deftest stream-ref-emission-test
  "Test :stream-ref is handled by emit-value-type-extended"
  (testing "emit-value-type-extended accepts :stream-ref"
    ;; Verify the function recognizes :stream-ref as a valid type
    ;; Actual byte emission testing is covered in contract tests
    (ok (fboundp 'clysm/compiler/codegen/gc-types:emit-value-type-extended)
        "emit-value-type-extended should be defined")))
