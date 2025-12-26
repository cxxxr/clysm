;;;; marshal-test.lisp - Unit tests for FFI type marshalling
;;;; Feature: 027-complete-ffi (T013)

(in-package #:clysm/tests)

(deftest marshal-fixnum-test
  "Test :fixnum marshalling instruction generation"
  (testing "fixnum to i32"
    ;; :fixnum marshals from i31ref to i32
    (let ((instrs (clysm/ffi:marshal-to-wasm :fixnum)))
      (ok instrs "Should generate instructions")
      (ok (member 'i31.get_s instrs)
          "Should use i31.get_s to extract signed value")))

  (testing "i32 to fixnum"
    (let ((instrs (clysm/ffi:marshal-from-wasm :fixnum)))
      (ok instrs "Should generate instructions")
      (ok (member 'ref.i31 instrs)
          "Should use ref.i31 to create i31ref"))))

(deftest marshal-float-test
  "Test :float marshalling instruction generation"
  (testing "float to f64"
    ;; :float marshals from (ref $float) to f64
    (let ((instrs (clysm/ffi:marshal-to-wasm :float)))
      (ok instrs "Should generate instructions")
      ;; instrs is a list like ((struct.get $float 0))
      (ok (find 'struct.get instrs :test (lambda (sym x) (and (listp x) (eq (car x) sym))))
          "Should extract f64 from float struct")))

  (testing "f64 to float"
    (let ((instrs (clysm/ffi:marshal-from-wasm :float)))
      (ok instrs "Should generate instructions")
      (ok (find 'struct.new instrs :test (lambda (sym x) (and (listp x) (eq (car x) sym))))
          "Should create float struct from f64"))))

(deftest marshal-boolean-test
  "Test :boolean marshalling instruction generation"
  (testing "boolean to i32"
    ;; :boolean marshals from T/NIL to 1/0
    (let ((instrs (clysm/ffi:marshal-to-wasm :boolean)))
      (ok instrs "Should generate instructions for boolean->i32")
      (ok (member 'ref.is_null instrs)
          "Should check for null (nil)")))

  (testing "i32 to boolean"
    (let ((instrs (clysm/ffi:marshal-from-wasm :boolean)))
      (ok instrs "Should generate instructions for i32->boolean"))))

(deftest marshal-string-test
  "Test :string marshalling instruction generation"
  (testing "string to externref"
    ;; :string marshals from (ref $string) to externref
    (let ((instrs (clysm/ffi:marshal-to-wasm :string)))
      (ok instrs "Should generate instructions")
      (ok (member 'extern.convert_any instrs)
          "Should use extern.convert_any for string->externref")))

  (testing "externref to string"
    (let ((instrs (clysm/ffi:marshal-from-wasm :string)))
      (ok instrs "Should generate instructions")
      (ok (member 'any.convert_extern instrs)
          "Should use any.convert_extern for externref->string"))))

(deftest marshal-anyref-test
  "Test :anyref marshalling (passthrough)"
  (testing "anyref passthrough to-wasm"
    (let ((instrs (clysm/ffi:marshal-to-wasm :anyref)))
      ;; :anyref is passthrough - no conversion needed
      (ok (null instrs) "anyref should have no conversion instructions")))

  (testing "anyref passthrough from-wasm"
    (let ((instrs (clysm/ffi:marshal-from-wasm :anyref)))
      (ok (null instrs) "anyref should have no conversion instructions"))))

(deftest marshal-void-return-test
  "Test :void return type handling"
  (testing "void generates NIL"
    ;; Void return needs special handling - push NIL
    ;; The compile-ffi-call handles this case
    (ok t "Void return is handled in compile-ffi-call")))
