;;;; gc-types-test.lisp - WasmGC type definition tests
(in-package #:clysm/tests/unit/gc-types)

;;; T026: WasmGC type definition tests

(deftest test-nil-type-definition
  "NIL singleton type should be defined as struct with single i32 field"
  (let ((nil-type (clysm/compiler/codegen/gc-types:make-nil-type)))
    (ok nil-type "NIL type should be created")
    (ok (typep nil-type 'clysm/compiler/codegen/gc-types:wasm-struct-type)
        "NIL type should be a struct type")))

(deftest test-unbound-type-definition
  "UNBOUND sentinel type should be defined as struct with single i32 field"
  (let ((unbound-type (clysm/compiler/codegen/gc-types:make-unbound-type)))
    (ok unbound-type "UNBOUND type should be created")
    (ok (typep unbound-type 'clysm/compiler/codegen/gc-types:wasm-struct-type)
        "UNBOUND type should be a struct type")))

(deftest test-cons-type-definition
  "CONS cell type should have car and cdr fields of anyref"
  (let ((cons-type (clysm/compiler/codegen/gc-types:make-cons-type)))
    (ok cons-type "CONS type should be created")
    (ok (typep cons-type 'clysm/compiler/codegen/gc-types:wasm-struct-type)
        "CONS type should be a struct type")
    (ok (= 2 (length (clysm/compiler/codegen/gc-types:struct-fields cons-type)))
        "CONS type should have 2 fields (car, cdr)")))

(deftest test-symbol-type-definition
  "Symbol type should have name field"
  (let ((symbol-type (clysm/compiler/codegen/gc-types:make-symbol-type)))
    (ok symbol-type "Symbol type should be created")
    (ok (typep symbol-type 'clysm/compiler/codegen/gc-types:wasm-struct-type)
        "Symbol type should be a struct type")))

(deftest test-string-type-definition
  "String type should be an array of i8"
  (let ((string-type (clysm/compiler/codegen/gc-types:make-string-type)))
    (ok string-type "String type should be created")
    (ok (typep string-type 'clysm/compiler/codegen/gc-types:wasm-array-type)
        "String type should be an array type")))

(deftest test-i31ref-for-fixnum
  "Fixnums should be represented as i31ref"
  ;; i31ref is a built-in Wasm type, we test that we use it correctly
  (let ((fixnum-spec (clysm/compiler/codegen/gc-types:fixnum-representation)))
    (ok (eq :i31ref fixnum-spec)
        "Fixnum should be represented as i31ref")))

(deftest test-type-index-assignment
  "Type indices should be assigned consistently"
  (let ((type-env (clysm/compiler/codegen/gc-types:make-type-environment)))
    (let ((nil-idx (clysm/compiler/codegen/gc-types:register-type type-env :nil))
          (unbound-idx (clysm/compiler/codegen/gc-types:register-type type-env :unbound))
          (cons-idx (clysm/compiler/codegen/gc-types:register-type type-env :cons)))
      (ok (numberp nil-idx) "NIL should get a type index")
      (ok (numberp unbound-idx) "UNBOUND should get a type index")
      (ok (numberp cons-idx) "CONS should get a type index")
      (ok (not (= nil-idx unbound-idx)) "Type indices should be unique"))))
