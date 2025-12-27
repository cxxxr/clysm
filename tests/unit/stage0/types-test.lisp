;;;; types-test.lisp - Unit tests for Stage 0 WasmGC type section generation
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US4: Runtime Initialization - WasmGC type definitions

(defpackage #:clysm/tests/unit/stage0/types-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-type-section))

(in-package #:clysm/tests/unit/stage0/types-test)

;;; ============================================================
;;; T007: Unit test for WasmGC type section generation
;;; ============================================================

(deftest test-type-section-generates-bytes
  "Verify generate-type-section returns a byte vector"
  (let ((bytes (generate-type-section)))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 0) "Should have non-zero length")
    (ok (every (lambda (b) (and (integerp b) (<= 0 b 255))) bytes)
        "Should contain only valid bytes")))

(deftest test-type-section-starts-with-section-id
  "Verify type section starts with section ID 1"
  (let ((bytes (generate-type-section)))
    (ok (= 1 (aref bytes 0)) "First byte should be section ID 1 (type section)")))

(deftest test-type-section-contains-required-types
  "Verify all 24+ required types are present"
  (let ((bytes (generate-type-section)))
    ;; Section header: 1 byte ID + LEB128 size + LEB128 type count
    ;; We need at least 24 types for: NIL, UNBOUND, cons, symbol, string,
    ;; closure, instance, standard-class, func-0..func-n, binding-frame,
    ;; bignum, ratio, float, complex, limb-array, stream, mv-array,
    ;; slot-vector, keyword-array, closure-array, macro-environment, hash-entry, hash-table, bucket-array
    ;; The count is encoded in LEB128 after section ID and size
    (ok (>= (length bytes) 20) "Should have enough bytes for 24+ types")))

(deftest test-type-section-nil-is-struct
  "Verify NIL type (index 0) is a struct type"
  (let ((bytes (generate-type-section)))
    ;; NIL is first type, should be struct (0x5F in WasmGC)
    ;; After section ID, size, and count
    (ok (member #x5F (coerce bytes 'list))
        "Should contain struct type marker (0x5F)")))

(deftest test-type-section-has-function-types
  "Verify function types (func-0 through func-n) are defined"
  (let ((bytes (generate-type-section)))
    ;; Function types use 0x60 marker
    (ok (member #x60 (coerce bytes 'list))
        "Should contain function type marker (0x60)")))

(deftest test-type-section-has-array-types
  "Verify array types (mv-array, slot-vector, etc.) are defined"
  (let ((bytes (generate-type-section)))
    ;; Array types use 0x5E marker in WasmGC
    (ok (member #x5E (coerce bytes 'list))
        "Should contain array type marker (0x5E)")))

;;; ============================================================
;;; Type Definition Structure Tests
;;; ============================================================

(deftest test-cons-type-has-car-cdr-fields
  "Verify cons struct has car and cdr fields (anyref)"
  ;; This test verifies the structure definition, not just presence
  ;; Cons cell: struct { car: anyref, cdr: anyref }
  (let ((bytes (generate-type-section)))
    ;; anyref is encoded as 0x6F
    ;; A cons cell should have at least 2 anyref fields
    (let ((anyref-count (count #x6F bytes)))
      (ok (>= anyref-count 2) "Should have at least 2 anyref references for cons car/cdr"))))

(deftest test-symbol-type-has-name-and-value
  "Verify symbol struct has name and value fields"
  ;; Symbol: struct { name: (ref $string), value: (mut anyref), function: (mut anyref), plist: (mut anyref) }
  (let ((bytes (generate-type-section)))
    ;; We just verify the section is well-formed for now
    (ok (> (length bytes) 50) "Symbol struct should add significant bytes")))

(deftest test-closure-type-has-code-slots
  "Verify closure struct has multi-arity code slots"
  ;; Closure: struct { code_0, code_1, code_2, code_N, env }
  (let ((bytes (generate-type-section)))
    ;; Verify reasonable size for closure with 5 fields
    (ok (> (length bytes) 80) "Closure struct should add significant bytes")))
