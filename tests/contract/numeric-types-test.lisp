;;;; numeric-types-test.lisp - WasmGC numeric type structure validation tests
;;;; T016: Contract tests for numeric tower type definitions
(in-package #:clysm/tests/contract/numeric-types)

;;; ============================================================
;;; Type Index Tests
;;; ============================================================

(deftest test-type-indices-unique
  "Verify all numeric type indices are unique and non-overlapping"
  ;; Test that each type has a distinct index
  (let ((indices (list clysm/compiler/codegen/gc-types:+type-bignum+
                       clysm/compiler/codegen/gc-types:+type-ratio+
                       clysm/compiler/codegen/gc-types:+type-float+
                       clysm/compiler/codegen/gc-types:+type-complex+
                       clysm/compiler/codegen/gc-types:+type-limb-array+)))
    ;; All indices should be unique
    (ok (= (length indices) (length (remove-duplicates indices)))
        "All numeric type indices should be unique")
    ;; All indices should be >= 14 (after existing types)
    (dolist (idx indices)
      (ok (>= idx 14)
          (format nil "Type index ~A should be >= 14" idx)))))

(deftest test-type-indices-correct-values
  "Verify numeric type indices have expected values"
  (ok (= 14 clysm/compiler/codegen/gc-types:+type-bignum+)
      "+type-bignum+ should be 14")
  (ok (= 15 clysm/compiler/codegen/gc-types:+type-ratio+)
      "+type-ratio+ should be 15")
  (ok (= 16 clysm/compiler/codegen/gc-types:+type-float+)
      "+type-float+ should be 16")
  (ok (= 17 clysm/compiler/codegen/gc-types:+type-complex+)
      "+type-complex+ should be 17")
  (ok (= 18 clysm/compiler/codegen/gc-types:+type-limb-array+)
      "+type-limb-array+ should be 18"))

;;; ============================================================
;;; Type Constructor Tests
;;; ============================================================

(deftest test-bignum-type-constructor
  "Verify bignum type constructor produces correct structure"
  (let ((bignum-type (clysm/compiler/codegen/gc-types:make-bignum-type)))
    (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p bignum-type)
        "make-bignum-type should return a struct type")
    (ok (string= "$BIGNUM" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name bignum-type)))
        "Bignum type name should be $bignum")
    (ok (= 14 (clysm/compiler/codegen/gc-types:gc-type-index bignum-type))
        "Bignum type index should be 14")
    ;; Check fields
    (let ((fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields bignum-type)))
      (ok (= 2 (length fields))
          "Bignum should have 2 fields")
      ;; First field: sign
      (let ((sign-field (first fields)))
        (ok (string= "SIGN" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name sign-field)))
            "First field should be 'sign")
        (ok (eq :i32 (clysm/compiler/codegen/gc-types:wasm-field-type sign-field))
            "Sign field should be i32"))
      ;; Second field: limbs
      (let ((limbs-field (second fields)))
        (ok (string= "LIMBS" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name limbs-field)))
            "Second field should be 'limbs")
        (ok (eq :limb-array-ref (clysm/compiler/codegen/gc-types:wasm-field-type limbs-field))
            "Limbs field should be limb-array-ref")))))

(deftest test-ratio-type-constructor
  "Verify ratio type constructor produces correct structure"
  (let ((ratio-type (clysm/compiler/codegen/gc-types:make-ratio-type)))
    (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p ratio-type)
        "make-ratio-type should return a struct type")
    (ok (string= "$RATIO" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name ratio-type)))
        "Ratio type name should be $ratio")
    (ok (= 15 (clysm/compiler/codegen/gc-types:gc-type-index ratio-type))
        "Ratio type index should be 15")
    ;; Check fields
    (let ((fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields ratio-type)))
      (ok (= 2 (length fields))
          "Ratio should have 2 fields")
      ;; First field: numerator
      (let ((num-field (first fields)))
        (ok (string= "NUMERATOR" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name num-field)))
            "First field should be 'numerator")
        (ok (eq :anyref (clysm/compiler/codegen/gc-types:wasm-field-type num-field))
            "Numerator field should be anyref"))
      ;; Second field: denominator
      (let ((den-field (second fields)))
        (ok (string= "DENOMINATOR" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name den-field)))
            "Second field should be 'denominator")
        (ok (eq :anyref (clysm/compiler/codegen/gc-types:wasm-field-type den-field))
            "Denominator field should be anyref")))))

(deftest test-float-type-constructor
  "Verify float type constructor produces correct structure"
  (let ((float-type (clysm/compiler/codegen/gc-types:make-float-type)))
    (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p float-type)
        "make-float-type should return a struct type")
    (ok (string= "$FLOAT" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name float-type)))
        "Float type name should be $float")
    (ok (= 16 (clysm/compiler/codegen/gc-types:gc-type-index float-type))
        "Float type index should be 16")
    ;; Check fields
    (let ((fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields float-type)))
      (ok (= 1 (length fields))
          "Float should have 1 field")
      ;; First field: value
      (let ((value-field (first fields)))
        (ok (string= "VALUE" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name value-field)))
            "Field should be 'value")
        (ok (eq :f64 (clysm/compiler/codegen/gc-types:wasm-field-type value-field))
            "Value field should be f64")))))

(deftest test-complex-type-constructor
  "Verify complex type constructor produces correct structure"
  (let ((complex-type (clysm/compiler/codegen/gc-types:make-complex-type)))
    (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p complex-type)
        "make-complex-type should return a struct type")
    (ok (string= "$COMPLEX" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name complex-type)))
        "Complex type name should be $complex")
    (ok (= 17 (clysm/compiler/codegen/gc-types:gc-type-index complex-type))
        "Complex type index should be 17")
    ;; Check fields
    (let ((fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields complex-type)))
      (ok (= 2 (length fields))
          "Complex should have 2 fields")
      ;; First field: real
      (let ((real-field (first fields)))
        (ok (string= "REAL" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name real-field)))
            "First field should be 'real")
        (ok (eq :anyref (clysm/compiler/codegen/gc-types:wasm-field-type real-field))
            "Real field should be anyref"))
      ;; Second field: imag
      (let ((imag-field (second fields)))
        (ok (string= "IMAG" (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name imag-field)))
            "Second field should be 'imag")
        (ok (eq :anyref (clysm/compiler/codegen/gc-types:wasm-field-type imag-field))
            "Imag field should be anyref")))))

(deftest test-limb-array-type-constructor
  "Verify limb array type constructor produces correct structure"
  (let ((limb-type (clysm/compiler/codegen/gc-types:make-limb-array-type)))
    (ok (clysm/compiler/codegen/gc-types:wasm-array-type-p limb-type)
        "make-limb-array-type should return an array type")
    (ok (string= "$LIMB_ARRAY" (symbol-name (clysm/compiler/codegen/gc-types:wasm-array-type-name limb-type)))
        "Limb array type name should be $limb_array")
    (ok (= 18 (clysm/compiler/codegen/gc-types:gc-type-index limb-type))
        "Limb array type index should be 18")
    (ok (eq :i32 (clysm/compiler/codegen/gc-types:wasm-array-type-element-type limb-type))
        "Limb array element type should be i32")
    (ok (clysm/compiler/codegen/gc-types:wasm-array-type-mutable limb-type)
        "Limb array should be mutable")))

;;; ============================================================
;;; Type Definitions Generation Tests
;;; ============================================================

(deftest test-generate-type-definitions-includes-numeric
  "Verify generate-type-definitions includes all numeric types"
  (let ((types (clysm/compiler/codegen/gc-types:generate-type-definitions)))
    ;; Should have at least 19 types (0-18)
    (ok (>= (length types) 19)
        "Should have at least 19 types")
    ;; Check numeric types are present at correct indices
    (let ((bignum (nth 14 types))
          (ratio (nth 15 types))
          (float-t (nth 16 types))
          (complex-t (nth 17 types))
          (limb-array (nth 18 types)))
      (ok (and bignum
               (string= "$BIGNUM" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name bignum))))
          "Type 14 should be $bignum")
      (ok (and ratio
               (string= "$RATIO" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name ratio))))
          "Type 15 should be $ratio")
      (ok (and float-t
               (string= "$FLOAT" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name float-t))))
          "Type 16 should be $float")
      (ok (and complex-t
               (string= "$COMPLEX" (symbol-name (clysm/compiler/codegen/gc-types:wasm-struct-type-name complex-t))))
          "Type 17 should be $complex")
      (ok (and limb-array
               (string= "$LIMB_ARRAY" (symbol-name (clysm/compiler/codegen/gc-types:wasm-array-type-name limb-array))))
          "Type 18 should be $limb_array"))))

;;; ============================================================
;;; Exception Tag Tests
;;; ============================================================

(deftest test-division-by-zero-tag
  "Verify division-by-zero exception tag is defined"
  (ok (= 0 clysm/compiler/codegen/gc-types:+tag-division-by-zero+)
      "+tag-division-by-zero+ should be 0")
  (let ((tag-wat (clysm/compiler/codegen/gc-types:emit-division-by-zero-tag)))
    (ok (stringp tag-wat)
        "emit-division-by-zero-tag should return a string")
    (ok (search "division-by-zero" tag-wat)
        "Tag WAT should contain 'division-by-zero'")))
