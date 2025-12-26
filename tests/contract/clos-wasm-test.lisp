;;;; clos-wasm-test.lisp - Contract tests for CLOS WasmGC type generation
;;;; Feature: 026-clos-foundation

(in-package #:clysm/tests/contract/clos-wasm)

;;; Contract tests verify that generated Wasm modules:
;;; 1. Validate with wasm-tools validate
;;; 2. Have correct type section structure for CLOS types
;;; 3. Include proper WasmGC struct/array definitions

;;; ============================================================
;;; Phase 2: Foundational WasmGC Type Tests
;;; T005-T009: Type structure validation
;;; ============================================================

(deftest instance-type-structure
  "T005: $instance type (index 6) has correct structure"
  (testing "$instance type is a struct"
    (let ((instance-type (clysm/compiler/codegen/gc-types:make-instance-type)))
      (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p instance-type)
          "$instance should be a struct type")))

  (testing "$instance has correct type index"
    (let ((instance-type (clysm/compiler/codegen/gc-types:make-instance-type)))
      (ok (= 6 (clysm/compiler/codegen/gc-types:gc-type-index instance-type))
          "$instance should have type index 6")))

  (testing "$instance has 2 fields (class, slots)"
    (let* ((instance-type (clysm/compiler/codegen/gc-types:make-instance-type))
           (fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields instance-type)))
      (ok (= 2 (length fields))
          "$instance should have 2 fields")))

  (testing "$instance fields have correct types"
    (let* ((instance-type (clysm/compiler/codegen/gc-types:make-instance-type))
           (fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields instance-type)))
      ;; Field 0: $class (ref $standard-class)
      (ok (eq :standard-class-ref (clysm/compiler/codegen/gc-types:wasm-field-type (first fields)))
          "Field 0 should be $class (ref $standard-class)")
      ;; Field 1: $slots (ref $slot-vector)
      (ok (eq :slot-vector-ref (clysm/compiler/codegen/gc-types:wasm-field-type (second fields)))
          "Field 1 should be $slots (ref $slot-vector)"))))

(deftest standard-class-type-structure
  "T006: $standard-class type (index 7) has correct structure"
  (testing "$standard-class type is a struct"
    (let ((class-type (clysm/compiler/codegen/gc-types:make-standard-class-type)))
      (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p class-type)
          "$standard-class should be a struct type")))

  (testing "$standard-class has correct type index"
    (let ((class-type (clysm/compiler/codegen/gc-types:make-standard-class-type)))
      (ok (= 7 (clysm/compiler/codegen/gc-types:gc-type-index class-type))
          "$standard-class should have type index 7")))

  (testing "$standard-class has 6 fields"
    (let* ((class-type (clysm/compiler/codegen/gc-types:make-standard-class-type))
           (fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields class-type)))
      (ok (= 6 (length fields))
          "$standard-class should have 6 fields")))

  (testing "$standard-class fields have correct names"
    (let* ((class-type (clysm/compiler/codegen/gc-types:make-standard-class-type))
           (fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields class-type))
           (field-names (mapcar (lambda (f)
                                  (symbol-name (clysm/compiler/codegen/gc-types:wasm-field-name f)))
                                fields)))
      (ok (equal '("NAME" "SUPERCLASS" "SLOT_COUNT" "INITARGS" "INITFORMS" "CLASS_ID") field-names)
          "$standard-class fields: name, superclass, slot_count, initargs, initforms, class_id"))))

(deftest slot-vector-type-structure
  "T007: $slot-vector array type (index 21) has correct structure"
  (testing "$slot-vector type is an array"
    (let ((sv-type (clysm/compiler/codegen/gc-types:make-slot-vector-type)))
      (ok (clysm/compiler/codegen/gc-types:wasm-array-type-p sv-type)
          "$slot-vector should be an array type")))

  (testing "$slot-vector has correct type index"
    (let ((sv-type (clysm/compiler/codegen/gc-types:make-slot-vector-type)))
      (ok (= 21 (clysm/compiler/codegen/gc-types:gc-type-index sv-type))
          "$slot-vector should have type index 21")))

  (testing "$slot-vector has mutable anyref elements"
    (let ((sv-type (clysm/compiler/codegen/gc-types:make-slot-vector-type)))
      (ok (eq :anyref (clysm/compiler/codegen/gc-types:wasm-array-type-element-type sv-type))
          "$slot-vector element type should be anyref")
      (ok (clysm/compiler/codegen/gc-types:wasm-array-type-mutable sv-type)
          "$slot-vector should be mutable"))))

(deftest keyword-array-type-structure
  "T008: $keyword-array type (index 22) has correct structure"
  (testing "$keyword-array type is an array"
    (let ((ka-type (clysm/compiler/codegen/gc-types:make-keyword-array-type)))
      (ok (clysm/compiler/codegen/gc-types:wasm-array-type-p ka-type)
          "$keyword-array should be an array type")))

  (testing "$keyword-array has correct type index"
    (let ((ka-type (clysm/compiler/codegen/gc-types:make-keyword-array-type)))
      (ok (= 22 (clysm/compiler/codegen/gc-types:gc-type-index ka-type))
          "$keyword-array should have type index 22")))

  (testing "$keyword-array has symbol references"
    (let ((ka-type (clysm/compiler/codegen/gc-types:make-keyword-array-type)))
      (ok (eq :symbol-ref (clysm/compiler/codegen/gc-types:wasm-array-type-element-type ka-type))
          "$keyword-array element type should be (ref $symbol)"))))

(deftest closure-array-type-structure
  "T009: $closure-array type (index 23) has correct structure"
  (testing "$closure-array type is an array"
    (let ((ca-type (clysm/compiler/codegen/gc-types:make-closure-array-type)))
      (ok (clysm/compiler/codegen/gc-types:wasm-array-type-p ca-type)
          "$closure-array should be an array type")))

  (testing "$closure-array has correct type index"
    (let ((ca-type (clysm/compiler/codegen/gc-types:make-closure-array-type)))
      (ok (= 23 (clysm/compiler/codegen/gc-types:gc-type-index ca-type))
          "$closure-array should have type index 23")))

  (testing "$closure-array has nullable closure references"
    (let ((ca-type (clysm/compiler/codegen/gc-types:make-closure-array-type)))
      (ok (eq :closure-ref-null (clysm/compiler/codegen/gc-types:wasm-array-type-element-type ca-type))
          "$closure-array element type should be (ref null $closure)"))))

;;; ============================================================
;;; Type Section Integration Test
;;; ============================================================

(deftest clos-types-in-type-definitions
  "Verify CLOS types are included in generate-type-definitions"
  (let ((types (clysm/compiler/codegen/gc-types:generate-type-definitions)))
    (testing "$instance is at index 6"
      (let ((instance-type (nth 6 types)))
        (ok (not (null instance-type))
            "$instance should not be nil placeholder")
        (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p instance-type)
            "$instance should be a struct type")))

    (testing "$standard-class is at index 7"
      (let ((class-type (nth 7 types)))
        (ok (not (null class-type))
            "$standard-class should not be nil placeholder")
        (ok (clysm/compiler/codegen/gc-types:wasm-struct-type-p class-type)
            "$standard-class should be a struct type")))

    (testing "CLOS array types are present"
      (ok (>= (length types) 24)
          "Type list should include up to index 23"))))

;;; ============================================================
;;; Phase 3+: User Story Implementation Contract Tests
;;; (Placeholder - to be implemented later)
;;; ============================================================

;;; Phase 3: User Story 1 - defclass codegen tests
(deftest class-global-emission
  "T025: class global ($class-point) emission"
  (testing "defclass produces valid Wasm module"
    ;; Test that compiling a defclass form produces Wasm that validates
    (let* ((form '(defclass point () ((x :initarg :x) (y :initarg :y))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form)))
      (ok ast "defclass should parse to AST")
      ;; Note: Full codegen test will be added when compile-defclass is implemented
      ;; For now, verify the AST structure is correct for codegen
      (ok (= 2 (length (clysm/compiler/ast:ast-defclass-slots ast)))
          "Should have 2 slots for codegen")
      (let ((slot-x (first (clysm/compiler/ast:ast-defclass-slots ast))))
        (ok (eq :x (clysm/compiler/ast:ast-slot-definition-initarg slot-x))
            "First slot should have :x initarg for initarg matching")))))

;;; Phase 4: User Story 2 - make-instance codegen tests
(deftest instance-creation-codegen-placeholder
  "T041: instance creation codegen (struct.new $instance)"
  (skip "Placeholder - implement in Phase 4 (US2)"))

;;; Phase 5: User Story 3 - accessor codegen tests
(deftest slot-read-codegen-placeholder
  "T055: slot read codegen (array.get $slot-vector)"
  (skip "Placeholder - implement in Phase 5 (US3)"))

(deftest slot-write-codegen-placeholder
  "T056: slot write codegen (array.set $slot-vector)"
  (skip "Placeholder - implement in Phase 5 (US3)"))

;;; Phase 6: User Stories 4+5 - method dispatch codegen tests
(deftest dispatch-codegen-placeholder
  "T070: dispatch codegen (class-id lookup + call)"
  (skip "Placeholder - implement in Phase 6 (US4+5)"))

;;; Phase 7: User Story 6 - inheritance codegen tests
(deftest superclass-field-linking-placeholder
  "T089: $superclass field linking in class global"
  (skip "Placeholder - implement in Phase 7 (US6)"))
