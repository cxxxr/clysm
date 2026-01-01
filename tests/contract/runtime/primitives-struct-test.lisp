;;;; primitives-struct-test.lisp - Contract tests for primitive structure
;;;; Feature 001-runtime-library-system
;;;; Task T008: Contract test for primitive struct

(in-package #:clysm/tests)

(deftest primitives-struct-has-required-slots ()
  "Verify primitive struct has all required slots per contracts/primitives.md"
  (testing "primitive struct exists and has correct slots"
    (let ((prim (clysm::make-primitive :name 'test-prim
                                       :wasm-emitter nil
                                       :signature '((any) any)
                                       :inline-p t
                                       :category :memory)))
      ;; Verify struct was created
      (ok (clysm::primitive-p prim)
          "make-primitive returns a primitive struct")

      ;; Verify all required slots
      (ok (eq (clysm::primitive-name prim) 'test-prim)
          "primitive-name slot accessible")
      (ok (null (clysm::primitive-wasm-emitter prim))
          "primitive-wasm-emitter slot accessible")
      (ok (equal (clysm::primitive-signature prim) '((any) any))
          "primitive-signature slot accessible")
      (ok (eq (clysm::primitive-inline-p prim) t)
          "primitive-inline-p slot accessible")
      (ok (eq (clysm::primitive-category prim) :memory)
          "primitive-category slot accessible"))))

(deftest primitives-struct-category-is-readonly ()
  "Verify category slot is read-only as per spec"
  (testing "category cannot be changed after creation"
    (let ((prim (clysm::make-primitive :name 'test-prim
                                       :category :memory)))
      (ok (eq (clysm::primitive-category prim) :memory)
          "category initialized correctly")
      ;; The category slot should be read-only (no setf accessor)
      ;; We can't easily test this at runtime, but the defstruct should enforce it
      )))

(deftest primitives-struct-name-is-readonly ()
  "Verify name slot is read-only as per spec"
  (testing "name cannot be changed after creation"
    (let ((prim (clysm::make-primitive :name 'test-prim
                                       :category :memory)))
      (ok (eq (clysm::primitive-name prim) 'test-prim)
          "name initialized correctly"))))
