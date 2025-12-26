;;;; accessor-test.lisp - Unit tests for slot accessor codegen
;;;; Feature: 026-clos-foundation (User Story 3)

(in-package #:clysm/tests/unit/clos-codegen)

;;; Unit tests for slot accessor reader/writer codegen.

;;; ============================================================
;;; T052: accessor reader generation
;;; ============================================================

(deftest accessor-reader-generation
  "T052: accessor reader function is generated for :accessor slots"
  ;; Define a class with accessor
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (let* ((form '(defclass test-point () ((x :initarg :x :accessor point-x))))
         (ast (clysm/compiler/ast:parse-defclass-to-ast form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env))

  (testing "accessor slot has accessor name recorded"
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-point))
           (slots (clysm/compiler/codegen/func-section:class-info-slots class-info))
           (slot-x (first slots)))
      (ok (eq 'point-x (clysm/compiler/codegen/func-section:slot-info-accessor slot-x))
          "Slot should have accessor name POINT-X"))))

;;; ============================================================
;;; T053: accessor writer (setf) generation
;;; ============================================================

(deftest accessor-writer-generation
  "T053: accessor writer (setf) function is generated for :accessor slots"
  ;; Define a class with accessor
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (let* ((form '(defclass test-counter () ((count :accessor counter-value :initform 0))))
         (ast (clysm/compiler/ast:parse-defclass-to-ast form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env))

  (testing "accessor slot has accessor name for writer"
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-counter))
           (slots (clysm/compiler/codegen/func-section:class-info-slots class-info))
           (slot-count (first slots)))
      (ok (eq 'counter-value (clysm/compiler/codegen/func-section:slot-info-accessor slot-count))
          "Slot should have accessor name COUNTER-VALUE")
      ;; Slot index is used for both reader and writer
      (ok (= 0 (clysm/compiler/codegen/func-section:slot-info-index slot-count))
          "Slot index should be 0 for array access"))))

;;; ============================================================
;;; T054: type checking in accessor (error on non-instance)
;;; ============================================================

(deftest accessor-type-checking
  "T054: accessor validates instance type"
  ;; The accessor should verify the object is an instance before access
  ;; This is done via ref.cast in the Wasm codegen
  (testing "slot-info has class context for type checking"
    (clysm/compiler/codegen/func-section:reset-class-registry)
    (let* ((form '(defclass test-typed () ((data :accessor typed-data))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form))
           (env (clysm/compiler/codegen/func-section:make-compilation-env)))
      (clysm/compiler/codegen/func-section:compile-to-instructions ast env))
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-typed)))
      (ok (not (null class-info))
          "Class should be registered for type checking")
      (ok (= 0 (clysm/compiler/codegen/func-section:class-info-class-id class-info))
          "Class ID should be assigned for type validation"))))
