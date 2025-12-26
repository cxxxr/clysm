;;;; make-instance-test.lisp - Unit tests for make-instance AST and codegen
;;;; Feature: 026-clos-foundation (User Story 2)

(in-package #:clysm/tests/unit/clos-codegen)

;;; Unit tests for make-instance AST parsing and WasmGC codegen.

;;; ============================================================
;;; T037: make-instance AST parsing
;;; ============================================================

(deftest make-instance-ast-parsing
  "T037: make-instance form parses to ast-make-instance node"
  (testing "simple make-instance parses correctly"
    (let ((form '(make-instance 'point :x 10 :y 20)))
      (ok (fboundp 'clysm/compiler/ast:parse-make-instance-to-ast)
          "parse-make-instance-to-ast should be defined")
      (let ((ast (clysm/compiler/ast:parse-make-instance-to-ast form)))
        (ok (clysm/compiler/ast:ast-make-instance-p ast)
            "Result should be ast-make-instance")
        (ok (eq 'point (clysm/compiler/ast:ast-make-instance-class-name ast))
            "Class name should be POINT")
        (ok (= 2 (length (clysm/compiler/ast:ast-make-instance-initargs ast)))
            "Should have 2 initarg pairs"))))

  (testing "make-instance without initargs parses correctly"
    (let* ((form '(make-instance 'empty-class))
           (ast (clysm/compiler/ast:parse-make-instance-to-ast form)))
      (ok (eq 'empty-class (clysm/compiler/ast:ast-make-instance-class-name ast))
          "Class name should be EMPTY-CLASS")
      (ok (null (clysm/compiler/ast:ast-make-instance-initargs ast))
          "Should have no initargs"))))

;;; ============================================================
;;; T038: initarg matching to slot indices
;;; ============================================================

(deftest initarg-matching
  "T038: initargs are matched to correct slot indices"
  ;; First define a test class through AST/codegen path
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (let* ((form '(defclass test-point () ((x :initarg :x) (y :initarg :y))))
         (ast (clysm/compiler/ast:parse-defclass-to-ast form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    ;; Compile to register in compile-time registry
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env))

  (testing "initargs match slot indices"
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-point))
           (slots (clysm/compiler/codegen/func-section:class-info-slots class-info)))
      (ok (= 2 (length slots)) "Should have 2 slots")
      (let ((slot-x (find 'x slots :key #'clysm/compiler/codegen/func-section:slot-info-name))
            (slot-y (find 'y slots :key #'clysm/compiler/codegen/func-section:slot-info-name)))
        (ok (= 0 (clysm/compiler/codegen/func-section:slot-info-index slot-x))
            "Slot X should be at index 0")
        (ok (= 1 (clysm/compiler/codegen/func-section:slot-info-index slot-y))
            "Slot Y should be at index 1")))))

;;; ============================================================
;;; T039: initform evaluation when initarg not provided
;;; ============================================================

(deftest initform-evaluation
  "T039: initform is used when initarg not provided"
  ;; Define a class with initform through AST/codegen path
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (let* ((form '(defclass test-counter () ((count :initarg :count :initform 0))))
         (ast (clysm/compiler/ast:parse-defclass-to-ast form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    ;; Compile to register in compile-time registry
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env))

  (testing "slot with initform has correct info"
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-counter))
           (slots (clysm/compiler/codegen/func-section:class-info-slots class-info))
           (slot-count (first slots)))
      (ok (clysm/compiler/codegen/func-section:slot-info-initform-p slot-count)
          "Slot should have initform-p = T")
      (ok (eql 0 (clysm/compiler/codegen/func-section:slot-info-initform slot-count))
          "Initform should be 0"))))

;;; ============================================================
;;; T040: UNBOUND sentinel when no initarg/initform
;;; ============================================================

(deftest unbound-sentinel
  "T040: slots without initarg or initform get UNBOUND"
  ;; Define a class with no initarg or initform through AST/codegen path
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (let* ((form '(defclass test-bare () ((value))))
         (ast (clysm/compiler/ast:parse-defclass-to-ast form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    ;; Compile to register in compile-time registry
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env))

  (testing "slot without initform has initform-p = NIL"
    (let* ((class-info (clysm/compiler/codegen/func-section:find-compile-time-class 'test-bare))
           (slots (clysm/compiler/codegen/func-section:class-info-slots class-info))
           (slot-value (first slots)))
      (ok (not (clysm/compiler/codegen/func-section:slot-info-initform-p slot-value))
          "Slot should have initform-p = NIL")
      (ok (null (clysm/compiler/codegen/func-section:slot-info-initarg slot-value))
          "Slot should have no initarg"))))
