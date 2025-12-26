;;;; defclass-test.lisp - Unit tests for defclass AST and codegen
;;;; Feature: 026-clos-foundation (User Story 1, 6)

(in-package #:clysm/tests/unit/clos-codegen)

;;; Unit tests for defclass AST parsing and WasmGC codegen.
;;; Tests follow TDD: written first, must fail, then implementation makes them pass.

;;; ============================================================
;;; T021: defclass AST parsing
;;; ============================================================

(deftest defclass-ast-parsing
  "T021: defclass form parses to ast-defclass node"
  (testing "simple defclass parses correctly"
    (let ((form '(defclass point () ((x) (y)))))
      (ok (fboundp 'clysm/compiler/ast:parse-defclass-to-ast)
          "parse-defclass-to-ast should be defined")
      (let ((ast (clysm/compiler/ast:parse-defclass-to-ast form)))
        (ok (typep ast 'clysm/compiler/ast:ast-defclass)
            "Result should be ast-defclass")
        (ok (eq 'point (clysm/compiler/ast:ast-defclass-name ast))
            "Class name should be POINT")
        (ok (null (clysm/compiler/ast:ast-defclass-superclass ast))
            "Superclass should be NIL for no parent")
        (ok (= 2 (length (clysm/compiler/ast:ast-defclass-slots ast)))
            "Should have 2 slots"))))

  (testing "defclass with superclass parses correctly"
    (let* ((form '(defclass colored-point (point) ((color))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form)))
      (ok (eq 'colored-point (clysm/compiler/ast:ast-defclass-name ast))
          "Class name should be COLORED-POINT")
      (ok (eq 'point (clysm/compiler/ast:ast-defclass-superclass ast))
          "Superclass should be POINT")
      (ok (= 1 (length (clysm/compiler/ast:ast-defclass-slots ast)))
          "Should have 1 slot (excluding inherited)"))))

;;; ============================================================
;;; T022: slot-definition creation with :initarg/:accessor/:initform
;;; ============================================================

(deftest slot-definition-creation
  "T022: slot-definition with slot options"
  (testing "slot with :initarg parses correctly"
    (let* ((form '(defclass point () ((x :initarg :x))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form))
           (slots (clysm/compiler/ast:ast-defclass-slots ast))
           (slot-x (first slots)))
      (ok (eq 'x (clysm/compiler/ast:ast-slot-definition-name slot-x))
          "Slot name should be X")
      (ok (eq :x (clysm/compiler/ast:ast-slot-definition-initarg slot-x))
          "Initarg should be :X")))

  (testing "slot with :accessor parses correctly"
    (let* ((form '(defclass point () ((x :accessor point-x))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form))
           (slots (clysm/compiler/ast:ast-defclass-slots ast))
           (slot-x (first slots)))
      (ok (eq 'point-x (clysm/compiler/ast:ast-slot-definition-accessor slot-x))
          "Accessor should be POINT-X")))

  (testing "slot with :initform parses correctly"
    (let* ((form '(defclass point () ((x :initform 0))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form))
           (slots (clysm/compiler/ast:ast-defclass-slots ast))
           (slot-x (first slots)))
      (ok (clysm/compiler/ast:ast-slot-definition-initform-p slot-x)
          "Should have initform")
      (ok (eql 0 (clysm/compiler/ast:ast-slot-definition-initform slot-x))
          "Initform should be 0")))

  (testing "slot with all options parses correctly"
    (let* ((form '(defclass point () ((x :initarg :x :accessor point-x :initform 0))))
           (ast (clysm/compiler/ast:parse-defclass-to-ast form))
           (slots (clysm/compiler/ast:ast-defclass-slots ast))
           (slot-x (first slots)))
      (ok (eq 'x (clysm/compiler/ast:ast-slot-definition-name slot-x))
          "Slot name should be X")
      (ok (eq :x (clysm/compiler/ast:ast-slot-definition-initarg slot-x))
          "Initarg should be :X")
      (ok (eq 'point-x (clysm/compiler/ast:ast-slot-definition-accessor slot-x))
          "Accessor should be POINT-X")
      (ok (clysm/compiler/ast:ast-slot-definition-initform-p slot-x)
          "Should have initform")
      (ok (eql 0 (clysm/compiler/ast:ast-slot-definition-initform slot-x))
          "Initform should be 0"))))

;;; ============================================================
;;; T023: class registration in class registry
;;; ============================================================

(deftest class-registry
  "T023: class registration in compile-time registry"
  (testing "defclass registers class in registry"
    ;; Use existing CLOS infrastructure
    (let ((form '(defclass test-class-23 () ((a) (b)))))
      ;; Parse and register
      (clysm/clos/defclass:define-class* form)
      ;; Check registry
      (let ((class (clysm/clos/mop:find-class* 'test-class-23)))
        (ok (not (null class))
            "Class should be registered")
        (ok (eq 'test-class-23 (clysm/clos/mop:class-name class))
            "Class name should match")))))

;;; ============================================================
;;; T024: class precedence list computation
;;; ============================================================

(deftest class-precedence-list
  "T024: class precedence list computation"
  (testing "CPL includes class itself"
    (clysm/clos/defclass:define-class* '(defclass test-class-24a () ()))
    (let ((class (clysm/clos/mop:find-class* 'test-class-24a)))
      (clysm/clos/mop:compute-class-precedence-list class)
      (let ((cpl (clysm/clos/mop:class-precedence-list class)))
        (ok (member class cpl :test #'eq)
            "CPL should include the class itself"))))

  (testing "CPL orders subclass before superclass"
    (clysm/clos/defclass:define-class* '(defclass test-parent-24 () ((a))))
    (clysm/clos/defclass:define-class* '(defclass test-child-24 (test-parent-24) ((b))))
    (let* ((parent (clysm/clos/mop:find-class* 'test-parent-24))
           (child (clysm/clos/mop:find-class* 'test-child-24)))
      (clysm/clos/mop:compute-class-precedence-list parent)
      (clysm/clos/mop:compute-class-precedence-list child)
      (let ((cpl (clysm/clos/mop:class-precedence-list child)))
        (ok (< (position child cpl) (position parent cpl))
            "Child should come before parent in CPL")))))

;;; ============================================================
;;; Phase 7 (US6) Tests - Single Inheritance
;;; ============================================================

(deftest superclass-resolution
  "T085: superclass is resolved from compile-time registry"
  ;; Reset registries for clean test
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (clysm/compiler/codegen/func-section:reset-class-id-counter)

  (testing "parent class is registered first"
    (let* ((parent-form '(defclass t085-parent () ((x :initarg :x))))
           (parent-ast (clysm/compiler/ast:parse-defclass-to-ast parent-form))
           (env (clysm/compiler/codegen/func-section:make-compilation-env)))
      (clysm/compiler/codegen/func-section:compile-to-instructions parent-ast env)
      (ok (clysm/compiler/codegen/func-section:find-compile-time-class 't085-parent)
          "Parent class should be registered")))

  (testing "child class resolves parent correctly"
    (let* ((child-form '(defclass t085-child (t085-parent) ((y :initarg :y))))
           (child-ast (clysm/compiler/ast:parse-defclass-to-ast child-form))
           (env (clysm/compiler/codegen/func-section:make-compilation-env)))
      (clysm/compiler/codegen/func-section:compile-to-instructions child-ast env)
      (let ((child-info (clysm/compiler/codegen/func-section:find-compile-time-class 't085-child)))
        (ok child-info "Child class should be registered")
        (ok (eq 't085-parent (clysm/compiler/codegen/func-section:class-info-superclass child-info))
            "Child superclass should be T085-PARENT")))))

(deftest slot-inheritance
  "T086: child class inherits slots from parent"
  ;; Reset registries for clean test
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (clysm/compiler/codegen/func-section:reset-class-id-counter)

  ;; Define parent with 2 slots
  (let* ((parent-form '(defclass t086-parent () ((a :initarg :a) (b :initarg :b))))
         (parent-ast (clysm/compiler/ast:parse-defclass-to-ast parent-form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions parent-ast env))

  ;; Define child with 1 slot
  (let* ((child-form '(defclass t086-child (t086-parent) ((c :initarg :c))))
         (child-ast (clysm/compiler/ast:parse-defclass-to-ast child-form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions child-ast env))

  (testing "child has inherited + own slots"
    (let* ((child-info (clysm/compiler/codegen/func-section:find-compile-time-class 't086-child))
           (slots (clysm/compiler/codegen/func-section:class-info-slots child-info)))
      (ok (= 3 (length slots))
          "Child should have 3 slots (2 inherited + 1 own)")
      ;; Inherited slots come first
      (ok (eq 'a (clysm/compiler/codegen/func-section:slot-info-name (first slots)))
          "First slot should be A (inherited)")
      (ok (eq 'b (clysm/compiler/codegen/func-section:slot-info-name (second slots)))
          "Second slot should be B (inherited)")
      (ok (eq 'c (clysm/compiler/codegen/func-section:slot-info-name (third slots)))
          "Third slot should be C (own)")
      ;; Indices should be recomputed
      (ok (= 0 (clysm/compiler/codegen/func-section:slot-info-index (first slots)))
          "Slot A should be at index 0")
      (ok (= 1 (clysm/compiler/codegen/func-section:slot-info-index (second slots)))
          "Slot B should be at index 1")
      (ok (= 2 (clysm/compiler/codegen/func-section:slot-info-index (third slots)))
          "Slot C should be at index 2"))))

(deftest duplicate-slot-detection
  "T087: duplicate slot names across hierarchy are detected"
  ;; Reset registries for clean test
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (clysm/compiler/codegen/func-section:reset-class-id-counter)

  ;; Define parent with slot X
  (let* ((parent-form '(defclass t087-parent () ((x :initarg :x))))
         (parent-ast (clysm/compiler/ast:parse-defclass-to-ast parent-form))
         (env (clysm/compiler/codegen/func-section:make-compilation-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions parent-ast env))

  (testing "child with duplicate slot X should shadow parent slot"
    ;; In CL, child slots shadow parent slots with the same name
    ;; The slot count should be 1, not 2
    (let* ((child-form '(defclass t087-child (t087-parent) ((x :initarg :x-new))))
           (child-ast (clysm/compiler/ast:parse-defclass-to-ast child-form))
           (env (clysm/compiler/codegen/func-section:make-compilation-env)))
      (clysm/compiler/codegen/func-section:compile-to-instructions child-ast env)
      (let* ((child-info (clysm/compiler/codegen/func-section:find-compile-time-class 't087-child))
             (slots (clysm/compiler/codegen/func-section:class-info-slots child-info)))
        ;; Shadowed: parent slot is replaced by child slot
        (ok (= 1 (length slots))
            "Child should have 1 slot (shadowed)")
        (ok (eq :x-new (clysm/compiler/codegen/func-section:slot-info-initarg (first slots)))
            "Slot should have child's initarg :X-NEW")))))

(deftest multiple-inheritance-rejection
  "T088: multiple inheritance is rejected with error"
  ;; Reset registries for clean test
  (clysm/compiler/codegen/func-section:reset-class-registry)
  (clysm/compiler/codegen/func-section:reset-class-id-counter)

  ;; AST parsing should reject multiple superclasses
  (testing "parse-defclass-to-ast rejects multiple superclasses"
    (let ((form '(defclass t088-child (parent1 parent2) ((x :initarg :x)))))
      (ok (handler-case
              (progn
                (clysm/compiler/ast:parse-defclass-to-ast form)
                nil) ; No error = test fails
            (error (c)
              (declare (ignore c))
              t))   ; Error caught = test passes
          "Multiple superclasses should signal an error"))))
