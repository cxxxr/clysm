;;;; constant-export-test.lisp - Tests for type constant export and DEFCONSTANT handling
;;;; Feature: 001-type-package-export
;;;; HyperSpec: resources/HyperSpec/Body/m_defcon.htm

(defpackage #:clysm/tests/unit/constant-export-test
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler/codegen/gc-types
                #:+type-nil+
                #:+type-unbound+
                #:+type-cons+
                #:+type-symbol+
                #:+type-string+
                #:+type-closure+
                #:+type-instance+
                #:+type-standard-class+
                #:+type-func-0+
                #:+type-func-1+
                #:+type-func-2+
                #:+type-func-3+
                #:+type-func-n+
                #:+type-binding-frame+
                #:+type-bignum+
                #:+type-ratio+
                #:+type-float+
                #:+type-complex+
                #:+type-limb-array+
                #:+type-stream+
                #:+type-mv-array+
                #:+type-slot-vector+
                #:+type-keyword-array+
                #:+type-closure-array+
                #:+type-macro-environment+
                #:+type-hash-entry+
                #:+type-hash-table+
                #:+type-bucket-array+
                #:+type-mdarray+))

(in-package #:clysm/tests/unit/constant-export-test)

;;; ============================================================
;;; Phase 2: DEFCONSTANT Handling Tests (US4)
;;; ============================================================

(deftest test-constant-bindings-registry-exists
  "T005: Verify *constant-bindings* hash-table exists"
  (testing "constant-bindings registry should be a hash-table"
    ;; Will fail until T009 implements *constant-bindings*
    (ok (boundp 'clysm/compiler::*constant-bindings*)
        "*constant-bindings* should be defined")
    (ok (hash-table-p (symbol-value 'clysm/compiler::*constant-bindings*))
        "*constant-bindings* should be a hash-table")))

(deftest test-register-constant-function
  "T006: Verify register-constant function works"
  (testing "register-constant should add binding to registry"
    ;; Will fail until T010 implements register-constant
    (let ((test-sym (gensym "TEST-CONST-")))
      (clysm/compiler:register-constant test-sym 42)
      (ok (gethash test-sym clysm/compiler::*constant-bindings*)
          "Constant should be registered"))))

(deftest test-lookup-constant-function
  "T007: Verify lookup-constant function works"
  (testing "lookup-constant should retrieve registered values"
    ;; Will fail until T011 implements lookup-constant
    (let ((test-sym (gensym "LOOKUP-TEST-")))
      (clysm/compiler:register-constant test-sym 123)
      (multiple-value-bind (value found-p)
          (clysm/compiler:lookup-constant test-sym)
        (ok found-p "Constant should be found")
        (ok (= value 123) "Value should match registered value")))))

(deftest test-defconstant-form-compilation
  "T008: Integration test for DEFCONSTANT form compilation"
  (testing "DEFCONSTANT form should be processed at compile-time"
    ;; Will fail until T012-T013 implement handle-defconstant
    ;; This test verifies the directive handler returns :skipped
    (let ((form '(defconstant +test-const+ 99 "Test constant")))
      (ok (eq :skipped (clysm/compiler:compile-directive form nil))
          "DEFCONSTANT should return :skipped"))))

;;; ============================================================
;;; Phase 3: Type Constant Export Tests (US1)
;;; ============================================================

(deftest test-type-constant-symbol-exports
  "T015: Verify type constants are exported from :clysm package"
  (testing "All 28 type constants should be accessible"
    ;; Core types
    (ok (integerp +type-nil+) "+TYPE-NIL+ should be an integer")
    (ok (integerp +type-cons+) "+TYPE-CONS+ should be an integer")
    (ok (integerp +type-symbol+) "+TYPE-SYMBOL+ should be an integer")
    (ok (integerp +type-string+) "+TYPE-STRING+ should be an integer")
    (ok (integerp +type-closure+) "+TYPE-CLOSURE+ should be an integer")

    ;; Verify expected values
    (ok (= +type-nil+ 0) "+TYPE-NIL+ should be 0")
    (ok (= +type-cons+ 2) "+TYPE-CONS+ should be 2")
    (ok (= +type-symbol+ 3) "+TYPE-SYMBOL+ should be 3")
    (ok (= +type-mdarray+ 28) "+TYPE-MDARRAY+ should be 28")))

(deftest test-constant-folding-i32-const
  "T016: Verify constant folding emits i32.const"
  (testing "Constant reference should compile to i32.const instruction"
    ;; Tests T019-T020 constant folding implementation
    ;; Parse the form into AST, then compile to instructions
    ;; The constant +TYPE-CONS+ should be folded to i32.const 2
    (let* ((form 'clysm/compiler/codegen/gc-types:+type-cons+)
           (ast (clysm/compiler/ast:parse-expr form))
           ;; Compile and check for i32.const instruction
           (instructions (clysm/compiler/codegen/func-section:compile-to-instructions
                          ast
                          (clysm/compiler/codegen/func-section:make-env))))
      (ok (member '(:i32.const 2) instructions :test #'equal)
          "Should emit (i32.const 2) for +TYPE-CONS+"))))

;;; ============================================================
;;; Helper for running all tests
;;; ============================================================

(defun run-all-tests ()
  "Run all constant export tests"
  (run-suite :clysm/tests/unit/constant-export-test))
