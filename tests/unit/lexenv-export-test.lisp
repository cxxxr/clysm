;;;; lexenv-export-test.lisp - Tests for lexical environment function exports
;;;; Feature: 001-lexenv-function-export
;;;; TDD Phase: Tests written before implementation

(defpackage #:clysm/tests/unit/lexenv-export
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/lexenv-export)

;;; ============================================================
;;; Phase 2: Foundational Tests (T004-T006)
;;; These tests must FAIL before implementation, PASS after
;;; ============================================================

;;; T004: Test env-add-local accessibility
(deftest env-add-local-export-test
  (testing "env-add-local is exported from clysm package"
    (multiple-value-bind (symbol status)
        (find-symbol "ENV-ADD-LOCAL" :clysm)
      (ok symbol "ENV-ADD-LOCAL symbol should exist in clysm package")
      (ok (eq status :external) "ENV-ADD-LOCAL should be :EXTERNAL")))

  (testing "env-add-local is a function"
    (let ((sym (find-symbol "ENV-ADD-LOCAL" :clysm)))
      (when sym
        (ok (fboundp sym) "ENV-ADD-LOCAL should be bound to a function")))))

;;; T005: Test loop-keyword-eq accessibility
(deftest loop-keyword-eq-export-test
  (testing "loop-keyword-eq is exported from clysm package"
    (multiple-value-bind (symbol status)
        (find-symbol "LOOP-KEYWORD-EQ" :clysm)
      (ok symbol "LOOP-KEYWORD-EQ symbol should exist in clysm package")
      (ok (eq status :external) "LOOP-KEYWORD-EQ should be :EXTERNAL")))

  (testing "loop-keyword-eq is a function"
    (let ((sym (find-symbol "LOOP-KEYWORD-EQ" :clysm)))
      (when sym
        (ok (fboundp sym) "LOOP-KEYWORD-EQ should be bound to a function")))))

;;; T006: Test numeric-literal-p accessibility
(deftest numeric-literal-p-export-test
  (testing "numeric-literal-p is exported from clysm package"
    (multiple-value-bind (symbol status)
        (find-symbol "NUMERIC-LITERAL-P" :clysm)
      (ok symbol "NUMERIC-LITERAL-P symbol should exist in clysm package")
      (ok (eq status :external) "NUMERIC-LITERAL-P should be :EXTERNAL")))

  (testing "numeric-literal-p is a function"
    (let ((sym (find-symbol "NUMERIC-LITERAL-P" :clysm)))
      (when sym
        (ok (fboundp sym) "NUMERIC-LITERAL-P should be bound to a function")))))

;;; ============================================================
;;; Phase 4: Runtime Table Tests (T014-T016)
;;; Tests for runtime function table registration
;;; ============================================================

;;; T014: Test env-add-local runtime table entry
(deftest env-add-local-runtime-table-test
  (testing "env-add-local is registered in runtime function table"
    (let ((entry (clysm/compiler/codegen/func-section::runtime-function-p
                  'clysm:env-add-local)))
      (ok entry "ENV-ADD-LOCAL should be in runtime function table")
      (when entry
        (ok (null (cdr entry)) "ENV-ADD-LOCAL arity should be NIL (variadic)")))))

;;; T015: Test loop-keyword-eq runtime table entry
(deftest loop-keyword-eq-runtime-table-test
  (testing "loop-keyword-eq is registered in runtime function table"
    (let ((entry (clysm/compiler/codegen/func-section::runtime-function-p
                  'clysm:loop-keyword-eq)))
      (ok entry "LOOP-KEYWORD-EQ should be in runtime function table")
      (when entry
        (ok (eq (cdr entry) 2) "LOOP-KEYWORD-EQ arity should be 2")))))

;;; T016: Test numeric-literal-p runtime table entry
(deftest numeric-literal-p-runtime-table-test
  (testing "numeric-literal-p is registered in runtime function table"
    (let ((entry (clysm/compiler/codegen/func-section::runtime-function-p
                  'clysm:numeric-literal-p)))
      (ok entry "NUMERIC-LITERAL-P should be in runtime function table")
      (when entry
        (ok (eq (cdr entry) 1) "NUMERIC-LITERAL-P arity should be 1")))))
