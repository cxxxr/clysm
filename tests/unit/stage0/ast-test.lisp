;;;; ast-test.lisp - Unit tests for Stage 0 AST parsing
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests Core Infrastructure: AST parsing

(defpackage #:clysm/tests/unit/stage0/ast-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:parse-expression
                #:ast-to-ir))

(in-package #:clysm/tests/unit/stage0/ast-test)

;;; ============================================================
;;; T022: Unit test for AST parsing
;;; ============================================================

(deftest test-parse-literal-number
  "Verify parsing literal numbers"
  (let ((ast (parse-expression 42)))
    (ok (not (null ast)) "Should return AST node")
    (ok (or (numberp ast) (and (listp ast) (eq :literal (first ast))))
        "Should be literal or AST node")))

(deftest test-parse-literal-string
  "Verify parsing literal strings"
  (let ((ast (parse-expression "hello")))
    (ok (not (null ast)) "Should return AST node")))

(deftest test-parse-symbol-reference
  "Verify parsing symbol references"
  (let ((ast (parse-expression 'foo)))
    (ok (not (null ast)) "Should return AST node")))

(deftest test-parse-function-call
  "Verify parsing function calls"
  (let ((ast (parse-expression '(+ 1 2))))
    (ok (not (null ast)) "Should return AST node")
    (ok (listp ast) "Should be a list")))

(deftest test-parse-if-expression
  "Verify parsing if expressions"
  (let ((ast (parse-expression '(if t 1 2))))
    (ok (not (null ast)) "Should return AST node")))

(deftest test-parse-lambda
  "Verify parsing lambda expressions"
  (let ((ast (parse-expression '(lambda (x) x))))
    (ok (not (null ast)) "Should return AST node")))

(deftest test-parse-let
  "Verify parsing let expressions"
  (let ((ast (parse-expression '(let ((x 1)) x))))
    (ok (not (null ast)) "Should return AST node")))

(deftest test-parse-defun
  "Verify parsing defun forms"
  (let ((ast (parse-expression '(defun foo (x) (+ x 1)))))
    (ok (not (null ast)) "Should return AST node")))

;;; ============================================================
;;; AST to IR Tests
;;; ============================================================

(deftest test-ast-to-ir-returns-ir
  "Verify ast-to-ir returns IR representation"
  (let* ((ast (parse-expression '(+ 1 2)))
         (ir (ast-to-ir ast)))
    (ok (not (null ir)) "Should return IR")))
