;;;; internal-export-test.lisp - Unit tests for internal function exports
;;;; Feature: 001-internal-function-export
;;;; Phase 13D: Verify internal compiler functions accessible via clysm package

(in-package #:clysm/tests/unit/internal-export)

;;; ============================================================
;;; User Story 1: Export Internal Compiler Functions (P1)
;;; These tests MUST FAIL before implementation
;;; ============================================================

;;; Helper to check if symbol is accessible in clysm package
(defun symbol-accessible-p (name)
  "Return T if NAME is accessible (external or inherited) in the clysm package."
  (multiple-value-bind (symbol status)
      (find-symbol name "CLYSM")
    (declare (ignore symbol))
    (member status '(:external :inherited))))

;;; T009: Test lexical-env-parent accessibility
(deftest lexical-env-parent-accessible
  (ok (symbol-accessible-p "LEXICAL-ENV-PARENT")
      "LEXICAL-ENV-PARENT should be accessible from clysm package"))

;;; T010: Test compile-to-instructions accessibility
(deftest compile-to-instructions-accessible
  (ok (symbol-accessible-p "COMPILE-TO-INSTRUCTIONS")
      "COMPILE-TO-INSTRUCTIONS should be accessible from clysm package"))

;;; T011: Test make-wasm-struct-type accessibility
(deftest make-wasm-struct-type-accessible
  (ok (symbol-accessible-p "MAKE-WASM-STRUCT-TYPE")
      "MAKE-WASM-STRUCT-TYPE should be accessible from clysm package"))

;;; T012: Test ast-literal-value accessibility
(deftest ast-literal-value-accessible
  (ok (symbol-accessible-p "AST-LITERAL-VALUE")
      "AST-LITERAL-VALUE should be accessible from clysm package"))

;;; ============================================================
;;; User Story 1: Additional Environment Functions
;;; ============================================================

(deftest lexical-env-bindings-accessible
  (ok (symbol-accessible-p "LEXICAL-ENV-BINDINGS")
      "LEXICAL-ENV-BINDINGS should be accessible from clysm package"))

(deftest make-lexical-env-accessible
  (ok (symbol-accessible-p "MAKE-LEXICAL-ENV")
      "MAKE-LEXICAL-ENV should be accessible from clysm package"))

;;; ============================================================
;;; User Story 1: Additional GC Types Functions
;;; ============================================================

(deftest wasm-struct-type-p-accessible
  (ok (symbol-accessible-p "WASM-STRUCT-TYPE-P")
      "WASM-STRUCT-TYPE-P should be accessible from clysm package"))

(deftest wasm-struct-type-fields-accessible
  (ok (symbol-accessible-p "WASM-STRUCT-TYPE-FIELDS")
      "WASM-STRUCT-TYPE-FIELDS should be accessible from clysm package"))

;;; ============================================================
;;; User Story 1: Additional AST Functions
;;; ============================================================

(deftest ast-literal-p-accessible
  (ok (symbol-accessible-p "AST-LITERAL-P")
      "AST-LITERAL-P should be accessible from clysm package"))

;;; ============================================================
;;; User Story 4: Previously Consolidated Functions (P4)
;;; These should already pass (already re-exported per research)
;;; ============================================================

;;; T037: Test compile-unary-math-ffi accessibility
(deftest compile-unary-math-ffi-accessible
  (ok (symbol-accessible-p "COMPILE-UNARY-MATH-FFI")
      "COMPILE-UNARY-MATH-FFI should be accessible from clysm package"))

;;; T038: Test compile-cxr-chain accessibility
(deftest compile-cxr-chain-accessible
  (ok (symbol-accessible-p "COMPILE-CXR-CHAIN")
      "COMPILE-CXR-CHAIN should be accessible from clysm package"))
