;;;; tests/e2e/package.lisp - E2E Test Package Definition
;;;;
;;;; End-to-end tests that compile Lisp to Wasm and verify actual results.

(in-package #:clysm/tests)

;;; E2E tests use the same package as unit tests (clysm/tests)
;;; This file exists for organizational purposes and to document the e2e test structure.

;;; E2E Test Suites:
;;; - e2e-literals-suite: Data types and literals
;;; - e2e-arithmetic-suite: Arithmetic operations
;;; - e2e-comparison-suite: Comparison operators
;;; - e2e-cons-suite: Cons cell operations
;;; - e2e-predicates-suite: Type predicates
;;; - e2e-equality-suite: Equality operations
;;; - e2e-symbol-suite: Symbol operations
;;; - e2e-if-suite: Conditional expressions
;;; - e2e-let-suite: Variable binding
;;; - e2e-block-suite: Block/return-from
;;; - e2e-tagbody-suite: Tagbody/go
;;; - e2e-catch-suite: Catch/throw
;;; - e2e-unwind-suite: Unwind-protect
;;; - e2e-defun-suite: Function definitions
;;; - e2e-lambda-suite: Lambda expressions
;;; - e2e-closure-suite: Closures
;;; - e2e-macro-suite: Macro expansion
;;; - e2e-special-var-suite: Dynamic scoping
;;; - e2e-vector-suite: Vector operations
;;; - e2e-error-suite: Error handling
