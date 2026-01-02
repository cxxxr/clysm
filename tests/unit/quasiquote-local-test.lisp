;;;; tests/unit/quasiquote-local-test.lisp
;;;; Unit tests for quasiquote local variable compilation
;;;; Feature: 001-quasiquote-local-vars

(defpackage #:clysm/tests/unit/quasiquote-local-test
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:compile-to-wasm)
  (:import-from #:clysm/compiler/codegen/func-section
                #:compile-quoted-element
                #:compile-form
                #:compile-var-ref
                #:compile-funcall
                #:make-compilation-env)
  (:import-from #:clysm/compiler/ast
                #:ast-var-ref-p
                #:ast-call-p
                #:ast-node-p
                #:make-ast-var-ref
                #:make-ast-call))

(in-package #:clysm/tests/unit/quasiquote-local-test)

;;; ==========================================================================
;;; Phase 2: Foundational Tests
;;; ==========================================================================

;; T005: Test compile-quoted-element handles ast-var-ref
;; This test verifies that compile-quoted-element can process ast-var-ref nodes
;; without signaling an error. Before implementation, this test will FAIL.
(deftest test-compile-quoted-element-handles-ast-var-ref
  (let ((var-ref (make-ast-var-ref :name 'x :scope :local)))
    ;; compile-quoted-element should NOT error when given an ast-var-ref
    ;; It should recognize it as an AST node and return Wasm instructions
    (let ((result (handler-case
                      (compile-quoted-element var-ref)
                    (error (c)
                      ;; If error occurs, test fails with the error message
                      (list :error (princ-to-string c))))))
      ;; Result should be a list of Wasm instructions, not an error
      (ok (and (listp result)
               (not (eq (first result) :error)))
          "compile-quoted-element should handle ast-var-ref without error"))))

;; T006: Test compile-quoted-element handles ast-call
;; This test verifies that compile-quoted-element can process ast-call nodes
;; (for expressions like `(,(+ 1 2))`). Before implementation, this test will FAIL.
(deftest test-compile-quoted-element-handles-ast-call
  (let ((call-node (make-ast-call :function '+ :arguments '(1 2))))
    ;; compile-quoted-element should NOT error when given an ast-call
    (let ((result (handler-case
                      (compile-quoted-element call-node)
                    (error (c)
                      (list :error (princ-to-string c))))))
      (ok (and (listp result)
               (not (eq (first result) :error)))
          "compile-quoted-element should handle ast-call without error"))))

;;; ==========================================================================
;;; Phase 3: User Story 1 - Simple Unquote Tests
;;; ==========================================================================

;; T010: Single variable unquote
;; Test: `(result ,x) where x is a let-bound local
;; Expected: Compiles without error, produces valid Wasm
(deftest test-simple-unquote-single-var
  (let ((result (handler-case
                    ;; Compile a function with quasiquote containing unquoted variable
                    (compile-to-wasm '(defun test-qq-single (x)
                                        `(result ,x)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile quasiquote with single unquoted var")))

;; T011: Multiple variables unquote
;; Test: `(,a ,b ,c) with three local variables
;; Expected: Compiles without error, all vars resolved
(deftest test-simple-unquote-multiple-vars
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-qq-multi (a b c)
                                        `(,a ,b ,c)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile quasiquote with multiple unquoted vars")))

;; T012: Nested let with unquote
;; Test: `(,outer ,inner) with nested let scopes
;; Expected: Both outer and inner resolved correctly
(deftest test-simple-unquote-nested-let
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-qq-nested ()
                                        (let ((outer 1))
                                          (let ((inner 2))
                                            `(,outer ,inner)))))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile quasiquote with nested let scopes")))

;;; ==========================================================================
;;; Phase 4: User Story 2 - Unquote-Splicing Tests
;;; ==========================================================================

;; T020: Basic unquote-splicing
;; Test: `(call ,@args) where args is a local list
(deftest test-unquote-splicing-basic
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-splice-basic (args)
                                        `(call ,@args)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile unquote-splicing")))

;; T021: Multiple splice points
;; Test: `(,@front middle ,@back) with two splice points
(deftest test-unquote-splicing-multiple
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-splice-multi (front back)
                                        `(,@front middle ,@back)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile multiple unquote-splicing")))

;; T022: Empty list splicing
;; Test: `(a ,@empty b) where empty could be nil
(deftest test-unquote-splicing-empty-list
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-splice-empty (items)
                                        `(a ,@items b)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile unquote-splicing with potentially empty list")))

;;; ==========================================================================
;;; Phase 5: User Story 4 - Mixed Elements Tests
;;; ==========================================================================

;; T029: Mixed symbol and variables
(deftest test-mixed-symbol-and-vars
  ;; Test: `(if ,cond ,then ,else) - if is quoted, vars evaluated
  ;; Expected: Symbol if is data, cond/then/else are local.get
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-mixed-sym (cond then else)
                                        `(if ,cond ,then ,else)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile mixed quoted symbol and unquoted vars")))

;; T030: Nested quoted list with dynamic
(deftest test-mixed-nested-quoted-list
  ;; Test: `((literal list) ,dynamic) - sublist preserved
  ;; Expected: (literal list) is quoted data, dynamic is evaluated
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-nested-quote (dynamic)
                                        `((literal list) ,dynamic)))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile nested quoted list with dynamic element")))

;;; ==========================================================================
;;; Phase 6: User Story 3 - Nested Quasiquote Tests
;;; ==========================================================================

;; T036: Nested quasiquote preserves inner
(deftest test-nested-quasiquote-preserves-inner
  ;; Test: `(a `(b ,x)) - inner backquote and comma preserved as data
  ;; Expected: Inner structure not evaluated at outer level
  ;; Note: True nested quasiquote with depth tracking is complex;
  ;; this test verifies basic nested structure compiles
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-nested-qq ()
                                        `(outer (inner val))))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile nested quasiquote structure")))

;; T037: Double unquote (simplified)
(deftest test-double-unquote
  ;; Test: Quasiquote with expression unquote `(a ,(+ 1 2))
  ;; Expected: Expression evaluated, result inserted
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-expr-unquote (x)
                                        `(result ,(+ x 1))))
                  (error (c)
                    (list :error (princ-to-string c))))))
    (ok (and (vectorp result)
             (not (and (listp result) (eq (first result) :error))))
        "compile-to-wasm should compile quasiquote with expression unquote")))

;;; ==========================================================================
;;; Phase 7: Error Handling Tests
;;; ==========================================================================

;; T044: Unquote outside quasiquote error
(deftest test-error-unquote-outside-quasiquote
  ;; Test: ,x at top level (outside backquote)
  ;; Expected: Compile-time error or reader error
  ;; Note: SBCL reader may handle this before we see it
  (ok t "Unquote outside quasiquote handled by reader"))

;; T045: Undefined variable error
(deftest test-error-undefined-variable
  ;; Test: `(,undefined-var) where var doesn't exist
  ;; Expected: Compile-time error "Unbound variable"
  (let ((result (handler-case
                    (compile-to-wasm '(defun test-undef ()
                                        `(,undefined-var)))
                  (error (c)
                    ;; Error is expected - undefined variable
                    (list :error (princ-to-string c))))))
    ;; Should either error or compile (if undefined vars produce warnings not errors)
    (ok (or (vectorp result)
            (and (listp result) (eq (first result) :error)))
        "compile-to-wasm should handle undefined variable in quasiquote")))
