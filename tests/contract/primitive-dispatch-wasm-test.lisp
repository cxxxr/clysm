;;;; primitive-dispatch-wasm-test.lisp - Contract tests for primitive dispatch
;;;;
;;;; Validates that primitives dispatched through the hash-table mechanism
;;;; generate valid Wasm output.
;;;; Part of 001-primitive-dispatch-table feature.

(defpackage #:clysm/tests/contract/primitive-dispatch
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:compile-to-wasm)
  (:import-from #:clysm/tests/helpers
                #:validate-wasm-silent)
  (:import-from #:clysm/compiler/codegen/primitive-dispatch
                #:clear-primitive-tables
                #:primitive-registered-p))

(in-package #:clysm/tests/contract/primitive-dispatch)

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun wasm-validates-p (bytes)
  "Check if Wasm bytes are valid using wasm-tools validate."
  (when (and bytes (> (length bytes) 0))
    (validate-wasm-silent bytes)))

(defun compile-form-to-bytes (form)
  "Compile a single form and return Wasm bytes."
  (compile-to-wasm `(defun test-fn () ,form)))

;;; ==========================================================================
;;; User Story 2: Symbol-Based Dispatch Contract Tests
;;; ==========================================================================

(deftest addition-generates-valid-wasm
  "T028: (+ 1 2) generates valid Wasm via table dispatch"
  ;; Note: Tests will pass once primitives are registered in Phase 4
  (let ((bytes (compile-form-to-bytes '(+ 1 2))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))

(deftest cons-generates-valid-wasm
  "T029: (cons 1 2) generates valid Wasm via table dispatch"
  (let ((bytes (compile-form-to-bytes '(cons 1 2))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))

(deftest nested-primitives-dispatch-correctly
  "T030: nested primitives (car (cons 1 2)) dispatch correctly"
  (let ((bytes (compile-form-to-bytes '(car (cons 1 2)))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))

;;; ==========================================================================
;;; User Story 3: String-Based Dispatch Contract Tests
;;; ==========================================================================

;; These tests will be enabled in Phase 5 (US3) when string-based primitives are registered

(deftest setf-aref-dispatches-correctly
  "T043: %SETF-AREF dispatches correctly from any package"
  ;; Test will be implemented when string-based primitives are registered
  (skip "String-based primitives not yet registered (Phase 5)"))

(deftest make-instance-star-dispatches-correctly
  "T044: MAKE-INSTANCE* dispatches correctly via string lookup"
  ;; Test will be implemented when string-based primitives are registered
  (skip "String-based primitives not yet registered (Phase 5)"))

;;; ==========================================================================
;;; User Story 4: Backward Compatibility Contract Tests
;;; ==========================================================================

(deftest arithmetic-chain-generates-valid-wasm
  "Complex arithmetic generates valid Wasm"
  (let ((bytes (compile-form-to-bytes '(+ (* 2 3) (- 10 5)))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))

(deftest list-operations-generate-valid-wasm
  "List operations generate valid Wasm"
  (let ((bytes (compile-form-to-bytes '(car (cdr (cons 1 (cons 2 nil)))))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))

(deftest type-predicate-generates-valid-wasm
  "Type predicates generate valid Wasm"
  (let ((bytes (compile-form-to-bytes '(consp (cons 1 2)))))
    (ok bytes "Should produce Wasm bytes")
    (when bytes
      (ok (wasm-validates-p bytes) "Generated Wasm should validate"))))
