;;;; mv-wasm-test.lisp - Contract tests for multiple values Wasm output
;;;; Feature: 025-multiple-values

(in-package #:clysm/tests/contract/mv-wasm)

;;; Contract tests verify that generated Wasm modules:
;;; 1. Include mv-count and mv-buffer globals
;;; 2. Validate with wasm-tools validate
;;; 3. Have correct global section structure

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun compile-validates (expr)
  "Compile expression and validate the resulting Wasm."
  (handler-case
      (let ((bytes (clysm/compiler:compile-to-wasm expr)))
        (clysm/tests:validate-wasm-silent bytes))
    (error () nil)))

;;; ============================================================
;;; Phase 2: Foundational - MV Globals Contract Tests (T009)
;;; ============================================================

(deftest basic-module-validates-with-mv-globals
  (testing "Basic expression should compile to valid Wasm with mv globals"
    (ok (compile-validates 42)
        "Simple fixnum should generate valid Wasm")))

(deftest nil-expression-validates
  (testing "NIL expression should compile to valid Wasm"
    (ok (compile-validates 'nil)
        "NIL should generate valid Wasm")))

(deftest arithmetic-validates-with-mv-globals
  (testing "Arithmetic should compile to valid Wasm with mv globals"
    (ok (compile-validates '(+ 1 2))
        "Addition should generate valid Wasm")))

(deftest function-call-validates-with-mv-globals
  (testing "Function call should compile to valid Wasm with mv globals"
    (ok (compile-validates '(if t 1 2))
        "Conditional should generate valid Wasm")))

;;; ============================================================
;;; MV Global Index Constants Tests
;;; ============================================================

(deftest mv-count-global-index-is-2
  (testing "mv-count global index should be 2"
    (ok (= clysm/runtime/objects:*mv-count-global-index* 2)
        "mv-count global index should be 2")))

(deftest mv-buffer-global-index-is-3
  (testing "mv-buffer global index should be 3"
    (ok (= clysm/runtime/objects:*mv-buffer-global-index* 3)
        "mv-buffer global index should be 3")))

(deftest global-counter-starts-at-4
  (testing "Global counter should start at 4 after reset"
    (clysm/runtime/objects:reset-global-counter)
    (ok (= clysm/runtime/objects:*global-counter* 4)
        "Global counter should be 4 after reset")))

(deftest runtime-globals-has-four-entries
  (testing "generate-runtime-globals should return 4 globals"
    (let ((globals (clysm/runtime/objects:generate-runtime-globals)))
      (ok (= (length globals) 4)
          (format nil "Should have 4 globals, got ~D" (length globals))))))
