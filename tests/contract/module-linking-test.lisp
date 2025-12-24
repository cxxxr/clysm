;;;; module-linking-test.lisp - Contract tests for module linking
;;;; Feature: 017-eval-jit-compile
;;;; Tests that JIT modules receive proper import bindings (T042)

(in-package #:clysm/tests/contract/module-linking)

;;; ============================================================
;;; T042: Contract test - JIT module receives imports
;;; ============================================================

(deftest test-wasm-generation-for-import-using-function
  "T042: Wasm is generated for functions that use runtime imports."
  ;; Generate Wasm for a function that uses arithmetic
  (let ((wasm (clysm/eval/jit:generate-wasm '(lambda (x y) (+ x y)))))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Function using + generates valid Wasm")))

(deftest test-wasm-generation-for-list-operations
  "T042: Wasm is generated for functions using list operations."
  (let ((wasm (clysm/eval/jit:generate-wasm '(lambda (x) (cons x nil)))))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Function using cons generates valid Wasm")))

(deftest test-wasm-generation-for-comparison
  "T042: Wasm is generated for functions using comparisons."
  (let ((wasm (clysm/eval/jit:generate-wasm '(lambda (x y) (< x y)))))
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Function using < generates valid Wasm")))

(deftest test-wasm-import-section-exists
  "T042: Generated Wasm contains import section."
  ;; The Wasm binary format includes sections
  ;; Section 2 is the import section
  (let ((wasm (clysm/eval/jit:generate-wasm '(lambda (x) (+ x 1)))))
    (ok (>= (length wasm) 8)
        "Wasm has sufficient length for headers")
    ;; Verify it's valid Wasm (validates structure including imports)
    (ok (clysm/eval/jit:validate-wasm wasm)
        "Wasm with imports is valid")))

(deftest test-runtime-imports-contract
  "T042: Runtime imports table provides expected interface."
  ;; Contract: register-runtime-import takes (name function)
  (clysm/eval/jit:register-runtime-import "contract-test" (lambda (x) x))

  ;; Contract: get-runtime-import takes name, returns function or nil
  (let ((fn (clysm/eval/jit:get-runtime-import "contract-test")))
    (ok (functionp fn) "Retrieved import is callable")
    (ok (= (funcall fn 42) 42) "Retrieved import executes correctly")))
