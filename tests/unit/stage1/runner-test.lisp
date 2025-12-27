;;;; runner-test.lisp - Unit tests for Stage 0 wasmtime runner
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for wasmtime availability check and form execution

(in-package #:clysm/tests/unit/stage1-runner)

;;; ==========================================================================
;;; Runtime Availability Tests
;;; ==========================================================================

(deftest test-wasmtime-available-p-returns-boolean
  "wasmtime-available-p should return T or NIL."
  (let ((result (clysm/stage1:wasmtime-available-p)))
    (ok (or (eq result t) (eq result nil))
        "wasmtime-available-p returns boolean")))

(deftest test-stage0-available-p-without-path
  "stage0-available-p should return NIL when path not set."
  (let ((clysm/stage1::*stage0-path* nil))
    (ok (null (clysm/stage1::stage0-available-p))
        "stage0-available-p returns NIL without path")))

(deftest test-stage0-available-p-with-nonexistent-path
  "stage0-available-p should return NIL for nonexistent file."
  (let ((clysm/stage1::*stage0-path* "/nonexistent/path.wasm"))
    (ok (null (clysm/stage1::stage0-available-p))
        "stage0-available-p returns NIL for nonexistent file")))

;;; ==========================================================================
;;; Stage 0 Loading Tests
;;; ==========================================================================

(deftest test-load-stage0-signals-on-missing-wasmtime
  "load-stage0 should signal stage1-wasmtime-unavailable if wasmtime not found."
  ;; This test only runs if wasmtime is actually unavailable
  (unless (clysm/stage1:wasmtime-available-p)
    (ok (signals 'clysm/stage1:stage1-runtime-error
                 (clysm/stage1:load-stage0 :path "/any/path.wasm"))
        "signals when wasmtime unavailable")))

(deftest test-load-stage0-validates-binary
  "load-stage0 with :validate t should validate the binary."
  (when (clysm/stage1:wasmtime-available-p)
    (ok (signals 'clysm/stage1:stage1-stage0-invalid
                 (clysm/stage1:load-stage0
                  :path "/nonexistent/stage0.wasm"
                  :validate t))
        "signals stage1-stage0-invalid for nonexistent binary")))

;;; ==========================================================================
;;; Form Execution Tests (Placeholder until wasmtime integration)
;;; ==========================================================================

(deftest test-run-form-returns-compilation-result
  "run-form should return a compilation-result struct."
  (let ((form (clysm/stage1:make-source-form
               :id "1:0"
               :sexp '(+ 1 2)
               :operator '+
               :compilable-p t)))
    (let ((result (clysm/stage1:run-form form)))
      (ok (clysm/stage1::compilation-result-p result)
          "run-form returns compilation-result")
      (ok (stringp (clysm/stage1:compilation-result-form-id result))
          "result has form-id"))))

(deftest test-run-form-with-sexp-directly
  "run-form should accept raw S-expression."
  (let ((result (clysm/stage1:run-form '(+ 1 2))))
    (ok (clysm/stage1::compilation-result-p result)
        "run-form accepts sexp directly")))

;;; ==========================================================================
;;; Error Conversion Tests
;;; ==========================================================================

(deftest test-error-from-wasm-unreachable
  "error-from-wasm should recognize unreachable trap."
  (let ((condition (clysm/stage1:error-from-wasm "wasm trap: unreachable")))
    (ok (typep condition 'clysm/stage1:stage1-runtime-error)
        "returns stage1-runtime-error")
    (ok (search "unreachable" (princ-to-string condition))
        "error message mentions unreachable")))

(deftest test-error-from-wasm-out-of-bounds
  "error-from-wasm should recognize out of bounds trap."
  (let ((condition (clysm/stage1:error-from-wasm "out of bounds memory access")))
    (ok (typep condition 'clysm/stage1:stage1-runtime-error)
        "returns stage1-runtime-error for out of bounds")))

(deftest test-error-from-wasm-generic
  "error-from-wasm should handle generic errors."
  (let ((condition (clysm/stage1:error-from-wasm "some unknown error")))
    (ok (typep condition 'clysm/stage1:stage1-runtime-error)
        "returns stage1-runtime-error for unknown errors")))
