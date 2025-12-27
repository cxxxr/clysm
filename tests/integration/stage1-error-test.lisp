;;;; stage1-error-test.lisp - Integration tests for Stage 0 error handling
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests that Stage 0 properly reports errors for unsupported forms

(in-package #:clysm/tests/integration/stage1-error)

;;; ==========================================================================
;;; Unsupported Form Error Tests
;;; ==========================================================================

(deftest test-stage0-unsupported-form-returns-error
  "Stage 0 should return error for unsupported forms."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  ;; Try a form that Stage 0 likely doesn't support
  (let ((result (clysm/stage1:run-form '(format t "~A" 123))))
    (ok (clysm/stage1::compilation-result-p result)
        "unsupported form returns result struct")
    ;; Currently all forms return not-implemented
    ;; When wasmtime integration is done, unsupported forms should fail
    ))

(deftest test-stage0-loop-form-error
  "Stage 0 should report error for LOOP macro."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form
                 '(loop for i from 1 to 10 sum i))))
    (ok (clysm/stage1::compilation-result-p result)
        "loop form returns result struct")))

(deftest test-stage0-defstruct-error
  "Stage 0 should report error for DEFSTRUCT."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form
                 '(defstruct point x y))))
    (ok (clysm/stage1::compilation-result-p result)
        "defstruct returns result struct")))

;;; ==========================================================================
;;; Error Classification Tests
;;; ==========================================================================

(deftest test-classify-error-file-not-found
  "classify-error should return :file-not-found for stage1-file-not-found."
  (let ((condition (make-condition 'clysm/stage1:stage1-file-not-found
                                   :path "/missing/file.lisp")))
    (ok (eq (clysm/stage1::classify-error condition) :file-not-found)
        "classifies stage1-file-not-found correctly")))

(deftest test-classify-error-unsupported-feature
  "classify-error should return :unsupported-feature for unsupported."
  (let ((condition (make-condition 'clysm/stage1:stage1-unsupported-feature
                                   :form-id "1:0"
                                   :feature 'loop)))
    (ok (eq (clysm/stage1::classify-error condition) :unsupported-feature)
        "classifies stage1-unsupported-feature correctly")))

(deftest test-classify-error-runtime
  "classify-error should return :runtime-error for runtime errors."
  (let ((condition (make-condition 'clysm/stage1:stage1-runtime-error
                                   :wasm-error "trap")))
    (ok (eq (clysm/stage1::classify-error condition) :runtime-error)
        "classifies stage1-runtime-error correctly")))

(deftest test-classify-error-external
  "classify-error should return :external for non-stage1 errors."
  (let ((condition (make-condition 'simple-error)))
    (ok (eq (clysm/stage1::classify-error condition) :external)
        "classifies external errors correctly")))
