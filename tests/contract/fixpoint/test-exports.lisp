;;;; test-exports.lisp - Contract test for Stage 1 exports (T010)
;;;; Part of 001-bootstrap-fixpoint Phase 13D-9
;;;;
;;;; This test verifies that Stage 1 Wasm module exports compile_form function.
;;;; TDD: This test must FAIL before implementation is complete.

(in-package #:clysm/tests/contract/fixpoint-exports)

(defparameter *stage1-wasm-path*
  (merge-pathnames #p"dist/clysm-stage1.wasm"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 1 Wasm binary.")

(deftest test-stage1-wasm-exists
  "Verify Stage 1 Wasm file exists"
  (ok (probe-file *stage1-wasm-path*)
      "dist/clysm-stage1.wasm should exist"))

(deftest test-stage1-exports-compile-form
  "Verify Stage 1 exports compile_form function (FR-001)"
  ;; Use wasm-tools to check exports
  (let ((exports-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    ;; Check that compile_form is in the exports
    (ok (search "(export \"compile_form\"" exports-output)
        "Stage 1 should export compile_form function")))

(deftest test-stage1-exports-compile-all
  "Verify Stage 1 exports compile_all function (FR-002)"
  (let ((exports-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    (ok (search "(export \"compile_all\"" exports-output)
        "Stage 1 should export compile_all function")))

(deftest test-stage1-exports-initialize
  "Verify Stage 1 exports _initialize function (FR-002)"
  (let ((exports-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    (ok (search "(export \"_initialize\"" exports-output)
        "Stage 1 should export _initialize function")))
