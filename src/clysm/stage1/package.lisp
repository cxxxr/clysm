;;;; package.lisp - Package definition for Stage 1 compiler generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Provides infrastructure for executing Stage 0 on wasmtime and generating Stage 1

(defpackage #:clysm/stage1
  (:use #:cl)
  (:documentation "Stage 1 compiler generation infrastructure.
Provides tools for:
- Running Stage 0 Wasm binary on wasmtime
- Reading and parsing compiler source modules
- Tracking compilation progress
- Analyzing blockers
- Generating binary diff reports")
  ;; Types (from types.lisp)
  (:export #:source-module
           #:make-source-module
           #:source-module-path
           #:source-module-forms
           #:source-module-status

           #:source-form
           #:make-source-form
           #:source-form-sexp
           #:source-form-module
           #:source-form-index
           #:source-form-operator

           #:compilation-result
           #:make-compilation-result
           #:compilation-result-form
           #:compilation-result-success-p
           #:compilation-result-error-type
           #:compilation-result-error-message
           #:compilation-result-wasm-bytes)
  ;; Conditions (from conditions.lisp)
  (:export #:stage1-error
           #:stage1-file-error
           #:stage1-parse-error
           #:stage1-compile-error
           #:stage1-runtime-error)
  ;; Reader (from reader.lisp)
  (:export #:read-source-forms
           #:compilable-form-p
           #:get-module-paths
           #:read-all-modules)
  ;; Runner (from runner.lisp)
  (:export #:wasmtime-available-p
           #:load-stage0
           #:run-form
           #:capture-result
           #:error-from-wasm)
  ;; Progress (from progress.lisp)
  (:export #:progress-report
           #:make-progress-report
           #:start-module-tracking
           #:record-form-result
           #:complete-module-tracking
           #:generate-summary
           #:write-progress-report)
  ;; Blocker (from blocker.lisp)
  (:export #:blocker-info
           #:analyze-blockers
           #:generate-blocker-report
           #:estimate-blocker-impact)
  ;; Diff (from diff.lisp)
  (:export #:diff-report
           #:compare-binaries
           #:generate-diff-report
           #:compare-exports
           #:compare-types)
  ;; Generator (from generator.lisp)
  (:export #:compile-form-to-wasm
           #:compile-all-forms
           #:accumulate-wasm-bytes
           #:write-stage1-binary
           #:validate-stage1
           #:generate-stage1))
