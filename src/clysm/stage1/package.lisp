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
           #:source-form-id
           #:source-form-sexp
           #:source-form-operator
           #:source-form-name
           #:source-form-source-text
           #:source-form-module
           #:source-form-index
           #:source-form-compilable-p

           #:compilation-result
           #:make-compilation-result
           #:compilation-result-form
           #:compilation-result-form-id
           #:compilation-result-success-p
           #:compilation-result-error-type
           #:compilation-result-error-message
           #:compilation-result-wasm-bytes
           #:compilation-result-unsupported-feature)
  ;; Verification types (from types.lisp - Feature 040)
  (:export #:verification-result
           #:make-verification-result
           #:verification-result-status
           #:verification-result-timestamp
           #:verification-result-stage1-info
           #:verification-result-stage2-info
           #:verification-result-identical-p
           #:verification-result-first-diff-offset
           #:verification-result-diff-byte-count
           #:verification-result-compilation-rate
           #:verification-result-modules-compiled
           #:verification-result-modules-total
           #:verification-result-stage2-gen-time-ms
           #:verification-result-comparison-time-ms
           #:verification-result-error-message

           #:byte-diff-info
           #:make-byte-diff-info
           #:byte-diff-info-first-offset
           #:byte-diff-info-total-diff-bytes
           #:byte-diff-info-size-mismatch-p
           #:byte-diff-info-size1
           #:byte-diff-info-size2
           #:byte-diff-info-diff-regions

           #:verification-history-entry
           #:make-verification-history-entry
           #:verification-history-entry-timestamp
           #:verification-history-entry-status
           #:verification-history-entry-diff-bytes
           #:verification-history-entry-compilation-rate
           #:verification-history-entry-stage1-size
           #:verification-history-entry-stage2-size
           #:verification-history-entry-git-commit)
  ;; Conditions (from conditions.lisp)
  (:export #:stage1-error
           #:stage1-file-error
           #:stage1-file-not-found
           #:stage1-file-not-found-path
           #:stage1-encoding-error
           #:stage1-encoding-error-path
           #:stage1-encoding-error-byte-position
           #:stage1-parse-error
           #:stage1-parse-error-form-text
           #:stage1-parse-error-position
           #:stage1-compile-error
           #:stage1-compile-error-form
           #:stage1-compile-error-condition-type
           #:stage1-unsupported-feature
           #:stage1-unsupported-feature-name
           #:stage1-unsupported-feature-form
           #:stage1-internal-error
           #:stage1-internal-error-phase
           #:stage1-runtime-error
           #:stage1-wasmtime-unavailable
           #:stage1-stage0-invalid
           #:stage1-stage0-invalid-path
           ;; Fixed-point conditions (Feature 040)
           #:fixpoint-error
           #:fixpoint-stage1-missing
           #:fixpoint-stage1-missing-path
           #:fixpoint-stage2-generation-error
           #:fixpoint-stage2-generation-error-module
           #:fixpoint-stage2-generation-error-compiled
           #:fixpoint-stage2-generation-error-total
           #:fixpoint-binary-invalid
           #:fixpoint-binary-invalid-path
           #:fixpoint-binary-invalid-error
           #:fixpoint-comparison-error
           #:fixpoint-comparison-error-stage1
           #:fixpoint-comparison-error-stage2
           #:fixpoint-dependency-missing
           #:fixpoint-dependency-missing-name
           #:fixpoint-dependency-missing-hint)
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
           #:error-from-wasm
           #:invoke-wasmtime-compile
           #:compute-host-shim-path)
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
           #:compare-types
           ;; Byte-level comparison (Feature 040)
           #:binaries-identical-p
           #:compute-byte-diff
           ;; Binary info (from diff.lisp)
           #:extract-binary-info
           #:validate-binary
           #:binary-info
           #:make-binary-info
           #:binary-info-path
           #:binary-info-size-bytes
           #:binary-info-exports
           #:binary-info-types
           #:binary-info-functions
           #:binary-info-valid-p)
  ;; Fixpoint (from fixpoint.lisp - Feature 040)
  (:export #:fixpoint-status
           #:status-to-exit-code
           #:exit-code-to-status
           #:format-fixpoint-status
           #:current-iso-timestamp
           #:current-git-commit
           #:+exit-code-achieved+
           #:+exit-code-not-achieved+
           #:+exit-code-compilation-error+
           #:+exit-code-missing-dependency+)
  ;; Generator (from generator.lisp)
  (:export #:compile-form-to-wasm
           #:compile-all-forms
           #:accumulate-wasm-bytes
           #:write-stage1-binary
           #:validate-stage1
           #:generate-stage1))
