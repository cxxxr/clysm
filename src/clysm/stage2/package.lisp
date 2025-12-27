;;;; package.lisp - Package definition for Stage 2 generation and fixed-point verification
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Provides infrastructure for generating Stage 2 and verifying self-hosting

(defpackage #:clysm/stage2
  (:use #:cl #:clysm/stage1)
  (:documentation "Fixed-point verification and Stage 2 generation infrastructure.
Provides tools for:
- Generating Stage 2 binary by running Stage 1 on wasmtime
- Verifying fixed-point (Stage 1 == Stage 2 byte-identical)
- Generating diff analysis reports for non-identical binaries
- Tracking verification history across iterations
- CI integration with exit codes and JSON output")
  ;; Verifier (from verifier.lisp)
  (:export #:verify-fixpoint
           #:check-dependencies
           #:validate-stage1-binary)
  ;; Generator (from generator.lisp)
  (:export #:generate-stage2
           #:compile-module-via-stage1
           #:run-stage1-compiler)
  ;; Comparison (from comparison.lisp)
  (:export #:binaries-identical-p
           #:compute-byte-diff
           #:format-verification-result)
  ;; History (from history.lisp)
  (:export #:append-to-history
           #:read-history-log
           #:compute-progress-summary)
  ;; Result types (re-export from stage1)
  (:export #:verification-result
           #:make-verification-result
           #:verification-result-status
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

           #:verification-history-entry
           #:make-verification-history-entry
           #:verification-history-entry-status
           #:verification-history-entry-diff-bytes
           #:verification-history-entry-compilation-rate)
  ;; Fixpoint status (from fixpoint.lisp)
  (:export #:fixpoint-status
           #:status-to-exit-code
           #:exit-code-to-status
           #:+exit-code-achieved+
           #:+exit-code-not-achieved+
           #:+exit-code-compilation-error+
           #:+exit-code-missing-dependency+))
