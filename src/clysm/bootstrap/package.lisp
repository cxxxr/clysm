;;;; package.lisp - Package definition for interpreter bootstrap
;;;; Feature 044: Interpreter Bootstrap Strategy

(defpackage #:clysm/interpreter-bootstrap
  (:use #:cl)
  (:documentation "Interpreter-based bootstrap for self-hosting.
   Provides infrastructure to generate Stage 0 Wasm binary using
   the Tier 1 interpreter instead of SBCL compilation.")
  (:export
   ;; Bootstrap result struct
   #:bootstrap-result
   #:make-bootstrap-result
   #:bootstrap-result-p
   #:bootstrap-result-success
   #:bootstrap-result-wasm-bytes
   #:bootstrap-result-modules-loaded
   #:bootstrap-result-forms-compiled
   #:bootstrap-result-errors
   #:bootstrap-result-elapsed-time

   ;; Main entry points
   #:generate-stage0-via-interpreter
   #:load-compiler-modules

   ;; Predicates
   #:form-compilable-p

   ;; Validation
   #:validate-stage0-binary

   ;; Progress tracking
   #:*bootstrap-progress-callback*

   ;; Fixpoint verification (Phase 6)
   #:run-full-bootstrap
   #:verify-fixpoint-interpreter
   #:verify-stages-identical

   ;; Fixpoint result struct
   #:fixpoint-result
   #:make-fixpoint-result
   #:fixpoint-result-p
   #:fixpoint-result-status
   #:fixpoint-result-timestamp
   #:fixpoint-result-stage0-path
   #:fixpoint-result-stage1-path
   #:fixpoint-result-stage2-path
   #:fixpoint-result-identical-p
   #:fixpoint-result-first-diff-offset
   #:fixpoint-result-elapsed-ms
   #:fixpoint-result-error-message
   #:fixpoint-exit-code

   ;; Report generation
   #:generate-json-report
   #:generate-text-report))
