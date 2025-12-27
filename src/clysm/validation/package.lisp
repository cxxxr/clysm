;;; package.lisp - Package definition for Clysm Validation Module
;;;
;;; This module provides static analysis and compilation validation
;;; for Clysm's self-hosting capability.

(defpackage :clysm-validation
  (:use :cl :alexandria)
  (:export
   ;; CL-Feature struct and registry
   #:cl-feature
   #:cl-feature-p
   #:cl-feature-symbol
   #:cl-feature-category
   #:cl-feature-status
   #:cl-feature-notes
   #:make-cl-feature
   #:*clysm-features*
   #:feature-status
   #:feature-status-with-notes
   #:get-feature
   #:list-features-by-status
   #:list-features-by-category

   ;; Module struct
   #:module-info
   #:make-module-info
   #:module-info-p
   #:module-info-path
   #:module-info-directory
   #:module-info-dependencies
   #:module-info-symbols-used

   ;; Feature-Usage struct
   #:feature-usage
   #:make-feature-usage
   #:feature-usage-p
   #:feature-usage-symbol
   #:feature-usage-file
   #:feature-usage-line-numbers
   #:feature-usage-occurrence-count

   ;; Analysis utilities
   #:*target-directories*
   #:count-unique-symbols
   #:get-all-unique-symbols
   #:classify-symbols

   ;; Analyzer functions
   #:extract-symbols
   #:read-source-file
   #:analyze-file
   #:analyze-directory
   #:analyze-all

   ;; Coverage-Report struct
   #:coverage-report
   #:make-coverage-report
   #:coverage-report-p
   #:coverage-report-scope
   #:coverage-report-total-symbols
   #:coverage-report-supported-count
   #:coverage-report-partial-count
   #:coverage-report-unsupported-count
   #:coverage-report-unknown-count
   #:coverage-report-coverage-pct
   #:coverage-report-unsupported-details

   ;; Reporter functions
   #:compute-coverage
   #:compute-directory-coverage
   #:compute-all-coverage
   #:generate-report
   #:generate-report-to-file

   ;; Compilation-Result struct
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-p
   #:compilation-result-module
   #:compilation-result-success
   #:compilation-result-wasm-bytes
   #:compilation-result-error-message
   #:compilation-result-unsupported-feature
   #:compilation-result-validation-passed
   #:compilation-result-validation-error

   ;; Compiler-order functions
   #:*compilation-order*
   #:compile-module
   #:validate-wasm
   #:get-dependency-order
   #:compile-in-order
   #:validate-all-modules
   #:log-compilation-error
   #:generate-compilation-report
   #:compilable-form-p
   #:filter-compilable-forms

   ;; Blessed-Subset struct
   #:blessed-subset
   #:make-blessed-subset
   #:blessed-subset-p
   #:blessed-subset-version
   #:blessed-subset-generation-date
   #:blessed-subset-special-forms
   #:blessed-subset-macros
   #:blessed-subset-functions
   #:blessed-subset-types
   #:blessed-subset-partial-notes

   ;; Blessed-Subset functions
   #:aggregate-verified-features
   #:categorize-features
   #:collect-partial-notes
   #:generate-blessed-subset))
