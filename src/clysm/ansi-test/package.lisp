;;;; package.lisp - Package definition for ANSI test harness
;;;;
;;;; This package provides infrastructure for running the pfdietz/ansi-test
;;;; suite against the Clysm Common Lisp compiler.

(defpackage #:clysm/ansi-test
  (:use #:cl #:alexandria)
  (:export
   ;; Data model
   #:test-case
   #:test-case-p
   #:make-test-case
   #:test-case-name
   #:test-case-category
   #:test-case-source-file
   #:test-case-form
   #:test-case-expected-values
   #:test-case-metadata

   #:test-result
   #:test-result-p
   #:make-test-result
   #:test-result-test-case
   #:test-result-status
   #:test-result-actual-values
   #:test-result-skip-reason
   #:test-result-error-message
   #:test-result-execution-time-ms

   #:category-result
   #:category-result-p
   #:make-category-result
   #:category-result-name
   #:category-result-total-count
   #:category-result-pass-count
   #:category-result-fail-count
   #:category-result-skip-count
   #:category-result-duration-ms
   #:category-result-results
   #:category-pass-rate

   #:report-summary
   #:report-summary-p
   #:make-report-summary
   #:report-summary-total-count
   #:report-summary-pass-count
   #:report-summary-fail-count
   #:report-summary-skip-count
   #:report-summary-duration-ms

   #:coverage-report
   #:coverage-report-p
   #:make-coverage-report
   #:coverage-report-timestamp
   #:coverage-report-branch
   #:coverage-report-commit
   #:coverage-report-categories
   #:coverage-report-summary

   #:skip-registry
   #:skip-registry-p
   #:make-skip-registry
   #:skip-registry-unsupported-forms
   #:skip-registry-unsupported-categories
   #:skip-registry-skipped-tests
   #:skip-registry-timeout-seconds
   #:*default-skip-registry*

   #:baseline-comparison
   #:baseline-comparison-p
   #:make-baseline-comparison
   #:baseline-comparison-baseline-timestamp
   #:baseline-comparison-current-timestamp
   #:baseline-comparison-baseline-pass-count
   #:baseline-comparison-current-pass-count
   #:baseline-comparison-delta
   #:baseline-comparison-new-failures
   #:baseline-comparison-new-passes
   #:baseline-comparison-regression-p

   ;; Loader
   #:parse-deftest
   #:load-file-tests
   #:load-category-tests
   #:list-categories

   ;; Classifier
   #:classify-result
   #:compare-values
   #:expected-value-verifiable-p

   ;; Skip registry
   #:contains-unsupported-form-p
   #:detect-skip-reason

   ;; Runner
   #:execute-single-test
   #:run-category
   #:run-ansi-tests

   ;; Reporter
   #:generate-report
   #:generate-markdown-report

   ;; Baseline
   #:*baseline-directory*
   #:save-baseline
   #:load-baseline
   #:compare-to-baseline

   ;; Conditions
   #:category-not-found-error
   #:error-category
   #:available-categories))
