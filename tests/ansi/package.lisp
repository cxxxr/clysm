;;;; package.lisp - Package definitions for ANSI test integration

(defpackage #:clysm/ansi-tests
  (:use #:cl)
  (:export
   ;; Main API
   #:run-ansi-tests
   #:run-category
   #:all-passed-p
   #:list-categories
   ;; Configuration
   #:*ansi-test-directory*
   #:*wasm-runner*
   #:*expected-failures*
   ;; Test info
   #:test-result
   #:test-result-name
   #:test-result-status
   #:test-result-expected
   #:test-result-actual
   #:test-result-error
   ;; Report
   #:test-report
   #:test-report-total
   #:test-report-passed
   #:test-report-failed
   #:test-report-skipped
   #:test-report-errors
   #:generate-report
   #:print-summary))
