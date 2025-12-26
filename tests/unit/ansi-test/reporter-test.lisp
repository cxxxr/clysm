;;;; reporter-test.lisp - Unit tests for ANSI test reporter
;;;;
;;;; T041: format-category-results tests
;;;; T042: format-summary-table tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T041: format-category-results tests
;;; ==========================================================================

(deftest generate-markdown-report-structure
  "Test that generate-markdown-report produces valid Markdown structure"
  (let* ((summary (make-report-summary
                   :total-count 100
                   :pass-count 40
                   :fail-count 10
                   :skip-count 50
                   :duration-ms 5000))
         (category (make-category-result
                    :name "cons"
                    :total-count 50
                    :pass-count 20
                    :fail-count 5
                    :skip-count 25
                    :duration-ms 2500))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :branch "main"
                  :commit "abc123"
                  :categories (list category)
                  :summary summary))
         (markdown (generate-markdown-report report)))
    (ok (stringp markdown) "Returns a string")
    (ok (search "# ANSI Test Coverage Report" markdown) "Contains header")
    (ok (search "## Summary" markdown) "Contains summary section")
    (ok (search "## Category Results" markdown) "Contains category results section")
    (ok (search "| Metric | Value |" markdown) "Contains summary table")
    (ok (search "| cons |" markdown) "Contains category name in table")))

(deftest generate-markdown-report-pass-rate
  "Test that generate-markdown-report calculates pass rate correctly"
  (let* ((summary (make-report-summary
                   :total-count 100
                   :pass-count 40))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :summary summary))
         (markdown (generate-markdown-report report)))
    (ok (search "40.0%" markdown) "Contains correct pass rate")))

(deftest generate-markdown-report-zero-tests
  "Test that generate-markdown-report handles zero tests gracefully"
  (let* ((summary (make-report-summary
                   :total-count 0
                   :pass-count 0))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :summary summary))
         (markdown (generate-markdown-report report)))
    (ok (stringp markdown) "Returns a string even with zero tests")
    (ok (search "0.0%" markdown) "Shows 0% pass rate for empty suite")))

;;; ==========================================================================
;;; T042: format-summary-table tests
;;; ==========================================================================

(deftest generate-report-creates-file
  "Test that generate-report writes to file when output-path provided"
  (let* ((summary (make-report-summary
                   :total-count 10
                   :pass-count 5))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :summary summary)))
    (uiop:with-temporary-file (:pathname path :type "md" :keep nil)
      (generate-report report :output-path path)
      (ok (probe-file path) "File is created")
      (let ((content (uiop:read-file-string path)))
        (ok (search "ANSI Test Coverage Report" content) "File contains report content")))))

(deftest generate-report-returns-string
  "Test that generate-report returns string when no output-path"
  (let* ((summary (make-report-summary
                   :total-count 10
                   :pass-count 5))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :summary summary))
         (result (generate-report report)))
    (ok (stringp result) "Returns a string when no output-path")))
