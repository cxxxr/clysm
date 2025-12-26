;;;; report-format-test.lisp - Contract tests for Markdown report format
;;;;
;;;; T040: Contract test for Markdown format compliance

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T040: Markdown format contract tests
;;; ==========================================================================

(deftest markdown-header-format
  "Test that generated Markdown has proper headers"
  (let* ((summary (make-report-summary
                   :total-count 10
                   :pass-count 5
                   :fail-count 3
                   :skip-count 2))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :branch "main"
                  :commit "abc123"
                  :categories nil
                  :summary summary))
         (content (generate-markdown-report report)))
    (ok (search "# ANSI Test Coverage Report" content)
        "Report has main header")
    (ok (search "## Summary" content)
        "Report has summary section")))

(deftest markdown-category-table-format
  "Test that category results are formatted as a table"
  (let* ((cat-result (make-category-result
                      :name "cons"
                      :total-count 50
                      :pass-count 40
                      :fail-count 5
                      :skip-count 5))
         (summary (make-report-summary
                   :total-count 50
                   :pass-count 40
                   :fail-count 5
                   :skip-count 5))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :categories (list cat-result)
                  :summary summary))
         (content (generate-markdown-report report)))
    (ok (search "cons" content)
        "Report contains category name")
    (ok (search "|" content)
        "Report uses table format")))
