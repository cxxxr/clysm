;;;; reporter.lisp - Report generation for ANSI test results
;;;;
;;;; Generates Markdown and JSON reports from test execution results.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T040-T048: Report generation (Phase 5 - stubbed for now)
;;; ==========================================================================

(defun generate-report (coverage-report &key output-path format)
  "Generate a report from COVERAGE-REPORT.
FORMAT can be :markdown (default) or :json.
If OUTPUT-PATH is provided, writes to file; otherwise returns string."
  (declare (ignore format))
  (let ((content (generate-markdown-report coverage-report)))
    (if output-path
        (with-open-file (out output-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
          (write-string content out)
          output-path)
        content)))

(defun generate-markdown-report (coverage-report)
  "Generate Markdown content from COVERAGE-REPORT."
  (with-output-to-string (s)
    (let ((summary (coverage-report-summary coverage-report)))
      (format s "# ANSI Test Coverage Report~%~%")
      (format s "**Generated**: ~A~%~%" (coverage-report-timestamp coverage-report))
      (when (coverage-report-branch coverage-report)
        (format s "**Branch**: ~A~%" (coverage-report-branch coverage-report)))
      (when (coverage-report-commit coverage-report)
        (format s "**Commit**: ~A~%~%" (coverage-report-commit coverage-report)))
      (format s "## Summary~%~%")
      (format s "| Metric | Value |~%")
      (format s "|--------|-------|~%")
      (format s "| Total Tests | ~D |~%" (report-summary-total-count summary))
      (format s "| Passed | ~D |~%" (report-summary-pass-count summary))
      (format s "| Failed | ~D |~%" (report-summary-fail-count summary))
      (format s "| Skipped | ~D |~%" (report-summary-skip-count summary))
      (format s "| Pass Rate | ~,1F% |~%~%"
              (* 100 (if (zerop (report-summary-total-count summary))
                         0.0
                         (float (/ (report-summary-pass-count summary)
                                   (report-summary-total-count summary))))))
      (format s "## Category Results~%~%")
      (format s "| Category | Pass | Fail | Skip | Total | Rate |~%")
      (format s "|----------|------|------|------|-------|------|~%")
      (dolist (cat (coverage-report-categories coverage-report))
        (format s "| ~A | ~D | ~D | ~D | ~D | ~,1F% |~%"
                (category-result-name cat)
                (category-result-pass-count cat)
                (category-result-fail-count cat)
                (category-result-skip-count cat)
                (category-result-total-count cat)
                (* 100 (category-pass-rate cat)))))))
