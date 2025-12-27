;;;; stage1-report-test.lisp - Contract tests for Stage 1 progress report format
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for progress report JSON format contract

(in-package #:clysm/tests/contract/stage1-report)

;;; ==========================================================================
;;; JSON Format Contract Tests
;;; ==========================================================================

(deftest test-report-json-has-required-fields
  "Progress report JSON should have all required top-level fields."
  (let* ((summary (clysm/stage1::make-summary
                   :total-forms 100 :compiled 80 :failed 20
                   :skipped 0 :coverage-pct 80.0 :top-blockers nil))
         (report (clysm/stage1::make-progress-report
                  :timestamp "2025-12-27T00:00:00Z"
                  :stage0-version "stage0-v1.0"
                  :modules nil
                  :summary summary))
         (json (with-output-to-string (s)
                 (clysm/stage1:write-progress-report report :stream s :format :json))))
    (ok (search "\"timestamp\":" json) "has timestamp field")
    (ok (search "\"stage0_version\":" json) "has stage0_version field")
    (ok (search "\"summary\":" json) "has summary field")
    (ok (search "\"modules\":" json) "has modules field")))

(deftest test-report-summary-fields
  "Summary section should have all required fields."
  (let* ((summary (clysm/stage1::make-summary
                   :total-forms 100 :compiled 80 :failed 20
                   :skipped 0 :coverage-pct 80.0 :top-blockers nil))
         (report (clysm/stage1::make-progress-report
                  :timestamp "2025-12-27T00:00:00Z"
                  :stage0-version "test"
                  :modules nil
                  :summary summary))
         (json (with-output-to-string (s)
                 (clysm/stage1:write-progress-report report :stream s :format :json))))
    (ok (search "\"total_forms\":" json) "summary has total_forms")
    (ok (search "\"compiled\":" json) "summary has compiled")
    (ok (search "\"failed\":" json) "summary has failed")
    (ok (search "\"coverage_pct\":" json) "summary has coverage_pct")
    (ok (search "\"top_blockers\":" json) "summary has top_blockers")))

(deftest test-report-coverage-format
  "Coverage percentage should be formatted correctly."
  (let* ((summary (clysm/stage1::make-summary
                   :total-forms 100 :compiled 80 :failed 20
                   :skipped 0 :coverage-pct 80.0 :top-blockers nil))
         (report (clysm/stage1::make-progress-report
                  :timestamp "2025-12-27T00:00:00Z"
                  :stage0-version "test"
                  :modules nil
                  :summary summary))
         (json (with-output-to-string (s)
                 (clysm/stage1:write-progress-report report :stream s :format :json))))
    (ok (search "80.00" json) "coverage is formatted as 80.00")))

(deftest test-report-timestamp-format
  "Timestamp should be ISO 8601 format."
  (let ((ts (clysm/stage1::format-timestamp)))
    (ok (= (length ts) 20) "timestamp is 20 chars (ISO 8601)")
    (ok (char= (char ts 4) #\-) "has dash at position 4")
    (ok (char= (char ts 7) #\-) "has dash at position 7")
    (ok (char= (char ts 10) #\T) "has T at position 10")
    (ok (char= (char ts 13) #\:) "has colon at position 13")
    (ok (char= (char ts 16) #\:) "has colon at position 16")
    (ok (char= (char ts 19) #\Z) "ends with Z")))

(deftest test-report-module-stats-format
  "Module stats should have correct JSON structure."
  (let* ((stats (clysm/stage1::make-module-stats
                 :path "src/test.lisp"
                 :total-forms 10
                 :compiled 8
                 :failed 2
                 :skipped 0
                 :failures nil))
         (summary (clysm/stage1::make-summary
                   :total-forms 10 :compiled 8 :failed 2
                   :skipped 0 :coverage-pct 80.0 :top-blockers nil))
         (report (clysm/stage1::make-progress-report
                  :timestamp "2025-12-27T00:00:00Z"
                  :stage0-version "test"
                  :modules (list stats)
                  :summary summary))
         (json (with-output-to-string (s)
                 (clysm/stage1:write-progress-report report :stream s :format :json))))
    (ok (search "\"path\":" json) "module has path field")
    (ok (search "src/test.lisp" json) "path value is correct")))

