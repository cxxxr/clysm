;;;; stage1-blocker-test.lisp - Contract test for blocker report JSON format
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; T058: Verifies blocker report JSON output format

(in-package #:clysm/tests/contract/stage1-blocker)

;;; ==========================================================================
;;; Blocker Report JSON Contract Tests
;;; ==========================================================================

(deftest test-blocker-report-has-required-fields
  "Blocker report JSON should have all required fields."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'loop :count 50 :example "(loop ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 50 :failed 50 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats)))
         (output (make-string-output-stream)))
    ;; Generate blocker report JSON
    (clysm/stage1::write-blocker-report summary output)
    (let ((json-string (get-output-stream-string output)))
      (ok (search "\"blockers\"" json-string)
          "JSON has 'blockers' field")
      (ok (search "\"operator\"" json-string)
          "JSON has 'operator' field")
      (ok (search "\"affected_forms\"" json-string)
          "JSON has 'affected_forms' field")
      (ok (search "\"priority\"" json-string)
          "JSON has 'priority' field")
      (ok (search "\"recommendation\"" json-string)
          "JSON has 'recommendation' field"))))

(deftest test-blocker-report-priority-values
  "Blocker priority should be HIGH, MEDIUM, or LOW."
  (let* ((high-failure (clysm/stage1::make-failure-group
                        :operator 'loop :count 50 :example "(loop ...)"))
         (low-failure (clysm/stage1::make-failure-group
                       :operator 'obscure :count 2 :example "(obscure ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 48 :failed 52 :skipped 0
                 :failures (list high-failure low-failure)))
         (summary (clysm/stage1::generate-summary (list stats)))
         (output (make-string-output-stream)))
    (clysm/stage1::write-blocker-report summary output)
    (let ((json-string (get-output-stream-string output)))
      (ok (or (search "\"HIGH\"" json-string)
              (search "\"MEDIUM\"" json-string)
              (search "\"LOW\"" json-string))
          "JSON contains valid priority value"))))

(deftest test-blocker-report-sorted-by-impact
  "Blockers should be sorted by impact (highest first)."
  (let* ((high-failure (clysm/stage1::make-failure-group
                        :operator 'loop :count 100 :example "(loop ...)"))
         (medium-failure (clysm/stage1::make-failure-group
                          :operator 'format :count 30 :example "(format ...)"))
         (low-failure (clysm/stage1::make-failure-group
                       :operator 'declare :count 5 :example "(declare ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 200
                 :compiled 65 :failed 135 :skipped 0
                 :failures (list medium-failure low-failure high-failure)))
         (summary (clysm/stage1::generate-summary (list stats))))
    (let ((blockers (clysm/stage1::summary-top-blockers summary)))
      (when (>= (length blockers) 2)
        (let ((first-count (clysm/stage1::blocker-info-affected-forms (first blockers)))
              (second-count (clysm/stage1::blocker-info-affected-forms (second blockers))))
          (ok (>= first-count second-count)
              "First blocker has highest or equal impact"))))))

(deftest test-blocker-report-valid-json-syntax
  "Blocker report should be valid JSON."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'loop :count 10 :example "(loop ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 90 :failed 10 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats)))
         (output (make-string-output-stream)))
    (clysm/stage1::write-blocker-report summary output)
    (let ((json-string (get-output-stream-string output)))
      ;; Basic JSON structure checks
      (ok (char= (char json-string 0) #\{)
          "JSON starts with {")
      (ok (char= (char json-string (1- (length json-string))) #\})
          "JSON ends with }")
      ;; Check for matching braces (simple count)
      (let ((open-count (count #\{ json-string))
            (close-count (count #\} json-string)))
        (ok (= open-count close-count)
            "Matching brace count")))))

