;;;; progress-test.lisp - Unit tests for Stage 1 progress tracking
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for ModuleStats, Summary, and FailureGroup aggregation

(in-package #:clysm/tests/unit/stage1-progress)

;;; ==========================================================================
;;; ModuleStats Tests
;;; ==========================================================================

(deftest test-module-stats-creation
  "ModuleStats struct should be creatable with initial values."
  (let ((stats (clysm/stage1::make-module-stats
                :path "src/test.lisp"
                :total-forms 10
                :compiled 0
                :failed 0
                :skipped 0
                :failures nil)))
    (ok (clysm/stage1::module-stats-p stats)
        "make-module-stats creates module-stats struct")
    (ok (string= (clysm/stage1::module-stats-path stats) "src/test.lisp")
        "path is correct")
    (ok (= (clysm/stage1::module-stats-total-forms stats) 10)
        "total-forms is correct")))

(deftest test-module-stats-tracking
  "Module tracking should count compiled and failed forms."
  (let ((module (clysm/stage1::make-source-module
                 :path #P"/tmp/test.lisp"
                 :relative-path "src/test.lisp"
                 :forms (list (clysm/stage1:make-source-form
                               :id "1:0" :sexp '(+ 1 2)
                               :operator '+ :compilable-p t)))))
    (clysm/stage1:start-module-tracking module)
    (clysm/stage1:record-form-result
     (clysm/stage1:make-compilation-result
      :form (first (clysm/stage1::source-module-forms module))
      :form-id "1:0"
      :success-p t))
    (let ((stats (clysm/stage1:complete-module-tracking)))
      (ok (= (clysm/stage1::module-stats-compiled stats) 1)
          "compiled count is 1")
      (ok (= (clysm/stage1::module-stats-failed stats) 0)
          "failed count is 0"))))

(deftest test-module-stats-failure-tracking
  "Module tracking should group failures by operator."
  (let ((module (clysm/stage1::make-source-module
                 :path #P"/tmp/test.lisp"
                 :relative-path "src/test.lisp"
                 :forms (list (clysm/stage1:make-source-form
                               :id "1:0" :sexp '(loop for i from 1 to 10 do i)
                               :operator 'loop :compilable-p t)
                              (clysm/stage1:make-source-form
                               :id "1:1" :sexp '(loop for j from 1 to 5 do j)
                               :operator 'loop :compilable-p t)))))
    (clysm/stage1:start-module-tracking module)
    (dolist (form (clysm/stage1::source-module-forms module))
      (clysm/stage1:record-form-result
       (clysm/stage1:make-compilation-result
        :form form
        :form-id (clysm/stage1::source-form-id form)
        :success-p nil
        :error-type :unsupported-feature)))
    (let ((stats (clysm/stage1:complete-module-tracking)))
      (ok (= (clysm/stage1::module-stats-failed stats) 2)
          "failed count is 2")
      (ok (= (length (clysm/stage1::module-stats-failures stats)) 1)
          "failures grouped to 1 operator")
      (ok (= (clysm/stage1::failure-group-count
              (first (clysm/stage1::module-stats-failures stats))) 2)
          "failure group has count 2"))))

;;; ==========================================================================
;;; Summary Aggregation Tests
;;; ==========================================================================

(deftest test-summary-empty-list
  "Summary should handle empty module list."
  (let ((summary (clysm/stage1::generate-summary nil)))
    (ok (clysm/stage1::summary-p summary)
        "returns summary struct")
    (ok (= (clysm/stage1::summary-total-forms summary) 0)
        "total-forms is 0")
    (ok (= (clysm/stage1::summary-coverage-pct summary) 0.0)
        "coverage is 0.0")))

(deftest test-summary-single-module
  "Summary should aggregate single module stats."
  (let* ((stats (clysm/stage1::make-module-stats
                 :path "test.lisp"
                 :total-forms 10
                 :compiled 8
                 :failed 2
                 :skipped 0
                 :failures nil))
         (summary (clysm/stage1::generate-summary (list stats))))
    (ok (= (clysm/stage1::summary-total-forms summary) 10)
        "total is 10")
    (ok (= (clysm/stage1::summary-compiled summary) 8)
        "compiled is 8")
    (ok (= (clysm/stage1::summary-failed summary) 2)
        "failed is 2")
    (ok (< (abs (- (clysm/stage1::summary-coverage-pct summary) 80.0)) 0.01)
        "coverage is 80%")))

(deftest test-summary-multiple-modules
  "Summary should aggregate multiple module stats."
  (let* ((stats1 (clysm/stage1::make-module-stats
                  :path "a.lisp" :total-forms 10 :compiled 5
                  :failed 3 :skipped 2 :failures nil))
         (stats2 (clysm/stage1::make-module-stats
                  :path "b.lisp" :total-forms 20 :compiled 15
                  :failed 5 :skipped 0 :failures nil))
         (summary (clysm/stage1::generate-summary (list stats1 stats2))))
    (ok (= (clysm/stage1::summary-total-forms summary) 30)
        "total is 30")
    (ok (= (clysm/stage1::summary-compiled summary) 20)
        "compiled is 20")
    (ok (= (clysm/stage1::summary-failed summary) 8)
        "failed is 8")))

;;; ==========================================================================
;;; FailureGroup Aggregation Tests
;;; ==========================================================================

(deftest test-failure-group-merging
  "Summary should merge failure groups across modules."
  (let* ((fg1 (clysm/stage1::make-failure-group
               :operator 'loop :count 5 :example "(loop ...)"))
         (fg2 (clysm/stage1::make-failure-group
               :operator 'loop :count 3 :example "(loop ...)"))
         (stats1 (clysm/stage1::make-module-stats
                  :path "a.lisp" :total-forms 10 :compiled 5
                  :failed 5 :skipped 0 :failures (list fg1)))
         (stats2 (clysm/stage1::make-module-stats
                  :path "b.lisp" :total-forms 10 :compiled 7
                  :failed 3 :skipped 0 :failures (list fg2)))
         (summary (clysm/stage1::generate-summary (list stats1 stats2))))
    (ok (= (length (clysm/stage1::summary-top-blockers summary)) 1)
        "merged to single blocker")
    (let ((blocker (first (clysm/stage1::summary-top-blockers summary))))
      (ok (= (clysm/stage1::blocker-info-affected-forms blocker) 8)
          "blocker has combined count 8"))))

(deftest test-top-blockers-limited-to-5
  "Summary should only include top 5 blockers."
  (let* ((failures (loop for i from 1 to 10
                         collect (clysm/stage1::make-failure-group
                                  :operator (intern (format nil "OP~D" i))
                                  :count (* i 10)
                                  :example (format nil "(op~D ...)" i))))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100 :compiled 0
                 :failed 100 :skipped 0 :failures failures))
         (summary (clysm/stage1::generate-summary (list stats))))
    (ok (<= (length (clysm/stage1::summary-top-blockers summary)) 5)
        "at most 5 blockers")))

;;; ==========================================================================
;;; Progress Report Tests
;;; ==========================================================================

(deftest test-format-timestamp
  "format-timestamp should return ISO 8601 format."
  (let ((ts (clysm/stage1::format-timestamp)))
    (ok (stringp ts) "returns a string")
    (ok (= (length ts) 20) "correct length for ISO 8601")
    (ok (char= (char ts 4) #\-) "has dash separator")
    (ok (char= (char ts 10) #\T) "has T separator")
    (ok (char= (char ts 19) #\Z) "ends with Z")))

(deftest test-write-progress-report-json
  "write-progress-report should output valid JSON structure."
  (let* ((summary (clysm/stage1::make-summary
                   :total-forms 100 :compiled 80 :failed 20
                   :skipped 0 :coverage-pct 80.0 :top-blockers nil))
         (report (clysm/stage1::make-progress-report
                  :timestamp "2025-12-27T00:00:00Z"
                  :stage0-version "test"
                  :modules nil
                  :summary summary))
         (output (with-output-to-string (s)
                   (clysm/stage1:write-progress-report report :stream s :format :json))))
    (ok (search "\"timestamp\"" output)
        "JSON has timestamp field")
    (ok (search "\"summary\"" output)
        "JSON has summary field")
    (ok (search "\"coverage_pct\"" output)
        "JSON has coverage_pct field")))

