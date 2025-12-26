;;;; baseline-test.lisp - Unit tests for baseline management
;;;;
;;;; T058: serialize-report-to-json tests
;;;; T059: deserialize-baseline-from-json tests
;;;; T060: compare-baseline regression detection tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T058: serialize-report-to-json tests (using S-expressions, not JSON)
;;; ==========================================================================

(deftest serialize-coverage-report-basic
  "Test that serialize-report produces readable output"
  (let* ((summary (make-report-summary
                   :total-count 100
                   :pass-count 40
                   :fail-count 10
                   :skip-count 50
                   :duration-ms 5000))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :branch "main"
                  :commit "abc123"
                  :categories nil
                  :summary summary)))
    (uiop:with-temporary-file (:pathname path :type "sexp" :keep nil)
      (let ((result-path (save-baseline report (directory-namestring path))))
        (ok (probe-file result-path) "Baseline file is created")))))

;;; ==========================================================================
;;; T059: deserialize-baseline-from-json tests (using S-expressions)
;;; ==========================================================================

(deftest roundtrip-baseline-save-load
  "Test that save-baseline and load-baseline produce consistent results"
  (let* ((summary (make-report-summary
                   :total-count 100
                   :pass-count 40
                   :fail-count 10
                   :skip-count 50
                   :duration-ms 5000))
         (report (make-coverage-report
                  :timestamp "2025-12-25T10:00:00Z"
                  :branch "main"
                  :commit "abc123"
                  :categories nil
                  :summary summary)))
    (uiop:with-temporary-file (:pathname path :type "sexp" :keep nil :direction :output)
      (let* ((dir (directory-namestring path))
             (_ (save-baseline report dir))
             (loaded (load-baseline dir)))
        (declare (ignore _))
        (when loaded
          (ok (equal (coverage-report-timestamp report)
                     (coverage-report-timestamp loaded))
              "Timestamp is preserved")
          (ok (equal (coverage-report-branch report)
                     (coverage-report-branch loaded))
              "Branch is preserved")
          (ok (= (report-summary-pass-count summary)
                 (report-summary-pass-count (coverage-report-summary loaded)))
              "Pass count is preserved"))))))

;;; ==========================================================================
;;; T060: compare-baseline regression detection tests
;;; ==========================================================================

(deftest compare-to-baseline-detects-regression
  "Test that compare-to-baseline detects pass count decrease"
  (let* ((baseline-summary (make-report-summary
                            :total-count 100
                            :pass-count 50))
         (current-summary (make-report-summary
                           :total-count 100
                           :pass-count 40))
         (baseline (make-coverage-report
                    :timestamp "2025-12-24T10:00:00Z"
                    :summary baseline-summary))
         (current (make-coverage-report
                   :timestamp "2025-12-25T10:00:00Z"
                   :summary current-summary))
         (comparison (compare-to-baseline current baseline)))
    (ok (baseline-comparison-p comparison) "Returns baseline-comparison struct")
    (ok (baseline-comparison-regression-p comparison) "Detects regression")
    (ok (= -10 (baseline-comparison-delta comparison)) "Calculates correct delta")))

(deftest compare-to-baseline-detects-improvement
  "Test that compare-to-baseline detects pass count increase"
  (let* ((baseline-summary (make-report-summary
                            :total-count 100
                            :pass-count 40))
         (current-summary (make-report-summary
                           :total-count 100
                           :pass-count 50))
         (baseline (make-coverage-report
                    :timestamp "2025-12-24T10:00:00Z"
                    :summary baseline-summary))
         (current (make-coverage-report
                   :timestamp "2025-12-25T10:00:00Z"
                   :summary current-summary))
         (comparison (compare-to-baseline current baseline)))
    (ok (not (baseline-comparison-regression-p comparison)) "Does not flag improvement as regression")
    (ok (= 10 (baseline-comparison-delta comparison)) "Calculates correct positive delta")))

(deftest compare-to-baseline-no-change
  "Test that compare-to-baseline handles no change"
  (let* ((summary (make-report-summary
                   :total-count 100
                   :pass-count 50))
         (baseline (make-coverage-report
                    :timestamp "2025-12-24T10:00:00Z"
                    :summary summary))
         (current (make-coverage-report
                   :timestamp "2025-12-25T10:00:00Z"
                   :summary summary))
         (comparison (compare-to-baseline current baseline)))
    (ok (not (baseline-comparison-regression-p comparison)) "No change is not regression")
    (ok (= 0 (baseline-comparison-delta comparison)) "Delta is zero")))
