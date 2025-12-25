;;;; data-model.lisp - Data structures for ANSI test harness
;;;;
;;;; Implements the core entities defined in data-model.md:
;;;; - test-case: A single ANSI CL test parsed from the test suite
;;;; - test-result: The outcome of executing a test case
;;;; - category-result: Aggregated results for a test category
;;;; - report-summary: Summary statistics for a coverage report
;;;; - coverage-report: Collection of all category results

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T012: test-case defstruct
;;; ==========================================================================

(defstruct (test-case (:constructor make-test-case))
  "A single test case from the ANSI test suite."
  (name nil :type symbol :read-only t)
  (category nil :type string :read-only t)
  (source-file nil :type (or null pathname) :read-only t)
  (form nil :type t :read-only t)
  (expected-values nil :type list :read-only t)
  (metadata nil :type list :read-only t))

;;; ==========================================================================
;;; T013: test-result defstruct
;;; ==========================================================================

(defstruct (test-result (:constructor make-test-result))
  "Result of running a single test case."
  (test-case nil :type (or null test-case) :read-only t)
  (status nil :type (member :pass :fail :skip nil) :read-only t)
  (actual-values nil :type list)
  (skip-reason nil :type (or null string))
  (error-message nil :type (or null string))
  (execution-time-ms 0 :type (unsigned-byte 32)))

;;; ==========================================================================
;;; T014: category-result defstruct with pass-rate accessor
;;; ==========================================================================

(defstruct (category-result (:constructor make-category-result))
  "Aggregated results for a single category."
  (name nil :type string :read-only t)
  (total-count 0 :type (unsigned-byte 32))
  (pass-count 0 :type (unsigned-byte 32))
  (fail-count 0 :type (unsigned-byte 32))
  (skip-count 0 :type (unsigned-byte 32))
  (duration-ms 0 :type (unsigned-byte 32))
  (results nil :type list))

(defun category-pass-rate (cr)
  "Return pass rate as a float between 0.0 and 1.0."
  (let ((total (category-result-total-count cr)))
    (if (zerop total)
        0.0
        (float (/ (category-result-pass-count cr) total)))))

;;; ==========================================================================
;;; T015: report-summary and coverage-report defstructs
;;; ==========================================================================

(defstruct (report-summary (:constructor make-report-summary))
  "Summary statistics for a coverage report."
  (total-count 0 :type (unsigned-byte 32))
  (pass-count 0 :type (unsigned-byte 32))
  (fail-count 0 :type (unsigned-byte 32))
  (skip-count 0 :type (unsigned-byte 32))
  (duration-ms 0 :type (unsigned-byte 32)))

(defstruct (coverage-report (:constructor make-coverage-report))
  "Complete test suite execution report."
  (timestamp nil :type (or null string) :read-only t)
  (branch nil :type (or null string))
  (commit nil :type (or null string))
  (categories nil :type list)
  (summary nil :type (or null report-summary)))

;;; ==========================================================================
;;; baseline-comparison defstruct (for Phase 7, but defined here for coherence)
;;; ==========================================================================

(defstruct (baseline-comparison (:constructor make-baseline-comparison))
  "Comparison between current results and baseline."
  (baseline-timestamp nil :type (or null string))
  (current-timestamp nil :type (or null string) :read-only t)
  (baseline-pass-count 0 :type (unsigned-byte 32))
  (current-pass-count 0 :type (unsigned-byte 32))
  (delta 0 :type integer)
  (new-failures nil :type list)
  (new-passes nil :type list)
  (regression-p nil :type boolean))
