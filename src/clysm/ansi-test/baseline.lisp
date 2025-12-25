;;;; baseline.lisp - Baseline management for regression detection
;;;;
;;;; Saves and loads baseline results for detecting regressions.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T049-T057: Baseline management (Phase 6 - stubbed for now)
;;; ==========================================================================

(defparameter *baseline-directory*
  (merge-pathnames #p"baselines/" (asdf:system-source-directory :clysm/ansi-test))
  "Directory for storing baseline files.")

(defun save-baseline (coverage-report &optional (path *baseline-directory*))
  "Save COVERAGE-REPORT as a baseline for future comparisons.
Returns the path to the saved baseline file."
  (ensure-directories-exist path)
  (let* ((timestamp (coverage-report-timestamp coverage-report))
         (filename (format nil "baseline-~A.sexp"
                          (substitute #\- #\: (substitute #\- #\T timestamp))))
         (filepath (merge-pathnames filename path)))
    (with-open-file (out filepath :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (let ((*print-readably* t)
            (*print-pretty* nil))
        (prin1 (serialize-coverage-report coverage-report) out)))
    filepath))

(defun load-baseline (&optional (path *baseline-directory*))
  "Load the most recent baseline from PATH.
Returns a coverage-report or NIL if no baseline exists."
  (let* ((pattern (merge-pathnames "baseline-*.sexp" path))
         (files (directory pattern)))
    (when files
      (let ((latest (first (sort files #'> :key #'file-write-date))))
        (with-open-file (in latest :direction :input)
          (deserialize-coverage-report (read in)))))))

(defun compare-to-baseline (current-report baseline-report)
  "Compare CURRENT-REPORT to BASELINE-REPORT.
Returns a baseline-comparison structure."
  (let* ((current-summary (coverage-report-summary current-report))
         (baseline-summary (coverage-report-summary baseline-report))
         (current-pass (report-summary-pass-count current-summary))
         (baseline-pass (report-summary-pass-count baseline-summary))
         (delta (- current-pass baseline-pass))
         (new-failures (find-new-failures current-report baseline-report))
         (new-passes (find-new-passes current-report baseline-report)))
    (make-baseline-comparison
     :baseline-timestamp (coverage-report-timestamp baseline-report)
     :current-timestamp (coverage-report-timestamp current-report)
     :baseline-pass-count baseline-pass
     :current-pass-count current-pass
     :delta delta
     :new-failures new-failures
     :new-passes new-passes
     :regression-p (< current-pass baseline-pass))))

;;; ==========================================================================
;;; Serialization helpers
;;; ==========================================================================

(defun serialize-coverage-report (report)
  "Serialize COVERAGE-REPORT to a readable S-expression."
  (list :coverage-report
        :timestamp (coverage-report-timestamp report)
        :branch (coverage-report-branch report)
        :commit (coverage-report-commit report)
        :summary (serialize-summary (coverage-report-summary report))
        :categories (mapcar #'serialize-category-result
                           (coverage-report-categories report))))

(defun serialize-summary (summary)
  "Serialize REPORT-SUMMARY to S-expression."
  (list :total-count (report-summary-total-count summary)
        :pass-count (report-summary-pass-count summary)
        :fail-count (report-summary-fail-count summary)
        :skip-count (report-summary-skip-count summary)
        :duration-ms (report-summary-duration-ms summary)))

(defun serialize-category-result (cat)
  "Serialize CATEGORY-RESULT to S-expression (without individual results)."
  (list :name (category-result-name cat)
        :total-count (category-result-total-count cat)
        :pass-count (category-result-pass-count cat)
        :fail-count (category-result-fail-count cat)
        :skip-count (category-result-skip-count cat)
        :duration-ms (category-result-duration-ms cat)))

(defun deserialize-coverage-report (data)
  "Deserialize coverage-report from S-expression DATA."
  (make-coverage-report
   :timestamp (getf (cdr data) :timestamp)
   :branch (getf (cdr data) :branch)
   :commit (getf (cdr data) :commit)
   :summary (deserialize-summary (getf (cdr data) :summary))
   :categories (mapcar #'deserialize-category-result
                       (getf (cdr data) :categories))))

(defun deserialize-summary (data)
  "Deserialize report-summary from S-expression DATA."
  (make-report-summary
   :total-count (getf data :total-count)
   :pass-count (getf data :pass-count)
   :fail-count (getf data :fail-count)
   :skip-count (getf data :skip-count)
   :duration-ms (getf data :duration-ms)))

(defun deserialize-category-result (data)
  "Deserialize category-result from S-expression DATA."
  (make-category-result
   :name (getf data :name)
   :total-count (getf data :total-count)
   :pass-count (getf data :pass-count)
   :fail-count (getf data :fail-count)
   :skip-count (getf data :skip-count)
   :duration-ms (getf data :duration-ms)
   :results nil))

;;; ==========================================================================
;;; Comparison helpers
;;; ==========================================================================

(defun find-new-failures (current baseline)
  "Find tests that passed in BASELINE but fail in CURRENT."
  (declare (ignore current baseline))
  ;; TODO: Implement in Phase 6
  nil)

(defun find-new-passes (current baseline)
  "Find tests that failed in BASELINE but pass in CURRENT."
  (declare (ignore current baseline))
  ;; TODO: Implement in Phase 6
  nil)
