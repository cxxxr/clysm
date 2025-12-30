;;;; progress.lisp - Compilation progress tracking and reporting
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tracks compilation progress and generates reports

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Progress Tracking State
;;; ==========================================================================

(defvar *current-progress* nil
  "Current progress-report being built during compilation.")

(defvar *current-module-stats* nil
  "Current module-stats being built.")

(defvar *failure-index* nil
  "Hash table mapping operators to failure groups.")

;;; ==========================================================================
;;; Module Tracking
;;; ==========================================================================

(defun start-module-tracking (module)
  "Begin tracking compilation for a module.
MODULE is a source-module struct."
  (setf *current-module-stats*
        (make-module-stats
         :path (source-module-relative-path module)
         :total-forms (length (source-module-forms module))
         :compiled 0
         :failed 0
         :skipped 0
         :failures nil))
  (setf *failure-index* (make-hash-table :test 'eq))
  *current-module-stats*)

(defun record-form-result (result)
  "Record the result of compiling a single form.
RESULT is a compilation-result struct."
  (when *current-module-stats*
    (if (compilation-result-success-p result)
        (incf (module-stats-compiled *current-module-stats*))
        (progn
          (incf (module-stats-failed *current-module-stats*))
          ;; Track failure by operator
          (let* ((form (compilation-result-form result))
                 (operator (if form
                               (source-form-operator form)
                               (compilation-result-error-type result)))
                 (existing (gethash operator *failure-index*)))
            (if existing
                (incf (failure-group-count existing))
                (setf (gethash operator *failure-index*)
                      (make-failure-group
                       :operator operator
                       :count 1
                       :example (if form
                                    (source-form-source-text form)
                                    (compilation-result-error-message result))))))))))

(defun record-skipped-form (form)
  "Record that a form was skipped (non-compilable)."
  (declare (ignore form))
  (when *current-module-stats*
    (incf (module-stats-skipped *current-module-stats*))))

(defun complete-module-tracking ()
  "Complete tracking for current module and return stats.
Converts failure index to failure list."
  (when *current-module-stats*
    (setf (module-stats-failures *current-module-stats*)
          (loop for group being the hash-values of *failure-index*
                collect group))
    (prog1 *current-module-stats*
      (setf *current-module-stats* nil
            *failure-index* nil))))

;;; ==========================================================================
;;; Report Generation
;;; ==========================================================================

(defun generate-summary (module-stats-list)
  "Generate summary from list of module-stats.
Returns a summary struct."
  (let ((total 0)
        (compiled 0)
        (failed 0)
        (skipped 0)
        (all-failures nil))
    ;; Aggregate counts
    (dolist (stats module-stats-list)
      (incf total (module-stats-total-forms stats))
      (incf compiled (module-stats-compiled stats))
      (incf failed (module-stats-failed stats))
      (incf skipped (module-stats-skipped stats))
      ;; Merge failures
      (dolist (failure (module-stats-failures stats))
        (let ((existing (find (failure-group-operator failure)
                              all-failures
                              :key #'failure-group-operator)))
          (if existing
              (incf (failure-group-count existing)
                    (failure-group-count failure))
              (push (copy-failure-group failure) all-failures)))))
    ;; Sort failures by count and get top 5
    (let ((sorted-failures (sort all-failures #'> :key #'failure-group-count))
          (coverage (if (zerop total) 0.0
                        (* 100.0 (/ compiled total)))))
      (make-summary
       :total-forms total
       :compiled compiled
       :failed failed
       :skipped skipped
       :coverage-pct coverage
       :top-blockers (mapcar #'failure-to-blocker
                             (subseq sorted-failures 0 (min 5 (length sorted-failures))))))))

(defun failure-to-blocker (failure-group)
  "Convert a failure-group to a blocker-info struct."
  (make-blocker-info
   :operator (failure-group-operator failure-group)
   :affected-forms (failure-group-count failure-group)
   :impact-pct 0.0  ; Calculated in generate-summary
   :priority (if (> (failure-group-count failure-group) 10) "HIGH"
                 (if (> (failure-group-count failure-group) 5) "MEDIUM" "LOW"))
   :recommendation (format nil "Implement ~A support"
                           (failure-group-operator failure-group))
   :examples (list (failure-group-example failure-group))))

(defun generate-progress-report (modules results)
  "Generate a complete progress report from modules and results.
MODULES is list of source-module structs.
RESULTS is list of compilation-result structs (matched by form-id).

Phase 13D-7: Updated to properly use the flat results list from
unified module compilation."
  ;; Build index from form-id to result for fast lookup
  (let ((result-index (make-hash-table :test 'equal))
        (module-stats nil))
    (dolist (result results)
      (setf (gethash (compilation-result-form-id result) result-index) result))
    ;; Generate stats per module
    (dolist (module modules)
      (start-module-tracking module)
      (dolist (form (source-module-forms module))
        (if (source-form-compilable-p form)
            (let ((result (gethash (source-form-id form) result-index)))
              (if result
                  (record-form-result result)
                  ;; No result found - mark as not compiled
                  (record-form-result
                   (make-compilation-result
                    :form form
                    :form-id (source-form-id form)
                    :success-p nil
                    :error-type :not-processed
                    :error-message "Form not processed"))))
            (record-skipped-form form)))
      (push (complete-module-tracking) module-stats))
    (setf module-stats (nreverse module-stats))
    ;; Build report
    (make-progress-report
     :timestamp (format-timestamp)
     :stage0-version (get-stage0-version)
     :modules module-stats
     :summary (generate-summary module-stats))))

(defun format-timestamp ()
  "Return current time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defun get-stage0-version ()
  "Get Stage 0 version string (currently returns placeholder)."
  ;; TODO: Read from Stage 0 binary metadata or git commit
  "stage0-v1.0")

;;; ==========================================================================
;;; Report Output
;;; ==========================================================================

(defun write-progress-report (report &key (stream *standard-output*) (format :json))
  "Write progress report to stream.
FORMAT can be :json or :text."
  (ecase format
    (:json (write-report-json report stream))
    (:text (write-report-text report stream))))

(defun write-report-json (report stream)
  "Write report as JSON to stream."
  (format stream "{~%")
  (format stream "  \"timestamp\": ~S,~%" (progress-report-timestamp report))
  (format stream "  \"stage0_version\": ~S,~%" (progress-report-stage0-version report))
  ;; Summary
  (let ((summary (progress-report-summary report)))
    (format stream "  \"summary\": {~%")
    (format stream "    \"total_forms\": ~D,~%" (summary-total-forms summary))
    (format stream "    \"compiled\": ~D,~%" (summary-compiled summary))
    (format stream "    \"failed\": ~D,~%" (summary-failed summary))
    (format stream "    \"skipped\": ~D,~%" (summary-skipped summary))
    (format stream "    \"coverage_pct\": ~,2F,~%" (summary-coverage-pct summary))
    (format stream "    \"top_blockers\": [")
    (loop for blocker in (summary-top-blockers summary)
          for first = t then nil
          do (unless first (format stream ","))
             (format stream "~%      {\"operator\": ~S, \"count\": ~D, \"priority\": ~S}"
                     (symbol-name (blocker-info-operator blocker))
                     (blocker-info-affected-forms blocker)
                     (blocker-info-priority blocker)))
    (format stream "~%    ]~%")
    (format stream "  },~%"))
  ;; Modules
  (format stream "  \"modules\": [")
  (loop for stats in (progress-report-modules report)
        for first = t then nil
        do (unless first (format stream ","))
           (format stream "~%    {\"path\": ~S, \"compiled\": ~D, \"failed\": ~D, \"total\": ~D}"
                   (module-stats-path stats)
                   (module-stats-compiled stats)
                   (module-stats-failed stats)
                   (module-stats-total-forms stats)))
  (format stream "~%  ]~%")
  (format stream "}~%"))

(defun write-report-text (report stream)
  "Write report as human-readable text to stream."
  (let ((summary (progress-report-summary report)))
    (format stream "=== Stage 1 Compilation Progress Report ===~%")
    (format stream "Generated: ~A~%" (progress-report-timestamp report))
    (format stream "Stage 0: ~A~%~%" (progress-report-stage0-version report))
    (format stream "Summary:~%")
    (format stream "  Total forms: ~D~%" (summary-total-forms summary))
    (format stream "  Compiled:    ~D~%" (summary-compiled summary))
    (format stream "  Failed:      ~D~%" (summary-failed summary))
    (format stream "  Skipped:     ~D~%" (summary-skipped summary))
    (format stream "  Coverage:    ~,2F%~%~%" (summary-coverage-pct summary))
    (format stream "Top Blockers:~%")
    (loop for blocker in (summary-top-blockers summary)
          do (format stream "  - ~A: ~D forms (~A)~%"
                     (blocker-info-operator blocker)
                     (blocker-info-affected-forms blocker)
                     (blocker-info-priority blocker)))))
