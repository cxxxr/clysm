;;;; blocker.lisp - Blocker analysis and impact estimation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Analyzes compilation blockers and estimates fix priorities

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Blocker Analysis
;;; ==========================================================================

(defun analyze-blockers (progress-report)
  "Analyze blockers from a progress report and generate detailed analysis.
Returns list of blocker-info structs with impact estimates."
  (let* ((report-summary (progress-report-summary progress-report))
         (all-failures (make-hash-table :test 'eq))
         (total-forms (summary-total-forms report-summary)))
    ;; Aggregate all failures across modules
    (dolist (module-stats (progress-report-modules progress-report))
      (dolist (failure (module-stats-failures module-stats))
        (let* ((op (failure-group-operator failure))
               (existing (gethash op all-failures)))
          (if existing
              (progn
                (incf (failure-group-count existing)
                      (failure-group-count failure))
                ;; Keep up to 3 examples
                (when (< (length (failure-group-examples existing)) 3)
                  (push (failure-group-example failure)
                        (failure-group-examples existing))))
              (setf (gethash op all-failures)
                    (make-failure-group
                     :operator op
                     :count (failure-group-count failure)
                     :example (failure-group-example failure)
                     :examples (list (failure-group-example failure))))))))
    ;; Convert to blocker-info list with impact
    (let ((blockers nil))
      (maphash (lambda (op failure)
                 (let* ((count (failure-group-count failure))
                        (impact (if (zerop total-forms)
                                    0.0
                                    (* 100.0 (/ count total-forms))))
                        (priority (estimate-priority op count impact)))
                   (push (make-blocker-info
                          :operator op
                          :affected-forms count
                          :impact-pct impact
                          :priority priority
                          :recommendation (generate-recommendation op)
                          :examples (failure-group-examples failure))
                         blockers)))
               all-failures)
      ;; Sort by impact
      (sort blockers #'> :key #'blocker-info-affected-forms))))

(defun estimate-priority (operator count impact)
  "Estimate fix priority based on operator type and impact.
Returns \"HIGH\", \"MEDIUM\", or \"LOW\"."
  (declare (ignore operator))
  (cond
    ((> impact 10.0) "HIGH")
    ((> impact 5.0) "MEDIUM")
    ((> count 10) "MEDIUM")
    (t "LOW")))

(defun generate-recommendation (operator)
  "Generate a recommendation string for fixing a blocker."
  (case operator
    (loop "Implement LOOP macro expansion to blessed subset")
    (format "Extend FORMAT directive support")
    (defstruct "Add DEFSTRUCT compilation or expand to DEFCLASS")
    (define-condition "Expand DEFINE-CONDITION to DEFCLASS")
    (declare "Handle DECLARE forms (skip or process declarations)")
    (t (format nil "Implement ~A support in Stage 0" operator))))

;;; ==========================================================================
;;; Impact Estimation
;;; ==========================================================================

(defun estimate-blocker-impact (blocker total-forms)
  "Update blocker-info with accurate impact percentage."
  (setf (blocker-info-impact-pct blocker)
        (if (zerop total-forms)
            0.0
            (* 100.0 (/ (blocker-info-affected-forms blocker) total-forms))))
  blocker)

(defun calculate-coverage-if-fixed (progress-report blockers-to-fix)
  "Calculate potential coverage if specified blockers were fixed.
BLOCKERS-TO-FIX is list of operator symbols.
Returns new coverage percentage."
  (let* ((summary (progress-report-summary progress-report))
         (total (summary-total-forms summary))
         (compiled (summary-compiled summary))
         (recovered 0))
    ;; Sum up forms that would be recovered
    (let ((all-blockers (analyze-blockers progress-report)))
      (dolist (blocker all-blockers)
        (when (member (blocker-info-operator blocker) blockers-to-fix)
          (incf recovered (blocker-info-affected-forms blocker)))))
    ;; Calculate new coverage
    (if (zerop total)
        0.0
        (* 100.0 (/ (+ compiled recovered) total)))))

;;; ==========================================================================
;;; Blocker Report Generation
;;; ==========================================================================

(defun generate-blocker-report (progress-report &key (stream *standard-output*)
                                                      (format :json))
  "Generate a detailed blocker report.
FORMAT can be :json or :text."
  (let ((blockers (analyze-blockers progress-report))
        (summary (progress-report-summary progress-report)))
    (ecase format
      (:json (write-blocker-json blockers summary stream))
      (:text (write-blocker-text blockers summary stream)))))

(defun write-blocker-json (blockers summary stream)
  "Write blocker report as JSON."
  (format stream "{~%")
  (format stream "  \"analysis_timestamp\": ~S,~%" (format-timestamp))
  (format stream "  \"total_forms\": ~D,~%" (summary-total-forms summary))
  (format stream "  \"current_coverage\": ~,2F,~%" (summary-coverage-pct summary))
  (format stream "  \"blocker_count\": ~D,~%" (length blockers))
  (format stream "  \"blockers\": [")
  (loop for blocker in blockers
        for first = t then nil
        do (unless first (format stream ","))
           (format stream "~%    {")
           (format stream "\"operator\": ~S, "
                   (symbol-name (blocker-info-operator blocker)))
           (format stream "\"affected_forms\": ~D, "
                   (blocker-info-affected-forms blocker))
           (format stream "\"impact_pct\": ~,2F, "
                   (blocker-info-impact-pct blocker))
           (format stream "\"priority\": ~S, "
                   (blocker-info-priority blocker))
           (format stream "\"recommendation\": ~S"
                   (blocker-info-recommendation blocker))
           (format stream "}"))
  (format stream "~%  ]~%")
  (format stream "}~%"))

(defun write-blocker-text (blockers summary stream)
  "Write blocker report as human-readable text."
  (format stream "=== Blocker Analysis Report ===~%~%")
  (format stream "Current Coverage: ~,2F% (~D/~D forms)~%~%"
          (summary-coverage-pct summary)
          (summary-compiled summary)
          (summary-total-forms summary))
  (format stream "Total Blockers: ~D~%~%" (length blockers))
  (format stream "Blockers by Impact:~%")
  (format stream "~40A ~8A ~8A ~8A~%"
          "Operator" "Forms" "Impact" "Priority")
  (format stream "~40,1,0,'-A~%" "")
  (dolist (blocker blockers)
    (format stream "~40A ~8D ~7,2F% ~8A~%"
            (blocker-info-operator blocker)
            (blocker-info-affected-forms blocker)
            (blocker-info-impact-pct blocker)
            (blocker-info-priority blocker)))
  (format stream "~%Recommendations:~%")
  (dolist (blocker (subseq blockers 0 (min 5 (length blockers))))
    (format stream "  - ~A: ~A~%"
            (blocker-info-operator blocker)
            (blocker-info-recommendation blocker))))
