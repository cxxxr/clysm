;;;; blocker.lisp - Blocker analysis and impact estimation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Phase 13D-9: Enhanced with structured blocker reports for fixpoint tracking
;;;; Analyzes compilation blockers and estimates fix priorities

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Phase 13D-9: Remediation Suggestions Map (T027)
;;; ==========================================================================

(defparameter *remediation-suggestions*
  '(;; Unsupported macros
    (loop . "Implement LOOP macro expansion to blessed subset")
    (format . "Extend FORMAT directive support")
    (defstruct . "Add DEFSTRUCT compilation or expand to DEFCLASS")
    (define-condition . "Expand DEFINE-CONDITION to DEFCLASS")
    (declare . "Handle DECLARE forms (skip or process declarations)")
    (handler-case . "Implement HANDLER-CASE for condition handling")
    (handler-bind . "Implement HANDLER-BIND for dynamic handlers")
    (restart-case . "Implement RESTART-CASE for restart handlers")
    (with-slots . "Expand WITH-SLOTS to SLOT-VALUE calls")
    (with-accessors . "Expand WITH-ACCESSORS to accessor calls")
    ;; Type errors
    (type-error . "Fix type mismatch in form compilation")
    (undefined-function . "Define missing function or add import")
    (undefined-variable . "Define missing variable or add special declaration")
    ;; Wasm validation errors
    (wasm-validation . "Check generated Wasm bytecode for spec compliance")
    (wasm-type-mismatch . "Fix Wasm type stack mismatch")
    ;; General
    (unknown . "Investigate error and add specific handling"))
  "Remediation suggestions keyed by error category symbol (T027).")

(defun get-remediation (category-symbol)
  "Get remediation suggestion for a category symbol."
  (or (cdr (assoc category-symbol *remediation-suggestions*))
      (format nil "Implement ~A support" category-symbol)))

;;; ==========================================================================
;;; Phase 13D-9: Error Categorization (T024)
;;; ==========================================================================

(defun categorize-error (error-message operator)
  "Categorize a compilation error into a category name (T024).
Returns a string category name like 'unsupported-macro' or 'type-error'."
  (cond
    ;; Unsupported macro forms
    ((member operator '(loop format defstruct define-condition
                         handler-case handler-bind restart-case
                         with-slots with-accessors))
     "unsupported-macro")
    ;; Declaration forms
    ((member operator '(declare proclamation declaim))
     "unsupported-declaration")
    ;; Missing definitions
    ((and error-message
          (or (search "undefined function" error-message)
              (search "undefined-function" error-message)))
     "missing-function")
    ((and error-message
          (or (search "undefined variable" error-message)
              (search "unbound variable" error-message)))
     "missing-variable")
    ;; Type errors
    ((and error-message
          (or (search "type error" error-message)
              (search "type-error" error-message)
              (search "type mismatch" error-message)))
     "type-error")
    ;; Wasm validation errors
    ((and error-message
          (or (search "wasm" error-message)
              (search "validation" error-message)))
     "wasm-validation-error")
    ;; Fallback to operator-based category
    (operator
     (format nil "unsupported-~(~A~)" operator))
    ;; Unknown
    (t "unknown-error")))

;;; ==========================================================================
;;; Phase 13D-9: Blocker Aggregation (T025)
;;; ==========================================================================

(defun aggregate-blockers (compilation-results)
  "Aggregate compilation failures by category (T025).
COMPILATION-RESULTS is a list of compilation-result structs.
Returns a list of blocker-category structs."
  (let ((categories (make-hash-table :test 'equal)))
    ;; Group failures by category
    (dolist (result compilation-results)
      (unless (compilation-result-success-p result)
        (let* ((form (compilation-result-form result))
               (operator (if (source-form-p form)
                             (source-form-operator form)
                             nil))
               (error-msg (compilation-result-error-message result))
               (category-name (categorize-error error-msg operator))
               (form-name (if (source-form-p form)
                              (or (source-form-name form)
                                  (format nil "~A" (source-form-operator form)))
                              "unknown")))
          (let ((existing (gethash category-name categories)))
            (if existing
                (progn
                  (incf (blocker-category-count existing))
                  (when (< (length (blocker-category-examples existing)) 5)
                    (push form-name (blocker-category-examples existing))))
                (setf (gethash category-name categories)
                      (make-blocker-category
                       :name category-name
                       :count 1
                       :examples (list form-name)
                       :remediation (get-remediation
                                      (or operator
                                          (intern (string-upcase category-name)
                                                  :keyword))))))))))
    ;; Convert to sorted list
    (let ((result nil))
      (maphash (lambda (name cat)
                 (declare (ignore name))
                 (push cat result))
               categories)
      (sort result #'> :key #'blocker-category-count))))

;;; ==========================================================================
;;; Phase 13D-9: Blocker Report Creation (T026, T028)
;;; ==========================================================================

(defun create-blocker-report (compilation-results &key (stage 1) fixpoint-status fixpoint-details)
  "Create a complete blocker report from compilation results (T026).
Returns a blocker-report struct matching the contract schema."
  (let* ((total (length compilation-results))
         (success (count-if #'compilation-result-success-p compilation-results))
         (failed (- total success))
         (rate (if (zerop total) 0.0 (* 100.0 (/ success total))))
         (categories (aggregate-blockers compilation-results))
         (top-5 (mapcar #'blocker-category-name
                        (subseq categories 0 (min 5 (length categories))))))
    (make-blocker-report
     :stage stage
     :timestamp (current-iso-timestamp)
     :compilation-rate rate
     :forms-total total
     :forms-success success
     :forms-failed failed
     :blockers categories
     :top-5-blockers top-5
     :fixpoint-status (or fixpoint-status "pending")
     :fixpoint-details fixpoint-details)))

(defun write-blocker-report-json (report stream)
  "Write a blocker-report as JSON to stream (T026).
Matches the schema in contracts/blocker-report.json."
  (format stream "{~%")
  (format stream "  \"stage\": ~D,~%" (blocker-report-stage report))
  (format stream "  \"timestamp\": ~S,~%" (blocker-report-timestamp report))
  (format stream "  \"compilation_rate\": ~,2F,~%" (blocker-report-compilation-rate report))
  (format stream "  \"forms_total\": ~D,~%" (blocker-report-forms-total report))
  (format stream "  \"forms_success\": ~D,~%" (blocker-report-forms-success report))
  (format stream "  \"forms_failed\": ~D,~%" (blocker-report-forms-failed report))
  ;; Blockers array
  (format stream "  \"blockers\": [")
  (loop for cat in (blocker-report-blockers report)
        for first = t then nil
        do (unless first (format stream ","))
           (format stream "~%    {")
           (format stream "\"category\": ~S, " (blocker-category-name cat))
           (format stream "\"count\": ~D, " (blocker-category-count cat))
           (format stream "\"examples\": [")
           (loop for ex in (blocker-category-examples cat)
                 for efirst = t then nil
                 do (unless efirst (format stream ", "))
                    (format stream "~S" ex))
           (format stream "], ")
           (format stream "\"remediation\": ~S" (blocker-category-remediation cat))
           (format stream "}"))
  (format stream "~%  ],~%")
  ;; Top 5 blockers
  (format stream "  \"top_5_blockers\": [")
  (loop for name in (blocker-report-top-5-blockers report)
        for first = t then nil
        do (unless first (format stream ", "))
           (format stream "~S" name))
  (format stream "],~%")
  ;; Fixpoint status
  (format stream "  \"fixpoint_status\": ~S" (blocker-report-fixpoint-status report))
  ;; Fixpoint details (if present)
  (when (blocker-report-fixpoint-details report)
    (format stream ",~%  \"fixpoint_details\": ")
    (write-fixpoint-details-json (blocker-report-fixpoint-details report) stream))
  (format stream "~%}~%"))

(defun write-fixpoint-details-json (details stream)
  "Write fixpoint details as JSON."
  (format stream "{~%")
  (format stream "    \"stage1_size\": ~D,~%"
          (or (getf details :stage1-size) 0))
  (format stream "    \"stage2_size\": ~D,~%"
          (or (getf details :stage2-size) 0))
  (format stream "    \"first_diff_offset\": ~D~%"
          (or (getf details :first-diff-offset) 0))
  (format stream "  }"))

;;; ==========================================================================
;;; Blocker Analysis (Legacy)
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
