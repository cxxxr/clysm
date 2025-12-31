;;;; error-analysis.lisp - Error analysis infrastructure for Stage 1 compilation
;;;;
;;;; Part of Phase 13D M4: DEFUN Blocker Analysis and Resolution
;;;; Provides detailed error logging, pattern classification, and reporting
;;;; for DEFUN compilation failures.

(in-package #:clysm/stage0)

;;; ============================================================================
;;; Error Log Entry
;;; ============================================================================

(defstruct (error-log-entry (:constructor make-error-log-entry-internal))
  "Individual DEFUN compilation failure with full context for debugging.
See spec: specs/001-m4-defun-blocker-analysis/data-model.md"
  (function-name "" :type string)
  (module-path "" :type string)
  (error-type :compile-error :type keyword) ; :compile-error or :validation-error
  (error-message "" :type string)
  (pattern-id "" :type string)
  (lambda-list nil :type (or null string))
  (failing-subform nil :type (or null string))
  (timestamp "" :type string))

(defun make-error-log-entry (&key function-name module-path error-type
                                   error-message pattern-id lambda-list
                                   failing-subform timestamp)
  "Create an error log entry with validation.
FUNCTION-NAME: Name of the DEFUN that failed
MODULE-PATH: Source file path
ERROR-TYPE: :compile-error or :validation-error
ERROR-MESSAGE: Full error message from condition
PATTERN-ID: Normalized pattern identifier (e.g., \"P001\")
LAMBDA-LIST: Original lambda-list string (optional)
FAILING-SUBFORM: First subform that caused the error (optional)
TIMESTAMP: ISO 8601 timestamp"
  (make-error-log-entry-internal
   :function-name (or function-name "")
   :module-path (or module-path "")
   :error-type (or error-type :compile-error)
   :error-message (or error-message "")
   :pattern-id (or pattern-id "")
   :lambda-list lambda-list
   :failing-subform failing-subform
   :timestamp (or timestamp (get-iso-timestamp))))

;;; ============================================================================
;;; Error Pattern Category
;;; ============================================================================

(defstruct (error-pattern-category (:constructor make-error-pattern-category-internal))
  "Classification of similar errors for batch analysis.
See spec: specs/001-m4-defun-blocker-analysis/data-model.md"
  (pattern-id "" :type string)
  (pattern "" :type string)
  (count 0 :type integer)
  (percentage 0.0 :type single-float)
  (priority :low :type keyword) ; :high, :medium, :low
  (examples nil :type list)     ; List of (:function name :module path)
  (affected-modules nil :type list)
  (suggested-fix nil :type (or null string)))

(defun make-error-pattern-category (&key pattern-id pattern count percentage
                                          priority examples affected-modules
                                          suggested-fix)
  "Create an error pattern category with computed priority.
PATTERN-ID: Unique identifier (e.g., \"P001\")
PATTERN: Normalized error pattern template
COUNT: Total occurrences
PERCENTAGE: Percentage of total DEFUN failures
PRIORITY: Computed from count/percentage (:high, :medium, :low)
EXAMPLES: First 3 example failures
AFFECTED-MODULES: List of unique module paths
SUGGESTED-FIX: Brief description of likely fix (optional)"
  (let ((computed-priority (or priority
                               (compute-pattern-priority count percentage))))
    (make-error-pattern-category-internal
     :pattern-id (or pattern-id "")
     :pattern (or pattern "")
     :count (or count 0)
     :percentage (or percentage 0.0)
     :priority computed-priority
     :examples (if (> (length examples) 3)
                   (subseq examples 0 3)
                   examples)
     :affected-modules (or affected-modules nil)
     :suggested-fix suggested-fix)))

(defun compute-pattern-priority (count percentage)
  "Compute priority based on count and percentage.
HIGH: count > 1000 OR percentage > 5%
MEDIUM: count > 100 OR percentage > 1%
LOW: everything else"
  (let ((c (or count 0))
        (p (or percentage 0.0)))
    (cond ((or (> c 1000) (> p 5.0)) :high)
          ((or (> c 100) (> p 1.0)) :medium)
          (t :low))))

;;; ============================================================================
;;; Compilation Report Extensions
;;; ============================================================================

(defstruct (error-patterns-summary (:constructor make-error-patterns-summary-internal))
  "Summary of error patterns for compilation report.
Extends the existing stage1-report.json with error_patterns section."
  (total-patterns 0 :type integer)
  (top-patterns nil :type list) ; List of error-pattern-category
  (coverage-pct 0.0 :type single-float)) ; Percentage of failures covered by top 10

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun get-iso-timestamp ()
  "Return current time as ISO 8601 timestamp string."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun extract-failing-subform (form condition)
  "Extract the most likely failing subform from a compilation error.
FORM: The DEFUN form being compiled
CONDITION: The error condition that was signaled
Returns: String representation of the failing subform, or NIL"
  (declare (ignore condition))
  ;; For now, return the first body form if it exists
  ;; This will be enhanced based on actual error analysis
  (when (and (listp form)
             (eq (car form) 'defun)
             (>= (length form) 4))
    (let ((body (cdddr form)))
      (when body
        (format nil "~S" (car body))))))

(defun normalize-error-pattern (error-message)
  "Normalize an error message into a pattern key.
- Remove line numbers and file paths
- Replace specific names with placeholders
Returns: Normalized pattern string"
  (let ((pattern error-message))
    ;; Take first line only for classification
    (let ((newline-pos (position #\Newline pattern)))
      (when newline-pos
        (setf pattern (subseq pattern 0 newline-pos))))
    ;; Simple normalization without regex:
    ;; - Replace sequences of digits with N
    ;; - Keep the structure of the message
    (let ((result (make-array (length pattern)
                              :element-type 'character
                              :fill-pointer 0
                              :adjustable t))
          (in-number nil))
      (loop for char across pattern
            do (cond
                 ;; Skip sequences of digits (line numbers, etc.)
                 ((digit-char-p char)
                  (unless in-number
                    (vector-push-extend #\N result))
                  (setf in-number t))
                 ;; Normal characters
                 (t
                  (setf in-number nil)
                  (vector-push-extend char result))))
      (coerce result 'string))))

(defun classify-error-pattern (error-message)
  "Classify an error message into a pattern ID.
Returns: (VALUES pattern-id normalized-pattern)"
  (let ((normalized (normalize-error-pattern error-message)))
    ;; Generate a pattern ID from hash of normalized pattern
    (let* ((hash (sxhash normalized))
           (pattern-id (format nil "P~3,'0D" (mod hash 1000))))
      (values pattern-id normalized))))

;;; ============================================================================
;;; Error Collection State
;;; ============================================================================

(defvar *defun-errors* nil
  "List of error-log-entry instances collected during Stage 1 compilation.")

(defvar *error-patterns* (make-hash-table :test 'equal)
  "Hash table mapping pattern-id to error-pattern-category.")

(defun clear-error-analysis ()
  "Clear all collected error data for a fresh compilation run."
  (setf *defun-errors* nil)
  (clrhash *error-patterns*))

(defun add-defun-error (entry)
  "Add an error log entry to the collection.
Also updates pattern aggregation."
  (push entry *defun-errors*)
  ;; Update pattern aggregation
  (let* ((pattern-id (error-log-entry-pattern-id entry))
         (existing (gethash pattern-id *error-patterns*)))
    (if existing
        ;; Update existing pattern
        (progn
          (incf (error-pattern-category-count existing))
          ;; Add to examples if under limit
          (when (< (length (error-pattern-category-examples existing)) 3)
            (push (list :function (error-log-entry-function-name entry)
                        :module (error-log-entry-module-path entry))
                  (error-pattern-category-examples existing)))
          ;; Add to affected modules if not already present
          (pushnew (error-log-entry-module-path entry)
                   (error-pattern-category-affected-modules existing)
                   :test #'string=))
        ;; Create new pattern
        (setf (gethash pattern-id *error-patterns*)
              (make-error-pattern-category
               :pattern-id pattern-id
               :pattern (normalize-error-pattern (error-log-entry-error-message entry))
               :count 1
               :examples (list (list :function (error-log-entry-function-name entry)
                                     :module (error-log-entry-module-path entry)))
               :affected-modules (list (error-log-entry-module-path entry)))))))

(defun collect-defun-error (function-name module-path condition
                            &key lambda-list form)
  "Wrapper to collect a DEFUN compilation error.
Called from handler-case in compile-form loop.
FUNCTION-NAME: Name of the DEFUN
MODULE-PATH: Source file path
CONDITION: The signaled condition
LAMBDA-LIST: Original lambda-list (optional)
FORM: The full DEFUN form (optional, for subform extraction)"
  (let ((error-message (format nil "~A" condition)))
    (multiple-value-bind (pattern-id normalized-pattern)
        (classify-error-pattern error-message)
      (declare (ignore normalized-pattern))
      (let ((entry (make-error-log-entry
                    :function-name function-name
                    :module-path module-path
                    :error-type :compile-error
                    :error-message error-message
                    :pattern-id pattern-id
                    :lambda-list lambda-list
                    :failing-subform (when form
                                       (extract-failing-subform form condition)))))
        (add-defun-error entry)
        entry))))

;;; ============================================================================
;;; Aggregation Functions
;;; ============================================================================

(defun aggregate-patterns ()
  "Return sorted list of error patterns by count (descending).
Also computes percentage and priority for each pattern."
  (let* ((total-errors (length *defun-errors*))
         (patterns nil))
    ;; Collect all patterns
    (maphash (lambda (id category)
               (declare (ignore id))
               ;; Update percentage
               (when (> total-errors 0)
                 (setf (error-pattern-category-percentage category)
                       (* 100.0 (/ (error-pattern-category-count category)
                                   total-errors))))
               ;; Update priority based on new percentage
               (setf (error-pattern-category-priority category)
                     (compute-pattern-priority
                      (error-pattern-category-count category)
                      (error-pattern-category-percentage category)))
               (push category patterns))
             *error-patterns*)
    ;; Sort by count descending
    (sort patterns #'> :key #'error-pattern-category-count)))

(defun get-top-patterns (&optional (n 10))
  "Get top N error patterns by occurrence count."
  (let ((sorted (aggregate-patterns)))
    (if (> (length sorted) n)
        (subseq sorted 0 n)
        sorted)))

(defun compute-top-patterns-coverage (&optional (n 10))
  "Compute what percentage of total errors the top N patterns cover."
  (let* ((top (get-top-patterns n))
         (top-count (reduce #'+ top :key #'error-pattern-category-count :initial-value 0))
         (total (length *defun-errors*)))
    (if (> total 0)
        (* 100.0 (/ top-count total))
        0.0)))

;;; ============================================================================
;;; JSON Output Functions
;;; ============================================================================

(defun error-log-entry-to-json (entry)
  "Convert an error-log-entry to a JSON-compatible plist."
  (list :function_name (error-log-entry-function-name entry)
        :module_path (error-log-entry-module-path entry)
        :error_type (string-downcase (symbol-name (error-log-entry-error-type entry)))
        :error_message (error-log-entry-error-message entry)
        :pattern_id (error-log-entry-pattern-id entry)
        :lambda_list (error-log-entry-lambda-list entry)
        :failing_subform (error-log-entry-failing-subform entry)
        :timestamp (error-log-entry-timestamp entry)))

(defun error-pattern-to-json (pattern)
  "Convert an error-pattern-category to a JSON-compatible plist."
  (list :pattern_id (error-pattern-category-pattern-id pattern)
        :pattern (error-pattern-category-pattern pattern)
        :count (error-pattern-category-count pattern)
        :percentage (error-pattern-category-percentage pattern)
        :priority (string-downcase (symbol-name (error-pattern-category-priority pattern)))
        :examples (mapcar (lambda (ex)
                            (list :function (getf ex :function)
                                  :module (getf ex :module)))
                          (error-pattern-category-examples pattern))
        :affected_modules (error-pattern-category-affected-modules pattern)
        :suggested_fix (error-pattern-category-suggested-fix pattern)))

(defun write-defun-errors-json (filepath)
  "Write detailed DEFUN error log to JSON file.
Output: dist/defun-errors.json"
  (with-open-file (out filepath :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (let ((entries (reverse *defun-errors*))) ; Chronological order
      (format out "{~%")
      (format out "  \"total_entries\": ~D,~%" (length entries))
      (format out "  \"generated_at\": ~S,~%" (get-iso-timestamp))
      (format out "  \"entries\": [~%")
      (loop for entry in entries
            for i from 0
            do (format out "    {~%")
               (let ((json (error-log-entry-to-json entry)))
                 (format out "      \"function_name\": ~S,~%" (getf json :function_name))
                 (format out "      \"module_path\": ~S,~%" (getf json :module_path))
                 (format out "      \"error_type\": ~S,~%" (getf json :error_type))
                 (format out "      \"error_message\": ~S,~%" (getf json :error_message))
                 (format out "      \"pattern_id\": ~S,~%" (getf json :pattern_id))
                 (format out "      \"lambda_list\": ~A,~%"
                         (if (getf json :lambda_list)
                             (format nil "~S" (getf json :lambda_list))
                             "null"))
                 (format out "      \"failing_subform\": ~A,~%"
                         (if (getf json :failing_subform)
                             (format nil "~S" (getf json :failing_subform))
                             "null"))
                 (format out "      \"timestamp\": ~S~%" (getf json :timestamp)))
               (format out "    }~A~%" (if (< i (1- (length entries))) "," "")))
      (format out "  ]~%")
      (format out "}~%"))))

(defun generate-error-patterns-section ()
  "Generate error_patterns section for stage1-report.json.
Returns: JSON string for the error_patterns array."
  (let ((patterns (get-top-patterns 10))
        (coverage (compute-top-patterns-coverage 10)))
    (with-output-to-string (out)
      (format out "\"error_patterns\": [~%")
      (loop for pattern in patterns
            for i from 0
            do (let ((json (error-pattern-to-json pattern)))
                 (format out "    {~%")
                 (format out "      \"pattern_id\": ~S,~%" (getf json :pattern_id))
                 (format out "      \"pattern\": ~S,~%" (getf json :pattern))
                 (format out "      \"count\": ~D,~%" (getf json :count))
                 (format out "      \"percentage\": ~,2F,~%" (getf json :percentage))
                 (format out "      \"priority\": ~S,~%" (getf json :priority))
                 (format out "      \"examples\": [")
                 (loop for ex in (getf json :examples)
                       for j from 0
                       do (format out "{\"function\": ~S, \"module\": ~S}"
                                  (getf ex :function) (getf ex :module))
                          (when (< j (1- (length (getf json :examples))))
                            (format out ", ")))
                 (format out "],~%")
                 (format out "      \"affected_modules\": [")
                 (loop for mod in (getf json :affected_modules)
                       for j from 0
                       do (format out "~S" mod)
                          (when (< j (1- (length (getf json :affected_modules))))
                            (format out ", ")))
                 (format out "]~%")
                 (format out "    }~A~%" (if (< i (1- (length patterns))) "," ""))))
      (format out "  ],~%")
      (format out "  \"top_patterns_coverage_pct\": ~,2F" coverage))))

(defun get-defun-failure-stats ()
  "Return statistics for summary section.
Returns: (VALUES defun-failures defun-failure-reduction)"
  (let ((failures (length *defun-errors*))
        (baseline 18997)) ; From spec.md
    (values failures (- baseline failures))))
