;;; reporter.lisp - Coverage Report Generator for Clysm Validation
;;;
;;; Generates coverage reports and blessed-subset documentation.

(in-package :clysm-validation)

;;; T022: Coverage-Report struct

(defstruct coverage-report
  "A coverage report for a module or directory.

   Slots:
   - scope: Name/path of the analyzed scope
   - total-symbols: Total unique CL symbols found
   - supported-count: Number of fully supported symbols
   - partial-count: Number of partially supported symbols
   - unsupported-count: Number of unsupported symbols
   - unknown-count: Number of unknown symbols (not in registry)
   - coverage-pct: Percentage of supported + partial symbols
   - unsupported-details: List of unsupported symbols with usage info"
  (scope "" :type string)
  (total-symbols 0 :type fixnum)
  (supported-count 0 :type fixnum)
  (partial-count 0 :type fixnum)
  (unsupported-count 0 :type fixnum)
  (unknown-count 0 :type fixnum)
  (coverage-pct 0.0 :type float)
  (unsupported-details nil :type list))

;;; T023: compute-coverage function

(defun compute-coverage (scope-name symbols)
  "Compute coverage report for a list of symbols.
   SCOPE-NAME is a string identifying what was analyzed.
   SYMBOLS is a list of CL symbols."
  (let ((classified (classify-symbols symbols))
        (total (length symbols)))
    (let ((supported (getf classified :supported))
          (partial (getf classified :partial))
          (unsupported (getf classified :unsupported))
          (unknown (getf classified :unknown)))
      (make-coverage-report
       :scope scope-name
       :total-symbols total
       :supported-count (length supported)
       :partial-count (length partial)
       :unsupported-count (length unsupported)
       :unknown-count (length unknown)
       :coverage-pct (if (zerop total)
                         100.0
                         (* 100.0 (/ (+ (length supported) (length partial))
                                     total)))
       :unsupported-details (append unsupported unknown)))))

(defun compute-directory-coverage (dir-name dir-result)
  "Compute coverage report for a directory analysis result."
  (let ((all-symbols (make-hash-table :test 'eq)))
    (maphash (lambda (path usages)
               (declare (ignore path))
               (dolist (usage usages)
                 (setf (gethash (feature-usage-symbol usage) all-symbols) t)))
             dir-result)
    (compute-coverage dir-name (alexandria:hash-table-keys all-symbols))))

(defun compute-all-coverage (analysis-result)
  "Compute coverage reports for all directories and overall.
   Returns a plist (:directories list-of-reports :overall report)"
  (let ((dir-reports nil)
        (all-symbols (make-hash-table :test 'eq)))
    ;; Process each directory
    (maphash (lambda (dir-name dir-result)
               (let ((report (compute-directory-coverage dir-name dir-result)))
                 (push report dir-reports))
               ;; Collect symbols for overall
               (maphash (lambda (path usages)
                          (declare (ignore path))
                          (dolist (usage usages)
                            (setf (gethash (feature-usage-symbol usage) all-symbols) t)))
                        dir-result))
             analysis-result)
    ;; Overall report
    (let ((overall (compute-coverage "Overall" (alexandria:hash-table-keys all-symbols))))
      (list :directories (nreverse dir-reports)
            :overall overall))))

;;; T024: generate-report function (Markdown output)

(defun format-percentage (pct)
  "Format a percentage with one decimal place."
  (format nil "~,1F%" pct))

(defun generate-report (coverage-data &optional (stream *standard-output*))
  "Generate a Markdown coverage report to STREAM.
   COVERAGE-DATA should be the result of compute-all-coverage."
  (let ((dirs (getf coverage-data :directories))
        (overall (getf coverage-data :overall)))
    ;; Header
    (format stream "# Feature Coverage Report~%~%")
    (format stream "Generated: ~A~%~%" (format-timestamp))

    ;; Overall Summary
    (format stream "## Summary~%~%")
    (format stream "- **Total symbols analyzed**: ~D~%"
            (coverage-report-total-symbols overall))
    (format stream "- **Supported**: ~D (~A)~%"
            (coverage-report-supported-count overall)
            (format-percentage (* 100.0 (/ (coverage-report-supported-count overall)
                                           (max 1 (coverage-report-total-symbols overall))))))
    (format stream "- **Partial**: ~D (~A)~%"
            (coverage-report-partial-count overall)
            (format-percentage (* 100.0 (/ (coverage-report-partial-count overall)
                                           (max 1 (coverage-report-total-symbols overall))))))
    (format stream "- **Unsupported**: ~D (~A)~%"
            (coverage-report-unsupported-count overall)
            (format-percentage (* 100.0 (/ (coverage-report-unsupported-count overall)
                                           (max 1 (coverage-report-total-symbols overall))))))
    (format stream "- **Unknown**: ~D~%"
            (coverage-report-unknown-count overall))
    (format stream "- **Overall coverage**: ~A~%~%"
            (format-percentage (coverage-report-coverage-pct overall)))

    ;; Per-Module Coverage Table
    (format stream "## Per-Module Coverage~%~%")
    (format stream "| Directory | Total | Supported | Partial | Unsupported | Coverage |~%")
    (format stream "|-----------|-------|-----------|---------|-------------|----------|~%")
    (dolist (report dirs)
      (format stream "| ~A | ~D | ~D | ~D | ~D | ~A |~%"
              (coverage-report-scope report)
              (coverage-report-total-symbols report)
              (coverage-report-supported-count report)
              (coverage-report-partial-count report)
              (coverage-report-unsupported-count report)
              (format-percentage (coverage-report-coverage-pct report))))
    (format stream "~%")

    ;; Unsupported Features Detail
    (when (coverage-report-unsupported-details overall)
      (format stream "## Unsupported Features~%~%")
      (format stream "| Symbol | Status |~%")
      (format stream "|--------|--------|~%")
      (dolist (sym (coverage-report-unsupported-details overall))
        (format stream "| ~A | ~A |~%"
                sym
                (feature-status sym))))
    (format stream "~%")))

(defun format-timestamp ()
  "Return current timestamp as YYYY-MM-DD string."
  (multiple-value-bind (sec min hr day mon yr)
      (get-decoded-time)
    (declare (ignore sec min hr))
    (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day)))

(defun generate-report-to-file (coverage-data output-path)
  "Generate a Markdown coverage report to a file."
  (with-open-file (stream output-path
                          :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (generate-report coverage-data stream)))

;;; T059-T064: Blessed-Subset struct and functions

(defstruct blessed-subset
  "The documented collection of self-compilable CL features.

   Slots:
   - version: Version string for this subset
   - generation-date: Date when this was generated
   - special-forms: List of supported special forms
   - macros: List of supported macros
   - functions: List of supported functions
   - types: List of supported type specifiers
   - partial-notes: Alist of (symbol . notes) for partial support"
  (version "1.0.0" :type string)
  (generation-date "" :type string)
  (special-forms nil :type list)
  (macros nil :type list)
  (functions nil :type list)
  (types nil :type list)
  (partial-notes nil :type list))

(defun aggregate-verified-features (validation-results)
  "Collect features from successful compilations.
   VALIDATION-RESULTS is a list of compilation-result structs.
   Returns a list of verified CL symbols."
  (let ((verified (make-hash-table :test 'eq)))
    (dolist (result validation-results)
      (when (and (compilation-result-success result)
                 (compilation-result-validation-passed result))
        ;; Get symbols from the module
        (let ((module (compilation-result-module result)))
          (dolist (sym (module-info-symbols-used module))
            (setf (gethash sym verified) t)))))
    (alexandria:hash-table-keys verified)))

(defun categorize-features (symbols)
  "Group symbols into special-forms, macros, functions, types.
   Returns a plist."
  (let ((special-forms nil)
        (macros nil)
        (functions nil)
        (types nil))
    (dolist (sym symbols)
      (let ((feature (get-feature sym)))
        (when feature
          (case (cl-feature-category feature)
            (:special-form (push sym special-forms))
            (:macro (push sym macros))
            (:function (push sym functions))
            (:type (push sym types))
            (:declaration nil)))))  ; Skip declarations
    (list :special-forms (sort special-forms #'string< :key #'symbol-name)
          :macros (sort macros #'string< :key #'symbol-name)
          :functions (sort functions #'string< :key #'symbol-name)
          :types (sort types #'string< :key #'symbol-name))))

(defun collect-partial-notes (symbols)
  "Collect notes for partially supported symbols."
  (let ((notes nil))
    (dolist (sym symbols)
      (let ((feature (get-feature sym)))
        (when (and feature
                   (eq (cl-feature-status feature) :partial)
                   (cl-feature-notes feature))
          (push (cons sym (cl-feature-notes feature)) notes))))
    (sort notes #'string< :key (lambda (x) (symbol-name (car x))))))

(defun generate-blessed-subset (symbols output-path)
  "Generate blessed-subset.lisp file documenting self-compilable features."
  (let* ((categorized (categorize-features symbols))
         (partial-notes (collect-partial-notes symbols))
         (subset (make-blessed-subset
                  :version "1.0.0"
                  :generation-date (format-timestamp)
                  :special-forms (getf categorized :special-forms)
                  :macros (getf categorized :macros)
                  :functions (getf categorized :functions)
                  :types (getf categorized :types)
                  :partial-notes partial-notes)))
    (with-open-file (stream output-path
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (format stream ";;; blessed-subset.lisp - Self-Compilable Common Lisp Subset for Clysm~%")
      (format stream ";;;~%")
      (format stream ";;; Generated: ~A~%" (blessed-subset-generation-date subset))
      (format stream ";;; Version: ~A~%" (blessed-subset-version subset))
      (format stream ";;;~%")
      (format stream ";;; This file documents which CL features are verified to work in~%")
      (format stream ";;; Clysm's self-compilation process.~%~%")

      (format stream "(in-package :clysm-validation)~%~%")

      ;; Special forms
      (format stream "(defparameter *blessed-special-forms*~%")
      (format stream "  '~S~%"  (blessed-subset-special-forms subset))
      (format stream "  \"Special forms verified for self-compilation.\")~%~%")

      ;; Macros
      (format stream "(defparameter *blessed-macros*~%")
      (format stream "  '~S~%"  (blessed-subset-macros subset))
      (format stream "  \"Macros verified for self-compilation.\")~%~%")

      ;; Functions
      (format stream "(defparameter *blessed-functions*~%")
      (format stream "  '~S~%"  (blessed-subset-functions subset))
      (format stream "  \"Functions verified for self-compilation.\")~%~%")

      ;; Types
      (format stream "(defparameter *blessed-types*~%")
      (format stream "  '~S~%"  (blessed-subset-types subset))
      (format stream "  \"Type specifiers verified for self-compilation.\")~%~%")

      ;; Partial support notes
      (format stream "(defparameter *partial-support-notes*~%")
      (format stream "  '(")
      (dolist (note (blessed-subset-partial-notes subset))
        (format stream "~%    (~S . ~S)" (car note) (cdr note)))
      (format stream ")~%")
      (format stream "  \"Notes for features with partial support.\")~%"))
    subset))
