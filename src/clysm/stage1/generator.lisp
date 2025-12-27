;;;; generator.lisp - Stage 1 binary generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Generates Stage 1 binary from compiled Clysm source

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Compilation State
;;; ==========================================================================

(defvar *wasm-bytes* nil
  "Accumulated Wasm bytes during compilation.")

(defvar *compiled-forms-count* 0
  "Count of successfully compiled forms.")

(defvar *failed-forms-count* 0
  "Count of failed form compilations.")

;;; ==========================================================================
;;; Form Compilation
;;; ==========================================================================

(defun compile-form-to-wasm (form)
  "Compile a single form to Wasm bytes.
Returns (values bytes success-p error-msg)."
  (handler-case
      (let* ((sexp (if (source-form-p form)
                       (source-form-sexp form)
                       form))
             ;; Use existing clysm compiler
             (wasm (clysm:compile-to-wasm sexp)))
        (values wasm t nil))
    (error (e)
      (values nil nil (format nil "~A" e)))))

(defun compile-all-forms (forms &key (progress-callback nil))
  "Compile all forms and collect Wasm output.
FORMS is a list of source-form structs.
PROGRESS-CALLBACK is called with (index total success-p) for each form.
Returns (values results stats)."
  (let ((results nil)
        (compiled 0)
        (failed 0)
        (total (length forms)))
    (loop for form in forms
          for index from 1
          do (multiple-value-bind (wasm success-p error-msg)
                 (compile-form-to-wasm form)
               (if success-p
                   (progn
                     (incf compiled)
                     (push (make-compilation-result
                            :form form
                            :form-id (if (source-form-p form)
                                         (source-form-id form)
                                         "0:0")
                            :success-p t
                            :wasm-bytes wasm)
                           results))
                   (progn
                     (incf failed)
                     (push (make-compilation-result
                            :form form
                            :form-id (if (source-form-p form)
                                         (source-form-id form)
                                         "0:0")
                            :success-p nil
                            :error-message error-msg)
                           results)))
               (when progress-callback
                 (funcall progress-callback index total success-p))))
    (values (nreverse results)
            (list :compiled compiled :failed failed :total total))))

;;; ==========================================================================
;;; Binary Accumulation
;;; ==========================================================================

(defun accumulate-wasm-bytes (results)
  "Accumulate Wasm bytes from compilation results.
Returns combined byte vector or NIL if no bytes."
  (let ((all-bytes nil))
    (dolist (result results)
      (when (and (compilation-result-success-p result)
                 (compilation-result-wasm-bytes result))
        (push (compilation-result-wasm-bytes result) all-bytes)))
    (when all-bytes
      (let* ((total-length (reduce #'+ all-bytes :key #'length))
             (combined (make-array total-length :element-type '(unsigned-byte 8)))
             (offset 0))
        (dolist (bytes (nreverse all-bytes))
          (replace combined bytes :start1 offset)
          (incf offset (length bytes)))
        combined))))

;;; ==========================================================================
;;; Binary Output
;;; ==========================================================================

(defun write-stage1-binary (bytes output-path)
  "Write Stage 1 binary to file.
Returns the number of bytes written."
  (with-open-file (out output-path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence bytes out)
    (length bytes)))

(defun validate-stage1 (path)
  "Validate Stage 1 binary using wasm-tools.
Returns T if valid, signals error if invalid."
  (multiple-value-bind (output error-output status)
      (uiop:run-program (list "wasm-tools" "validate" (namestring path))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (if (zerop status)
        t
        (error 'stage1-stage0-invalid
               :stage0-path (namestring path)
               :context error-output))))

;;; ==========================================================================
;;; Main Generation Entry Point
;;; ==========================================================================

(defun generate-stage1 (&key (output-path nil)
                              (report-path nil)
                              (validate t)
                              (progress-callback nil))
  "Generate Stage 1 binary from Clysm source.
OUTPUT-PATH: Path for Stage 1 binary (default: dist/clysm-stage1.wasm)
REPORT-PATH: Path for progress report (default: dist/stage1-report.json)
VALIDATE: If T, validate output with wasm-tools
PROGRESS-CALLBACK: Called with (module-name form-index total success-p)
Returns progress-report struct."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (or output-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
         (report-out (or report-path (merge-pathnames "dist/stage1-report.json" root)))
         (modules (read-all-modules))
         (all-results nil)
         (all-stats nil))
    ;; Compile each module
    (dolist (module modules)
      (let ((forms (source-module-forms module)))
        (multiple-value-bind (results stats)
            (compile-all-forms (remove-if-not #'source-form-compilable-p forms)
                               :progress-callback progress-callback)
          (push results all-results)
          (push stats all-stats))))
    ;; Flatten results
    (let* ((flat-results (apply #'append (nreverse all-results)))
           (bytes (accumulate-wasm-bytes flat-results))
           (report (generate-progress-report modules flat-results)))
      ;; Write binary
      (when bytes
        (ensure-directories-exist output)
        (write-stage1-binary bytes output)
        (when validate
          (validate-stage1 output)))
      ;; Write report
      (ensure-directories-exist report-out)
      (with-open-file (s report-out
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (write-progress-report report :stream s :format :json))
      report)))

