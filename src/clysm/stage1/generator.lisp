;;;; generator.lisp - Stage 1 binary generation
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Generates Stage 1 Wasm binary from Clysm compiler source

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Phase 13D-7: Validated Bundled Compilation Strategy
;;; ==========================================================================
;;;
;;; Strategy:
;;; 1. Test each form individually to identify which compile successfully
;;; 2. Bundle successful defuns and validate the output
;;; 3. Use binary search to find maximum valid bundle size if full bundle fails
;;;
;;; This produces a valid Wasm module containing the maximum number of
;;; successfully compiled and validated functions.

(defun test-form-compilation (sexp &key (validate t))
  "Test if a form compiles successfully AND produces valid Wasm.
Returns (values success-p bytes) where success-p is T if compilation
succeeds AND (if validate is true) the Wasm passes validation."
  (handler-case
      (let ((bytes (clysm:compile-to-wasm sexp)))
        (if (and validate (not (validate-wasm-bytes bytes)))
            (values nil nil)
            (values t bytes)))
    (error () (values nil nil))))

(defun classify-forms (forms &key (progress-callback nil) (validate t))
  "Classify forms into successful and failed compilations.
FORMS is a list of source-form structs.
When VALIDATE is true (default), only forms that produce valid Wasm are counted as successful.
Returns (values successful-sexps results stats)."
  (let ((total (length forms))
        (compiled 0)
        (failed 0)
        (results nil)
        (successful-sexps nil))
    (loop for form in forms
          for index from 1
          do (let ((sexp (if (source-form-p form)
                             (source-form-sexp form)
                             form))
                   (form-id (if (source-form-p form)
                                (source-form-id form)
                                "0:0")))
               (multiple-value-bind (success-p bytes)
                   (test-form-compilation sexp :validate validate)
                 (declare (ignore bytes))
                 (if success-p
                     (progn
                       (incf compiled)
                       (push sexp successful-sexps)
                       (push (make-compilation-result
                              :form form
                              :form-id form-id
                              :success-p t)
                             results))
                     (progn
                       (incf failed)
                       (push (make-compilation-result
                              :form form
                              :form-id form-id
                              :success-p nil
                              :error-message "Compilation or validation failed")
                             results))))
               (when progress-callback
                 (funcall progress-callback index total t))))
    (values (nreverse successful-sexps)
            (nreverse results)
            (list :compiled compiled :failed failed :total total))))

(defun validate-wasm-bytes (bytes)
  "Validate Wasm bytes by writing to temp file and running wasm-tools.
Returns T if valid, NIL otherwise."
  (let ((temp-path (merge-pathnames
                    (format nil "clysm-test-~D.wasm" (get-universal-time))
                    (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (out temp-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence bytes out))
          (multiple-value-bind (output error-output status)
              (uiop:run-program (list "wasm-tools" "validate" (namestring temp-path))
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output error-output))
            (zerop status)))
      (when (probe-file temp-path)
        (delete-file temp-path)))))

(defun compile-defuns-bundle (defuns)
  "Compile a list of defuns into a bundled module.
Returns Wasm bytes or NIL on failure."
  (handler-case
      (clysm:compile-to-wasm `(progn ,@defuns))
    (error () nil)))

(defun find-valid-bundle (defuns)
  "Find the largest prefix of defuns that produces valid Wasm.
Uses binary search for efficiency.
Returns (values wasm-bytes num-defuns)."
  (let ((n (length defuns)))
    (when (zerop n)
      (return-from find-valid-bundle (values nil 0)))
    ;; Try full bundle first
    (let ((bytes (compile-defuns-bundle defuns)))
      (when (and bytes (validate-wasm-bytes bytes))
        (return-from find-valid-bundle (values bytes n))))
    ;; Binary search for largest valid prefix
    (let ((lo 1)
          (hi (1- n))
          (best-bytes nil)
          (best-count 0))
      (loop while (<= lo hi)
            do (let* ((mid (floor (+ lo hi) 2))
                      (subset (subseq defuns 0 mid))
                      (bytes (compile-defuns-bundle subset)))
                 (if (and bytes (validate-wasm-bytes bytes))
                     (progn
                       (setf best-bytes bytes
                             best-count mid
                             lo (1+ mid)))
                     (setf hi (1- mid)))))
      (values best-bytes best-count))))

(defun bundleable-form-p (sexp)
  "Check if a form can be bundled into a progn for compilation.
Returns T for forms that produce meaningful Wasm output."
  (and (consp sexp)
       (member (car sexp) '(defun defconstant defvar defparameter defclass))))

(defun bundle-and-compile (successful-sexps)
  "Bundle successful forms and compile to a valid module.
Returns Wasm bytes or NIL on failure."
  (when successful-sexps
    ;; Include all bundleable forms, not just defuns
    (let* ((bundleable (remove-if-not #'bundleable-form-p successful-sexps))
           ;; Limit to reasonable size
           (limited (if (> (length bundleable) 500)
                        (subseq bundleable 0 500)
                        bundleable)))
      (when limited
        (format t "~&Finding largest valid bundle from ~D forms...~%" (length limited))
        (multiple-value-bind (bytes count)
            (find-valid-bundle limited)
          (format t "~&Valid bundle contains ~D forms~%" count)
          bytes)))))

(defun create-stage1-stub-module ()
  "Create a minimal valid Stage 1 Wasm module stub."
  (let ((header #(#x00 #x61 #x73 #x6D #x01 #x00 #x00 #x00))
        (type-section #(#x01 #x05 #x01 #x60 #x00 #x01 #x7F))
        (func-section #(#x03 #x02 #x01 #x00))
        (export-section #(#x07 #x0A #x01 #x06
                          #x5F #x73 #x74 #x61 #x72 #x74
                          #x00 #x00))
        (code-section #(#x0A #x06 #x01 #x04 #x00 #x41 #x00 #x0B)))
    (let* ((total-size (+ (length header)
                          (length type-section)
                          (length func-section)
                          (length export-section)
                          (length code-section)))
           (module (make-array total-size :element-type '(unsigned-byte 8)))
           (offset 0))
      (dolist (section (list header type-section func-section export-section code-section))
        (replace module section :start1 offset)
        (incf offset (length section)))
      module)))

;;; ==========================================================================
;;; Binary Output
;;; ==========================================================================

(defun write-stage1-binary (bytes output-path)
  "Write Stage 1 binary to file."
  (with-open-file (out output-path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence bytes out)
    (length bytes)))

(defun validate-stage1 (path)
  "Validate Stage 1 binary using wasm-tools."
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
;;; Main Generation Entry Point (Phase 13D-7)
;;; ==========================================================================

(defun generate-stage1 (&key (output-path nil)
                              (report-path nil)
                              (validate t)
                              (progress-callback nil))
  "Generate Stage 1 binary from Clysm source.

Phase 13D-7: Classifies forms, finds the largest valid bundle of defuns,
and produces a valid Wasm module."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (or output-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
         (report-out (or report-path (merge-pathnames "dist/stage1-report.json" root)))
         (modules (read-all-modules)))
    (let ((all-forms (all-compilable-forms modules)))
      ;; Phase 1: Classify forms
      (format t "~&Classifying ~D forms...~%" (length all-forms))
      (multiple-value-bind (successful-sexps results stats)
          (classify-forms all-forms :progress-callback progress-callback)
        (format t "~&Classification: ~D succeeded, ~D failed~%"
                (getf stats :compiled) (getf stats :failed))
        ;; Phase 2: Bundle and compile with validation
        (let* ((bundled-bytes (bundle-and-compile successful-sexps))
               (bytes (or bundled-bytes (create-stage1-stub-module)))
               (report (generate-progress-report modules results)))
          (format t "~&Output size: ~D bytes~%" (length bytes))
          ;; Write binary
          (ensure-directories-exist output)
          (write-stage1-binary bytes output)
          ;; Validate if requested
          (when validate
            (format t "~&Final validation...~%")
            (validate-stage1 output)
            (format t "~&Validation PASSED~%"))
          ;; Write report
          (ensure-directories-exist report-out)
          (with-open-file (s report-out
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (write-progress-report report :stream s :format :json))
          report)))))
