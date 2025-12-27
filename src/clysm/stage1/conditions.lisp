;;;; conditions.lisp - Condition types for Stage 1 compiler generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Defines error condition hierarchy for compilation failures

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Base Condition
;;; ==========================================================================

(define-condition stage1-error (error)
  ((context :initarg :context
            :initform nil
            :reader stage1-error-context
            :documentation "Additional context about the error"))
  (:documentation "Base condition for all Stage 1 compilation errors.")
  (:report (lambda (condition stream)
             (format stream "Stage 1 error~@[: ~A~]"
                     (stage1-error-context condition)))))

;;; ==========================================================================
;;; File System Errors
;;; ==========================================================================

(define-condition stage1-file-error (stage1-error)
  ((path :initarg :path
         :initform ""
         :reader stage1-file-error-path
         :documentation "The file path that caused the error"))
  (:documentation "Error related to file system operations.")
  (:report (lambda (condition stream)
             (format stream "Stage 1 file error~@[ for ~A~]~@[: ~A~]"
                     (stage1-file-error-path condition)
                     (stage1-error-context condition)))))

(define-condition stage1-file-not-found (stage1-file-error)
  ()
  (:documentation "Source file does not exist.")
  (:report (lambda (condition stream)
             (format stream "Source file not found: ~A"
                     (stage1-file-error-path condition)))))

(define-condition stage1-encoding-error (stage1-file-error)
  ((expected-encoding :initarg :expected-encoding
                      :initform :utf-8
                      :reader stage1-encoding-error-expected))
  (:documentation "File has invalid encoding.")
  (:report (lambda (condition stream)
             (format stream "Encoding error in ~A (expected ~A)"
                     (stage1-file-error-path condition)
                     (stage1-encoding-error-expected condition)))))

;;; ==========================================================================
;;; Parse Errors
;;; ==========================================================================

(define-condition stage1-parse-error (stage1-error)
  ((source :initarg :source
           :initform ""
           :reader stage1-parse-error-source
           :documentation "The source text that failed to parse")
   (position :initarg :position
             :initform 0
             :reader stage1-parse-error-position
             :documentation "Position in source where error occurred"))
  (:documentation "Error during S-expression parsing.")
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~D~@[: ~A~]"
                     (stage1-parse-error-position condition)
                     (stage1-error-context condition)))))

;;; ==========================================================================
;;; Compile Errors
;;; ==========================================================================

(define-condition stage1-compile-error (stage1-error)
  ((form :initarg :form
         :initform nil
         :reader stage1-compile-error-form
         :documentation "The form that failed to compile")
   (form-id :initarg :form-id
            :initform ""
            :reader stage1-compile-error-form-id
            :documentation "Unique identifier of the failing form")
   (operator :initarg :operator
             :initform nil
             :reader stage1-compile-error-operator
             :documentation "The operator that caused the failure"))
  (:documentation "Error during form compilation.")
  (:report (lambda (condition stream)
             (format stream "Compile error in form ~A (~A)~@[: ~A~]"
                     (stage1-compile-error-form-id condition)
                     (stage1-compile-error-operator condition)
                     (stage1-error-context condition)))))

(define-condition stage1-unsupported-feature (stage1-compile-error)
  ((feature :initarg :feature
            :initform nil
            :reader stage1-unsupported-feature-name
            :documentation "The unsupported CL feature"))
  (:documentation "Form uses a CL feature not in the blessed subset.")
  (:report (lambda (condition stream)
             (format stream "Unsupported feature ~A in form ~A"
                     (stage1-unsupported-feature-name condition)
                     (stage1-compile-error-form-id condition)))))

(define-condition stage1-internal-error (stage1-compile-error)
  ()
  (:documentation "Internal compiler error during compilation.")
  (:report (lambda (condition stream)
             (format stream "Internal error compiling ~A: ~A"
                     (stage1-compile-error-form-id condition)
                     (stage1-error-context condition)))))

;;; ==========================================================================
;;; Runtime Errors
;;; ==========================================================================

(define-condition stage1-runtime-error (stage1-error)
  ((wasm-error :initarg :wasm-error
               :initform nil
               :reader stage1-runtime-error-wasm
               :documentation "The underlying Wasm runtime error"))
  (:documentation "Error during Wasm runtime execution.")
  (:report (lambda (condition stream)
             (format stream "Wasm runtime error~@[: ~A~]~@[ (~A)~]"
                     (stage1-error-context condition)
                     (stage1-runtime-error-wasm condition)))))

(define-condition stage1-wasmtime-unavailable (stage1-runtime-error)
  ()
  (:documentation "wasmtime runtime is not available.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "wasmtime runtime not available (run: nix develop)"))))

(define-condition stage1-stage0-invalid (stage1-runtime-error)
  ((stage0-path :initarg :stage0-path
                :initform ""
                :reader stage1-stage0-invalid-path))
  (:documentation "Stage 0 binary is invalid or missing.")
  (:report (lambda (condition stream)
             (format stream "Stage 0 binary invalid: ~A"
                     (stage1-stage0-invalid-path condition)))))

;;; ==========================================================================
;;; Error Category Classification
;;; ==========================================================================

(defparameter *error-categories*
  '(:file-not-found :encoding-error :parse-error
    :unsupported-feature :internal-error :runtime-error)
  "List of error category keywords for classification.")

(defun classify-error (condition)
  "Classify a condition into an error category keyword."
  (typecase condition
    (stage1-file-not-found :file-not-found)
    (stage1-encoding-error :encoding-error)
    (stage1-parse-error :parse-error)
    (stage1-unsupported-feature :unsupported-feature)
    (stage1-internal-error :internal-error)
    (stage1-runtime-error :runtime-error)
    (stage1-compile-error :compile-error)
    (stage1-file-error :file-error)
    (stage1-error :unknown)
    (t :external)))

;;; ==========================================================================
;;; Fixed-Point Verification Errors (Feature 040)
;;; ==========================================================================

(define-condition fixpoint-error (stage1-error)
  ()
  (:documentation "Base condition for fixed-point verification errors.")
  (:report (lambda (condition stream)
             (format stream "Fixed-point verification error~@[: ~A~]"
                     (stage1-error-context condition)))))

(define-condition fixpoint-stage1-missing (fixpoint-error)
  ((stage1-path :initarg :stage1-path
                :initform "dist/clysm-stage1.wasm"
                :reader fixpoint-stage1-missing-path))
  (:documentation "Stage 1 binary not found.")
  (:report (lambda (condition stream)
             (format stream "Stage 1 binary not found: ~A~%Generate with: sbcl --load build/stage1-gen.lisp"
                     (fixpoint-stage1-missing-path condition)))))

(define-condition fixpoint-stage2-generation-error (fixpoint-error)
  ((module-path :initarg :module-path
                :initform ""
                :reader fixpoint-stage2-generation-error-module)
   (modules-compiled :initarg :modules-compiled
                     :initform 0
                     :reader fixpoint-stage2-generation-error-compiled)
   (modules-total :initarg :modules-total
                  :initform 0
                  :reader fixpoint-stage2-generation-error-total))
  (:documentation "Stage 2 generation failed.")
  (:report (lambda (condition stream)
             (format stream "Stage 2 generation failed at module ~A (~D/~D compiled)"
                     (fixpoint-stage2-generation-error-module condition)
                     (fixpoint-stage2-generation-error-compiled condition)
                     (fixpoint-stage2-generation-error-total condition)))))

(define-condition fixpoint-binary-invalid (fixpoint-error)
  ((binary-path :initarg :binary-path
                :initform ""
                :reader fixpoint-binary-invalid-path)
   (validation-error :initarg :validation-error
                     :initform ""
                     :reader fixpoint-binary-invalid-error))
  (:documentation "Binary failed wasm-tools validation.")
  (:report (lambda (condition stream)
             (format stream "Invalid Wasm binary: ~A~%~A"
                     (fixpoint-binary-invalid-path condition)
                     (fixpoint-binary-invalid-error condition)))))

(define-condition fixpoint-comparison-error (fixpoint-error)
  ((stage1-path :initarg :stage1-path
                :initform ""
                :reader fixpoint-comparison-error-stage1)
   (stage2-path :initarg :stage2-path
                :initform ""
                :reader fixpoint-comparison-error-stage2))
  (:documentation "Error during binary comparison.")
  (:report (lambda (condition stream)
             (format stream "Error comparing binaries:~%  Stage 1: ~A~%  Stage 2: ~A~@[~%  ~A~]"
                     (fixpoint-comparison-error-stage1 condition)
                     (fixpoint-comparison-error-stage2 condition)
                     (stage1-error-context condition)))))

(define-condition fixpoint-dependency-missing (fixpoint-error)
  ((dependency :initarg :dependency
               :initform ""
               :reader fixpoint-dependency-missing-name)
   (install-hint :initarg :install-hint
                 :initform ""
                 :reader fixpoint-dependency-missing-hint))
  (:documentation "Required dependency not available.")
  (:report (lambda (condition stream)
             (format stream "Missing dependency: ~A~@[~%Install with: ~A~]"
                     (fixpoint-dependency-missing-name condition)
                     (fixpoint-dependency-missing-hint condition)))))
