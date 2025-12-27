;;; compiler-order.lisp - Dependency-Order Compilation for Clysm Validation
;;;
;;; Compiles compiler modules in dependency order and validates Wasm output.

(in-package :clysm-validation)

;;; T033: Compilation-Result struct

(defstruct compilation-result
  "Result of compiling a single module.

   Slots:
   - module: The module-info struct
   - success: T if compilation succeeded
   - wasm-bytes: The compiled Wasm binary (vector of bytes)
   - error-message: Error message if compilation failed
   - unsupported-feature: Specific unsupported CL feature if any
   - validation-passed: T if wasm-tools validate passed
   - validation-error: Validation error message if any"
  (module nil :type (or null module-info))
  (success nil :type boolean)
  (wasm-bytes nil :type (or null (vector (unsigned-byte 8))))
  (error-message nil :type (or null string))
  (unsupported-feature nil :type (or null symbol))
  (validation-passed nil :type boolean)
  (validation-error nil :type (or null string)))

;;; T036: get-dependency-order function
;;; Based on FR-006: leb128.lisp → sections.lisp → tokenizer.lisp → parser.lisp
;;;                   → ast.lisp → codegen/* → compiler.lisp

(defparameter *compilation-order*
  '(;; Backend (no dependencies)
    "src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"
    "src/clysm/backend/wat-print.lisp"

    ;; Reader (depends on backend via UTF-8)
    "src/clysm/reader/tokenizer.lisp"
    "src/clysm/reader/parser.lisp"
    "src/clysm/reader/package.lisp"
    "src/clysm/reader/reader.lisp"

    ;; Compiler core (depends on reader)
    "src/clysm/compiler/ast.lisp"
    "src/clysm/compiler/env.lisp"
    "src/clysm/compiler/analyzer/free-vars.lisp"
    "src/clysm/compiler/analyzer/tail-call.lisp"
    "src/clysm/compiler/analyzer/type-infer.lisp"
    "src/clysm/compiler/analyzer/io-usage.lisp"
    "src/clysm/compiler/transform/closure.lisp"
    "src/clysm/compiler/transform/macro.lisp"
    "src/clysm/compiler/codegen/wasm-ir.lisp"
    "src/clysm/compiler/codegen/gc-types.lisp"
    "src/clysm/compiler/codegen/type-section.lisp"
    "src/clysm/compiler/codegen/func-section.lisp"
    "src/clysm/compiler/compiler.lisp"

    ;; Runtime (depends on compiler)
    "src/clysm/runtime/objects.lisp"
    "src/clysm/runtime/special-vars.lisp"
    "src/clysm/runtime/multi-value.lisp"
    "src/clysm/runtime/printer.lisp"
    "src/clysm/runtime/condition-runtime.lisp"

    ;; CLOS (depends on runtime)
    "src/clysm/clos/mop.lisp"
    "src/clysm/clos/defclass.lisp"
    "src/clysm/clos/instance.lisp"
    "src/clysm/clos/slot-access.lisp"
    "src/clysm/clos/generic.lisp"
    "src/clysm/clos/defmethod.lisp"
    "src/clysm/clos/combination.lisp"
    "src/clysm/clos/dispatch.lisp"
    "src/clysm/clos/method-combination.lisp"

    ;; Conditions (depends on CLOS)
    "src/clysm/conditions/package.lisp"
    "src/clysm/conditions/types.lisp"
    "src/clysm/conditions/handlers.lisp"
    "src/clysm/conditions/restarts.lisp"
    "src/clysm/conditions/signaling.lisp"
    "src/clysm/conditions/standard.lisp")
  "The ordered list of modules to compile for self-hosting validation.
   Order matches FR-006 specification.")

(defun get-dependency-order (&optional (base-dir (asdf:system-source-directory :clysm)))
  "Return list of module-info structs in compilation order."
  (mapcar (lambda (relative-path)
            (let* ((full-path (merge-pathnames relative-path base-dir))
                   (dir-path (make-pathname :directory (pathname-directory full-path))))
              (make-module-info :path full-path
                                :directory dir-path
                                :dependencies nil  ; Could be computed
                                :symbols-used nil)))
          *compilation-order*))

;;; T034: compile-module function and helpers

;; Helper functions (defined first for forward reference)

(defun compilable-form-p (form)
  "Return T if FORM is a compilable top-level form.
   Non-compilable forms include in-package, defpackage, declare, etc."
  (and (consp form)
       (symbolp (car form))
       (member (car form)
               '(defun defmacro defvar defparameter defconstant
                 defstruct defclass defgeneric defmethod
                 deftype define-condition define-compiler-macro
                 define-symbol-macro define-setf-expander
                 define-modify-macro define-method-combination
                 ;; Also allow simple expressions
                 progn let let* flet labels block tagbody
                 lambda setf setq if when unless cond case
                 ;; Allow function calls that might have side effects
                 export import-from use-package)
               :test #'eq)))

(defun filter-compilable-forms (forms)
  "Filter forms to only include compilable ones.
   Skips in-package, defpackage, declare, etc."
  (remove-if-not #'compilable-form-p forms))

(defun compile-forms-to-wasm (forms path)
  "Compile a list of forms to Wasm bytes.
   Filters out non-compilable forms (in-package, defpackage, declare)
   and compiles only the compilable forms."
  (declare (ignore path))
  (let ((compilable-forms (filter-compilable-forms forms)))
    (when (null compilable-forms)
      (return-from compile-forms-to-wasm
        (make-array 0 :element-type '(unsigned-byte 8))))
    ;; Wrap forms in a progn for single expression compilation
    (let ((expr (if (= 1 (length compilable-forms))
                    (first compilable-forms)
                    `(progn ,@compilable-forms))))
      ;; Call Clysm's compiler
      (clysm/compiler:compile-to-wasm expr))))

(defun extract-unsupported-feature (error)
  "Try to extract unsupported feature from error message."
  (declare (ignore error))
  ;; TODO: Parse error messages for specific unsupported features
  nil)

;; Main compile-module function

(defun compile-module (module-info)
  "Compile a single module using Clysm compiler.
   Returns a compilation-result struct."
  (let ((path (module-info-path module-info)))
    (handler-case
        (let* ((forms (read-source-file path))
               ;; Use Clysm's compile function to compile all forms
               ;; This is a placeholder - actual implementation depends on Clysm's API
               (wasm-bytes (compile-forms-to-wasm forms path)))
          (make-compilation-result
           :module module-info
           :success t
           :wasm-bytes wasm-bytes
           :error-message nil
           :unsupported-feature nil
           :validation-passed nil
           :validation-error nil))
      (error (e)
        (make-compilation-result
         :module module-info
         :success nil
         :wasm-bytes nil
         :error-message (format nil "~A" e)
         :unsupported-feature (extract-unsupported-feature e)
         :validation-passed nil
         :validation-error nil)))))

;;; T035: validate-wasm function

(defun validate-wasm (wasm-bytes)
  "Validate Wasm binary using wasm-tools validate.
   Returns (values success-p error-message)."
  (when (or (null wasm-bytes) (zerop (length wasm-bytes)))
    (return-from validate-wasm (values nil "Empty Wasm binary")))

  (uiop:with-temporary-file (:pathname path :type "wasm" :direction :io
                             :element-type '(unsigned-byte 8))
    (write-sequence wasm-bytes path)
    (finish-output path)
    (file-position path 0)
    ;; Run wasm-tools validate
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasm-tools" "validate" (namestring path))
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore output))
      (if (zerop exit-code)
          (values t nil)
          (values nil error-output)))))

;;; T037: compile-in-order function

(defun compile-in-order (&key (halt-on-failure t))
  "Compile all modules in dependency order.
   If HALT-ON-FAILURE is T, stops at first failure.
   Returns a list of compilation-result structs."
  (let ((modules (get-dependency-order))
        (results nil))
    (dolist (module modules)
      (let ((result (compile-module module)))
        (push result results)
        (when (and halt-on-failure (not (compilation-result-success result)))
          (return-from compile-in-order (nreverse results)))))
    (nreverse results)))

;;; T039: validate-all-modules function

(defun validate-all-modules ()
  "Compile all modules in order and validate each Wasm output.
   Returns a list of compilation-result structs with validation info."
  (let ((results (compile-in-order :halt-on-failure nil)))
    (dolist (result results)
      (when (and (compilation-result-success result)
                 (compilation-result-wasm-bytes result))
        (multiple-value-bind (valid-p error)
            (validate-wasm (compilation-result-wasm-bytes result))
          (setf (compilation-result-validation-passed result) valid-p
                (compilation-result-validation-error result) error))))
    results))

;;; T038: Error logging with file:line and unsupported feature identification

(defun log-compilation-error (result &optional (stream *error-output*))
  "Log a compilation error with file:line and unsupported feature info."
  (let ((module (compilation-result-module result)))
    (format stream "ERROR: Failed to compile ~A~%"
            (namestring (module-info-path module)))
    (when (compilation-result-error-message result)
      (format stream "  Message: ~A~%"
              (compilation-result-error-message result)))
    (when (compilation-result-unsupported-feature result)
      (format stream "  Unsupported feature: ~A~%"
              (compilation-result-unsupported-feature result)))
    (when (compilation-result-validation-error result)
      (format stream "  Validation error: ~A~%"
              (compilation-result-validation-error result)))))

(defun generate-compilation-report (results &optional (stream *standard-output*))
  "Generate a compilation report for a list of results."
  (let ((total (length results))
        (compiled 0)
        (validated 0)
        (failed nil))
    (dolist (result results)
      (when (compilation-result-success result)
        (incf compiled)
        (when (compilation-result-validation-passed result)
          (incf validated)))
      (unless (and (compilation-result-success result)
                   (compilation-result-validation-passed result))
        (push result failed)))

    (format stream "~%=== Compilation Report ===~%~%")
    (format stream "Total modules: ~D~%" total)
    (format stream "Compiled successfully: ~D (~,1F%)~%"
            compiled (* 100.0 (/ compiled (max 1 total))))
    (format stream "Wasm validated: ~D (~,1F%)~%"
            validated (* 100.0 (/ validated (max 1 total))))

    (when failed
      (format stream "~%Failed modules:~%")
      (dolist (result (nreverse failed))
        (log-compilation-error result stream)))))
