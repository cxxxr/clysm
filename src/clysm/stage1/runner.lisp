;;;; runner.lisp - Stage 0 Wasm runtime execution wrapper
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Provides interface to execute Stage 0 binary on wasmtime

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Runtime Configuration
;;; ==========================================================================

(defvar *stage0-path* nil
  "Path to Stage 0 Wasm binary. Set during initialization.")

(defvar *wasmtime-path* "wasmtime"
  "Path to wasmtime executable.")

(defvar *host-shim-path* nil
  "Path to Node.js host shim for FFI.")

;;; ==========================================================================
;;; Runtime Availability Checks
;;; ==========================================================================

(defun wasmtime-available-p ()
  "Check if wasmtime runtime is available in PATH.
Returns T if available, NIL otherwise."
  (handler-case
      (let ((result (uiop:run-program
                     (list *wasmtime-path* "--version")
                     :output :string
                     :error-output nil
                     :ignore-error-status t)))
        (if (and result (search "wasmtime" result))
            t
            nil))
    (error () nil)))

(defun stage0-available-p ()
  "Check if Stage 0 binary exists and is valid."
  (and *stage0-path*
       (probe-file *stage0-path*)))

(defun validate-stage0 ()
  "Validate Stage 0 binary using wasm-tools.
Returns T if valid, signals stage1-stage0-invalid if not."
  (unless (stage0-available-p)
    (error 'stage1-stage0-invalid
           :stage0-path (or *stage0-path* "(not set)")))
  (handler-case
      (let ((result (uiop:run-program
                     (list "wasm-tools" "validate" (namestring *stage0-path*))
                     :output :string
                     :error-output :string
                     :ignore-error-status t)))
        (declare (ignore result))
        t)
    (error (e)
      (error 'stage1-stage0-invalid
             :stage0-path (namestring *stage0-path*)
             :context (format nil "~A" e)))))

;;; ==========================================================================
;;; Stage 0 Loading and Execution
;;; ==========================================================================

(defun load-stage0 (&key (path *stage0-path*) (validate t))
  "Load Stage 0 Wasm binary for execution.
If VALIDATE is T, validates the binary before loading.
Returns Stage 0 info hash-table on success."
  (unless (wasmtime-available-p)
    (error 'stage1-wasmtime-unavailable))
  (setf *stage0-path* path)
  (when validate
    (validate-stage0))
  ;; Return metadata about Stage 0
  (let ((info (make-hash-table :test 'equal)))
    (setf (gethash "path" info) (namestring path))
    (setf (gethash "size" info) (with-open-file (s path) (file-length s)))
    (setf (gethash "valid" info) t)
    info))

(defun run-form (form &key (stage0-path *stage0-path*))
  "Execute a single form using Stage 0 compiler on wasmtime.
FORM can be a source-form struct or an S-expression.
Returns a compilation-result struct."
  (declare (ignore stage0-path))
  (let* ((sexp (if (source-form-p form)
                   (source-form-sexp form)
                   form))
         (form-id (if (source-form-p form)
                      (source-form-id form)
                      "0:0"))
         (operator (form-operator sexp)))
    ;; TODO: Actually invoke wasmtime with host shim
    ;; For now, return a placeholder result
    (make-compilation-result
     :form (when (source-form-p form) form)
     :form-id form-id
     :success-p nil
     :error-type :not-implemented
     :error-message "Stage 0 execution not yet implemented"
     :unsupported-feature operator)))

(defun run-expression (expr-string &key (stage0-path *stage0-path*))
  "Execute a Lisp expression string using Stage 0.
Returns the evaluation result as a string."
  (declare (ignore stage0-path))
  ;; TODO: Implement via host shim invocation
  (format nil "Expression execution not implemented: ~A" expr-string))

;;; ==========================================================================
;;; Result Capture and Error Conversion
;;; ==========================================================================

(defun capture-result (process-output)
  "Parse Stage 0 output and extract the evaluation result.
Returns the result value or signals appropriate condition."
  ;; TODO: Parse wasmtime/host-shim output format
  (declare (ignore process-output))
  nil)

(defun error-from-wasm (wasm-error-string)
  "Convert a Wasm runtime error string to a stage1 condition.
Returns appropriate condition subtype based on error content."
  (cond
    ((search "unreachable" wasm-error-string)
     (make-condition 'stage1-runtime-error
                     :wasm-error wasm-error-string
                     :context "Wasm trap: unreachable"))
    ((search "out of bounds" wasm-error-string)
     (make-condition 'stage1-runtime-error
                     :wasm-error wasm-error-string
                     :context "Wasm trap: out of bounds access"))
    ((search "stack overflow" wasm-error-string)
     (make-condition 'stage1-runtime-error
                     :wasm-error wasm-error-string
                     :context "Wasm trap: stack overflow"))
    (t
     (make-condition 'stage1-runtime-error
                     :wasm-error wasm-error-string))))

;;; ==========================================================================
;;; Batch Execution
;;; ==========================================================================

(defun run-all-forms (forms &key (progress-callback nil))
  "Execute all forms using Stage 0 and collect results.
FORMS is a list of source-form structs.
PROGRESS-CALLBACK is called with (index total form) for each form.
Returns list of compilation-result structs."
  (let ((results nil)
        (total (length forms)))
    (loop for form in forms
          for index from 1
          do (when progress-callback
               (funcall progress-callback index total form))
             (handler-case
                 (push (run-form form) results)
               (stage1-error (e)
                 (push (make-compilation-result
                        :form form
                        :form-id (source-form-id form)
                        :success-p nil
                        :error-type (classify-error e)
                        :error-message (format nil "~A" e))
                       results))
               (error (e)
                 (push (make-compilation-result
                        :form form
                        :form-id (source-form-id form)
                        :success-p nil
                        :error-type :external-error
                        :error-message (format nil "~A" e))
                       results))))
    (nreverse results)))
