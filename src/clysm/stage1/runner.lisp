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

(defvar *node-path* "node"
  "Path to Node.js executable.")

(defvar *host-shim-compile-path* nil
  "Path to host shim for compilation mode. Computed at runtime.")

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

(defun run-form (form &key (stage-path *stage0-path*))
  "Execute a single form using Stage compiler on wasmtime.
FORM can be a source-form struct or an S-expression.
STAGE-PATH is the path to the Stage compiler Wasm binary (Stage 0 or Stage 1).
Returns a compilation-result struct."
  (let* ((sexp (if (source-form-p form)
                   (source-form-sexp form)
                   form))
         (form-id (if (source-form-p form)
                      (source-form-id form)
                      "0:0"))
         (operator (form-operator sexp)))
    (handler-case
        (let* ((form-string (with-output-to-string (s) (prin1 sexp s)))
               (host-shim (compute-host-shim-path))
               (result (invoke-wasmtime-compile stage-path form-string host-shim)))
          (if (and result (getf result :success))
              (make-compilation-result
               :form (when (source-form-p form) form)
               :form-id form-id
               :success-p t
               :wasm-bytes (getf result :wasm-bytes))
              (make-compilation-result
               :form (when (source-form-p form) form)
               :form-id form-id
               :success-p nil
               :error-type (or (getf result :error-type) :compile-error)
               :error-message (or (getf result :error-message) "Compilation failed")
               :unsupported-feature operator)))
      (stage1-error (e)
        (make-compilation-result
         :form (when (source-form-p form) form)
         :form-id form-id
         :success-p nil
         :error-type (classify-error e)
         :error-message (format nil "~A" e)
         :unsupported-feature operator))
      (error (e)
        (make-compilation-result
         :form (when (source-form-p form) form)
         :form-id form-id
         :success-p nil
         :error-type :external-error
         :error-message (format nil "~A" e)
         :unsupported-feature operator)))))

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

(defun run-all-forms (forms &key (progress-callback nil) (stage-path *stage0-path*))
  "Execute all forms using Stage compiler and collect results.
FORMS is a list of source-form structs.
PROGRESS-CALLBACK is called with (index total form) for each form.
STAGE-PATH is the path to the Stage compiler Wasm binary.
Returns list of compilation-result structs."
  (let ((results nil)
        (total (length forms)))
    (loop for form in forms
          for index from 1
          do (when progress-callback
               (funcall progress-callback index total form))
             (handler-case
                 (push (run-form form :stage-path stage-path) results)
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

;;; ==========================================================================
;;; Host Shim Integration (Feature 040)
;;; ==========================================================================

(defun compute-host-shim-path ()
  "Compute path to host shim for compilation mode.
Returns cached path if already computed."
  (or *host-shim-compile-path*
      (setf *host-shim-compile-path*
            (or *host-shim-path*
                (let ((default-path (merge-pathnames
                                     "host-shim/stage1-host.js"
                                     (uiop:getcwd))))
                  (if (probe-file default-path)
                      default-path
                      (error 'fixpoint-dependency-missing
                             :dependency "stage1-host.js"
                             :install-hint "Ensure host-shim/stage1-host.js exists")))))))

(defun invoke-wasmtime-compile (stage-path form-string host-shim)
  "Invoke wasmtime to compile a form using the stage compiler.
STAGE-PATH is the path to the Stage Wasm binary.
FORM-STRING is the S-expression to compile as a string.
HOST-SHIM is the path to the Node.js host shim.
Returns plist with :success, :wasm-bytes, :error-type, :error-message."
  (unless (wasmtime-available-p)
    (error 'stage1-wasmtime-unavailable))
  (unless (and stage-path (probe-file stage-path))
    (error 'fixpoint-stage1-missing :stage1-path (or (namestring stage-path) "(not set)")))
  (let* ((timestamp (get-universal-time))
         (temp-input (format nil "/tmp/clysm-form-~A.lisp" timestamp))
         (temp-output (format nil "/tmp/clysm-form-~A.wasm" timestamp)))
    ;; Write form to temp input file
    (with-open-file (out temp-input :direction :output :if-exists :supersede)
      (write-string form-string out))
    (unwind-protect
        (handler-case
            (let* ((args (list *node-path*
                               (namestring host-shim)
                               "--mode" "compile"
                               "--stage1" (namestring stage-path)
                               "--input" temp-input
                               "--output" temp-output))
                   (result (uiop:run-program args
                                             :output :string
                                             :error-output :string
                                             :ignore-error-status t)))
              (if (and (probe-file temp-output)
                       (> (with-open-file (s temp-output) (file-length s)) 0))
                  (list :success t
                        :wasm-bytes (read-binary-file temp-output))
                  (list :success nil
                        :error-type :compile-error
                        :error-message (or result "No output generated"))))
          (error (e)
            (list :success nil
                  :error-type :runtime-error
                  :error-message (format nil "~A" e))))
      ;; Cleanup temp files
      (when (probe-file temp-input) (delete-file temp-input))
      (when (probe-file temp-output) (delete-file temp-output)))))

(defun read-binary-file (path)
  "Read a binary file and return contents as unsigned-byte vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))
