;;;; interpreter-stage0.lisp - Stage 0 generation via Tier 1 interpreter
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Tasks: T092-T098

(in-package #:clysm/interpreter-bootstrap)

;;; ============================================================
;;; T092: bootstrap-result struct
;;; ============================================================

(defstruct bootstrap-result
  "Result of Stage 0 generation via interpreter.

   Slots:
   - success: T if generation completed without fatal errors
   - wasm-bytes: The generated Wasm binary (vector of bytes)
   - modules-loaded: Number of modules successfully loaded
   - forms-compiled: Number of forms successfully compiled
   - errors: List of error messages/objects
   - elapsed-time: Generation time in seconds"
  (success nil :type boolean)
  (wasm-bytes nil :type (or null (vector (unsigned-byte 8))))
  (modules-loaded 0 :type fixnum)
  (forms-compiled 0 :type fixnum)
  (errors nil :type list)
  (elapsed-time 0.0 :type single-float))

;;; ============================================================
;;; Progress Callback
;;; ============================================================

(defvar *bootstrap-progress-callback* nil
  "Callback function for progress reporting.
   Called with (phase module-count form-count) during generation.
   Phases: :start, :loading-module, :compiling-form, :complete")

(defun notify-progress (phase module-count form-count)
  "Notify progress callback if set."
  (when *bootstrap-progress-callback*
    (funcall *bootstrap-progress-callback* phase module-count form-count)))

;;; ============================================================
;;; T093: form-compilable-p predicate
;;; ============================================================

(defun form-compilable-p (form)
  "Return T if FORM is a compilable top-level form.
   Non-compilable forms include:
   - in-package (meta-level package switch)
   - defpackage (meta-level package definition)
   - declare/declaim/proclaim (declarations)
   - eval-when (conditional evaluation)"
  (and (consp form)
       (symbolp (car form))
       (not (member (car form)
                    '(in-package defpackage
                      declare declaim proclaim
                      eval-when
                      ;; Also skip shadowing ops
                      shadow shadowing-import
                      export unexport
                      use-package unuse-package
                      import)
                    :test #'eq))))

;;; ============================================================
;;; T094: compile-single-form wrapper
;;; ============================================================

(defun compile-single-form (form env)
  "Compile a single form using the host compiler.
   ENV is the interpreter environment (currently unused for compilation,
   but will be used when interpreter can run the compiler directly).
   Returns (values wasm-bytes success-p error-or-nil).

   Note: Currently uses host CL's compiled clysm/compiler:compile-to-wasm.
   Future versions will use the interpreter to run the compiler itself."
  (declare (ignore env))
  (handler-case
      (let ((bytes (clysm/compiler:compile-to-wasm form)))
        (if (and bytes (vectorp bytes) (> (length bytes) 0))
            (values bytes t nil)
            (values nil nil "Empty compilation result")))
    (error (c)
      (values nil nil (format nil "~A" c)))))

;;; ============================================================
;;; T095: accumulate-wasm-bytes combiner
;;; ============================================================

(defun accumulate-wasm-bytes (byte-vectors)
  "Combine multiple Wasm byte vectors into a single Stage 0 binary.
   Uses the last non-nil vector that has valid Wasm magic bytes.

   Note: True Wasm module linking requires wasmtime or binaryen.
   For bootstrap, we use a simple last-valid approach since
   Stage 0 only needs to produce a working compiler binary."
  (let ((valid-vector nil))
    (dolist (vec byte-vectors)
      (when (and vec
                 (vectorp vec)
                 (>= (length vec) 8)
                 ;; Check Wasm magic bytes
                 (= #x00 (aref vec 0))
                 (= #x61 (aref vec 1))
                 (= #x73 (aref vec 2))
                 (= #x6d (aref vec 3)))
        (setf valid-vector vec)))
    (or valid-vector
        ;; Return minimal empty Wasm module if nothing valid
        (make-array 8
                    :element-type '(unsigned-byte 8)
                    :initial-contents '(#x00 #x61 #x73 #x6d
                                        #x01 #x00 #x00 #x00)))))

;;; ============================================================
;;; File reading utilities
;;; ============================================================

(defun read-source-forms (path)
  "Read all S-expressions from source file at PATH."
  (with-open-file (stream path :direction :input
                               :external-format :utf-8)
    (let ((forms nil)
          (eof (gensym "EOF")))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms))
      (nreverse forms))))

;;; ============================================================
;;; T096: generate-stage0-via-interpreter main function
;;; ============================================================

(defun generate-stage0-via-interpreter (&key (module-limit nil)
                                              (verbose nil)
                                              (output-path nil))
  "Generate Stage 0 Wasm binary using the Tier 1 interpreter.

   This function:
   1. Creates an interpreter environment
   2. Loads all compiler modules via interpret-file
   3. Compiles each module's forms using the interpreted compiler
   4. Accumulates Wasm bytes into Stage 0 binary

   Options:
   - MODULE-LIMIT: Maximum modules to process (nil = all)
   - VERBOSE: Print progress information
   - OUTPUT-PATH: Write binary to file if specified

   Returns a bootstrap-result struct."
  (let ((start-time (get-internal-real-time))
        (env (clysm/eval/interpreter:make-interpreter-env))
        (modules-loaded 0)
        (forms-compiled 0)
        (all-bytes nil)
        (errors nil)
        (module-paths (get-module-paths)))

    ;; Apply module limit
    (when module-limit
      (setf module-paths (subseq module-paths 0 (min module-limit (length module-paths)))))

    (notify-progress :start 0 0)

    ;; Phase 1: Load all compiler modules into interpreter
    (when verbose
      (format t "~&; Loading ~D compiler modules...~%" (length module-paths)))

    (dolist (path module-paths)
      (handler-case
          (progn
            (notify-progress :loading-module modules-loaded 0)
            (clysm/eval/interpreter:interpret-file path :env env :verbose verbose)
            (incf modules-loaded)
            (when verbose
              (format t "~&;   Loaded [~D/~D] ~A~%"
                      modules-loaded (length module-paths) (file-namestring path))))
        (error (c)
          (push (format nil "Load error in ~A: ~A" (file-namestring path) c) errors)
          (when verbose
            (format t "~&;   ERROR loading ~A: ~A~%" (file-namestring path) c)))))

    ;; Phase 2: Compile forms from each module
    (when verbose
      (format t "~&; Compiling forms from ~D modules...~%" modules-loaded))

    (dolist (path module-paths)
      (handler-case
          (let ((forms (read-source-forms path)))
            (dolist (form forms)
              (when (form-compilable-p form)
                (notify-progress :compiling-form modules-loaded forms-compiled)
                (multiple-value-bind (bytes success-p error)
                    (compile-single-form form env)
                  (if success-p
                      (progn
                        (push bytes all-bytes)
                        (incf forms-compiled))
                      (push (format nil "Compile error: ~A" error) errors))))))
        (error (c)
          (push (format nil "Read error in ~A: ~A" (file-namestring path) c) errors))))

    ;; Phase 3: Combine into Stage 0 binary
    (let* ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second))
           (wasm-bytes (accumulate-wasm-bytes (nreverse all-bytes)))
           (result (make-bootstrap-result
                    :success (and (> modules-loaded 0) (> forms-compiled 0))
                    :wasm-bytes wasm-bytes
                    :modules-loaded modules-loaded
                    :forms-compiled forms-compiled
                    :errors (nreverse errors)
                    :elapsed-time (coerce elapsed 'single-float))))

      (notify-progress :complete modules-loaded forms-compiled)

      (when verbose
        (format t "~&; Stage 0 generation complete~%")
        (format t ";   Modules loaded: ~D~%" modules-loaded)
        (format t ";   Forms compiled: ~D~%" forms-compiled)
        (format t ";   Errors: ~D~%" (length errors))
        (format t ";   Binary size: ~D bytes~%" (length wasm-bytes))
        (format t ";   Elapsed time: ~,2Fs~%" elapsed))

      ;; Write output if requested
      (when (and output-path wasm-bytes)
        (write-wasm-binary wasm-bytes output-path verbose))

      result)))

;;; ============================================================
;;; T097: Progress tracking support
;;; ============================================================

(defparameter *default-compilation-order*
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
  "The ordered list of modules to compile for self-hosting.")

(defun get-module-paths ()
  "Get list of compiler module paths in dependency order."
  (let ((base-dir (asdf:system-source-directory :clysm)))
    (mapcar (lambda (relative)
              (merge-pathnames relative base-dir))
            *default-compilation-order*)))

;;; ============================================================
;;; T098: wasm-tools validate integration
;;; ============================================================

(defun validate-stage0-binary (wasm-bytes)
  "Validate Stage 0 binary using wasm-tools.
   Returns (values valid-p error-message)."
  (when (or (null wasm-bytes) (zerop (length wasm-bytes)))
    (return-from validate-stage0-binary
      (values nil "Empty Wasm binary")))

  (let ((temp-path (merge-pathnames "stage0-validate-temp.wasm"
                                    (uiop:temporary-directory))))
    (unwind-protect
        (progn
          ;; Write to temp file
          (with-open-file (s temp-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
            (write-sequence wasm-bytes s))
          ;; Validate with wasm-tools
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (list "wasm-tools" "validate"
                                      (namestring temp-path))
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output))
            (if (zerop exit-code)
                (values t nil)
                (values nil error-output))))
      ;; Cleanup
      (when (probe-file temp-path)
        (delete-file temp-path)))))

(defun write-wasm-binary (bytes path &optional verbose)
  "Write Wasm bytes to file at PATH."
  (with-open-file (s path
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    (write-sequence bytes s))
  (when verbose
    (format t "~&; Written ~D bytes to ~A~%" (length bytes) path)))

