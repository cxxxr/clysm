;;;; entry.lisp - Entry points for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements compile_form and compile_all exported functions
;;;;
;;;; These are the main entry points that wasmtime calls.

(in-package #:clysm/stage0)

;;; ============================================================
;;; compile_form - Compile a Single Expression (US1)
;;; ============================================================

(defun compile-form (source-string)
  "Compile a single Lisp expression to Wasm bytes.
   This is the main entry point for single-form compilation.

   Args:
     source-string: String containing Lisp expression, e.g. \"(+ 1 2)\"

   Returns:
     stage0-result struct with:
       - success-p: T if compilation succeeded
       - wasm-bytes: Vector of Wasm bytes (if success)
       - error-message: Error description (if failure)
       - form-count: Number of forms compiled (usually 1)

   Example:
     (compile-form \"(+ 1 2)\")
     => #S(stage0-result :success-p T :wasm-bytes #(...) :form-count 1)"
  (clear-progress-log)
  (handler-case
      (let* ((forms (read-source-string source-string))
             (compiled (compile-forms forms))
             (exports (generate-exports))
             (wasm-bytes (emit-stage0-module compiled exports)))
        (if (validate-wasm-bytes wasm-bytes)
            (make-stage0-result
             :success-p t
             :wasm-bytes wasm-bytes
             :form-count (length forms))
            (make-stage0-result
             :success-p nil
             :error-message "Generated Wasm failed validation"
             :form-count (length forms))))
    (error (e)
      (make-stage0-result
       :success-p nil
       :error-message (format nil "~A" e)
       :form-count 0))))

;;; ============================================================
;;; compile_all - Compile All Modules (US2)
;;; ============================================================

(defun compile-all (&key (output-path *output-path*))
  "Compile all Clysm source modules to produce Stage 1 binary.
   This is the main entry point for full compilation.

   Args:
     output-path: Path for output binary (default: dist/clysm-stage1.wasm)

   Returns:
     stage0-result struct with:
       - success-p: T if compilation succeeded
       - wasm-bytes: Vector of Wasm bytes (if success)
       - error-message: Error description (if failure)
       - form-count: Total number of forms compiled

   Example:
     (compile-all)
     => #S(stage0-result :success-p T :wasm-bytes #(...) :form-count 900)"
  (clear-progress-log)
  (handler-case
      (let* (;; Load all modules
             (modules (load-all-modules))
             ;; Compile all forms with graceful degradation
             (compiled (mapcan #'compile-module-with-degradation modules))
             ;; Generate exports
             (exports (generate-exports))
             ;; Emit final binary
             (wasm-bytes (emit-stage0-module compiled exports)))
        ;; Validate and write output
        (if (validate-wasm-bytes wasm-bytes)
            (progn
              (write-stage1-binary wasm-bytes :path output-path)
              (make-stage0-result
               :success-p t
               :wasm-bytes wasm-bytes
               :form-count (length compiled)))
            (make-stage0-result
             :success-p nil
             :error-message "Generated Wasm failed validation"
             :form-count (length compiled))))
    (error (e)
      (make-stage0-result
       :success-p nil
       :error-message (format nil "~A" e)
       :form-count 0))))

;;; ============================================================
;;; CLI Entry Point
;;; ============================================================

(defun main (&optional args)
  "Main entry point for CLI invocation.
   Called when wasmtime runs the Stage 0 binary.

   Args:
     args: Command-line arguments

   Usage:
     wasmtime run dist/clysm-stage0.wasm --invoke compile_form '(+ 1 2)'
     wasmtime run dist/clysm-stage0.wasm --invoke compile_all"
  (cond
    ;; No args - compile all
    ((null args)
     (let ((result (compile-all)))
       (if (stage0-result-success-p result)
           (format t "~&Success: Compiled ~D forms~%"
                   (stage0-result-form-count result))
           (format t "~&Error: ~A~%"
                   (stage0-result-error-message result)))
       (if (stage0-result-success-p result) 0 1)))

    ;; Single expression
    ((stringp (first args))
     (let ((result (compile-form (first args))))
       (if (stage0-result-success-p result)
           (format t "~&Success: Compiled ~D form(s)~%"
                   (stage0-result-form-count result))
           (format t "~&Error: ~A~%"
                   (stage0-result-error-message result)))
       (if (stage0-result-success-p result) 0 1)))

    (t
     (format t "~&Usage: compile_form <expression> | compile_all~%")
     1)))
