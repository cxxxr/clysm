;;;; bootstrap-interp.lisp - CLI entry point for interpreter-based Stage 0 generation
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Task: T099

;;; Usage: sbcl --load build/bootstrap-interp.lisp
;;; Or with options: sbcl --load build/bootstrap-interp.lisp --eval '(run :verbose t)'

(require :asdf)

;; Load the Clysm system
(format t "~&; Loading Clysm system...~%")
(asdf:load-system :clysm)
(asdf:load-system :clysm/validation)

(defpackage #:clysm/cli/bootstrap-interp
  (:use #:cl)
  (:export #:run #:main))

(in-package #:clysm/cli/bootstrap-interp)

(defvar *default-output-path*
  (merge-pathnames "dist/clysm-stage0-interp.wasm"
                   (asdf:system-source-directory :clysm)))

(defun print-banner ()
  "Print startup banner."
  (format t "~&~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║       Clysm Interpreter-Based Stage 0 Generator              ║~%")
  (format t "║       Feature 044: Interpreter Bootstrap Strategy            ║~%")
  (format t "╚══════════════════════════════════════════════════════════════╝~%")
  (format t "~%"))

(defun print-usage ()
  "Print usage information."
  (format t "~&Usage: sbcl --load build/bootstrap-interp.lisp~%")
  (format t "~%")
  (format t "Options (via --eval):~%")
  (format t "  (run :verbose t)      - Enable verbose output~%")
  (format t "  (run :output PATH)    - Specify output path~%")
  (format t "  (run :module-limit N) - Limit modules processed~%")
  (format t "  (run :validate t)     - Validate output with wasm-tools~%")
  (format t "~%"))

(defun run (&key (verbose nil)
                 (output nil)
                 (module-limit nil)
                 (validate t)
                 (help nil))
  "Run interpreter-based Stage 0 generation.

   Options:
   - VERBOSE: Print detailed progress
   - OUTPUT: Output file path (default: dist/clysm-stage0-interp.wasm)
   - MODULE-LIMIT: Maximum modules to process (default: all)
   - VALIDATE: Validate output with wasm-tools (default: t)
   - HELP: Print usage and exit"
  (when help
    (print-usage)
    (return-from run nil))

  (print-banner)

  (let ((output-path (or output *default-output-path*)))
    ;; Ensure output directory exists
    (ensure-directories-exist output-path)

    (format t "; Starting interpreter-based Stage 0 generation...~%")
    (format t "; Output: ~A~%" output-path)
    (when module-limit
      (format t "; Module limit: ~D~%" module-limit))
    (format t "~%")

    ;; Run generation
    (let ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                   :verbose verbose
                   :output-path output-path
                   :module-limit module-limit)))

      ;; Print results
      (format t "~%")
      (format t "═══════════════════════════════════════════════════════════════~%")
      (format t " Generation Results~%")
      (format t "═══════════════════════════════════════════════════════════════~%")
      (format t "  Status:          ~A~%"
              (if (clysm/interpreter-bootstrap:bootstrap-result-success result)
                  "SUCCESS" "FAILED"))
      (format t "  Modules loaded:  ~D~%"
              (clysm/interpreter-bootstrap:bootstrap-result-modules-loaded result))
      (format t "  Forms compiled:  ~D~%"
              (clysm/interpreter-bootstrap:bootstrap-result-forms-compiled result))
      (format t "  Elapsed time:    ~,2F seconds~%"
              (clysm/interpreter-bootstrap:bootstrap-result-elapsed-time result))

      (let ((bytes (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result)))
        (when bytes
          (format t "  Binary size:     ~D bytes~%" (length bytes))))

      ;; Print errors if any
      (let ((errors (clysm/interpreter-bootstrap:bootstrap-result-errors result)))
        (when errors
          (format t "~%  Errors (~D):~%" (length errors))
          (dolist (err (subseq errors 0 (min 10 (length errors))))
            (format t "    - ~A~%" err))
          (when (> (length errors) 10)
            (format t "    ... and ~D more~%" (- (length errors) 10)))))

      ;; Validate if requested
      (when (and validate
                 (clysm/interpreter-bootstrap:bootstrap-result-success result)
                 (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result))
        (format t "~%  Validating with wasm-tools...~%")
        (multiple-value-bind (valid-p error)
            (clysm/interpreter-bootstrap:validate-stage0-binary
             (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result))
          (if valid-p
              (format t "  Validation:      PASSED~%")
              (format t "  Validation:      FAILED - ~A~%" error))))

      (format t "═══════════════════════════════════════════════════════════════~%")

      result)))

(defun main ()
  "Main entry point for command-line invocation."
  (run :verbose t :validate t))

;; Auto-run if loaded directly
(format t "~&; Interpreter bootstrap module loaded.~%")
(format t "; Call (clysm/cli/bootstrap-interp:run) to generate Stage 0~%")
(format t "; Or (clysm/cli/bootstrap-interp:run :help t) for options~%")

