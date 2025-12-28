;;;; main.lisp - CLI entry point
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements T029-T033: CLI main entry point

(in-package #:clysm/cli)

;;; ============================================================
;;; T029: CLI main entry point
;;; ============================================================

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for Clysm CLI.
   ARGS is the list of command-line arguments.

   Returns the exit code that should be used.
   For use as ASDF program-op entry-point, use ENTRY-POINT instead."
  (let ((parsed (parse-args args)))

    ;; Handle help
    (when (parsed-args-help parsed)
      (show-help)
      (return-from main +exit-success+))

    ;; Handle version
    (when (parsed-args-version parsed)
      (show-version)
      (return-from main +exit-success+))

    ;; Validate arguments
    (let ((errors (validate-args parsed)))
      (when errors
        (dolist (err errors)
          (format *error-output* "error: ~A~%" err))
        (format *error-output* "~%usage: clysm compile <patterns...> -o <output> [options]~%")
        (format *error-output* "Try 'clysm --help' for more information.~%")
        (return-from main +exit-invalid-args+)))

    ;; Dispatch to command
    (case (parsed-args-command parsed)
      (:compile (run-compile-command parsed))
      (otherwise
       (format *error-output* "error: Unknown command~%")
       +exit-invalid-args+))))

;;; ============================================================
;;; T030: Compile command implementation
;;; ============================================================

(defun run-compile-command (args)
  "Execute the compile command with parsed ARGS.
   Returns exit code."
  (let* ((patterns (parsed-args-patterns args))
         (output (parsed-args-output args))
         (verbose (parsed-args-verbose args))
         (force (parsed-args-force args))
         (continue-on-error (parsed-args-continue args))
         (cache-dir (parsed-args-cache-dir args))
         (no-progress (parsed-args-no-progress args))
         (json-output (parsed-args-json args)))

    ;; Compile
    (handler-case
        (let ((session (clysm/workflow:compile-project
                        patterns output
                        :verbose verbose
                        :force force
                        :continue-on-error continue-on-error
                        :cache-dir cache-dir
                        :progress-callback (unless no-progress
                                             #'display-progress))))

          ;; Output results
          (if json-output
              (output-json-results session)
              (output-text-results session))

          ;; Determine exit code
          (determine-exit-code session))

      (error (e)
        (format *error-output* "error: ~A~%" e)
        +exit-failure+))))

;;; ============================================================
;;; T033: Exit code handling
;;; ============================================================

(defun determine-exit-code (session)
  "Determine the exit code based on SESSION results."
  (let ((modules (clysm/workflow:compilation-session-modules session))
        (results (clysm/workflow:compilation-session-results session))
        (errors (clysm/workflow:compilation-session-errors session)))

    (cond
      ;; No files matched
      ((null modules)
       +exit-no-files+)

      ;; No errors - complete success
      ((null errors)
       +exit-success+)

      ;; Some results successful
      ((some #'clysm/workflow:compilation-result-success-p results)
       +exit-partial+)

      ;; All failed
      (t
       +exit-failure+))))

;;; ============================================================
;;; Progress display
;;; ============================================================

(defun display-progress (progress-info)
  "Display progress information."
  (format t "~A~%"
          (clysm/workflow:progress-info-message progress-info))
  (force-output))

;;; ============================================================
;;; Output formatting
;;; ============================================================

(defun output-text-results (session)
  "Output compilation results in text format."
  (let* ((modules (clysm/workflow:compilation-session-modules session))
         (results (clysm/workflow:compilation-session-results session))
         (errors (clysm/workflow:compilation-session-errors session))
         (successful (count-if #'clysm/workflow:compilation-result-success-p results))
         (failed (- (length results) successful)))

    ;; Summary
    (format t "~%Compilation complete: ~D/~D modules, ~D error~:P~%"
            successful (length modules) failed)

    ;; Errors
    (when errors
      (format *error-output* "~%Errors:~%")
      (dolist (err errors)
        (format *error-output* "  ~A:~A: ~A~%"
                (clysm/workflow:compilation-error-path err)
                (clysm/workflow:compilation-error-line err)
                (clysm/workflow:compilation-error-message err))))

    ;; Output file
    (when (and (null errors)
               (uiop:file-exists-p (clysm/workflow:compilation-session-output-path session)))
      (let ((size (ignore-errors
                    (with-open-file (s (clysm/workflow:compilation-session-output-path session))
                      (file-length s)))))
        (format t "Output: ~A~@[ (~D bytes)~]~%"
                (clysm/workflow:compilation-session-output-path session)
                size)))))

(defun output-json-results (session)
  "Output compilation results in JSON format."
  (let* ((modules (clysm/workflow:compilation-session-modules session))
         (results (clysm/workflow:compilation-session-results session))
         (errors (clysm/workflow:compilation-session-errors session))
         (successful (count-if #'clysm/workflow:compilation-result-success-p results))
         (failed (- (length results) successful)))

    (format t "{~%")
    (format t "  \"status\": ~A,~%"
            (cond ((null errors) "\"success\"")
                  ((> successful 0) "\"partial\"")
                  (t "\"failure\"")))
    (format t "  \"modules\": {~%")
    (format t "    \"total\": ~D,~%" (length modules))
    (format t "    \"compiled\": ~D,~%" successful)
    (format t "    \"failed\": ~D~%" failed)
    (format t "  },~%")

    ;; Errors array
    (format t "  \"errors\": [")
    (loop for err in errors
          for first = t then nil
          do (unless first (format t ","))
             (format t "~%    {")
             (format t "\"path\": \"~A\", "
                     (clysm/workflow:compilation-error-path err))
             (format t "\"line\": ~D, "
                     (clysm/workflow:compilation-error-line err))
             (format t "\"message\": \"~A\"}"
                     (escape-json-string (clysm/workflow:compilation-error-message err))))
    (format t "~%  ],~%")

    ;; Output path
    (format t "  \"output\": ~A~%"
            (if (null errors)
                (format nil "\"~A\"" (clysm/workflow:compilation-session-output-path session))
                "null"))
    (format t "}~%")))

(defun escape-json-string (str)
  "Escape special characters in STR for JSON output."
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char char out))))))

;;; ============================================================
;;; Entry point for ASDF program-op
;;; ============================================================

(defun entry-point ()
  "Entry point for standalone executable built with ASDF program-op.
   Calls MAIN and exits with the appropriate exit code.

   Build with: (asdf:make :clysm/executable)"
  (let ((exit-code (main)))
    (uiop:quit exit-code)))
