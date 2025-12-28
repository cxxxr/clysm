;;;; package.lisp - Package definition for CLI module
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Provides command-line interface for Clysm compiler

(defpackage #:clysm/cli
  (:use #:cl #:clysm/workflow)
  (:documentation "Command-line interface for Clysm compiler.
Provides:
- Argument parsing for ./clysm compile command
- Exit code handling
- Progress display
- Error output formatting")
  ;; Argument parsing (from args.lisp)
  (:export #:parsed-args
           #:make-parsed-args
           #:parsed-args-command
           #:parsed-args-patterns
           #:parsed-args-output
           #:parsed-args-verbose
           #:parsed-args-force
           #:parsed-args-continue
           #:parsed-args-cache-dir
           #:parsed-args-help
           #:parsed-args-version
           #:parsed-args-errors

           #:parse-args
           #:validate-args
           #:show-help
           #:show-version)
  ;; Main entry point (from main.lisp)
  (:export #:main
           #:entry-point
           #:run-compile-command))
