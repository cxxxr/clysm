;;;; fixpoint-check.lisp - CLI entry point for interpreter bootstrap fixpoint verification
;;;;
;;;; Part of Feature 044: Interpreter Bootstrap Strategy
;;;; Phase 6: T107 - CLI entry point
;;;;
;;;; Usage:
;;;;   sbcl --load build/fixpoint-check.lisp
;;;;   sbcl --load build/fixpoint-check.lisp -- --json
;;;;   sbcl --load build/fixpoint-check.lisp -- --skip-generate
;;;;   sbcl --load build/fixpoint-check.lisp -- --verbose

;;; Load the system
(require :asdf)
(asdf:load-system :clysm :verbose nil)

;;; Parse command-line arguments
(defparameter *json-output* nil)
(defparameter *skip-generate* nil)
(defparameter *verbose* nil)
(defparameter *module-limit* nil)

(defun parse-args ()
  "Parse command-line arguments."
  (let ((args (uiop:command-line-arguments)))
    (loop for arg in args do
      (cond
        ((string= arg "--json") (setf *json-output* t))
        ((string= arg "--skip-generate") (setf *skip-generate* t))
        ((string= arg "--verbose") (setf *verbose* t))
        ((string= arg "-v") (setf *verbose* t))
        ((string= arg "--help") (show-help) (sb-ext:exit :code 0))
        ((string= arg "-h") (show-help) (sb-ext:exit :code 0))
        ((and (> (length arg) 15) (string= (subseq arg 0 15) "--module-limit="))
         (setf *module-limit* (parse-integer (subseq arg 15))))))))

(defun show-help ()
  "Display help message."
  (format t "~
Interpreter Bootstrap Fixed-Point Verification

Usage: sbcl --load build/fixpoint-check.lisp [-- options]

Options:
  --json            Output results in JSON format
  --skip-generate   Use existing Stage 0 (don't regenerate)
  --verbose, -v     Show detailed progress
  --module-limit=N  Limit modules to process for testing
  --help, -h        Show this help message

Exit Codes:
  0 - ACHIEVED: Stage 1 == Stage 2 (fixed-point verified)
  1 - NOT_ACHIEVED: Binaries differ
  2 - COMPILATION_ERROR: Stage generation failed
  3 - MISSING_DEPENDENCY: Required tools not available

Example:
  sbcl --load build/fixpoint-check.lisp -- --verbose
  sbcl --load build/fixpoint-check.lisp -- --json --skip-generate
"))

;;; Main entry point
(defun main ()
  "Run fixed-point verification and exit with appropriate code."
  (parse-args)

  (format t "~&=== Interpreter Bootstrap Fixed-Point Verification ===~%~%")

  (let ((result (clysm/interpreter-bootstrap:verify-fixpoint-interpreter
                 :verbose *verbose*
                 :skip-generate *skip-generate*
                 :module-limit *module-limit*
                 :output-format (if *json-output* :json :text))))

    (let ((exit-code (clysm/interpreter-bootstrap:fixpoint-exit-code result)))
      (format t "~%Exit code: ~D~%" exit-code)
      (sb-ext:exit :code exit-code))))

;;; Run
(main)
