;;;; args.lisp - Command-line argument parsing for Clysm CLI
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements T022-T024: parse-args, validation, help/version

(in-package #:clysm/cli)

;;; ============================================================
;;; T022: Implement parse-args function
;;; ============================================================

(defstruct parsed-args
  "Parsed command-line arguments."
  (command nil :type (or symbol null))    ; :compile, :help, :version
  (patterns nil :type list)                ; List of glob patterns
  (output "" :type string)                 ; Output file path (-o/--output)
  (verbose nil :type boolean)              ; Verbose mode (-v/--verbose)
  (force nil :type boolean)                ; Force rebuild (-f/--force)
  (continue t :type boolean)               ; Continue on error (-c/--continue)
  (cache-dir ".clysm-cache" :type string)  ; Cache directory
  (no-progress nil :type boolean)          ; Disable progress
  (json nil :type boolean)                 ; JSON output format
  (help nil :type boolean)                 ; Show help (-h/--help)
  (version nil :type boolean)              ; Show version (-V/--version)
  (errors nil :type list))                 ; Parsing errors

(defun parse-args (args)
  "Parse command-line arguments ARGS into a parsed-args structure.
   ARGS is a list of strings (command-line arguments).

   Returns a parsed-args structure.
   If there are parsing errors, they are stored in the errors slot."
  (let ((result (make-parsed-args))
        (remaining args)
        (patterns nil))

    ;; If empty args, return with help flag
    (when (null args)
      (setf (parsed-args-help result) t)
      (return-from parse-args result))

    ;; First arg should be command
    (let ((cmd (car remaining)))
      (cond
        ((or (equal cmd "compile") (equal cmd "c"))
         (setf (parsed-args-command result) :compile)
         (setf remaining (cdr remaining)))
        ((or (equal cmd "--help") (equal cmd "-h") (equal cmd "help"))
         (setf (parsed-args-help result) t)
         (return-from parse-args result))
        ((or (equal cmd "--version") (equal cmd "-V") (equal cmd "version"))
         (setf (parsed-args-version result) t)
         (return-from parse-args result))
        (t
         ;; Unknown command - assume compile if it looks like a pattern
         (if (or (search "." cmd) (search "*" cmd) (search "/" cmd))
             (progn
               (setf (parsed-args-command result) :compile)
               ;; Don't consume, treat as pattern
               )
             (progn
               (push (format nil "Unknown command: ~A" cmd) (parsed-args-errors result))
               (setf remaining (cdr remaining)))))))

    ;; Parse remaining args
    (loop while remaining do
      (let ((arg (pop remaining)))
        (cond
          ;; Output flag
          ((or (equal arg "-o") (equal arg "--output"))
           (if remaining
               (setf (parsed-args-output result) (pop remaining))
               (push "Missing argument for --output" (parsed-args-errors result))))

          ;; Combined -o=value form
          ((and (>= (length arg) 3) (equal (subseq arg 0 2) "-o"))
           (if (char= (char arg 2) #\=)
               (setf (parsed-args-output result) (subseq arg 3))
               (setf (parsed-args-output result) (subseq arg 2))))

          ;; Verbose flag
          ((or (equal arg "-v") (equal arg "--verbose"))
           (setf (parsed-args-verbose result) t))

          ;; Force flag
          ((or (equal arg "-f") (equal arg "--force"))
           (setf (parsed-args-force result) t))

          ;; Continue flag
          ((or (equal arg "-c") (equal arg "--continue"))
           (setf (parsed-args-continue result) t))

          ;; No-continue flag (--no-continue)
          ((equal arg "--no-continue")
           (setf (parsed-args-continue result) nil))

          ;; Cache directory
          ((equal arg "--cache-dir")
           (if remaining
               (setf (parsed-args-cache-dir result) (pop remaining))
               (push "Missing argument for --cache-dir" (parsed-args-errors result))))

          ;; No progress
          ((equal arg "--no-progress")
           (setf (parsed-args-no-progress result) t))

          ;; JSON output
          ((equal arg "--json")
           (setf (parsed-args-json result) t))

          ;; Help
          ((or (equal arg "-h") (equal arg "--help"))
           (setf (parsed-args-help result) t))

          ;; Version
          ((or (equal arg "-V") (equal arg "--version"))
           (setf (parsed-args-version result) t))

          ;; Unknown flag
          ((and (> (length arg) 0) (char= (char arg 0) #\-))
           (push (format nil "Unknown option: ~A" arg) (parsed-args-errors result)))

          ;; Pattern (not a flag)
          (t
           (push arg patterns)))))

    ;; Store patterns in order
    (setf (parsed-args-patterns result) (nreverse patterns))

    result))

;;; ============================================================
;;; T023: Implement option validation
;;; ============================================================

(defun validate-args (args)
  "Validate parsed-args structure ARGS.
   Returns a list of error strings, or NIL if valid.

   Validation rules:
   - compile command requires --output
   - compile command requires at least one pattern
   - patterns must not be empty strings"
  (let ((errors (copy-list (parsed-args-errors args))))

    ;; Skip validation for help/version
    (when (or (parsed-args-help args) (parsed-args-version args))
      (return-from validate-args nil))

    ;; Check command
    (unless (parsed-args-command args)
      (push "No command specified" errors))

    ;; For compile command
    (when (eq (parsed-args-command args) :compile)
      ;; Require output
      (when (or (null (parsed-args-output args))
                (equal (parsed-args-output args) ""))
        (push "Missing required option: --output" errors))

      ;; Require at least one pattern
      (when (null (parsed-args-patterns args))
        (push "No input patterns specified" errors))

      ;; Check for empty patterns
      (dolist (pattern (parsed-args-patterns args))
        (when (equal pattern "")
          (push "Empty pattern not allowed" errors))))

    (nreverse errors)))

;;; ============================================================
;;; T024: Implement help and version display
;;; ============================================================

(defparameter *clysm-version* "0.1.0"
  "Current version of Clysm CLI.")

(defun show-help (&optional (stream *standard-output*))
  "Display help message to STREAM."
  (format stream "~
clysm - WebAssembly GC Common Lisp Compiler

USAGE:
    clysm compile <patterns...> -o <output> [options]

COMMANDS:
    compile, c    Compile Lisp source files to WebAssembly

OPTIONS:
    -o, --output <path>    Output Wasm file path (required)
    -v, --verbose          Show detailed compilation output
    -f, --force            Force full recompilation (ignore cache)
    -c, --continue         Continue on compilation errors (default)
    --no-continue          Stop on first compilation error
    --cache-dir <path>     Cache directory (default: .clysm-cache/)
    --no-progress          Disable progress indicator
    --json                 Output results as JSON
    -h, --help             Show this help message
    -V, --version          Show version information

EXAMPLES:
    # Compile all Lisp files
    clysm compile 'src/**/*.lisp' -o output.wasm

    # Verbose mode with force rebuild
    clysm compile 'src/**/*.lisp' -o output.wasm -v -f

    # Multiple patterns
    clysm compile 'src/core/*.lisp' 'src/lib/*.lisp' -o output.wasm

EXIT CODES:
    0    Success - all files compiled
    1    Partial success - some files had errors
    2    Failure - compilation failed
    3    Invalid arguments
    4    No files matched patterns
    5    Output error

For more information, see: https://github.com/clysm/clysm
"))

(defun show-version (&optional (stream *standard-output*))
  "Display version information to STREAM."
  (format stream "clysm version ~A~%" *clysm-version*))

;;; ============================================================
;;; Exit codes
;;; ============================================================

(defparameter +exit-success+ 0 "All files compiled successfully.")
(defparameter +exit-partial+ 1 "Some files compiled; others had errors.")
(defparameter +exit-failure+ 2 "Compilation failed; no output produced.")
(defparameter +exit-invalid-args+ 3 "Invalid command-line arguments.")
(defparameter +exit-no-files+ 4 "No files matched input patterns.")
(defparameter +exit-output-error+ 5 "Could not write output file.")
