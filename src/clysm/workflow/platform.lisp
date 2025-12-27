;;;; platform.lisp - Platform abstraction for development workflow
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Provides cross-platform abstractions for file operations:
;;;; - SBCL: Uses UIOP for file operations
;;;; - Stage 1+: Uses FFI to host environment (Node.js/browser)

(in-package #:clysm/workflow)

;;; ============================================================
;;; T014: FFI declarations for fs.glob, fs.mtime, fs.read, fs.exists
;;; ============================================================
;;;
;;; Host shim must provide these functions under "clysm:fs" namespace.

#|
The following FFI functions are required from the host (for Stage 1+):

  clysm:fs.glob (pattern directory) -> list-of-strings
    Expands glob pattern relative to directory.
    - pattern: string - glob pattern (e.g., "**/*.lisp")
    - directory: string - base directory path
    Returns: list of absolute paths matching pattern

  clysm:fs.mtime (pathname) -> integer
    Returns file modification time as Unix epoch seconds.
    - pathname: string - file path
    Returns: integer - Unix timestamp, or 0 if file doesn't exist

  clysm:fs.read (pathname) -> string
    Reads entire file contents as UTF-8 string.
    - pathname: string - file path
    Returns: string - file contents
    Throws: on error (file not found, permission denied)

  clysm:fs.exists (pathname) -> boolean
    Checks if file exists.
    - pathname: string - file path
    Returns: boolean (as i32: 0=false, 1=true)
|#

;;; ============================================================
;;; T015: SBCL-side glob-expand using UIOP
;;; ============================================================

(defun glob-expand (pattern &optional (directory (uiop:getcwd)))
  "Expand PATTERN to matching file paths relative to DIRECTORY.
   Returns a list of absolute paths matching the pattern.

   PATTERN is a glob pattern (e.g., \"**/*.lisp\" or \"src/*.lisp\").
   DIRECTORY is the base directory for relative patterns.

   For SBCL, uses UIOP:DIRECTORY-FILES with wildcard handling.
   For Stage 1+, uses FFI to host glob implementation."
  (let ((base-dir (uiop:ensure-directory-pathname directory)))
    (cond
      ;; Handle ** recursive patterns
      ((search "**" pattern)
       (let* ((parts (uiop:split-string pattern :separator "/"))
              (before-star (loop for p in parts
                                 while (not (equal p "**"))
                                 collect p))
              (after-star (cdr (member "**" parts :test #'equal)))
              (file-pattern (car (last after-star)))
              (subdir (format nil "~{~A/~}" before-star))
              (search-dir (uiop:merge-pathnames* subdir base-dir)))
         ;; Recursively find files matching the pattern
         (loop for file in (uiop:directory-files search-dir)
               when (and (pathname-name file)
                         (or (null file-pattern)
                             (uiop:string-suffix-p file file-pattern)))
               collect (namestring file) into results
               finally (return (sort results #'string<)))))

      ;; Handle simple wildcard patterns
      ((find #\* pattern)
       (let* ((search-path (uiop:merge-pathnames* (pathname pattern) base-dir))
              (files (directory search-path)))
         (sort (mapcar #'namestring files) #'string<)))

      ;; No wildcards - just check if file exists
      (t
       (let ((path (uiop:merge-pathnames* pattern base-dir)))
         (if (uiop:file-exists-p path)
             (list (namestring path))
             nil))))))

;;; ============================================================
;;; T016: file-mtime wrapper for both SBCL and Stage 1
;;; ============================================================

(defun file-mtime (pathname)
  "Return the modification time of PATHNAME as Unix epoch seconds.
   Returns 0 if file does not exist or cannot be accessed.

   For SBCL, uses UIOP:SAFE-FILE-WRITE-DATE with Unix epoch conversion.
   For Stage 1+, uses FFI to host."
  (let ((write-date (uiop:safe-file-write-date pathname)))
    (if write-date
        ;; Convert from Universal Time to Unix epoch
        ;; Universal Time is seconds since 1900-01-01 00:00:00 UTC
        ;; Unix epoch is seconds since 1970-01-01 00:00:00 UTC
        ;; Difference: 2208988800 seconds
        (- write-date 2208988800)
        0)))

;;; ============================================================
;;; T017: read-file-string wrapper for both SBCL and Stage 1
;;; ============================================================

(defun read-file-string (pathname)
  "Read entire contents of PATHNAME as a UTF-8 string.
   Signals an error if file does not exist or cannot be read.

   For SBCL, uses UIOP:READ-FILE-STRING.
   For Stage 1+, uses FFI to host."
  (uiop:read-file-string pathname :external-format :utf-8))

(defun file-exists-p (pathname)
  "Return T if PATHNAME exists and is a regular file, NIL otherwise.

   For SBCL, uses UIOP:FILE-EXISTS-P.
   For Stage 1+, uses FFI to host."
  (uiop:file-exists-p pathname))

(defun ensure-directory (pathname)
  "Ensure that the directory for PATHNAME exists, creating it if necessary.
   Returns the directory pathname.

   For SBCL, uses UIOP:ENSURE-ALL-DIRECTORIES-EXIST.
   For Stage 1+, uses FFI to host."
  (let ((dir (uiop:pathname-directory-pathname pathname)))
    (ensure-directories-exist dir)
    dir))

;;; ============================================================
;;; Additional utilities for path handling
;;; ============================================================

(defun relative-pathname (path base-directory)
  "Return PATH as a string relative to BASE-DIRECTORY.
   If PATH is not under BASE-DIRECTORY, returns the absolute path."
  (let ((abs-path (namestring (truename path)))
        (abs-base (namestring (truename base-directory))))
    ;; Ensure base ends with /
    (unless (char= (char abs-base (1- (length abs-base))) #\/)
      (setf abs-base (concatenate 'string abs-base "/")))
    ;; Check if path starts with base
    (if (uiop:string-prefix-p abs-base abs-path)
        (subseq abs-path (length abs-base))
        abs-path)))

(defun absolute-pathname (path &optional (base-directory (uiop:getcwd)))
  "Return PATH as an absolute pathname string.
   If PATH is relative, resolves it relative to BASE-DIRECTORY."
  (namestring (uiop:merge-pathnames* path base-directory)))
