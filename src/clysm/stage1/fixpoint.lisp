;;;; fixpoint.lisp - Fixed-point verification status and utilities
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Defines fixpoint-status type and exit code mappings

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Exit Code Constants (FR-007)
;;; ==========================================================================

(defconstant +exit-code-achieved+ 0
  "Exit code for fixed-point achieved (Stage 1 == Stage 2).")

(defconstant +exit-code-not-achieved+ 1
  "Exit code for fixed-point not achieved (binaries differ).")

(defconstant +exit-code-compilation-error+ 2
  "Exit code for Stage 2 compilation failure.")

(defconstant +exit-code-missing-dependency+ 3
  "Exit code for missing dependency (wasmtime, Stage 1, etc.).")

;;; ==========================================================================
;;; Status Type
;;; ==========================================================================

(deftype fixpoint-status ()
  "Valid status values for fixed-point verification."
  '(member :achieved :not-achieved :compilation-error :missing-dependency))

;;; ==========================================================================
;;; Status/Exit Code Conversion
;;; ==========================================================================

(defun status-to-exit-code (status)
  "Convert fixpoint-status keyword to exit code integer.
Returns 0 for :achieved, 1 for :not-achieved, 2 for :compilation-error,
3 for :missing-dependency."
  (ecase status
    (:achieved +exit-code-achieved+)
    (:not-achieved +exit-code-not-achieved+)
    (:compilation-error +exit-code-compilation-error+)
    (:missing-dependency +exit-code-missing-dependency+)))

(defun exit-code-to-status (code)
  "Convert exit code integer to fixpoint-status keyword."
  (case code
    (0 :achieved)
    (1 :not-achieved)
    (2 :compilation-error)
    (3 :missing-dependency)
    (otherwise :unknown)))

;;; ==========================================================================
;;; Status Formatting
;;; ==========================================================================

(defun format-fixpoint-status (status &key (stream *standard-output*))
  "Format fixpoint status for human-readable output."
  (ecase status
    (:achieved
     (format stream "FIXED-POINT ACHIEVED~%~%Binaries are byte-identical. Self-hosting verified."))
    (:not-achieved
     (format stream "FIXED-POINT NOT ACHIEVED~%~%Stage 1 and Stage 2 binaries differ."))
    (:compilation-error
     (format stream "COMPILATION ERROR~%~%Stage 2 generation failed."))
    (:missing-dependency
     (format stream "MISSING DEPENDENCY~%~%Required tools not available."))))

;;; ==========================================================================
;;; Timestamp Utilities
;;; ==========================================================================

(defun current-iso-timestamp ()
  "Return current time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; ==========================================================================
;;; Git Commit Utilities
;;; ==========================================================================

(defun current-git-commit ()
  "Return current git commit hash or empty string."
  (handler-case
      (let ((output (uiop:run-program
                     '("git" "rev-parse" "--short" "HEAD")
                     :output :string
                     :error-output nil
                     :ignore-error-status t)))
        (string-trim '(#\Space #\Newline #\Return) (or output "")))
    (error () "")))
