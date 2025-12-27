;;;; types.lisp - Type definitions for Stage 1 compiler generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Defines structs for source modules, forms, compilation results, and reports

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Source Module Types
;;; ==========================================================================

(defstruct source-module
  "Represents a Lisp source file in the compiler codebase."
  (path "" :type string)                ; Absolute file path
  (relative-path "" :type string)       ; Path relative to repo root
  (module-index 0 :type fixnum)         ; Position in compilation order (1-41)
  (forms nil :type list)                ; List of source-form structs
  (dependency-group "" :type string)    ; Logical grouping: backend, reader, compiler, etc.
  (status :pending :type keyword))      ; :pending, :reading, :parsed, :compiling, :done

(defstruct source-form
  "A single top-level S-expression from a source file."
  (id "" :type string)                  ; Unique identifier: {module_index}:{form_index}
  (sexp nil)                            ; The actual S-expression
  (operator nil :type symbol)           ; Head of the form (defun, defmacro, etc.)
  (name nil)                            ; Name being defined (if applicable)
  (source-text "" :type string)         ; Original source code for error reporting
  (module nil)                          ; Back-reference to source-module
  (index 0 :type fixnum)                ; Position within module
  (compilable-p nil :type boolean))     ; Whether form is in blessed subset

(defstruct compilation-result
  "Result of attempting to compile a single form."
  (form nil :type (or null source-form)) ; Reference to source form
  (form-id "" :type string)              ; Form identifier
  (success-p nil :type boolean)          ; Whether compilation succeeded
  (wasm-bytes nil :type (or null vector)) ; Compiled output (if success)
  (error-type nil :type symbol)          ; Category of error (if failed)
  (error-message "" :type string)        ; Detailed error description
  (unsupported-feature nil :type symbol)) ; Specific CL feature causing failure

;;; ==========================================================================
;;; Progress Report Types
;;; ==========================================================================

(defstruct failure-group
  "Failures grouped by operator type."
  (operator nil :type symbol)           ; The failing operator
  (count 0 :type fixnum)                ; Number of failures
  (example "" :type string)             ; Representative failing form
  (examples nil :type list))            ; Up to 3 examples

(defstruct module-stats
  "Per-module compilation statistics."
  (path "" :type string)                ; Module relative path
  (total-forms 0 :type fixnum)          ; Number of forms in module
  (compiled 0 :type fixnum)             ; Successfully compiled forms
  (failed 0 :type fixnum)               ; Failed forms
  (skipped 0 :type fixnum)              ; Non-compilable forms skipped
  (failures nil :type list))            ; List of failure-group structs

(defstruct summary
  "Overall compilation summary."
  (total-forms 0 :type fixnum)          ; Forms across all modules
  (compiled 0 :type fixnum)             ; Total successful compilations
  (failed 0 :type fixnum)               ; Total failed compilations
  (skipped 0 :type fixnum)              ; Total skipped forms
  (coverage-pct 0.0 :type float)        ; Compilation success rate 0.0-100.0
  (top-blockers nil :type list))        ; Top 5 blockers by impact

(defstruct progress-report
  "Aggregated statistics for a compilation run."
  (timestamp "" :type string)           ; ISO 8601 timestamp
  (stage0-version "" :type string)      ; Version/commit of Stage 0 used
  (modules nil :type list)              ; List of module-stats (41 entries)
  (summary nil :type (or null summary))) ; Overall statistics

;;; ==========================================================================
;;; Blocker Analysis Types
;;; ==========================================================================

(defstruct blocker-info
  "Information about a compilation blocker."
  (operator nil :type symbol)           ; Unsupported operator
  (affected-forms 0 :type fixnum)       ; Forms affected by this blocker
  (impact-pct 0.0 :type float)          ; Percentage of total forms
  (priority "" :type string)            ; HIGH, MEDIUM, LOW
  (recommendation "" :type string)      ; Suggested action
  (examples nil :type list))            ; Up to 3 example forms

;;; ==========================================================================
;;; Diff Analysis Types
;;; ==========================================================================

(defstruct binary-info
  "Wasm binary metadata."
  (path "" :type string)                ; File path
  (size-bytes 0 :type fixnum)           ; File size
  (exports nil :type list)              ; Exported function names
  (types 0 :type fixnum)                ; Number of type definitions
  (functions 0 :type fixnum)            ; Number of functions
  (valid-p nil :type boolean))          ; Whether passes validation

(defstruct diff-details
  "Differences between binaries."
  (size-delta "" :type string)          ; e.g., "+516 bytes"
  (missing-exports nil :type list)      ; Exports in Stage 0 not in Stage 1
  (new-exports nil :type list)          ; Exports in Stage 1 not in Stage 0
  (type-changes nil :type list))        ; Modified type definitions

(defstruct diff-report
  "Comparison between two Wasm binaries."
  (stage0 nil :type (or null binary-info)) ; Stage 0 binary details
  (stage1 nil :type (or null binary-info)) ; Stage 1 binary details
  (differences nil :type (or null diff-details))) ; What changed

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun make-form-id (module-index form-index)
  "Create a unique form identifier string."
  (format nil "~D:~D" module-index form-index))

(defun parse-form-id (form-id)
  "Parse a form-id string into (module-index . form-index)."
  (let ((colon-pos (position #\: form-id)))
    (when colon-pos
      (cons (parse-integer form-id :end colon-pos)
            (parse-integer form-id :start (1+ colon-pos))))))

(defun form-operator (sexp)
  "Extract the operator from a top-level form.
Returns the car of the form if it's a list, otherwise nil."
  (if (consp sexp)
      (car sexp)
      nil))

(defun form-name (sexp)
  "Extract the name being defined from a defining form.
Returns the second element for defun, defmacro, defvar, etc."
  (when (and (consp sexp)
             (member (car sexp) '(defun defmacro defmethod defgeneric
                                  defvar defparameter defconstant
                                  defstruct defclass define-condition)))
    (cadr sexp)))
