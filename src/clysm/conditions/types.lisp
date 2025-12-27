;;;; types.lisp - Condition class hierarchy
;;;; ANSI Common Lisp condition types implemented via CLOS

(in-package #:clysm/conditions)

;;; ============================================================
;;; Base Condition Class (T008)
;;; ============================================================

(defclass condition ()
  ()
  (:documentation "Base class for all conditions.
   All exceptional situations are represented as conditions.
   FR-001: System MUST provide a condition base class."))

;;; ============================================================
;;; Serious Condition (T009)
;;; ============================================================

(defclass serious-condition (condition)
  ()
  (:documentation "Condition that requires intervention.
   Subclasses of serious-condition are not expected to be ignored.
   FR-002: System MUST provide serious-condition as subclass of condition."))

;;; ============================================================
;;; Error Class (T010)
;;; ============================================================

(defclass error (serious-condition)
  ()
  (:documentation "Error condition - usually fatal if unhandled.
   FR-003: System MUST provide error as subclass of serious-condition."))

;;; ============================================================
;;; Warning Class (T011)
;;; ============================================================

(defclass warning (condition)
  ()
  (:documentation "Warning condition - non-fatal.
   Warnings can be ignored or muffled without stopping program execution.
   FR-004: System MUST provide warning as subclass of condition."))

;;; ============================================================
;;; Simple Condition Mixin (T012)
;;; ============================================================

(defclass simple-condition (condition)
  ((format-control
    :initarg :format-control
    :initform nil
    :reader simple-condition-format-control
    :documentation "Format control string for condition message")
   (format-arguments
    :initarg :format-arguments
    :initform nil
    :reader simple-condition-format-arguments
    :documentation "Format arguments for format-control"))
  (:documentation "Condition with format-control and format-arguments slots.
   FR-005: System MUST provide simple-condition as condition subclass."))

;;; ============================================================
;;; Simple Error (T013)
;;; ============================================================

(defclass simple-error (simple-condition error)
  ()
  (:documentation "Error with format control.
   FR-006: System MUST provide simple-error combining simple-condition and error."))

;;; ============================================================
;;; Simple Warning (T014)
;;; ============================================================

(defclass simple-warning (simple-condition warning)
  ()
  (:documentation "Warning with format control.
   FR-007: System MUST provide simple-warning combining simple-condition and warning."))

;;; ============================================================
;;; Type Error (T015)
;;; ============================================================

(defclass type-error (error)
  ((datum
    :initarg :datum
    :reader type-error-datum
    :documentation "The object that failed the type check")
   (expected-type
    :initarg :expected-type
    :reader type-error-expected-type
    :documentation "The type that was expected"))
  (:documentation "Condition for type mismatch.
   FR-008: System MUST provide type-error with datum and expected-type slots."))

;;; ============================================================
;;; Cell Error (T016)
;;; ============================================================

(defclass cell-error (error)
  ((name
    :initarg :name
    :reader cell-error-name
    :documentation "The name of the cell (variable or function)"))
  (:documentation "Error related to a cell (variable or function).
   Base class for unbound-variable and undefined-function."))

;;; ============================================================
;;; Unbound Variable (T017)
;;; ============================================================

(defclass unbound-variable (cell-error)
  ()
  (:documentation "Condition for reference to an unbound variable."))

;;; ============================================================
;;; Undefined Function (T018)
;;; ============================================================

(defclass undefined-function (cell-error)
  ()
  (:documentation "Condition for call to an undefined function.
   FR-009: System MUST provide undefined-function with name slot."))

;;; ============================================================
;;; Control Error (T019)
;;; ============================================================

(defclass control-error (error)
  ()
  (:documentation "Condition for invalid control transfer.
   Signaled when invoke-restart is called with an invalid restart."))

;;; ============================================================
;;; Program Error
;;; ============================================================

(defclass program-error (error)
  ()
  (:documentation "Condition for invalid program structure."))

;;; ============================================================
;;; Stream Error (015-ffi-stream-io, T009)
;;; ============================================================

(defclass stream-error (error)
  ((stream
    :initarg :stream
    :reader clysm-stream-error-stream
    :documentation "The stream that caused the error"))
  (:documentation "Condition for stream-related errors.
   FR-014: Signaled when stream operations encounter errors."))

;;; ============================================================
;;; End of File (015-ffi-stream-io, T010)
;;; ============================================================

(defclass end-of-file (stream-error)
  ()
  (:documentation "Condition signaled when EOF is reached on input stream.
   FR-007: Input functions MUST handle end-of-file conditions appropriately."))

;;; ============================================================
;;; Format Error (032-format-function, T009)
;;; ============================================================

(defclass format-error (simple-error)
  ((control-string
    :initarg :control-string
    :initform ""
    :reader format-error-control-string
    :documentation "The format control string that caused the error")
   (position
    :initarg :position
    :initform 0
    :reader format-error-position
    :documentation "Position in control-string where error occurred"))
  (:documentation "Condition for format string errors.
   FR-015: format-error is signaled for malformed format strings.
   Subtype of simple-error with control-string and position slots."))

;;; ============================================================
;;; File Error (035-ffi-filesystem, T007)
;;; ============================================================

(defclass file-error (error)
  ((pathname
    :initarg :pathname
    :initform ""
    :reader clysm-file-error-pathname
    :type string
    :documentation "The pathname that caused the error"))
  (:documentation "Condition signaled for filesystem-related errors.
   FR-006: System MUST signal file-error condition when file operations fail.
   Contains pathname slot identifying the file that caused the error."))

;;; ============================================================
;;; Make Condition (T032)
;;; ============================================================

(defun make-condition (type &rest slot-initializations)
  "Create a condition of TYPE with given slot initializations.
   TYPE can be a condition class or type name.
   SLOT-INITIALIZATIONS are keyword arguments for the condition slots."
  (apply #'make-instance type slot-initializations))
