;;;; condition-api.lisp - Condition System API Contracts
;;;; Defines the public interface for the ANSI CL Condition System

(in-package #:clysm/conditions)

;;; ============================================================
;;; Condition Type Hierarchy
;;; ============================================================

;;; Base condition class
(defclass condition ()
  ()
  (:documentation "Base class for all conditions."))

(defclass serious-condition (condition)
  ()
  (:documentation "Condition requiring intervention."))

(defclass error (serious-condition)
  ()
  (:documentation "Error condition - usually fatal if unhandled."))

(defclass warning (condition)
  ()
  (:documentation "Warning condition - non-fatal."))

;;; Simple condition mixin
(defclass simple-condition ()
  ((format-control :initarg :format-control
                   :initform nil
                   :reader simple-condition-format-control)
   (format-arguments :initarg :format-arguments
                     :initform nil
                     :reader simple-condition-format-arguments))
  (:documentation "Mixin for conditions with format strings."))

(defclass simple-error (simple-condition error)
  ()
  (:documentation "Error with format control."))

(defclass simple-warning (simple-condition warning)
  ()
  (:documentation "Warning with format control."))

;;; Type error
(defclass type-error (error)
  ((datum :initarg :datum
          :reader type-error-datum)
   (expected-type :initarg :expected-type
                  :reader type-error-expected-type))
  (:documentation "Condition for type mismatch."))

;;; Cell errors
(defclass cell-error (error)
  ((name :initarg :name
         :reader cell-error-name))
  (:documentation "Error related to a cell (variable or function)."))

(defclass unbound-variable (cell-error)
  ()
  (:documentation "Reference to unbound variable."))

(defclass undefined-function (cell-error)
  ()
  (:documentation "Call to undefined function."))

;;; Control errors
(defclass control-error (error)
  ()
  (:documentation "Invalid control transfer."))

(defclass program-error (error)
  ()
  (:documentation "Invalid program structure."))

;;; ============================================================
;;; Handler Establishment
;;; ============================================================

;;; handler-case macro signature
;;; (handler-case expression
;;;   (condition-type ([var]) . handler-body)
;;;   ...)
;;; Returns: Value from expression or handler body

;;; handler-bind macro signature
;;; (handler-bind ((condition-type handler-function) ...)
;;;   . body)
;;; Returns: Value from body

;;; ============================================================
;;; Restart Establishment
;;; ============================================================

;;; restart-case macro signature
;;; (restart-case expression
;;;   (restart-name ([lambda-list])
;;;     [:interactive interactive-fn]
;;;     [:report report-fn]
;;;     [:test test-fn]
;;;     . restart-body)
;;;   ...)
;;; Returns: Value from expression or restart body

;;; restart-bind macro signature
;;; (restart-bind ((name function . options) ...)
;;;   . body)
;;; Returns: Value from body

;;; with-simple-restart macro signature
;;; (with-simple-restart (name format-control . format-args)
;;;   . body)
;;; Returns: (values result nil) or (values nil t)

;;; ============================================================
;;; Signaling Functions
;;; ============================================================

(defgeneric signal (datum &rest arguments)
  (:documentation
   "Signal a condition. Searches for handlers and invokes them.
    If no handler transfers control, returns NIL.
    DATUM: Condition object or condition type designator
    ARGUMENTS: Initargs if DATUM is a type designator"))

(defgeneric warn (datum &rest arguments)
  (:documentation
   "Signal a warning. If not handled, prints to *error-output*.
    DATUM: Condition object or condition type designator
    ARGUMENTS: Initargs if DATUM is a type designator"))

(defgeneric error (datum &rest arguments)
  (:documentation
   "Signal an error. If not handled, enters debugger (traps).
    DATUM: Condition object or condition type designator
    ARGUMENTS: Initargs if DATUM is a type designator"))

(defgeneric cerror (continue-format-control datum &rest arguments)
  (:documentation
   "Signal a continuable error with CONTINUE restart.
    CONTINUE-FORMAT-CONTROL: Format string for continue restart report
    DATUM: Condition object or condition type designator
    ARGUMENTS: Initargs if DATUM is a type designator"))

;;; ============================================================
;;; Restart Functions
;;; ============================================================

(defun find-restart (identifier &optional condition)
  "Find a restart by name, optionally associated with CONDITION.
   IDENTIFIER: Restart name (symbol) or restart object
   CONDITION: If provided, only restarts applicable to this condition
   Returns: Restart object or NIL"
  (declare (ignore identifier condition))
  nil)

(defun compute-restarts (&optional condition)
  "Return a list of all currently active restarts.
   CONDITION: If provided, only restarts applicable to this condition
   Returns: List of restart objects"
  (declare (ignore condition))
  nil)

(defun invoke-restart (restart-designator &rest arguments)
  "Invoke the designated restart with ARGUMENTS.
   RESTART-DESIGNATOR: Restart name (symbol) or restart object
   ARGUMENTS: Arguments to pass to restart function
   Does not return (performs non-local transfer)"
  (declare (ignore restart-designator arguments))
  nil)

(defun invoke-restart-interactively (restart-designator)
  "Invoke restart, prompting for arguments if needed.
   RESTART-DESIGNATOR: Restart name (symbol) or restart object
   Does not return (performs non-local transfer)"
  (declare (ignore restart-designator))
  nil)

;;; ============================================================
;;; Standard Restart Functions
;;; ============================================================

(defun abort (&optional condition)
  "Invoke the ABORT restart.
   CONDITION: Associated condition (optional)
   Does not return (performs non-local transfer)"
  (invoke-restart 'abort condition))

(defun continue (&optional condition)
  "Invoke the CONTINUE restart.
   CONDITION: Associated condition (optional)
   Returns NIL if CONTINUE restart not found"
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defun muffle-warning (&optional condition)
  "Invoke the MUFFLE-WARNING restart.
   CONDITION: Associated condition (optional)
   Does not return (performs non-local transfer)"
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart
      (invoke-restart restart))
    (error 'control-error)))

(defun use-value (value &optional condition)
  "Invoke the USE-VALUE restart with VALUE.
   VALUE: Value to use
   CONDITION: Associated condition (optional)
   Does not return (performs non-local transfer)"
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (invoke-restart restart value))
    (error 'control-error)))

(defun store-value (value &optional condition)
  "Invoke the STORE-VALUE restart with VALUE.
   VALUE: Value to store
   CONDITION: Associated condition (optional)
   Does not return (performs non-local transfer)"
  (let ((restart (find-restart 'store-value condition)))
    (when restart
      (invoke-restart restart value))
    (error 'control-error)))

;;; ============================================================
;;; Condition Accessors
;;; ============================================================

(defgeneric restart-name (restart)
  (:documentation "Return the name of RESTART."))

(defgeneric print-object (condition stream)
  (:documentation "Print condition to stream."))

(defgeneric make-condition (type &rest slot-initializations)
  (:documentation "Create a condition of TYPE with given initializations."))

;;; ============================================================
;;; Internal Runtime Support
;;; ============================================================

;;; *handler-clusters* - Stack of active handler clusters
;;; *restart-clusters* - Stack of active restart clusters

;;; Handler search: (find-handler condition) -> handler or nil
;;; Restart search: (find-restart-internal name condition) -> restart or nil

;;; Handler invocation: (invoke-handler handler condition) -> returns or transfers
;;; Restart invocation: (invoke-restart-internal restart args) -> transfers (no return)
