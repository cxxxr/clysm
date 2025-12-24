;;;; signaling.lisp - Condition signaling functions
;;;; Implements signal, warn, error, cerror (FR-020 to FR-023)

(in-package #:clysm/conditions)

;;; ============================================================
;;; Internal Signaling (T027)
;;; ============================================================

(defun signal-internal (condition)
  "Internal function to dispatch a condition to handlers.
   Iterates through handler clusters from innermost to outermost.
   For each matching handler, calls it with proper cluster unbinding.
   If a handler returns normally (declines), continues to next handler.
   If a handler performs non-local exit, control transfers immediately.
   Returns NIL if all handlers decline or no handlers match."
  (let ((clusters clysm/runtime/condition:*handler-clusters*))
    (loop while clusters do
      (let* ((cluster (car clusters))
             (rest-clusters (cdr clusters)))
        ;; Search handlers in this cluster
        (dolist (handler (clysm/runtime/condition:handler-cluster-handlers cluster))
          (when (typep condition (clysm/runtime/condition:handler-type handler))
            ;; Found matching handler - call it with cluster unbound
            ;; If handler returns normally, it declines - continue searching
            ;; If handler does non-local exit, control transfers
            (clysm/runtime/condition:call-handler handler condition rest-clusters)))
        ;; Move to next cluster
        (setf clusters rest-clusters))))
  ;; All handlers declined or no handlers found
  nil)

;;; ============================================================
;;; Coercion Utility
;;; ============================================================

(defun coerce-to-condition (datum arguments default-type)
  "Coerce DATUM to a condition object.
   If DATUM is a condition, return it.
   If DATUM is a symbol (type), create condition of that type with ARGUMENTS.
   If DATUM is a string, create DEFAULT-TYPE with format-control and format-arguments."
  (typecase datum
    (condition datum)
    (symbol (apply #'make-condition datum arguments))
    (string (make-condition default-type
                            :format-control datum
                            :format-arguments arguments))
    (t (cl:error "Invalid condition designator: ~S" datum))))

;;; ============================================================
;;; Error Function (T031)
;;; ============================================================

(defun error (datum &rest arguments)
  "Signal an error condition. If not handled, enters debugger (traps in WasmGC).
   DATUM: Condition object, condition type, or format string.
   ARGUMENTS: Initargs if DATUM is a type, or format args if DATUM is a string.
   FR-021: error function MUST signal error and enter debugger if unhandled."
  (let ((condition (coerce-to-condition datum arguments 'simple-error)))
    ;; Signal the condition - if a handler transfers control, we don't return
    (signal-internal condition)
    ;; No handler transferred control - invoke unhandled error behavior (T036)
    (invoke-debugger condition)))

;;; ============================================================
;;; Unhandled Error Behavior (T036)
;;; ============================================================

(defun invoke-debugger (condition)
  "Handle an unhandled error condition.
   In WasmGC environment, prints message and traps.
   FR-021: Unhandled errors MUST enter debugger (trap in WasmGC)."
  ;; Print error message to *error-output*
  (format cl:*error-output* "~&Unhandled error: ~A~%" condition)
  ;; In WasmGC, we trap (unreachable instruction)
  ;; For now in host Lisp, we call CL:ERROR
  (cl:error "Unhandled condition: ~A" condition))

;;; ============================================================
;;; Signal Function (T081)
;;; ============================================================

(defun signal (datum &rest arguments)
  "Signal a condition. If no handler transfers control, returns NIL.
   DATUM: Condition object, condition type, or format string.
   ARGUMENTS: Initargs if DATUM is a type, or format args if DATUM is a string.
   FR-020: signal function MUST invoke handlers without default action."
  (let ((condition (coerce-to-condition datum arguments 'simple-condition)))
    (signal-internal condition)
    nil))

;;; ============================================================
;;; Warn Function (T051)
;;; ============================================================

(defun warn (datum &rest arguments)
  "Signal a warning. If not muffled, prints to *error-output* and continues.
   DATUM: Condition object, condition type, or format string.
   ARGUMENTS: Initargs if DATUM is a type, or format args if DATUM is a string.
   FR-022: warn function MUST signal warning and print if not muffled."
  (let ((condition (coerce-to-condition datum arguments 'simple-warning)))
    ;; Establish muffle-warning restart (T052, T053)
    (restart-case
        (progn
          ;; Signal the warning - handlers may muffle or decline
          (signal-internal condition)
          ;; No handler muffled - output warning (T054)
          (format cl:*error-output* "~&Warning: ~A~%" (format-condition condition)))
      (muffle-warning ()
        :report "Muffle the warning"
        nil)))
  nil)

(defun format-condition (condition)
  "Format a condition for display.
   Uses format-control and format-arguments for simple-condition types."
  (if (typep condition 'simple-condition)
      (let ((format-control (simple-condition-format-control condition))
            (format-arguments (simple-condition-format-arguments condition)))
        (if format-control
            (apply #'format nil format-control format-arguments)
            (format nil "~S" condition)))
      (format nil "~S" condition)))

;;; ============================================================
;;; Cerror Function (T057)
;;; ============================================================

(defun cerror (continue-format-control datum &rest arguments)
  "Signal a continuable error with CONTINUE restart.
   CONTINUE-FORMAT-CONTROL: Format string for continue restart report.
   DATUM: Condition object, condition type, or format string.
   ARGUMENTS: Initargs if DATUM is a type, or format args if DATUM is a string.
   FR-023: cerror function MUST establish continue restart."
  (declare (ignore continue-format-control)) ; Used for :report in full impl
  (let ((condition (coerce-to-condition datum arguments 'simple-error)))
    ;; Establish continue restart (T058, T059)
    (restart-case
        (progn
          (signal-internal condition)
          ;; No handler transferred - invoke debugger
          (invoke-debugger condition))
      (continue ()
        :report "Continue from the error"
        nil)))
  nil)


