;;;; condition-runtime.lisp - Runtime support for condition system
;;;; Implements handler and restart stack management using shallow binding

(in-package #:cl-user)

(defpackage #:clysm/runtime/condition
  (:use #:cl)
  (:documentation "Runtime support for condition system")
  (:export
   ;; Handler structures
   #:handler
   #:make-handler
   #:handler-type
   #:handler-function
   #:handler-cluster
   #:make-handler-cluster
   #:handler-cluster-handlers
   ;; Restart structures (%restart to avoid CL:RESTART conflict)
   #:%restart
   #:make-%restart
   #:%restart-name
   #:%restart-function
   #:%restart-report-function
   #:%restart-interactive-function
   #:%restart-test-function
   #:%restart-associated-conditions
   #:restart-cluster
   #:make-restart-cluster
   #:restart-cluster-restarts
   #:restart-cluster-catch-tag
   ;; Dynamic variables
   #:*handler-clusters*
   #:*restart-clusters*
   ;; Stack operations
   #:push-handler-cluster
   #:pop-handler-cluster
   #:push-restart-cluster
   #:pop-restart-cluster
   #:with-handler-cluster
   #:with-restart-cluster
   ;; Handler dispatch
   #:find-handler
   #:call-handler))

(in-package #:clysm/runtime/condition)

;;; ============================================================
;;; Handler Structures (T020)
;;; ============================================================

(defstruct (handler (:conc-name handler-))
  "A handler binding: associates a condition type with a handler function.
   The handler function takes one argument (the condition) and either:
   - Returns normally (declines to handle, for handler-bind)
   - Performs a non-local transfer (handles, for handler-case)"
  (type nil :type t)                    ; Condition type specifier
  (function nil :type (or null function))) ; Handler function (condition) -> any

(defstruct (handler-cluster (:conc-name handler-cluster-))
  "A group of handlers established by a single handler-bind or handler-case.
   When a condition is signaled, handlers in a cluster are searched in order."
  (handlers nil :type list))            ; List of handler structs

;;; ============================================================
;;; Restart Structures (T021)
;;; ============================================================

(defstruct (%restart (:conc-name %restart-))
  "A restart object representing a recovery option.
   Contains the restart function and optional reporting/interaction functions.
   Named %restart with %restart- accessor prefix to avoid CL package conflicts."
  (name nil :type symbol)               ; Restart name for find-restart (or nil)
  (function nil :type (or null function)) ; Function to invoke
  (report-function nil :type (or null function)) ; For describe-restart
  (interactive-function nil :type (or null function)) ; For invoke-restart-interactively
  (test-function nil :type (or null function)) ; Predicate for applicability
  (associated-conditions nil :type list)) ; Conditions this restart applies to

(defstruct (restart-cluster (:conc-name restart-cluster-))
  "A group of restarts established by a single restart-case or restart-bind.
   Contains a catch tag for control transfer via throw."
  (restarts nil :type list)             ; List of restart structs
  (catch-tag nil :type symbol))         ; Unique tag for throw

;;; ============================================================
;;; Dynamic Variables (T022-T023)
;;; ============================================================

(defvar *handler-clusters* nil
  "Stack of active handler clusters. Innermost cluster is at the front.
   Managed using shallow binding pattern like special variables.")

(defvar *restart-clusters* nil
  "Stack of active restart clusters. Innermost cluster is at the front.
   Managed using shallow binding pattern like special variables.")

;;; ============================================================
;;; Handler Stack Operations (T024)
;;; ============================================================

(defun push-handler-cluster (cluster)
  "Push a handler cluster onto the handler stack.
   Returns the previous stack for unwind-protect restoration."
  (let ((previous *handler-clusters*))
    (setf *handler-clusters* (cons cluster *handler-clusters*))
    previous))

(defun pop-handler-cluster (previous)
  "Restore the handler stack to a previous state.
   Called from unwind-protect cleanup."
  (setf *handler-clusters* previous))

(defmacro with-handler-cluster (cluster &body body)
  "Execute BODY with CLUSTER pushed onto the handler stack.
   Ensures proper cleanup on normal and abnormal exit."
  (let ((prev (gensym "PREV-HANDLERS")))
    `(let ((,prev (push-handler-cluster ,cluster)))
       (unwind-protect
           (progn ,@body)
         (pop-handler-cluster ,prev)))))

;;; ============================================================
;;; Restart Stack Operations (T025)
;;; ============================================================

(defun push-restart-cluster (cluster)
  "Push a restart cluster onto the restart stack.
   Returns the previous stack for unwind-protect restoration."
  (let ((previous *restart-clusters*))
    (setf *restart-clusters* (cons cluster *restart-clusters*))
    previous))

(defun pop-restart-cluster (previous)
  "Restore the restart stack to a previous state.
   Called from unwind-protect cleanup."
  (setf *restart-clusters* previous))

(defmacro with-restart-cluster (cluster &body body)
  "Execute BODY with CLUSTER pushed onto the restart stack.
   Ensures proper cleanup on normal and abnormal exit."
  (let ((prev (gensym "PREV-RESTARTS")))
    `(let ((,prev (push-restart-cluster ,cluster)))
       (unwind-protect
           (progn ,@body)
         (pop-restart-cluster ,prev)))))

;;; ============================================================
;;; Handler Dispatch (T026)
;;; ============================================================

(defun find-handler (condition)
  "Find the first applicable handler for CONDITION.
   Searches from innermost to outermost handler cluster.
   Returns (values handler remaining-clusters) if found, nil otherwise.
   REMAINING-CLUSTERS is the clusters after the matching one (for declining)."
  (labels ((search-clusters (clusters)
             (when clusters
               (let* ((cluster (car clusters))
                      (rest-clusters (cdr clusters)))
                 (dolist (handler (handler-cluster-handlers cluster))
                   (when (typep condition (handler-type handler))
                     (return-from find-handler
                       (values handler rest-clusters))))
                 (search-clusters rest-clusters)))))
    (search-clusters *handler-clusters*)
    nil))

(defun call-handler (handler condition remaining-clusters)
  "Call HANDLER with CONDITION, temporarily unbinding the cluster.
   During handler execution, only remaining clusters are visible.
   This prevents infinite recursion if the handler signals the same condition.
   Returns the handler function's return value."
  (let ((saved-clusters *handler-clusters*))
    (setf *handler-clusters* remaining-clusters)
    (unwind-protect
        (funcall (handler-function handler) condition)
      (setf *handler-clusters* saved-clusters))))
