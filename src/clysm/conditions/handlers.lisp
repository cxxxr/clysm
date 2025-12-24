;;;; handlers.lisp - Handler establishment macros
;;;; Implements handler-case and handler-bind (FR-010, FR-011)

(in-package #:clysm/conditions)

;;; ============================================================
;;; Handler-Case Macro (T033)
;;; ============================================================

;;; handler-case syntax:
;;; (handler-case expression
;;;   (condition-type ([var]) declaration* form*)
;;;   ...)
;;;
;;; Establishes handlers for the given condition types around the
;;; evaluation of expression. If a condition matching any type is
;;; signaled, control transfers to that handler's forms.

(defmacro handler-case (expression &rest clauses)
  "Execute EXPRESSION with handlers for the specified condition types.
   If EXPRESSION signals a condition matching a clause, transfer control
   to that clause's handler body. The handler body's values are returned.
   If no condition is signaled, return EXPRESSION's values.

   Syntax: (handler-case expression
             (condition-type ([var]) {declaration}* {form}*)
             ...)

   FR-010: handler-case MUST transfer control on match (non-local exit)."
  (if (null clauses)
      ;; No handlers - just evaluate expression
      expression
      ;; Generate code with handlers
      (let ((block-name (gensym "HANDLER-CASE-BLOCK"))
            (condition-var (gensym "CONDITION")))
        `(block ,block-name
           (let ((,condition-var nil))
             (declare (ignorable ,condition-var))
             (clysm/runtime/condition:with-handler-cluster
                 (clysm/runtime/condition:make-handler-cluster
                  :handlers (list ,@(mapcar
                                     (lambda (clause)
                                       (expand-handler-case-clause clause block-name condition-var))
                                     clauses)))
               ,expression))))))

(defun expand-handler-case-clause (clause block-name condition-var)
  "Expand a single handler-case clause into a handler struct.
   CLAUSE: (condition-type ([var]) . body)
   BLOCK-NAME: Symbol for the enclosing block
   CONDITION-VAR: Symbol for the condition variable"
  (destructuring-bind (type lambda-list &body body) clause
    (let ((var (if (and lambda-list (car lambda-list))
                   (car lambda-list)
                   (gensym "UNUSED-CONDITION"))))
      ;; Create handler that transfers control via return-from
      `(clysm/runtime/condition:make-handler
        :type ',type
        :function (lambda (,condition-var)
                    (return-from ,block-name
                      (let ((,var ,condition-var))
                        (declare (ignorable ,var))
                        ,@body)))))))

;;; ============================================================
;;; Handler-Bind Macro (T063)
;;; ============================================================

;;; handler-bind syntax:
;;; (handler-bind ((condition-type handler-function) ...)
;;;   . body)
;;;
;;; Unlike handler-case, handler-bind handlers do NOT automatically
;;; transfer control. The handler function is called and can:
;;; - Return normally (decline the condition, continue searching)
;;; - Invoke a restart to transfer control
;;; - Perform a non-local exit

(defmacro handler-bind (bindings &body body)
  "Execute BODY with the specified handlers established.
   Each binding is (condition-type handler-function).
   Handler functions are called when matching conditions are signaled.
   If a handler returns normally, it declines and search continues.

   Syntax: (handler-bind ((condition-type handler-fn) ...)
             {form}*)

   FR-011: handler-bind handlers MUST NOT automatically transfer control."
  (if (null bindings)
      `(progn ,@body)
      `(clysm/runtime/condition:with-handler-cluster
           (clysm/runtime/condition:make-handler-cluster
            :handlers (list ,@(mapcar #'expand-handler-bind-binding bindings)))
         ,@body)))

(defun expand-handler-bind-binding (binding)
  "Expand a single handler-bind binding into a handler struct.
   BINDING: (condition-type handler-function)"
  (destructuring-bind (type function) binding
    `(clysm/runtime/condition:make-handler
      :type ',type
      :function ,function)))

