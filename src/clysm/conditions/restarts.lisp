;;;; restarts.lisp - Restart system implementation
;;;; Implements restart-case, restart-bind, find-restart, etc. (FR-014 to FR-019)

(in-package #:clysm/conditions)

;;; ============================================================
;;; Find Restart (T041)
;;; ============================================================

(defun find-restart (identifier &optional condition)
  "Find a restart by name or return the restart object itself.
   IDENTIFIER: Restart name (symbol) or restart object
   CONDITION: If provided, only restarts applicable to this condition
   Returns: Restart object or NIL
   FR-017: find-restart MUST search visible restarts by name."
  (declare (ignore condition)) ; TODO: condition association in later phase
  (etypecase identifier
    (symbol
     ;; Search through all restart clusters for a restart with this name
     (dolist (cluster clysm/runtime/condition:*restart-clusters*)
       (dolist (restart (clysm/runtime/condition:restart-cluster-restarts cluster))
         (when (eq identifier (clysm/runtime/condition:%restart-name restart))
           (return-from find-restart restart))))
     nil)
    (clysm/runtime/condition:%restart
     ;; Already a restart object - return it
     identifier)))

;;; ============================================================
;;; Compute Restarts (T042)
;;; ============================================================

(defun compute-restarts (&optional condition)
  "Return a list of all currently visible restarts.
   CONDITION: If provided, filter to restarts applicable to this condition
   Returns: List of restart objects, innermost first
   FR-018: compute-restarts MUST return all visible restarts."
  (declare (ignore condition)) ; TODO: condition association in later phase
  (let ((result nil))
    (dolist (cluster clysm/runtime/condition:*restart-clusters*)
      (dolist (restart (clysm/runtime/condition:restart-cluster-restarts cluster))
        (push restart result)))
    (nreverse result)))

;;; ============================================================
;;; Invoke Restart (T043)
;;; ============================================================

(defun invoke-restart (restart-designator &rest arguments)
  "Invoke a restart, transferring control to it.
   RESTART-DESIGNATOR: Restart name (symbol) or restart object
   ARGUMENTS: Arguments to pass to the restart function
   FR-016: invoke-restart MUST use catch/throw for control transfer."
  (let ((restart (if (symbolp restart-designator)
                     (find-restart restart-designator)
                     restart-designator)))
    (unless restart
      (error 'control-error))
    ;; Find the cluster containing this restart to get the catch tag
    (let ((catch-tag (find-restart-catch-tag restart)))
      (unless catch-tag
        (error 'control-error))
      ;; Throw to the catch tag with the restart and arguments
      (throw catch-tag (cons restart arguments)))))

(defun find-restart-catch-tag (restart)
  "Find the catch tag for the cluster containing RESTART."
  (dolist (cluster clysm/runtime/condition:*restart-clusters*)
    (when (member restart (clysm/runtime/condition:restart-cluster-restarts cluster))
      (return-from find-restart-catch-tag
        (clysm/runtime/condition:restart-cluster-catch-tag cluster))))
  nil)

;;; ============================================================
;;; Restart Case Macro (T044)
;;; ============================================================

;;; restart-case syntax:
;;; (restart-case expression
;;;   (restart-name (lambda-list)
;;;     [:interactive interactive-fn]
;;;     [:report report-fn]
;;;     [:test test-fn]
;;;     . body)
;;;   ...)

(defmacro restart-case (expression &rest clauses)
  "Execute EXPRESSION with the specified restarts established.
   If a restart is invoked, transfer control to its body.
   Returns the value of EXPRESSION or the invoked restart body.

   Syntax: (restart-case expression
             (restart-name (lambda-list)
               [:interactive fn] [:report fn] [:test fn]
               {form}*)
             ...)

   FR-014: restart-case MUST establish restarts visible during body."
  (if (null clauses)
      expression
      (let ((catch-tag (gensym "RESTART-CASE-TAG"))
            (restart-result (gensym "RESTART-RESULT")))
        `(let ((,restart-result
                 (catch ',catch-tag
                   (clysm/runtime/condition:with-restart-cluster
                       (clysm/runtime/condition:make-restart-cluster
                        :restarts (list ,@(mapcar
                                           (lambda (clause)
                                             (expand-restart-case-clause clause))
                                           clauses))
                        :catch-tag ',catch-tag)
                     ;; Return a special marker for normal completion
                     (cons :normal-return ,expression)))))
           ;; Check if we got normal return or restart invocation
           (if (and (consp ,restart-result)
                    (eq :normal-return (car ,restart-result)))
               (cdr ,restart-result)
               ;; Restart was invoked - dispatch to appropriate handler
               (let ((restart (car ,restart-result))
                     (args (cdr ,restart-result)))
                 (apply (clysm/runtime/condition:%restart-function restart) args)))))))

(defun expand-restart-case-clause (clause)
  "Expand a single restart-case clause into a restart struct."
  (destructuring-bind (name lambda-list &body body-and-options) clause
    (multiple-value-bind (body options)
        (parse-restart-options body-and-options)
      (let ((interactive-fn (getf options :interactive))
            (report-fn (getf options :report))
            (test-fn (getf options :test)))
        `(clysm/runtime/condition:make-%restart
          :name ',name
          :function (lambda ,lambda-list ,@body)
          ,@(when interactive-fn `(:interactive-function ,interactive-fn))
          ,@(when report-fn `(:report-function ,report-fn))
          ,@(when test-fn `(:test-function ,test-fn)))))))

(defun parse-restart-options (body-and-options)
  "Parse keyword options from the beginning of a restart clause body.
   Returns (values body options-plist)."
  (let ((options nil)
        (remaining body-and-options))
    (loop while (and remaining
                     (keywordp (car remaining))
                     (member (car remaining) '(:interactive :report :test)))
          do (push (cadr remaining) options)
             (push (car remaining) options)
             (setf remaining (cddr remaining)))
    (values remaining (nreverse options))))

;;; ============================================================
;;; Invoke Restart Interactively (T047)
;;; ============================================================

(defun invoke-restart-interactively (restart-designator)
  "Invoke a restart, using its interactive function to get arguments.
   RESTART-DESIGNATOR: Restart name (symbol) or restart object
   FR-019: invoke-restart-interactively MUST call interactive function."
  (let ((restart (if (symbolp restart-designator)
                     (find-restart restart-designator)
                     restart-designator)))
    (unless restart
      (error 'control-error))
    (let ((interactive-fn (clysm/runtime/condition:%restart-interactive-function restart)))
      (if interactive-fn
          (apply #'invoke-restart restart (funcall interactive-fn))
          (invoke-restart restart)))))

;;; ============================================================
;;; With-Simple-Restart Macro (T075)
;;; ============================================================

(defmacro with-simple-restart ((name format-control &rest format-arguments) &body body)
  "Execute BODY with a simple restart established.
   Returns (values result nil) on normal completion.
   Returns (values nil t) if the restart is invoked.

   Syntax: (with-simple-restart (name format-control . format-args)
             {form}*)

   FR-029: with-simple-restart MUST return (values nil t) when invoked."
  (declare (ignore format-control format-arguments)) ; Used for :report in full impl
  (let ((normal-return-p (gensym "NORMAL-RETURN")))
    `(let ((,normal-return-p t))
       (values
        (restart-case
            (progn ,@body)
          (,name ()
            :report "Simple restart"
            (setf ,normal-return-p nil)
            nil))
        (not ,normal-return-p)))))

;;; ============================================================
;;; Restart Bind Macro (T080)
;;; ============================================================

(defmacro restart-bind (bindings &body body)
  "Execute BODY with the specified restart bindings established.
   Unlike restart-case, restart-bind doesn't wrap restarts in a catch.
   The restart functions must handle their own control transfer.

   Syntax: (restart-bind ((name function {key value}*) ...)
             {form}*)

   FR-015: restart-bind MUST establish restarts without implicit catch."
  (if (null bindings)
      `(progn ,@body)
      `(clysm/runtime/condition:with-restart-cluster
           (clysm/runtime/condition:make-restart-cluster
            :restarts (list ,@(mapcar #'expand-restart-bind-binding bindings))
            :catch-tag nil)  ; No catch tag - caller handles control transfer
         ,@body)))

(defun expand-restart-bind-binding (binding)
  "Expand a single restart-bind binding into a restart struct."
  (destructuring-bind (name function &key interactive-function report-function test-function) binding
    `(clysm/runtime/condition:make-%restart
      :name ',name
      :function ,function
      ,@(when interactive-function `(:interactive-function ,interactive-function))
      ,@(when report-function `(:report-function ,report-function))
      ,@(when test-function `(:test-function ,test-function)))))

