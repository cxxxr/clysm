;;;; free-vars.lisp - Free variable analysis (T078-T080)
;;;; Collects free variables from lambda expressions for closure conversion

(in-package #:clysm/compiler/analyzer/free-vars)

;;; ============================================================
;;; Primitive Operations
;;; ============================================================

(defparameter *primitive-operators*
  '(+ - * / truncate rem mod
    ;; Rounding functions (001-division-rounding-primitives)
    floor ceiling round ffloor fceiling fround
    = /= < <= > >=
    eq eql equal
    null not
    consp atom listp symbolp
    car cdr cons list
    first second third rest
    1+ 1-)
  "List of primitive operators that are not considered free variables.")

(defun primitive-p (sym)
  "Check if SYM is a primitive operator."
  (member sym *primitive-operators*))

;;; ============================================================
;;; Free Variable Collection (T078)
;;; ============================================================

(defun collect-free-variables (ast &optional bound-vars)
  "Collect free variables in an AST node.
   BOUND-VARS is a list of currently bound variable names.
   Returns a list of free variable symbols."
  (etypecase ast
    ;; Lambda: parameters become bound, collect from body
    (ast-lambda
     (let ((new-bound (append (ast-lambda-parameters ast) bound-vars)))
       (remove-duplicates
        (loop for form in (ast-lambda-body ast)
              append (collect-free-variables form new-bound)))))

    ;; Variable reference: free if not bound and not primitive
    (ast-var-ref
     (let ((name (ast-var-ref-name ast)))
       (if (or (member name bound-vars)
               (primitive-p name))
           nil
           (list name))))

    ;; Let/let*: bindings become bound, collect from body and init forms
    (ast-let
     (let* ((bindings (ast-let-bindings ast))
            (sequential-p (ast-let-sequential-p ast))
            ;; Collect from init forms
            (init-free
             (if sequential-p
                 ;; let*: each binding sees previous bindings
                 (loop with current-bound = bound-vars
                       for (name . init) in bindings
                       append (collect-free-variables init current-bound)
                       do (push name current-bound))
                 ;; let: all inits see only outer bindings
                 (loop for (name . init) in bindings
                       append (collect-free-variables init bound-vars))))
            ;; All bindings are visible in body
            (body-bound (append (mapcar #'car bindings) bound-vars))
            ;; Collect from body
            (body-free
             (loop for form in (ast-let-body ast)
                   append (collect-free-variables form body-bound))))
       (remove-duplicates (append init-free body-free))))

    ;; Function call: collect from arguments (function name is not a variable)
    (ast-call
     (let ((fn (ast-call-function ast)))
       (remove-duplicates
        (append
         ;; If function position is an AST node (funcall), collect from it
         (when (typep fn 'ast-node)
           (collect-free-variables fn bound-vars))
         ;; Collect from arguments
         (loop for arg in (ast-call-arguments ast)
               append (collect-free-variables arg bound-vars))))))

    ;; If: collect from all branches
    (ast-if
     (remove-duplicates
      (append (collect-free-variables (ast-if-test ast) bound-vars)
              (collect-free-variables (ast-if-then ast) bound-vars)
              (when (ast-if-else ast)
                (collect-free-variables (ast-if-else ast) bound-vars)))))

    ;; Progn: collect from all forms
    (ast-progn
     (remove-duplicates
      (loop for form in (ast-progn-forms ast)
            append (collect-free-variables form bound-vars))))

    ;; Setq: the variable might be free, collect from value
    (ast-setq
     (let ((name (ast-setq-name ast)))
       (remove-duplicates
        (append (unless (member name bound-vars)
                  (list name))
                (collect-free-variables (ast-setq-value ast) bound-vars)))))

    ;; Block: collect from body
    (ast-block
     (remove-duplicates
      (loop for form in (ast-block-body ast)
            append (collect-free-variables form bound-vars))))

    ;; Return-from: collect from value
    (ast-return-from
     (when (ast-return-from-value ast)
       (collect-free-variables (ast-return-from-value ast) bound-vars)))

    ;; Defun: parameters become bound, collect from body
    (ast-defun
     (let ((new-bound (append (ast-defun-parameters ast) bound-vars)))
       (remove-duplicates
        (loop for form in (ast-defun-body ast)
              append (collect-free-variables form new-bound)))))

    ;; Literals: no free variables
    (ast-literal
     nil)))
