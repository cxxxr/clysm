;;;; interpreter.lisp - Tier 1 S-expression interpreter
;;;; Phase 9: Eval/JIT Infrastructure

(in-package #:clysm/eval/interpreter)

;;; ============================================================
;;; Interpreter Environment
;;; ============================================================

(defstruct (interpreter-env (:constructor %make-interpreter-env))
  "Interpreter environment for variable bindings."
  (bindings (make-hash-table :test 'eq) :type hash-table)
  (parent nil :type (or null interpreter-env)))

(defun make-interpreter-env (&optional parent)
  "Create a new interpreter environment, optionally with a parent."
  (let ((env (%make-interpreter-env :parent parent)))
    ;; Initialize with built-in functions
    (when (null parent)
      (install-builtins env))
    env))

(defun env-bind (env name value)
  "Bind NAME to VALUE in ENV."
  (setf (gethash name (interpreter-env-bindings env)) value))

(defun env-lookup (env name)
  "Look up NAME in ENV, searching parent environments."
  (multiple-value-bind (value found)
      (gethash name (interpreter-env-bindings env))
    (if found
        value
        (if (interpreter-env-parent env)
            (env-lookup (interpreter-env-parent env) name)
            (error "Unbound variable: ~S" name)))))

(defun env-bound-p (env name)
  "Check if NAME is bound in ENV."
  (multiple-value-bind (value found)
      (gethash name (interpreter-env-bindings env))
    (declare (ignore value))
    (or found
        (and (interpreter-env-parent env)
             (env-bound-p (interpreter-env-parent env) name)))))

(defun extend-env (env bindings)
  "Create a new environment extending ENV with BINDINGS (alist)."
  (let ((new-env (make-interpreter-env env)))
    (dolist (binding bindings)
      (env-bind new-env (car binding) (cdr binding)))
    new-env))

;;; ============================================================
;;; Built-in Functions
;;; ============================================================

(defun install-builtins (env)
  "Install built-in functions into ENV."
  ;; Arithmetic
  (env-bind env '+ (lambda (&rest args) (apply #'+ args)))
  (env-bind env '- (lambda (&rest args) (apply #'- args)))
  (env-bind env '* (lambda (&rest args) (apply #'* args)))
  (env-bind env '/ (lambda (&rest args) (apply #'/ args)))

  ;; Comparisons
  (env-bind env '< #'<)
  (env-bind env '> #'>)
  (env-bind env '<= #'<=)
  (env-bind env '>= #'>=)
  (env-bind env '= #'=)
  (env-bind env 'eq #'eq)
  (env-bind env 'eql #'eql)
  (env-bind env 'equal #'equal)

  ;; List operations
  (env-bind env 'cons #'cons)
  (env-bind env 'car #'car)
  (env-bind env 'cdr #'cdr)
  (env-bind env 'list #'list)
  (env-bind env 'null #'null)
  (env-bind env 'listp #'listp)
  (env-bind env 'consp #'consp)
  (env-bind env 'atom #'atom)
  (env-bind env 'first #'first)
  (env-bind env 'rest #'rest)
  (env-bind env 'length #'length)
  (env-bind env 'append #'append)
  (env-bind env 'reverse #'reverse)
  (env-bind env 'nth #'nth)
  (env-bind env 'nthcdr #'nthcdr)

  ;; Type predicates
  (env-bind env 'numberp #'numberp)
  (env-bind env 'symbolp #'symbolp)
  (env-bind env 'stringp #'stringp)
  (env-bind env 'functionp #'functionp)

  ;; Other utilities
  (env-bind env 'not #'not)
  (env-bind env 'identity #'identity)
  (env-bind env 'funcall #'funcall)
  (env-bind env 'apply #'apply)

  ;; Constants
  (env-bind env 't t)
  (env-bind env 'nil nil))

;;; ============================================================
;;; Interpreter Core
;;; ============================================================

(defvar *default-env* nil
  "Default interpreter environment.")

(defun get-default-env ()
  "Get or create the default interpreter environment."
  (unless *default-env*
    (setf *default-env* (make-interpreter-env)))
  *default-env*)

(defun interpret (expr &optional env)
  "Interpret an S-expression in the given environment."
  (let ((env (or env (get-default-env))))
    (interpret-form expr env)))

(defun interpret-form (form env)
  "Interpret a form in the given environment."
  (cond
    ;; Self-evaluating forms
    ((self-evaluating-p form)
     form)
    ;; Symbol (variable reference)
    ((symbolp form)
     (env-lookup env form))
    ;; List (special form or function call)
    ((consp form)
     (interpret-list form env))
    ;; Unknown
    (t
     (error "Cannot interpret: ~S" form))))

(defun self-evaluating-p (form)
  "Check if FORM is self-evaluating."
  (or (numberp form)
      (stringp form)
      (keywordp form)
      (characterp form)
      (null form)
      (eq form t)
      (vectorp form)))

;;; ============================================================
;;; Special Forms
;;; ============================================================

(defun interpret-list (form env)
  "Interpret a list form (special form or function call)."
  (let ((op (first form)))
    (case op
      ;; Quote
      (quote
       (second form))
      ;; If
      (if
       (interpret-if form env))
      ;; Progn
      (progn
       (interpret-progn (rest form) env))
      ;; Let
      (let
       (interpret-let (second form) (cddr form) env nil))
      ;; Let*
      (let*
       (interpret-let (second form) (cddr form) env t))
      ;; Lambda
      (lambda
       (interpret-lambda (second form) (cddr form) env))
      ;; Setq
      (setq
       (interpret-setq (second form) (third form) env))
      ;; Block
      (block
       (interpret-block (second form) (cddr form) env))
      ;; Return-from
      (return-from
       (interpret-return-from (second form) (third form) env))
      ;; Tagbody
      (tagbody
       (interpret-tagbody (rest form) env))
      ;; Go
      (go
       (interpret-go (second form)))
      ;; Flet
      (flet
       (interpret-flet (second form) (cddr form) env))
      ;; Labels
      (labels
       (interpret-labels (second form) (cddr form) env))
      ;; Funcall
      (funcall
       (let ((fn (interpret-form (second form) env))
             (args (mapcar (lambda (arg) (interpret-form arg env))
                           (cddr form))))
         (apply fn args)))
      ;; Function application
      (otherwise
       (interpret-application form env)))))

;;; If

(defun interpret-if (form env)
  "Interpret an if form."
  (let ((test (second form))
        (then (third form))
        (else (fourth form)))
    (if (interpret-form test env)
        (interpret-form then env)
        (when else
          (interpret-form else env)))))

;;; Progn

(defun interpret-progn (forms env)
  "Interpret a progn (sequence of forms)."
  (let ((result nil))
    (dolist (form forms result)
      (setf result (interpret-form form env)))))

;;; Let / Let*

(defun interpret-let (bindings body env sequential-p)
  "Interpret a let or let* form."
  (let ((new-env (if sequential-p
                     (interpret-let*-bindings bindings env)
                     (interpret-let-bindings bindings env))))
    (interpret-progn body new-env)))

(defun interpret-let-bindings (bindings env)
  "Process parallel let bindings."
  (let ((pairs (mapcar (lambda (binding)
                         (if (consp binding)
                             (cons (first binding)
                                   (interpret-form (second binding) env))
                             (cons binding nil)))
                       bindings)))
    (extend-env env pairs)))

(defun interpret-let*-bindings (bindings env)
  "Process sequential let* bindings."
  (let ((current-env env))
    (dolist (binding bindings)
      (let* ((name (if (consp binding) (first binding) binding))
             (value (if (consp binding)
                        (interpret-form (second binding) current-env)
                        nil)))
        (setf current-env (extend-env current-env (list (cons name value))))))
    current-env))

;;; Lambda

(defstruct interpreted-closure
  "A closure created by lambda interpretation."
  (params nil :type list)
  (body nil :type list)
  (env nil :type (or null interpreter-env)))

(defun interpret-lambda (params body env)
  "Interpret a lambda expression, returning a closure."
  ;; Return a host function that captures the closure
  (let ((closure (make-interpreted-closure :params params :body body :env env)))
    (lambda (&rest args)
      (apply-interpreted-closure closure args))))

(defun apply-interpreted-closure (closure args)
  "Apply an interpreted closure to arguments."
  (let* ((params (interpreted-closure-params closure))
         (body (interpreted-closure-body closure))
         (env (interpreted-closure-env closure))
         (bindings (mapcar #'cons params args))
         (new-env (extend-env env bindings)))
    (interpret-progn body new-env)))

;;; Setq

(defun interpret-setq (name value-form env)
  "Interpret a setq form."
  (let ((value (interpret-form value-form env)))
    ;; Find and update the binding in the environment chain
    (labels ((find-and-set (e)
               (when e
                 (multiple-value-bind (old-val found)
                     (gethash name (interpreter-env-bindings e))
                   (declare (ignore old-val))
                   (if found
                       (setf (gethash name (interpreter-env-bindings e)) value)
                       (find-and-set (interpreter-env-parent e)))))))
      (if (env-bound-p env name)
          (find-and-set env)
          (env-bind env name value)))
    value))

;;; Block / Return-from

(define-condition block-return ()
  ((block-name :initarg :block-name :reader block-return-name)
   (value :initarg :value :reader block-return-value)))

(defun interpret-block (name body env)
  "Interpret a block form."
  (handler-case
      (progn
        (env-bind env name (list :block name))
        (interpret-progn body env))
    (block-return (c)
      (if (eq name (block-return-name c))
          (block-return-value c)
          (error c)))))

(defun interpret-return-from (name value-form env)
  "Interpret a return-from form."
  (let ((value (interpret-form value-form env)))
    (error 'block-return :block-name name :value value)))

;;; Tagbody / Go

(define-condition go-tag ()
  ((tag :initarg :tag :reader go-tag-name)))

(defun interpret-tagbody (forms env)
  "Interpret a tagbody form."
  (let ((tag-positions (make-hash-table :test 'eq))
        (current-forms forms))
    ;; Build tag position table
    (loop for remaining on forms
          for form = (car remaining)
          when (symbolp form)
            do (setf (gethash form tag-positions) (cdr remaining)))
    ;; Execute
    (loop
      (handler-case
          (progn
            (dolist (form current-forms)
              (unless (symbolp form)
                (interpret-form form env)))
            (return nil))
        (go-tag (c)
          (let ((pos (gethash (go-tag-name c) tag-positions)))
            (if pos
                (setf current-forms pos)
                (error c))))))))

(defun interpret-go (tag)
  "Interpret a go form."
  (error 'go-tag :tag tag))

;;; Flet / Labels

(defun interpret-flet (definitions body env)
  "Interpret a flet form (non-recursive local functions)."
  (let* ((fns (mapcar (lambda (def)
                        (let ((name (first def))
                              (params (second def))
                              (fn-body (cddr def)))
                          (cons name (interpret-lambda params fn-body env))))
                      definitions))
         (new-env (extend-env env fns)))
    (interpret-progn body new-env)))

(defun interpret-labels (definitions body env)
  "Interpret a labels form (recursive local functions)."
  ;; Create environment with forward references
  (let ((new-env (make-interpreter-env env)))
    ;; First pass: bind names to placeholders
    (dolist (def definitions)
      (env-bind new-env (first def) nil))
    ;; Second pass: create closures with new-env and update bindings
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (fn-body (cddr def))
             (closure (interpret-lambda params fn-body new-env)))
        (env-bind new-env name closure)))
    (interpret-progn body new-env)))

;;; Function Application

(defun interpret-application (form env)
  "Interpret a function application."
  (let* ((op (first form))
         (fn (cond
               ((symbolp op) (env-lookup env op))
               ((and (consp op) (eq (first op) 'lambda))
                (interpret-lambda (second op) (cddr op) env))
               (t (error "Invalid function: ~S" op))))
         (args (mapcar (lambda (arg) (interpret-form arg env))
                       (rest form))))
    (unless (functionp fn)
      (error "Not a function: ~S" fn))
    (apply fn args)))
