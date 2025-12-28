;;;; eval.lisp - Core evaluator for Stage 0 interpreter
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T006: Core evaluator with eval-form dispatch
;;;;
;;;; Implements recursive evaluator with environment passing
;;;; per research.md architecture decisions.
;;;;
;;;; Supported forms:
;;;; - Literals: fixnum, NIL, T
;;;; - Variables: symbol lookup
;;;; - Special forms: quote, if, let, let*, defun, lambda
;;;; - Applications: primitive and user-defined functions

(in-package #:clysm/stage0)

;;; ============================================================
;;; Global Function Environment
;;; ============================================================

(defvar *global-functions* (make-hash-table :test #'eq)
  "Global function definitions created by defun")

(defun get-global-function (name)
  "Get global function by name"
  (gethash name *global-functions*))

(defun set-global-function (name fn)
  "Set global function definition"
  (setf (gethash name *global-functions*) fn))

(defun clear-global-functions ()
  "Clear all global function definitions"
  (clrhash *global-functions*))

;;; ============================================================
;;; Closure Representation
;;; ============================================================

(defstruct (closure (:constructor make-closure-internal))
  "Closure capturing parameters, body, and environment"
  (params nil :type list)
  (body nil)
  (env nil))

(defun make-closure (params body env)
  "Create a closure with given parameters, body, and captured environment"
  (make-closure-internal :params params :body body :env env))

;;; ============================================================
;;; Core Evaluator
;;; ============================================================

(defun eval-form (form env)
  "Evaluate FORM in environment ENV.
   Dispatches based on form type:
   - Self-evaluating: numbers, NIL, T
   - Symbols: variable lookup or special constants
   - Lists: special forms or function applications"
  (cond
    ;; Self-evaluating literals
    ((null form) nil)                      ; NIL
    ((eq form t) t)                        ; T
    ((integerp form) form)                 ; Fixnum
    ((stringp form) form)                  ; String literal

    ;; Symbol lookup
    ((symbolp form)
     (eval-symbol form env))

    ;; Compound forms
    ((consp form)
     (eval-compound form env))

    ;; Unknown form type
    (t (error "Cannot evaluate form: ~S" form))))

;;; ============================================================
;;; Symbol Evaluation
;;; ============================================================

(defun eval-symbol (sym env)
  "Evaluate a symbol by looking it up in the environment"
  ;; Check for special constants first
  (cond
    ((eq sym 'nil) nil)
    ((eq sym 't) t)
    (t
     ;; Look up in lexical environment
     (multiple-value-bind (value found-p) (lookup sym env)
       (if found-p
           value
           (error "Unbound variable: ~S" sym))))))

;;; ============================================================
;;; Compound Form Evaluation
;;; ============================================================

(defun eval-compound (form env)
  "Evaluate a compound form (list).
   Handles special forms and function applications."
  (let ((op (car form))
        (args (cdr form)))
    (cond
      ;; Special forms
      ((eq op 'quote)
       (eval-quote args))
      ((eq op 'if)
       (eval-if args env))
      ((eq op 'let)
       (eval-let args env))
      ((eq op 'let*)
       (eval-let* args env))
      ((eq op 'defun)
       (eval-defun args env))
      ((eq op 'lambda)
       (eval-lambda args env))
      ((eq op 'progn)
       (eval-progn args env))

      ;; Function application
      (t
       (eval-application op args env)))))

;;; ============================================================
;;; T081 [US6]: Quote
;;; ============================================================

(defun eval-quote (args)
  "Evaluate (quote datum) - return datum unevaluated"
  (unless (= (length args) 1)
    (error "quote requires exactly one argument"))
  (first args))

;;; ============================================================
;;; T061 [US4]: If
;;; ============================================================

(defun eval-if (args env)
  "Evaluate (if test then else)"
  (let ((test (first args))
        (then (second args))
        (else (third args)))
    (if (eval-form test env)
        (eval-form then env)
        (when else
          (eval-form else env)))))

;;; ============================================================
;;; T062-T063 [US4]: Let and Let*
;;; ============================================================

(defun eval-let (args env)
  "Evaluate (let ((var val) ...) body...)
   All bindings evaluated in parallel (original env)"
  (let ((bindings (first args))
        (body (rest args)))
    ;; Evaluate all values in original environment
    (let ((new-bindings
            (mapcar (lambda (binding)
                      (list (first binding)
                            (eval-form (second binding) env)))
                    bindings)))
      ;; Extend environment with all bindings
      (let ((new-env (extend-env* new-bindings env)))
        ;; Evaluate body forms
        (eval-progn body new-env)))))

(defun eval-let* (args env)
  "Evaluate (let* ((var val) ...) body...)
   Bindings evaluated sequentially"
  (let ((bindings (first args))
        (body (rest args)))
    ;; Extend environment sequentially
    (let ((new-env env))
      (dolist (binding bindings)
        (let ((var (first binding))
              (val (eval-form (second binding) new-env)))
          (setf new-env (extend-env var val new-env))))
      ;; Evaluate body forms
      (eval-progn body new-env))))

;;; ============================================================
;;; Progn (implicit in let body)
;;; ============================================================

(defun eval-progn (forms env)
  "Evaluate forms sequentially, return last value"
  (let ((result nil))
    (dolist (form forms result)
      (setf result (eval-form form env)))))

;;; ============================================================
;;; T034-T037 [US2]: Defun
;;; ============================================================

(defun eval-defun (args env)
  "Evaluate (defun name (params...) body...)
   Creates global function definition"
  (declare (ignore env))
  (let ((name (first args))
        (params (second args))
        (body (cddr args)))
    ;; Create closure with empty env (defun captures nothing)
    (let ((fn (make-closure params
                            (if (= (length body) 1)
                                (first body)
                                (cons 'progn body))
                            nil)))
      ;; Register in global function table
      (set-global-function name fn)
      ;; Return function name per CL convention
      name)))

;;; ============================================================
;;; T070-T072 [US5]: Lambda
;;; ============================================================

(defun eval-lambda (args env)
  "Evaluate (lambda (params...) body...)
   Creates closure capturing current environment"
  (let ((params (first args))
        (body (rest args)))
    (make-closure params
                  (if (= (length body) 1)
                      (first body)
                      (cons 'progn body))
                  env)))

;;; ============================================================
;;; Function Application
;;; ============================================================

(defun eval-application (op args env)
  "Evaluate function application (op arg1 arg2 ...)"
  ;; Evaluate all arguments
  (let ((evaluated-args (mapcar (lambda (arg) (eval-form arg env)) args)))
    (cond
      ;; Check for primitive
      ((and (symbolp op) (primitive-p op))
       (funcall (get-primitive op) evaluated-args))

      ;; Check for global function
      ((and (symbolp op) (get-global-function op))
       (apply-closure (get-global-function op) evaluated-args))

      ;; Lambda expression in operator position
      ((and (consp op) (eq (car op) 'lambda))
       (let ((closure (eval-lambda (cdr op) env)))
         (apply-closure closure evaluated-args)))

      ;; Closure value
      ((closure-p op)
       (apply-closure op evaluated-args))

      ;; Evaluate operator and retry
      ((symbolp op)
       (let ((fn-val (eval-form op env)))
         (if (closure-p fn-val)
             (apply-closure fn-val evaluated-args)
             (error "Not a function: ~S" op))))

      (t (error "Invalid operator: ~S" op)))))

(defun apply-closure (closure args)
  "Apply closure to arguments"
  (let ((params (closure-params closure))
        (body (closure-body closure))
        (captured-env (closure-env closure)))
    ;; Check arity
    (unless (= (length params) (length args))
      (error "Wrong number of arguments: expected ~D, got ~D"
             (length params) (length args)))
    ;; Bind parameters to arguments
    (let ((new-env captured-env))
      (loop for param in params
            for arg in args
            do (setf new-env (extend-env param arg new-env)))
      ;; Evaluate body in extended environment
      (eval-form body new-env))))
