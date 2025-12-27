;;;; ast.lisp - AST parser for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements T025: AST parser (cross-compiled)
;;;;
;;;; This parses S-expressions into AST nodes for compilation.

(in-package #:clysm/stage0)

;;; ============================================================
;;; AST Node Types
;;; ============================================================

(defstruct ast-node
  "Base AST node"
  (type nil :type keyword)
  (source-location nil :type (or null source-location)))

(defstruct (ast-literal (:include ast-node))
  "Literal value node"
  (value nil))

(defstruct (ast-symbol-ref (:include ast-node))
  "Symbol reference node"
  (name nil :type symbol)
  (lexical-p nil :type boolean))

(defstruct (ast-call (:include ast-node))
  "Function call node"
  (operator nil)
  (arguments nil :type list))

(defstruct (ast-if (:include ast-node))
  "If expression node"
  (test nil)
  (then nil)
  (else nil))

(defstruct (ast-lambda (:include ast-node))
  "Lambda expression node"
  (params nil :type list)
  (body nil :type list))

(defstruct (ast-let (:include ast-node))
  "Let binding node"
  (bindings nil :type list)
  (body nil :type list))

(defstruct (ast-defun (:include ast-node))
  "Function definition node"
  (name nil :type symbol)
  (params nil :type list)
  (body nil :type list))

;;; ============================================================
;;; Expression Parsing
;;; ============================================================

(defun parse-expression (form)
  "Parse S-expression into AST node"
  (cond
    ;; Literals
    ((null form) (make-ast-literal :type :literal :value nil))
    ((numberp form) (make-ast-literal :type :literal :value form))
    ((stringp form) (make-ast-literal :type :literal :value form))
    ((characterp form) (make-ast-literal :type :literal :value form))
    ((keywordp form) (make-ast-literal :type :literal :value form))

    ;; Symbol reference
    ((symbolp form)
     (make-ast-symbol-ref :type :symbol-ref :name form))

    ;; Special forms and function calls
    ((consp form)
     (parse-compound-form form))

    ;; Unknown
    (t (error "Cannot parse: ~S" form))))

(defun parse-compound-form (form)
  "Parse compound form (list)"
  (let ((op (car form))
        (args (cdr form)))
    (case op
      ;; Special forms
      (if (parse-if-form args))
      (lambda (parse-lambda-form args))
      (let (parse-let-form args))
      (let* (parse-let*-form args))
      (defun (parse-defun-form args))
      (quote (make-ast-literal :type :literal :value (first args)))
      (progn (parse-progn-form args))

      ;; Function call
      (otherwise
       (make-ast-call
        :type :call
        :operator (parse-expression op)
        :arguments (mapcar #'parse-expression args))))))

(defun parse-if-form (args)
  "Parse (if test then else)"
  (make-ast-if
   :type :if
   :test (parse-expression (first args))
   :then (parse-expression (second args))
   :else (when (third args) (parse-expression (third args)))))

(defun parse-lambda-form (args)
  "Parse (lambda (params) body...)"
  (make-ast-lambda
   :type :lambda
   :params (first args)
   :body (mapcar #'parse-expression (rest args))))

(defun parse-let-form (args)
  "Parse (let ((var val)...) body...)"
  (make-ast-let
   :type :let
   :bindings (mapcar (lambda (binding)
                       (list (first binding)
                             (parse-expression (second binding))))
                     (first args))
   :body (mapcar #'parse-expression (rest args))))

(defun parse-let*-form (args)
  "Parse (let* ((var val)...) body...)"
  ;; For now, treat like let (sequential binding in codegen)
  (parse-let-form args))

(defun parse-defun-form (args)
  "Parse (defun name (params) body...)"
  (make-ast-defun
   :type :defun
   :name (first args)
   :params (second args)
   :body (mapcar #'parse-expression (cddr args))))

(defun parse-progn-form (args)
  "Parse (progn form...)"
  (make-ast-call
   :type :call
   :operator :progn
   :arguments (mapcar #'parse-expression args)))

;;; ============================================================
;;; AST to IR Conversion
;;; ============================================================

(defun ast-to-ir (ast)
  "Convert AST node to intermediate representation.
   Returns IR form suitable for Wasm code generation."
  (etypecase ast
    (ast-literal
     `(:literal ,(ast-literal-value ast)))

    (ast-symbol-ref
     `(:var-ref ,(ast-symbol-ref-name ast)))

    (ast-call
     `(:call ,(ast-to-ir (ast-call-operator ast))
             ,(mapcar #'ast-to-ir (ast-call-arguments ast))))

    (ast-if
     `(:if ,(ast-to-ir (ast-if-test ast))
           ,(ast-to-ir (ast-if-then ast))
           ,(when (ast-if-else ast) (ast-to-ir (ast-if-else ast)))))

    (ast-lambda
     `(:lambda ,(ast-lambda-params ast)
               ,(mapcar #'ast-to-ir (ast-lambda-body ast))))

    (ast-let
     `(:let ,(mapcar (lambda (b) (list (first b) (ast-to-ir (second b))))
                     (ast-let-bindings ast))
            ,(mapcar #'ast-to-ir (ast-let-body ast))))

    (ast-defun
     `(:defun ,(ast-defun-name ast)
              ,(ast-defun-params ast)
              ,(mapcar #'ast-to-ir (ast-defun-body ast))))))
