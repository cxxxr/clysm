;;;; ast.lisp - Abstract Syntax Tree definitions
;;;; Defines the intermediate representation between parsing and codegen

(in-package #:clysm/compiler/ast)

;;; ============================================================
;;; Source Location (for error reporting)
;;; ============================================================

(defstruct (source-location (:conc-name sl-))
  "Source code location for error reporting."
  (file nil :type (or null string))
  (line 0 :type fixnum)
  (column 0 :type fixnum))

;;; ============================================================
;;; Base AST Node
;;; ============================================================

(defstruct (ast-node (:conc-name ast-node-))
  "Base AST node."
  (source-location nil :type (or null source-location)))

;;; ============================================================
;;; Literal Values (T043)
;;; ============================================================

(defstruct (ast-literal (:include ast-node) (:conc-name ast-literal-))
  "Literal value node (fixnum, string, T, NIL, etc.)"
  (value nil :type t)
  (literal-type nil :type keyword))  ; :fixnum, :string, :nil, :t, :keyword

(defun make-fixnum-literal (value)
  "Create a fixnum literal AST node"
  (make-ast-literal :value value :literal-type :fixnum))

(defun make-nil-literal ()
  "Create a NIL literal AST node"
  (make-ast-literal :value nil :literal-type :nil))

(defun make-t-literal ()
  "Create a T literal AST node"
  (make-ast-literal :value t :literal-type :t))

;;; ============================================================
;;; Variable References (T044)
;;; ============================================================

(defstruct (ast-var-ref (:include ast-node) (:conc-name ast-var-ref-))
  "Variable reference node."
  (name nil :type symbol)
  (binding nil :type t)  ; Filled during analysis
  (scope nil :type (member nil :local :global :special)))

(defun make-var-ref (name)
  "Create a variable reference AST node"
  (make-ast-var-ref :name name :scope nil))

;;; ============================================================
;;; Function Calls (T045)
;;; ============================================================

(defstruct (ast-call (:include ast-node) (:conc-name ast-call-))
  "Function call node."
  (function nil :type t)  ; Symbol or AST node (for funcall)
  (arguments nil :type list)
  (call-type nil :type (member nil :named :primitive :funcall)))

(defun make-call (function args)
  "Create a function call AST node"
  (make-ast-call :function function :arguments args))

;;; ============================================================
;;; Lambda Expressions
;;; ============================================================

(defstruct (ast-lambda (:include ast-node) (:conc-name ast-lambda-))
  "Lambda expression node."
  (parameters nil :type list)  ; Parameter names
  (body nil :type list)        ; Body forms (AST nodes)
  (free-vars nil :type list)   ; Free variables (filled during analysis)
  (env-type nil :type t))      ; Environment type for closure

;;; ============================================================
;;; Function Definition
;;; ============================================================

(defstruct (ast-defun (:include ast-node) (:conc-name ast-defun-))
  "Function definition node."
  (name nil :type symbol)
  (parameters nil :type list)
  (body nil :type list)
  (docstring nil :type (or null string)))

;;; ============================================================
;;; Binding Forms
;;; ============================================================

(defstruct (ast-let (:include ast-node) (:conc-name ast-let-))
  "Let binding node."
  (bindings nil :type list)       ; ((name . value-ast) ...)
  (body nil :type list)           ; Body forms (AST nodes)
  (sequential-p nil :type boolean))  ; T for let*, NIL for let

(defstruct (ast-setq (:include ast-node) (:conc-name ast-setq-))
  "Variable assignment node."
  (name nil :type symbol)
  (value nil :type t)  ; AST node for value
  (binding nil :type t))

;;; ============================================================
;;; Control Flow
;;; ============================================================

(defstruct (ast-if (:include ast-node) (:conc-name ast-if-))
  "Conditional node."
  (test nil :type t)    ; AST node for condition
  (then nil :type t)    ; AST node for then branch
  (else nil :type t))   ; AST node for else branch (may be NIL)

(defstruct (ast-progn (:include ast-node) (:conc-name ast-progn-))
  "Sequential execution node."
  (forms nil :type list))  ; List of AST nodes

(defstruct (ast-block (:include ast-node) (:conc-name ast-block-))
  "Block node for non-local exits."
  (name nil :type symbol)
  (body nil :type list))

(defstruct (ast-return-from (:include ast-node) (:conc-name ast-return-from-))
  "Return-from node."
  (block-name nil :type symbol)
  (value nil :type t))

;;; ============================================================
;;; Tagbody/Go
;;; ============================================================

(defstruct (ast-tagbody (:include ast-node) (:conc-name ast-tagbody-))
  "Tagbody node for goto-based control flow."
  (tags nil :type list)      ; List of tag symbols in order
  (segments nil :type list)) ; ((tag . (forms...)) ...) where tag is symbol or nil

(defstruct (ast-go (:include ast-node) (:conc-name ast-go-))
  "Go node for transfer to a tag."
  (tag nil :type symbol))

;;; ============================================================
;;; Catch/Throw
;;; ============================================================

(defstruct (ast-catch (:include ast-node) (:conc-name ast-catch-))
  "Catch node for dynamic non-local exit."
  (tag nil :type t)        ; AST node for tag expression
  (body nil :type list))   ; Body forms (AST nodes)

(defstruct (ast-throw (:include ast-node) (:conc-name ast-throw-))
  "Throw node for dynamic non-local exit."
  (tag nil :type t)        ; AST node for tag expression
  (value nil :type t))     ; AST node for result value

;;; ============================================================
;;; Unwind-Protect
;;; ============================================================

(defstruct (ast-unwind-protect (:include ast-node) (:conc-name ast-unwind-protect-))
  "Unwind-protect node for cleanup on exit."
  (protected-form nil :type t)   ; AST node for protected form
  (cleanup-forms nil :type list)) ; List of cleanup AST nodes

;;; ============================================================
;;; Special Variable Definitions (T017-T018)
;;; ============================================================

(defstruct (ast-defvar (:include ast-node) (:conc-name ast-defvar-))
  "Defvar node for declaring special variables (T017).
   defvar declares a variable as special and optionally initializes it
   only if it is currently unbound."
  (name nil :type symbol)
  (init-form nil :type t)   ; AST node or NIL if no init
  (docstring nil :type (or null string)))

(defstruct (ast-defparameter (:include ast-node) (:conc-name ast-defparameter-))
  "Defparameter node for declaring special variables (T018).
   Unlike defvar, defparameter always initializes the variable."
  (name nil :type symbol)
  (init-form nil :type t)   ; AST node (required)
  (docstring nil :type (or null string)))

;;; ============================================================
;;; Local Function Definitions
;;; ============================================================

(defstruct (ast-flet (:include ast-node) (:conc-name ast-flet-))
  "Flet node for local non-recursive function definitions."
  (definitions nil :type list)  ; ((name params body...) ...)
  (body nil :type list))        ; Body forms (AST nodes)

(defstruct (ast-labels (:include ast-node) (:conc-name ast-labels-))
  "Labels node for local recursive function definitions."
  (definitions nil :type list)  ; ((name params body...) ...)
  (body nil :type list))        ; Body forms (AST nodes)

;;; ============================================================
;;; Wasm IR Structures (T046)
;;; ============================================================

(defstruct (wasm-ir (:conc-name wasm-ir-))
  "Wasm intermediate representation for a module."
  (types nil :type list)      ; Type definitions
  (functions nil :type list)  ; Function definitions
  (globals nil :type list)    ; Global variables
  (exports nil :type list)    ; Exported symbols
  (start-fn nil :type (or null fixnum)))  ; Start function index

(defstruct (wasm-func (:conc-name wasm-func-))
  "Wasm function definition."
  (name nil :type symbol)
  (type-idx nil :type (or null fixnum))
  (params nil :type list)     ; ((name type) ...)
  (results nil :type list)    ; (type ...)
  (locals nil :type list)     ; ((name type) ...)
  (body nil :type list))      ; Instructions

;;; ============================================================
;;; S-expression to AST Conversion
;;; ============================================================

(defun parse-expr (form)
  "Parse an S-expression into an AST node."
  (cond
    ;; NIL literal
    ((null form)
     (make-nil-literal))
    ;; T literal
    ((eq form t)
     (make-t-literal))
    ;; Fixnum literal
    ((integerp form)
     (make-fixnum-literal form))
    ;; Symbol (variable reference)
    ((symbolp form)
     (make-var-ref form))
    ;; List (compound form)
    ((consp form)
     (parse-compound-form form))
    ;; Other atoms
    (t
     (make-ast-literal :value form :literal-type :unknown))))

(defun parse-compound-form (form)
  "Parse a compound (list) form."
  (let ((op (car form))
        (args (cdr form)))
    (case op
      ;; Special forms
      (if (parse-if-form args))
      (let (parse-let-form args nil))
      (let* (parse-let-form args t))
      (progn (parse-progn-form args))
      (defun (parse-defun-form args))
      (lambda (parse-lambda-form args))
      (setq (parse-setq-form args))
      (quote (make-ast-literal :value (car args) :literal-type :quoted))
      (block (parse-block-form args))
      (return-from (parse-return-from-form args))
      (flet (parse-flet-form args))
      (labels (parse-labels-form args))
      ;; Tagbody/go
      (tagbody (parse-tagbody-form args))
      (go (parse-go-form args))
      ;; Catch/throw
      (catch (parse-catch-form args))
      (throw (parse-throw-form args))
      ;; Unwind-protect
      (unwind-protect (parse-unwind-protect-form args))
      ;; Special variable declarations (T021)
      (defvar (parse-defvar-form args))
      (defparameter (parse-defparameter-form args))
      ;; Macros (expand inline for now)
      (when (parse-when-form args))
      (unless (parse-unless-form args))
      (cond (parse-cond-form args))
      (and (parse-and-form args))
      (or (parse-or-form args))
      ;; Function call
      (otherwise
       (make-ast-call
        :function op
        :arguments (mapcar #'parse-expr args)
        :call-type (if (symbolp op) :named :funcall))))))

(defun parse-if-form (args)
  "Parse (if test then [else])."
  (make-ast-if
   :test (parse-expr (first args))
   :then (parse-expr (second args))
   :else (if (third args) (parse-expr (third args)) (make-nil-literal))))

(defun parse-let-form (args sequential-p)
  "Parse (let/let* bindings body...)."
  (let ((bindings (first args))
        (body (rest args)))
    (make-ast-let
     :bindings (mapcar (lambda (b)
                         (if (consp b)
                             (cons (first b) (parse-expr (second b)))
                             (cons b (make-nil-literal))))
                       bindings)
     :body (mapcar #'parse-expr body)
     :sequential-p sequential-p)))

(defun parse-progn-form (args)
  "Parse (progn forms...)."
  (make-ast-progn :forms (mapcar #'parse-expr args)))

(defun parse-defun-form (args)
  "Parse (defun name params body...)."
  (let ((name (first args))
        (params (second args))
        (body (cddr args)))
    ;; Check for docstring
    (multiple-value-bind (docstring actual-body)
        (if (and (stringp (car body)) (cdr body))
            (values (car body) (cdr body))
            (values nil body))
      (make-ast-defun
       :name name
       :parameters params
       :body (mapcar #'parse-expr actual-body)
       :docstring docstring))))

(defun parse-lambda-form (args)
  "Parse (lambda params body...)."
  (let ((params (first args))
        (body (rest args)))
    (make-ast-lambda
     :parameters params
     :body (mapcar #'parse-expr body))))

(defun parse-setq-form (args)
  "Parse (setq var value)."
  (make-ast-setq
   :name (first args)
   :value (parse-expr (second args))))

(defun parse-block-form (args)
  "Parse (block name body...)."
  (make-ast-block
   :name (first args)
   :body (mapcar #'parse-expr (rest args))))

(defun parse-return-from-form (args)
  "Parse (return-from name [value])."
  (make-ast-return-from
   :block-name (first args)
   :value (if (second args) (parse-expr (second args)) (make-nil-literal))))

(defun parse-flet-form (args)
  "Parse (flet ((name params body...) ...) forms...)."
  (let ((definitions (first args))
        (body (rest args)))
    (make-ast-flet
     :definitions (mapcar #'parse-local-function-def definitions)
     :body (mapcar #'parse-expr body))))

(defun parse-labels-form (args)
  "Parse (labels ((name params body...) ...) forms...)."
  (let ((definitions (first args))
        (body (rest args)))
    (make-ast-labels
     :definitions (mapcar #'parse-local-function-def definitions)
     :body (mapcar #'parse-expr body))))

(defun parse-local-function-def (def)
  "Parse a local function definition (name params body...).
   Returns (name params parsed-body...) where body is list of AST nodes."
  (let ((name (first def))
        (params (second def))
        (body (cddr def)))
    (list name params (mapcar #'parse-expr body))))

;;; Macro expansions

(defun parse-when-form (args)
  "Expand (when test body...) to (if test (progn body...) nil)."
  (make-ast-if
   :test (parse-expr (first args))
   :then (make-ast-progn :forms (mapcar #'parse-expr (rest args)))
   :else (make-nil-literal)))

(defun parse-unless-form (args)
  "Expand (unless test body...) to (if test nil (progn body...))."
  (make-ast-if
   :test (parse-expr (first args))
   :then (make-nil-literal)
   :else (make-ast-progn :forms (mapcar #'parse-expr (rest args)))))

(defun parse-cond-form (clauses)
  "Expand (cond clauses...) to nested if."
  (if (null clauses)
      (make-nil-literal)
      (let ((clause (first clauses)))
        (make-ast-if
         :test (parse-expr (first clause))
         :then (if (rest clause)
                   (make-ast-progn :forms (mapcar #'parse-expr (rest clause)))
                   (parse-expr (first clause)))
         :else (parse-cond-form (rest clauses))))))

(defun parse-and-form (args)
  "Expand (and forms...) to nested if."
  (cond
    ((null args) (make-t-literal))
    ((null (cdr args)) (parse-expr (car args)))
    (t (make-ast-if
        :test (parse-expr (car args))
        :then (parse-and-form (cdr args))
        :else (make-nil-literal)))))

(defun parse-or-form (args)
  "Expand (or forms...) to nested if with temp variable."
  (cond
    ((null args) (make-nil-literal))
    ((null (cdr args)) (parse-expr (car args)))
    (t
     ;; TODO: Use a temp variable to avoid double evaluation
     ;; For now, simplified version
     (make-ast-if
      :test (parse-expr (car args))
      :then (parse-expr (car args))  ; Double eval, fix later
      :else (parse-or-form (cdr args))))))

;;; ============================================================
;;; Tagbody/Go Parsing
;;; ============================================================

(defun parse-tagbody-form (args)
  "Parse (tagbody {tag | form}*).
   Returns ast-tagbody with tags list and segments alist."
  (let ((tags '())
        (segments '())
        (current-tag nil)
        (current-forms '()))
    ;; Process each element
    (dolist (elem args)
      (if (and (atom elem) (symbolp elem) (not (null elem)))
          ;; It's a tag
          (progn
            ;; Save previous segment
            (when (or current-forms current-tag)
              (push (cons current-tag (nreverse current-forms)) segments))
            ;; Start new segment
            (push elem tags)
            (setf current-tag elem)
            (setf current-forms nil))
          ;; It's a form
          (push (parse-expr elem) current-forms)))
    ;; Save final segment
    (push (cons current-tag (nreverse current-forms)) segments)
    (make-ast-tagbody
     :tags (nreverse tags)
     :segments (nreverse segments))))

(defun parse-go-form (args)
  "Parse (go tag)."
  (make-ast-go :tag (first args)))

;;; ============================================================
;;; Catch/Throw Parsing
;;; ============================================================

(defun parse-catch-form (args)
  "Parse (catch tag forms...)."
  (make-ast-catch
   :tag (parse-expr (first args))
   :body (mapcar #'parse-expr (rest args))))

(defun parse-throw-form (args)
  "Parse (throw tag value)."
  (make-ast-throw
   :tag (parse-expr (first args))
   :value (if (second args)
              (parse-expr (second args))
              (make-nil-literal))))

;;; ============================================================
;;; Unwind-Protect Parsing
;;; ============================================================

(defun parse-unwind-protect-form (args)
  "Parse (unwind-protect protected-form cleanup-forms...)."
  (make-ast-unwind-protect
   :protected-form (parse-expr (first args))
   :cleanup-forms (mapcar #'parse-expr (rest args))))

;;; ============================================================
;;; Special Variable Definition Parsing (T019-T020)
;;; ============================================================

(defun parse-defvar-form (args)
  "Parse (defvar name [init-form [docstring]]) (T019).
   Registers the symbol as special and creates an AST node."
  (let* ((name (first args))
         (init-form (second args))
         (docstring (third args)))
    ;; Register as special variable
    (clysm/compiler/env:register-special-variable
     name :has-init-form (not (null init-form)))
    ;; Create AST node
    (make-ast-defvar
     :name name
     :init-form (when init-form (parse-expr init-form))
     :docstring docstring)))

(defun parse-defparameter-form (args)
  "Parse (defparameter name init-form [docstring]) (T020).
   Unlike defvar, defparameter requires an initial value."
  (let* ((name (first args))
         (init-form (second args))
         (docstring (third args)))
    ;; Error if no init-form (defparameter requires it)
    (unless init-form
      (error "DEFPARAMETER requires an initial value: ~S" name))
    ;; Register as special variable
    (clysm/compiler/env:register-special-variable
     name :has-init-form t)
    ;; Create AST node
    (make-ast-defparameter
     :name name
     :init-form (parse-expr init-form)
     :docstring docstring)))
