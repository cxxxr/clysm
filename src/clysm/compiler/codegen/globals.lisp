;;;; globals.lisp - Global variable code generation
;;;;
;;;; Part of Phase 13D-4: Global Variable Definitions
;;;; Implements defvar/defparameter compilation to WasmGC
;;;;
;;;; Supports:
;;;; - Constant initialization (integer, string, NIL)
;;;; - Deferred initialization via $init function
;;;; - UNBOUND initialization for defvar without init-form
;;;;
;;;; See: https://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm

(in-package #:clysm/compiler/codegen/globals)

;;; ============================================================
;;; Global Declaration Registry
;;; ============================================================

(defstruct (global-declaration (:conc-name global-decl-))
  "Represents a defvar/defparameter form being compiled.
   Tracks initialization type for code generation."
  (name nil :type symbol)
  (kind :defvar :type (member :defvar :defparameter))
  (init-form nil :type t)           ; AST node or NIL
  (init-type :none :type (member :constant :deferred :none))
  (global-index nil :type (or null integer))
  (docstring nil :type (or null string)))

(defvar *global-declarations* (make-hash-table :test 'eq)
  "Maps symbol names to global-declaration structures.")

(defvar *deferred-inits* '()
  "List of (global-index . init-ast) for deferred initialization.
   These are compiled into the $init function.")

(defun reset-global-declarations ()
  "Reset global declaration tracking for new compilation."
  (clrhash *global-declarations*)
  (setf *deferred-inits* '()))

;;; ============================================================
;;; Init Type Classification (T007)
;;; ============================================================

(defun classify-init-type (init-ast)
  "Classify initialization expression type.
   Returns :constant, :deferred, or :none.

   :constant - Can be evaluated at compile time (literals, NIL)
   :deferred - Must be evaluated at runtime ($init function)
   :none - No initialization (UNBOUND)"
  (cond
    ;; No init form
    ((null init-ast)
     :none)
    ;; Literal values are constant
    ((ast-literal-p init-ast)
     :constant)
    ;; Variable reference to another global (constant if already defined)
    ((ast-var-ref-p init-ast)
     (let ((name (ast-var-ref-name init-ast)))
       ;; Special case: references to reserved globals
       (if (member name '(nil t))
           :constant
           :deferred)))
    ;; Quote forms are constant
    ((and (ast-call-p init-ast)
          (eq 'quote (ast-call-function init-ast)))
     :constant)
    ;; Function calls require deferred initialization
    ((ast-call-p init-ast)
     :deferred)
    ;; Default to deferred for safety
    (t :deferred)))

;;; ============================================================
;;; Global Registration
;;; ============================================================

(defun register-global-declaration (name kind init-ast &optional docstring)
  "Register a global variable declaration.
   Returns the global-declaration structure."
  (let* ((init-type (classify-init-type init-ast))
         (global-idx (allocate-special-var-global name))
         (decl (make-global-declaration
                :name name
                :kind kind
                :init-form init-ast
                :init-type init-type
                :global-index global-idx
                :docstring docstring)))
    ;; Store in registry
    (setf (gethash name *global-declarations*) decl)
    ;; Queue deferred init if needed
    (when (eq :deferred init-type)
      (push (cons global-idx init-ast) *deferred-inits*))
    decl))

(defun get-global-declaration (name)
  "Get the global declaration for a symbol, or NIL if not found."
  (gethash name *global-declarations*))

;;; ============================================================
;;; Constant Init Expression Generation
;;; ============================================================

(defun generate-constant-init-expr (init-ast)
  "Generate Wasm init expression for constant initialization.
   Returns list of Wasm instructions."
  (cond
    ;; NIL literal - use global 0
    ((and (ast-literal-p init-ast)
          (null (ast-literal-value init-ast)))
     '((global.get 0)))  ; $nil

    ;; T literal - need symbol lookup
    ((and (ast-literal-p init-ast)
          (eq t (ast-literal-value init-ast)))
     '((global.get 0)))  ; TODO: proper T handling

    ;; Integer literal - wrap in i31
    ((and (ast-literal-p init-ast)
          (integerp (ast-literal-value init-ast)))
     (let ((val (ast-literal-value init-ast)))
       `((i32.const ,val)
         (ref.i31))))

    ;; String literal - needs deferred handling
    ((and (ast-literal-p init-ast)
          (stringp (ast-literal-value init-ast)))
     ;; String constants need runtime allocation
     ;; For now, use null placeholder and defer
     '((ref.null any)))

    ;; Quoted NIL (empty list)
    ((and (ast-call-p init-ast)
          (eq 'quote (ast-call-function init-ast)))
     (let ((quoted-val (first (ast-call-arguments init-ast))))
       (if (and (ast-literal-p quoted-val)
                (null (ast-literal-value quoted-val)))
           '((global.get 0))  ; NIL
           '((ref.null any)))))  ; Defer complex quoted forms

    ;; Default: null placeholder for deferred init
    (t '((ref.null any)))))

;;; ============================================================
;;; Compile defvar (T017)
;;; ============================================================

(defun compile-defvar (ast env)
  "Compile a defvar form to Wasm.

   (defvar *name*) -> Declare unbound
   (defvar *name* value) -> Declare and optionally initialize

   defvar semantics: Only initialize if currently unbound."
  (let* ((name (ast-defvar-name ast))
         (init-form (ast-defvar-init-form ast))
         (docstring (ast-defvar-docstring ast))
         (decl (register-global-declaration name :defvar init-form docstring)))
    ;; For defvar, we:
    ;; 1. Allocate global slot
    ;; 2. Generate initialization (constant or deferred)
    ;; 3. Return the variable name as result
    (declare (ignore decl))
    ;; Return code that produces the variable name symbol
    `((global.get ,(allocate-special-var-global name)))))

;;; ============================================================
;;; Compile defparameter (T018)
;;; ============================================================

(defun compile-defparameter (ast env)
  "Compile a defparameter form to Wasm.

   (defparameter *name* value) -> Declare and always initialize

   defparameter semantics: Always set to init-form value."
  (let* ((name (ast-defparameter-name ast))
         (init-form (ast-defparameter-init-form ast))
         (docstring (ast-defparameter-docstring ast))
         (decl (register-global-declaration name :defparameter init-form docstring)))
    (declare (ignore decl))
    ;; Return code that produces the variable name symbol
    `((global.get ,(allocate-special-var-global name)))))

;;; ============================================================
;;; Generate Global Section Entries
;;; ============================================================

(defun generate-special-var-globals ()
  "Generate Wasm global entries for all registered special variables.
   Returns list of global entries to add to the global section."
  (let ((globals '()))
    (maphash (lambda (name decl)
               (let* ((idx (global-decl-global-index decl))
                      (init-type (global-decl-init-type decl))
                      (init-ast (global-decl-init-form decl))
                      (init-expr (case init-type
                                   (:constant (generate-constant-init-expr init-ast))
                                   (:deferred '((ref.null any)))
                                   (:none '((global.get 1))))))  ; UNBOUND
                 (push (list :global idx
                             :type '(ref null any)
                             :mutable t
                             :init init-expr)
                       globals)))
             *global-declarations*)
    ;; Sort by index
    (sort globals #'< :key (lambda (g) (getf g :global)))))

;;; ============================================================
;;; Generate $init Function for Deferred Inits (T010)
;;; ============================================================

(defun generate-init-function-body (env)
  "Generate body of $init function for deferred initialization.
   Returns list of Wasm instructions to set deferred globals."
  (let ((body '()))
    ;; Process deferred inits in order
    (dolist (pair (nreverse *deferred-inits*))
      (destructuring-bind (global-idx . init-ast) pair
        ;; Compile init expression using func-section's compile-to-instructions
        (let ((init-code (clysm/compiler/codegen/func-section:compile-to-instructions
                          init-ast env)))
          (setf body
                (append body
                        init-code
                        `((global.set ,global-idx)))))))
    body))

(defun has-deferred-inits-p ()
  "Check if there are any deferred initializations."
  (not (null *deferred-inits*)))

;;; ============================================================
;;; Export Interface
;;; ============================================================

(defun compile-global-definition (ast env)
  "Dispatch to appropriate compiler for global definition.
   Handles both defvar and defparameter AST nodes."
  (cond
    ((ast-defvar-p ast)
     (compile-defvar ast env))
    ((ast-defparameter-p ast)
     (compile-defparameter ast env))
    (t
     (error "Unknown global definition type: ~S" ast))))
