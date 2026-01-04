;;;; compiler/ast.lisp - Abstract Syntax Tree Definitions
;;;;
;;;; Defines the AST nodes used by the Clysm compiler.
;;;; S-expressions are parsed into these structures before code generation.

(in-package #:clysm)

;;; ============================================================
;;; AST Node Types
;;; ============================================================

(defstruct (ast-node (:constructor nil))
  "Base type for all AST nodes."
  (source nil)  ; Source location for error reporting
  )

;;; ------------------------------------------------------------
;;; Literals
;;; ------------------------------------------------------------

(defstruct (ast-literal (:include ast-node)
                        (:constructor make-ast-literal (value &optional type)))
  "A literal value (number, string, character)."
  value    ; The literal value
  type)    ; :fixnum, :string, :character, or nil (inferred)

(defstruct (ast-quote (:include ast-node)
                      (:constructor make-ast-quote (value)))
  "A quoted form."
  value)   ; The quoted S-expression

;;; ------------------------------------------------------------
;;; Variables
;;; ------------------------------------------------------------

(defstruct (ast-var (:include ast-node)
                    (:constructor make-ast-var (name)))
  "A variable reference."
  name)    ; Symbol naming the variable

(defstruct (ast-setq (:include ast-node)
                     (:constructor make-ast-setq (name value)))
  "Variable assignment."
  name     ; Symbol naming the variable
  value)   ; AST node for the value

;;; ------------------------------------------------------------
;;; Control Flow
;;; ------------------------------------------------------------

(defstruct (ast-if (:include ast-node)
                   (:constructor make-ast-if (test then &optional else)))
  "Conditional expression."
  test     ; AST node for the test
  then     ; AST node for the then branch
  else)    ; AST node for the else branch (or nil)

(defstruct (ast-progn (:include ast-node)
                      (:constructor make-ast-progn (forms)))
  "Sequential execution."
  forms)   ; List of AST nodes

(defstruct (ast-block (:include ast-node)
                      (:constructor make-ast-block (name body)))
  "Named block for non-local return."
  name     ; Block name (symbol)
  body)    ; AST node for body (usually ast-progn)

(defstruct (ast-return-from (:include ast-node)
                            (:constructor make-ast-return-from (name value)))
  "Return from a named block."
  name     ; Block name to return from
  value)   ; AST node for return value

(defstruct (ast-tagbody (:include ast-node)
                        (:constructor make-ast-tagbody (segments)))
  "Tagbody for go-based control flow.
SEGMENTS is a list of (tag . forms) where tag is a symbol or nil."
  segments)  ; List of (tag . ast-progn) pairs

(defstruct (ast-go (:include ast-node)
                   (:constructor make-ast-go (tag)))
  "Go to a tag in a tagbody."
  tag)       ; Tag symbol to jump to

(defstruct (ast-catch (:include ast-node)
                      (:constructor make-ast-catch (tag body)))
  "Catch form for dynamic non-local exit."
  tag        ; AST node evaluating to catch tag
  body)      ; AST node for body

(defstruct (ast-throw (:include ast-node)
                      (:constructor make-ast-throw (tag value)))
  "Throw to a catch with matching tag."
  tag        ; AST node evaluating to throw tag
  value)     ; AST node for thrown value

(defstruct (ast-unwind-protect (:include ast-node)
                               (:constructor make-ast-unwind-protect (protected cleanup)))
  "Unwind-protect for guaranteed cleanup."
  protected  ; AST node for protected form
  cleanup)   ; AST node (progn) for cleanup forms

;;; ------------------------------------------------------------
;;; Binding Forms
;;; ------------------------------------------------------------

(defstruct (ast-let (:include ast-node)
                    (:constructor make-ast-let (bindings body &optional sequential-p)))
  "Local variable binding."
  bindings      ; List of (name . ast-node) pairs
  body          ; AST node for body (usually ast-progn)
  sequential-p) ; T for let*, nil for let

(defstruct (ast-lambda (:include ast-node)
                       (:constructor make-ast-lambda (params body &optional name)))
  "Lambda expression."
  params   ; List of parameter names (symbols)
  body     ; AST node for body
  name)    ; Optional name for debugging

;;; ------------------------------------------------------------
;;; Function Application
;;; ------------------------------------------------------------

(defstruct (ast-call (:include ast-node)
                     (:constructor make-ast-call (func args)))
  "Function call."
  func     ; AST node for the function (ast-var or ast-lambda)
  args)    ; List of AST nodes for arguments

(defstruct (ast-primitive-call (:include ast-node)
                               (:constructor make-ast-primitive-call (name args)))
  "Direct primitive call (optimization)."
  name     ; Primitive name (symbol)
  args)    ; List of AST nodes for arguments

;;; ------------------------------------------------------------
;;; Definitions
;;; ------------------------------------------------------------

(defstruct (ast-defun (:include ast-node)
                      (:constructor make-ast-defun (name params body)))
  "Function definition."
  name     ; Function name (symbol)
  params   ; Parameter list
  body)    ; AST node for body

(defstruct (ast-defvar (:include ast-node)
                       (:constructor make-ast-defvar (name value &optional special-p)))
  "Variable definition."
  name       ; Variable name (symbol)
  value      ; AST node for initial value (or nil)
  special-p) ; T if declared special

(defstruct (ast-defmacro (:include ast-node)
                         (:constructor make-ast-defmacro (name lambda-list body)))
  "Macro definition.
Note: Macro expansion happens in the host Lisp at compile time.
The AST node is used for tracking but macros are registered immediately."
  name         ; Macro name (symbol)
  lambda-list  ; Macro lambda list (may include &body, &rest, etc.)
  body)        ; List of body forms (unevaluated)

;;; ============================================================
;;; AST Utilities
;;; ============================================================

(defun ast-self-evaluating-p (node)
  "Check if NODE is self-evaluating (doesn't need compilation)."
  (and (ast-literal-p node)
       (let ((type (ast-literal-type node)))
         (member type '(:fixnum :character nil)))))

;;; ============================================================
;;; S-expression to AST Parser
;;; ============================================================

(defvar *special-forms* (make-hash-table :test 'eq)
  "Registry of special form parsers.")

(defvar *macros* (make-hash-table :test 'eq)
  "Registry of macro expander functions.
Each entry maps a symbol to a function that takes the macro call form
and returns the expanded form.")

(defun register-special-form (name parser)
  "Register a special form parser."
  (setf (gethash name *special-forms*) parser))

(defun special-form-p (name)
  "Check if NAME is a special form."
  (and (symbolp name)
       (gethash name *special-forms*)))

;;; ============================================================
;;; Macro System
;;; ============================================================

(defun register-macro (name expander)
  "Register a macro expander function.
EXPANDER is a function that takes the full macro call form and returns
the expanded form."
  (setf (gethash name *macros*) expander))

(defun unregister-macro (name)
  "Remove a macro definition."
  (remhash name *macros*))

(defun get-macro-expander (name)
  "Return the macro expander function for NAME, or NIL."
  (gethash name *macros*))

(defun macrop (name)
  "Check if NAME is a defined macro."
  (and (symbolp name)
       (gethash name *macros*)))

(defun clysm-macroexpand-1 (form)
  "Expand FORM once if it's a macro call.
Returns (values expanded-form expanded-p)."
  (if (and (consp form)
           (symbolp (car form))
           (macrop (car form)))
      (let ((expander (get-macro-expander (car form))))
        (values (funcall expander form) t))
      (values form nil)))

(defun clysm-macroexpand (form)
  "Repeatedly expand FORM until it's no longer a macro call.
Returns (values fully-expanded-form expanded-p)."
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (new-form did-expand)
          (clysm-macroexpand-1 form)
        (unless did-expand
          (return (values form expanded-p)))
        (setf form new-form
              expanded-p t)))))

(defun parse-sexp (form)
  "Parse an S-expression into an AST node."
  (cond
    ;; Self-evaluating atoms
    ((null form)
     (make-ast-quote nil))

    ((eq form t)
     (make-ast-quote t))

    ((integerp form)
     (make-ast-literal form :fixnum))

    ((characterp form)
     (make-ast-literal form :character))

    ((stringp form)
     (make-ast-literal form :string))

    ;; Symbols (variable references)
    ((symbolp form)
     (make-ast-var form))

    ;; Compound forms
    ((consp form)
     (let ((op (car form)))
       (cond
         ;; Macro expansion (before special forms to allow shadowing)
         ((and (symbolp op) (macrop op))
          (parse-sexp (clysm-macroexpand form)))

         ;; Special forms
         ((special-form-p op)
          (funcall (gethash op *special-forms*) form))

         ;; Primitive call optimization
         ((primitivep op)
          (make-ast-primitive-call op (mapcar #'parse-sexp (cdr form))))

         ;; Regular function call
         (t
          (make-ast-call (if (symbolp op)
                             (make-ast-var op)
                             (parse-sexp op))
                         (mapcar #'parse-sexp (cdr form)))))))

    (t
     (error "Cannot parse: ~S" form))))

;;; ============================================================
;;; Special Form Parsers
;;; ============================================================

(defun parse-quote (form)
  "Parse (quote x)."
  (unless (= (length form) 2)
    (error "QUOTE requires exactly one argument: ~S" form))
  (make-ast-quote (second form)))

(defun parse-if (form)
  "Parse (if test then [else])."
  (let ((len (length form)))
    (unless (<= 3 len 4)
      (error "IF requires 2 or 3 arguments: ~S" form))
    (make-ast-if (parse-sexp (second form))
                 (parse-sexp (third form))
                 (when (= len 4)
                   (parse-sexp (fourth form))))))

(defun parse-progn (form)
  "Parse (progn forms...)."
  (make-ast-progn (mapcar #'parse-sexp (cdr form))))

(defun parse-let (form)
  "Parse (let bindings body...)."
  (unless (>= (length form) 2)
    (error "LET requires bindings: ~S" form))
  (let ((bindings (second form))
        (body (cddr form)))
    (unless (listp bindings)
      (error "LET bindings must be a list: ~S" form))
    (make-ast-let
     (mapcar (lambda (binding)
               (cond
                 ((symbolp binding)
                  (cons binding (make-ast-quote nil)))
                 ((and (consp binding)
                       (symbolp (car binding))
                       (<= 1 (length binding) 2))
                  (cons (car binding)
                        (if (cdr binding)
                            (parse-sexp (second binding))
                            (make-ast-quote nil))))
                 (t
                  (error "Invalid LET binding: ~S" binding))))
             bindings)
     (make-ast-progn (mapcar #'parse-sexp body))
     nil)))  ; not sequential

(defun parse-let* (form)
  "Parse (let* bindings body...)."
  (unless (>= (length form) 2)
    (error "LET* requires bindings: ~S" form))
  (let ((bindings (second form))
        (body (cddr form)))
    (unless (listp bindings)
      (error "LET* bindings must be a list: ~S" form))
    (make-ast-let
     (mapcar (lambda (binding)
               (cond
                 ((symbolp binding)
                  (cons binding (make-ast-quote nil)))
                 ((and (consp binding)
                       (symbolp (car binding))
                       (<= 1 (length binding) 2))
                  (cons (car binding)
                        (if (cdr binding)
                            (parse-sexp (second binding))
                            (make-ast-quote nil))))
                 (t
                  (error "Invalid LET* binding: ~S" binding))))
             bindings)
     (make-ast-progn (mapcar #'parse-sexp body))
     t)))  ; sequential

(defun parse-lambda (form)
  "Parse (lambda params body...)."
  (unless (>= (length form) 2)
    (error "LAMBDA requires parameter list: ~S" form))
  (let ((params (second form))
        (body (cddr form)))
    (unless (listp params)
      (error "LAMBDA parameters must be a list: ~S" form))
    (unless (every #'symbolp params)
      (error "LAMBDA parameters must be symbols: ~S" form))
    (make-ast-lambda params
                     (make-ast-progn (mapcar #'parse-sexp body)))))

(defun parse-setq (form)
  "Parse (setq var value [var value]...)."
  (let ((pairs (cdr form)))
    (unless (and pairs (evenp (length pairs)))
      (error "SETQ requires variable-value pairs: ~S" form))
    (if (= (length pairs) 2)
        (make-ast-setq (first pairs) (parse-sexp (second pairs)))
        ;; Multiple assignments become progn
        (make-ast-progn
         (loop for (var val) on pairs by #'cddr
               collect (make-ast-setq var (parse-sexp val)))))))

(defun parse-block (form)
  "Parse (block name body...)."
  (unless (>= (length form) 2)
    (error "BLOCK requires a name: ~S" form))
  (let ((name (second form))
        (body (cddr form)))
    (unless (symbolp name)
      (error "BLOCK name must be a symbol: ~S" form))
    (make-ast-block name
                    (make-ast-progn (mapcar #'parse-sexp body)))))

(defun parse-return-from (form)
  "Parse (return-from name [value])."
  (let ((len (length form)))
    (unless (<= 2 len 3)
      (error "RETURN-FROM requires 1 or 2 arguments: ~S" form))
    (let ((name (second form))
          (value (if (= len 3) (third form) nil)))
      (unless (symbolp name)
        (error "RETURN-FROM name must be a symbol: ~S" form))
      (make-ast-return-from name (parse-sexp value)))))

(defun parse-defun (form)
  "Parse (defun name params body...)."
  (unless (>= (length form) 3)
    (error "DEFUN requires name and parameters: ~S" form))
  (let ((name (second form))
        (params (third form))
        (body (cdddr form)))
    (unless (symbolp name)
      (error "DEFUN name must be a symbol: ~S" form))
    (unless (listp params)
      (error "DEFUN parameters must be a list: ~S" form))
    (make-ast-defun name params
                    (make-ast-progn (mapcar #'parse-sexp body)))))

(defun parse-defvar (form)
  "Parse (defvar name [value])."
  (let ((len (length form)))
    (unless (<= 2 len 3)
      (error "DEFVAR requires 1 or 2 arguments: ~S" form))
    (let ((name (second form))
          (value (when (= len 3) (third form))))
      (unless (symbolp name)
        (error "DEFVAR name must be a symbol: ~S" form))
      (make-ast-defvar name
                       (when value (parse-sexp value))
                       t))))

(defun parse-defmacro (form)
  "Parse (defmacro name lambda-list body...).
Registers the macro expander in the host Lisp for compile-time expansion."
  (unless (>= (length form) 3)
    (error "DEFMACRO requires name and lambda-list: ~S" form))
  (let ((name (second form))
        (lambda-list (third form))
        (body (cdddr form)))
    (unless (symbolp name)
      (error "DEFMACRO name must be a symbol: ~S" form))
    (unless (listp lambda-list)
      (error "DEFMACRO lambda-list must be a list: ~S" form))

    ;; Create and register the macro expander function
    ;; The expander takes the whole form and destructures it
    (let ((expander (make-macro-expander name lambda-list body)))
      (register-macro name expander))

    ;; Return AST node for tracking
    (make-ast-defmacro name lambda-list body)))

(defun make-macro-expander (name lambda-list body)
  "Create a macro expander function for DEFMACRO.
Returns a function that takes a macro call form and returns the expansion."
  (declare (ignore name))
  ;; Build a lambda that destructures the arguments and evaluates the body
  ;; We use &whole to get the whole form, then skip the macro name
  (let* ((whole-var (gensym "WHOLE-"))
         (expander-lambda
           `(lambda (,whole-var)
              (destructuring-bind ,lambda-list (cdr ,whole-var)
                ,@body))))
    ;; Evaluate in host Lisp to create the function
    (eval expander-lambda)))

(defun parse-tagbody (form)
  "Parse (tagbody {tag | form}*)."
  (let ((body (cdr form))
        (segments nil)
        (current-tag nil)
        (current-forms nil))
    ;; Process body elements
    (dolist (elem body)
      (if (or (symbolp elem) (integerp elem))
          ;; It's a tag
          (progn
            ;; Save previous segment if any
            (when (or current-tag current-forms)
              (push (cons current-tag
                          (make-ast-progn (nreverse current-forms)))
                    segments))
            (setf current-tag elem)
            (setf current-forms nil))
          ;; It's a form
          (push (parse-sexp elem) current-forms)))
    ;; Save final segment
    (when (or current-tag current-forms)
      (push (cons current-tag
                  (make-ast-progn (nreverse current-forms)))
            segments))
    (make-ast-tagbody (nreverse segments))))

(defun parse-go (form)
  "Parse (go tag)."
  (unless (= (length form) 2)
    (error "GO requires exactly one argument: ~S" form))
  (let ((tag (second form)))
    (unless (or (symbolp tag) (integerp tag))
      (error "GO tag must be a symbol or integer: ~S" form))
    (make-ast-go tag)))

(defun parse-catch (form)
  "Parse (catch tag body...)."
  (unless (>= (length form) 2)
    (error "CATCH requires a tag: ~S" form))
  (make-ast-catch (parse-sexp (second form))
                  (make-ast-progn (mapcar #'parse-sexp (cddr form)))))

(defun parse-throw (form)
  "Parse (throw tag value)."
  (unless (= (length form) 3)
    (error "THROW requires exactly two arguments: ~S" form))
  (make-ast-throw (parse-sexp (second form))
                  (parse-sexp (third form))))

(defun parse-unwind-protect (form)
  "Parse (unwind-protect protected-form cleanup-forms...)."
  (unless (>= (length form) 2)
    (error "UNWIND-PROTECT requires a protected form: ~S" form))
  (make-ast-unwind-protect (parse-sexp (second form))
                           (make-ast-progn (mapcar #'parse-sexp (cddr form)))))

;;; ============================================================
;;; Register Special Forms
;;; ============================================================

(defun init-special-forms ()
  "Initialize special form registry."
  (clrhash *special-forms*)
  (register-special-form 'quote #'parse-quote)
  (register-special-form 'if #'parse-if)
  (register-special-form 'progn #'parse-progn)
  (register-special-form 'let #'parse-let)
  (register-special-form 'let* #'parse-let*)
  (register-special-form 'lambda #'parse-lambda)
  (register-special-form 'setq #'parse-setq)
  (register-special-form 'block #'parse-block)
  (register-special-form 'return-from #'parse-return-from)
  (register-special-form 'defun #'parse-defun)
  (register-special-form 'defvar #'parse-defvar)
  (register-special-form 'defmacro #'parse-defmacro)
  (register-special-form 'tagbody #'parse-tagbody)
  (register-special-form 'go #'parse-go)
  (register-special-form 'catch #'parse-catch)
  (register-special-form 'throw #'parse-throw)
  (register-special-form 'unwind-protect #'parse-unwind-protect)
  *special-forms*)

;; Initialize on load
(init-special-forms)
