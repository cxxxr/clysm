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
  "Literal value node (fixnum, string, character, T, NIL, etc.)"
  (value nil :type t)
  (literal-type nil :type keyword))  ; :fixnum, :string, :character, :nil, :t, :keyword

(defun make-fixnum-literal (value)
  "Create a fixnum literal AST node"
  (make-ast-literal :value value :literal-type :fixnum))

(defun make-nil-literal ()
  "Create a NIL literal AST node"
  (make-ast-literal :value nil :literal-type :nil))

(defun make-t-literal ()
  "Create a T literal AST node"
  (make-ast-literal :value t :literal-type :t))

;;; Character literal support (008-character-string)
(defun make-character-literal (value)
  "Create a character literal AST node"
  (make-ast-literal :value value :literal-type :character))

;;; String literal support (008-character-string)
(defun make-string-literal (value)
  "Create a string literal AST node"
  (make-ast-literal :value value :literal-type :string))

;;; ============================================================
;;; Numeric Tower Literal Support (010-numeric-tower)
;;; ============================================================

(defconstant +i31-min+ -1073741824 "Minimum value for i31ref (fixnum)")
(defconstant +i31-max+ 1073741823 "Maximum value for i31ref (fixnum)")

(defun i31-range-p (value)
  "Check if value fits in i31ref range (-2^30 to 2^30-1)"
  (and (integerp value)
       (<= +i31-min+ value +i31-max+)))

(defun make-bignum-literal (value)
  "Create a bignum literal AST node"
  (make-ast-literal :value value :literal-type :bignum))

(defun make-ratio-literal (value)
  "Create a ratio literal AST node"
  (make-ast-literal :value value :literal-type :ratio))

(defun make-float-literal (value)
  "Create a float literal AST node"
  (make-ast-literal :value value :literal-type :float))

(defun make-complex-literal (value)
  "Create a complex literal AST node"
  (make-ast-literal :value value :literal-type :complex))

(defun make-integer-literal (value)
  "Create an integer literal, choosing fixnum or bignum based on range"
  (if (i31-range-p value)
      (make-fixnum-literal value)
      (make-bignum-literal value)))

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
;;; Function Reference (043-self-hosting-blockers)
;;; ============================================================

(defstruct (ast-function (:include ast-node) (:conc-name ast-function-))
  "Function reference node for (function name) or #'name.
   Can reference either a named function or a lambda."
  (name nil :type (or symbol ast-lambda))  ; Function name or lambda AST
  (local-p nil :type boolean))              ; T if from flet/labels

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
;;; Parameter Info (043-self-hosting-blockers)
;;; ============================================================

(defstruct (ast-param-info (:conc-name ast-param-info-))
  "Structured parameter information for &optional and &key with defaults.
   Feature: 043-self-hosting-blockers

   Slots:
   - name: The parameter variable name (symbol)
   - kind: One of :required, :optional, :key, :rest, :aux
   - default-form: The default value expression (AST node or raw form), or NIL
   - supplied-p: The supplied-p variable name (symbol), or NIL
   - keyword: For &key, the keyword symbol (defaults to name upcased), or NIL"
  (name nil :type symbol)
  (kind :required :type (member :required :optional :key :rest :aux))
  (default-form nil :type t)
  (supplied-p nil :type (or null symbol))
  (keyword nil :type (or null symbol)))

(defstruct (keyword-arg-info (:conc-name keyword-arg-info-))
  "Information about a keyword argument in a function call.
   Feature: 043-self-hosting-blockers

   Used to track :test, :key, :start, :end, :from-end in function calls."
  (keyword nil :type symbol)
  (value nil :type t))

(defstruct (ast-parsed-lambda-list (:conc-name ast-parsed-lambda-list-))
  "Parsed lambda list with structured parameter information.
   Feature: 043-self-hosting-blockers"
  (required nil :type list)    ; List of ast-param-info
  (optional nil :type list)    ; List of ast-param-info
  (rest nil :type (or null ast-param-info))
  (keys nil :type list)        ; List of ast-param-info
  (allow-other-keys nil :type boolean)
  (aux nil :type list))        ; List of ast-param-info

(defun parse-lambda-list (lambda-list)
  "Parse a lambda list into an ast-parsed-lambda-list structure.
   Feature: 043-self-hosting-blockers

   Handles:
   - Required parameters: (a b c)
   - &optional with defaults: (&optional (x 10) (y 20 y-p))
   - &rest: (&rest args)
   - &key with defaults: (&key ((:keyword var) default supplied-p))
   - &allow-other-keys
   - &aux: (&aux (x 1))

   Returns an ast-parsed-lambda-list structure."
  (let ((required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (allow-other-keys nil)
        (aux nil)
        (state :required))
    (dolist (item lambda-list)
      (cond
        ;; Lambda list keywords
        ((eq item '&optional) (setf state :optional))
        ((eq item '&rest) (setf state :rest))
        ((eq item '&body) (setf state :rest))  ; &body is like &rest
        ((eq item '&key) (setf state :key))
        ((eq item '&allow-other-keys) (setf allow-other-keys t))
        ((eq item '&aux) (setf state :aux))
        ;; Skip &environment and &whole (handled by macro system)
        ((member item '(&environment &whole)) nil)
        ;; Process parameters based on current state
        (t
         (case state
           (:required
            (push (make-ast-param-info :name item :kind :required) required))
           (:optional
            (push (parse-optional-param item) optional))
           (:rest
            (setf rest (make-ast-param-info :name item :kind :rest)))
           (:key
            (push (parse-key-param item) keys))
           (:aux
            (push (parse-aux-param item) aux))))))
    (make-ast-parsed-lambda-list
     :required (nreverse required)
     :optional (nreverse optional)
     :rest rest
     :keys (nreverse keys)
     :allow-other-keys allow-other-keys
     :aux (nreverse aux))))

(defun parse-optional-param (param)
  "Parse an &optional parameter specification.
   PARAM can be:
   - symbol: just the name
   - (name): same as symbol
   - (name default): with default value
   - (name default supplied-p): with default and supplied-p"
  (cond
    ((symbolp param)
     (make-ast-param-info :name param :kind :optional))
    ((consp param)
     (let ((name (first param))
           (default (second param))
           (supplied-p (third param)))
       (make-ast-param-info
        :name name
        :kind :optional
        :default-form default
        :supplied-p supplied-p)))
    (t (error "Invalid &optional parameter: ~S" param))))

(defun parse-key-param (param)
  "Parse a &key parameter specification.
   PARAM can be:
   - symbol: name and keyword are the same
   - (name): same as symbol
   - (name default): with default value
   - (name default supplied-p): with default and supplied-p
   - ((:keyword name)): explicit keyword
   - ((:keyword name) default): with default
   - ((:keyword name) default supplied-p): full form"
  (cond
    ((symbolp param)
     (make-ast-param-info :name param :kind :key
                          :keyword (intern (symbol-name param) :keyword)))
    ((consp param)
     (let* ((first (first param))
            (has-explicit-keyword (and (consp first) (keywordp (car first)))))
       (if has-explicit-keyword
           ;; ((:keyword name) default supplied-p)
           (let ((keyword (first first))
                 (name (second first))
                 (default (second param))
                 (supplied-p (third param)))
             (make-ast-param-info
              :name name
              :kind :key
              :keyword keyword
              :default-form default
              :supplied-p supplied-p))
           ;; (name default supplied-p)
           (let ((name first)
                 (default (second param))
                 (supplied-p (third param)))
             (make-ast-param-info
              :name name
              :kind :key
              :keyword (intern (symbol-name name) :keyword)
              :default-form default
              :supplied-p supplied-p)))))
    (t (error "Invalid &key parameter: ~S" param))))

(defun parse-aux-param (param)
  "Parse an &aux parameter specification.
   PARAM can be:
   - symbol: just the name (initialized to NIL)
   - (name value): with initial value"
  (cond
    ((symbolp param)
     (make-ast-param-info :name param :kind :aux))
    ((consp param)
     (make-ast-param-info
      :name (first param)
      :kind :aux
      :default-form (second param)))
    (t (error "Invalid &aux parameter: ~S" param))))

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
;;; Exception Handling (001-control-structure-extension US4)
;;; ============================================================

;; HyperSpec: resources/HyperSpec/Body/m_hand_1.htm
(defstruct handler-clause
  "A single handler clause in handler-case.
   Represents (type ([var]) . body) where:
   - type: condition type specifier (symbol or compound type)
   - var: binding variable for the condition (or nil if unused)
   - body: handler body forms (list of AST nodes)"
  (type nil :type t)                  ; Condition type specifier
  (var nil :type (or null symbol))    ; Binding variable (or nil if unused)
  (body nil :type list))              ; Handler body forms (AST nodes)

(defstruct (ast-handler-case (:include ast-node) (:conc-name ast-handler-case-))
  "Handler-case node for exception handling with typed handlers.
   (handler-case expression {(type ([var]) . body)}*)
   Compiled to Wasm try_table/catch for direct exception handling."
  (expression nil :type t)            ; Protected expression (AST node)
  (handlers nil :type list))          ; List of handler-clause structures

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

;;; Feature 038: Constant definitions
(defstruct (ast-defconstant (:include ast-node) (:conc-name ast-defconstant-))
  "Defconstant node for defining compile-time constants.
   Constants are immutable and their values are computed at compile time.
   Used for configuration values, type indices, and opcodes in compiler source."
  (name nil :type symbol)
  (value-form nil :type t)  ; AST node representing the value expression
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
;;; Macro Introspection (016-macro-system T044-T045)
;;; ============================================================

(defstruct (ast-macroexpand-1 (:include ast-node) (:conc-name ast-macroexpand-1-))
  "Macroexpand-1 node for single macro expansion.
   (macroexpand-1 form) - Expand form once if it's a macro call."
  (form nil :type t))   ; AST node for form to expand

(defstruct (ast-macroexpand (:include ast-node) (:conc-name ast-macroexpand-))
  "Macroexpand node for full macro expansion.
   (macroexpand form) - Repeatedly expand form until not a macro call."
  (form nil :type t))   ; AST node for form to expand

(defstruct (ast-macro-function (:include ast-node) (:conc-name ast-macro-function-))
  "Macro-function node for compile-time macro function lookup.
   (macro-function 'name) - Returns the macro expander function or NIL.
   Feature 042: Advanced Defmacro.
   Note: At compile time, if the name is a quoted symbol, the lookup
   happens immediately and returns a quoted result (closure or NIL).
   For runtime lookup, the form compiles to code that queries the registry."
  (name nil :type t)    ; AST node for the macro name (usually quoted symbol)
  (env nil :type t))    ; AST node for optional environment (usually NIL)

;;; ============================================================
;;; Multiple Values (025-multiple-values)
;;; ============================================================

(defstruct (ast-values (:include ast-node) (:conc-name ast-values-))
  "Values node for returning multiple values.
   (values) -> NIL with mv-count=0
   (values x) -> x with mv-count=1
   (values x y z) -> x on stack, y z in mv-buffer, mv-count=3"
  (forms nil :type list))   ; List of AST nodes for values

(defstruct (ast-multiple-value-bind (:include ast-node) (:conc-name ast-mvb-))
  "Multiple-value-bind node for receiving multiple values.
   (multiple-value-bind (a b c) values-form body...)
   Binds variables a, b, c to the values from values-form, then executes body."
  (vars nil :type list)        ; List of variable symbols to bind
  (values-form nil :type t)    ; AST node producing the values
  (body nil :type list))       ; Body forms (AST nodes)

(defstruct (ast-multiple-value-list (:include ast-node) (:conc-name ast-mvl-))
  "Multiple-value-list node for collecting values into a list.
   (multiple-value-list form) -> (v1 v2 v3 ...)"
  (form nil :type t))          ; AST node producing values

(defstruct (ast-nth-value (:include ast-node) (:conc-name ast-nth-value-))
  "Nth-value node for accessing a specific value by index.
   (nth-value n form) -> nth value or NIL if out of range"
  (index nil :type t)          ; AST node for index expression
  (form nil :type t))          ; AST node producing values

(defstruct (ast-values-list (:include ast-node) (:conc-name ast-values-list-))
  "Values-list node for spreading a list as multiple values.
   (values-list list) -> (values (car list) (cadr list) ...)"
  (form nil :type t))          ; AST node for the list expression

(defstruct (ast-multiple-value-prog1 (:include ast-node) (:conc-name ast-mvp1-))
  "Multiple-value-prog1 node for preserving values through side effects.
   (multiple-value-prog1 first-form body...) -> values of first-form"
  (first-form nil :type t)     ; AST node whose values are preserved
  (body nil :type list))       ; Side-effect forms (AST nodes)

(defstruct (ast-multiple-value-call (:include ast-node) (:conc-name ast-mvc-))
  "Multiple-value-call node for passing all values to a function.
   (multiple-value-call fn form1 form2 ...) -> (apply fn (all-values))"
  (function nil :type t)       ; Function to call (AST node)
  (forms nil :type list))      ; Forms producing values (AST nodes)

;;; ============================================================
;;; CLOS Support (026-clos-foundation)
;;; ============================================================

(defstruct (ast-slot-definition (:conc-name ast-slot-definition-))
  "Slot definition for CLOS defclass.
   Captures :initarg, :accessor, :initform slot options."
  (name nil :type symbol)                     ; Slot name
  (initarg nil :type (or null keyword))       ; :initarg keyword
  (accessor nil :type (or null symbol))       ; :accessor function name
  (initform nil :type t)                      ; :initform value (AST node or raw value)
  (initform-p nil :type boolean))             ; T if :initform was specified

(defstruct (ast-defclass (:include ast-node) (:conc-name ast-defclass-))
  "Defclass node for CLOS class definition.
   (defclass name (superclass) ((slot-spec)...))"
  (name nil :type symbol)                     ; Class name
  (superclass nil :type (or null symbol))     ; Single superclass (or nil)
  (slots nil :type list))                     ; List of ast-slot-definition

(defun parse-slot-option (slot-spec key)
  "Extract a slot option value from slot-spec.
   Returns (values value found-p)."
  (let ((tail (member key slot-spec)))
    (if tail
        (values (second tail) t)
        (values nil nil))))

(defun parse-slot-definition (slot-spec)
  "Parse a slot specifier into an ast-slot-definition.
   SLOT-SPEC is either a symbol or (name :initarg :x :accessor foo :initform val)."
  (if (symbolp slot-spec)
      ;; Simple slot: just a name
      (make-ast-slot-definition :name slot-spec)
      ;; Complex slot: (name options...)
      (let ((name (first slot-spec)))
        (multiple-value-bind (initarg initarg-p) (parse-slot-option slot-spec :initarg)
          (declare (ignore initarg-p))
          (multiple-value-bind (accessor accessor-p) (parse-slot-option slot-spec :accessor)
            (declare (ignore accessor-p))
            (multiple-value-bind (initform initform-p) (parse-slot-option slot-spec :initform)
              (make-ast-slot-definition
               :name name
               :initarg initarg
               :accessor accessor
               :initform initform
               :initform-p initform-p)))))))

(defun parse-defclass-to-ast (form)
  "Parse a defclass form into an ast-defclass node.
   FORM is (defclass name (supers...) (slots...) options...).
   Only single inheritance is supported."
  (destructuring-bind (defclass-sym name supers slots &rest options) form
    (declare (ignore defclass-sym options))  ; class options ignored for now
    ;; Validate single inheritance
    (when (> (length supers) 1)
      (error "Multiple inheritance not supported: ~S" supers))
    (make-ast-defclass
     :name name
     :superclass (first supers)
     :slots (mapcar #'parse-slot-definition slots))))

;;; ============================================================
;;; CLOS make-instance Support (026-clos-foundation)
;;; ============================================================

(defstruct (ast-make-instance (:include ast-node) (:conc-name ast-make-instance-))
  "Make-instance node for CLOS instance creation.
   (make-instance 'class-name :initarg1 val1 :initarg2 val2 ...)"
  (class-name nil :type symbol)              ; Class name (unquoted)
  (initargs nil :type list))                 ; List of (keyword . value-ast) pairs

(defun parse-make-instance-to-ast (form)
  "Parse a make-instance form into an ast-make-instance node.
   FORM is (make-instance 'class-name :key1 val1 :key2 val2 ...)."
  (let ((class-form (second form))
        (initarg-plist (cddr form)))
    ;; Extract the class name from the quoted form
    (let ((class-name (if (and (consp class-form)
                               (eq (car class-form) 'quote))
                          (second class-form)
                          (error "make-instance class must be quoted: ~S" class-form))))
      ;; Parse initarg pairs
      (let ((initargs (loop for (key val) on initarg-plist by #'cddr
                            collect (cons key (parse-expr val)))))
        (make-ast-make-instance
         :class-name class-name
         :initargs initargs)))))

;;; ============================================================
;;; CLOS defmethod Support (026-clos-foundation)
;;; ============================================================

(defstruct (ast-defmethod (:include ast-node) (:conc-name ast-defmethod-))
  "Defmethod node for CLOS method definition.
   (defmethod name [qualifier] ((param class) ...) body...)"
  (name nil :type symbol)                    ; Generic function name
  (qualifier nil :type (or null keyword))    ; :before, :after, :around, or nil for primary
  (specializers nil :type list)              ; List of class names for specialization
  (lambda-list nil :type list)               ; Original parameter names
  (body nil :type list))                     ; Method body (list of AST nodes)

(defun parse-specialized-lambda-list (lambda-list)
  "Parse a specialized lambda-list and extract parameter names and specializers.
   Returns (values param-names specializers).
   Example: ((a animal) (b number)) -> ((A B) (ANIMAL NUMBER))"
  (let ((params nil)
        (specializers nil))
    (dolist (param lambda-list)
      (if (consp param)
          (progn
            (push (first param) params)
            (push (second param) specializers))
          (progn
            (push param params)
            (push t specializers))))  ; T means no specialization
    (values (nreverse params) (nreverse specializers))))

(defun parse-defmethod-to-ast (form)
  "Parse a defmethod form into an ast-defmethod node.
   FORM is (defmethod name [qualifier] lambda-list body...)."
  (let ((name (second form))
        (rest (cddr form)))
    ;; Check for optional qualifier
    (let ((qualifier nil)
          (lambda-list nil)
          (body nil))
      (if (keywordp (first rest))
          (progn
            (setf qualifier (first rest))
            (setf lambda-list (second rest))
            (setf body (cddr rest)))
          (progn
            (setf lambda-list (first rest))
            (setf body (cdr rest))))
      (multiple-value-bind (params specializers)
          (parse-specialized-lambda-list lambda-list)
        (make-ast-defmethod
         :name name
         :qualifier qualifier
         :specializers specializers
         :lambda-list params
         :body (mapcar #'parse-expr body))))))

;;; ============================================================
;;; FFI Call Support (027-complete-ffi)
;;; ============================================================

(defstruct (ast-ffi-call (:include ast-node) (:conc-name ast-ffi-call-))
  "AST node for FFI function calls (statically declared via define-foreign-function).
   The declaration field references the foreign-function-decl from the FFI environment.
   Arguments are parsed AST nodes that will be marshalled at codegen time."
  (declaration nil :type t)           ; Reference to ForeignFunctionDecl
  (arguments nil :type list))         ; List of argument AST nodes

(defstruct (ast-call-host (:include ast-node) (:conc-name ast-call-host-))
  "AST node for dynamic host function calls (ffi:call-host).
   The function-name is an AST node (string expression) evaluated at runtime.
   Arguments are marshalled based on their runtime types."
  (function-name nil :type t)         ; AST node for function name expression (string)
  (arguments nil :type list))         ; List of argument AST nodes

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
    ;; Integer literal - choose fixnum or bignum based on range (010-numeric-tower)
    ((integerp form)
     (make-integer-literal form))
    ;; Ratio literal (010-numeric-tower) - use typep since ratiop is not standard CL
    ((typep form 'ratio)
     (make-ratio-literal form))
    ;; Float literal (010-numeric-tower)
    ((floatp form)
     (make-float-literal form))
    ;; Complex literal (010-numeric-tower)
    ((complexp form)
     (make-complex-literal form))
    ;; Character literal (008-character-string)
    ((characterp form)
     (make-character-literal form))
    ;; String literal (008-character-string)
    ((stringp form)
     (make-string-literal form))
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
      ;; Feature 043: FUNCTION special form (#'fn syntax)
      (function (parse-function-form args))
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
      ;; Exception handling (001-control-structure-extension US4)
      (handler-case (parse-handler-case-form args))
      ;; Special variable declarations (T021)
      (defvar (parse-defvar-form args))
      (defparameter (parse-defparameter-form args))
      ;; Feature 038: Constant definitions
      (defconstant (parse-defconstant-form args))
      ;; CLOS class definition (026-clos-foundation)
      (defclass (parse-defclass-to-ast form))  ; Pass full form, not just args
      ;; CLOS instance creation (026-clos-foundation)
      (make-instance (parse-make-instance-to-ast form))  ; Pass full form
      ;; CLOS method definition (026-clos-foundation)
      (defmethod (parse-defmethod-to-ast form))  ; Pass full form
      ;; Macros (expand inline for now)
      (when (parse-when-form args))
      (unless (parse-unless-form args))
      (cond (parse-cond-form args))
      (and (parse-and-form args))
      (or (parse-or-form args))
      ;; Macro introspection (016-macro-system T046-T047, 042-advanced-defmacro)
      (macroexpand-1 (parse-macroexpand-1-form args))
      (macroexpand (parse-macroexpand-form args))
      (macro-function (parse-macro-function-form args))
      ;; Multiple values (025-multiple-values)
      (values (make-ast-values :forms (mapcar #'parse-expr args)))
      (multiple-value-bind (parse-multiple-value-bind-form args))
      (multiple-value-list
       (make-ast-multiple-value-list :form (parse-expr (first args))))
      (nth-value
       (make-ast-nth-value :index (parse-expr (first args))
                           :form (parse-expr (second args))))
      (values-list
       (make-ast-values-list :form (parse-expr (first args))))
      (multiple-value-prog1 (parse-multiple-value-prog1-form args))
      (multiple-value-call (parse-multiple-value-call-form args))
      ;; Arithmetic with constant folding (010-numeric-tower)
      ((+ - * /)
       (parse-arithmetic-form op args))
      ;; Integer division/rounding with constant folding (010-numeric-tower)
      ((truncate floor ceiling round mod rem)
       (parse-rounding-form op args))
      ;; Exponentiation with constant folding (010-numeric-tower)
      (expt (parse-expt-form args))
      ;; Math functions with constant folding (010-numeric-tower T087-T097)
      (sqrt (parse-sqrt-form args))
      (abs (parse-abs-form args))
      (gcd (parse-gcd-form args))
      (lcm (parse-lcm-form args))
      ;; Comparison with constant folding (010-numeric-tower)
      ((= < > <= >= /=)
       (parse-comparison-form op args))
      ;; Complex number functions with constant folding (010-numeric-tower T072-T079)
      (complex (parse-complex-form args))
      (realpart (parse-realpart-form args))
      (imagpart (parse-imagpart-form args))
      (conjugate (parse-conjugate-form args))
      ;; Numeric type predicates with constant folding (010-numeric-tower T107-T115)
      (numberp (parse-numberp-form args))
      (integerp (parse-integerp-form args))
      (rationalp (parse-rationalp-form args))
      (realp (parse-realp-form args))
      (floatp (parse-floatp-form args))
      (complexp (parse-complexp-form args))
      (zerop (parse-zerop-form args))
      (plusp (parse-plusp-form args))
      (minusp (parse-minusp-form args))
      (evenp (parse-evenp-form args))
      (oddp (parse-oddp-form args))
      ;; FFI call-host (027-complete-ffi)
      (clysm/ffi:call-host (parse-call-host-form args))
      ;; THE special form - type declaration, just evaluate the form
      ;; (the type form) -> parse-expr(form)
      ;; Feature: 043-self-hosting-blockers
      (the (if (>= (length args) 2)
               (parse-expr (second args))
               (error "THE requires type and form")))
      ;; Function call
      (otherwise
       (parse-function-call-or-ffi op args)))))

;;; ============================================================
;;; FFI Function Call Parsing (027-complete-ffi)
;;; ============================================================

(defun ffi-declared-function-p (name)
  "Check if NAME is a declared FFI function in the global environment."
  (and (boundp 'clysm/ffi:*ffi-environment*)
       (clysm/ffi:lookup-foreign-function
        clysm/ffi:*ffi-environment* name)))

(defun parse-call-host-form (args)
  "Parse (ffi:call-host function-name &rest args) into ast-call-host.
   ARGS is (function-name arg1 arg2 ...) where function-name is a string expression."
  (make-ast-call-host
   :function-name (parse-expr (first args))
   :arguments (mapcar #'parse-expr (rest args))))

(defun parse-function-call-or-ffi (op args)
  "Parse a function call, checking if it's an FFI-declared function.
   If the function is registered in *ffi-environment*, create ast-ffi-call.
   Otherwise, create a normal ast-call."
  (if (symbolp op)
      (let ((ffi-decl (ffi-declared-function-p op)))
        (if ffi-decl
            ;; FFI function - create ast-ffi-call
            (make-ast-ffi-call
             :declaration ffi-decl
             :arguments (mapcar #'parse-expr args))
            ;; Regular function call
            (make-ast-call
             :function op
             :arguments (mapcar #'parse-expr args)
             :call-type :named)))
      ;; Non-symbol operator (funcall)
      (make-ast-call
       :function op
       :arguments (mapcar #'parse-expr args)
       :call-type :funcall)))

;;; ============================================================
;;; Constant Folding for Arithmetic (010-numeric-tower)
;;; ============================================================

(defun numeric-literal-p (ast)
  "Check if an AST node is a numeric literal."
  (and (typep ast 'ast-literal)
       (member (ast-literal-literal-type ast)
               '(:fixnum :bignum :ratio :float :complex))))

(defun get-numeric-value (ast)
  "Extract the numeric value from a numeric literal AST node."
  (ast-literal-value ast))

(defun all-numeric-literals-p (asts)
  "Check if all AST nodes in the list are numeric literals."
  (every #'numeric-literal-p asts))

(defun make-numeric-literal (value)
  "Create the appropriate literal AST node for a numeric value."
  (typecase value
    (integer (make-integer-literal value))
    (ratio (make-ratio-literal value))
    (float (make-float-literal value))
    (complex (make-complex-literal value))
    (t (make-ast-literal :value value :literal-type :number))))

(defun fold-arithmetic (op values)
  "Fold arithmetic operation on constant values.
   Uses sb-int:with-float-traps-masked to allow IEEE 754 special values
   (infinity, NaN) to be produced by float division by zero."
  ;; Mask float traps to allow IEEE 754 special values
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
    (case op
      (+ (reduce #'+ values :initial-value 0))
      (* (reduce #'* values :initial-value 1))
      (- (if (= 1 (length values))
             (- (first values))
             (reduce #'- values)))
      (/ (if (= 1 (length values))
             (/ 1 (first values))
             (reduce #'/ values))))))

(defun parse-arithmetic-form (op args)
  "Parse an arithmetic form with constant folding.
   If all arguments are numeric literals, compute at parse time.
   Otherwise, create a function call AST."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (not (null parsed-args))
             (all-numeric-literals-p parsed-args))
        ;; All constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (fold-arithmetic op values)))
          (make-numeric-literal result))
        ;; Not all constants - create function call
        (make-ast-call
         :function op
         :arguments parsed-args
         :call-type :named))))

(defun fold-comparison (op values)
  "Fold comparison operation on constant values."
  (let ((result
          (case op
            (= (apply #'= values))
            (< (apply #'< values))
            (> (apply #'> values))
            (<= (apply #'<= values))
            (>= (apply #'>= values))
            (/= (apply #'/= values)))))
    (if result t nil)))

(defun parse-comparison-form (op args)
  "Parse a comparison form with constant folding.
   If all arguments are numeric literals, compute at parse time.
   Otherwise, create a function call AST."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (>= (length parsed-args) 2)
             (all-numeric-literals-p parsed-args))
        ;; All constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (fold-comparison op values)))
          (if result (make-t-literal) (make-nil-literal)))
        ;; Not all constants - create function call
        (make-ast-call
         :function op
         :arguments parsed-args
         :call-type :named))))

(defun fold-rounding (op dividend divisor)
  "Fold rounding operation on constant values.
   Returns the quotient (first value of the multiple-value result)."
  (case op
    (truncate (truncate dividend divisor))
    (floor (floor dividend divisor))
    (ceiling (ceiling dividend divisor))
    (round (round dividend divisor))
    (mod (mod dividend divisor))
    (rem (rem dividend divisor))))

(defun parse-rounding-form (op args)
  "Parse a rounding form with constant folding.
   If all arguments are numeric literals, compute at parse time.
   Otherwise, create a function call AST."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 2 (length parsed-args))
             (all-numeric-literals-p parsed-args))
        ;; All constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (fold-rounding op (first values) (second values))))
          (make-numeric-literal result))
        ;; Not all constants - create function call
        (make-ast-call
         :function op
         :arguments parsed-args
         :call-type :named))))

(defun parse-expt-form (args)
  "Parse expt with constant folding.
   If both arguments are numeric literals, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 2 (length parsed-args))
             (all-numeric-literals-p parsed-args))
        ;; All constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (expt (first values) (second values))))
          (make-numeric-literal result))
        ;; Not all constants - create function call
        (make-ast-call
         :function 'expt
         :arguments parsed-args
         :call-type :named))))

;;; ============================================================
;;; Constant Folding for Complex Number Functions (010-numeric-tower)
;;; T072-T079: Complex number operations
;;; ============================================================

(defun real-literal-p (ast)
  "Check if an AST node is a real numeric literal (not complex)."
  (and (typep ast 'ast-literal)
       (member (ast-literal-literal-type ast)
               '(:fixnum :bignum :ratio :float))))

(defun parse-complex-form (args)
  "Parse (complex real [imag]) with constant folding.
   Creates a complex number from real and optional imaginary parts.
   If both arguments are numeric literals, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (cond
      ;; Single argument: (complex real) = real + 0i
      ((and (= 1 (length parsed-args))
            (real-literal-p (first parsed-args)))
       ;; In CL, (complex x) where x is real just returns x
       ;; (complex 5) => 5 (not #C(5 0))
       (first parsed-args))
      ;; Two arguments: (complex real imag)
      ((and (= 2 (length parsed-args))
            (real-literal-p (first parsed-args))
            (real-literal-p (second parsed-args)))
       (let* ((real-val (get-numeric-value (first parsed-args)))
              (imag-val (get-numeric-value (second parsed-args)))
              (result (complex real-val imag-val)))
         (make-numeric-literal result)))
      ;; Not all constants - create function call
      (t
       (make-ast-call
        :function 'complex
        :arguments parsed-args
        :call-type :named)))))

(defun parse-realpart-form (args)
  "Parse (realpart x) with constant folding.
   If argument is a numeric literal, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        ;; Constant - fold at parse time
        (let* ((value (get-numeric-value (first parsed-args)))
               (result (realpart value)))
          (make-numeric-literal result))
        ;; Not a constant - create function call
        (make-ast-call
         :function 'realpart
         :arguments parsed-args
         :call-type :named))))

(defun parse-imagpart-form (args)
  "Parse (imagpart x) with constant folding.
   If argument is a numeric literal, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        ;; Constant - fold at parse time
        (let* ((value (get-numeric-value (first parsed-args)))
               (result (imagpart value)))
          (make-numeric-literal result))
        ;; Not a constant - create function call
        (make-ast-call
         :function 'imagpart
         :arguments parsed-args
         :call-type :named))))

(defun parse-conjugate-form (args)
  "Parse (conjugate x) with constant folding.
   If argument is a numeric literal, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        ;; Constant - fold at parse time
        (let* ((value (get-numeric-value (first parsed-args)))
               (result (conjugate value)))
          (make-numeric-literal result))
        ;; Not a constant - create function call
        (make-ast-call
         :function 'conjugate
         :arguments parsed-args
         :call-type :named))))

;;; ============================================================
;;; Constant Folding for Math Functions (010-numeric-tower)
;;; T087-T097: Mathematical functions
;;; ============================================================

(defun parse-sqrt-form (args)
  "Parse (sqrt x) with constant folding.
   If argument is a numeric literal, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        ;; Constant - fold at parse time
        (let* ((value (get-numeric-value (first parsed-args)))
               (result (sqrt value)))
          (make-numeric-literal result))
        ;; Not a constant - create function call
        (make-ast-call
         :function 'sqrt
         :arguments parsed-args
         :call-type :named))))

(defun parse-abs-form (args)
  "Parse (abs x) with constant folding.
   If argument is a numeric literal, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        ;; Constant - fold at parse time
        (let* ((value (get-numeric-value (first parsed-args)))
               (result (abs value)))
          (make-numeric-literal result))
        ;; Not a constant - create function call
        (make-ast-call
         :function 'abs
         :arguments parsed-args
         :call-type :named))))

(defun parse-gcd-form (args)
  "Parse (gcd &rest integers) with constant folding.
   If all arguments are integer literals, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (not (null parsed-args))
             (every (lambda (arg)
                      (and (numeric-literal-p arg)
                           (integerp (get-numeric-value arg))))
                    parsed-args))
        ;; All integer constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (apply #'gcd values)))
          (make-numeric-literal result))
        ;; Not all integer constants - create function call
        (make-ast-call
         :function 'gcd
         :arguments parsed-args
         :call-type :named))))

(defun parse-lcm-form (args)
  "Parse (lcm &rest integers) with constant folding.
   If all arguments are integer literals, compute at parse time."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (not (null parsed-args))
             (every (lambda (arg)
                      (and (numeric-literal-p arg)
                           (integerp (get-numeric-value arg))))
                    parsed-args))
        ;; All integer constants - fold at parse time
        (let* ((values (mapcar #'get-numeric-value parsed-args))
               (result (apply #'lcm values)))
          (make-numeric-literal result))
        ;; Not all integer constants - create function call
        (make-ast-call
         :function 'lcm
         :arguments parsed-args
         :call-type :named))))

;;; ============================================================
;;; Constant Folding for Numeric Predicates (010-numeric-tower)
;;; T107-T115: Numeric type predicates
;;; ============================================================

(defun parse-numberp-form (args)
  "Parse (numberp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (numberp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'numberp :arguments parsed-args :call-type :named))))

(defun parse-integerp-form (args)
  "Parse (integerp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (integerp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'integerp :arguments parsed-args :call-type :named))))

(defun parse-rationalp-form (args)
  "Parse (rationalp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (rationalp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'rationalp :arguments parsed-args :call-type :named))))

(defun parse-realp-form (args)
  "Parse (realp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (realp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'realp :arguments parsed-args :call-type :named))))

(defun parse-floatp-form (args)
  "Parse (floatp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (floatp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'floatp :arguments parsed-args :call-type :named))))

(defun parse-complexp-form (args)
  "Parse (complexp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (typep (first parsed-args) 'ast-literal))
        (let ((value (ast-literal-value (first parsed-args))))
          (if (complexp value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'complexp :arguments parsed-args :call-type :named))))

(defun parse-zerop-form (args)
  "Parse (zerop x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        (let ((value (get-numeric-value (first parsed-args))))
          (if (zerop value) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'zerop :arguments parsed-args :call-type :named))))

(defun parse-plusp-form (args)
  "Parse (plusp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        (let ((value (get-numeric-value (first parsed-args))))
          (if (and (realp value) (plusp value)) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'plusp :arguments parsed-args :call-type :named))))

(defun parse-minusp-form (args)
  "Parse (minusp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        (let ((value (get-numeric-value (first parsed-args))))
          (if (and (realp value) (minusp value)) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'minusp :arguments parsed-args :call-type :named))))

(defun parse-evenp-form (args)
  "Parse (evenp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        (let ((value (get-numeric-value (first parsed-args))))
          (if (and (integerp value) (evenp value)) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'evenp :arguments parsed-args :call-type :named))))

(defun parse-oddp-form (args)
  "Parse (oddp x) with constant folding."
  (let ((parsed-args (mapcar #'parse-expr args)))
    (if (and (= 1 (length parsed-args))
             (numeric-literal-p (first parsed-args)))
        (let ((value (get-numeric-value (first parsed-args))))
          (if (and (integerp value) (oddp value)) (make-t-literal) (make-nil-literal)))
        (make-ast-call :function 'oddp :arguments parsed-args :call-type :named))))

(defun parse-if-form (args)
  "Parse (if test then [else])."
  (make-ast-if
   :test (parse-expr (first args))
   :then (parse-expr (second args))
   :else (if (third args) (parse-expr (third args)) (make-nil-literal))))

(defun parse-let-form (args sequential-p)
  "Parse (let/let* bindings body...).
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((bindings (first args))
        (body (rest args)))
    ;; Feature 043: Filter out declare forms before parsing
    (let ((filtered-body (filter-declare-forms body)))
      (make-ast-let
       :bindings (mapcar (lambda (b)
                           (if (consp b)
                               (cons (first b) (parse-expr (second b)))
                               (cons b (make-nil-literal))))
                         bindings)
       :body (mapcar #'parse-expr filtered-body)
       :sequential-p sequential-p))))

(defun parse-progn-form (args)
  "Parse (progn forms...)."
  (make-ast-progn :forms (mapcar #'parse-expr args)))

(defun parse-defun-form (args)
  "Parse (defun name params body...).
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((name (first args))
        (params (second args))
        (body (cddr args)))
    ;; Check for docstring
    (multiple-value-bind (docstring actual-body)
        (if (and (stringp (car body)) (cdr body))
            (values (car body) (cdr body))
            (values nil body))
      ;; Feature 043: Filter out declare forms before parsing
      (let ((filtered-body (filter-declare-forms actual-body)))
        (make-ast-defun
         :name name
         :parameters params
         :body (mapcar #'parse-expr filtered-body)
         :docstring docstring)))))

(defun parse-lambda-form (args)
  "Parse (lambda params body...).
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((params (first args))
        (body (rest args)))
    ;; Feature 043: Filter out declare forms before parsing
    (let ((filtered-body (filter-declare-forms body)))
      (make-ast-lambda
       :parameters params
       :body (mapcar #'parse-expr filtered-body)))))

;;; Feature 043: FUNCTION special form parsing
(defun parse-function-form (args)
  "Parse (function name) or (function (lambda ...)).
   ARGS is (name) or ((lambda ...)).
   Returns ast-function node."
  (let ((arg (first args)))
    (cond
      ;; (function (lambda ...)) - anonymous function
      ((and (consp arg) (eq (car arg) 'lambda))
       (make-ast-function :name (parse-lambda-form (cdr arg))))
      ;; (function name) - named function reference
      ((symbolp arg)
       (make-ast-function :name arg))
      (t
       (error "FUNCTION: Invalid argument ~S" arg)))))

;;; Multiple-value-bind parsing (025-multiple-values)
(defun parse-multiple-value-bind-form (args)
  "Parse (multiple-value-bind (vars...) values-form body...).
   ARGS is ((vars...) values-form body...)."
  (let ((vars (first args))
        (values-form (second args))
        (body (cddr args)))
    (make-ast-multiple-value-bind
     :vars vars
     :values-form (parse-expr values-form)
     :body (mapcar #'parse-expr body))))

;;; Multiple-value-prog1 parsing (025-multiple-values)
(defun parse-multiple-value-prog1-form (args)
  "Parse (multiple-value-prog1 first-form body...).
   Returns values of first-form after evaluating body for side effects."
  (make-ast-multiple-value-prog1
   :first-form (parse-expr (first args))
   :body (mapcar #'parse-expr (rest args))))

;;; Multiple-value-call parsing (025-multiple-values)
(defun parse-multiple-value-call-form (args)
  "Parse (multiple-value-call function form1 form2 ...).
   Calls function with all values from all forms as arguments."
  (make-ast-multiple-value-call
   :function (parse-expr (first args))
   :forms (mapcar #'parse-expr (rest args))))

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
  "Parse (flet ((name params body...) ...) forms...).
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((definitions (first args))
        (body (rest args)))
    ;; Feature 043: Filter out declare forms before parsing
    (let ((filtered-body (filter-declare-forms body)))
      (make-ast-flet
       :definitions (mapcar #'parse-local-function-def definitions)
       :body (mapcar #'parse-expr filtered-body)))))

(defun parse-labels-form (args)
  "Parse (labels ((name params body...) ...) forms...).
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((definitions (first args))
        (body (rest args)))
    ;; Feature 043: Filter out declare forms before parsing
    (let ((filtered-body (filter-declare-forms body)))
      (make-ast-labels
       :definitions (mapcar #'parse-local-function-def definitions)
       :body (mapcar #'parse-expr filtered-body)))))

(defun parse-local-function-def (def)
  "Parse a local function definition (name params body...).
   Returns (name params parsed-body...) where body is list of AST nodes.
   Feature 043: Filters out DECLARE forms from body before parsing."
  (let ((name (first def))
        (params (second def))
        (body (cddr def)))
    ;; Feature 043: Filter out declare forms before parsing
    (let ((filtered-body (filter-declare-forms body)))
      (list name params (mapcar #'parse-expr filtered-body)))))

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
;;; Macro Introspection Parsing (016-macro-system T046-T047)
;;; ============================================================

(defun parse-macroexpand-1-form (args)
  "Parse (macroexpand-1 form &optional env).
   Creates an ast-macroexpand-1 node for runtime macro expansion."
  (unless args
    (error "MACROEXPAND-1 requires a form argument"))
  ;; Note: env argument is ignored for now (not supported in target runtime)
  (make-ast-macroexpand-1 :form (parse-expr (car args))))

(defun parse-macroexpand-form (args)
  "Parse (macroexpand form &optional env).
   Creates an ast-macroexpand node for runtime macro expansion."
  (unless args
    (error "MACROEXPAND requires a form argument"))
  ;; Note: env argument is ignored for now (not supported in target runtime)
  (make-ast-macroexpand :form (parse-expr (car args))))

(defun parse-macro-function-form (args)
  "Parse (macro-function name &optional env).
   Creates an ast-macro-function node for macro function lookup.
   Feature 042: Advanced Defmacro."
  (unless args
    (error "MACRO-FUNCTION requires a name argument"))
  (make-ast-macro-function
   :name (parse-expr (car args))
   :env (when (cdr args) (parse-expr (cadr args)))))

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
;;; Exception Handling Parsing (001-control-structure-extension US4)
;;; ============================================================

;; HyperSpec: resources/HyperSpec/Body/m_hand_1.htm
(defun parse-handler-clause (clause)
  "Parse a single handler clause (type ([var]) . body).
   TYPE is the condition type specifier.
   ([var]) is the optional lambda list with variable to bind the condition.
   BODY is the handler body forms."
  (let ((type (first clause))
        (lambda-list (second clause))
        (body (cddr clause)))
    (make-handler-clause
     :type type
     :var (and lambda-list (first lambda-list))
     :body (mapcar #'parse-expr body))))

(defun parse-handler-case-form (args)
  "Parse (handler-case expression {(type ([var]) . body)}*).
   EXPRESSION is the protected form to evaluate.
   Each handler clause specifies a condition type and handler body."
  (when (null args)
    (error "HANDLER-CASE requires at least an expression"))
  (make-ast-handler-case
   :expression (parse-expr (first args))
   :handlers (mapcar #'parse-handler-clause (rest args))))

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

;;; ============================================================
;;; Feature 038: Constant Definitions
;;; ============================================================

(defun parse-defconstant-form (args)
  "Parse (defconstant name value-form [docstring]).
   Constants are immutable and their values are computed at compile time.
   Used for configuration values, type indices, and opcodes."
  (let* ((name (first args))
         (value-form (second args))
         (docstring (third args)))
    ;; Error if no value-form (defconstant requires it)
    (unless value-form
      (error "DEFCONSTANT requires a value: ~S" name))
    (unless (symbolp name)
      (error "DEFCONSTANT name must be a symbol: ~S" name))
    ;; Create AST node
    (make-ast-defconstant
     :name name
     :value-form (parse-expr value-form)
     :docstring docstring)))

;;; ============================================================
;;; Feature 038: Declaration Filtering (US3)
;;; ============================================================

(defun filter-declare-forms (body-forms)
  "Filter out DECLARE forms from body, returning filtered body and declarations.
   Returns (VALUES filtered-body declarations) where:
   - filtered-body: Body forms without declare forms
   - declarations: List of extracted declare specifiers

   DECLARE forms are only valid at the beginning of the body."
  (let ((declarations '())
        (filtered '())
        (in-declarations t))
    (dolist (form body-forms)
      (if (and in-declarations
               (consp form)
               (eq (car form) 'declare))
          ;; Collect declaration specifiers
          (dolist (spec (cdr form))
            (push spec declarations))
          ;; Not a declare or past declarations section
          (progn
            (setf in-declarations nil)
            (push form filtered))))
    (values (nreverse filtered)
            (nreverse declarations))))
