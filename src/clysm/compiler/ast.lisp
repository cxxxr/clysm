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
      ;; Function call
      (otherwise
       (make-ast-call
        :function op
        :arguments (mapcar #'parse-expr args)
        :call-type (if (symbolp op) :named :funcall))))))

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
  "Fold arithmetic operation on constant values."
  (case op
    (+ (reduce #'+ values :initial-value 0))
    (* (reduce #'* values :initial-value 1))
    (- (if (= 1 (length values))
           (- (first values))
           (reduce #'- values)))
    (/ (if (= 1 (length values))
           (/ 1 (first values))
           (reduce #'/ values)))))

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
