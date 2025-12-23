;;;; func-section.lisp - Function and Code Section generation
;;;; Compiles AST nodes to Wasm instructions

(in-package #:clysm/compiler/codegen/func-section)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct (compilation-env (:conc-name cenv-)
                            (:copier nil))
  "Compilation environment tracking locals and scope."
  (locals nil :type list)              ; ((name . index) ...)
  (local-counter-box nil :type list)   ; (counter) - mutable box for shared counter
  (local-types-box nil :type list)     ; ((index . type) ...) - boxed for sharing
  (functions nil :type list)           ; ((name . index) ...)
  (function-counter 0 :type fixnum)
  (in-tail-position nil :type boolean)
  (captured-vars nil :type list)       ; ((name . position) ...) for captured vars
  (local-functions nil :type list)     ; ((name . local-idx) ...) for flet/labels
  ;; Phase 6: Exception handling
  (blocks nil :type list)              ; ((name . block-depth) ...) for block/return-from
  (block-depth 0 :type fixnum)         ; Current nesting depth for br targets
  (tagbody-tags nil :type list)        ; ((tag . label-idx) ...) for tagbody/go - legacy field
  (tagbody-context nil :type (or null tagbody-context)) ; Active tagbody compilation context
  (catch-tags nil :type list)          ; ((tag-expr . handler-depth) ...) for catch/throw
  (unwind-stack nil :type list))       ; Stack of unwind-protect handlers

;;; ============================================================
;;; Tagbody Context (for tagbody/go compilation)
;;; ============================================================

(defstruct tagbody-context
  "Context for compiling tagbody/go control flow.
   Created when entering compile-tagbody, stored in compilation-env,
   accessed by compile-go to determine jump target."
  (strategy nil :type keyword)
  ;; :sequential | :simple-loop | :dispatch

  (tags nil :type list)
  ;; Association list: ((tag-symbol . segment-index) ...)
  ;; Maps tag names to their segment indices for go target resolution

  (pc-local nil :type (or null fixnum))
  ;; Index of $pc local variable (dispatch strategy only)
  ;; NIL for sequential and simple-loop strategies

  (base-loop-depth 0 :type fixnum)
  ;; Base br depth from the loop body to the loop target
  ;; For simple-loop: 0 (br 0 targets the loop)
  ;; For dispatch: varies per segment (num-segments - seg-idx)

  (base-block-depth 0 :type fixnum)
  ;; Block depth when entering the tagbody's loop
  ;; Used to calculate correct br depth when nested in if/block structures

  (loop-label nil :type (or null symbol))
  ;; Wasm label for loop target (simple-loop and dispatch strategies)
  ;; e.g., $LOOP for simple-loop, $dispatch for dispatch

  (num-segments nil :type (or null fixnum))
  ;; Total number of segments (used for depth calculation)
  )

(defun make-env ()
  "Create a fresh compilation environment."
  (make-compilation-env :local-counter-box (list 0)
                        :local-types-box (list nil)))

(defun cenv-local-counter (env)
  "Get the local counter value."
  (car (cenv-local-counter-box env)))

(defun (setf cenv-local-counter) (value env)
  "Set the local counter value."
  (setf (car (cenv-local-counter-box env)) value))

(defun env-lookup-local (env name)
  "Look up a local variable index."
  (cdr (assoc name (cenv-locals env))))

(defun env-add-local (env name &optional (type :anyref))
  "Add a local variable with optional type and return its index.
   Type defaults to :anyref. Non-anyref types are tracked in local-types-box."
  (let ((idx (cenv-local-counter env)))
    (push (cons name idx) (cenv-locals env))
    ;; Track non-anyref types in the boxed list
    (unless (eq type :anyref)
      (setf (car (cenv-local-types-box env))
            (cons (cons idx type) (car (cenv-local-types-box env)))))
    (incf (car (cenv-local-counter-box env)))
    idx))

(defun env-local-type (env idx)
  "Get the type of a local by index. Returns :anyref if not explicitly tracked."
  (or (cdr (assoc idx (car (cenv-local-types-box env)))) :anyref))

(defun env-lookup-function (env name)
  "Look up a function index."
  (cdr (assoc name (cenv-functions env))))

(defun env-add-function (env name)
  "Add a function and return its index."
  (let ((idx (cenv-function-counter env)))
    (push (cons name idx) (cenv-functions env))
    (incf (cenv-function-counter env))
    idx))

(defun env-set-function-counter (env value)
  "Set the function counter to a specific value."
  (setf (cenv-function-counter env) value))

;;; ============================================================
;;; Wasm Instruction Opcodes
;;; ============================================================

(defparameter *wasm-opcodes*
  '(;; Control
    (:unreachable . #x00)
    (:nop . #x01)
    (:block . #x02)
    (:loop . #x03)
    (:if . #x04)
    (:else . #x05)
    (:end . #x0B)
    (:br . #x0C)
    (:br_if . #x0D)
    (:return . #x0F)
    (:call . #x10)
    (:call_indirect . #x11)
    ;; Reference
    (:ref.null . #xD0)
    (:ref.is_null . #xD1)
    (:ref.func . #xD2)
    (:ref.eq . #xD3)
    ;; Parametric
    (:drop . #x1A)
    (:select . #x1B)
    ;; Variable
    (:local.get . #x20)
    (:local.set . #x21)
    (:local.tee . #x22)
    (:global.get . #x23)
    (:global.set . #x24)
    ;; Numeric i32
    (:i32.const . #x41)
    (:i32.eqz . #x45)
    (:i32.eq . #x46)
    (:i32.ne . #x47)
    (:i32.lt_s . #x48)
    (:i32.lt_u . #x49)
    (:i32.gt_s . #x4A)
    (:i32.gt_u . #x4B)
    (:i32.le_s . #x4C)
    (:i32.le_u . #x4D)
    (:i32.ge_s . #x4E)
    (:i32.ge_u . #x4F)
    (:i32.add . #x6A)
    (:i32.sub . #x6B)
    (:i32.mul . #x6C)
    (:i32.div_s . #x6D)
    (:i32.div_u . #x6E)
    (:i32.rem_s . #x6F)
    (:i32.rem_u . #x70)
    (:i32.and . #x71)
    (:i32.or . #x72)
    (:i32.xor . #x73)
    ;; i64
    (:i64.const . #x42)
    ;; GC instructions (0xFB prefix)
    (:ref.i31 . (#xFB #x1C))      ; Create i31ref from i32
    (:i31.get_s . (#xFB #x1D))   ; Extract signed i32 from i31ref
    (:i31.get_u . (#xFB #x1E))   ; Extract unsigned i32 from i31ref
    (:struct.new . (#xFB #x00))
    (:struct.get . (#xFB #x02))
    (:struct.set . (#xFB #x05))
    (:ref.cast . (#xFB #x17))    ; ref.cast to a specific type
    ;; Function reference instructions
    (:call_ref . #x14)           ; call_ref (typed function calls)
    (:return_call . #x12)        ; return_call (tail call)
    (:return_call_ref . #x15)))  ; return_call_ref (tail call through ref)

;;; ============================================================
;;; Main Compilation Entry Point
;;; ============================================================

(defun compile-to-instructions (ast env)
  "Compile an AST node to a list of Wasm instructions."
  (etypecase ast
    (clysm/compiler/ast:ast-literal
     (compile-literal ast))
    (clysm/compiler/ast:ast-var-ref
     (compile-var-ref ast env))
    (clysm/compiler/ast:ast-call
     (compile-call ast env))
    (clysm/compiler/ast:ast-if
     (compile-if ast env))
    (clysm/compiler/ast:ast-let
     (compile-let ast env))
    (clysm/compiler/ast:ast-progn
     (compile-progn ast env))
    (clysm/compiler/ast:ast-setq
     (compile-setq ast env))
    (clysm/compiler/ast:ast-defun
     (compile-defun ast env))
    (clysm/compiler/ast:ast-lambda
     (compile-lambda ast env))
    (clysm/compiler/ast:ast-flet
     (compile-flet ast env))
    (clysm/compiler/ast:ast-labels
     (compile-labels ast env))
    (clysm/compiler/ast:ast-block
     (compile-block ast env))
    (clysm/compiler/ast:ast-return-from
     (compile-return-from ast env))
    (clysm/compiler/ast:ast-tagbody
     (compile-tagbody ast env))
    (clysm/compiler/ast:ast-go
     (compile-go ast env))
    (clysm/compiler/ast:ast-catch
     (compile-catch ast env))
    (clysm/compiler/ast:ast-throw
     (compile-throw ast env))
    (clysm/compiler/ast:ast-unwind-protect
     (compile-unwind-protect ast env))
    ;; Special variable declarations (T022-T024)
    (clysm/compiler/ast:ast-defvar
     (compile-defvar ast env))
    (clysm/compiler/ast:ast-defparameter
     (compile-defparameter ast env))))

;;; ============================================================
;;; Literal Compilation
;;; ============================================================

(defun compile-literal (ast)
  "Compile a literal value to Wasm instructions."
  (let ((value (clysm/compiler/ast:ast-literal-value ast))
        (type (clysm/compiler/ast:ast-literal-literal-type ast)))
    (case type
      (:fixnum
       ;; Fixnums are represented as i31ref (T047)
       ;; i32.const value, ref.i31
       (list (list :i32.const value)
             :ref.i31))
      (:nil
       ;; NIL is represented as ref.null (null reference)
       (list '(:ref.null :none)))
      (:t
       ;; T is represented as non-null (use i31ref of 1)
       (list '(:i32.const 1)
             :ref.i31))
      (:quoted
       ;; Quoted symbols - for now, represent as i31ref of symbol hash
       ;; Full implementation would use actual symbol objects
       (if (null value)
           (list '(:ref.null :none))
           ;; Use symbol hash as placeholder for symbol identity
           ;; Truncate to 32 bits for i32.const, then to 30 bits for i31
           (let ((hash (logand (sxhash value) #x3FFFFFFF)))  ; 30-bit for i31ref
             (list (list :i32.const hash)
                   :ref.i31))))
      (otherwise
       (error "Unsupported literal type: ~A" type)))))

;;; ============================================================
;;; Variable Reference Compilation (T048)
;;; ============================================================

(defun compile-var-ref (ast env)
  "Compile a variable reference.
   Handles locals, captured variables from closures, special variables, and globals."
  (let* ((name (clysm/compiler/ast:ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable
      (local-idx
       (list (list :local.get local-idx)))
      ;; Captured variable from closure environment
      ((env-lookup-captured env name)
       (compile-captured-var-access name env))
      ;; Special variable (T037)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-var-ref name))
      ;; Unknown variable
      (t
       (error "Unbound variable: ~A" name)))))

(defun compile-special-var-ref (name)
  "Compile a reference to a special variable (T037).
   Generates code to read the symbol's current value.
   Pattern: global.get <index>, struct.get $symbol $value"
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    `((:global.get ,global-idx)
      (:struct.get ,symbol-type 1))))

(defun env-lookup-captured (env name)
  "Look up a captured variable position."
  (cdr (assoc name (cenv-captured-vars env))))

(defun compile-captured-var-access (name env)
  "Generate instructions to access a captured variable from the closure's env.
   The closure is in local 0 ($closure).
   The env is a cons-list: (cons var0 (cons var1 (cons var2 nil)))
   To get var at position N, we need N cdrs followed by a car."
  (let* ((position (env-lookup-captured env name))
         (result '()))
    ;; Get the closure (local 0)
    (setf result (append result '((:local.get 0))))
    ;; Cast to closure type
    (setf result (append result
                         (list (list :ref.cast
                                     clysm/compiler/codegen/gc-types:+type-closure+))))
    ;; Get the env field (field index 4)
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-closure+
                                     4))))
    ;; Navigate the cons-list: N times cdr, then car
    (dotimes (i position)
      ;; Cast to cons type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-cons+))))
      ;; Get cdr (field index 1)
      (setf result (append result
                           (list (list :struct.get
                                       clysm/compiler/codegen/gc-types:+type-cons+
                                       1)))))
    ;; Cast final position to cons and get car (field index 0)
    (setf result (append result
                         (list (list :ref.cast
                                     clysm/compiler/codegen/gc-types:+type-cons+))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-cons+
                                     0))))
    result))

;;; ============================================================
;;; Function Call Compilation (T049-T052)
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((function (clysm/compiler/ast:ast-call-function ast))
        (args (clysm/compiler/ast:ast-call-arguments ast)))
    ;; Check for special forms and primitives
    (cond
      ;; funcall special form
      ((and (symbolp function) (eq function 'funcall))
       (compile-funcall args env))
      ;; Primitive operators
      ((and (symbolp function)
            (member function '(+ - * / < > <= >= = /= truncate)))
       (compile-primitive-call function args env))
      ;; Local function (from flet/labels)
      ((and (symbolp function) (env-lookup-local-function env function))
       (compile-local-function-call function args env))
      ;; Regular function call
      (t
       (compile-regular-call function args env)))))

(defun env-lookup-local-function (env name)
  "Look up a local function's local variable index or :captured marker."
  (cdr (assoc name (cenv-local-functions env))))

(defun compile-local-function-call (function args env)
  "Compile a call to a local function (from flet/labels).
   The function is stored as a closure in a local variable or captured env."
  (let* ((local-func-info (env-lookup-local-function env function))
         (arity (length args))
         (result '()))
    ;; Get the closure - either from local or from captured env
    (if (eq local-func-info :captured)
        ;; Captured in closure env - use captured-var access
        (setf result (append result (compile-captured-var-access function env)))
        ;; In a local variable
        (setf result (append result (list (list :local.get local-func-info)))))
    ;; Duplicate for the first parameter (closure as self-reference)
    (let ((closure-local (cenv-local-counter env)))
      (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
      ;; Save closure to temp local
      (setf result (append result (list (list :local.set closure-local))))
      ;; Push closure as first argument (self reference)
      (setf result (append result (list (list :local.get closure-local))))
      ;; Push all call arguments
      (dolist (arg args)
        (setf result (append result (compile-to-instructions arg env))))
      ;; Get closure and extract code field
      (setf result (append result (list (list :local.get closure-local))))
      ;; Cast to closure type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      ;; Get the appropriate code field based on arity
      (let ((code-field (case arity
                          (0 0)   ; code_0
                          (1 1)   ; code_1
                          (2 2)   ; code_2
                          (t 3)))) ; code_N
        (setf result (append result
                             (list (list :struct.get
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         code-field)))))
      ;; Cast funcref to specific function type and call
      (let ((func-type (case arity
                         (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                         (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                         (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                         (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                         (t clysm/compiler/codegen/gc-types:+type-func-n+))))
        (setf result (append result (list (list :ref.cast func-type))))
        (setf result (append result (list (list :call_ref func-type))))))
    result))

(defun compile-primitive-call (op args env)
  "Compile a primitive operation."
  (case op
    ;; Arithmetic operators (T049-T052)
    (+  (compile-arithmetic-op :i32.add args env 0))
    (-  (if (= 1 (length args))
            (compile-unary-minus (first args) env)
            (compile-arithmetic-op :i32.sub args env nil)))
    (*  (compile-arithmetic-op :i32.mul args env 1))
    (/  (compile-arithmetic-op :i32.div_s args env nil))
    (truncate (compile-truncate args env))
    ;; Comparison operators (T053)
    (<  (compile-comparison-op :i32.lt_s args env))
    (>  (compile-comparison-op :i32.gt_s args env))
    (<= (compile-comparison-op :i32.le_s args env))
    (>= (compile-comparison-op :i32.ge_s args env))
    (=  (compile-comparison-op :i32.eq args env))
    (/= (compile-not-equal args env))))

(defun compile-arithmetic-op (op args env identity)
  "Compile an arithmetic operation with variadic args.
   For (+ 1 2):
     i32.const 1, ref.i31    ; create i31 for 1
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.const 2, ref.i31    ; create i31 for 2
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.add                 ; add
     ref.i31                 ; wrap result as i31"
  (cond
    ;; Zero args: return identity
    ((null args)
     (if identity
         (list (list :i32.const identity) :ref.i31)
         (error "Operator requires at least one argument")))
    ;; One arg: return value (for +) or apply unary (for -)
    ((null (cdr args))
     (compile-to-instructions (first args) env))
    ;; Two or more args: fold left
    (t
     (let ((result '()))
       ;; Compile first arg and unwrap (cast to i31 first)
       (setf result (append result (compile-to-instructions (first args) env)))
       (setf result (append result (list '(:ref.cast :i31) :i31.get_s)))
       ;; For each remaining arg: compile, cast, unwrap, apply op
       (dolist (arg (rest args))
         (setf result (append result (compile-to-instructions arg env)))
         (setf result (append result (list '(:ref.cast :i31) :i31.get_s op))))
       ;; Wrap result as i31ref
       (setf result (append result (list :ref.i31)))
       result))))

(defun compile-unary-minus (arg env)
  "Compile unary minus: (- x) => (- 0 x)."
  (append
   '((:i32.const 0))
   (compile-to-instructions arg env)
   '((:ref.cast :i31) :i31.get_s
     :i32.sub
     :ref.i31)))

(defun compile-truncate (args env)
  "Compile truncate division."
  (when (< (length args) 2)
    (error "truncate requires two arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.div_s
     :ref.i31)))

(defun compile-comparison-op (op args env)
  "Compile a comparison operation."
  (when (< (length args) 2)
    (error "Comparison requires at least two arguments"))
  ;; For now, only support two args
  ;; TODO: Chain comparisons (< a b c) => (and (< a b) (< b c))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s)
   (list op)
   ;; Convert i32 boolean to Lisp boolean (T or NIL)
   `((:if (:result :anyref))
     (:i32.const 1) :ref.i31  ; T
     :else
     (:ref.null :none)        ; NIL
     :end)))

(defun compile-not-equal (args env)
  "Compile not-equal."
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.ne)
   `((:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-regular-call (function args env)
  "Compile a regular function call (T061)."
  (let ((func-idx (env-lookup-function env function)))
    (unless func-idx
      (error "Undefined function: ~A" function))
    ;; Compile arguments
    (let ((result '()))
      (dolist (arg args)
        (setf result (append result (compile-to-instructions arg env))))
      ;; Call
      (append result (list (list :call func-idx))))))

;;; ============================================================
;;; Funcall Compilation (T085-T087)
;;; ============================================================

(defun compile-funcall (args env)
  "Compile (funcall fn arg1 arg2 ...).
   The first arg is a closure, remaining args are passed to it."
  (when (null args)
    (error "funcall requires at least one argument"))
  (let* ((closure-expr (first args))
         (call-args (rest args))
         (arity (length call-args))
         (result '()))
    ;; Compile the closure expression - will be on top of stack
    (setf result (append result (compile-to-instructions closure-expr env)))
    ;; Duplicate closure ref for call_ref (closure goes as first param too)
    ;; We need: closure arg1 arg2 ... closure-code
    ;; Stack after closure compilation: [..., closure]
    ;; We need to:
    ;; 1. Save closure to a local
    ;; 2. Push closure (for first param)
    ;; 3. Push all args
    ;; 4. Get closure, extract code field, call_ref
    (let ((closure-local (cenv-local-counter env)))
      (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
      ;; Save closure to local
      (setf result (append result (list (list :local.set closure-local))))
      ;; Push closure as first argument (self reference)
      (setf result (append result (list (list :local.get closure-local))))
      ;; Push all call arguments
      (dolist (arg call-args)
        (setf result (append result (compile-to-instructions arg env))))
      ;; Get closure and extract code_N field (field 3 = code_N for variadic)
      ;; For specific arities, we could use code_0/1/2 but code_N always works
      (setf result (append result (list (list :local.get closure-local))))
      ;; Cast to closure type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      ;; Get the appropriate code field based on arity
      (let ((code-field (case arity
                          (0 0)  ; code_0
                          (1 1)  ; code_1
                          (2 2)  ; code_2
                          (t 3)))) ; code_N
        (setf result (append result
                             (list (list :struct.get
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         code-field)))))
      ;; Cast the funcref to the specific function type before call_ref
      ;; call_ref requires a typed function reference
      (let ((func-type (case arity
                         (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                         (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                         (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                         (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                         (t clysm/compiler/codegen/gc-types:+type-func-n+))))
        ;; Cast funcref to the specific function type
        (setf result (append result (list (list :ref.cast func-type))))
        ;; Call through the typed function reference
        (setf result (append result (list (list :call_ref func-type))))))
    result))

;;; ============================================================
;;; Conditional Compilation (T054-T055)
;;; ============================================================

(defun compile-if (ast env)
  "Compile an if expression.
   Note: if creates a Wasm block, so we increment block-depth for nested go/return-from."
  (let ((test (clysm/compiler/ast:ast-if-test ast))
        (then-branch (clysm/compiler/ast:ast-if-then ast))
        (else-branch (clysm/compiler/ast:ast-if-else ast))
        ;; Create new env with incremented block depth for if block
        (if-env (copy-compilation-env env)))
    (incf (cenv-block-depth if-env))
    (append
     ;; Compile test
     (compile-to-instructions test env)
     ;; Check if not NIL
     (compile-nil-check)
     ;; If-then-else
     '((:if (:result :anyref)))
     (compile-to-instructions then-branch if-env)
     '(:else)
     (compile-to-instructions else-branch if-env)
     '(:end))))

(defun compile-nil-check ()
  "Generate code to check if TOS is not NIL.
   Returns i32: 1 if not-nil, 0 if nil.
   NIL is represented as null reference, so we use ref.is_null."
  '(:ref.is_null     ; Is this null?
    :i32.eqz))       ; Invert (not nil => 1)

;;; ============================================================
;;; Binding Compilation (T056-T058)
;;; ============================================================

;;; ------------------------------------------------------------
;;; Special Binding Detection and Helpers (T030)
;;; ------------------------------------------------------------

(defun binding-is-special-p (name)
  "Check if a binding name refers to a special variable (T030).
   A binding is special if the variable has been declared with defvar/defparameter."
  (clysm/compiler/env:special-variable-p name))

(defun partition-bindings (bindings)
  "Partition bindings into (special-bindings . lexical-bindings) (T030).
   Returns two lists: special bindings and lexical bindings."
  (let ((special '())
        (lexical '()))
    (dolist (binding bindings)
      (let ((name (car binding)))
        (if (binding-is-special-p name)
            (push binding special)
            (push binding lexical))))
    (cons (nreverse special) (nreverse lexical))))

;;; ------------------------------------------------------------
;;; Special Binding Save/Restore Code Generation (T031-T032)
;;; ------------------------------------------------------------

(defun emit-save-special-binding (name env)
  "Emit code to save the current value of a special variable (T031).
   Returns (instructions . save-local-idx).
   The current value is saved to a local for later restoration."
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (save-local (env-add-local env (gensym "save"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Generate: global.get sym, struct.get value, local.set save
    (cons `((:global.get ,global-idx)
            (:struct.get ,symbol-type 1)  ; Get value field
            (:local.set ,save-local))
          save-local)))

(defun emit-set-special-binding (name value-instrs env)
  "Emit code to set a special variable's value (T031).
   VALUE-INSTRS are the instructions that produce the new value."
  (declare (ignore env))
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized" name))
    ;; Generate: global.get sym, <value-instrs>, struct.set value
    `((:global.get ,global-idx)
      ,@value-instrs
      (:struct.set ,symbol-type 1))))

(defun emit-restore-special-binding (name save-local)
  "Emit code to restore a special variable from a saved local (T032).
   SAVE-LOCAL is the local index where the old value was saved."
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Generate: global.get sym, local.get save, struct.set value
    `((:global.get ,global-idx)
      (:local.get ,save-local)
      (:struct.set ,symbol-type 1))))

(defun compile-let (ast env)
  "Compile a let or let* expression (T033).
   Handles both lexical and special (dynamic) bindings:
   - Lexical bindings use Wasm locals
   - Special bindings modify the global symbol's value with save/restore"
  (let ((bindings (clysm/compiler/ast:ast-let-bindings ast))
        (body (clysm/compiler/ast:ast-let-body ast))
        (sequential-p (clysm/compiler/ast:ast-let-sequential-p ast))
        (new-env (extend-compilation-env env))
        (result '())
        (special-save-locals '()))  ; ((name . save-local-idx) ...)
    ;; Process bindings based on let vs let*
    (if sequential-p
        ;; let*: each binding sees previous bindings
        (dolist (binding bindings)
          (let* ((name (car binding))
                 (value-form (cdr binding)))
            (if (binding-is-special-p name)
                ;; Special binding: save, compute, set global
                (let* ((save-result (emit-save-special-binding name new-env))
                       (save-instrs (car save-result))
                       (save-local (cdr save-result))
                       (value-instrs (compile-to-instructions value-form new-env))
                       (set-instrs (emit-set-special-binding name value-instrs new-env)))
                  (push (cons name save-local) special-save-locals)
                  (setf result (append result save-instrs set-instrs)))
                ;; Lexical binding: use local
                (let ((idx (env-add-local new-env name)))
                  (setf result (append result
                                       (compile-to-instructions value-form new-env)
                                       (list (list :local.set idx))))))))
        ;; let: all values computed before any binding visible
        ;; For special bindings, we still need to save first, then set
        (let ((compiled-values '())
              (binding-actions '()))
          ;; First pass: save special vars and compile all values
          (dolist (binding bindings)
            (let* ((name (car binding))
                   (value-form (cdr binding)))
              (if (binding-is-special-p name)
                  ;; Special: save current value first
                  (let* ((save-result (emit-save-special-binding name new-env))
                         (save-instrs (car save-result))
                         (save-local (cdr save-result)))
                    (push (cons name save-local) special-save-locals)
                    (setf result (append result save-instrs))
                    ;; Compile value (uses original env for parallel semantics)
                    (push (cons :special (cons name (compile-to-instructions value-form env)))
                          compiled-values))
                  ;; Lexical: just allocate local and compile value
                  (let ((idx (env-add-local new-env name)))
                    (push (cons :lexical (cons idx (compile-to-instructions value-form env)))
                          compiled-values)))))
          (setf compiled-values (nreverse compiled-values))
          ;; Second pass: set all bindings
          (dolist (cv compiled-values)
            (let ((kind (car cv))
                  (data (cdr cv)))
              (if (eq kind :special)
                  ;; Set special variable's global
                  (let ((name (car data))
                        (value-instrs (cdr data)))
                    (setf result (append result
                                         (emit-set-special-binding name value-instrs new-env))))
                  ;; Set lexical local
                  (let ((idx (car data))
                        (value-instrs (cdr data)))
                    (setf result (append result
                                         value-instrs
                                         (list (list :local.set idx))))))))))
    ;; Compile body
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form new-env)
                           '(:drop))))
    (let ((body-result-instrs
            (when body
              (compile-to-instructions (car (last body)) new-env))))
      ;; Save result to a local before restore (if we have special bindings)
      (if special-save-locals
          (let ((result-local (env-add-local new-env (gensym "let-result"))))
            (setf result (append result
                                 body-result-instrs
                                 (list (list :local.set result-local))))
            ;; Restore special bindings in reverse order
            (dolist (save-entry (reverse special-save-locals))
              (let ((name (car save-entry))
                    (save-local (cdr save-entry)))
                (setf result (append result
                                     (emit-restore-special-binding name save-local)))))
            ;; Return the saved result
            (setf result (append result (list (list :local.get result-local)))))
          ;; No special bindings, just append body result
          (setf result (append result body-result-instrs))))
    result))

(defun extend-compilation-env (env)
  "Create an extended copy of the compilation environment for a new scope.
   Shares the local-counter-box and local-types-box so child scopes can allocate locals."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields
   :blocks (cenv-blocks env)
   :block-depth (cenv-block-depth env)
   :tagbody-tags (cenv-tagbody-tags env)
   :tagbody-context (cenv-tagbody-context env)  ; Inherit tagbody context
   :catch-tags (cenv-catch-tags env)
   :unwind-stack (cenv-unwind-stack env)))

(defun copy-compilation-env (env)
  "Create a copy of the compilation environment with mutable fields copied.
   Used by block/return-from, tagbody/go, etc. for nested scopes."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields - copy lists for mutation safety
   :blocks (copy-list (cenv-blocks env))
   :block-depth (cenv-block-depth env)
   :tagbody-tags (copy-list (cenv-tagbody-tags env))
   :tagbody-context (cenv-tagbody-context env)  ; Reference (not copied for nesting)
   :catch-tags (copy-list (cenv-catch-tags env))
   :unwind-stack (copy-list (cenv-unwind-stack env))))

;;; ============================================================
;;; Flet/Labels Compilation (T089-T090)
;;; ============================================================

(defun compile-flet (ast env)
  "Compile a flet expression.
   Each local function is compiled as a lambda bound to a local variable.
   Functions in flet cannot call themselves (non-recursive)."
  (let* ((definitions (clysm/compiler/ast:ast-flet-definitions ast))
         (body (clysm/compiler/ast:ast-flet-body ast))
         (new-env (extend-compilation-env env))
         (result '())
         (local-func-bindings '()))
    ;; For each function definition:
    ;; 1. Create a lambda from the function
    ;; 2. Allocate a local for the closure
    ;; 3. Compile the lambda and store in local
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (func-body (third def))  ; List of parsed AST nodes
             ;; Allocate a local to hold the closure
             (local-idx (env-add-local new-env name)))
        ;; Remember this binding for later funcall translation
        (push (cons name local-idx) local-func-bindings)
        ;; Create a lambda AST node
        (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                           :parameters params
                           :body func-body)))
          ;; Compile the lambda (in original env, not new-env, so it can't see itself)
          (setf result (append result (compile-lambda lambda-ast env)))
          ;; Store in local
          (setf result (append result (list (list :local.set local-idx)))))))
    ;; Register local functions for funcall-style access
    (setf (cenv-local-functions new-env) (nreverse local-func-bindings))
    ;; Compile body
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form new-env)
                           '(:drop))))
    (when body
      (setf result (append result
                           (compile-to-instructions (car (last body)) new-env))))
    result))

(defun compile-labels (ast env)
  "Compile a labels expression.
   Functions in labels CAN call themselves and each other (recursive).
   Implementation: Two-phase closure creation to handle mutual recursion:
   1. Create closures with null env, store in locals
   2. Update env fields to point to the closures (now that they exist)"
  (let* ((definitions (clysm/compiler/ast:ast-labels-definitions ast))
         (body (clysm/compiler/ast:ast-labels-body ast))
         (new-env (extend-compilation-env env))
         (result '())
         (local-func-bindings '()))
    ;; First pass: allocate locals for all function closures
    (dolist (def definitions)
      (let* ((name (first def))
             (local-idx (env-add-local new-env name)))
        (push (cons name local-idx) local-func-bindings)))
    (setf local-func-bindings (nreverse local-func-bindings))
    ;; Register local functions so the body can call them
    (setf (cenv-local-functions new-env) local-func-bindings)
    ;; Phase 1: Create closures with null env initially
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (func-body (third def))
             (local-idx (cdr (assoc name local-func-bindings))))
        ;; Create lambda AST
        (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                           :parameters params
                           :body func-body)))
          ;; Compile the lambda - initially with null env
          ;; The func-names will be in captured-vars but we create with null env first
          (setf result (append result
                               (compile-labels-lambda-phase1 lambda-ast new-env local-func-bindings)))
          ;; Store in local
          (setf result (append result (list (list :local.set local-idx)))))))
    ;; Phase 2: Now update each closure's env field to point to the env cons-list
    ;; containing references to all the local function closures
    (dolist (def definitions)
      (let* ((name (first def))
             (local-idx (cdr (assoc name local-func-bindings))))
        ;; Get the closure
        (setf result (append result (list (list :local.get local-idx))))
        ;; Cast to closure type
        (setf result (append result
                             (list (list :ref.cast
                                         clysm/compiler/codegen/gc-types:+type-closure+))))
        ;; Build the env cons-list containing all local function closures
        (setf result (append result
                             (generate-labels-env-update local-func-bindings new-env)))
        ;; Update the env field (field 4) - struct.set $closure 4
        (setf result (append result
                             (list (list :struct.set
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         4))))))
    ;; Compile body
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form new-env)
                           '(:drop))))
    (when body
      (setf result (append result
                           (compile-to-instructions (car (last body)) new-env))))
    result))

(defun compile-labels-lambda-phase1 (ast env local-func-bindings)
  "Phase 1: Create a closure with null env - env will be filled in phase 2."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (body (clysm/compiler/ast:ast-lambda-body ast))
         ;; Collect regular free vars
         (regular-free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         ;; All local functions defined in labels
         (func-names (mapcar #'car local-func-bindings))
         ;; Check which functions are actually called in the body
         (called-funcs (collect-called-functions body))
         ;; Filter to only include function names that are actually called
         (used-func-names (intersection func-names called-funcs :test #'eq))
         ;; For labels, we need to capture the function closures themselves
         ;; Remove any func-names from regular free vars (they're handled separately)
         (free-vars (set-difference regular-free-vars func-names))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params)))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-names used-func-names
                :local-func-bindings local-func-bindings
                :func-index func-index
                :parent-env env
                :arity arity
                :is-labels-lambda t)
          *pending-lambdas*)
    ;; Create closure with null env (will be updated in phase 2)
    (let ((result '()))
      ;; code fields
      (if (= arity 0)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (if (= arity 1)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (if (= arity 2)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (setf result (append result (list (list :ref.func func-index))))
      ;; env - null initially, will be set in phase 2
      (setf result (append result '((:ref.null :any))))
      ;; Create the struct
      (setf result (append result
                           (list (list :struct.new
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      result)))

(defun generate-labels-env-update (local-func-bindings env)
  "Generate instructions to create the env cons-list for labels closures.
   Returns instructions that build a cons-list of all local function closures."
  (declare (ignore env))
  ;; Build cons-list from the local function closures
  ;; (cons f1 (cons f2 (cons f3 nil)))
  (let ((result '())
        (bindings (reverse local-func-bindings)))  ; Build from end
    (if (null bindings)
        '((:ref.null :none))
        (progn
          ;; Start with nil
          (setf result '((:ref.null :none)))
          ;; For each function (in reverse order)
          (dolist (binding bindings)
            (let ((local-idx (cdr binding)))
              ;; Stack: rest-of-list
              ;; Push the function closure
              (setf result (append (list (list :local.get local-idx)) result))
              ;; Stack: closure, rest-of-list
              ;; Note: Need to swap order for struct.new which takes car, cdr
              ;; Actually struct.new $cons takes (car, cdr) so we need car first
              ;; Current: closure, rest
              ;; Create cons: struct.new takes (car, cdr) in stack order = (cdr, car) push order
              ;; We want (cons closure rest), so push closure, then rest, then struct.new
              ;; But we have rest on stack already. Let me re-think...
              ))
          ;; Actually let me do this differently - push all closures, then build list
          (setf result '())
          ;; Push closures in order
          (dolist (binding local-func-bindings)
            (setf result (append result (list (list :local.get (cdr binding))))))
          ;; Now build the cons list from end
          ;; Stack: f1, f2, f3
          ;; We want: (cons f1 (cons f2 (cons f3 nil)))
          ;; Push nil
          (setf result (append result '((:ref.null :none))))
          ;; For each closure (in reverse), create cons
          (dotimes (i (length local-func-bindings))
            (setf result (append result
                                 (list (list :struct.new
                                             clysm/compiler/codegen/gc-types:+type-cons+)))))
          result))))

(defun collect-called-functions (body)
  "Collect all function names that are called in the body (list of AST nodes)."
  (let ((result '()))
    (labels ((collect (ast)
               (etypecase ast
                 (clysm/compiler/ast:ast-call
                  (let ((fn (clysm/compiler/ast:ast-call-function ast)))
                    (when (symbolp fn)
                      (pushnew fn result)))
                  (dolist (arg (clysm/compiler/ast:ast-call-arguments ast))
                    (collect arg)))
                 (clysm/compiler/ast:ast-if
                  (collect (clysm/compiler/ast:ast-if-test ast))
                  (collect (clysm/compiler/ast:ast-if-then ast))
                  (when (clysm/compiler/ast:ast-if-else ast)
                    (collect (clysm/compiler/ast:ast-if-else ast))))
                 (clysm/compiler/ast:ast-let
                  (dolist (b (clysm/compiler/ast:ast-let-bindings ast))
                    (collect (cdr b)))
                  (dolist (form (clysm/compiler/ast:ast-let-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-progn
                  (dolist (form (clysm/compiler/ast:ast-progn-forms ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-lambda
                  (dolist (form (clysm/compiler/ast:ast-lambda-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-block
                  (dolist (form (clysm/compiler/ast:ast-block-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-return-from
                  (when (clysm/compiler/ast:ast-return-from-value ast)
                    (collect (clysm/compiler/ast:ast-return-from-value ast))))
                 (clysm/compiler/ast:ast-setq
                  (collect (clysm/compiler/ast:ast-setq-value ast)))
                 ;; Terminals - no function calls
                 (clysm/compiler/ast:ast-literal nil)
                 (clysm/compiler/ast:ast-var-ref nil)
                 (clysm/compiler/ast:ast-defun nil))))
      (dolist (form body)
        (collect form)))
    result))

;;; ============================================================
;;; Progn Compilation
;;; ============================================================

(defun compile-progn (ast env)
  "Compile a progn expression.
   First scans for defuns and registers them, then compiles all forms."
  (let ((forms (clysm/compiler/ast:ast-progn-forms ast))
        (result '()))
    ;; First pass: register all defuns so they can be called
    (dolist (form forms)
      (when (typep form 'clysm/compiler/ast:ast-defun)
        (let ((name (clysm/compiler/ast:ast-defun-name form)))
          (unless (env-lookup-function env name)
            (env-add-function env name)))))
    ;; Second pass: compile all forms
    (dolist (form (butlast forms))
      (setf result (append result
                           (compile-to-instructions form env)
                           '(:drop))))
    (when forms
      (setf result (append result
                           (compile-to-instructions (car (last forms)) env))))
    (or result (compile-literal (clysm/compiler/ast:make-nil-literal)))))

;;; ============================================================
;;; Variable Assignment
;;; ============================================================

(defun compile-setq (ast env)
  "Compile a setq expression (T042).
   Handles both lexical (local) and special (dynamic) variables."
  (let* ((name (clysm/compiler/ast:ast-setq-name ast))
         (value (clysm/compiler/ast:ast-setq-value ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable - use local.tee for assignment + return value
      (local-idx
       (append
        (compile-to-instructions value env)
        (list (list :local.tee local-idx))))
      ;; Special variable - set symbol's value field (T042-T043)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-setq name value env))
      ;; Unknown variable
      (t
       (error "Cannot setq undefined variable: ~A" name)))))

(defun compile-special-setq (name value-form env)
  "Compile setq for a special variable (T043).
   Sets the symbol's value field and returns the new value.
   Pattern: global.get sym, <value>, local.tee temp, struct.set sym 1, local.get temp"
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (result-local (env-add-local env (gensym "setq-result"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Compile value, save to temp, then set symbol's value field
    (append
     ;; Get symbol reference
     `((:global.get ,global-idx))
     ;; Compile value and save for return
     (compile-to-instructions value-form env)
     ;; Save value to local for return
     `((:local.tee ,result-local)
       ;; Set symbol's value field
       (:struct.set ,symbol-type 1)
       ;; Return the value
       (:local.get ,result-local)))))

;;; ============================================================
;;; Function Definition (T059)
;;; ============================================================

(defun compile-defun (ast env)
  "Compile a function definition.
   Returns info about the compiled function."
  (let* ((name (clysm/compiler/ast:ast-defun-name ast))
         (params (clysm/compiler/ast:ast-defun-parameters ast))
         (body (clysm/compiler/ast:ast-defun-body ast))
         (func-env (make-env)))
    ;; Add parameters as locals
    (dolist (param params)
      (env-add-local func-env param))
    ;; Inherit function definitions
    (setf (cenv-functions func-env) (cenv-functions env))
    (setf (cenv-function-counter func-env) (cenv-function-counter env))
    ;; Compile body
    (let ((body-instrs '()))
      (dolist (form (butlast body))
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions form func-env)
                                  '(:drop))))
      (when body
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions (car (last body)) func-env))))
      ;; Return function info
      (list :name name
            :params (mapcar (lambda (p) (list p :anyref)) params)
            :result :anyref
            :locals (loop for i from (length params) below (cenv-local-counter func-env)
                          collect (list (gensym "local") (env-local-type func-env i)))
            :body body-instrs))))

;;; ============================================================
;;; Lambda Compilation (T081-T084)
;;; ============================================================

(defparameter *pending-lambdas* nil
  "List of lambda functions waiting to be compiled.
   Each entry is a plist: (:name :params :body :free-vars :func-index)")

(defparameter *lambda-counter* 0
  "Counter for generating unique lambda function names.")

(defun reset-lambda-state ()
  "Reset lambda compilation state."
  (setf *pending-lambdas* nil)
  (setf *lambda-counter* 0))

(defun allocate-lambda-function ()
  "Allocate a new lambda function index and return its name."
  (let ((name (intern (format nil "$LAMBDA-~D" (incf *lambda-counter*)))))
    name))

(defun compile-lambda (ast env)
  "Compile a lambda expression to create a closure struct.
   Returns instructions that push a closure reference onto the stack."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (body (clysm/compiler/ast:ast-lambda-body ast))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params)))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-index func-index
                :parent-env env
                :arity arity)
          *pending-lambdas*)
    ;; Generate code to create closure struct with captured environment
    (generate-closure-creation func-index arity free-vars env)))

(defun generate-closure-creation (func-index arity free-vars env)
  "Generate instructions to create a closure struct.
   The closure has code_0, code_1, code_2, code_N, and env fields.
   Free variables are captured in a cons-list stored in the env field."
  ;; struct.new $closure (code_0, code_1, code_2, code_N, env)
  (let ((result '()))
    ;; code_0 - null if arity != 0, ref.func if arity == 0
    (if (= arity 0)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_1 - null if arity != 1
    (if (= arity 1)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_2 - null if arity != 2
    (if (= arity 2)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_N - always the fallback
    (setf result (append result (list (list :ref.func func-index))))
    ;; env - capture free variables as a cons-list
    (if free-vars
        ;; Build cons-list: (cons var1 (cons var2 (cons var3 nil)))
        (setf result (append result (generate-env-capture free-vars env)))
        ;; No free vars, use null
        (setf result (append result '((:ref.null :any)))))
    ;; Create the struct
    (setf result (append result
                         (list (list :struct.new
                                     clysm/compiler/codegen/gc-types:+type-closure+))))
    result))

(defun generate-env-capture (free-vars env)
  "Generate instructions to capture free variables as a cons-list.
   Returns instructions that push a cons-list onto the stack.
   Each captured variable becomes (cons value rest)."
  (if (null free-vars)
      ;; Base case: nil (null reference)
      '((:ref.null :none))
      ;; Build cons from end: (cons first-var (cons second-var ...))
      (let ((result '())
            (var (first free-vars)))
        ;; Push the variable value
        (let ((local-idx (env-lookup-local env var)))
          (if local-idx
              (setf result (append result (list (list :local.get local-idx))))
              (error "Cannot capture unbound variable: ~A" var)))
        ;; Push rest of the list (recursive)
        (setf result (append result (generate-env-capture (rest free-vars) env)))
        ;; Create cons cell: struct.new $cons (car, cdr)
        (setf result (append result
                             (list (list :struct.new
                                         clysm/compiler/codegen/gc-types:+type-cons+))))
        result)))

(defun compile-pending-lambdas ()
  "Compile all pending lambda functions and return their definitions.
   Handles nested lambdas by repeatedly compiling until no more are pending."
  (let ((results '())
        (compiled (make-hash-table :test 'equal)))  ; Track compiled lambda names
    (loop while *pending-lambdas*
          do (let ((batch (reverse *pending-lambdas*)))
               (setf *pending-lambdas* nil)  ; Clear before compiling (new ones may be added)
               (dolist (lambda-info batch)
                 (let ((name (getf lambda-info :name)))
                   (unless (gethash name compiled)
                     (setf (gethash name compiled) t)
                     (let ((def (compile-lambda-body lambda-info)))
                       (push def results)))))))
    (nreverse results)))

(defun compile-lambda-body (lambda-info)
  "Compile a lambda function body."
  (let* ((name (getf lambda-info :name))
         (params (getf lambda-info :params))
         (body (getf lambda-info :body))
         (free-vars (getf lambda-info :free-vars))
         (func-names (getf lambda-info :func-names))  ; For labels lambdas
         (is-labels-lambda (getf lambda-info :is-labels-lambda))
         (parent-env (getf lambda-info :parent-env))
         (func-env (make-env)))
    ;; First parameter is the closure itself (for accessing env)
    (env-add-local func-env '$closure)
    ;; Add regular parameters
    (dolist (param params)
      (env-add-local func-env param))
    ;; Register captured variables - they will be accessed via env extraction
    ;; Store the mapping of captured var name -> position in cons-list
    ;; For labels lambdas, func-names come after free-vars
    (let ((position 0))
      (setf (cenv-captured-vars func-env)
            (append
             ;; Regular free variables first
             (loop for var in free-vars
                   collect (prog1 (cons var position) (incf position)))
             ;; Then captured function closures (for labels)
             (loop for fname in (or func-names '())
                   collect (prog1 (cons fname position) (incf position))))))
    ;; For labels lambdas, also set up local-functions so direct calls work
    (when is-labels-lambda
      ;; The captured functions can be accessed via captured-vars mechanism
      ;; but we also need to register them as local-functions for compile-call
      ;; Actually, they're in captured-vars, so compile-var-ref will find them
      ;; But compile-call needs them as local-functions for the call mechanism
      ;; Let's register them - they'll be extracted from captured vars on each call
      (setf (cenv-local-functions func-env)
            (loop for fname in (or func-names '())
                  for i from (length free-vars)
                  collect (cons fname :captured))))  ; Mark as captured, not local
    ;; Inherit function definitions
    (setf (cenv-functions func-env) (cenv-functions parent-env))
    (setf (cenv-function-counter func-env) (cenv-function-counter parent-env))
    ;; Compile body
    (let ((body-instrs '()))
      (dolist (form (butlast body))
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions form func-env)
                                  '(:drop))))
      (when body
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions (car (last body)) func-env))))
      ;; Return function info (with closure param first)
      (list :name name
            :params (cons '($closure :anyref)
                          (mapcar (lambda (p) (list p :anyref)) params))
            :result :anyref
            :locals (loop for i from (1+ (length params)) below (cenv-local-counter func-env)
                          collect (list (gensym "local") (env-local-type func-env i)))
            :body body-instrs))))

;;; ============================================================
;;; Block/Return-from Compilation (T107-T109)
;;; ============================================================

(defun compile-block (ast env)
  "Compile a block form.
   Uses Wasm block instruction for normal flow, with br for return-from.
   Pattern: (block :anyref body... end)"
  (let* ((name (clysm/compiler/ast:ast-block-name ast))
         (body (clysm/compiler/ast:ast-block-body ast))
         ;; Create new env with block registered
         (block-env (copy-compilation-env env))
         (result '()))
    ;; Register block at current depth (0 means we can br 0 to exit)
    (push (cons name 0) (cenv-blocks block-env))
    ;; Increment depth for any nested blocks
    (incf (cenv-block-depth block-env))
    ;; Start block with anyref result type
    (setf result (append result '((:block (:result :anyref)))))
    ;; Compile body forms (all but last are dropped)
    (dolist (form (butlast body))
      (setf result (append result (compile-to-instructions form block-env)))
      (setf result (append result '(:drop))))
    ;; Last form (or nil if empty)
    (if body
        (setf result (append result (compile-to-instructions (car (last body)) block-env)))
        (setf result (append result '((:ref.null :none)))))
    ;; End block
    (setf result (append result '(:end)))
    result))

(defun compile-return-from (ast env)
  "Compile return-from.
   Uses br to exit to the named block."
  (let* ((block-name (clysm/compiler/ast:ast-return-from-block-name ast))
         (value (clysm/compiler/ast:ast-return-from-value ast))
         (block-info (assoc block-name (cenv-blocks env))))
    (unless block-info
      (error "return-from: no block named ~A" block-name))
    (let* ((target-depth (cdr block-info))
           ;; Calculate relative br depth
           ;; If we're at depth 2 and block is at depth 0, br 2
           (br-depth (- (cenv-block-depth env) target-depth 1))
           (result '()))
      ;; Compile value
      (setf result (compile-to-instructions value env))
      ;; Branch to block exit
      (setf result (append result (list (list :br br-depth))))
      result)))

;;; ============================================================
;;; Tagbody/Go Strategy Analysis (T008-T010)
;;; ============================================================

(defun collect-go-targets-from-form (form)
  "Collect all go target tags from a single AST form.
   Recursively walks into nested forms (if, progn, etc.)."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (list (clysm/compiler/ast:ast-go-tag form)))
    (clysm/compiler/ast:ast-if
     (append (collect-go-targets-from-form (clysm/compiler/ast:ast-if-test form))
             (collect-go-targets-from-form (clysm/compiler/ast:ast-if-then form))
             (when (clysm/compiler/ast:ast-if-else form)
               (collect-go-targets-from-form (clysm/compiler/ast:ast-if-else form)))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           append (collect-go-targets-from-form subform)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           append (collect-go-targets-from-form subform)))
    ;; For other forms, no go targets inside
    (t nil)))

(defun collect-go-targets (segments)
  "Collect all go target tags from all segments.
   Returns a list of tag symbols that are targets of go statements."
  (loop for segment in segments
        append (loop for form in (cdr segment)
                     append (collect-go-targets-from-form form))))

(defun find-tag-segment-index (segments tag)
  "Find the segment index where TAG is defined.
   Returns nil if tag not found."
  (loop for segment in segments
        for idx from 0
        when (eq (car segment) tag)
        return idx))

(defun check-go-is-backward (segments go-form segment-idx form-position)
  "Check if a go from the given position is a backward jump.
   A backward jump targets a tag at or before the current position."
  (let* ((target-tag (clysm/compiler/ast:ast-go-tag go-form))
         (target-idx (find-tag-segment-index segments target-tag)))
    (when target-idx
      ;; Backward if target segment index is less than current segment index
      ;; OR if target is same segment but we're after the tag definition
      (or (< target-idx segment-idx)
          (and (= target-idx segment-idx) (> form-position 0))))))

(defun check-form-goes-backward (segments form segment-idx form-position tag)
  "Check if all goes to TAG in this form are backward jumps."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (if (eq (clysm/compiler/ast:ast-go-tag form) tag)
         (check-go-is-backward segments form segment-idx form-position)
         t)) ; Not targeting our tag, doesn't affect result
    (clysm/compiler/ast:ast-if
     (and (check-form-goes-backward segments (clysm/compiler/ast:ast-if-test form)
                                    segment-idx form-position tag)
          (check-form-goes-backward segments (clysm/compiler/ast:ast-if-then form)
                                    segment-idx form-position tag)
          (or (null (clysm/compiler/ast:ast-if-else form))
              (check-form-goes-backward segments (clysm/compiler/ast:ast-if-else form)
                                        segment-idx form-position tag))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (t t))) ; Other forms don't contain go

(defun all-goes-are-backward-p (segments tag)
  "Check if all go statements targeting TAG are backward jumps.
   Returns T if all goes to TAG are backward, NIL otherwise."
  (loop for segment in segments
        for seg-idx from 0
        always (loop for form in (cdr segment)
                     for form-pos from 0
                     always (check-form-goes-backward segments form seg-idx form-pos tag))))

(defun analyze-tagbody-strategy (segments)
  "Analyze tagbody segments and determine the compilation strategy.
   Returns one of:
   - :sequential - no go statements, just sequential execution
   - :simple-loop - single tag with backward jumps only (Wasm loop)
   - :dispatch - complex patterns requiring br_table dispatch"
  (let* ((tags (remove nil (mapcar #'car segments)))
         (go-targets (remove-duplicates (collect-go-targets segments))))
    (cond
      ;; No go statements -> sequential
      ((null go-targets) :sequential)
      ;; Single tag, single target, all backward -> simple loop
      ((and (= (length tags) 1)
            (= (length go-targets) 1)
            (eq (first tags) (first go-targets))
            (all-goes-are-backward-p segments (first tags)))
       :simple-loop)
      ;; Everything else -> dispatch
      (t :dispatch))))

;;; ============================================================
;;; Tagbody/Go Compilation (T110-T112)
;;; ============================================================

(defun compile-tagbody-sequential (segments env)
  "Compile tagbody with sequential strategy (no go statements).
   Simply compiles all forms in order and returns NIL."
  (let ((result '()))
    (dolist (segment segments)
      (let ((forms (cdr segment)))
        (dolist (form forms)
          (setf result (append result (compile-to-instructions form env)))
          (setf result (append result '(:drop))))))
    ;; Return NIL
    (append result '((:ref.null :none)))))

(defun compile-tagbody-simple-loop (segments env context)
  "Compile tagbody with simple-loop strategy.
   Used when there's a single tag with only backward jumps.
   Generates: (loop body... (br N for go where N accounts for nesting) ... end) NIL

   Structure:
   - loop (br 0 continues loop from direct context)
     - forms...
     - go -> br (current-depth - base-depth) to reach loop
   - end
   - ref.null none"
  (let* ((result '())
         (loop-env (copy-compilation-env env)))
    ;; Increment block depth for the loop
    (incf (cenv-block-depth loop-env))
    ;; Set up context for compile-go to find
    ;; Record the block depth at which the loop starts
    (setf (tagbody-context-base-block-depth context) (cenv-block-depth loop-env))
    (setf (tagbody-context-base-loop-depth context) 0)  ; br to loop is relative
    (setf (cenv-tagbody-context loop-env) context)
    ;; Start loop with no result type (void loop)
    (setf result '((:loop nil)))
    ;; Compile preamble (forms before first tag)
    (let ((first-segment (first segments)))
      (when (null (car first-segment))
        (dolist (form (cdr first-segment))
          (setf result (append result (compile-to-instructions form loop-env)))
          (setf result (append result '(:drop))))))
    ;; Compile the main tagged segment
    (dolist (segment segments)
      (when (car segment) ; Skip preamble, compile tagged segment
        (dolist (form (cdr segment))
          (setf result (append result (compile-to-instructions form loop-env)))
          (setf result (append result '(:drop))))))
    ;; End loop
    (setf result (append result '(:end)))
    ;; Return NIL
    (append result '((:ref.null :none)))))

(defun compile-tagbody-dispatch (segments env context)
  "Compile tagbody with dispatch strategy.
   Used for complex jump patterns (forward jumps, multiple tags).

   Structure (for N segments):
   block (exit block, depth N+1 from innermost)
     loop (dispatch loop, depth N from innermost)
       block (segment N-1, depth N-1 from innermost)
         block (segment N-2, depth N-2)
           ...
           block (segment 0, depth 0)
             br_table [N N-1 ... 1] 0 $pc
           end
           ;; segment 0 code here
           ;; go -> set $pc, br (depth to loop = N - seg_idx)
         end
         ;; segment 1 code here
       end
       ;; segment N-1 code here
       br (depth to exit = 1)
     end
   end"
  (let* ((result '())
         (num-segments (length segments))
         (pc-local (tagbody-context-pc-local context))
         (dispatch-env (copy-compilation-env env)))
    ;; Track block depth: exit block + loop + num-segments blocks
    ;; After br_table, we're inside all nested blocks
    ;; block-depth at segment 0 = env depth + 2 (exit, loop) + num-segments
    ;; But for go, we need depth relative to the loop, not absolute
    (incf (cenv-block-depth dispatch-env) 2)  ; exit block + loop
    ;; Set up context for compile-go
    (setf (cenv-tagbody-context dispatch-env) context)
    ;; Initialize $pc to 0 (stored as i31ref since all locals are anyref)
    (setf result (list (list :i32.const 0)
                       :ref.i31
                       (list :local.set pc-local)))
    ;; Start exit block (result anyref for NIL)
    (setf result (append result '((:block (:result :anyref)))))
    ;; Start dispatch loop with anyref result type
    ;; The loop never falls through (always exits via br 1 to exit block),
    ;; but the result type is needed for Wasm validation.
    (setf result (append result '((:loop (:result :anyref)))))
    ;; Generate nested blocks (innermost = segment 0)
    (dotimes (i num-segments)
      (setf result (append result '((:block nil)))))
    ;; Generate br_table
    ;; br_table[i] = i (segment index equals br depth to reach that segment's code)
    ;; When inside innermost block (seg 0), br i exits i blocks to segment i's code area
    (let ((br-targets (loop for i from 0 below num-segments collect i)))
      ;; Extract $pc from i31ref to i32 for br_table
      (setf result (append result (list (list :local.get pc-local)
                                        '(:ref.cast :i31)
                                        :i31.get_s)))
      ;; Default target is num-segments (exit all segment blocks, unreachable in normal operation)
      (setf result (append result (list (cons :br_table (append br-targets (list num-segments)))))))
    ;; Close blocks and generate segment code
    (loop for seg-idx from 0 below num-segments
          for segment in segments
          do (let ((segment-env (copy-compilation-env dispatch-env)))
               ;; Close the block for this segment FIRST
               (setf result (append result '(:end)))
               ;; Now segment code runs here, after closing (seg-idx + 1) blocks
               ;; Remaining nesting from segment seg-idx's code area:
               ;; - (num-segments - seg-idx - 1) more segment blocks to exit
               ;; - then the loop (which is the target for br)
               ;; So base-loop-depth = (num-segments - seg-idx - 1)
               (let ((remaining-blocks (- num-segments seg-idx 1)))
                 (setf (cenv-block-depth segment-env)
                       (+ (cenv-block-depth env) 2 remaining-blocks))
                 (setf (tagbody-context-base-block-depth (cenv-tagbody-context segment-env))
                       (cenv-block-depth segment-env))
                 (setf (tagbody-context-base-loop-depth (cenv-tagbody-context segment-env))
                       remaining-blocks))
               ;; Compile segment forms
               (dolist (form (cdr segment))
                 (setf result (append result (compile-to-instructions form segment-env)))
                 (setf result (append result '(:drop))))))
    ;; After all segments, exit the tagbody with NIL
    ;; We're inside the loop, exit block is at depth 1
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '((:br 1))))
    ;; Close dispatch loop
    (setf result (append result '(:end)))
    ;; Close exit block
    (setf result (append result '(:end)))
    result))

(defun compile-tagbody (ast env)
  "Compile tagbody using appropriate strategy based on analysis.
   Strategies:
   - :sequential - no go, just sequential execution
   - :simple-loop - single tag with backward jumps only
   - :dispatch - complex patterns with br_table"
  (let* ((segments (clysm/compiler/ast:ast-tagbody-segments ast))
         (strategy (analyze-tagbody-strategy segments))
         (tag-env (copy-compilation-env env)))
    ;; Register tags in legacy format for backward compatibility
    (loop for segment in segments
          for seg-idx from 0
          do (let ((tag (car segment)))
               (when tag
                 (push (cons tag seg-idx) (cenv-tagbody-tags tag-env)))))
    (case strategy
      (:sequential
       (compile-tagbody-sequential segments tag-env))
      (:simple-loop
       (let* ((context (make-tagbody-context
                        :strategy :simple-loop
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :base-loop-depth 0  ; br 0 targets the loop
                        :num-segments (length segments))))
         (compile-tagbody-simple-loop segments tag-env context)))
      (:dispatch
       (let* ((pc-local (env-add-local tag-env (gensym "pc")))
              (context (make-tagbody-context
                        :strategy :dispatch
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :pc-local pc-local
                        :num-segments (length segments))))
         (compile-tagbody-dispatch segments tag-env context))))))

(defun compile-go (ast env)
  "Compile go statement.
   Uses the tagbody-context from environment to determine how to jump.
   Calculates correct br depth based on current nesting within the tagbody."
  (let* ((tag (clysm/compiler/ast:ast-go-tag ast))
         (context (cenv-tagbody-context env)))
    (unless context
      (error "go outside tagbody: ~A" tag))
    (let ((tag-info (assoc tag (tagbody-context-tags context))))
      (unless tag-info
        (error "undefined tag: ~A" tag))
      (let ((segment-idx (cdr tag-info))
            (strategy (tagbody-context-strategy context))
            ;; Calculate how many blocks we're nested inside relative to the loop
            (nesting-depth (- (cenv-block-depth env)
                              (tagbody-context-base-block-depth context))))
        (case strategy
          (:simple-loop
           ;; For simple loop, branch back to the loop start
           ;; br depth = nesting depth (how many blocks to exit to reach loop)
           (list (list :br nesting-depth)))
          (:dispatch
           ;; For dispatch, set $pc (as i31ref) and branch to dispatch loop
           ;; br depth = nesting depth + base loop depth
           (let ((pc-local (tagbody-context-pc-local context))
                 (base-depth (tagbody-context-base-loop-depth context)))
             (list (list :i32.const segment-idx)
                   :ref.i31  ; wrap as i31ref to match local type
                   (list :local.set pc-local)
                   (list :br (+ nesting-depth base-depth)))))
          (t
           (error "go in sequential tagbody should not be compiled: ~A" tag)))))))

;;; ============================================================
;;; Catch/Throw Compilation (T113-T115)
;;; ============================================================

(defun compile-catch (ast env)
  "Compile catch form using Wasm exception handling.
   Delegates to compile-catch-simple for the actual implementation.
   Uses try_table with catch clause to get exception values directly."
  (compile-catch-simple ast env))

(defun compile-catch-simple (ast env)
  "Compile catch form using Wasm exception handling.

   The key challenge is that when a catch expression is used as a sub-expression
   inside another catch's body, and it catches an exception and returns a value,
   that value should be the result of the catch expression - sibling code should
   not continue.

   Structure with if/else directly producing the result (no br):

   block $catch (type 15)               ;; () -> (anyref anyref)
     try_table (result anyref) (catch 0 $catch)
       ...body...
     end
     ;; Normal path
     local.set $body_result
     ref.null none                      ;; dummy thrown-tag
     ref.null none                      ;; dummy thrown-value
     br $catch
   end  ; $catch - (anyref anyref) on stack

   ;; Handler code - both paths use if with (result anyref)
   local.set $thrown_value
   local.set $thrown_tag
   local.get $thrown_tag
   ref.is_null
   if (result anyref)                   ;; if normal (null tag)
     local.get $body_result
   else                                 ;; exception path
     <tag comparison>
     if (result anyref)                 ;; if tags match
       local.get $thrown_value
     else                               ;; tags don't match - rethrow
       local.get $thrown_tag
       local.get $thrown_value
       throw 0
     end
   end
   ;; Result anyref is now on stack - this IS the catch expression's value"
  (let* ((tag (clysm/compiler/ast:ast-catch-tag ast))
         (body (clysm/compiler/ast:ast-catch-body ast))
         (result '()))
    (let ((tag-local (env-add-local env (gensym "catch-tag")))
          (thrown-tag-local (env-add-local env (gensym "thrown-tag")))
          (thrown-value-local (env-add-local env (gensym "thrown-value")))
          (normal-result-local (env-add-local env (gensym "normal-result"))))
      ;; Evaluate and store the catch tag
      (setf result (append result (compile-to-instructions tag env)))
      (setf result (append result (list (list :local.set tag-local))))

      ;; Create catch environment
      (let ((catch-env (copy-compilation-env env)))
        (push (cons tag-local 0) (cenv-catch-tags catch-env))

        ;; Block $catch - type 15: () -> (anyref anyref)
        (setf result (append result '((:block (:type 15)))))  ;; $catch

        ;; try_table inside $catch
        ;; Catch clause: catch tag 0, branch to label 0 ($catch)
        (setf result (append result '((:try_table (:result :anyref) (:catch 0 0)))))

        ;; Compile body
        (dolist (form (butlast body))
          (setf result (append result (compile-to-instructions form catch-env)))
          (setf result (append result (list :drop))))
        (when body
          (setf result (append result (compile-to-instructions (car (last body)) catch-env))))
        (unless body
          (setf result (append result '((:ref.null :none)))))

        (setf result (append result (list :end)))  ;; end try_table

        ;; Normal path: store result, push (nil nil), branch to $catch
        (setf result (append result (list (list :local.set normal-result-local))))
        (setf result (append result '((:ref.null :none))))
        (setf result (append result '((:ref.null :none))))
        (setf result (append result '((:br 0))))  ;; br to $catch with (nil nil)

        (setf result (append result (list :end)))  ;; end $catch block

        ;; At this point: (anyref anyref) on stack
        ;; Either (nil nil) from normal path, or (tag value) from exception
        (setf result (append result (list (list :local.set thrown-value-local))))
        (setf result (append result (list (list :local.set thrown-tag-local))))

        ;; Check if this was a real exception (thrown-tag is not null)
        ;; Use if with (result anyref) so the result is directly on stack
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result (list :ref.is_null)))
        (setf result (append result '((:if (:result :anyref)))))

        ;; Normal case (null tag): return normal result
        (setf result (append result (list (list :local.get normal-result-local))))

        (setf result (append result (list :else)))

        ;; Exception case: compare tags and handle
        (setf result (append result (list (list :local.get tag-local))))
        (setf result (append result '((:ref.cast :eq))))
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result '((:ref.cast :eq))))
        (setf result (append result (list :ref.eq)))
        (setf result (append result '((:if (:result :anyref)))))

        ;; Tags match: return thrown-value
        (setf result (append result (list (list :local.get thrown-value-local))))

        (setf result (append result (list :else)))
        ;; Tags don't match: rethrow (throw never returns, so dummy value for type check)
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result (list (list :local.get thrown-value-local))))
        (setf result (append result '((:throw 0))))
        (setf result (append result (list :end)))  ;; end inner if

        (setf result (append result (list :end)))))  ;; end outer if
    ;; Result anyref is on stack - this is the catch expression's value
    result))

(defun compile-throw (ast env)
  "Compile throw using Wasm throw instruction.
   Throws exception with tag index 0 ($lisp-throw) with (tag-symbol, value) payload."
  (declare (ignore env))
  (let* ((tag (clysm/compiler/ast:ast-throw-tag ast))
         (value (clysm/compiler/ast:ast-throw-value ast))
         (result '()))
    ;; Compile tag expression (evaluates to symbol)
    (setf result (append result (compile-to-instructions tag env)))
    ;; Compile value expression
    (setf result (append result (compile-to-instructions value env)))
    ;; Throw with tag 0 ($lisp-throw)
    (setf result (append result '((:throw 0))))
    result))

;;; ============================================================
;;; Unwind-Protect Compilation (T116-T119)
;;; ============================================================

(defun compile-unwind-protect (ast env)
  "Compile unwind-protect with exception handling.
   Pattern:
   - Normal exit: execute protected form, save result, run cleanup, return result
   - Exception exit: catch any exception, run cleanup, rethrow

   Wasm structure:
   block (result anyref)                    ;; final result
     block (result exnref)                  ;; exception if caught
       try_table (result anyref) (catch_all_ref 0)
         ... protected form ...
       end
       local.set $result
       ... cleanup forms (drop values) ...
       local.get $result
       br 1                                 ;; branch to outer block
     end
     local.set $exnref
     ... cleanup forms (drop values) ...
     local.get $exnref
     throw_ref
   end"
  (let* ((protected-form (clysm/compiler/ast:ast-unwind-protect-protected-form ast))
         (cleanup-forms (clysm/compiler/ast:ast-unwind-protect-cleanup-forms ast))
         ;; Allocate locals
         (result-local (env-add-local env (gensym "unwind-result")))
         (exnref-local (env-add-local env (gensym "exnref") :exnref))
         (result '())
         (cleanup-code '()))
    ;; Pre-compile cleanup forms (we'll use this twice)
    (dolist (form cleanup-forms)
      (setf cleanup-code (append cleanup-code (compile-to-instructions form env)))
      (setf cleanup-code (append cleanup-code '(:drop))))

    ;; block (result anyref) - outer block for final result
    (setf result (append result '((:block (:result :anyref)))))

    ;; block (result exnref) - inner block for exception ref
    (setf result (append result '((:block (:result :exnref)))))

    ;; try_table (result anyref) (catch_all_ref 0)
    (setf result (append result (list (list :try_table '(:result :anyref)
                                            (list :catch_all_ref 0)))))  ; 0 = inner block

    ;; Compile protected form
    (setf result (append result (compile-to-instructions protected-form env)))

    ;; end try_table
    (setf result (append result '(:end)))

    ;; Normal path: save result, run cleanup, return result, br 1 (to outer block)
    (setf result (append result (list (list :local.set result-local))))
    (setf result (append result cleanup-code))
    (setf result (append result (list (list :local.get result-local))))
    (setf result (append result '((:br 1))))  ; br to outer block (skip exception path)

    ;; end inner block
    (setf result (append result '(:end)))

    ;; Exception path: save exnref, run cleanup, throw_ref
    (setf result (append result (list (list :local.set exnref-local))))
    (setf result (append result cleanup-code))
    (setf result (append result (list (list :local.get exnref-local))))
    (setf result (append result '((:throw_ref))))

    ;; end outer block
    (setf result (append result '(:end)))

    result))

;;; ============================================================
;;; Special Variable Binding Support (T009, T034)
;;; ============================================================

(defun generate-restore-binding-instructions ()
  "Generate instructions for the $restore-binding helper (T009).
   Restores the top binding frame: pops the frame, restores old value.

   Pattern:
   (global.get $binding_stack)    ; Get top frame
   (local.tee $frame)
   (struct.get $binding_frame $old_value)  ; Get old value
   (local.get $frame)
   (struct.get $binding_frame $symbol)     ; Get symbol
   (struct.set $symbol $value)             ; Restore value to symbol
   (local.get $frame)
   (struct.get $binding_frame $prev)       ; Get previous frame
   (global.set $binding_stack)             ; Pop frame"
  (let ((frame-local 0)   ; Temporary local for frame reference
        (binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Return instruction list for restore-binding logic
    `(;; Get and save the current binding frame
      (:global.get $binding_stack)
      (:local.tee ,frame-local)
      ;; Get old value from frame (field 1)
      (:struct.get ,binding-frame-type 1)
      ;; Get symbol from frame (field 0)
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 0)
      ;; Restore value to symbol's $value field (field 1)
      (:struct.set ,symbol-type 1)
      ;; Pop the frame: set binding_stack to frame.prev
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 2)  ; Get $prev field
      (:global.set $binding_stack))))

(defun generate-push-binding-frame (symbol-global-name value-instructions)
  "Generate instructions to push a new binding frame (T031).
   Saves the current value and sets a new one.

   Parameters:
   - symbol-global-name: Global variable name for the symbol
   - value-instructions: Instructions that produce the new value

   Pattern:
   1. Create binding frame: (symbol, old_value, prev)
   2. Push to binding stack
   3. Set new value"
  (let ((binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    `(;; Create binding frame with: symbol, old_value, prev
      ;; Field 0: symbol reference
      (:global.get ,symbol-global-name)
      ;; Field 1: old value (current symbol value)
      (:global.get ,symbol-global-name)
      (:struct.get ,symbol-type 1)
      ;; Field 2: prev (current binding stack)
      (:global.get $binding_stack)
      ;; Create the frame struct
      (:struct.new ,binding-frame-type)
      ;; Push to binding stack
      (:global.set $binding_stack)
      ;; Set new value on symbol
      (:global.get ,symbol-global-name)
      ,@value-instructions
      (:struct.set ,symbol-type 1))))

;;; ============================================================
;;; Special Variable Global Tracking (T025)
;;; ============================================================

(defvar *special-var-globals* (make-hash-table :test 'eq)
  "Maps special variable names to their global indices.")

(defvar *next-special-global-index* 2
  "Next available global index for special variables.
   Starts at 2 because 0=NIL, 1=UNBOUND.")

(defun reset-special-var-globals ()
  "Reset special variable global tracking for new compilation."
  (clrhash *special-var-globals*)
  (setf *next-special-global-index* 2))

(defun allocate-special-var-global (name)
  "Allocate a global index for a special variable.
   Returns the index, creating a new one if needed."
  (or (gethash name *special-var-globals*)
      (setf (gethash name *special-var-globals*)
            (prog1 *next-special-global-index*
              (incf *next-special-global-index*)))))

(defun get-special-var-global-index (name)
  "Get the global index for a special variable.
   Returns NIL if not allocated."
  (gethash name *special-var-globals*))

(defun get-all-special-var-globals ()
  "Get all allocated special variable globals as an alist.
   Returns ((name . index) ...) sorted by index."
  (let ((result '()))
    (maphash (lambda (name index)
               (push (cons name index) result))
             *special-var-globals*)
    (sort result #'< :key #'cdr)))

;;; ============================================================
;;; Special Variable Definition Compilation (T022-T025)
;;; ============================================================

(defun make-symbol-global-name (name)
  "Create a global variable name for a symbol.
   Example: *foo* -> $sym_FOO"
  (intern (format nil "$sym_~A" (string-upcase (symbol-name name)))))

(defun compile-defvar (ast env)
  "Compile a defvar form (T022-T023).
   defvar only initializes if the variable is currently unbound.
   Symbols are initialized with null value fields (via struct.new_default),
   so we check for null to determine if uninitialized.

   Generated code pattern:
   1. Get symbol's current value
   2. Check if it's null (uninitialized)
   3. If null, set the initial value
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defvar-name ast))
         (init-form (clysm/compiler/ast:ast-defvar-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (if init-form
        ;; With init-form: conditionally initialize
        ;; Check if symbol.value is null (uninitialized), if so set value
        (append
         ;; Get symbol's value field
         `((:global.get ,global-idx)
           (:struct.get ,symbol-type 1)  ; Get value field
           ;; Check if it's null (uninitialized)
           :ref.is_null
           ;; If null, initialize the value
           (:if (:result :anyref)))
         ;; Then branch: set new value
         `((:global.get ,global-idx))
         (compile-to-instructions init-form env)
         `((:struct.set ,symbol-type 1)
           ;; Return the symbol
           (:global.get ,global-idx)
           :else
           ;; Else branch: already bound, just return symbol
           (:global.get ,global-idx)
           :end))
        ;; Without init-form: just ensure the symbol exists (no-op at runtime)
        `((:global.get ,global-idx)))))

(defun compile-defparameter (ast env)
  "Compile a defparameter form (T024).
   Unlike defvar, defparameter always initializes the variable.

   Generated code pattern:
   1. Get symbol reference
   2. Compile init form
   3. Set symbol's value field
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defparameter-name ast))
         (init-form (clysm/compiler/ast:ast-defparameter-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Always set the value (unlike defvar)
    `(;; Set new value
      (:global.get ,global-idx)
      ,@(compile-to-instructions init-form env)
      (:struct.set ,symbol-type 1)
      ;; Return the symbol
      (:global.get ,global-idx))))

;;; ============================================================
;;; Function Section Generation
;;; ============================================================

(defun generate-func-section (funcs)
  "Generate Wasm Function Section content from compiled functions."
  ;; Function section contains type indices for each function
  (mapcar (lambda (f)
            (declare (ignore f))
            0)  ; Type index (placeholder)
          funcs))

(defun compile-expression (expr env)
  "Compile a Lisp expression to Wasm instructions.
   Main entry point for expression compilation."
  (let ((ast (clysm/compiler/ast:parse-expr expr)))
    (compile-to-instructions ast env)))
