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
  (functions nil :type list)           ; ((name . index) ...)
  (function-counter 0 :type fixnum)
  (in-tail-position nil :type boolean)
  (captured-vars nil :type list)       ; ((name . position) ...) for captured vars
  (local-functions nil :type list)     ; ((name . local-idx) ...) for flet/labels
  ;; Phase 6: Exception handling
  (blocks nil :type list)              ; ((name . block-depth) ...) for block/return-from
  (block-depth 0 :type fixnum)         ; Current nesting depth for br targets
  (tagbody-tags nil :type list)        ; ((tag . label-idx) ...) for tagbody/go
  (catch-tags nil :type list)          ; ((tag-expr . handler-depth) ...) for catch/throw
  (unwind-stack nil :type list))       ; Stack of unwind-protect handlers

(defun make-env ()
  "Create a fresh compilation environment."
  (make-compilation-env :local-counter-box (list 0)))

(defun cenv-local-counter (env)
  "Get the local counter value."
  (car (cenv-local-counter-box env)))

(defun (setf cenv-local-counter) (value env)
  "Set the local counter value."
  (setf (car (cenv-local-counter-box env)) value))

(defun env-lookup-local (env name)
  "Look up a local variable index."
  (cdr (assoc name (cenv-locals env))))

(defun env-add-local (env name)
  "Add a local variable and return its index."
  (let ((idx (cenv-local-counter env)))
    (push (cons name idx) (cenv-locals env))
    (incf (car (cenv-local-counter-box env)))
    idx))

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
    (:ref.eq . #xD5)
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
     (compile-unwind-protect ast env))))

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
   Handles locals, captured variables from closures, and globals."
  (let* ((name (clysm/compiler/ast:ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable
      (local-idx
       (list (list :local.get local-idx)))
      ;; Captured variable from closure environment
      ((env-lookup-captured env name)
       (compile-captured-var-access name env))
      ;; Unknown variable
      (t
       (error "Unbound variable: ~A" name)))))

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
  "Compile an if expression."
  (let ((test (clysm/compiler/ast:ast-if-test ast))
        (then-branch (clysm/compiler/ast:ast-if-then ast))
        (else-branch (clysm/compiler/ast:ast-if-else ast)))
    (append
     ;; Compile test
     (compile-to-instructions test env)
     ;; Check if not NIL
     (compile-nil-check)
     ;; If-then-else
     '((:if (:result :anyref)))
     (compile-to-instructions then-branch env)
     '(:else)
     (compile-to-instructions else-branch env)
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

(defun compile-let (ast env)
  "Compile a let or let* expression."
  (let ((bindings (clysm/compiler/ast:ast-let-bindings ast))
        (body (clysm/compiler/ast:ast-let-body ast))
        (sequential-p (clysm/compiler/ast:ast-let-sequential-p ast))
        (new-env (extend-compilation-env env))
        (result '())
        (local-indices '()))
    ;; For let*, bindings are sequential; for let, parallel
    (if sequential-p
        ;; let*: each binding sees previous
        (dolist (binding bindings)
          (let* ((name (car binding))
                 (value (cdr binding))
                 (idx (env-add-local new-env name)))
            (push idx local-indices)
            (setf result (append result
                                 (compile-to-instructions value new-env)
                                 (list (list :local.set idx))))))
        ;; let: all values computed before any binding
        (progn
          ;; First allocate all locals
          (dolist (binding bindings)
            (let ((name (car binding)))
              (push (env-add-local new-env name) local-indices)))
          (setf local-indices (nreverse local-indices))
          ;; Then compile values and set
          (loop for binding in bindings
                for idx in local-indices
                do (let ((value (cdr binding)))
                     (setf result (append result
                                          (compile-to-instructions value env)
                                          (list (list :local.set idx))))))))
    ;; Compile body
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form new-env)
                           '(:drop))))
    (when body
      (setf result (append result
                           (compile-to-instructions (car (last body)) new-env))))
    result))

(defun extend-compilation-env (env)
  "Create an extended copy of the compilation environment for a new scope.
   Shares the local-counter-box so child scopes can allocate locals."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields
   :blocks (cenv-blocks env)
   :block-depth (cenv-block-depth env)
   :tagbody-tags (cenv-tagbody-tags env)
   :catch-tags (cenv-catch-tags env)
   :unwind-stack (cenv-unwind-stack env)))

(defun copy-compilation-env (env)
  "Create a copy of the compilation environment with mutable fields copied.
   Used by block/return-from, tagbody/go, etc. for nested scopes."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields - copy lists for mutation safety
   :blocks (copy-list (cenv-blocks env))
   :block-depth (cenv-block-depth env)
   :tagbody-tags (copy-list (cenv-tagbody-tags env))
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
  "Compile a setq expression."
  (let* ((name (clysm/compiler/ast:ast-setq-name ast))
         (value (clysm/compiler/ast:ast-setq-value ast))
         (local-idx (env-lookup-local env name)))
    (if local-idx
        (append
         (compile-to-instructions value env)
         (list (list :local.tee local-idx)))
        (error "Cannot setq undefined variable: ~A" name))))

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
                          collect (list (gensym "local") :anyref))
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
                          collect (list (gensym "local") :anyref))
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
;;; Tagbody/Go Compilation (T110-T112)
;;; ============================================================

(defun compile-tagbody (ast env)
  "Compile tagbody.
   MVP implementation: just compile all forms sequentially.
   Go support requires loop/dispatch mechanism.

   For simple tagbody without go, this just executes all forms
   and returns NIL."
  (let* ((segments (clysm/compiler/ast:ast-tagbody-segments ast))
         (tag-env (copy-compilation-env env))
         (result '()))
    ;; Register tags (for go lookup - MVP: error if go is used)
    (loop for segment in segments
          for seg-idx from 0
          do (let ((tag (car segment)))
               (when tag
                 (push (cons tag seg-idx) (cenv-tagbody-tags tag-env)))))
    ;; Compile all segment forms sequentially
    (dolist (segment segments)
      (let ((forms (cdr segment)))
        (dolist (form forms)
          (setf result (append result (compile-to-instructions form tag-env)))
          (setf result (append result '(:drop))))))
    ;; Return NIL
    (setf result (append result '((:ref.null :none))))
    result))

(defun compile-go (ast env)
  "Compile go.
   MVP: Not fully implemented - signals error."
  (declare (ignore env))
  (let ((tag (clysm/compiler/ast:ast-go-tag ast)))
    (error "go is not yet implemented (tag: ~A). Tagbody without go works." tag)))

;;; ============================================================
;;; Catch/Throw Compilation (T113-T115)
;;; ============================================================

(defun compile-catch (ast env)
  "Compile catch form.
   For now, uses simple pattern matching. Full exception handling
   requires Wasm exception handling proposal (try_table/throw)."
  (let* ((tag (clysm/compiler/ast:ast-catch-tag ast))
         (body (clysm/compiler/ast:ast-catch-body ast))
         (result '()))
    ;; For MVP: Implement as block with special handling
    ;; Full implementation would use try_table
    ;; Compile tag and save to local for comparison
    (let ((tag-local (env-add-local env (gensym "catch-tag"))))
      (setf result (append result (compile-to-instructions tag env)))
      (setf result (append result (list (list :local.set tag-local))))
      ;; Create catch environment
      (let ((catch-env (copy-compilation-env env)))
        (push (cons tag-local 0) (cenv-catch-tags catch-env))
        ;; Compile body as block
        (setf result (append result '((:block (:result :anyref)))))
        (dolist (form (butlast body))
          (setf result (append result (compile-to-instructions form catch-env)))
          (setf result (append result '(:drop))))
        (when body
          (setf result (append result (compile-to-instructions (car (last body)) catch-env))))
        (unless body
          (setf result (append result '((:ref.null :none)))))
        (setf result (append result '(:end)))))
    result))

(defun compile-throw (ast env)
  "Compile throw.
   For now, simplified implementation. Full version needs exception handling."
  (let* ((tag (clysm/compiler/ast:ast-throw-tag ast))
         (value (clysm/compiler/ast:ast-throw-value ast))
         (result '()))
    ;; For MVP: Just return the value (simplified)
    ;; Full implementation would use throw instruction
    (setf result (compile-to-instructions value env))
    ;; For now, we also need to handle the unwinding
    ;; This requires more infrastructure
    result))

;;; ============================================================
;;; Unwind-Protect Compilation (T116-T119)
;;; ============================================================

(defun compile-unwind-protect (ast env)
  "Compile unwind-protect.
   Pattern: Execute protected form, then cleanup forms, return protected value.
   On exception: catch, run cleanup, rethrow."
  (let* ((protected-form (clysm/compiler/ast:ast-unwind-protect-protected-form ast))
         (cleanup-forms (clysm/compiler/ast:ast-unwind-protect-cleanup-forms ast))
         ;; Allocate local to save protected form's result
         (result-local (env-add-local env (gensym "unwind-result")))
         (result '()))
    ;; For MVP: Simple implementation without exception handling
    ;; Compile protected form and save result
    (setf result (append result (compile-to-instructions protected-form env)))
    (setf result (append result (list (list :local.set result-local))))
    ;; Compile cleanup forms (drop their values)
    (dolist (form cleanup-forms)
      (setf result (append result (compile-to-instructions form env)))
      (setf result (append result '(:drop))))
    ;; Return saved result
    (setf result (append result (list (list :local.get result-local))))
    result))

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
