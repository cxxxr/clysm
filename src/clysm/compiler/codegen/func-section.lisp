;;;; func-section.lisp - Function and Code Section generation
;;;; Compiles AST nodes to Wasm instructions

(in-package #:clysm/compiler/codegen/func-section)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct (compilation-env (:conc-name cenv-))
  "Compilation environment tracking locals and scope."
  (locals nil :type list)           ; ((name . index) ...)
  (local-counter 0 :type fixnum)
  (functions nil :type list)        ; ((name . index) ...)
  (function-counter 0 :type fixnum)
  (in-tail-position nil :type boolean))

(defun make-env ()
  "Create a fresh compilation environment."
  (make-compilation-env))

(defun env-lookup-local (env name)
  "Look up a local variable index."
  (cdr (assoc name (cenv-locals env))))

(defun env-add-local (env name)
  "Add a local variable and return its index."
  (let ((idx (cenv-local-counter env)))
    (push (cons name idx) (cenv-locals env))
    (incf (cenv-local-counter env))
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
    (:struct.set . (#xFB #x05))))

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
     (compile-lambda ast env))))

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
       ;; NIL is a global singleton (T039)
       (list (list :global.get 0)))  ; global 0 = NIL
      (:t
       ;; T is represented as non-null (could use a global or just non-NIL i31ref)
       ;; For simplicity, use i31ref of 1
       (list '(:i32.const 1)
             :ref.i31))
      (otherwise
       (error "Unsupported literal type: ~A" type)))))

;;; ============================================================
;;; Variable Reference Compilation (T048)
;;; ============================================================

(defun compile-var-ref (ast env)
  "Compile a variable reference."
  (let* ((name (clysm/compiler/ast:ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (if local-idx
        (list (list :local.get local-idx))
        ;; Not a local, check globals or error
        (error "Unbound variable: ~A" name))))

;;; ============================================================
;;; Function Call Compilation (T049-T052)
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((function (clysm/compiler/ast:ast-call-function ast))
        (args (clysm/compiler/ast:ast-call-arguments ast)))
    ;; Check for primitive operators
    (if (and (symbolp function)
             (member function '(+ - * / < > <= >= = /= truncate)))
        (compile-primitive-call function args env)
        ;; Regular function call
        (compile-regular-call function args env))))

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
  "Compile an arithmetic operation with variadic args."
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
       ;; Compile first arg
       (setf result (append result (compile-to-instructions (first args) env)))
       ;; Unwrap i31ref
       (push :i31.get_s result)
       ;; For each remaining arg
       (dolist (arg (rest args))
         (setf result (append result (compile-to-instructions arg env)))
         (push :i31.get_s result)
         (push op result))
       ;; Wrap result as i31ref
       (push :ref.i31 result)
       (nreverse result)))))

(defun compile-unary-minus (arg env)
  "Compile unary minus: (- x) => (- 0 x)."
  (append
   '((:i32.const 0))
   (compile-to-instructions arg env)
   '(:i31.get_s
     :i32.sub
     :ref.i31)))

(defun compile-truncate (args env)
  "Compile truncate division."
  (when (< (length args) 2)
    (error "truncate requires two arguments"))
  (append
   (compile-to-instructions (first args) env)
   '(:i31.get_s)
   (compile-to-instructions (second args) env)
   '(:i31.get_s
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
   '(:i31.get_s)
   (compile-to-instructions (second args) env)
   '(:i31.get_s)
   (list op)
   ;; Convert i32 boolean to Lisp boolean (T or NIL)
   '((:if (:result anyref))
     (:i32.const 1) :ref.i31  ; T
     :else
     (:global.get 0)          ; NIL
     :end)))

(defun compile-not-equal (args env)
  "Compile not-equal."
  (append
   (compile-to-instructions (first args) env)
   '(:i31.get_s)
   (compile-to-instructions (second args) env)
   '(:i31.get_s
     :i32.ne)
   '((:if (:result anyref))
     (:i32.const 1) :ref.i31
     :else
     (:global.get 0)
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
     '((:if (:result anyref)))
     (compile-to-instructions then-branch env)
     '(:else)
     (compile-to-instructions else-branch env)
     '(:end))))

(defun compile-nil-check ()
  "Generate code to check if TOS is not NIL.
   Returns i32: 1 if not-nil, 0 if nil."
  ;; Compare with NIL global using ref.eq, then invert
  '((:global.get 0)  ; Get NIL
    :ref.eq          ; Compare
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
  "Create an extended copy of the compilation environment for a new scope."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter (cenv-local-counter env)
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)))

;;; ============================================================
;;; Progn Compilation
;;; ============================================================

(defun compile-progn (ast env)
  "Compile a progn expression."
  (let ((forms (clysm/compiler/ast:ast-progn-forms ast))
        (result '()))
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
;;; Lambda Compilation
;;; ============================================================

(defun compile-lambda (ast env)
  "Compile a lambda expression (creates a closure)."
  (declare (ignore ast env))
  ;; TODO: Implement closures in Phase 5
  (error "Lambda expressions not yet supported"))

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
