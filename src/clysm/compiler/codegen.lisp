;;;; compiler/codegen.lisp - Code Generation
;;;;
;;;; Generates WebAssembly bytecode from AST nodes.

(in-package #:clysm)

;;; ============================================================
;;; Code Generation Context
;;; ============================================================

(defstruct (codegen-context (:constructor %make-codegen-context))
  "Context for code generation."
  module          ; The wasm-module being built
  type-registry   ; Type registry for this module
  (functions nil) ; List of generated functions
  (pending nil)   ; Pending lambda bodies to compile
  (lambda-counter 0))  ; Counter for generating unique lambda names

(defun make-codegen-context (&key module type-registry)
  "Create a new codegen context."
  (%make-codegen-context :module module
                         :type-registry type-registry))

;;; ============================================================
;;; Main Code Generation Entry Point
;;; ============================================================

(defun compile-toplevel (form &optional context)
  "Compile a top-level form.
Returns the codegen context with the compiled form."
  (unless context
    (let ((module (make-wasm-module)))
      (setf context (make-codegen-context
                     :module module
                     :type-registry (register-core-types module)))))
  (let ((ast (parse-sexp form)))
    (compile-toplevel-ast ast context))
  context)

(defun compile-toplevel-ast (ast context)
  "Compile a top-level AST node."
  (etypecase ast
    (ast-defun
     (compile-defun ast context))
    (ast-defvar
     (compile-defvar ast context))
    (t
     ;; For other forms, wrap in an anonymous function
     (compile-expression-as-function ast context))))

;;; ============================================================
;;; Expression Compilation
;;; ============================================================

(defun compile-expression (ast env)
  "Compile an expression AST node.
Returns a list of Wasm bytecode."
  (etypecase ast
    (ast-literal
     (compile-literal ast env))
    (ast-quote
     (compile-quote ast env))
    (ast-var
     (compile-var ast env))
    (ast-setq
     (compile-setq ast env))
    (ast-if
     (compile-if ast env))
    (ast-progn
     (compile-progn ast env))
    (ast-let
     (compile-let ast env))
    (ast-lambda
     (compile-lambda ast env))
    (ast-call
     (compile-call ast env))
    (ast-primitive-call
     (compile-primitive-call ast env))
    (ast-block
     (compile-block ast env))
    (ast-return-from
     (compile-return-from ast env))))

;;; ============================================================
;;; Literal Compilation
;;; ============================================================

(defun compile-literal (ast env)
  "Compile a literal value."
  (declare (ignore env))
  (let ((value (ast-literal-value ast))
        (type (ast-literal-type ast)))
    (case type
      (:fixnum
       ;; Wrap integer in i31ref
       (append (emit-i32.const value)
               (emit-ref.i31)))
      (:character
       ;; Characters are also i31ref (character code)
       (append (emit-i32.const (char-code value))
               (emit-ref.i31)))
      (:string
       ;; Strings need more complex handling - placeholder for now
       ;; TODO: Implement string literals via data section
       (error "String literals not yet implemented"))
      (t
       ;; Infer type
       (cond
         ((integerp value)
          (append (emit-i32.const value)
                  (emit-ref.i31)))
         ((characterp value)
          (append (emit-i32.const (char-code value))
                  (emit-ref.i31)))
         (t
          (error "Unknown literal type: ~S" value)))))))

(defun compile-quote (ast env)
  "Compile a quoted form."
  (let ((value (ast-quote-value ast)))
    (cond
      ;; NIL
      ((null value)
       (compile-nil env))
      ;; T
      ((eq value t)
       (compile-t env))
      ;; Numbers
      ((integerp value)
       (append (emit-i32.const value)
               (emit-ref.i31)))
      ;; Symbols - need symbol table lookup
      ((symbolp value)
       ;; TODO: Implement symbol interning
       (error "Symbol literals not yet implemented: ~S" value))
      ;; Lists - need to construct at runtime
      ((consp value)
       ;; TODO: Implement list literal construction
       (error "List literals not yet implemented: ~S" value))
      (t
       (error "Cannot compile quoted value: ~S" value)))))

(defun compile-nil (env)
  "Compile a reference to NIL."
  ;; TODO: Return reference to NIL global
  ;; For now, use ref.null with nil type
  (declare (ignore env))
  (list (opcode :ref.null) #x6E))  ; null anyref

(defun compile-t (env)
  "Compile a reference to T."
  ;; TODO: Return reference to T symbol
  ;; For now, use i31ref with 1
  (declare (ignore env))
  (append (emit-i32.const 1)
          (emit-ref.i31)))

;;; ============================================================
;;; Variable Compilation
;;; ============================================================

(defun compile-var (ast env)
  "Compile a variable reference."
  (let ((name (ast-var-name ast)))
    (multiple-value-bind (binding found-p)
        (env-lookup env name)
      (if found-p
          (ecase (binding-kind binding)
            (:local
             (emit-local.get (binding-index binding)))
            (:param
             (emit-local.get (binding-index binding)))
            (:closure
             ;; Get from closure environment
             (let ((registry (compile-env-type-registry env)))
               (append (emit-local.get 0)  ; env is always local 0
                       (emit-i32.const (binding-index binding))
                       (emit-env-ref registry))))
            (:global
             ;; Get from symbol's value slot
             ;; TODO: Implement global variable access
             (error "Global variable access not yet implemented: ~S" name)))
          ;; Not found - treat as global
          (error "Undefined variable: ~S" name)))))

(defun compile-setq (ast env)
  "Compile variable assignment."
  (let ((name (ast-setq-name ast))
        (value-code (compile-expression (ast-setq-value ast)
                                        (env-not-in-tail-position env))))
    (multiple-value-bind (binding found-p)
        (env-lookup env name)
      (if found-p
          (ecase (binding-kind binding)
            ((:local :param)
             (append value-code
                     ;; Duplicate value for return
                     ;; (for now, setq doesn't return the value properly)
                     (emit-local.set (binding-index binding))))
            (:closure
             (let ((registry (compile-env-type-registry env)))
               (append (emit-local.get 0)  ; env
                       (emit-i32.const (binding-index binding))
                       value-code
                       (emit-env-set registry))))
            (:global
             (error "Global variable assignment not yet implemented: ~S" name)))
          (error "Undefined variable: ~S" name)))))

;;; ============================================================
;;; Control Flow Compilation
;;; ============================================================

(defun compile-if (ast env)
  "Compile conditional expression."
  (let* ((test-code (compile-expression (ast-if-test ast)
                                        (env-not-in-tail-position env)))
         (then-code (compile-expression (ast-if-then ast) env))
         (else-code (if (ast-if-else ast)
                        (compile-expression (ast-if-else ast) env)
                        (compile-nil env))))
    (append
     ;; Evaluate test
     test-code
     ;; Convert to i32 for branch (check if not nil)
     ;; For now, assume test returns i31ref, check if non-zero
     (emit-i31.get-s)
     ;; if instruction with anyref result
     (list (opcode :if))
     (encode-blocktype :anyref)  ; result type
     ;; then branch
     then-code
     ;; else
     (list (opcode :else))
     else-code
     ;; end
     (emit-end))))

(defun compile-progn (ast env)
  "Compile sequential execution."
  (let ((forms (ast-progn-forms ast)))
    (cond
      ((null forms)
       (compile-nil env))
      ((null (cdr forms))
       (compile-expression (car forms) env))
      (t
       (let ((non-tail-env (env-not-in-tail-position env)))
         (append
          ;; Compile all but last, dropping results
          (loop for form in (butlast forms)
                append (append (compile-expression form non-tail-env)
                               (list (opcode :drop))))
          ;; Compile last form in original position
          (compile-expression (lastcar forms) env)))))))

(defun compile-block (ast env)
  "Compile a named block."
  (let* ((name (ast-block-name ast))
         (label (env-push-block env name))
         (body-code (compile-expression (ast-block-body ast) env)))
    (declare (ignore label))
    (env-pop-block env)
    ;; Use try_table for non-local returns
    ;; For now, simplified: just compile body
    body-code))

(defun compile-return-from (ast env)
  "Compile return from a named block."
  (let ((name (ast-return-from-name ast))
        (value-code (compile-expression (ast-return-from-value ast)
                                        (env-not-in-tail-position env))))
    (multiple-value-bind (label depth)
        (env-find-block env name)
      (unless label
        (error "Block not found: ~S" name))
      ;; Use br instruction to jump to block
      (append value-code
              (list (opcode :br))
              (encode-uleb128 depth)))))

;;; ============================================================
;;; Binding Forms Compilation
;;; ============================================================

(defun compile-let (ast env)
  "Compile let/let* binding."
  (let* ((bindings (ast-let-bindings ast))
         (body (ast-let-body ast))
         (sequential-p (ast-let-sequential-p ast))
         (saved-count (compile-env-local-count env)))

    (if sequential-p
        ;; let* - bind sequentially
        (let ((code nil))
          (dolist (binding bindings)
            (let* ((name (car binding))
                   (init-ast (cdr binding))
                   (init-code (compile-expression init-ast env))
                   (new-binding (env-bind-local env name :mutable t)))
              (setf code (append code
                                 init-code
                                 (emit-local.set (binding-index new-binding))))))
          ;; Compile body
          (setf code (append code (compile-expression body env)))
          ;; Restore environment
          (env-pop-scope env saved-count)
          code)

        ;; let - evaluate all inits first, then bind
        (let ((code nil)
              (temp-indices nil))
          ;; Evaluate all initializers
          (dolist (binding bindings)
            (let* ((init-ast (cdr binding))
                   (init-code (compile-expression init-ast env)))
              (setf code (append code init-code))))
          ;; Now bind all variables (in reverse order from stack)
          (dolist (binding (reverse bindings))
            (let* ((name (car binding))
                   (new-binding (env-bind-local env name :mutable t)))
              (push (binding-index new-binding) temp-indices)
              (setf code (append code
                                 (emit-local.set (binding-index new-binding))))))
          ;; Compile body
          (setf code (append code (compile-expression body env)))
          ;; Restore environment
          (env-pop-scope env saved-count)
          code))))

;;; ============================================================
;;; Lambda Compilation
;;; ============================================================

(defun compile-lambda (ast env)
  "Compile a lambda expression.
Returns code to create a closure."
  (let* ((params (ast-lambda-params ast))
         (body (ast-lambda-body ast))
         (context (compile-env-codegen-context env))
         (registry (compile-env-type-registry env))
         (module (codegen-context-module context))
         (arity (length params)))

    ;; Analyze free variables
    (let ((free-vars (analyze-free-variables ast env)))

      ;; Generate unique name for the lambda
      (incf (codegen-context-lambda-counter context))
      (let ((lambda-name (format nil "lambda_~D" (codegen-context-lambda-counter context))))

        ;; Compile the lambda body as a new function
        (let ((func-idx (compile-lambda-body lambda-name params body free-vars
                                              arity context)))

          ;; Generate code to create the closure at the call site
          (generate-closure-creation func-idx arity free-vars env registry))))))

(defun compile-lambda-body (name params body free-vars arity context)
  "Compile a lambda body as a Wasm function.
Returns the function index."
  (let* ((module (codegen-context-module context))
         (registry (codegen-context-type-registry context))
         (env-type-idx (env-type-index registry)))

    ;; Determine which function type to use based on arity
    (let* ((func-type-idx (case arity
                            (0 (func-0-type-index registry))
                            (1 (func-1-type-index registry))
                            (2 (func-2-type-index registry))
                            (t (func-n-type-index registry))))
           ;; Create new environment for lambda body
           (lambda-env (make-compile-env :type-registry registry
                                         :codegen-context context)))

      ;; Bind parameters
      ;; For arity 0-2: (env arg1 arg2 ...)
      ;; For arity N: (env args-array)
      (env-bind-param lambda-env '%env 0 :type `(:ref :null ,env-type-idx))

      (if (<= arity 2)
          ;; Direct parameters
          (loop for param in params
                for i from 1
                do (env-bind-param lambda-env param i :type :anyref))
          ;; Array parameter - need to extract at runtime
          (env-bind-param lambda-env '%args 1 :type :anyref))

      ;; Bind captured variables (accessed via env array)
      (loop for var in free-vars
            for i from 0
            do (env-bind-closure lambda-env var i))

      ;; Set tail position for body
      (setf (compile-env-tail-position-p lambda-env) t)

      ;; Compile body
      (let ((body-code
              (if (> arity 2)
                  ;; For N-arg functions, need to extract args first
                  (append (compile-extract-args params lambda-env registry)
                          (compile-expression body lambda-env))
                  (compile-expression body lambda-env))))

        ;; Create the function
        (let* ((locals (env-collect-local-types lambda-env))
               (full-code (append body-code (emit-end)))
               (func (make-wasm-func func-type-idx
                                     :name name
                                     :locals locals
                                     :body full-code)))
          (module-add-func module func))))))

(defun compile-extract-args (params env registry)
  "Generate code to extract arguments from args array for N-arg functions.
Binds each parameter as a local variable."
  (declare (ignore registry))
  (let ((code nil))
    (loop for param in params
          for i from 0
          do (let ((binding (env-bind-local env param)))
               ;; Get args array, index, and extract
               (setf code
                     (append code
                             (emit-local.get 1)  ; args array
                             (emit-i32.const i)
                             (emit-array.get (env-type-index (compile-env-type-registry env)))
                             (emit-local.set (binding-index binding))))))
    code))

(defun generate-closure-creation (func-idx arity free-vars env registry)
  "Generate code to create a closure struct.
Returns bytecode that leaves a closure on the stack."
  (let ((code nil)
        (num-free-vars (length free-vars)))

    ;; Push null refs for unused arity slots, and ref.func for the used slot
    (loop for a from 0 to 2
          do (setf code
                   (append code
                           (if (= a arity)
                               (emit-ref.func func-idx)
                               (emit-ref.null (case a
                                                (0 (func-0-type-index registry))
                                                (1 (func-1-type-index registry))
                                                (2 (func-2-type-index registry))))))))

    ;; N-arg slot (null for now if arity <= 2)
    (setf code
          (append code
                  (if (> arity 2)
                      (emit-ref.func func-idx)
                      (emit-ref.null (func-n-type-index registry)))))

    ;; Create environment array with captured variables
    (if (zerop num-free-vars)
        ;; Empty environment - push null
        (setf code (append code
                           (emit-ref.null (env-type-index registry))))
        ;; Capture free variables
        (progn
          ;; Push each captured variable's value
          (loop for var in free-vars
                do (setf code
                         (append code
                                 (compile-var (make-ast-var var) env))))
          ;; Create the env array
          (setf code (append code
                             (emit-create-env registry num-free-vars)))))

    ;; Create the closure struct
    (append code (emit-make-closure registry))))

;;; ============================================================
;;; Function Call Compilation
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((func (ast-call-func ast))
        (args (ast-call-args ast))
        (non-tail-env (env-not-in-tail-position env))
        (registry (compile-env-type-registry env)))
    ;; Check if it's a direct call to a known function
    (if (and (typep func 'ast-var)
             (primitivep (ast-var-name func)))
        ;; Optimize to primitive call
        (compile-primitive-call
         (make-ast-primitive-call (ast-var-name func) args)
         env)
        ;; General function call through closure
        (compile-closure-call func args env non-tail-env registry))))

(defun compile-closure-call (func-ast args env non-tail-env registry)
  "Compile a call through a closure.
FUNC-AST is the expression for the closure.
ARGS is the list of argument AST nodes."
  (let* ((arity (length args))
         (func-code (compile-expression func-ast non-tail-env))
         (tail-p (compile-env-tail-position-p env)))

    (cond
      ;; Arity 0-2: direct call with individual arguments
      ((<= arity 2)
       (compile-direct-closure-call func-code args arity env non-tail-env registry tail-p))

      ;; Arity > 2: pack arguments into array
      (t
       (compile-array-closure-call func-code args env non-tail-env registry tail-p)))))

(defun compile-direct-closure-call (func-code args arity env non-tail-env registry tail-p)
  "Compile a closure call with arity 0-2."
  (let ((code nil)
        ;; We need a local to store the closure so we can access it twice
        ;; (once for env, once for code)
        (closure-local-idx (compile-env-local-count env)))

    ;; Allocate a local for the closure
    (env-bind-local env '%closure)

    ;; Evaluate function and store in local
    (setf code (append code
                       func-code
                       (emit-local.tee closure-local-idx)))

    ;; Get environment from closure (first argument to called function)
    (setf code (append code
                       (emit-closure-get-env registry)))

    ;; Compile and push arguments
    (dolist (arg args)
      (setf code (append code
                         (compile-expression arg non-tail-env))))

    ;; Get the appropriate code slot from closure
    (setf code (append code
                       (emit-local.get closure-local-idx)
                       (emit-closure-get-code registry arity)))

    ;; Call the function via call_ref
    (let ((func-type-idx (case arity
                           (0 (func-0-type-index registry))
                           (1 (func-1-type-index registry))
                           (2 (func-2-type-index registry)))))
      (setf code (append code
                         (if tail-p
                             (emit-return-call-ref func-type-idx)
                             (emit-call-ref func-type-idx)))))

    code))

(defun compile-array-closure-call (func-code args env non-tail-env registry tail-p)
  "Compile a closure call with arity > 2 (arguments packed in array)."
  (let ((code nil)
        (arity (length args))
        ;; Local for the closure
        (closure-local-idx (compile-env-local-count env)))

    ;; Allocate local for closure
    (env-bind-local env '%closure)

    ;; Evaluate function and store in local
    (setf code (append code
                       func-code
                       (emit-local.tee closure-local-idx)))

    ;; Get environment from closure
    (setf code (append code
                       (emit-closure-get-env registry)))

    ;; Create arguments array
    ;; First push all argument values
    (dolist (arg args)
      (setf code (append code
                         (compile-expression arg non-tail-env))))

    ;; Create array with arguments
    (setf code (append code
                       (emit-array.new-fixed (vector-type-index registry) arity)))

    ;; Get the N-arg code slot from closure
    (setf code (append code
                       (emit-local.get closure-local-idx)
                       (emit-closure-get-code registry :n)))

    ;; Call via call_ref
    (let ((func-type-idx (func-n-type-index registry)))
      (setf code (append code
                         (if tail-p
                             (emit-return-call-ref func-type-idx)
                             (emit-call-ref func-type-idx)))))

    code))

(defun compile-primitive-call (ast env)
  "Compile a direct primitive call."
  (let* ((name (ast-primitive-call-name ast))
         (args (ast-primitive-call-args ast))
         (prim (find-primitive name))
         (non-tail-env (env-not-in-tail-position env)))
    (unless prim
      (error "Unknown primitive: ~S" name))

    ;; Check arity
    (let ((expected (primitive-arity prim))
          (actual (length args)))
      (unless (or (eq expected :variadic)
                  (= expected actual))
        (error "Primitive ~S expects ~D arguments, got ~D"
               name expected actual)))

    ;; Compile arguments
    (let ((arg-code
            (loop for arg in args
                  append (compile-expression arg non-tail-env))))
      ;; For fixnum operations, we need to unwrap i31ref first
      (when (member name '(+ - * truncate rem < <= > >= = /=
                           zerop plusp minusp))
        ;; Unwrap all fixnum arguments
        (setf arg-code
              (loop for arg in args
                    append (append (compile-expression arg non-tail-env)
                                   (emit-i31.get-s)))))

      ;; Generate primitive code
      (let* ((registry (compile-env-type-registry env))
             (generator (primitive-generator prim))
             (prim-code (funcall generator registry)))
        (append arg-code prim-code)))))

;;; ============================================================
;;; Definition Compilation
;;; ============================================================

(defun compile-defun (ast context)
  "Compile a function definition."
  (let* ((name (ast-defun-name ast))
         (params (ast-defun-params ast))
         (body (ast-defun-body ast))
         (module (codegen-context-module context))
         (registry (codegen-context-type-registry context))
         (arity (length params)))

    ;; Use standard function type based on arity
    (let ((type-idx (case arity
                      (0 (func-0-type-index registry))
                      (1 (func-1-type-index registry))
                      (2 (func-2-type-index registry))
                      (t (func-n-type-index registry)))))

      ;; Create compilation environment for function body
      (let ((env (make-compile-env :type-registry registry
                                   :codegen-context context)))
        ;; Bind parameters (env is param 0, then user params)
        (env-bind-param env '%env 0 :type `(:ref :null ,(env-type-index registry)))

        (if (<= arity 2)
            ;; Direct parameters
            (loop for param in params
                  for i from 1
                  do (env-bind-param env param i :type :anyref))
            ;; For N-arg, we get an array - extract params as locals
            (env-bind-param env '%args 1 :type :anyref))

        ;; Set tail position for body
        (setf (compile-env-tail-position-p env) t)

        ;; Compile body (with arg extraction for N-arg functions)
        (let ((body-code
                (if (> arity 2)
                    (append (compile-extract-args params env registry)
                            (compile-expression body env))
                    (compile-expression body env))))
          ;; Create function
          (let* ((locals (env-collect-local-types env))
                 (full-code (append body-code (emit-end)))
                 (func (make-wasm-func type-idx
                                       :name (symbol-name name)
                                       :locals locals
                                       :body full-code))
                 (func-idx (module-add-func module func)))

            ;; Export the function
            (module-add-export module
                               (make-export (symbol-name name) :func func-idx))

            func-idx))))))

(defun compile-defvar (ast context)
  "Compile a variable definition."
  (declare (ignore ast context))
  ;; TODO: Implement global variable definition
  ;; This requires setting up a global and initializing it
  (error "DEFVAR not yet implemented"))

(defun compile-expression-as-function (ast context)
  "Compile an expression as an anonymous function."
  (let* ((module (codegen-context-module context))
         (registry (codegen-context-type-registry context)))
    ;; Create function type: () -> anyref
    (let* ((func-type (make-functype nil '(:anyref)))
           (type-def (make-wasm-type func-type))
           (type-idx (module-add-type module type-def)))

      ;; Create compilation environment
      (let ((env (make-compile-env :type-registry registry)))
        (setf (compile-env-tail-position-p env) t)

        ;; Compile expression
        (let ((body-code (compile-expression ast env)))
          ;; Create function
          (let* ((locals (env-collect-local-types env))
                 (full-code (append body-code (emit-end)))
                 (func (make-wasm-func type-idx
                                       :locals locals
                                       :body full-code)))
            (module-add-func module func)))))))

;;; ============================================================
;;; High-Level Compilation Interface
;;; ============================================================

(defun compile-forms (forms)
  "Compile a list of forms into a Wasm module.
Returns the wasm-module."
  (let ((context nil))
    (dolist (form forms)
      (setf context (compile-toplevel form context)))
    (codegen-context-module context)))

(defun compile-to-wasm (forms &optional pathname)
  "Compile forms to Wasm binary.
If PATHNAME is provided, write to file. Otherwise return bytes."
  (let ((module (compile-forms forms)))
    (if pathname
        (emit-wasm-to-file module pathname)
        (emit-wasm-binary module))))
