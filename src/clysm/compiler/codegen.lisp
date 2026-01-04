;;;; compiler/codegen.lisp - Code Generation
;;;;
;;;; Generates WebAssembly bytecode from AST nodes.

(in-package #:clysm)

;;; ============================================================
;;; Code Generation Context
;;; ============================================================

(defstruct (codegen-context (:constructor make-codegen-context
                                          (&key module type-registry)))
  "Context for code generation."
  module          ; The wasm-module being built
  type-registry   ; Type registry for this module
  (functions nil) ; List of generated functions
  (pending nil))  ; Pending lambda bodies to compile

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
  (declare (ignore env))
  ;; Lambda compilation is complex - involves:
  ;; 1. Analyze free variables
  ;; 2. Create a new function
  ;; 3. Create closure struct with function refs and captured env
  ;; For now, return a placeholder
  (error "Lambda compilation not yet fully implemented"))

;;; ============================================================
;;; Function Call Compilation
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((func (ast-call-func ast))
        (args (ast-call-args ast))
        (non-tail-env (env-not-in-tail-position env)))
    ;; Check if it's a direct call to a known function
    (if (and (typep func 'ast-var)
             (primitivep (ast-var-name func)))
        ;; Optimize to primitive call
        (compile-primitive-call
         (make-ast-primitive-call (ast-var-name func) args)
         env)
        ;; General function call through closure
        (let ((arg-code (loop for arg in args
                              append (compile-expression arg non-tail-env)))
              (func-code (compile-expression func non-tail-env)))
          ;; TODO: Implement closure dispatch
          ;; For now, error
          (declare (ignore arg-code func-code))
          (error "General function calls not yet implemented")))))

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
         (registry (codegen-context-type-registry context)))

    ;; Create function type: (env, params...) -> anyref
    (let* ((param-types (cons `(:ref :null ,(env-type-index registry))
                              (make-list (length params)
                                         :initial-element :anyref)))
           (func-type (make-functype param-types '(:anyref)))
           (type-def (make-wasm-type func-type :name (symbol-name name)))
           (type-idx (module-add-type module type-def)))

      ;; Create compilation environment for function body
      (let ((env (make-compile-env :type-registry registry)))
        ;; Bind parameters (env is param 0, then user params)
        (env-bind-param env '%env 0)
        (loop for param in params
              for i from 1
              do (env-bind-param env param i))

        ;; Set tail position for body
        (setf (compile-env-tail-position-p env) t)

        ;; Compile body
        (let ((body-code (compile-expression body env)))
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
