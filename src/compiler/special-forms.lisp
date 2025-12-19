;;;; special-forms.lisp - Special form compilation

(in-package #:cl-wasm/compiler)

;;; Special Form Handlers

(defparameter *special-forms*
  (make-hash-table :test 'eq)
  "Hash table mapping special form symbols to their compilers.")

(defmacro define-special-form (name (form env) &body body)
  "Define a compiler for a special form."
  `(setf (gethash ',name *special-forms*)
         (lambda (,form ,env)
           ,@body)))

(defun special-form-p (symbol)
  "Check if SYMBOL names a special form."
  (gethash symbol *special-forms*))

(defun compile-special-form (form env)
  "Compile a special form."
  (let ((handler (gethash (car form) *special-forms*)))
    (if handler
        (funcall handler form env)
        (error "Unknown special form: ~A" (car form)))))

;;; IF

(defun env-increment-block-depth (env)
  "Increment block depth for entering a WASM block scope."
  (let ((new-env (copy-compile-env env)))
    (incf (compile-env-block-depth new-env))
    new-env))

(define-special-form if (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    (let* ((test-code (compile-form test env))
           ;; WASM 'if' creates a block scope, so increment depth for branches
           (branch-env (env-increment-block-depth env))
           (then-code (compile-form then branch-env))
           (else-code (if else
                          (compile-form else branch-env)
                          `((,+op-i32-const+ 0)))))  ; nil = 0
      `(,@test-code
        (,+op-if+ ,+type-i32+)
        ,@then-code
        ,+op-else+
        ,@else-code
        ,+op-end+))))

;;; LET

(define-special-form let (form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (let ((new-env env)
          (init-code nil)
          (local-indices nil))
      ;; Process bindings
      (dolist (binding bindings)
        (let* ((name (if (consp binding) (car binding) binding))
               (init (if (consp binding) (cadr binding) nil)))
          (multiple-value-bind (env* index)
              (env-add-local new-env name +type-i32+)
            (setf new-env env*)
            (push index local-indices)
            (when init
              (let ((init-code* (compile-form init env)))
                (setf init-code
                      (append init-code
                              init-code*
                              `((,+op-local-set+ ,index)))))))))
      ;; Compile body
      (let ((body-code (compile-progn body new-env)))
        (append init-code body-code)))))

;;; PROGN

(define-special-form progn (form env)
  (compile-progn (cdr form) env))

(defun compile-progn (forms env)
  "Compile a sequence of forms, returning the value of the last."
  (if (null forms)
      `((,+op-i32-const+ 0))  ; nil
      (let ((code nil))
        (loop for (form . rest) on forms
              for form-code = (compile-form form env)
              do (setf code (append code form-code))
                 ;; Drop intermediate values
                 (when rest
                   (setf code (append code `(,+op-drop+)))))
        code)))

;;; SETQ

(define-special-form setq (form env)
  (destructuring-bind (var value) (cdr form)
    (let ((info (env-lookup env var)))
      (if info
          (let ((value-code (compile-form value env)))
            `(,@value-code
              (,+op-local-tee+ ,(local-info-index info))))
          (error "Undefined variable: ~A" var)))))

;;; QUOTE

(defun compile-quoted-value (value env)
  "Compile a quoted value to code that constructs it at runtime."
  (cond
    ;; NIL
    ((null value)
     `((,+op-i32-const+ 0)))
    ;; T (true)
    ((eq value t)
     `((,+op-i32-const+ 1)))
    ;; Integer
    ((integerp value)
     `((,+op-i32-const+ ,value)))
    ;; Float
    ((floatp value)
     `((,+op-f64-const+ ,(float value 1.0d0))))
    ;; List - build using cons at runtime
    ((consp value)
     (let ((car-code (compile-quoted-value (car value) env))
           (cdr-code (compile-quoted-value (cdr value) env)))
       ;; Use the cons primitive to build the cell
       `(;; Get heap pointer (return value)
         (,+op-global-get+ ,*heap-pointer-global*)
         ;; Store car
         (,+op-global-get+ ,*heap-pointer-global*)
         ,@car-code
         (,+op-i32-store+ 2 0)
         ;; Store cdr
         (,+op-global-get+ ,*heap-pointer-global*)
         ,@cdr-code
         (,+op-i32-store+ 2 4)
         ;; Increment heap pointer
         (,+op-global-get+ ,*heap-pointer-global*)
         (,+op-i32-const+ ,*cons-size*)
         ,+op-i32-add+
         (,+op-global-set+ ,*heap-pointer-global*))))
    ;; Symbol - not yet implemented (would need symbol table)
    ((symbolp value)
     (error "Cannot quote symbol yet: ~A" value))
    ;; Other
    (t
     (error "Cannot quote: ~A" value))))

(define-special-form quote (form env)
  (let ((value (cadr form)))
    (compile-quoted-value value env)))

;;; WHEN and UNLESS

(define-special-form when (form env)
  (destructuring-bind (test &rest body) (cdr form)
    ;; (when test body...) => (if test (progn body...) nil)
    (compile-form `(if ,test (progn ,@body) nil) env)))

(define-special-form unless (form env)
  (destructuring-bind (test &rest body) (cdr form)
    ;; (unless test body...) => (if test nil (progn body...))
    (compile-form `(if ,test nil (progn ,@body)) env)))

;;; COND

(define-special-form cond (form env)
  (let ((clauses (cdr form)))
    (if (null clauses)
        ;; No clauses - return NIL
        `((,+op-i32-const+ 0))
        ;; Transform to nested if
        (let ((clause (first clauses)))
          (if (eq (car clause) t)
              ;; (t body...) - always execute
              (compile-progn (cdr clause) env)
              ;; (test body...) => (if test (progn body...) (cond rest...))
              (compile-form
               `(if ,(car clause)
                    (progn ,@(or (cdr clause) (list (car clause))))
                    (cond ,@(rest clauses)))
               env))))))

;;; AND

(define-special-form and (form env)
  (let ((args (cdr form)))
    (cond
      ((null args)
       ;; (and) => t
       `((,+op-i32-const+ 1)))
      ((null (cdr args))
       ;; (and x) => x
       (compile-form (first args) env))
      (t
       ;; (and x y ...) => (if x (and y ...) nil)
       (compile-form `(if ,(first args)
                          (and ,@(rest args))
                          nil)
                     env)))))

;;; OR

(define-special-form or (form env)
  (let ((args (cdr form)))
    (cond
      ((null args)
       ;; (or) => nil
       `((,+op-i32-const+ 0)))
      ((null (cdr args))
       ;; (or x) => x
       (compile-form (first args) env))
      (t
       ;; (or x y ...) => (let ((temp x)) (if temp temp (or y ...)))
       ;; Simplified: since we don't have side effects in expressions yet,
       ;; we can evaluate x twice
       (compile-form `(if ,(first args)
                          ,(first args)
                          (or ,@(rest args)))
                     env)))))

;;; LET*

(define-special-form let* (form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (if (null bindings)
        ;; No bindings, just compile body
        (compile-progn body env)
        ;; Process first binding, then recurse
        (let* ((binding (first bindings))
               (name (if (consp binding) (car binding) binding))
               (init (if (consp binding) (cadr binding) nil)))
          (multiple-value-bind (new-env index)
              (env-add-local env name +type-i32+)
            (let ((init-code (if init
                                 (append (compile-form init env)
                                         `((,+op-local-set+ ,index)))
                                 nil))
                  ;; Compile rest as nested let*
                  (rest-code (compile-special-form
                              `(let* ,(rest bindings) ,@body)
                              new-env)))
              (append init-code rest-code)))))))

;;; BLOCK and RETURN-FROM

(defun env-add-block (env name)
  "Add a named block to the environment. Returns new env."
  (let ((new-env (copy-compile-env env)))
    (push (cons name (compile-env-block-depth env))
          (compile-env-blocks new-env))
    (incf (compile-env-block-depth new-env))
    new-env))

(defun env-lookup-block (env name)
  "Look up a block by name. Returns the relative branch depth or nil."
  (let ((entry (assoc name (compile-env-blocks env))))
    (when entry
      ;; Calculate relative depth: current depth - block depth - 1
      (- (compile-env-block-depth env) (cdr entry) 1))))

(define-special-form block (form env)
  "Compile a block that can be exited with return-from."
  (destructuring-bind (name &rest body) (cdr form)
    (let* ((block-env (env-add-block env name))
           (body-code (compile-progn body block-env)))
      ;; Wrap in a WASM block
      `((,+op-block+ ,+type-i32+)
        ,@body-code
        ,+op-end+))))

(define-special-form return-from (form env)
  "Compile a return-from that exits a named block."
  (destructuring-bind (name &optional value) (cdr form)
    (let ((depth (env-lookup-block env name)))
      (unless depth
        (error "No block named ~A in scope" name))
      (let ((value-code (if value
                            (compile-form value env)
                            `((,+op-i32-const+ 0)))))
        `(,@value-code
          (,+op-br+ ,depth))))))

(define-special-form return (form env)
  "Compile a return (return-from nil)."
  (compile-special-form `(return-from nil ,@(cdr form)) env))

;;; DOTIMES

(define-special-form dotimes (form env)
  "Compile (dotimes (var count [result]) body...).
  Executes body count times with var bound to 0, 1, ..., count-1."
  (destructuring-bind ((var count &optional result) &rest body) (cdr form)
    ;; We need two locals: one for var, one for limit
    (multiple-value-bind (env1 var-idx)
        (env-add-local env var +type-i32+)
      (multiple-value-bind (env2 limit-idx)
          (env-add-local env1 (gensym "LIMIT") +type-i32+)
        ;; Increment block depth for the block and loop
        (let* ((loop-env (env-increment-block-depth
                          (env-increment-block-depth env2)))
               (body-code (compile-progn body loop-env))
               (result-code (if result
                                (compile-form result env2)
                                `((,+op-i32-const+ 0)))))
          `(;; Initialize var to 0
            (,+op-i32-const+ 0)
            (,+op-local-set+ ,var-idx)
            ;; Evaluate count and store in limit
            ,@(compile-form count env)
            (,+op-local-set+ ,limit-idx)
            ;; Loop structure: block wraps loop for exit
            (,+op-block+ ,+type-void+)  ; exit block
            (,+op-loop+ ,+type-void+)   ; continue loop
            ;; Check: if var >= limit, exit
            (,+op-local-get+ ,var-idx)
            (,+op-local-get+ ,limit-idx)
            ,+op-i32-ge-s+
            (,+op-br-if+ 1)  ; br to exit block (depth 1)
            ;; Execute body (drop result since dotimes is for side effects)
            ,@body-code
            ,+op-drop+
            ;; Increment var
            (,+op-local-get+ ,var-idx)
            (,+op-i32-const+ 1)
            ,+op-i32-add+
            (,+op-local-set+ ,var-idx)
            ;; Branch back to loop start
            (,+op-br+ 0)  ; br to loop (depth 0)
            ,+op-end+  ; end loop
            ,+op-end+  ; end block
            ;; Return result
            ,@result-code))))))

;;; DOLIST

(define-special-form dolist (form env)
  "Compile (dolist (var list [result]) body...).
  Iterates over list elements."
  (destructuring-bind ((var list-form &optional result) &rest body) (cdr form)
    ;; We need two locals: one for var (current element), one for the list tail
    (multiple-value-bind (env1 var-idx)
        (env-add-local env var +type-i32+)
      (multiple-value-bind (env2 tail-idx)
          (env-add-local env1 (gensym "TAIL") +type-i32+)
        (let* ((loop-env (env-increment-block-depth
                          (env-increment-block-depth env2)))
               (body-code (compile-progn body loop-env))
               (result-code (if result
                                (compile-form result env2)
                                `((,+op-i32-const+ 0)))))
          `(;; Evaluate list and store in tail
            ,@(compile-form list-form env)
            (,+op-local-set+ ,tail-idx)
            ;; Loop structure
            (,+op-block+ ,+type-void+)  ; exit block
            (,+op-loop+ ,+type-void+)   ; continue loop
            ;; Check: if tail is null (0), exit
            (,+op-local-get+ ,tail-idx)
            ,+op-i32-eqz+
            (,+op-br-if+ 1)  ; br to exit block
            ;; Get car of tail into var
            (,+op-local-get+ ,tail-idx)
            (,+op-i32-load+ 2 0)  ; load car
            (,+op-local-set+ ,var-idx)
            ;; Execute body (drop result)
            ,@body-code
            ,+op-drop+
            ;; Move to cdr
            (,+op-local-get+ ,tail-idx)
            (,+op-i32-load+ 2 4)  ; load cdr
            (,+op-local-set+ ,tail-idx)
            ;; Continue loop
            (,+op-br+ 0)
            ,+op-end+  ; end loop
            ,+op-end+  ; end block
            ;; Return result
            ,@result-code))))))

;;; LAMBDA and FUNCALL - Closure support

;;; Free Variable Analysis

(defun find-free-variables (form bound-vars)
  "Find variables in FORM that are not in BOUND-VARS.
   Returns a list of free variable names."
  (let ((free-vars nil))
    (labels ((collect (expr bound)
               (cond
                 ;; Atoms
                 ((null expr) nil)
                 ((eq expr t) nil)
                 ((numberp expr) nil)
                 ;; Variable reference
                 ((symbolp expr)
                  (when (and (not (member expr bound))
                             (not (member expr free-vars))
                             (not (special-form-p expr))
                             (not (primitive-p expr)))
                    (push expr free-vars)))
                 ;; Special forms
                 ((and (consp expr) (eq (car expr) 'quote))
                  nil)  ; Don't look inside quote
                 ((and (consp expr) (eq (car expr) 'lambda))
                  ;; Lambda introduces new bindings
                  (destructuring-bind (params &rest body) (cdr expr)
                    (let ((new-bound (append params bound)))
                      (dolist (b body)
                        (collect b new-bound)))))
                 ((and (consp expr) (eq (car expr) 'let))
                  (destructuring-bind (bindings &rest body) (cdr expr)
                    ;; First process init forms with current bindings
                    (dolist (b bindings)
                      (when (consp b)
                        (collect (second b) bound)))
                    ;; Then body with extended bindings
                    (let ((new-bound (append (mapcar (lambda (b)
                                                       (if (consp b) (car b) b))
                                                     bindings)
                                             bound)))
                      (dolist (b body)
                        (collect b new-bound)))))
                 ((and (consp expr) (eq (car expr) 'let*))
                  (destructuring-bind (bindings &rest body) (cdr expr)
                    ;; Process sequentially
                    (let ((current-bound bound))
                      (dolist (b bindings)
                        (when (consp b)
                          (collect (second b) current-bound))
                        (push (if (consp b) (car b) b) current-bound))
                      (dolist (b body)
                        (collect b current-bound)))))
                 ((and (consp expr) (member (car expr) '(dotimes dolist)))
                  ;; Loop variable is bound in body
                  (destructuring-bind ((var init &optional result) &rest body)
                      (cdr expr)
                    (collect init bound)
                    (let ((new-bound (cons var bound)))
                      (dolist (b body)
                        (collect b new-bound))
                      (when result
                        (collect result new-bound)))))
                 ;; General list - process all elements
                 ((consp expr)
                  (dolist (e expr)
                    (collect e bound))))))
      (collect form bound-vars))
    (nreverse free-vars)))

;;; Closure Representation
;;; A closure is stored on the heap as:
;;;   [func_index: i32, env_size: i32, captured_val1, captured_val2, ...]
;;; Total size = 8 + 4 * num_captured bytes

(defparameter *closure-header-size* 8
  "Size of closure header (func_index + env_size) in bytes.")

(defparameter *closure-func-offset* 0
  "Offset of function index in closure.")

(defparameter *closure-envsize-offset* 4
  "Offset of environment size in closure.")

(defparameter *closure-env-offset* 8
  "Offset of first captured variable in closure.")

;;; Lambda compilation creates a closure
;;; For non-capturing lambdas, we still create a closure structure for uniformity

(define-special-form lambda (form env)
  "Compile a lambda expression. Creates a closure on the heap."
  (destructuring-bind (params &rest body) (cdr form)
    ;; Find free variables
    (let* ((free-vars (find-free-variables `(progn ,@body) params))
           (num-captured (length free-vars))
           (closure-size (+ *closure-header-size* (* 4 num-captured)))
           (module (compile-env-module env))
           (arity (length params)))

      ;; Get the shared type index for this arity
      (let* ((type-idx (get-closure-type-index module arity))
             ;; Calculate function index: defuns first, then lambdas
             (defun-count (compile-env-func-count env))
             (lambda-idx (if *pending-lambdas*
                             (length (car *pending-lambdas*))
                             (wasm-module-func-count module)))
             (func-idx (+ defun-count lambda-idx)))

          ;; Create body environment with parameters as locals
          (let ((body-env (make-compile-env :module module)))
            ;; Add closure-env as local 0
            (setf body-env (env-add-local body-env 'closure-env +type-i32+))
            ;; Add declared parameters as locals
            (dolist (param params)
              (setf body-env (env-add-local body-env param +type-i32+)))

            ;; Add captured variables as real locals
            ;; and initialize them from the closure at function start
            (let* ((init-code nil))

              ;; Add each captured variable as a local
              (dolist (var free-vars)
                (multiple-value-bind (env* idx)
                    (env-add-local body-env var +type-i32+)
                  (setf body-env env*)
                  ;; Load from closure into local
                  (let ((offset (+ *closure-env-offset*
                                   (* 4 (position var free-vars)))))
                    (setf init-code
                          (append init-code
                                  `((,+op-local-get+ 0)  ; closure-env
                                    (,+op-i32-load+ 2 ,offset)
                                    (,+op-local-set+ ,idx)))))))

              ;; Compile the body
              (let* ((body-code (compile-progn body body-env))
                     (full-code (append init-code body-code))
                     ;; Find max local index
                     (max-local-idx (find-max-local-index full-code))
                     ;; Locals = indices beyond params (closure-env + declared params)
                     (param-count (1+ arity))  ; closure-env + params
                     (num-extra-locals (max 0 (- max-local-idx param-count -1)))
                     (locals (if (> num-extra-locals 0)
                                 (list (cons num-extra-locals +type-i32+))
                                 nil)))

                ;; Add function to pending list if available, else directly to module
                (if *pending-lambdas*
                    (push (list type-idx locals full-code) (car *pending-lambdas*))
                    (add-function module type-idx locals full-code))

                ;; Now generate code to create the closure structure
                `(;; Reserve space for closure
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-add+
                  (,+op-global-set+ ,*heap-pointer-global*)

                  ;; Push closure address (return value)
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+

                  ;; Store function index
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+
                  (,+op-i32-const+ ,func-idx)
                  (,+op-i32-store+ 2 ,*closure-func-offset*)

                  ;; Store env size
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+
                  (,+op-i32-const+ ,num-captured)
                  (,+op-i32-store+ 2 ,*closure-envsize-offset*)

                  ;; Store captured variables
                  ,@(loop for var in free-vars
                          for i from 0
                          for var-info = (env-lookup env var)
                          for offset = (+ *closure-env-offset* (* 4 i))
                          when var-info
                          append `((,+op-global-get+ ,*heap-pointer-global*)
                                   (,+op-i32-const+ ,closure-size)
                                   ,+op-i32-sub+
                                   (,+op-local-get+ ,(local-info-index var-info))
                                   (,+op-i32-store+ 2 ,offset)))
                  ;; Closure address is already on stack
                  ))))))))

;;; FUNCALL - Call a closure

(define-special-form funcall (form env)
  "Call a closure. The closure is the first argument."
  (let* ((closure-form (second form))
         (args (cddr form))
         (arity (length args))
         (module (compile-env-module env))
         (type-idx (get-closure-type-index module arity)))
    ;; Compile closure to get its address
    (let ((closure-code (compile-form closure-form env)))
      ;; Need a local to hold the closure address
      (multiple-value-bind (env* closure-idx)
          (env-add-local env (gensym "CLOSURE") +type-i32+)
        (let ((args-code nil))
          ;; Compile all arguments
          (dolist (arg args)
            (setf args-code (append args-code (compile-form arg env*))))

          `(;; Evaluate closure and save address
            ,@closure-code
            (,+op-local-set+ ,closure-idx)
            ;; Push closure address as first argument (for captured vars)
            (,+op-local-get+ ,closure-idx)
            ,@args-code
            ;; Get function index from closure and call indirectly
            (,+op-local-get+ ,closure-idx)
            (,+op-i32-load+ 2 ,*closure-func-offset*)
            (,+op-call-indirect+ ,type-idx 0)))))))
