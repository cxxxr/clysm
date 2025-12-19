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

(define-special-form if (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    (let ((test-code (compile-form test env))
          (then-code (compile-form then env))
          (else-code (if else
                         (compile-form else env)
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

;;; LAMBDA (placeholder - full closure support comes later)

(define-special-form lambda (form env)
  (declare (ignore form env))
  (error "Lambda not yet implemented in Phase 1"))

;;; FUNCALL (placeholder)

(define-special-form funcall (form env)
  (declare (ignore form env))
  (error "Funcall not yet implemented in Phase 1"))
