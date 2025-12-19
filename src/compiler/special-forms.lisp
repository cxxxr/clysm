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

(define-special-form quote (form env)
  (declare (ignore env))
  (let ((value (cadr form)))
    (cond
      ((null value)
       `((,+op-i32-const+ 0)))
      ((integerp value)
       `((,+op-i32-const+ ,value)))
      (t
       (error "Cannot quote: ~A" value)))))

;;; LAMBDA (placeholder - full closure support comes later)

(define-special-form lambda (form env)
  (declare (ignore form env))
  (error "Lambda not yet implemented in Phase 1"))

;;; FUNCALL (placeholder)

(define-special-form funcall (form env)
  (declare (ignore form env))
  (error "Funcall not yet implemented in Phase 1"))
