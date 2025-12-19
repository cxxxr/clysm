;;;; compiler.lisp - Main compilation driver

(in-package #:cl-wasm/compiler)

;;; Form Compilation

(defun compile-form (form env)
  "Compile a Lisp form to WASM instructions."
  (cond
    ;; Self-evaluating
    ((null form)
     `((,+op-i32-const+ 0)))
    ((eq form t)
     `((,+op-i32-const+ 1)))
    ((integerp form)
     `((,+op-i32-const+ ,form)))
    ((floatp form)
     `((,+op-f64-const+ ,(float form 1.0d0))))
    ;; Variable reference
    ((symbolp form)
     (let ((info (env-lookup env form)))
       (if info
           `((,+op-local-get+ ,(local-info-index info)))
           (error "Undefined variable: ~A" form))))
    ;; Compound form
    ((consp form)
     (let ((op (car form)))
       (cond
         ;; Special form
         ((special-form-p op)
          (compile-special-form form env))
         ;; Primitive
         ((primitive-p op)
          (compile-primitive op (cdr form) env))
         ;; Function call (not yet implemented)
         (t
          (error "Unknown function: ~A" op)))))
    (t
     (error "Cannot compile: ~A" form))))

;;; Top-level Compilation

(defun compile-toplevel (form env)
  "Compile a top-level form."
  (cond
    ((and (consp form) (eq (car form) 'defun))
     (compile-defun form env))
    (t
     ;; Wrap as main function
     (compile-main form env))))

(defun compile-defun (form env)
  "Compile a function definition."
  (destructuring-bind (name params &rest body) (cdr form)
    (let* ((module (compile-env-module env))
           ;; Add function type
           (param-types (make-list (length params) :initial-element +type-i32+))
           (result-types (list +type-i32+)))
      (multiple-value-bind (new-env func-idx type-idx)
          (env-add-function env name param-types result-types)
        ;; Create local environment for function body
        (let ((body-env new-env))
          ;; Add parameters as locals
          (loop for param in params
                do (setf body-env (env-add-local body-env param +type-i32+)))
          ;; Compile body
          (let* ((body-code (compile-progn body body-env))
                 (locals (loop for i from (length params)
                               below (compile-env-local-count body-env)
                               collect (cons 1 +type-i32+))))
            ;; Add function to module
            (add-function module type-idx locals body-code)
            ;; Export function
            (add-export module (string-downcase (symbol-name name))
                        +export-func+ func-idx)
            new-env))))))

(defun compile-main (form env)
  "Compile a form as the main function."
  (let* ((module (compile-env-module env))
         (type-idx (add-func-type module nil (list +type-i32+)))
         (body-code (compile-form form env))
         (func-idx (add-function module type-idx nil body-code)))
    (add-export module "main" +export-func+ func-idx)
    env))

;;; Module Compilation

(defun compile-module (forms)
  "Compile a list of top-level forms to a WASM module."
  (let* ((module (make-wasm-module))
         (env (make-initial-env module)))
    (dolist (form forms)
      (setf env (compile-toplevel form env)))
    (finalize-module module)
    module))
