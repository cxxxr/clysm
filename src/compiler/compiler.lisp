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
         ;; User-defined function call
         ((and (symbolp op) (env-lookup env op :function))
          (compile-call form env))
         ;; Unknown function
         (t
          (error "Unknown function: ~A" op)))))
    (t
     (error "Cannot compile: ~A" form))))

;;; Function Calls

(defun compile-call (form env)
  "Compile a call to a user-defined function."
  (let* ((name (car form))
         (args (cdr form))
         (func-info (env-lookup env name :function))
         (func-idx (func-info-index func-info)))
    ;; Compile all arguments
    (let ((arg-code nil))
      (dolist (arg args)
        (setf arg-code (append arg-code (compile-form arg env))))
      ;; Emit call instruction
      (append arg-code `((,+op-call+ ,func-idx))))))

;;; Top-level Compilation

(defun compile-toplevel (form env)
  "Compile a top-level form."
  (cond
    ((and (consp form) (eq (car form) 'defun))
     (compile-defun form env))
    (t
     ;; Wrap as main function
     (compile-main form env))))

(defun find-max-local-index (code)
  "Find the maximum local index used in the generated code."
  (let ((max-idx -1))
    (labels ((scan (items)
               (dolist (item items)
                 (cond
                   ((and (listp item)
                         (or (eql (car item) +op-local-get+)
                             (eql (car item) +op-local-set+)
                             (eql (car item) +op-local-tee+)))
                    (setf max-idx (max max-idx (cadr item))))
                   ((listp item)
                    (scan item))))))
      (scan code))
    max-idx))

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
                 ;; Find max local index used in the code
                 (max-local-idx (find-max-local-index body-code))
                 ;; Locals = indices from (length params) to max-local-idx
                 (num-extra-locals (max 0 (- max-local-idx (1- (length params)))))
                 (locals (if (> num-extra-locals 0)
                             (list (cons num-extra-locals +type-i32+))
                             nil)))
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

(defun compile-module (forms &key (enable-memory t))
  "Compile a list of top-level forms to a WASM module."
  (let* ((module (make-wasm-module))
         (env (make-initial-env module)))
    ;; Add memory and heap pointer if enabled
    (when enable-memory
      (setup-runtime module))
    ;; Compile forms
    (dolist (form forms)
      (setf env (compile-toplevel form env)))
    (finalize-module module)
    module))

;;; Runtime Setup

(defparameter *heap-pointer-global* 0
  "Index of the heap pointer global variable.")

(defparameter *cons-size* 8
  "Size of a cons cell in bytes (car + cdr).")

(defun setup-runtime (module)
  "Set up runtime support: memory and heap pointer."
  ;; Add memory (1 page = 64KB, can grow)
  (add-memory module 1 16)
  ;; Add heap pointer global (starts at 1024 to leave room for constants)
  (setf *heap-pointer-global*
        (length (wasm-module-globals module)))
  (add-global module +type-i32+ t `((,+op-i32-const+ 1024))))
