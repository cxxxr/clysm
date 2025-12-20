;;;; compiler.lisp - Main compilation driver

(in-package #:clysm/compiler)

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
        (declare (ignore func-idx))  ; We'll get actual index from add-function
        ;; Create local environment for function body
        (let ((body-env new-env))
          ;; Add parameters as locals
          (loop for param in params
                do (setf body-env (env-add-local body-env param +type-i32+)))
          ;; Compile body (may add lambda functions to module first!)
          (let* ((body-code (compile-progn body body-env))
                 ;; Find max local index used in the code
                 (max-local-idx (find-max-local-index body-code))
                 ;; Locals = indices from (length params) to max-local-idx
                 (num-extra-locals (max 0 (- max-local-idx (1- (length params)))))
                 (locals (if (> num-extra-locals 0)
                             (list (cons num-extra-locals +type-i32+))
                             nil))
                 ;; Add function to module and get ACTUAL function index
                 (actual-func-idx (add-function module type-idx locals body-code)))
            ;; Export function with actual index (not the pre-calculated one)
            (add-export module (string-downcase (symbol-name name))
                        +export-func+ actual-func-idx)
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
  "Compile a list of top-level forms to a WASM module.
   Uses two-pass compilation to give defuns stable indices."
  (let* ((module (make-wasm-module))
         (env (make-initial-env module))
         (defuns nil)
         (lambda-holder (list nil)))  ; Box to collect lambda functions
    ;; Reset closure type cache for fresh compilation
    (reset-closure-type-cache)
    ;; Add memory and heap pointer if enabled
    (when enable-memory
      (setup-runtime module))
    ;; First pass: collect all defuns and register them
    (dolist (form forms)
      (when (and (consp form) (eq (car form) 'defun))
        (push form defuns)
        (destructuring-bind (name params &rest body) (cdr form)
          (declare (ignore body))
          (let ((param-types (make-list (length params) :initial-element +type-i32+))
                (result-types (list +type-i32+)))
            (setf env (env-add-function env name param-types result-types))))))
    (setf defuns (nreverse defuns))
    ;; Store lambda-holder in a dynamic variable for lambda compilation to use
    (let ((*pending-lambdas* lambda-holder))
      ;; Second pass: compile defun bodies (lambdas go to pending list)
      (dolist (form defuns)
        (compile-defun-body form env lambda-holder module))
      ;; Compile any non-defun forms
      (dolist (form forms)
        (unless (and (consp form) (eq (car form) 'defun))
          (setf env (compile-toplevel form env))))
      ;; Add pending lambda functions to module
      (dolist (lambda-entry (car lambda-holder))
        (destructuring-bind (type-idx locals body) lambda-entry
          (add-function module type-idx locals body))))
    ;; Add element section with all functions for indirect calls
    (setup-element-section module)
    (finalize-module module)
    module))


(defun compile-defun-body (form env lambda-holder module)
  "Compile a defun body after registration."
  (destructuring-bind (name params &rest body) (cdr form)
    (let ((func-info (env-lookup env name :function)))
      (unless func-info
        (error "Function ~A not registered" name))
      (let ((type-idx (func-info-type-index func-info))
            (func-idx (func-info-index func-info)))
        ;; Create local environment for function body
        (let ((body-env env))
          ;; Add parameters as locals
          (loop for param in params
                do (setf body-env (env-add-local body-env param +type-i32+)))
          ;; Compile body
          (let* ((body-code (compile-progn body body-env))
                 (max-local-idx (find-max-local-index body-code))
                 (num-extra-locals (max 0 (- max-local-idx (1- (length params)))))
                 (locals (if (> num-extra-locals 0)
                             (list (cons num-extra-locals +type-i32+))
                             nil)))
            ;; Add function to module
            (add-function module type-idx locals body-code)
            ;; Export function
            (add-export module (string-downcase (symbol-name name))
                        +export-func+ func-idx)))))))

(defun setup-element-section (module)
  "Populate the function table with all functions for indirect calls."
  (let ((func-count (+ (wasm-module-import-func-count module)
                       (wasm-module-func-count module))))
    (when (plusp func-count)
      ;; Create element segment with all functions starting at index 0
      (add-element module
                   0  ; table index
                   `((,+op-i32-const+ 0))  ; offset expression
                   (loop for i from 0 below func-count collect i)))))

;;; Runtime Setup

(defparameter *heap-pointer-global* 0
  "Index of the heap pointer global variable.")

(defparameter *cons-size* 8
  "Size of a cons cell in bytes (car + cdr).")

(defun setup-runtime (module)
  "Set up runtime support: memory, heap pointer, and function table."
  ;; Add memory (1 page = 64KB, can grow)
  (add-memory module 1 16)
  ;; Add heap pointer global (starts at 1024 to leave room for constants)
  (setf *heap-pointer-global*
        (length (wasm-module-globals module)))
  (add-global module +type-i32+ t `((,+op-i32-const+ 1024)))
  ;; Add function table for indirect calls (closures)
  ;; Initial size 256, can grow to 4096
  (add-table module +type-funcref+ 256 4096))

;;; Closure Type Index Cache

(defparameter *closure-type-indices* (make-hash-table)
  "Cache of type indices for closure types by arity.
   Key is arity (number of declared params, not including closure-env).
   Value is type index.")

(defun get-closure-type-index (module arity)
  "Get or create type index for a closure with ARITY parameters.
   All closures have signature (closure-env, arg1, ..., argN) -> i32."
  (or (gethash arity *closure-type-indices*)
      (let* ((param-count (1+ arity))  ; +1 for closure-env
             (params (make-list param-count :initial-element +type-i32+))
             (results (list +type-i32+))
             (type-idx (add-func-type module params results)))
        (setf (gethash arity *closure-type-indices*) type-idx)
        type-idx)))

(defun reset-closure-type-cache ()
  "Reset the closure type cache (called at start of compile-module)."
  (clrhash *closure-type-indices*))
