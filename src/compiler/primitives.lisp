;;;; primitives.lisp - Primitive function compilation

(in-package #:cl-wasm/compiler)

;;; Primitive Operations

(defparameter *primitives*
  (make-hash-table :test 'eq)
  "Hash table mapping primitive names to their code generators.")

(defmacro define-primitive (name (args env) &body body)
  "Define a code generator for a primitive operation."
  `(setf (gethash ',name *primitives*)
         (lambda (,args ,env)
           (declare (ignorable ,env))
           ,@body)))

(defun primitive-p (symbol)
  "Check if SYMBOL names a primitive."
  (gethash symbol *primitives*))

(defun compile-primitive (name args env)
  "Compile a primitive function call."
  (let ((handler (gethash name *primitives*)))
    (if handler
        (funcall handler args env)
        (error "Unknown primitive: ~A" name))))

;;; Arithmetic Primitives

(define-primitive + (args env)
  (case (length args)
    (0 `((,+op-i32-const+ 0)))
    (1 (compile-form (first args) env))
    (2 `(,@(compile-form (first args) env)
         ,@(compile-form (second args) env)
         ,+op-i32-add+))
    (otherwise
     ;; Reduce left to right
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-add+))))
       code))))

(define-primitive - (args env)
  (case (length args)
    (0 (error "- requires at least one argument"))
    (1 `((,+op-i32-const+ 0)
         ,@(compile-form (first args) env)
         ,+op-i32-sub+))
    (2 `(,@(compile-form (first args) env)
         ,@(compile-form (second args) env)
         ,+op-i32-sub+))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-sub+))))
       code))))

(define-primitive * (args env)
  (case (length args)
    (0 `((,+op-i32-const+ 1)))
    (1 (compile-form (first args) env))
    (2 `(,@(compile-form (first args) env)
         ,@(compile-form (second args) env)
         ,+op-i32-mul+))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-mul+))))
       code))))

(define-primitive / (args env)
  (case (length args)
    (0 (error "/ requires at least one argument"))
    (1 `((,+op-i32-const+ 1)
         ,@(compile-form (first args) env)
         ,+op-i32-div-s+))
    (2 `(,@(compile-form (first args) env)
         ,@(compile-form (second args) env)
         ,+op-i32-div-s+))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-div-s+))))
       code))))

(define-primitive mod (args env)
  (unless (= (length args) 2)
    (error "mod requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-rem-s+))

(define-primitive rem (args env)
  (unless (= (length args) 2)
    (error "rem requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-rem-s+))

;;; Comparison Primitives

(define-primitive < (args env)
  (unless (= (length args) 2)
    (error "< requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-lt-s+))

(define-primitive > (args env)
  (unless (= (length args) 2)
    (error "> requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-gt-s+))

(define-primitive <= (args env)
  (unless (= (length args) 2)
    (error "<= requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-le-s+))

(define-primitive >= (args env)
  (unless (= (length args) 2)
    (error ">= requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-ge-s+))

(define-primitive = (args env)
  (unless (= (length args) 2)
    (error "= requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-eq+))

(define-primitive /= (args env)
  (unless (= (length args) 2)
    (error "/= requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-ne+))

;;; Boolean Primitives

(define-primitive not (args env)
  (unless (= (length args) 1)
    (error "not requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-i32-eqz+))

(define-primitive null (args env)
  (unless (= (length args) 1)
    (error "null requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-i32-eqz+))

;;; Bitwise Primitives

(define-primitive logand (args env)
  (case (length args)
    (0 `((,+op-i32-const+ -1)))
    (1 (compile-form (first args) env))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-and+))))
       code))))

(define-primitive logior (args env)
  (case (length args)
    (0 `((,+op-i32-const+ 0)))
    (1 (compile-form (first args) env))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-or+))))
       code))))

(define-primitive logxor (args env)
  (case (length args)
    (0 `((,+op-i32-const+ 0)))
    (1 (compile-form (first args) env))
    (otherwise
     (let ((code (compile-form (first args) env)))
       (dolist (arg (rest args))
         (setf code (append code
                            (compile-form arg env)
                            (list +op-i32-xor+))))
       code))))

(define-primitive ash (args env)
  (unless (= (length args) 2)
    (error "ash requires exactly 2 arguments"))
  ;; Note: WASM shift amount is always positive, need runtime check for negative
  ;; For now, simple positive shift left
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-shl+))
