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

;;; List Primitives
;;; Using linear memory for cons cells:
;;; - NIL = 0
;;; - cons cell: 8 bytes (car at offset 0, cdr at offset 4)
;;; - heap pointer stored in global 0

(define-primitive cons (args env)
  "Allocate a cons cell and store car and cdr."
  (unless (= (length args) 2)
    (error "cons requires exactly 2 arguments"))
  ;; Code sequence:
  ;; 1. Get heap pointer
  ;; 2. Store car at heap pointer
  ;; 3. Store cdr at heap pointer + 4
  ;; 4. Save old heap pointer (return value)
  ;; 5. Increment heap pointer by 8
  `(;; Get current heap pointer (this will be the cons cell address)
    (,+op-global-get+ ,*heap-pointer-global*)
    ;; Store car: memory[hp] = car
    (,+op-global-get+ ,*heap-pointer-global*)
    ,@(compile-form (first args) env)
    (,+op-i32-store+ 2 0)  ; align=2 (4 bytes), offset=0
    ;; Store cdr: memory[hp+4] = cdr
    (,+op-global-get+ ,*heap-pointer-global*)
    ,@(compile-form (second args) env)
    (,+op-i32-store+ 2 4)  ; align=2, offset=4
    ;; Increment heap pointer by 8
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-add+
    (,+op-global-set+ ,*heap-pointer-global*)))

(define-primitive car (args env)
  "Get the car of a cons cell."
  (unless (= (length args) 1)
    (error "car requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 0)))  ; align=2, offset=0

(define-primitive cdr (args env)
  "Get the cdr of a cons cell."
  (unless (= (length args) 1)
    (error "cdr requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 4)))  ; align=2, offset=4

(define-primitive rplaca (args env)
  "Replace the car of a cons cell. Returns the cons cell."
  (unless (= (length args) 2)
    (error "rplaca requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)  ; cons cell address
    ,@(compile-form (first args) env)  ; duplicate for return value
    ,@(compile-form (second args) env) ; new car value
    (,+op-i32-store+ 2 0)))

(define-primitive rplacd (args env)
  "Replace the cdr of a cons cell. Returns the cons cell."
  (unless (= (length args) 2)
    (error "rplacd requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)  ; cons cell address (return value)
    ,@(compile-form (first args) env)  ; cons cell address
    ,@(compile-form (second args) env) ; new cdr value
    (,+op-i32-store+ 2 4)))

(define-primitive consp (args env)
  "Check if argument is a cons cell (non-nil pointer)."
  (unless (= (length args) 1)
    (error "consp requires exactly 1 argument"))
  ;; For now: consp = (not (null x))
  ;; A proper implementation would check tag bits
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 0)
    ,+op-i32-ne+))

(define-primitive atom (args env)
  "Check if argument is an atom (not a cons)."
  (unless (= (length args) 1)
    (error "atom requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-i32-eqz+))

;;; List length and access

(define-primitive length (args env)
  "Get the length of a list."
  (declare (ignore env))
  (unless (= (length args) 1)
    (error "length requires exactly 1 argument"))
  ;; This requires a loop - will be implemented later
  ;; For now, just return 0 as placeholder
  `((,+op-i32-const+ 0)))

(define-primitive list (args env)
  "Create a list from arguments.
  (list) => nil
  (list a b c) => (cons a (cons b (cons c nil)))"
  (if (null args)
      ;; Empty list = NIL
      `((,+op-i32-const+ 0))
      ;; Build nested cons: (cons a (cons b (cons c nil)))
      ;; and compile it
      (let ((form 'nil))
        (dolist (arg (reverse args))
          (setf form `(cons ,arg ,form)))
        (compile-form form env))))

(define-primitive list* (args env)
  "Create a list with the last argument as the final cdr.
  (list* a) => a
  (list* a b) => (cons a b)
  (list* a b c) => (cons a (cons b c))"
  (cond
    ((null args)
     (error "list* requires at least one argument"))
    ((null (cdr args))
     ;; Single argument: just return it
     (compile-form (first args) env))
    (t
     ;; Build nested cons with last element as final cdr
     (let ((form (car (last args))))
       (dolist (arg (reverse (butlast args)))
         (setf form `(cons ,arg ,form)))
       (compile-form form env)))))

;;; List accessors

(define-primitive first (args env)
  "Get the first element of a list (same as car)."
  (compile-form `(car ,(first args)) env))

(define-primitive rest (args env)
  "Get the rest of a list (same as cdr)."
  (compile-form `(cdr ,(first args)) env))

(define-primitive second (args env)
  "Get the second element of a list."
  (compile-form `(car (cdr ,(first args))) env))

(define-primitive third (args env)
  "Get the third element of a list."
  (compile-form `(car (cdr (cdr ,(first args)))) env))

(define-primitive fourth (args env)
  "Get the fourth element of a list."
  (compile-form `(car (cdr (cdr (cdr ,(first args))))) env))

(define-primitive nth (args env)
  "Get the nth element of a list (0-indexed)."
  (unless (= (length args) 2)
    (error "nth requires exactly 2 arguments"))
  ;; For now, only handle constant indices
  (let ((n (first args))
        (lst (second args)))
    (if (integerp n)
        ;; Constant index - expand inline
        (let ((form lst))
          (dotimes (i n)
            (setf form `(cdr ,form)))
          (compile-form `(car ,form) env))
        ;; Variable index - need a loop (not implemented yet)
        (error "nth with variable index not yet supported"))))

(define-primitive nthcdr (args env)
  "Get the nth cdr of a list (0-indexed)."
  (unless (= (length args) 2)
    (error "nthcdr requires exactly 2 arguments"))
  (let ((n (first args))
        (lst (second args)))
    (if (integerp n)
        ;; Constant index - expand inline
        (let ((form lst))
          (dotimes (i n)
            (setf form `(cdr ,form)))
          (compile-form form env))
        (error "nthcdr with variable index not yet supported"))))

;;; Number predicates

(define-primitive zerop (args env)
  "Check if argument is zero."
  (unless (= (length args) 1)
    (error "zerop requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-i32-eqz+))

(define-primitive plusp (args env)
  "Check if argument is positive."
  (unless (= (length args) 1)
    (error "plusp requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 0)
    ,+op-i32-gt-s+))

(define-primitive minusp (args env)
  "Check if argument is negative."
  (unless (= (length args) 1)
    (error "minusp requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 0)
    ,+op-i32-lt-s+))

(define-primitive 1+ (args env)
  "Add 1 to argument."
  (unless (= (length args) 1)
    (error "1+ requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 1)
    ,+op-i32-add+))

(define-primitive 1- (args env)
  "Subtract 1 from argument."
  (unless (= (length args) 1)
    (error "1- requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 1)
    ,+op-i32-sub+))

;;; Equality predicates

(define-primitive eq (args env)
  "Check if two arguments are the same object (pointer equality)."
  (unless (= (length args) 2)
    (error "eq requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-eq+))

(define-primitive eql (args env)
  "Check equality (for now, same as eq for fixnums)."
  (unless (= (length args) 2)
    (error "eql requires exactly 2 arguments"))
  ;; For fixnums, eql is same as eq
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-eq+))
