;;;; primitives.lisp - Primitive function compilation

(in-package #:clysm/compiler)

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

;;; Helper for multi-argument comparisons
;;; (< a b c) => (and (< a b) (< b c))
(defun compile-chained-comparison (op args env)
  "Compile a chained comparison like (< a b c) => (and (< a b) (< b c))."
  (cond
    ((null args)
     (error "~A requires at least 1 argument" op))
    ((null (cdr args))
     ;; Single argument: always true
     `(,@(compile-form (first args) env)
       ,+op-drop+
       (,+op-i32-const+ 1)))
    ((null (cddr args))
     ;; Two arguments: simple comparison
     (let ((wasm-op (ecase op
                      (< +op-i32-lt-s+)
                      (> +op-i32-gt-s+)
                      (<= +op-i32-le-s+)
                      (>= +op-i32-ge-s+)
                      (= +op-i32-eq+))))
       `(,@(compile-form (first args) env)
         ,@(compile-form (second args) env)
         ,wasm-op)))
    (t
     ;; Multiple arguments: expand to AND form
     ;; (< a b c) => (and (< a b) (< b c))
     (let ((comparisons nil))
       (loop for (a b) on args
             while b
             do (push `(,op ,a ,b) comparisons))
       (compile-form `(and ,@(nreverse comparisons)) env)))))

(define-primitive < (args env)
  (compile-chained-comparison '< args env))

(define-primitive > (args env)
  (compile-chained-comparison '> args env))

(define-primitive <= (args env)
  (compile-chained-comparison '<= args env))

(define-primitive >= (args env)
  (compile-chained-comparison '>= args env))

(define-primitive = (args env)
  (compile-chained-comparison '= args env))

(define-primitive /= (args env)
  ;; Note: /= is special - it checks that ALL pairs are different
  ;; (/= a b c) means a≠b AND a≠c AND b≠c (not just adjacent pairs)
  ;; For now, implement pairwise for 2 args only
  (cond
    ((null args)
     (error "/= requires at least 1 argument"))
    ((null (cdr args))
     ;; Single argument: always true
     `(,@(compile-form (first args) env)
       ,+op-drop+
       (,+op-i32-const+ 1)))
    ((null (cddr args))
     ;; Two arguments: simple comparison
     `(,@(compile-form (first args) env)
       ,@(compile-form (second args) env)
       ,+op-i32-ne+))
    (t
     ;; Multiple arguments: check ALL pairs are different
     ;; (/= a b c) => (and (/= a b) (/= a c) (/= b c))
     (let ((comparisons nil))
       (loop for rest on args
             for a = (car rest)
             do (loop for b in (cdr rest)
                      do (push `(/= ,a ,b) comparisons)))
       (compile-form `(and ,@(nreverse comparisons)) env)))))

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
  ;; Important: We must reserve space BEFORE evaluating arguments,
  ;; because argument evaluation might allocate more cons cells.
  ;;
  ;; Strategy: Increment hp first to reserve space, then evaluate args
  ;; and store them. The cell address is (hp - 8).
  `(;; First increment hp to reserve space for this cons cell
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-add+
    (,+op-global-set+ ,*heap-pointer-global*)
    ;; Now hp points past the reserved cell. The cell is at hp-8.
    ;; Push cell address as return value
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-sub+
    ;; Stack: [cell-addr]
    ;; Store car at cell-addr (need: addr, value)
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-sub+
    ,@(compile-form (first args) env)
    (,+op-i32-store+ 2 0)
    ;; Store cdr at cell-addr+4
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-sub+
    ,@(compile-form (second args) env)
    (,+op-i32-store+ 2 4)
    ;; Return value (cell-addr) is already on stack
    ))

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

;;; Additional numeric primitives

(define-primitive abs (args env)
  "Return absolute value of a number."
  (unless (= (length args) 1)
    (error "abs requires exactly 1 argument"))
  ;; Implementation: (if (< x 0) (- 0 x) x)
  ;; WASM select: if cond≠0 then val1 else val2
  ;; Stack: [val1, val2, cond] -> [result]
  ;; We want: if x<0 then (0-x) else x
  ;; So: val1 = (0-x), val2 = x, cond = x<0
  (let ((arg-code (compile-form (first args) env)))
    `((,+op-i32-const+ 0)
      ,@arg-code                    ; compute 0 - x
      ,+op-i32-sub+                 ; val1 = (0 - x)
      ,@arg-code                    ; val2 = x
      ,@arg-code
      (,+op-i32-const+ 0)
      ,+op-i32-lt-s+                ; cond = x < 0
      ,+op-select+)))

(define-primitive max (args env)
  "Return the maximum of the arguments."
  (case (length args)
    (0 (error "max requires at least 1 argument"))
    (1 (compile-form (first args) env))
    (2 (let ((a-code (compile-form (first args) env))
             (b-code (compile-form (second args) env)))
         ;; WASM select: if cond≠0 then val1 else val2
         ;; We want: if a>b then a else b
         ;; So: val1=a, val2=b, cond=a>b
         `(,@a-code                  ; val1 = a
           ,@b-code                  ; val2 = b
           ,@a-code
           ,@b-code
           ,+op-i32-gt-s+            ; cond = a > b
           ,+op-select+)))
    (otherwise
     ;; Reduce: (max a b c) => (max (max a b) c)
     (compile-form `(max (max ,(first args) ,(second args))
                         ,@(cddr args))
                   env))))

(define-primitive min (args env)
  "Return the minimum of the arguments."
  (case (length args)
    (0 (error "min requires at least 1 argument"))
    (1 (compile-form (first args) env))
    (2 (let ((a-code (compile-form (first args) env))
             (b-code (compile-form (second args) env)))
         ;; WASM select: if cond≠0 then val1 else val2
         ;; We want: if a<b then a else b
         ;; So: val1=a, val2=b, cond=a<b
         `(,@a-code                  ; val1 = a
           ,@b-code                  ; val2 = b
           ,@a-code
           ,@b-code
           ,+op-i32-lt-s+            ; cond = a < b
           ,+op-select+)))
    (otherwise
     ;; Reduce: (min a b c) => (min (min a b) c)
     (compile-form `(min (min ,(first args) ,(second args))
                         ,@(cddr args))
                   env))))

(define-primitive evenp (args env)
  "Check if number is even."
  (unless (= (length args) 1)
    (error "evenp requires exactly 1 argument"))
  ;; (= (logand x 1) 0)
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 1)
    ,+op-i32-and+
    ,+op-i32-eqz+))                  ; result is 0 if even

(define-primitive oddp (args env)
  "Check if number is odd."
  (unless (= (length args) 1)
    (error "oddp requires exactly 1 argument"))
  ;; (= (logand x 1) 1)
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 1)
    ,+op-i32-and+))                  ; result is 1 if odd, 0 if even

(define-primitive numberp (args env)
  "Check if argument is a number (currently only fixnums)."
  (unless (= (length args) 1)
    (error "numberp requires exactly 1 argument"))
  ;; For now, all values are fixnums, so always true
  ;; TODO: Update when other types are added
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 1)))

(define-primitive integerp (args env)
  "Check if argument is an integer."
  (unless (= (length args) 1)
    (error "integerp requires exactly 1 argument"))
  ;; For now, all values are fixnums (integers), so always true
  ;; TODO: Update when floats are added
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 1)))

;;; GCD and LCM
;;; GCD uses Euclidean algorithm: while b≠0, (a,b) = (b, a mod b), return |a|

(define-primitive gcd (args env)
  "Compute greatest common divisor."
  (case (length args)
    (0 `((,+op-i32-const+ 0)))
    (1 ;; (gcd n) = |n|
     (compile-form `(abs ,(first args)) env))
    (2 ;; Two argument case - use Euclidean algorithm
     ;; Need locals for the loop and temp storage
     (multiple-value-bind (env1 a-idx)
         (env-add-local env (gensym "A") +type-i32+)
       (multiple-value-bind (env2 b-idx)
           (env-add-local env1 (gensym "B") +type-i32+)
         (multiple-value-bind (env3 tmp-idx)
             (env-add-local env2 (gensym "TMP") +type-i32+)
           (let ((a-code (compile-form (first args) env3))
                 (b-code (compile-form (second args) env3)))
             `(;; Store first arg in tmp, compute abs, store in a
               ,@a-code
               (,+op-local-set+ ,tmp-idx)
               ;; abs: if tmp<0 then 0-tmp else tmp
               (,+op-i32-const+ 0)
               (,+op-local-get+ ,tmp-idx)
               ,+op-i32-sub+              ; 0 - tmp = -tmp (val1)
               (,+op-local-get+ ,tmp-idx) ; tmp (val2)
               (,+op-local-get+ ,tmp-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+             ; tmp < 0 (cond)
               ,+op-select+               ; if cond then val1 else val2
               (,+op-local-set+ ,a-idx)
               ;; Store second arg in tmp, compute abs, store in b
               ,@b-code
               (,+op-local-set+ ,tmp-idx)
               ;; abs
               (,+op-i32-const+ 0)
               (,+op-local-get+ ,tmp-idx)
               ,+op-i32-sub+
               (,+op-local-get+ ,tmp-idx)
               (,+op-local-get+ ,tmp-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+
               ,+op-select+
               (,+op-local-set+ ,b-idx)
               ;; Loop while b != 0
               (,+op-block+ ,+type-void+)  ; outer block for exit
               (,+op-loop+ ,+type-void+)   ; loop
               ;; Check b == 0
               (,+op-local-get+ ,b-idx)
               ,+op-i32-eqz+
               (,+op-br-if+ 1)             ; if b==0, exit to outer block
               ;; tmp = b
               (,+op-local-get+ ,b-idx)
               (,+op-local-set+ ,tmp-idx)
               ;; b = a mod b
               (,+op-local-get+ ,a-idx)
               (,+op-local-get+ ,b-idx)
               ,+op-i32-rem-s+
               (,+op-local-set+ ,b-idx)
               ;; a = tmp
               (,+op-local-get+ ,tmp-idx)
               (,+op-local-set+ ,a-idx)
               ;; Continue loop
               (,+op-br+ 0)
               ,+op-end+  ; end loop
               ,+op-end+  ; end block
               ;; Return a (already positive)
               (,+op-local-get+ ,a-idx)))))))
    (otherwise
     ;; Reduce: (gcd a b c) => (gcd (gcd a b) c)
     (compile-form `(gcd (gcd ,(first args) ,(second args))
                         ,@(cddr args))
                   env))))

(define-primitive lcm (args env)
  "Compute least common multiple."
  (case (length args)
    (0 `((,+op-i32-const+ 1)))
    (1 ;; (lcm n) = |n|
     (compile-form `(abs ,(first args)) env))
    (2 ;; lcm(a,b) = |a*b| / gcd(a,b)
     ;; To avoid overflow, use: |a| / gcd(a,b) * |b|
     (compile-form `(* (/ (abs ,(first args))
                          (gcd ,(first args) ,(second args)))
                       (abs ,(second args)))
                   env))
    (otherwise
     ;; Reduce: (lcm a b c) => (lcm (lcm a b) c)
     (compile-form `(lcm (lcm ,(first args) ,(second args))
                         ,@(cddr args))
                   env))))

;;; Division functions with different rounding modes
;;; For integers, these return two values: quotient and remainder
;;; But since we don't have multiple-values yet, they return just the quotient

(define-primitive truncate (args env)
  "Truncate toward zero (standard integer division)."
  (case (length args)
    (1 ;; (truncate x) = x for integers
     (compile-form (first args) env))
    (2 ;; (truncate a b) = integer division toward zero
     ;; This is exactly what WASM i32.div_s does
     `(,@(compile-form (first args) env)
       ,@(compile-form (second args) env)
       ,+op-i32-div-s+))
    (otherwise
     (error "truncate requires 1 or 2 arguments"))))

(define-primitive floor (args env)
  "Floor toward negative infinity."
  (case (length args)
    (1 ;; (floor x) = x for integers
     (compile-form (first args) env))
    (2 ;; (floor a b) - round toward -infinity
     ;; floor differs from truncate when result is negative and remainder != 0
     ;; floor(a,b) = truncate(a,b) - 1 if (a<0 xor b<0) and rem(a,b) != 0
     (multiple-value-bind (env1 a-idx)
         (env-add-local env (gensym "A") +type-i32+)
       (multiple-value-bind (env2 b-idx)
           (env-add-local env1 (gensym "B") +type-i32+)
         (multiple-value-bind (env3 q-idx)
             (env-add-local env2 (gensym "Q") +type-i32+)
           (let ((a-code (compile-form (first args) env3))
                 (b-code (compile-form (second args) env3)))
             `(;; Store a and b
               ,@a-code
               (,+op-local-set+ ,a-idx)
               ,@b-code
               (,+op-local-set+ ,b-idx)
               ;; Compute q = a / b (truncate)
               (,+op-local-get+ ,a-idx)
               (,+op-local-get+ ,b-idx)
               ,+op-i32-div-s+
               (,+op-local-set+ ,q-idx)
               ;; val1 = q - 1
               (,+op-local-get+ ,q-idx)
               (,+op-i32-const+ 1)
               ,+op-i32-sub+
               ;; val2 = q
               (,+op-local-get+ ,q-idx)
               ;; cond = (rem != 0) AND (signs differ)
               ;; Compute remainder != 0
               (,+op-local-get+ ,a-idx)
               (,+op-local-get+ ,b-idx)
               ,+op-i32-rem-s+
               (,+op-i32-const+ 0)
               ,+op-i32-ne+
               ;; Compute (a < 0) xor (b < 0)
               (,+op-local-get+ ,a-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+
               (,+op-local-get+ ,b-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+
               ,+op-i32-xor+
               ;; AND the two conditions
               ,+op-i32-and+
               ;; select: if cond then val1 else val2
               ,+op-select+))))))
    (otherwise
     (error "floor requires 1 or 2 arguments"))))

(define-primitive ceiling (args env)
  "Ceiling toward positive infinity."
  (case (length args)
    (1 ;; (ceiling x) = x for integers
     (compile-form (first args) env))
    (2 ;; (ceiling a b) - round toward +infinity
     ;; ceiling differs from truncate when result is positive and remainder != 0
     ;; ceiling(a,b) = truncate(a,b) + 1 if (a>=0 == b>=0) and rem(a,b) != 0
     (multiple-value-bind (env1 a-idx)
         (env-add-local env (gensym "A") +type-i32+)
       (multiple-value-bind (env2 b-idx)
           (env-add-local env1 (gensym "B") +type-i32+)
         (multiple-value-bind (env3 q-idx)
             (env-add-local env2 (gensym "Q") +type-i32+)
           (let ((a-code (compile-form (first args) env3))
                 (b-code (compile-form (second args) env3)))
             `(;; Store a and b
               ,@a-code
               (,+op-local-set+ ,a-idx)
               ,@b-code
               (,+op-local-set+ ,b-idx)
               ;; Compute q = a / b (truncate)
               (,+op-local-get+ ,a-idx)
               (,+op-local-get+ ,b-idx)
               ,+op-i32-div-s+
               (,+op-local-set+ ,q-idx)
               ;; val1 = q + 1
               (,+op-local-get+ ,q-idx)
               (,+op-i32-const+ 1)
               ,+op-i32-add+
               ;; val2 = q
               (,+op-local-get+ ,q-idx)
               ;; cond = (rem != 0) AND (signs same)
               ;; Compute remainder != 0
               (,+op-local-get+ ,a-idx)
               (,+op-local-get+ ,b-idx)
               ,+op-i32-rem-s+
               (,+op-i32-const+ 0)
               ,+op-i32-ne+
               ;; Compute (a >= 0) == (b >= 0) - same signs
               ;; Using: NOT((a < 0) xor (b < 0))
               (,+op-local-get+ ,a-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+
               (,+op-local-get+ ,b-idx)
               (,+op-i32-const+ 0)
               ,+op-i32-lt-s+
               ,+op-i32-xor+
               ,+op-i32-eqz+             ; NOT of xor = same signs
               ;; AND the two conditions
               ,+op-i32-and+
               ;; select: if cond then val1 else val2
               ,+op-select+))))))
    (otherwise
     (error "ceiling requires 1 or 2 arguments"))))

(define-primitive round (args env)
  "Round to nearest integer (ties to even)."
  (case (length args)
    (1 ;; (round x) = x for integers
     (compile-form (first args) env))
    (2 ;; (round a b) - round to nearest
     ;; For integers: round(a,b) = truncate(a + b/2, b) for positive
     ;; This is simplified - proper rounding to even is complex
     ;; For now, use: if |rem| * 2 >= |b| then adjust toward larger magnitude
     ;; Simplified: just use truncate for now (TODO: proper rounding)
     `(,@(compile-form (first args) env)
       ,@(compile-form (second args) env)
       ,+op-i32-div-s+))
    (otherwise
     (error "round requires 1 or 2 arguments"))))

;;; Symbol Primitives
;;; Symbols are represented as pointers to symbol structures in memory.
;;; Symbol structure layout (see runtime.lisp):
;;;   [name-ptr:i32] [value:i32] [function:i32] [plist:i32]
;;; name-ptr points to a string: [length:i32][utf8-bytes...]

(define-primitive symbolp (args env)
  "Check if argument is a symbol.
   For now, we check if the pointer is in the valid static data area.
   TODO: Use proper tag bits when we implement tagged values."
  (unless (= (length args) 1)
    (error "symbolp requires exactly 1 argument"))
  ;; Simplified check: is it a non-nil pointer in the static data range?
  ;; In a proper implementation, we'd check tag bits.
  ;; For now, check if pointer >= 256 (static data base) and is properly aligned
  `(,@(compile-form (first args) env)
    ;; Check if >= 256 (static data base)
    (,+op-i32-const+ 256)
    ,+op-i32-ge-u+))

(define-primitive symbol-name (args env)
  "Get the name-ptr from a symbol.
   Returns a pointer to the string (not the string itself)."
  (unless (= (length args) 1)
    (error "symbol-name requires exactly 1 argument"))
  ;; Load the name-ptr field from offset 0 of the symbol structure
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 0)))  ; align=2, offset=0

(define-primitive symbol-value (args env)
  "Get the value from a symbol."
  (unless (= (length args) 1)
    (error "symbol-value requires exactly 1 argument"))
  ;; Load the value field from offset 4 of the symbol structure
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 4)))  ; align=2, offset=4

(define-primitive symbol-function (args env)
  "Get the function from a symbol."
  (unless (= (length args) 1)
    (error "symbol-function requires exactly 1 argument"))
  ;; Load the function field from offset 8 of the symbol structure
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 8)))  ; align=2, offset=8

(define-primitive symbol-plist (args env)
  "Get the property list from a symbol."
  (unless (= (length args) 1)
    (error "symbol-plist requires exactly 1 argument"))
  ;; Load the plist field from offset 12 of the symbol structure
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 12)))  ; align=2, offset=12

;;; String Primitives
;;; Strings are represented as pointers to string structures in memory.
;;; String structure layout: [length:i32][utf8-bytes...]

(define-primitive string-length (args env)
  "Get the length of a string in bytes."
  (unless (= (length args) 1)
    (error "string-length requires exactly 1 argument"))
  ;; Load the length field from offset 0 of the string structure
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 0)))  ; align=2, offset=0
