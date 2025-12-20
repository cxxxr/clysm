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

(defun find-primitive (symbol)
  "Find a primitive by SYMBOL, matching by symbol-name.
   This allows primitives to work across packages."
  (let ((name (symbol-name symbol)))
    (or (gethash symbol *primitives*)  ; Fast path: exact symbol match
        (block found
          (maphash (lambda (key val)
                     (when (string= (symbol-name key) name)
                       (return-from found val)))
                   *primitives*)
          nil))))

(defun primitive-p (symbol)
  "Check if SYMBOL names a primitive."
  (find-primitive symbol))

(defun compile-primitive (name args env)
  "Compile a primitive function call."
  (let ((handler (find-primitive name)))
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

(define-primitive listp (args env)
  "Check if argument is a list (nil or cons)."
  (unless (= (length args) 1)
    (error "listp requires exactly 1 argument"))
  ;; listp = (or (null x) (consp x))
  ;; Since our consp is just non-nil check, listp is always true for now
  ;; A proper implementation would check tag bits
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 1)))  ; always true for now

(define-primitive symbolp (args env)
  "Check if argument is a symbol."
  (unless (= (length args) 1)
    (error "symbolp requires exactly 1 argument"))
  ;; Check if value has symbol tag (simplified: check if it's interned)
  ;; For now, return nil as we don't have full symbol support in target
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 0)))  ; placeholder

(define-primitive numberp (args env)
  "Check if argument is a number."
  (unless (= (length args) 1)
    (error "numberp requires exactly 1 argument"))
  ;; For now, assume all non-cons values could be numbers
  ;; A proper implementation would check tag bits
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 1)))  ; placeholder - assume true

(define-primitive integerp (args env)
  "Check if argument is an integer."
  (unless (= (length args) 1)
    (error "integerp requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 1)))  ; placeholder

(define-primitive stringp (args env)
  "Check if argument is a string."
  (unless (= (length args) 1)
    (error "stringp requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-drop+
    (,+op-i32-const+ 0)))  ; placeholder

(define-primitive functionp (args env)
  "Check if argument is a function."
  (unless (= (length args) 1)
    (error "functionp requires exactly 1 argument"))
  ;; For closures, check if it has a valid function index at offset 0
  `(,@(compile-form (first args) env)
    (,+op-i32-const+ 0)
    ,+op-i32-ne+))  ; non-nil = function (simplified)

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

;;; append - concatenate two lists
;;; Implementation: iterate backwards through first list using temp local,
;;; cons each element onto second list
;;; This is O(n) but creates a fresh copy of the first list

(defvar *append-helper-added* nil
  "Flag to track if append helper has been added to the current module.")

(define-primitive append (args env)
  "Concatenate two lists. (append list1 list2) => new list"
  (cond
    ((null args)
     ;; (append) => nil
     `((,+op-i32-const+ 0)))
    ((null (cdr args))
     ;; (append x) => x
     (compile-form (first args) env))
    ((> (length args) 2)
     ;; (append a b c ...) => (append a (append b c ...))
     (compile-form `(append ,(first args) (append ,@(rest args))) env))
    (t
     ;; Two-argument case: (append list1 list2)
     ;; If list1 is nil, return list2
     ;; Otherwise, build (cons (car list1) ... (cons (car last1) list2))
     ;; We use WASM block/loop with local variables
     (let* ((list1-code (compile-form (first args) env))
            (list2-code (compile-form (second args) env))
            ;; We need 3 locals: list1-ptr, result-end, current
            (env-count (compile-env-local-count env))
            (list1-local env-count)
            (result-local (1+ env-count))
            (temp-local (+ 2 env-count)))
       `(;; Store list1 in local
         ,@list1-code
         (,+op-local-set+ ,list1-local)
         ;; Store list2 as initial result
         ,@list2-code
         (,+op-local-set+ ,result-local)
         ;; Check if list1 is nil - if so, just return list2
         (,+op-local-get+ ,list1-local)
         (,+op-i32-eqz+)
         (,+op-if+ ,+type-i32+)
           ;; list1 is nil - return list2
           (,+op-local-get+ ,result-local)
         (,+op-else+)
           ;; list1 is not nil - we need to reverse it, then cons onto list2
           ;; First pass: reverse list1 onto temp
           (,+op-i32-const+ 0)  ; temp = nil
           (,+op-local-set+ ,temp-local)
           (,+op-block+ ,+type-void+)  ; outer block for breaking
             (,+op-loop+ ,+type-void+)  ; loop to reverse
               ;; if list1 is nil, break
               (,+op-local-get+ ,list1-local)
               (,+op-i32-eqz+)
               (,+op-br-if+ 1)  ; break to outer block
               ;; temp = cons(car(list1), temp)
               ;; Allocate cons cell
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-add+
               (,+op-global-set+ ,*heap-pointer-global*)
               ;; Store car
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-get+ ,list1-local)
               (,+op-i32-load+ 2 0)  ; car of list1
               (,+op-i32-store+ 2 0)
               ;; Store cdr (old temp)
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-get+ ,temp-local)
               (,+op-i32-store+ 2 4)
               ;; Update temp
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-set+ ,temp-local)
               ;; list1 = cdr(list1)
               (,+op-local-get+ ,list1-local)
               (,+op-i32-load+ 2 4)
               (,+op-local-set+ ,list1-local)
               ;; continue loop
               (,+op-br+ 0)
             (,+op-end+)  ; end loop
           (,+op-end+)  ; end block
           ;; Second pass: cons temp onto result (list2)
           (,+op-block+ ,+type-void+)
             (,+op-loop+ ,+type-void+)
               ;; if temp is nil, break
               (,+op-local-get+ ,temp-local)
               (,+op-i32-eqz+)
               (,+op-br-if+ 1)
               ;; result = cons(car(temp), result)
               ;; Allocate cons cell
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-add+
               (,+op-global-set+ ,*heap-pointer-global*)
               ;; Store car
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-get+ ,temp-local)
               (,+op-i32-load+ 2 0)
               (,+op-i32-store+ 2 0)
               ;; Store cdr (old result)
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-get+ ,result-local)
               (,+op-i32-store+ 2 4)
               ;; Update result
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-set+ ,result-local)
               ;; temp = cdr(temp)
               (,+op-local-get+ ,temp-local)
               (,+op-i32-load+ 2 4)
               (,+op-local-set+ ,temp-local)
               (,+op-br+ 0)
             (,+op-end+)
           (,+op-end+)
           ;; Return result
           (,+op-local-get+ ,result-local)
         (,+op-end+))))))

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

(define-primitive char-code (args env)
  "Return the character code of a character."
  (unless (= (length args) 1)
    (error "char-code requires exactly 1 argument"))
  ;; Characters are represented as their code points (i32)
  (compile-form (first args) env))

(define-primitive code-char (args env)
  "Return the character with the given code."
  (unless (= (length args) 1)
    (error "code-char requires exactly 1 argument"))
  ;; Characters are represented as their code points (i32)
  (compile-form (first args) env))

(define-primitive char= (args env)
  "Compare two characters for equality."
  (unless (= (length args) 2)
    (error "char= requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-eq+))

(define-primitive char< (args env)
  "Compare two characters."
  (unless (= (length args) 2)
    (error "char< requires exactly 2 arguments"))
  `(,@(compile-form (first args) env)
    ,@(compile-form (second args) env)
    ,+op-i32-lt-s+))

(define-primitive char-upcase (args env)
  "Convert character to uppercase."
  (unless (= (length args) 1)
    (error "char-upcase requires exactly 1 argument"))
  (let* ((env-count (compile-env-local-count env))
         (char-local env-count))
    `(,@(compile-form (first args) env)
      (,+op-local-tee+ ,char-local)
      ;; Check if lowercase a-z (97-122)
      (,+op-i32-const+ 97)
      ,+op-i32-ge-s+
      (,+op-local-get+ ,char-local)
      (,+op-i32-const+ 122)
      ,+op-i32-le-s+
      ,+op-i32-and+
      (,+op-if+ ,+type-i32+)
        ;; Convert to uppercase: subtract 32
        (,+op-local-get+ ,char-local)
        (,+op-i32-const+ 32)
        ,+op-i32-sub+
      (,+op-else+)
        (,+op-local-get+ ,char-local)
      (,+op-end+))))

(define-primitive char-downcase (args env)
  "Convert character to lowercase."
  (unless (= (length args) 1)
    (error "char-downcase requires exactly 1 argument"))
  (let* ((env-count (compile-env-local-count env))
         (char-local env-count))
    `(,@(compile-form (first args) env)
      (,+op-local-tee+ ,char-local)
      ;; Check if uppercase A-Z (65-90)
      (,+op-i32-const+ 65)
      ,+op-i32-ge-s+
      (,+op-local-get+ ,char-local)
      (,+op-i32-const+ 90)
      ,+op-i32-le-s+
      ,+op-i32-and+
      (,+op-if+ ,+type-i32+)
        ;; Convert to lowercase: add 32
        (,+op-local-get+ ,char-local)
        (,+op-i32-const+ 32)
        ,+op-i32-add+
      (,+op-else+)
        (,+op-local-get+ ,char-local)
      (,+op-end+))))

(define-primitive schar (args env)
  "Get character at index from a simple string."
  (unless (= (length args) 2)
    (error "schar requires exactly 2 arguments"))
  ;; String layout: [length:i32][utf8-bytes...]
  ;; Load byte at offset 4 + index
  `(,@(compile-form (first args) env)   ; string pointer
    (,+op-i32-const+ 4)
    ,+op-i32-add+                        ; skip length field
    ,@(compile-form (second args) env)  ; index
    ,+op-i32-add+                        ; byte address
    (,+op-i32-load8-u+ 0 0)))            ; load unsigned byte

(define-primitive string= (args env)
  "Compare two strings for equality."
  (unless (= (length args) 2)
    (error "string= requires exactly 2 arguments"))
  ;; Compare strings byte by byte
  (let* ((env-count (compile-env-local-count env))
         (str1-local env-count)
         (str2-local (+ env-count 1))
         (len1-local (+ env-count 2))
         (len2-local (+ env-count 3))
         (idx-local (+ env-count 4)))
    `(;; Store string pointers
      ,@(compile-form (first args) env)
      (,+op-local-set+ ,str1-local)
      ,@(compile-form (second args) env)
      (,+op-local-set+ ,str2-local)
      ;; Get lengths
      (,+op-local-get+ ,str1-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len1-local)
      (,+op-local-get+ ,str2-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len2-local)
      ;; If lengths differ, return false
      (,+op-local-get+ ,len1-local)
      (,+op-local-get+ ,len2-local)
      ,+op-i32-ne+
      (,+op-if+ ,+type-i32+)
        (,+op-i32-const+ 0)  ; false
      (,+op-else+)
        ;; Initialize index to 0
        (,+op-i32-const+ 0)
        (,+op-local-set+ ,idx-local)
        ;; Compare loop
        (,+op-block+ ,+type-i32+)
          (,+op-loop+ ,+type-void+)
            ;; If idx >= len, strings are equal
            (,+op-local-get+ ,idx-local)
            (,+op-local-get+ ,len1-local)
            ,+op-i32-ge-s+
            (,+op-if+ ,+type-void+)
              (,+op-i32-const+ 1)  ; true
              (,+op-br+ 2)
            (,+op-end+)
            ;; Compare bytes at idx
            (,+op-local-get+ ,str1-local)
            (,+op-i32-const+ 4)
            ,+op-i32-add+
            (,+op-local-get+ ,idx-local)
            ,+op-i32-add+
            (,+op-i32-load8-u+ 0 0)
            (,+op-local-get+ ,str2-local)
            (,+op-i32-const+ 4)
            ,+op-i32-add+
            (,+op-local-get+ ,idx-local)
            ,+op-i32-add+
            (,+op-i32-load8-u+ 0 0)
            ,+op-i32-ne+
            (,+op-if+ ,+type-void+)
              (,+op-i32-const+ 0)  ; false
              (,+op-br+ 2)
            (,+op-end+)
            ;; Increment index
            (,+op-local-get+ ,idx-local)
            (,+op-i32-const+ 1)
            ,+op-i32-add+
            (,+op-local-set+ ,idx-local)
            (,+op-br+ 0)
          (,+op-end+)
          ;; Should not reach here
          (,+op-i32-const+ 0)
        (,+op-end+)
      (,+op-end+))))

(define-primitive string-downcase (args env)
  "Convert string to lowercase, returning a new string."
  (unless (= (length args) 1)
    (error "string-downcase requires exactly 1 argument"))
  ;; Allocate new string and copy with conversion
  (let* ((env-count (compile-env-local-count env))
         (src-local env-count)
         (len-local (+ env-count 1))
         (dst-local (+ env-count 2))
         (idx-local (+ env-count 3))
         (char-local (+ env-count 4)))
    `(;; Get source string and length
      ,@(compile-form (first args) env)
      (,+op-local-tee+ ,src-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len-local)
      ;; Allocate new string: length field + bytes
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-set+ ,dst-local)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-get+ ,len-local)
      (,+op-i32-const+ 4)
      ,+op-i32-add+  ; total size = 4 + len
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*)
      ;; Store length in new string
      (,+op-local-get+ ,dst-local)
      (,+op-local-get+ ,len-local)
      (,+op-i32-store+ 2 0)
      ;; Copy and convert bytes
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,idx-local)
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; If idx >= len, done
          (,+op-local-get+ ,idx-local)
          (,+op-local-get+ ,len-local)
          ,+op-i32-ge-s+
          (,+op-br-if+ 1)
          ;; Load source byte
          (,+op-local-get+ ,src-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-i32-load8-u+ 0 0)
          (,+op-local-set+ ,char-local)
          ;; Convert to lowercase if A-Z
          (,+op-local-get+ ,char-local)
          (,+op-i32-const+ 65)
          ,+op-i32-ge-s+
          (,+op-local-get+ ,char-local)
          (,+op-i32-const+ 90)
          ,+op-i32-le-s+
          ,+op-i32-and+
          (,+op-if+ ,+type-void+)
            (,+op-local-get+ ,char-local)
            (,+op-i32-const+ 32)
            ,+op-i32-add+
            (,+op-local-set+ ,char-local)
          (,+op-end+)
          ;; Store converted byte
          (,+op-local-get+ ,dst-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-local-get+ ,char-local)
          (,+op-i32-store8+ 0 0)
          ;; Increment index
          (,+op-local-get+ ,idx-local)
          (,+op-i32-const+ 1)
          ,+op-i32-add+
          (,+op-local-set+ ,idx-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return new string pointer
      (,+op-local-get+ ,dst-local))))

(define-primitive string-upcase (args env)
  "Convert string to uppercase, returning a new string."
  (unless (= (length args) 1)
    (error "string-upcase requires exactly 1 argument"))
  ;; Allocate new string and copy with conversion
  (let* ((env-count (compile-env-local-count env))
         (src-local env-count)
         (len-local (+ env-count 1))
         (dst-local (+ env-count 2))
         (idx-local (+ env-count 3))
         (char-local (+ env-count 4)))
    `(;; Get source string and length
      ,@(compile-form (first args) env)
      (,+op-local-tee+ ,src-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len-local)
      ;; Allocate new string: length field + bytes
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-set+ ,dst-local)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-get+ ,len-local)
      (,+op-i32-const+ 4)
      ,+op-i32-add+  ; total size = 4 + len
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*)
      ;; Store length in new string
      (,+op-local-get+ ,dst-local)
      (,+op-local-get+ ,len-local)
      (,+op-i32-store+ 2 0)
      ;; Copy and convert bytes
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,idx-local)
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; If idx >= len, done
          (,+op-local-get+ ,idx-local)
          (,+op-local-get+ ,len-local)
          ,+op-i32-ge-s+
          (,+op-br-if+ 1)
          ;; Load source byte
          (,+op-local-get+ ,src-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-i32-load8-u+ 0 0)
          (,+op-local-set+ ,char-local)
          ;; Convert to uppercase if a-z
          (,+op-local-get+ ,char-local)
          (,+op-i32-const+ 97)
          ,+op-i32-ge-s+
          (,+op-local-get+ ,char-local)
          (,+op-i32-const+ 122)
          ,+op-i32-le-s+
          ,+op-i32-and+
          (,+op-if+ ,+type-void+)
            (,+op-local-get+ ,char-local)
            (,+op-i32-const+ 32)
            ,+op-i32-sub+
            (,+op-local-set+ ,char-local)
          (,+op-end+)
          ;; Store converted byte
          (,+op-local-get+ ,dst-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-local-get+ ,char-local)
          (,+op-i32-store8+ 0 0)
          ;; Increment index
          (,+op-local-get+ ,idx-local)
          (,+op-i32-const+ 1)
          ,+op-i32-add+
          (,+op-local-set+ ,idx-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return new string pointer
      (,+op-local-get+ ,dst-local))))

(define-primitive string-append (args env)
  "Concatenate two strings, returning a new string."
  (unless (= (length args) 2)
    (error "string-append requires exactly 2 arguments"))
  (let* ((env-count (compile-env-local-count env))
         (str1-local env-count)
         (str2-local (+ env-count 1))
         (len1-local (+ env-count 2))
         (len2-local (+ env-count 3))
         (dst-local (+ env-count 4))
         (idx-local (+ env-count 5)))
    `(;; Get strings and their lengths
      ,@(compile-form (first args) env)
      (,+op-local-tee+ ,str1-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len1-local)
      ,@(compile-form (second args) env)
      (,+op-local-tee+ ,str2-local)
      (,+op-i32-load+ 2 0)
      (,+op-local-set+ ,len2-local)
      ;; Allocate new string: 4 + len1 + len2
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-set+ ,dst-local)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-get+ ,len1-local)
      (,+op-local-get+ ,len2-local)
      ,+op-i32-add+
      (,+op-i32-const+ 4)
      ,+op-i32-add+
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*)
      ;; Store total length
      (,+op-local-get+ ,dst-local)
      (,+op-local-get+ ,len1-local)
      (,+op-local-get+ ,len2-local)
      ,+op-i32-add+
      (,+op-i32-store+ 2 0)
      ;; Copy first string bytes
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,idx-local)
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          (,+op-local-get+ ,idx-local)
          (,+op-local-get+ ,len1-local)
          ,+op-i32-ge-s+
          (,+op-br-if+ 1)
          (,+op-local-get+ ,dst-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-local-get+ ,str1-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-i32-load8-u+ 0 0)
          (,+op-i32-store8+ 0 0)
          (,+op-local-get+ ,idx-local)
          (,+op-i32-const+ 1)
          ,+op-i32-add+
          (,+op-local-set+ ,idx-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Copy second string bytes
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,idx-local)
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          (,+op-local-get+ ,idx-local)
          (,+op-local-get+ ,len2-local)
          ,+op-i32-ge-s+
          (,+op-br-if+ 1)
          (,+op-local-get+ ,dst-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,len1-local)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-local-get+ ,str2-local)
          (,+op-i32-const+ 4)
          ,+op-i32-add+
          (,+op-local-get+ ,idx-local)
          ,+op-i32-add+
          (,+op-i32-load8-u+ 0 0)
          (,+op-i32-store8+ 0 0)
          (,+op-local-get+ ,idx-local)
          (,+op-i32-const+ 1)
          ,+op-i32-add+
          (,+op-local-set+ ,idx-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return new string pointer
      (,+op-local-get+ ,dst-local))))

;;; Higher-order list functions

(define-primitive reverse (args env)
  "Reverse a list, creating a new list."
  (unless (= (length args) 1)
    (error "reverse requires exactly 1 argument"))
  (let* ((list-code (compile-form (first args) env))
         (env-count (compile-env-local-count env))
         (list-local env-count)
         (result-local (1+ env-count)))
    `(;; Store list in local
      ,@list-code
      (,+op-local-set+ ,list-local)
      ;; Initialize result to nil
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,result-local)
      ;; Loop through list, consing each element onto result
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; if list is nil, break
          (,+op-local-get+ ,list-local)
          (,+op-i32-eqz+)
          (,+op-br-if+ 1)
          ;; result = cons(car(list), result)
          ;; Allocate cons cell
          (,+op-global-get+ ,*heap-pointer-global*)
          (,+op-i32-const+ 8)
          ,+op-i32-add+
          (,+op-global-set+ ,*heap-pointer-global*)
          ;; Store car
          (,+op-global-get+ ,*heap-pointer-global*)
          (,+op-i32-const+ 8)
          ,+op-i32-sub+
          (,+op-local-get+ ,list-local)
          (,+op-i32-load+ 2 0)
          (,+op-i32-store+ 2 0)
          ;; Store cdr (old result)
          (,+op-global-get+ ,*heap-pointer-global*)
          (,+op-i32-const+ 8)
          ,+op-i32-sub+
          (,+op-local-get+ ,result-local)
          (,+op-i32-store+ 2 4)
          ;; Update result
          (,+op-global-get+ ,*heap-pointer-global*)
          (,+op-i32-const+ 8)
          ,+op-i32-sub+
          (,+op-local-set+ ,result-local)
          ;; list = cdr(list)
          (,+op-local-get+ ,list-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,list-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return result
      (,+op-local-get+ ,result-local))))

(define-primitive nreverse (args env)
  "Destructively reverse a list in place."
  (unless (= (length args) 1)
    (error "nreverse requires exactly 1 argument"))
  (let* ((list-code (compile-form (first args) env))
         (env-count (compile-env-local-count env))
         (current-local env-count)
         (prev-local (1+ env-count))
         (next-local (+ 2 env-count)))
    `(;; Store list in current
      ,@list-code
      (,+op-local-set+ ,current-local)
      ;; prev = nil
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,prev-local)
      ;; Loop through list, reversing cdr pointers
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; if current is nil, break
          (,+op-local-get+ ,current-local)
          (,+op-i32-eqz+)
          (,+op-br-if+ 1)
          ;; next = cdr(current)
          (,+op-local-get+ ,current-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,next-local)
          ;; cdr(current) = prev
          (,+op-local-get+ ,current-local)
          (,+op-local-get+ ,prev-local)
          (,+op-i32-store+ 2 4)
          ;; prev = current
          (,+op-local-get+ ,current-local)
          (,+op-local-set+ ,prev-local)
          ;; current = next
          (,+op-local-get+ ,next-local)
          (,+op-local-set+ ,current-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return prev (new head)
      (,+op-local-get+ ,prev-local))))

(define-primitive member (args env)
  "Find item in list using eq. Returns sublist starting with item, or nil."
  (unless (= (length args) 2)
    (error "member requires exactly 2 arguments"))
  (let* ((item-code (compile-form (first args) env))
         (list-code (compile-form (second args) env))
         (env-count (compile-env-local-count env))
         (item-local env-count)
         (list-local (1+ env-count)))
    `(;; Store item and list in locals
      ,@item-code
      (,+op-local-set+ ,item-local)
      ,@list-code
      (,+op-local-set+ ,list-local)
      ;; Loop through list
      (,+op-block+ ,+type-i32+)  ; outer block returns i32
        (,+op-loop+ ,+type-void+)
          ;; if list is nil, return nil
          (,+op-local-get+ ,list-local)
          (,+op-i32-eqz+)
          (,+op-if+ ,+type-void+)
            (,+op-i32-const+ 0)
            (,+op-br+ 2)  ; break to outer block with nil
          (,+op-end+)
          ;; if car(list) == item, return list
          (,+op-local-get+ ,list-local)
          (,+op-i32-load+ 2 0)  ; car
          (,+op-local-get+ ,item-local)
          ,+op-i32-eq+
          (,+op-if+ ,+type-void+)
            (,+op-local-get+ ,list-local)
            (,+op-br+ 2)  ; break to outer block with list
          (,+op-end+)
          ;; list = cdr(list)
          (,+op-local-get+ ,list-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,list-local)
          (,+op-br+ 0)  ; continue loop
        (,+op-end+)
        ;; Fallthrough (shouldn't happen, but need a value)
        (,+op-i32-const+ 0)
      (,+op-end+))))

(define-primitive assoc (args env)
  "Find pair in alist where car equals key. Returns pair or nil."
  (unless (= (length args) 2)
    (error "assoc requires exactly 2 arguments"))
  (let* ((key-code (compile-form (first args) env))
         (alist-code (compile-form (second args) env))
         (env-count (compile-env-local-count env))
         (key-local env-count)
         (alist-local (1+ env-count))
         (pair-local (+ 2 env-count)))
    `(;; Store key and alist in locals
      ,@key-code
      (,+op-local-set+ ,key-local)
      ,@alist-code
      (,+op-local-set+ ,alist-local)
      ;; Loop through alist
      (,+op-block+ ,+type-i32+)
        (,+op-loop+ ,+type-void+)
          ;; if alist is nil, return nil
          (,+op-local-get+ ,alist-local)
          (,+op-i32-eqz+)
          (,+op-if+ ,+type-void+)
            (,+op-i32-const+ 0)
            (,+op-br+ 2)
          (,+op-end+)
          ;; pair = car(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-set+ ,pair-local)
          ;; if pair is not nil and car(pair) == key, return pair
          (,+op-local-get+ ,pair-local)
          (,+op-i32-eqz+)
          (,+op-i32-eqz+)  ; not nil
          (,+op-if+ ,+type-void+)
            (,+op-local-get+ ,pair-local)
            (,+op-i32-load+ 2 0)  ; car of pair
            (,+op-local-get+ ,key-local)
            ,+op-i32-eq+
            (,+op-if+ ,+type-void+)
              (,+op-local-get+ ,pair-local)
              (,+op-br+ 3)
            (,+op-end+)
          (,+op-end+)
          ;; alist = cdr(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,alist-local)
          (,+op-br+ 0)
        (,+op-end+)
        (,+op-i32-const+ 0)
      (,+op-end+))))

(define-primitive last (args env)
  "Return the last cons of a list."
  (cond
    ((null args)
     (error "last requires at least 1 argument"))
    ((= (length args) 1)
     (let* ((list-code (compile-form (first args) env))
            (env-count (compile-env-local-count env))
            (list-local env-count))
       `(;; Store list in local
         ,@list-code
         (,+op-local-set+ ,list-local)
         ;; Check if list is nil
         (,+op-local-get+ ,list-local)
         (,+op-i32-eqz+)
         (,+op-if+ ,+type-i32+)
           (,+op-i32-const+ 0)
         (,+op-else+)
           ;; Loop until cdr is nil
           (,+op-block+ ,+type-void+)
             (,+op-loop+ ,+type-void+)
               ;; if cdr(list) is nil, break
               (,+op-local-get+ ,list-local)
               (,+op-i32-load+ 2 4)
               (,+op-i32-eqz+)
               (,+op-br-if+ 1)
               ;; list = cdr(list)
               (,+op-local-get+ ,list-local)
               (,+op-i32-load+ 2 4)
               (,+op-local-set+ ,list-local)
               (,+op-br+ 0)
             (,+op-end+)
           (,+op-end+)
           ;; Return list (last cons)
           (,+op-local-get+ ,list-local)
         (,+op-end+))))
    (t
     (error "last with n not yet supported"))))

(define-primitive length (args env)
  "Return the length of a list."
  (unless (= (length args) 1)
    (error "length requires exactly 1 argument"))
  (let* ((list-code (compile-form (first args) env))
         (env-count (compile-env-local-count env))
         (list-local env-count)
         (count-local (1+ env-count)))
    `(;; Store list in local
      ,@list-code
      (,+op-local-set+ ,list-local)
      ;; Initialize count to 0
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,count-local)
      ;; Loop through list, counting
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; if list is nil, break
          (,+op-local-get+ ,list-local)
          (,+op-i32-eqz+)
          (,+op-br-if+ 1)
          ;; count++
          (,+op-local-get+ ,count-local)
          (,+op-i32-const+ 1)
          ,+op-i32-add+
          (,+op-local-set+ ,count-local)
          ;; list = cdr(list)
          (,+op-local-get+ ,list-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,list-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Return count
      (,+op-local-get+ ,count-local))))

;;; Copy-list - create a shallow copy of a list

(define-primitive copy-list (args env)
  "Create a shallow copy of a list."
  (unless (= (length args) 1)
    (error "copy-list requires exactly 1 argument"))
  ;; Use append with nil to copy: (append lst nil)
  (compile-form `(append ,(first args) nil) env))

;;; Butlast - return list without last n elements

(define-primitive butlast (args env)
  "Return list without the last n elements (default n=1)."
  (let ((list-arg (first args))
        (n (if (cdr args) (second args) 1)))
    (cond
      ((not (integerp n))
       (error "butlast: n must be a constant integer for now"))
      ((= n 1)
       ;; Common case: butlast with n=1
       ;; Loop until (cdr list) is nil, collecting elements
       (let* ((list-code (compile-form list-arg env))
              (env-count (compile-env-local-count env))
              (list-local env-count)
              (result-local (1+ env-count))
              (tail-local (+ 2 env-count)))
         `(;; Store list in local
           ,@list-code
           (,+op-local-set+ ,list-local)
           ;; Initialize result to nil
           (,+op-i32-const+ 0)
           (,+op-local-set+ ,result-local)
           (,+op-i32-const+ 0)
           (,+op-local-set+ ,tail-local)
           ;; Loop through list until cdr is nil
           (,+op-block+ ,+type-void+)
             (,+op-loop+ ,+type-void+)
               ;; if list is nil or cdr(list) is nil, break
               (,+op-local-get+ ,list-local)
               (,+op-i32-eqz+)
               (,+op-br-if+ 1)
               (,+op-local-get+ ,list-local)
               (,+op-i32-load+ 2 4)  ; cdr
               (,+op-i32-eqz+)
               (,+op-br-if+ 1)
               ;; Append car(list) to result
               ;; Allocate cons cell
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-add+
               (,+op-global-set+ ,*heap-pointer-global*)
               ;; new-cell address
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               ;; Store car
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-get+ ,list-local)
               (,+op-i32-load+ 2 0)  ; car
               (,+op-i32-store+ 2 0)
               ;; Store cdr = nil for now
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-i32-const+ 0)
               (,+op-i32-store+ 2 4)
               ;; If result is nil, set result to new cell
               ;; Otherwise, set cdr of tail to new cell
               (,+op-local-get+ ,result-local)
               (,+op-i32-eqz+)
               (,+op-if+ ,+type-void+)
                 ;; result = new cell
                 (,+op-global-get+ ,*heap-pointer-global*)
                 (,+op-i32-const+ 8)
                 ,+op-i32-sub+
                 (,+op-local-set+ ,result-local)
               (,+op-else+)
                 ;; cdr(tail) = new cell
                 (,+op-local-get+ ,tail-local)
                 (,+op-global-get+ ,*heap-pointer-global*)
                 (,+op-i32-const+ 8)
                 ,+op-i32-sub+
                 (,+op-i32-store+ 2 4)
               (,+op-end+)
               ;; tail = new cell
               (,+op-global-get+ ,*heap-pointer-global*)
               (,+op-i32-const+ 8)
               ,+op-i32-sub+
               (,+op-local-set+ ,tail-local)
               ;; list = cdr(list)
               (,+op-local-get+ ,list-local)
               (,+op-i32-load+ 2 4)
               (,+op-local-set+ ,list-local)
               (,+op-br+ 0)
             (,+op-end+)
           (,+op-end+)
           ;; Return result
           (,+op-local-get+ ,result-local))))
      (t
       (error "butlast with n != 1 not yet supported")))))

;;; not - logical negation

(define-primitive not (args env)
  "Logical negation: (not x) returns t if x is nil, nil otherwise."
  (unless (= (length args) 1)
    (error "not requires exactly 1 argument"))
  `(,@(compile-form (first args) env)
    ,+op-i32-eqz+))

;;; Higher-order functions

(define-primitive mapcar (args env)
  "Apply function to each element of list and collect results.
  Only supports single-list mapcar with unary function."
  (unless (= (length args) 2)
    (error "mapcar requires exactly 2 arguments (function and list)"))
  (let* ((func-form (first args))
         (list-form (second args))
         (module (compile-env-module env))
         ;; Get type index for arity 1 (closure-env + 1 arg)
         (type-idx (get-closure-type-index module 1)))
    ;; Allocate locals
    (multiple-value-bind (env1 func-local)
        (env-add-local env (gensym "MAPCAR-FUNC") +type-i32+)
      (multiple-value-bind (env2 list-local)
          (env-add-local env1 (gensym "MAPCAR-LIST") +type-i32+)
        (multiple-value-bind (env3 result-local)
            (env-add-local env2 (gensym "MAPCAR-RESULT") +type-i32+)
          (multiple-value-bind (env4 tail-local)
              (env-add-local env3 (gensym "MAPCAR-TAIL") +type-i32+)
            (multiple-value-bind (env5 value-local)
                (env-add-local env4 (gensym "MAPCAR-VALUE") +type-i32+)
              (let ((func-code (compile-form func-form env))
                    (list-code (compile-form list-form env)))
                `(;; Store function closure
                  ,@func-code
                  (,+op-local-set+ ,func-local)
                  ;; Store initial list
                  ,@list-code
                  (,+op-local-set+ ,list-local)
                  ;; result = nil
                  (,+op-i32-const+ 0)
                  (,+op-local-set+ ,result-local)
                  ;; tail = nil
                  (,+op-i32-const+ 0)
                  (,+op-local-set+ ,tail-local)
                  ;; Loop
                  (,+op-block+ ,+type-void+)
                    (,+op-loop+ ,+type-void+)
                      ;; if list == nil, exit
                      (,+op-local-get+ ,list-local)
                      ,+op-i32-eqz+
                      (,+op-br-if+ 1)
                      ;; value = funcall(func, car(list))
                      ;; Push closure address as first arg
                      (,+op-local-get+ ,func-local)
                      ;; Push car(list) as second arg
                      (,+op-local-get+ ,list-local)
                      (,+op-i32-load+ 2 0)  ; car
                      ;; Get function index and call
                      (,+op-local-get+ ,func-local)
                      (,+op-i32-load+ 2 ,*closure-func-offset*)
                      (,+op-call-indirect+ ,type-idx 0)
                      ;; Store value in temp local
                      (,+op-local-set+ ,value-local)
                      ;; Allocate new cons cell
                      (,+op-global-get+ ,*heap-pointer-global*)
                      (,+op-i32-const+ 8)
                      ,+op-i32-add+
                      (,+op-global-set+ ,*heap-pointer-global*)
                      ;; cell address = heap - 8
                      ;; Store value in car
                      (,+op-global-get+ ,*heap-pointer-global*)
                      (,+op-i32-const+ 8)
                      ,+op-i32-sub+
                      (,+op-local-get+ ,value-local)
                      (,+op-i32-store+ 2 0)  ; store car
                      ;; Store nil in cdr
                      (,+op-global-get+ ,*heap-pointer-global*)
                      (,+op-i32-const+ 8)
                      ,+op-i32-sub+
                      (,+op-i32-const+ 0)
                      (,+op-i32-store+ 2 4)  ; store cdr = nil
                      ;; If result is nil, set result and tail to new cell
                      (,+op-local-get+ ,result-local)
                      ,+op-i32-eqz+
                      (,+op-if+ ,+type-void+)
                        ;; result = tail = new cell
                        (,+op-global-get+ ,*heap-pointer-global*)
                        (,+op-i32-const+ 8)
                        ,+op-i32-sub+
                        (,+op-local-set+ ,result-local)
                        (,+op-global-get+ ,*heap-pointer-global*)
                        (,+op-i32-const+ 8)
                        ,+op-i32-sub+
                        (,+op-local-set+ ,tail-local)
                      (,+op-else+)
                        ;; cdr(tail) = new cell
                        (,+op-local-get+ ,tail-local)
                        (,+op-global-get+ ,*heap-pointer-global*)
                        (,+op-i32-const+ 8)
                        ,+op-i32-sub+
                        (,+op-i32-store+ 2 4)  ; store cdr
                        ;; tail = new cell
                        (,+op-global-get+ ,*heap-pointer-global*)
                        (,+op-i32-const+ 8)
                        ,+op-i32-sub+
                        (,+op-local-set+ ,tail-local)
                      (,+op-end+)
                      ;; list = cdr(list)
                      (,+op-local-get+ ,list-local)
                      (,+op-i32-load+ 2 4)  ; cdr
                      (,+op-local-set+ ,list-local)
                      (,+op-br+ 0)
                    (,+op-end+)
                  (,+op-end+)
                  ;; Return result
                  (,+op-local-get+ ,result-local))))))))))

(define-primitive mapc (args env)
  "Apply function to each element of list for side effects.
  Returns the original list."
  (unless (= (length args) 2)
    (error "mapc requires exactly 2 arguments (function and list)"))
  (let* ((func-form (first args))
         (list-form (second args))
         (module (compile-env-module env))
         (type-idx (get-closure-type-index module 1)))
    (multiple-value-bind (env1 func-local)
        (env-add-local env (gensym "MAPC-FUNC") +type-i32+)
      (multiple-value-bind (env2 list-local)
          (env-add-local env1 (gensym "MAPC-LIST") +type-i32+)
        (multiple-value-bind (env3 orig-list-local)
            (env-add-local env2 (gensym "MAPC-ORIG") +type-i32+)
          (let ((func-code (compile-form func-form env))
                (list-code (compile-form list-form env)))
            `(;; Store function closure
              ,@func-code
              (,+op-local-set+ ,func-local)
              ;; Store initial list and save original
              ,@list-code
              (,+op-local-tee+ ,list-local)
              (,+op-local-set+ ,orig-list-local)
              ;; Loop
              (,+op-block+ ,+type-void+)
                (,+op-loop+ ,+type-void+)
                  ;; if list == nil, exit
                  (,+op-local-get+ ,list-local)
                  ,+op-i32-eqz+
                  (,+op-br-if+ 1)
                  ;; funcall(func, car(list))
                  (,+op-local-get+ ,func-local)
                  (,+op-local-get+ ,list-local)
                  (,+op-i32-load+ 2 0)  ; car
                  (,+op-local-get+ ,func-local)
                  (,+op-i32-load+ 2 ,*closure-func-offset*)
                  (,+op-call-indirect+ ,type-idx 0)
                  ,+op-drop+  ; discard result
                  ;; list = cdr(list)
                  (,+op-local-get+ ,list-local)
                  (,+op-i32-load+ 2 4)  ; cdr
                  (,+op-local-set+ ,list-local)
                  (,+op-br+ 0)
                (,+op-end+)
              (,+op-end+)
              ;; Return original list
              (,+op-local-get+ ,orig-list-local))))))))

(define-primitive reduce (args env)
  "Reduce a list with a binary function.
  (reduce func list) applies func cumulatively.
  (reduce func list :initial-value init) uses init as starting value."
  (unless (>= (length args) 2)
    (error "reduce requires at least 2 arguments (function and list)"))
  (let* ((func-form (first args))
         (list-form (second args))
         ;; Check for :initial-value keyword
         (rest-args (cddr args))
         (has-initial-value (and rest-args
                                  (eq (first rest-args) :initial-value)))
         (initial-value (if has-initial-value (second rest-args) nil))
         (module (compile-env-module env))
         ;; Type for binary function (closure-env + 2 args)
         (type-idx (get-closure-type-index module 2)))
    (multiple-value-bind (env1 func-local)
        (env-add-local env (gensym "REDUCE-FUNC") +type-i32+)
      (multiple-value-bind (env2 list-local)
          (env-add-local env1 (gensym "REDUCE-LIST") +type-i32+)
        (multiple-value-bind (env3 acc-local)
            (env-add-local env2 (gensym "REDUCE-ACC") +type-i32+)
          (let ((func-code (compile-form func-form env))
                (list-code (compile-form list-form env))
                (init-code (if has-initial-value
                               (compile-form initial-value env)
                               nil)))
            `(;; Store function closure
              ,@func-code
              (,+op-local-set+ ,func-local)
              ;; Store list
              ,@list-code
              (,+op-local-set+ ,list-local)
              ;; Initialize accumulator
              ,@(if has-initial-value
                    `(,@init-code
                      (,+op-local-set+ ,acc-local))
                    ;; No initial value: use first element
                    `((,+op-local-get+ ,list-local)
                      (,+op-i32-load+ 2 0)  ; car
                      (,+op-local-set+ ,acc-local)
                      ;; list = cdr(list)
                      (,+op-local-get+ ,list-local)
                      (,+op-i32-load+ 2 4)
                      (,+op-local-set+ ,list-local)))
              ;; Loop
              (,+op-block+ ,+type-void+)
                (,+op-loop+ ,+type-void+)
                  ;; if list == nil, exit
                  (,+op-local-get+ ,list-local)
                  ,+op-i32-eqz+
                  (,+op-br-if+ 1)
                  ;; acc = funcall(func, acc, car(list))
                  (,+op-local-get+ ,func-local)
                  (,+op-local-get+ ,acc-local)
                  (,+op-local-get+ ,list-local)
                  (,+op-i32-load+ 2 0)  ; car
                  (,+op-local-get+ ,func-local)
                  (,+op-i32-load+ 2 ,*closure-func-offset*)
                  (,+op-call-indirect+ ,type-idx 0)
                  (,+op-local-set+ ,acc-local)
                  ;; list = cdr(list)
                  (,+op-local-get+ ,list-local)
                  (,+op-i32-load+ 2 4)  ; cdr
                  (,+op-local-set+ ,list-local)
                  (,+op-br+ 0)
                (,+op-end+)
              (,+op-end+)
              ;; Return accumulator
              (,+op-local-get+ ,acc-local))))))))

;;; Error handling

(define-primitive error (args env)
  "Compile error to unreachable instruction. Arguments are ignored.
   The WASM unreachable instruction traps execution immediately."
  (declare (ignore args env))
  `((,+op-unreachable+)))

;;; Hash Tables (alist-based implementation for bootstrap)
;;; Hash table structure: (count . entries-alist)
;;; Each entry is a cons cell: (key . value)

(define-primitive make-hash-table (args env)
  "Create a new hash table. Arguments (:test) are ignored in this simple implementation.
   Returns (cons 0 nil) - count=0, empty alist."
  (declare (ignore args))
  ;; Allocate cons cell for (0 . nil)
  `(;; Allocate 8 bytes for cons cell
    (,+op-global-get+ ,*heap-pointer-global*)
    ;; Store 0 as car (count)
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ 0)
    (,+op-i32-store+ 2 0)
    ;; Store nil as cdr (entries)
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ 0)
    (,+op-i32-store+ 2 4)
    ;; Bump heap pointer
    (,+op-global-get+ ,*heap-pointer-global*)
    (,+op-i32-const+ ,*cons-size*)
    ,+op-i32-add+
    (,+op-global-set+ ,*heap-pointer-global*)))

(define-primitive hash-table-count (args env)
  "Return the number of entries in a hash table."
  (unless (= (length args) 1)
    (error "hash-table-count requires exactly 1 argument"))
  ;; Hash table is (count . entries), return car
  `(,@(compile-form (first args) env)
    (,+op-i32-load+ 2 0)))

(define-primitive gethash (args env)
  "Get value from hash table. Returns value if found, nil otherwise.
   Also sets mv-1 to t if found, nil if not (for multiple-value-bind)."
  (unless (member (length args) '(2 3))
    (error "gethash requires 2 or 3 arguments"))
  (let* ((key-code (compile-form (first args) env))
         (ht-code (compile-form (second args) env))
         (env-count (compile-env-local-count env))
         (key-local env-count)
         (ht-local (1+ env-count))
         (pair-local (+ 2 env-count))
         (alist-local (+ 3 env-count)))
    `(;; Store key and hash-table
      ,@key-code
      (,+op-local-set+ ,key-local)
      ,@ht-code
      (,+op-local-set+ ,ht-local)
      ;; Get entries alist: cdr of hash-table
      (,+op-local-get+ ,ht-local)
      (,+op-i32-load+ 2 4)
      (,+op-local-set+ ,alist-local)
      ;; Loop through alist to find key
      (,+op-block+ ,+type-i32+)
        (,+op-loop+ ,+type-void+)
          ;; if alist is nil, return nil and set mv-1 to nil (not found)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-eqz+)
          (,+op-if+ ,+type-void+)
            ;; Set mv-1 to nil (not found)
            (,+op-i32-const+ 0)
            (,+op-global-set+ ,(+ *mv-count-global* 1))
            ;; Return nil
            (,+op-i32-const+ 0)
            (,+op-br+ 2)
          (,+op-end+)
          ;; pair = car(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-set+ ,pair-local)
          ;; if car(pair) == key, return cdr(pair)
          (,+op-local-get+ ,pair-local)
          (,+op-i32-load+ 2 0)  ; car(pair) = key
          (,+op-local-get+ ,key-local)
          ,+op-i32-eq+
          (,+op-if+ ,+type-void+)
            ;; Set mv-1 to t (found) - use 1 as t
            (,+op-i32-const+ 1)
            (,+op-global-set+ ,(+ *mv-count-global* 1))
            ;; Return cdr(pair) = value
            (,+op-local-get+ ,pair-local)
            (,+op-i32-load+ 2 4)
            (,+op-br+ 2)
          (,+op-end+)
          ;; alist = cdr(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,alist-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+))))

(define-primitive sethash (args env)
  "Set value in hash table: (sethash key value hash-table). Returns value.
   This is a simplified alternative to (setf (gethash key ht) value)."
  (unless (= (length args) 3)
    (error "sethash requires exactly 3 arguments: key, value, hash-table"))
  (let* ((key-code (compile-form (first args) env))
         (value-code (compile-form (second args) env))
         (ht-code (compile-form (third args) env))
         (env-count (compile-env-local-count env))
         (key-local env-count)
         (value-local (1+ env-count))
         (ht-local (+ 2 env-count))
         (alist-local (+ 3 env-count))
         (pair-local (+ 4 env-count)))
    `(;; Store key, value, hash-table
      ,@key-code
      (,+op-local-set+ ,key-local)
      ,@value-code
      (,+op-local-set+ ,value-local)
      ,@ht-code
      (,+op-local-set+ ,ht-local)
      ;; Get entries alist
      (,+op-local-get+ ,ht-local)
      (,+op-i32-load+ 2 4)
      (,+op-local-set+ ,alist-local)
      ;; Search for existing entry
      (,+op-block+ ,+type-void+)
        (,+op-loop+ ,+type-void+)
          ;; if alist is nil, add new entry
          (,+op-local-get+ ,alist-local)
          (,+op-i32-eqz+)
          (,+op-br-if+ 1)  ; break out to add new entry
          ;; pair = car(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-set+ ,pair-local)
          ;; if car(pair) == key, update cdr(pair)
          (,+op-local-get+ ,pair-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-get+ ,key-local)
          ,+op-i32-eq+
          (,+op-if+ ,+type-void+)
            ;; rplacd pair with new value
            (,+op-local-get+ ,pair-local)
            (,+op-local-get+ ,value-local)
            (,+op-i32-store+ 2 4)
            (,+op-br+ 2)  ; exit both loops, value already set
          (,+op-end+)
          ;; alist = cdr(alist)
          (,+op-local-get+ ,alist-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,alist-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+)
      ;; Key not found, add new entry:
      ;; 1. Allocate new pair (key . value)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-set+ ,pair-local)
      (,+op-local-get+ ,pair-local)
      (,+op-local-get+ ,key-local)
      (,+op-i32-store+ 2 0)
      (,+op-local-get+ ,pair-local)
      (,+op-local-get+ ,value-local)
      (,+op-i32-store+ 2 4)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-i32-const+ ,*cons-size*)
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*)
      ;; 2. Allocate new cons for alist: (pair . old-entries)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-local-set+ ,alist-local)
      (,+op-local-get+ ,alist-local)
      (,+op-local-get+ ,pair-local)
      (,+op-i32-store+ 2 0)
      (,+op-local-get+ ,alist-local)
      ;; Get old entries
      (,+op-local-get+ ,ht-local)
      (,+op-i32-load+ 2 4)
      (,+op-i32-store+ 2 4)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-i32-const+ ,*cons-size*)
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*)
      ;; 3. Update hash-table cdr to new alist
      (,+op-local-get+ ,ht-local)
      (,+op-local-get+ ,alist-local)
      (,+op-i32-store+ 2 4)
      ;; 4. Increment count
      (,+op-local-get+ ,ht-local)
      (,+op-local-get+ ,ht-local)
      (,+op-i32-load+ 2 0)
      (,+op-i32-const+ 1)
      ,+op-i32-add+
      (,+op-i32-store+ 2 0)
      ;; Return value
      (,+op-local-get+ ,value-local))))

(define-primitive remhash (args env)
  "Remove entry from hash table. Returns t if found and removed, nil otherwise."
  (unless (= (length args) 2)
    (error "remhash requires exactly 2 arguments"))
  (let* ((key-code (compile-form (first args) env))
         (ht-code (compile-form (second args) env))
         (env-count (compile-env-local-count env))
         (key-local env-count)
         (ht-local (1+ env-count))
         (prev-local (+ 2 env-count))
         (curr-local (+ 3 env-count))
         (pair-local (+ 4 env-count)))
    `(;; Store key and hash-table
      ,@key-code
      (,+op-local-set+ ,key-local)
      ,@ht-code
      (,+op-local-set+ ,ht-local)
      ;; prev = nil, curr = entries
      (,+op-i32-const+ 0)
      (,+op-local-set+ ,prev-local)
      (,+op-local-get+ ,ht-local)
      (,+op-i32-load+ 2 4)
      (,+op-local-set+ ,curr-local)
      ;; Loop through alist
      (,+op-block+ ,+type-i32+)
        (,+op-loop+ ,+type-void+)
          ;; if curr is nil, return nil (not found)
          (,+op-local-get+ ,curr-local)
          (,+op-i32-eqz+)
          (,+op-if+ ,+type-void+)
            (,+op-i32-const+ 0)
            (,+op-br+ 2)
          (,+op-end+)
          ;; pair = car(curr)
          (,+op-local-get+ ,curr-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-set+ ,pair-local)
          ;; if car(pair) == key, remove this entry
          (,+op-local-get+ ,pair-local)
          (,+op-i32-load+ 2 0)
          (,+op-local-get+ ,key-local)
          ,+op-i32-eq+
          (,+op-if+ ,+type-void+)
            ;; If prev is nil, update hash-table cdr
            (,+op-local-get+ ,prev-local)
            (,+op-i32-eqz+)
            (,+op-if+ ,+type-void+)
              (,+op-local-get+ ,ht-local)
              (,+op-local-get+ ,curr-local)
              (,+op-i32-load+ 2 4)  ; cdr(curr)
              (,+op-i32-store+ 2 4)
            (,+op-else+)
              ;; rplacd prev with cdr(curr)
              (,+op-local-get+ ,prev-local)
              (,+op-local-get+ ,curr-local)
              (,+op-i32-load+ 2 4)
              (,+op-i32-store+ 2 4)
            (,+op-end+)
            ;; Decrement count
            (,+op-local-get+ ,ht-local)
            (,+op-local-get+ ,ht-local)
            (,+op-i32-load+ 2 0)
            (,+op-i32-const+ 1)
            ,+op-i32-sub+
            (,+op-i32-store+ 2 0)
            ;; Return t (1)
            (,+op-i32-const+ 1)
            (,+op-br+ 2)
          (,+op-end+)
          ;; prev = curr, curr = cdr(curr)
          (,+op-local-get+ ,curr-local)
          (,+op-local-set+ ,prev-local)
          (,+op-local-get+ ,curr-local)
          (,+op-i32-load+ 2 4)
          (,+op-local-set+ ,curr-local)
          (,+op-br+ 0)
        (,+op-end+)
      (,+op-end+))))

(define-primitive clrhash (args env)
  "Clear all entries from hash table. Returns the hash table."
  (unless (= (length args) 1)
    (error "clrhash requires exactly 1 argument"))
  ;; Set count to 0 and entries to nil
  `(,@(compile-form (first args) env)
    ;; Duplicate for return value
    ,@(compile-form (first args) env)
    ;; Set count to 0
    ,@(compile-form (first args) env)
    (,+op-i32-const+ 0)
    (,+op-i32-store+ 2 0)
    ;; Set entries to nil
    (,+op-i32-const+ 0)
    (,+op-i32-store+ 2 4)))
