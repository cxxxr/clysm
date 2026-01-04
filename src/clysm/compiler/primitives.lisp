;;;; compiler/primitives.lisp - Primitive Operation Code Generators
;;;;
;;;; Emits WebAssembly bytecode for primitive Lisp operations.
;;;; All primitives operate on anyref values using WasmGC instructions.

(in-package #:clysm)

;;; ============================================================
;;; Primitive Registry
;;; ============================================================

(defvar *primitives* (make-hash-table :test 'eq)
  "Registry of primitive operations and their code generators.")

(defstruct primitive
  "A primitive operation."
  name           ; Symbol name
  arity          ; Number of arguments (or :variadic)
  generator      ; Function (registry &rest args) -> bytecode
  pure-p         ; No side effects?
  foldable-p)    ; Can be constant-folded?

(defun register-primitive (name arity generator &key pure foldable)
  "Register a primitive operation."
  (setf (gethash name *primitives*)
        (make-primitive :name name
                        :arity arity
                        :generator generator
                        :pure-p pure
                        :foldable-p foldable)))

(defun find-primitive (name)
  "Find a primitive by name."
  (gethash name *primitives*))

(defun primitivep (name)
  "Check if NAME is a registered primitive."
  (not (null (find-primitive name))))

;;; ============================================================
;;; Boolean Representation
;;; ============================================================
;;;
;;; Lisp booleans: NIL = false, anything else = true
;;; In Wasm, we use i32 (0/1) for comparisons, then convert to anyref.
;;;
;;; For boolean results, we need to return NIL or T (a symbol).
;;; Since T is not yet defined, we use i31ref with 0/1 as a temporary
;;; solution. The full implementation will use proper NIL/T references.

(defun emit-i32-to-boolean (registry)
  "Convert i32 (0 or 1) to Lisp boolean (NIL or T).
Currently uses i31ref as a placeholder.
Stack: [i32] -> [anyref]"
  (declare (ignore registry))
  ;; For now, wrap i32 in i31ref (0 = false, non-0 = true)
  ;; Full implementation will branch to return NIL or T
  (emit-ref.i31))

(defun emit-boolean-to-i32 (registry)
  "Convert Lisp boolean to i32.
NIL (ref.eq with NIL global) -> 0, else -> 1
Stack: [anyref] -> [i32]"
  (declare (ignore registry))
  ;; Placeholder: assume i31ref, extract value
  ;; Full implementation will use ref.eq with NIL
  (append (emit-i31.get-s)
          (list (opcode :i32.eqz))   ; 0 if non-zero (true), 1 if zero (false)
          (list (opcode :i32.eqz)))) ; flip: now 1 if true, 0 if false

;;; ============================================================
;;; Fixnum Arithmetic
;;; ============================================================
;;;
;;; Fixnums are represented as i31ref (31-bit signed integers).
;;; Operations extract the i32 value, perform arithmetic, and rewrap.

(defun emit-fixnum-binary-op (op)
  "Emit a binary fixnum operation.
Stack: [fixnum1 fixnum2] -> [fixnum-result]"
  (append
   ;; Extract second operand (on top of stack)
   (emit-i31.get-s)
   ;; Swap and extract first operand
   ;; Note: We need local variables for proper operand order
   ;; For now, assume caller has arranged stack correctly
   ;; This is a simplified version
   ))

(defun gen-fixnum+ (registry)
  "Generate code for fixnum addition.
Stack: [a b] -> [a+b]"
  (declare (ignore registry))
  (append
   ;; b is on top, extract it
   (emit-i31.get-s)
   ;; Now stack is [a i32-b], need to get a
   ;; Use local.set/get pattern (will be handled by compiler)
   ;; For inline: swap won't work directly, need temp
   ;; Simplified: assume proper stack management by caller
   ))

;; For proper implementation, we need the compiler to manage locals.
;; Here we define the operation patterns that the compiler will use.

(defun emit-fixnum-add ()
  "Emit fixnum addition assuming operands are already i32.
Stack: [i32-a i32-b] -> [fixnum-result]"
  (append (list (opcode :i32.add))
          (emit-ref.i31)))

(defun emit-fixnum-sub ()
  "Emit fixnum subtraction.
Stack: [i32-a i32-b] -> [fixnum-result]"
  (append (list (opcode :i32.sub))
          (emit-ref.i31)))

(defun emit-fixnum-mul ()
  "Emit fixnum multiplication.
Stack: [i32-a i32-b] -> [fixnum-result]"
  (append (list (opcode :i32.mul))
          (emit-ref.i31)))

(defun emit-fixnum-div ()
  "Emit fixnum division (truncating).
Stack: [i32-a i32-b] -> [fixnum-result]"
  (append (list (opcode :i32.div_s))
          (emit-ref.i31)))

(defun emit-fixnum-rem ()
  "Emit fixnum remainder.
Stack: [i32-a i32-b] -> [fixnum-result]"
  (append (list (opcode :i32.rem_s))
          (emit-ref.i31)))

(defun emit-fixnum-negate ()
  "Emit fixnum negation.
Stack: [i32] -> [fixnum-result]"
  (append (list (opcode :i32.const) 0)  ; push 0
          ;; swap (need temp local in real impl)
          (list (opcode :i32.sub))      ; 0 - x = -x
          (emit-ref.i31)))

;;; ============================================================
;;; Fixnum Comparisons
;;; ============================================================

(defun emit-fixnum-lt ()
  "Emit fixnum less-than comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.lt_s)))

(defun emit-fixnum-le ()
  "Emit fixnum less-than-or-equal comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.le_s)))

(defun emit-fixnum-gt ()
  "Emit fixnum greater-than comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.gt_s)))

(defun emit-fixnum-ge ()
  "Emit fixnum greater-than-or-equal comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.ge_s)))

(defun emit-fixnum-eq ()
  "Emit fixnum equality comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.eq)))

(defun emit-fixnum-neq ()
  "Emit fixnum inequality comparison.
Stack: [i32-a i32-b] -> [i32 (0 or 1)]"
  (list (opcode :i32.ne)))

(defun emit-fixnum-zerop ()
  "Emit fixnum zero test.
Stack: [i32] -> [i32 (0 or 1)]"
  (list (opcode :i32.eqz)))

(defun emit-fixnum-plusp ()
  "Emit fixnum positive test.
Stack: [i32] -> [i32 (0 or 1)]"
  (append (list (opcode :i32.const) 0)
          (list (opcode :i32.gt_s))))

(defun emit-fixnum-minusp ()
  "Emit fixnum negative test.
Stack: [i32] -> [i32 (0 or 1)]"
  (append (list (opcode :i32.const) 0)
          (list (opcode :i32.lt_s))))

;;; ============================================================
;;; Cons Cell Operations
;;; ============================================================

(defun emit-cons (registry)
  "Emit cons cell creation.
Stack: [car cdr] -> [cons]"
  (emit-struct.new (cons-type-index registry)))

(defun emit-car-op (registry)
  "Emit car operation.
Stack: [cons] -> [car]"
  (emit-struct.get (cons-type-index registry) +cons-car+))

(defun emit-cdr-op (registry)
  "Emit cdr operation.
Stack: [cons] -> [cdr]"
  (emit-struct.get (cons-type-index registry) +cons-cdr+))

(defun emit-rplaca-op (registry)
  "Emit rplaca operation.
Stack: [cons new-car] -> [cons]
Note: Returns the cons cell (Common Lisp semantics)."
  ;; Need to duplicate cons reference before set
  ;; Simplified: assume compiler handles this
  (emit-struct.set (cons-type-index registry) +cons-car+))

(defun emit-rplacd-op (registry)
  "Emit rplacd operation.
Stack: [cons new-cdr] -> [cons]"
  (emit-struct.set (cons-type-index registry) +cons-cdr+))

(defun emit-set-car (registry)
  "Emit (setf car) - same as rplaca but may have different return.
Stack: [cons new-car] -> []"
  (emit-struct.set (cons-type-index registry) +cons-car+))

(defun emit-set-cdr (registry)
  "Emit (setf cdr) - same as rplacd.
Stack: [cons new-cdr] -> []"
  (emit-struct.set (cons-type-index registry) +cons-cdr+))

;;; ============================================================
;;; Type Predicates
;;; ============================================================

(defun emit-null-p (registry)
  "Emit null predicate (checks if value is NIL).
Stack: [anyref] -> [i32 (0 or 1)]
Note: Requires NIL global to be set up."
  ;; Full implementation: (global.get $nil) (ref.eq)
  ;; Placeholder: check if it's nil-type struct
  (emit-ref.test (nil-type-index registry)))

(defun emit-consp-op (registry)
  "Emit consp predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (cons-type-index registry)))

(defun emit-atom-op (registry)
  "Emit atom predicate (not consp).
Stack: [anyref] -> [i32 (0 or 1)]"
  (append (emit-ref.test (cons-type-index registry))
          (list (opcode :i32.eqz))))  ; NOT

(defun emit-symbolp-op (registry)
  "Emit symbolp predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (symbol-type-index registry)))

(defun emit-numberp-op ()
  "Emit numberp predicate (checks for i31ref).
Stack: [anyref] -> [i32 (0 or 1)]
Note: Currently only supports fixnums (i31ref)."
  ;; ref.test for i31 heap type
  ;; The encoding for i31 test is: 0xFB 0x14 followed by i31 type
  (append (list #xFB #x14)  ; ref.test
          (list #x6C)))     ; i31ref type

(defun emit-fixnump-op ()
  "Emit fixnump predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-numberp-op))

(defun emit-functionp-op (registry)
  "Emit functionp predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (closure-type-index registry)))

(defun emit-stringp-op (registry)
  "Emit stringp predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (string-type-index registry)))

(defun emit-vectorp-op (registry)
  "Emit vectorp predicate.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (vector-type-index registry)))

(defun emit-listp-op (registry)
  "Emit listp predicate (null or cons).
Stack: [anyref] -> [i32 (0 or 1)]"
  ;; (or (null x) (consp x))
  ;; Simplified: just check consp for now
  ;; Full: duplicate, check null, if not check consp, or results
  (emit-ref.test (cons-type-index registry)))

;;; ============================================================
;;; Equality Operations
;;; ============================================================

(defun emit-eq-op ()
  "Emit eq comparison.
Stack: [anyref anyref] -> [i32 (0 or 1)]"
  (list (opcode :ref.eq)))

(defun emit-eql-op ()
  "Emit eql comparison.
EQL is EQ for non-numbers, numeric equality for numbers.
Stack: [anyref anyref] -> [i32 (0 or 1)]
Note: Simplified - just uses ref.eq for now."
  ;; Full implementation needs:
  ;; 1. Check if both are i31ref (numbers)
  ;; 2. If so, compare values
  ;; 3. Otherwise, use ref.eq
  ;; Placeholder: just use ref.eq
  (list (opcode :ref.eq)))

;;; ============================================================
;;; Symbol Operations
;;; ============================================================

(defun emit-symbol-name (registry)
  "Emit symbol-name accessor.
Stack: [symbol] -> [string]"
  (emit-struct.get (symbol-type-index registry) +symbol-name+))

(defun emit-symbol-value (registry)
  "Emit symbol-value accessor.
Stack: [symbol] -> [anyref]"
  (emit-struct.get (symbol-type-index registry) +symbol-value+))

(defun emit-symbol-function (registry)
  "Emit symbol-function accessor.
Stack: [symbol] -> [anyref]"
  (emit-struct.get (symbol-type-index registry) +symbol-function+))

(defun emit-symbol-plist (registry)
  "Emit symbol-plist accessor.
Stack: [symbol] -> [anyref]"
  (emit-struct.get (symbol-type-index registry) +symbol-plist+))

(defun emit-set-symbol-value (registry)
  "Emit symbol-value setter.
Stack: [symbol value] -> []"
  (emit-struct.set (symbol-type-index registry) +symbol-value+))

(defun emit-set-symbol-function (registry)
  "Emit symbol-function setter.
Stack: [symbol function] -> []"
  (emit-struct.set (symbol-type-index registry) +symbol-function+))

(defun emit-set-symbol-plist (registry)
  "Emit symbol-plist setter.
Stack: [symbol plist] -> []"
  (emit-struct.set (symbol-type-index registry) +symbol-plist+))

;;; ============================================================
;;; Vector/Array Operations
;;; ============================================================

(defun emit-make-vector (registry)
  "Emit vector creation with initial value.
Stack: [length init-value] -> [vector]"
  (emit-array.new (vector-type-index registry)))

(defun emit-vector-ref (registry)
  "Emit vector element access.
Stack: [vector index] -> [element]"
  (emit-array.get (vector-type-index registry)))

(defun emit-vector-set (registry)
  "Emit vector element mutation.
Stack: [vector index value] -> []"
  (emit-array.set (vector-type-index registry)))

(defun emit-vector-length ()
  "Emit vector length accessor.
Stack: [vector] -> [i32]"
  (emit-array.len))

;;; ============================================================
;;; Closure Operations
;;; ============================================================

(defun emit-closure-env (registry)
  "Emit closure environment accessor.
Stack: [closure] -> [env]"
  (emit-struct.get (closure-type-index registry) +closure-env+))

(defun emit-make-env (registry length)
  "Emit environment array creation.
Stack: [values...] -> [env]"
  (emit-array.new-fixed (env-type-index registry) length))

(defun emit-env-ref (registry)
  "Emit environment variable access.
Stack: [env index] -> [value]"
  (emit-array.get (env-type-index registry)))

(defun emit-env-set (registry)
  "Emit environment variable mutation.
Stack: [env index value] -> []"
  (emit-array.set (env-type-index registry)))

;;; ============================================================
;;; Primitive Registration
;;; ============================================================

(defun init-primitives ()
  "Initialize the primitive registry with all core primitives."
  (clrhash *primitives*)

  ;; Arithmetic
  (register-primitive '+ 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-add))
    :pure t :foldable t)
  (register-primitive '- 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-sub))
    :pure t :foldable t)
  (register-primitive '* 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-mul))
    :pure t :foldable t)
  (register-primitive 'truncate 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-div))
    :pure t :foldable t)
  (register-primitive 'rem 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-rem))
    :pure t :foldable t)

  ;; Comparisons
  (register-primitive '< 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-lt))
    :pure t :foldable t)
  (register-primitive '<= 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-le))
    :pure t :foldable t)
  (register-primitive '> 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-gt))
    :pure t :foldable t)
  (register-primitive '>= 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-ge))
    :pure t :foldable t)
  (register-primitive '= 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-eq))
    :pure t :foldable t)
  (register-primitive '/= 2
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-neq))
    :pure t :foldable t)
  (register-primitive 'zerop 1
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-zerop))
    :pure t :foldable t)
  (register-primitive 'plusp 1
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-plusp))
    :pure t :foldable t)
  (register-primitive 'minusp 1
    (lambda (reg) (declare (ignore reg)) (emit-fixnum-minusp))
    :pure t :foldable t)

  ;; Cons operations
  (register-primitive 'cons 2 #'emit-cons :pure t)
  (register-primitive 'car 1 #'emit-car-op :pure t)
  (register-primitive 'cdr 1 #'emit-cdr-op :pure t)
  (register-primitive 'rplaca 2 #'emit-rplaca-op :pure nil)
  (register-primitive 'rplacd 2 #'emit-rplacd-op :pure nil)

  ;; Type predicates
  (register-primitive 'null 1 #'emit-null-p :pure t :foldable t)
  (register-primitive 'consp 1 #'emit-consp-op :pure t :foldable t)
  (register-primitive 'atom 1 #'emit-atom-op :pure t :foldable t)
  (register-primitive 'symbolp 1 #'emit-symbolp-op :pure t :foldable t)
  (register-primitive 'numberp 1
    (lambda (reg) (declare (ignore reg)) (emit-numberp-op))
    :pure t :foldable t)
  (register-primitive 'fixnump 1
    (lambda (reg) (declare (ignore reg)) (emit-fixnump-op))
    :pure t :foldable t)
  (register-primitive 'functionp 1 #'emit-functionp-op :pure t :foldable t)
  (register-primitive 'stringp 1 #'emit-stringp-op :pure t :foldable t)
  (register-primitive 'vectorp 1 #'emit-vectorp-op :pure t :foldable t)
  (register-primitive 'listp 1 #'emit-listp-op :pure t :foldable t)

  ;; Equality
  (register-primitive 'eq 2
    (lambda (reg) (declare (ignore reg)) (emit-eq-op))
    :pure t :foldable t)
  (register-primitive 'eql 2
    (lambda (reg) (declare (ignore reg)) (emit-eql-op))
    :pure t :foldable t)

  ;; Symbol operations
  (register-primitive 'symbol-name 1 #'emit-symbol-name :pure t)
  (register-primitive 'symbol-value 1 #'emit-symbol-value :pure nil)
  (register-primitive 'symbol-function 1 #'emit-symbol-function :pure nil)
  (register-primitive 'symbol-plist 1 #'emit-symbol-plist :pure nil)

  ;; Vector operations
  (register-primitive 'make-array 2 #'emit-make-vector :pure t)
  (register-primitive 'aref 2 #'emit-vector-ref :pure t)
  (register-primitive 'svref 2 #'emit-vector-ref :pure t)
  (register-primitive 'length 1
    (lambda (reg) (declare (ignore reg)) (emit-vector-length))
    :pure t)

  *primitives*)

;; Initialize on load
(init-primitives)
