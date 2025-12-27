;;;; func-section.lisp - Function and Code Section generation
;;;; Compiles AST nodes to Wasm instructions

(in-package #:clysm/compiler/codegen/func-section)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct (compilation-env (:conc-name cenv-)
                            (:copier nil))
  "Compilation environment tracking locals and scope."
  (locals nil :type list)              ; ((name . index) ...)
  (local-counter-box nil :type list)   ; (counter) - mutable box for shared counter
  (local-types-box nil :type list)     ; ((index . type) ...) - boxed for sharing
  (functions nil :type list)           ; ((name . index) ...)
  (function-counter 0 :type fixnum)
  (in-tail-position nil :type boolean)
  (captured-vars nil :type list)       ; ((name . position) ...) for captured vars
  (local-functions nil :type list)     ; ((name . local-idx) ...) for flet/labels
  ;; Phase 6: Exception handling
  (blocks nil :type list)              ; ((name . block-depth) ...) for block/return-from
  (block-depth 0 :type fixnum)         ; Current nesting depth for br targets
  (tagbody-tags nil :type list)        ; ((tag . label-idx) ...) for tagbody/go - legacy field
  (tagbody-context nil :type (or null tagbody-context)) ; Active tagbody compilation context
  (catch-tags nil :type list)          ; ((tag-expr . handler-depth) ...) for catch/throw
  (unwind-stack nil :type list))       ; Stack of unwind-protect handlers

;;; ============================================================
;;; Tagbody Context (for tagbody/go compilation)
;;; ============================================================

(defstruct tagbody-context
  "Context for compiling tagbody/go control flow.
   Created when entering compile-tagbody, stored in compilation-env,
   accessed by compile-go to determine jump target."
  (strategy nil :type keyword)
  ;; :sequential | :simple-loop | :dispatch

  (tags nil :type list)
  ;; Association list: ((tag-symbol . segment-index) ...)
  ;; Maps tag names to their segment indices for go target resolution

  (pc-local nil :type (or null fixnum))
  ;; Index of $pc local variable (dispatch strategy only)
  ;; NIL for sequential and simple-loop strategies

  (base-loop-depth 0 :type fixnum)
  ;; Base br depth from the loop body to the loop target
  ;; For simple-loop: 0 (br 0 targets the loop)
  ;; For dispatch: varies per segment (num-segments - seg-idx)

  (base-block-depth 0 :type fixnum)
  ;; Block depth when entering the tagbody's loop
  ;; Used to calculate correct br depth when nested in if/block structures

  (loop-label nil :type (or null symbol))
  ;; Wasm label for loop target (simple-loop and dispatch strategies)
  ;; e.g., $LOOP for simple-loop, $dispatch for dispatch

  (num-segments nil :type (or null fixnum))
  ;; Total number of segments (used for depth calculation)
  )

(defun make-env ()
  "Create a fresh compilation environment."
  (make-compilation-env :local-counter-box (list 0)
                        :local-types-box (list nil)))

(defun cenv-local-counter (env)
  "Get the local counter value."
  (car (cenv-local-counter-box env)))

(defun (setf cenv-local-counter) (value env)
  "Set the local counter value."
  (setf (car (cenv-local-counter-box env)) value))

(defun env-lookup-local (env name)
  "Look up a local variable index."
  (cdr (assoc name (cenv-locals env))))

(defun env-add-local (env name &optional (type :anyref))
  "Add a local variable with optional type and return its index.
   Type defaults to :anyref. Non-anyref types are tracked in local-types-box."
  (let ((idx (cenv-local-counter env)))
    (push (cons name idx) (cenv-locals env))
    ;; Track non-anyref types in the boxed list
    (unless (eq type :anyref)
      (setf (car (cenv-local-types-box env))
            (cons (cons idx type) (car (cenv-local-types-box env)))))
    (incf (car (cenv-local-counter-box env)))
    idx))

(defun env-local-type (env idx)
  "Get the type of a local by index. Returns :anyref if not explicitly tracked."
  (or (cdr (assoc idx (car (cenv-local-types-box env)))) :anyref))

(defun env-lookup-function (env name)
  "Look up a function index."
  (cdr (assoc name (cenv-functions env))))

(defun env-add-function (env name)
  "Add a function and return its index."
  (let ((idx (cenv-function-counter env)))
    (push (cons name idx) (cenv-functions env))
    (incf (cenv-function-counter env))
    idx))

(defun env-set-function-counter (env value)
  "Set the function counter to a specific value."
  (setf (cenv-function-counter env) value))

;;; ============================================================
;;; Wasm Instruction Opcodes
;;; ============================================================

(defparameter *wasm-opcodes*
  '(;; Control
    (:unreachable . #x00)
    (:nop . #x01)
    (:block . #x02)
    (:loop . #x03)
    (:if . #x04)
    (:else . #x05)
    (:end . #x0B)
    (:br . #x0C)
    (:br_if . #x0D)
    (:return . #x0F)
    (:call . #x10)
    (:call_indirect . #x11)
    ;; Reference
    (:ref.null . #xD0)
    (:ref.is_null . #xD1)
    (:ref.func . #xD2)
    (:ref.eq . #xD3)
    ;; Parametric
    (:drop . #x1A)
    (:select . #x1B)
    ;; Variable
    (:local.get . #x20)
    (:local.set . #x21)
    (:local.tee . #x22)
    (:global.get . #x23)
    (:global.set . #x24)
    ;; Numeric i32
    (:i32.const . #x41)
    (:i32.eqz . #x45)
    (:i32.eq . #x46)
    (:i32.ne . #x47)
    (:i32.lt_s . #x48)
    (:i32.lt_u . #x49)
    (:i32.gt_s . #x4A)
    (:i32.gt_u . #x4B)
    (:i32.le_s . #x4C)
    (:i32.le_u . #x4D)
    (:i32.ge_s . #x4E)
    (:i32.ge_u . #x4F)
    (:i32.add . #x6A)
    (:i32.sub . #x6B)
    (:i32.mul . #x6C)
    (:i32.div_s . #x6D)
    (:i32.div_u . #x6E)
    (:i32.rem_s . #x6F)
    (:i32.rem_u . #x70)
    (:i32.and . #x71)
    (:i32.or . #x72)
    (:i32.xor . #x73)
    ;; i64
    (:i64.const . #x42)
    ;; GC instructions (0xFB prefix)
    (:ref.i31 . (#xFB #x1C))      ; Create i31ref from i32
    (:i31.get_s . (#xFB #x1D))   ; Extract signed i32 from i31ref
    (:i31.get_u . (#xFB #x1E))   ; Extract unsigned i32 from i31ref
    (:struct.new . (#xFB #x00))
    (:struct.get . (#xFB #x02))
    (:struct.set . (#xFB #x05))
    (:ref.cast . (#xFB #x17))    ; ref.cast to a specific type
    ;; Function reference instructions
    (:call_ref . #x14)           ; call_ref (typed function calls)
    (:return_call . #x12)        ; return_call (tail call)
    (:return_call_ref . #x15)))  ; return_call_ref (tail call through ref)

;;; ============================================================
;;; Main Compilation Entry Point
;;; ============================================================

(defun compile-to-instructions (ast env)
  "Compile an AST node to a list of Wasm instructions."
  (etypecase ast
    (clysm/compiler/ast:ast-literal
     (compile-literal ast))
    (clysm/compiler/ast:ast-var-ref
     (compile-var-ref ast env))
    (clysm/compiler/ast:ast-call
     (compile-call ast env))
    (clysm/compiler/ast:ast-if
     (compile-if ast env))
    (clysm/compiler/ast:ast-let
     (compile-let ast env))
    (clysm/compiler/ast:ast-progn
     (compile-progn ast env))
    (clysm/compiler/ast:ast-setq
     (compile-setq ast env))
    (clysm/compiler/ast:ast-defun
     (compile-defun ast env))
    (clysm/compiler/ast:ast-lambda
     (compile-lambda ast env))
    (clysm/compiler/ast:ast-flet
     (compile-flet ast env))
    (clysm/compiler/ast:ast-labels
     (compile-labels ast env))
    (clysm/compiler/ast:ast-block
     (compile-block ast env))
    (clysm/compiler/ast:ast-return-from
     (compile-return-from ast env))
    (clysm/compiler/ast:ast-tagbody
     (compile-tagbody ast env))
    (clysm/compiler/ast:ast-go
     (compile-go ast env))
    (clysm/compiler/ast:ast-catch
     (compile-catch ast env))
    (clysm/compiler/ast:ast-throw
     (compile-throw ast env))
    (clysm/compiler/ast:ast-unwind-protect
     (compile-unwind-protect ast env))
    ;; Special variable declarations (T022-T024)
    (clysm/compiler/ast:ast-defvar
     (compile-defvar ast env))
    (clysm/compiler/ast:ast-defparameter
     (compile-defparameter ast env))
    ;; Feature 038: Constant definitions
    (clysm/compiler/ast:ast-defconstant
     (compile-defconstant ast env))
    ;; Macro introspection (016-macro-system T048, 042-advanced-defmacro)
    (clysm/compiler/ast:ast-macroexpand-1
     (compile-macroexpand-1 ast env))
    (clysm/compiler/ast:ast-macroexpand
     (compile-macroexpand ast env))
    (clysm/compiler/ast:ast-macro-function
     (compile-macro-function ast env))
    ;; Multiple values (025-multiple-values)
    (clysm/compiler/ast:ast-values
     (compile-values ast env))
    (clysm/compiler/ast:ast-multiple-value-bind
     (compile-multiple-value-bind ast env))
    (clysm/compiler/ast:ast-multiple-value-list
     (compile-multiple-value-list ast env))
    (clysm/compiler/ast:ast-nth-value
     (compile-nth-value ast env))
    (clysm/compiler/ast:ast-values-list
     (compile-values-list ast env))
    (clysm/compiler/ast:ast-multiple-value-prog1
     (compile-multiple-value-prog1 ast env))
    (clysm/compiler/ast:ast-multiple-value-call
     (compile-multiple-value-call ast env))
    ;; CLOS (026-clos-foundation)
    (clysm/compiler/ast:ast-defclass
     (compile-defclass ast env))
    (clysm/compiler/ast:ast-make-instance
     (compile-make-instance ast env))
    (clysm/compiler/ast:ast-defmethod
     (compile-defmethod ast env))
    ;; FFI (027-complete-ffi)
    (clysm/compiler/ast:ast-ffi-call
     (compile-ffi-call ast env))
    (clysm/compiler/ast:ast-call-host
     (compile-call-host ast env))))

;;; ============================================================
;;; FFI Call Compilation (027-complete-ffi)
;;; ============================================================

(defun compile-ffi-call (ast env)
  "Compile an FFI function call to Wasm instructions.
   This generates:
   1. Compile and marshal each argument from Lisp to Wasm type
   2. Call the imported function by its assigned function index
   3. Unmarshal the result from Wasm to Lisp type (or produce NIL for :void)

   Constitution I compliance: Uses WasmGC types only, no linear memory."
  (let* ((decl (clysm/compiler/ast:ast-ffi-call-declaration ast))
         (args (clysm/compiler/ast:ast-ffi-call-arguments ast))
         (param-types (clysm/ffi:ffd-param-types decl))
         (return-type (clysm/ffi:ffd-return-type decl))
         (func-index (or (clysm/ffi:ffd-type-index decl) 0))
         (result '()))
    ;; Step 1: Compile and marshal each argument
    (loop for arg in args
          for param-type in param-types
          do
             ;; Compile the argument expression
             (let ((arg-instrs (compile-to-instructions arg env)))
               (dolist (instr arg-instrs)
                 (push instr result)))
             ;; Add marshalling instructions (Lisp -> Wasm)
             (let ((marshal-instrs (clysm/ffi:marshal-to-wasm param-type)))
               (when marshal-instrs
                 (if (atom marshal-instrs)
                     ;; Single instruction like 'i31.get_s
                     (push (list marshal-instrs) result)
                     ;; List of instructions
                     (dolist (instr marshal-instrs)
                       (push (if (listp instr) instr (list instr)) result))))))
    ;; Step 2: Call the imported function
    (push (list :call func-index) result)
    ;; Step 3: Unmarshal return value
    (if (eq return-type :void)
        ;; Void functions return NIL
        (push '(:ref.null :none) result)
        ;; Non-void: unmarshal the result
        (let ((unmarshal-instrs (clysm/ffi:marshal-from-wasm return-type)))
          (when unmarshal-instrs
            (if (atom unmarshal-instrs)
                (push (list unmarshal-instrs) result)
                (dolist (instr unmarshal-instrs)
                  (push (if (listp instr) instr (list instr)) result))))))
    (nreverse result)))

(defun compile-call-host (ast env)
  "Compile a dynamic host function call (ffi:call-host) to Wasm instructions.
   This generates:
   1. Evaluate the function name expression (string) → externref
   2. Compile each argument to anyref
   3. Create an anyref array with the arguments → externref
   4. Call the $ffi_call_host_dynamic import
   5. Convert result from externref to anyref

   The dynamic dispatch is handled by the host environment."
  (let* ((func-name (clysm/compiler/ast:ast-call-host-function-name ast))
         (args (clysm/compiler/ast:ast-call-host-arguments ast))
         (num-args (length args))
         (result '()))
    ;; Step 1: Compile function name and convert to externref
    (dolist (instr (compile-to-instructions func-name env))
      (push instr result))
    ;; Convert the string (anyref) to externref for host
    (push '(:extern.convert_any) result)

    ;; Step 2: Compile arguments and create array
    (if (zerop num-args)
        ;; No arguments - push null externref
        (push '(:ref.null :extern) result)
        (progn
          ;; Compile each argument (result is anyref)
          (dolist (arg args)
            (dolist (instr (compile-to-instructions arg env))
              (push instr result)))
          ;; Create anyref array from stack values
          (push `(:array.new_fixed ,clysm/compiler/codegen/gc-types:+type-anyref-array+ ,num-args) result)
          ;; Convert array to externref
          (push '(:extern.convert_any) result)))

    ;; Step 3: Call the dynamic dispatch import
    ;; The import index is assigned during module compilation
    ;; Use a placeholder that will be resolved during linking
    (push '(:call $ffi_call_host_dynamic) result)

    ;; Step 4: Convert result from externref to anyref
    (push '(:any.convert_extern) result)

    (nreverse result)))

;;; ============================================================
;;; Literal Compilation
;;; ============================================================

(defun compile-literal (ast)
  "Compile a literal value to Wasm instructions."
  (let ((value (clysm/compiler/ast:ast-literal-value ast))
        (type (clysm/compiler/ast:ast-literal-literal-type ast)))
    (case type
      (:fixnum
       ;; Fixnums are represented as i31ref (T047)
       ;; i32.const value, ref.i31
       (list (list :i32.const value)
             :ref.i31))
      (:character
       ;; Characters are represented as i31ref with Unicode codepoint (008-character-string)
       ;; Same as fixnum, but semantically different type
       (list (list :i32.const (char-code value))
             :ref.i31))
      (:nil
       ;; NIL is represented as ref.null (null reference)
       (list '(:ref.null :none)))
      (:t
       ;; T is represented as non-null (use i31ref of 1)
       (list '(:i32.const 1)
             :ref.i31))
      (:string
       ;; Strings are UTF-8 byte arrays (008-character-string)
       ;; Use array.new_fixed to create array from bytes on stack
       (compile-string-literal value))
      (:quoted
       ;; Quoted values - handle lists, symbols, and atoms
       (cond
         ;; NIL
         ((null value)
          (list '(:ref.null :none)))
         ;; List - build cons structure
         ((listp value)
          (compile-quoted-list value))
         ;; For symbols, represent as i31ref of symbol hash (placeholder)
         (t
          (let ((hash (logand (sxhash value) #x3FFFFFFF)))  ; 30-bit for i31ref
            (list (list :i32.const hash)
                  :ref.i31)))))
      ;; Numeric tower types (010-numeric-tower)
      (:bignum
       (compile-bignum-literal value))
      (:ratio
       (compile-ratio-literal value))
      (:float
       (compile-float-literal value))
      (:complex
       (compile-complex-literal value))
      (otherwise
       (error "Unsupported literal type: ~A" type)))))

(defun compile-string-literal (string)
  "Compile a string literal to Wasm instructions.
   Creates a UTF-8 byte array using array.new_fixed.
   Stack: [] -> [ref $string]"
  (let ((bytes (clysm/lib/utf8:string-to-utf8-octets string))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    (if (zerop (length bytes))
        ;; Empty string: just create empty array
        (list (list :array.new_fixed string-type 0))
        ;; Non-empty: push each byte then create array
        (let ((result '()))
          (loop for byte across bytes
                do (push (list :i32.const byte) result))
          (setf result (nreverse result))
          (append result
                  (list (list :array.new_fixed string-type (length bytes))))))))

(defun compile-quoted-list (lst)
  "Compile a quoted list to a cons structure.
   Builds the list from right to left, creating cons cells."
  (if (null lst)
      '((:ref.null :none))
      (let ((result '()))
        ;; Compile car element
        (setf result (append result (compile-quoted-element (car lst))))
        ;; Compile cdr (rest of list)
        (setf result (append result (compile-quoted-list (cdr lst))))
        ;; Create cons cell
        (setf result (append result
                             (list (list :struct.new
                                         clysm/compiler/codegen/gc-types:+type-cons+))))
        result)))

(defun compile-quoted-element (elem)
  "Compile a single quoted element to Wasm instructions."
  (cond
    ((null elem) '((:ref.null :none)))
    ((integerp elem)
     ;; Handle fixnum vs bignum (010-numeric-tower)
     (if (clysm/compiler/ast:i31-range-p elem)
         (list (list :i32.const elem) :ref.i31)
         (compile-bignum-literal elem)))
    ((typep elem 'ratio) (compile-ratio-literal elem))
    ((floatp elem) (compile-float-literal elem))
    ((complexp elem) (compile-complex-literal elem))
    ((listp elem) (compile-quoted-list elem))
    ((symbolp elem)
     (let ((hash (logand (sxhash elem) #x3FFFFFFF)))
       (list (list :i32.const hash) :ref.i31)))
    (t (error "Cannot compile quoted element: ~A" elem))))

;;; ============================================================
;;; Numeric Tower Literal Compilation (010-numeric-tower)
;;; ============================================================

(defun integer-to-limbs (n)
  "Convert a non-negative integer to a list of 32-bit limbs (little-endian).
   Returns (values limbs sign) where sign is 0 for non-negative, 1 for negative."
  (let ((sign (if (minusp n) 1 0))
        (abs-n (abs n))
        (limbs '()))
    (if (zerop abs-n)
        (values '(0) 0)
        (progn
          (loop while (plusp abs-n)
                do (push (logand abs-n #xFFFFFFFF) limbs)
                   (setf abs-n (ash abs-n -32)))
          (values (nreverse limbs) sign)))))

(defun unsigned-to-signed-i32 (n)
  "Convert an unsigned 32-bit integer to its signed representation.
   Values >= 2^31 become negative numbers in two's complement."
  (if (>= n (expt 2 31))
      (- n (expt 2 32))
      n))

(defun compile-bignum-literal (value)
  "Compile a bignum literal to Wasm instructions.
   Creates a $bignum struct with sign and limb array.
   Stack: [] -> [ref $bignum]"
  (multiple-value-bind (limbs sign) (integer-to-limbs value)
    (let ((limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+)
          (bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
          (result '()))
      ;; Push sign
      (push (list :i32.const sign) result)
      ;; Push each limb value, then create the array
      ;; Convert unsigned limb values to signed i32 for proper LEB128 encoding
      (dolist (limb limbs)
        (push (list :i32.const (unsigned-to-signed-i32 limb)) result))
      ;; Create limb array with array.new_fixed
      (push (list :array.new_fixed limb-array-type (length limbs)) result)
      ;; Create bignum struct (sign, limbs)
      (push (list :struct.new bignum-type) result)
      (nreverse result))))

(defun compile-ratio-literal (value)
  "Compile a ratio literal to Wasm instructions.
   Creates a $ratio struct with numerator and denominator.
   Stack: [] -> [ref $ratio]"
  (let ((num (numerator value))
        (den (denominator value))
        (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+))
    ;; Compile numerator (may be fixnum or bignum)
    (append
     (if (clysm/compiler/ast:i31-range-p num)
         (list (list :i32.const num) :ref.i31)
         (compile-bignum-literal num))
     ;; Compile denominator (always positive, may be fixnum or bignum)
     (if (clysm/compiler/ast:i31-range-p den)
         (list (list :i32.const den) :ref.i31)
         (compile-bignum-literal den))
     ;; Create ratio struct
     (list (list :struct.new ratio-type)))))

(defun compile-float-literal (value)
  "Compile a float literal to Wasm instructions.
   Creates a $float struct with f64 value.
   Stack: [] -> [ref $float]"
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+))
    (list (list :f64.const (coerce value 'double-float))
          (list :struct.new float-type))))

(defun compile-complex-literal (value)
  "Compile a complex literal to Wasm instructions.
   Creates a $complex struct with real and imaginary parts.
   Stack: [] -> [ref $complex]"
  (let ((real-part (realpart value))
        (imag-part (imagpart value))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (append
     ;; Compile real part
     (compile-numeric-literal-part real-part)
     ;; Compile imaginary part
     (compile-numeric-literal-part imag-part)
     ;; Create complex struct
     (list (list :struct.new complex-type)))))

(defun compile-numeric-literal-part (value)
  "Compile a numeric value that can be any real type.
   Used for complex number parts."
  (cond
    ((and (integerp value) (clysm/compiler/ast:i31-range-p value))
     (list (list :i32.const value) :ref.i31))
    ((integerp value)
     (compile-bignum-literal value))
    ((typep value 'ratio)
     (compile-ratio-literal value))
    ((floatp value)
     (compile-float-literal value))
    (t (error "Cannot compile numeric part: ~A" value))))

;;; ============================================================
;;; Variable Reference Compilation (T048)
;;; ============================================================

(defun compile-var-ref (ast env)
  "Compile a variable reference.
   Handles locals, captured variables from closures, special variables, and globals."
  (let* ((name (clysm/compiler/ast:ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable
      (local-idx
       (list (list :local.get local-idx)))
      ;; Captured variable from closure environment
      ((env-lookup-captured env name)
       (compile-captured-var-access name env))
      ;; Special variable (T037)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-var-ref name))
      ;; Unknown variable
      (t
       (error "Unbound variable: ~A" name)))))

(defun compile-special-var-ref (name)
  "Compile a reference to a special variable (T037).
   Generates code to read the symbol's current value.
   Pattern: global.get <index>, struct.get $symbol $value"
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    `((:global.get ,global-idx)
      (:struct.get ,symbol-type 1))))

(defun env-lookup-captured (env name)
  "Look up a captured variable position."
  (cdr (assoc name (cenv-captured-vars env))))

(defun compile-captured-var-access (name env)
  "Generate instructions to access a captured variable from the closure's env.
   The closure is in local 0 ($closure).
   The env is a cons-list: (cons var0 (cons var1 (cons var2 nil)))
   To get var at position N, we need N cdrs followed by a car."
  (let* ((position (env-lookup-captured env name))
         (result '()))
    ;; Get the closure (local 0)
    (setf result (append result '((:local.get 0))))
    ;; Cast to closure type
    (setf result (append result
                         (list (list :ref.cast
                                     clysm/compiler/codegen/gc-types:+type-closure+))))
    ;; Get the env field (field index 4)
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-closure+
                                     4))))
    ;; Navigate the cons-list: N times cdr, then car
    (dotimes (i position)
      ;; Cast to cons type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-cons+))))
      ;; Get cdr (field index 1)
      (setf result (append result
                           (list (list :struct.get
                                       clysm/compiler/codegen/gc-types:+type-cons+
                                       1)))))
    ;; Cast final position to cons and get car (field index 0)
    (setf result (append result
                         (list (list :ref.cast
                                     clysm/compiler/codegen/gc-types:+type-cons+))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-cons+
                                     0))))
    result))

;;; ============================================================
;;; Function Call Compilation (T049-T052)
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((function (clysm/compiler/ast:ast-call-function ast))
        (args (clysm/compiler/ast:ast-call-arguments ast)))
    ;; Check for special forms and primitives
    (cond
      ;; funcall special form
      ((and (symbolp function) (eq function 'funcall))
       (compile-funcall args env))
      ;; Primitive operators
      ((and (symbolp function)
            (member function '(+ - * / < > <= >= = /= truncate
                               ;; Cons/list operations (006-cons-list-ops)
                               cons car cdr list
                               consp null not atom listp
                               ;; Equality predicates (024-equality-predicates)
                               eq eql equal equalp
                               rplaca rplacd
                               first second third fourth fifth
                               sixth seventh eighth ninth tenth
                               rest nth nthcdr
                               ;; Sequence functions (007-sequence-functions)
                               length append reverse nreverse last butlast copy-list
                               mapcar mapc maplist reduce
                               find find-if position position-if
                               remove remove-if remove-if-not
                               count count-if
                               member assoc rassoc
                               every some notany notevery
                               ;; Character functions (008-character-string)
                               char-code code-char
                               char= char/= char< char> char<= char>=
                               char-equal char-lessp char-greaterp
                               char-not-lessp char-not-greaterp
                               char-upcase char-downcase
                               characterp alpha-char-p digit-char-p
                               alphanumericp upper-case-p lower-case-p
                               stringp char schar
                               string= string/= string< string> string<= string>=
                               string-equal string-lessp string-greaterp
                               string-not-lessp string-not-greaterp string-not-equal
                               make-string string string-upcase string-downcase string-capitalize
                               subseq concatenate
                               ;; Numeric accessors (019-numeric-accessors)
                               numerator denominator
                               ;; ANSI CL Type Predicates (023-type-predicates)
                               integerp floatp rationalp complexp numberp
                               symbolp functionp
                               ;; ANSI CL Numeric Predicates (023-type-predicates)
                               zerop plusp minusp oddp evenp
                               ;; ANSI CL Signum (023-type-predicates)
                               signum)))
       (compile-primitive-call function args env))
      ;; Local function (from flet/labels)
      ((and (symbolp function) (env-lookup-local-function env function))
       (compile-local-function-call function args env))
      ;; Regular function call
      (t
       (compile-regular-call function args env)))))

(defun env-lookup-local-function (env name)
  "Look up a local function's local variable index or :captured marker."
  (cdr (assoc name (cenv-local-functions env))))

(defun compile-local-function-call (function args env)
  "Compile a call to a local function (from flet/labels).
   The function is stored as a closure in a local variable or captured env.
   Uses :return_call_ref for tail calls (TCO), :call_ref otherwise."
  (let* ((local-func-info (env-lookup-local-function env function))
         (arity (length args))
         (result '())
         (arg-env (env-with-non-tail env)))
    ;; Get the closure - either from local or from captured env
    (if (eq local-func-info :captured)
        ;; Captured in closure env - use captured-var access
        (setf result (append result (compile-captured-var-access function env)))
        ;; In a local variable
        (setf result (append result (list (list :local.get local-func-info)))))
    ;; Duplicate for the first parameter (closure as self-reference)
    (let ((closure-local (cenv-local-counter env)))
      (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
      ;; Save closure to temp local
      (setf result (append result (list (list :local.set closure-local))))
      ;; Push closure as first argument (self reference)
      (setf result (append result (list (list :local.get closure-local))))
      ;; Push all call arguments (not in tail position)
      (dolist (arg args)
        (setf result (append result (compile-to-instructions arg arg-env))))
      ;; Get closure and extract code field
      (setf result (append result (list (list :local.get closure-local))))
      ;; Cast to closure type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      ;; Get the appropriate code field based on arity
      (let ((code-field (case arity
                          (0 0)   ; code_0
                          (1 1)   ; code_1
                          (2 2)   ; code_2
                          (t 3)))) ; code_N
        (setf result (append result
                             (list (list :struct.get
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         code-field)))))
      ;; Cast funcref to specific function type and call
      ;; Use return_call_ref for tail calls
      (let ((func-type (case arity
                         (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                         (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                         (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                         (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                         (t clysm/compiler/codegen/gc-types:+type-func-n+))))
        (setf result (append result (list (list :ref.cast func-type))))
        (if (cenv-in-tail-position env)
            (setf result (append result (list (list :return_call_ref func-type))))
            (setf result (append result (list (list :call_ref func-type)))))))
    result))

(defun compile-primitive-call (op args env)
  "Compile a primitive operation.
   Arguments to primitives are NOT in tail position."
  ;; Clear tail position for all arguments - primitive ops aren't tail calls
  (let ((env (env-with-non-tail env)))
    (case op
    ;; Arithmetic operators (T049-T052)
    (+  (compile-arithmetic-op :i32.add args env 0))
    (-  (if (= 1 (length args))
            (compile-unary-minus (first args) env)
            (compile-arithmetic-op :i32.sub args env nil)))
    (*  (compile-arithmetic-op :i32.mul args env 1))
    (/  (compile-arithmetic-op :i32.div_s args env nil))
    (truncate (compile-truncate args env))
    ;; Comparison operators (T053)
    (<  (compile-comparison-op :i32.lt_s args env))
    (>  (compile-comparison-op :i32.gt_s args env))
    (<= (compile-comparison-op :i32.le_s args env))
    (>= (compile-comparison-op :i32.ge_s args env))
    (=  (compile-comparison-op :i32.eq args env))
    (/= (compile-not-equal args env))
    ;; Cons/list operations (006-cons-list-ops)
    (cons (compile-cons args env))
    (car (compile-car args env))
    (cdr (compile-cdr args env))
    (list (compile-list args env))
    ;; Type predicates
    (consp (compile-consp args env))
    (null (compile-null args env))
    (not (compile-not args env))
    (atom (compile-atom args env))
    (listp (compile-listp args env))
    ;; Equality predicates (024-equality-predicates)
    (eq (compile-eq args env))
    (eql (compile-eql args env))
    (equal (compile-equal args env))
    (equalp (compile-equalp args env))
    ;; ANSI CL Type Predicates (023-type-predicates)
    (integerp (compile-integerp args env))
    (floatp (compile-floatp args env))
    (rationalp (compile-rationalp args env))
    (complexp (compile-complexp args env))
    (numberp (compile-numberp args env))
    (symbolp (compile-symbolp args env))
    (functionp (compile-functionp args env))
    ;; ANSI CL Numeric Predicates (023-type-predicates)
    (zerop (compile-zerop args env))
    (plusp (compile-plusp args env))
    (minusp (compile-minusp args env))
    (oddp (compile-oddp args env))
    (evenp (compile-evenp args env))
    ;; ANSI CL Signum (023-type-predicates)
    (signum (compile-signum args env))
    ;; Destructive modification
    (rplaca (compile-rplaca args env))
    (rplacd (compile-rplacd args env))
    ;; List accessors
    (first (compile-car args env))
    (rest (compile-cdr args env))
    (second (compile-nth-accessor 1 args env))
    (third (compile-nth-accessor 2 args env))
    (fourth (compile-nth-accessor 3 args env))
    (fifth (compile-nth-accessor 4 args env))
    (sixth (compile-nth-accessor 5 args env))
    (seventh (compile-nth-accessor 6 args env))
    (eighth (compile-nth-accessor 7 args env))
    (ninth (compile-nth-accessor 8 args env))
    (tenth (compile-nth-accessor 9 args env))
    (nth (compile-nth args env))
    (nthcdr (compile-nthcdr args env))
    ;; Sequence functions (007-sequence-functions)
    (length (compile-length args env))
    (append (compile-append args env))
    (reverse (compile-reverse args env))
    (nreverse (compile-nreverse args env))
    (last (compile-last args env))
    (butlast (compile-butlast args env))
    (copy-list (compile-copy-list args env))
    ;; Higher-order sequence functions
    (mapcar (compile-mapcar args env))
    (mapc (compile-mapc args env))
    (maplist (compile-maplist args env))
    (reduce (compile-reduce args env))
    ;; Search and filter functions
    (find (compile-find args env))
    (find-if (compile-find-if args env))
    (position (compile-position args env))
    (position-if (compile-position-if args env))
    (remove (compile-remove args env))
    (remove-if (compile-remove-if args env))
    (remove-if-not (compile-remove-if-not args env))
    (count (compile-count args env))
    (count-if (compile-count-if args env))
    ;; Membership and association
    (member (compile-member args env))
    (assoc (compile-assoc args env))
    (rassoc (compile-rassoc args env))
    ;; Quantifier predicates
    (every (compile-every args env))
    (some (compile-some args env))
    (notany (compile-notany args env))
    (notevery (compile-notevery args env))
    ;; Character functions (008-character-string)
    (char-code (compile-char-code args env))
    (code-char (compile-code-char args env))
    (char= (compile-char= args env))
    (char/= (compile-char/= args env))
    (char< (compile-char< args env))
    (char> (compile-char> args env))
    (char<= (compile-char<= args env))
    (char>= (compile-char>= args env))
    (char-equal (compile-char-equal args env))
    (char-lessp (compile-char-lessp args env))
    (char-greaterp (compile-char-greaterp args env))
    (char-not-lessp (compile-char-not-lessp args env))
    (char-not-greaterp (compile-char-not-greaterp args env))
    (char-upcase (compile-char-upcase args env))
    (char-downcase (compile-char-downcase args env))
    (characterp (compile-characterp args env))
    (alpha-char-p (compile-alpha-char-p args env))
    (digit-char-p (compile-digit-char-p args env))
    (alphanumericp (compile-alphanumericp args env))
    (upper-case-p (compile-upper-case-p args env))
    (lower-case-p (compile-lower-case-p args env))
    ;; String functions (008-character-string)
    (stringp (compile-stringp args env))
    (char (compile-string-char args env))
    (schar (compile-string-char args env))
    ;; String comparison functions
    (string= (compile-string= args env))
    (string/= (compile-string/= args env))
    (string< (compile-string< args env))
    (string> (compile-string> args env))
    (string<= (compile-string<= args env))
    (string>= (compile-string>= args env))
    ;; Case-insensitive string comparison functions
    (string-equal (compile-string-equal args env))
    (string-lessp (compile-string-lessp args env))
    (string-greaterp (compile-string-greaterp args env))
    (string-not-lessp (compile-string-not-lessp args env))
    (string-not-greaterp (compile-string-not-greaterp args env))
    (string-not-equal (compile-string-not-equal args env))
    ;; String generation/conversion functions
    (make-string (compile-make-string args env))
    (string (compile-string args env))
    (string-upcase (compile-string-upcase args env))
    (string-downcase (compile-string-downcase args env))
    (string-capitalize (compile-string-capitalize args env))
    ;; Substring and concatenation
    (subseq (compile-subseq args env))
    (concatenate (compile-concatenate args env))
    ;; Numeric accessors (019-numeric-accessors)
    (numerator (compile-numerator args env))
    (denominator (compile-denominator args env)))))

;;; ============================================================
;;; Numeric Tower Type Dispatch (T013-T015)
;;; ============================================================

(defun emit-numeric-type-p ()
  "Generate instructions to check if a value on stack is any numeric type (T013).
   Expects: anyref on stack
   Returns: i32 (1 if numeric, 0 otherwise)
   Checks: i31ref (fixnum), $bignum, $ratio, $float, $complex"
  `(;; Value is on stack as anyref
    ;; Check if i31ref (fixnum)
    (:block (:result :i32)
      (:block
        ;; Try casting to i31 - if succeeds, it's a fixnum
        (:br_on_cast 1 :anyref :i31 ,clysm/compiler/codegen/gc-types:+type-bignum+))
      ;; Check bignum
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+)))
      ;; Check ratio
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+)))
      ;; Check float
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
      ;; Check complex
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Not a number
      (:i32.const 0)
      (:br 5))  ; exit with 0
    ;; Was a number - return 1
    :drop  ; drop the casted value
    (:i32.const 1)))

(defun emit-get-numeric-type ()
  "Generate instructions to get the numeric type of a value (T014).
   Expects: anyref on stack
   Returns: i32 type code
     0 = not-a-number
     1 = fixnum (i31ref)
     2 = bignum
     3 = ratio
     4 = float
     5 = complex"
  `(;; Duplicate value for testing (original stays on stack)
    ;; We need block-based type dispatch
    (:block (:result :i32)
      ;; Check i31 (fixnum) first
      (:block
        (:block
          (:block
            (:block
              (:block
                ;; Try i31
                (:br_on_cast 0 :anyref :i31))
              ;; Try bignum
              (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+)))
            ;; Try ratio
            (:br_on_cast 2 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+)))
          ;; Try float
          (:br_on_cast 3 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
        ;; Try complex
        (:br_on_cast 4 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Not a number
      :drop
      (:i32.const 0)
      (:br 0))
    ;; i31/fixnum path
    :drop
    (:i32.const 1)
    (:br 0))
  ;; bignum path
  ;; ... (needs proper block structure)
  )

(defconstant +numeric-type-not-number+ 0)
(defconstant +numeric-type-fixnum+ 1)
(defconstant +numeric-type-bignum+ 2)
(defconstant +numeric-type-ratio+ 3)
(defconstant +numeric-type-float+ 4)
(defconstant +numeric-type-complex+ 5)

(defun numeric-type-contagion (type1 type2)
  "Determine result type from two numeric types per CLHS 12.1.4 (T015).
   Returns the more general type:
   complex > float > ratio > bignum > fixnum"
  (cond
    ;; Complex contagion: if either is complex, result is complex
    ((or (= type1 +numeric-type-complex+)
         (= type2 +numeric-type-complex+))
     +numeric-type-complex+)
    ;; Float contagion: if either is float, result is float
    ((or (= type1 +numeric-type-float+)
         (= type2 +numeric-type-float+))
     +numeric-type-float+)
    ;; Ratio contagion: if either is ratio, result is ratio
    ((or (= type1 +numeric-type-ratio+)
         (= type2 +numeric-type-ratio+))
     +numeric-type-ratio+)
    ;; Bignum contagion: if either is bignum, result is bignum
    ((or (= type1 +numeric-type-bignum+)
         (= type2 +numeric-type-bignum+))
     +numeric-type-bignum+)
    ;; Both fixnum
    (t +numeric-type-fixnum+)))

(defun emit-coerce-to-float ()
  "Generate instructions to coerce a numeric value to float.
   Expects: anyref on stack
   Returns: (ref $float) on stack"
  `(;; Dispatch on type
    (:block (:result (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
      ;; Check if already float
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
      ;; Check if fixnum
      (:block
        (:block
          (:br_on_cast 0 :anyref :i31))
        ;; Was fixnum - convert to float
        :i31.get_s
        :f64.convert_i32_s
        (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
        (:br 2))
      ;; For other types, need runtime conversion
      ;; For now, error (will be implemented in user stories)
      :unreachable)))

(defun emit-coerce-to-complex ()
  "Generate instructions to coerce a numeric value to complex.
   Expects: anyref on stack
   Returns: (ref $complex) on stack"
  `(;; Dispatch on type
    (:block (:result (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
      ;; Check if already complex
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Real value - create complex with 0 imaginary
      (:i32.const 0) :ref.i31  ; zero for imaginary part
      (:struct.new ,clysm/compiler/codegen/gc-types:+type-complex+))))

;;; ============================================================
;;; Numeric Accessors (019-numeric-accessors)
;;; ============================================================

(defun compile-numerator (args env)
  "Compile (numerator x) - ANSI CL numerator accessor.
   For fixnum/bignum: returns the value itself.
   For ratio: returns the numerator field (struct.get 0).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "numerator requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "NUM-TMP"))))
    (append
     ;; Compile and store the argument
     (compile-to-instructions (first args) env)
     (list (list :local.set temp-local))
     ;; Type dispatch: check if it's a ratio
     `((:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio - extract numerator field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-ratio+ 0)
       :else
       ;; Not ratio (integer) - return itself
       (:local.get ,temp-local)
       :end))))

(defun compile-denominator (args env)
  "Compile (denominator x) - ANSI CL denominator accessor.
   For fixnum/bignum: returns 1.
   For ratio: returns the denominator field (struct.get 1).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "denominator requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "DENOM-TMP"))))
    (append
     ;; Compile and store the argument
     (compile-to-instructions (first args) env)
     (list (list :local.set temp-local))
     ;; Type dispatch: check if it's a ratio
     `((:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio - extract denominator field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-ratio+ 1)
       :else
       ;; Not ratio (integer) - return 1
       (:i32.const 1) :ref.i31
       :end))))

(defun compile-arithmetic-op (op args env identity)
  "Compile an arithmetic operation with variadic args.
   For (+ 1 2):
     i32.const 1, ref.i31    ; create i31 for 1
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.const 2, ref.i31    ; create i31 for 2
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.add                 ; add
     ref.i31                 ; wrap result as i31"
  (cond
    ;; Zero args: return identity
    ((null args)
     (if identity
         (list (list :i32.const identity) :ref.i31)
         (error "Operator requires at least one argument")))
    ;; One arg: return value (for +) or apply unary (for -)
    ((null (cdr args))
     (compile-to-instructions (first args) env))
    ;; Two or more args: fold left
    (t
     (let ((result '()))
       ;; Compile first arg and unwrap (cast to i31 first)
       (setf result (append result (compile-to-instructions (first args) env)))
       (setf result (append result (list '(:ref.cast :i31) :i31.get_s)))
       ;; For each remaining arg: compile, cast, unwrap, apply op
       (dolist (arg (rest args))
         (setf result (append result (compile-to-instructions arg env)))
         (setf result (append result (list '(:ref.cast :i31) :i31.get_s op))))
       ;; Wrap result as i31ref
       (setf result (append result (list :ref.i31)))
       result))))

(defun compile-unary-minus (arg env)
  "Compile unary minus: (- x) => (- 0 x)."
  (append
   '((:i32.const 0))
   (compile-to-instructions arg env)
   '((:ref.cast :i31) :i31.get_s
     :i32.sub
     :ref.i31)))

(defun compile-truncate (args env)
  "Compile truncate division."
  (when (< (length args) 2)
    (error "truncate requires two arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.div_s
     :ref.i31)))

(defun compile-comparison-op (op args env)
  "Compile a comparison operation.
   Handles both fixnum (i31) and float ($float) operands.
   Uses runtime type dispatch to determine comparison method."
  (when (< (length args) 2)
    (error "Comparison requires at least two arguments"))
  ;; For now, only support two args
  ;; TODO: Chain comparisons (< a b c) => (and (< a b) (< b c))
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+)
        (arg1-local (env-add-local env (gensym "CMP-A")))
        (arg2-local (env-add-local env (gensym "CMP-B")))
        (f64-op (case op
                  (:i32.lt_s :f64.lt)
                  (:i32.gt_s :f64.gt)
                  (:i32.le_s :f64.le)
                  (:i32.ge_s :f64.ge)
                  (:i32.eq :f64.eq)
                  (t :f64.eq))))  ; default for = operator
    (append
     ;; Compile and store both arguments
     (compile-to-instructions (first args) env)
     (list (list :local.set arg1-local))
     (compile-to-instructions (second args) env)
     (list (list :local.set arg2-local))
     ;; Check if either operand is a float
     `((:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       :i32.or  ; either is float?
       (:if (:result :anyref))
       ;; Float path: extract f64 values and compare
       (:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       ;; arg1 is float
       (:local.get ,arg1-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       ;; arg1 is fixnum - convert to f64
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       ;; arg2 is float
       (:local.get ,arg2-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       ;; arg2 is fixnum - convert to f64
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       ,f64-op  ; f64 comparison
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :else
       ;; Integer path: both are fixnums
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       ,op  ; i32 comparison
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :end))))

(defun compile-not-equal (args env)
  "Compile not-equal (/=).
   Handles both fixnum (i31) and float ($float) operands."
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+)
        (arg1-local (env-add-local env (gensym "NE-A")))
        (arg2-local (env-add-local env (gensym "NE-B"))))
    (append
     ;; Compile and store both arguments
     (compile-to-instructions (first args) env)
     (list (list :local.set arg1-local))
     (compile-to-instructions (second args) env)
     (list (list :local.set arg2-local))
     ;; Check if either operand is a float
     `((:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       :i32.or  ; either is float?
       (:if (:result :anyref))
       ;; Float path: extract f64 values and compare
       (:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       (:local.get ,arg1-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       (:local.get ,arg2-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       :f64.ne  ; f64 not-equal
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :else
       ;; Integer path: both are fixnums
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :i32.ne
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :end))))

;;; ============================================================
;;; Cons/List Operations (006-cons-list-ops)
;;; ============================================================

(defun compile-cons (args env)
  "Compile (cons x y) to struct.new 2.
   Creates a cons cell with car=x and cdr=y.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "cons requires exactly 2 arguments"))
  (let ((result '()))
    ;; Compile car value
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Compile cdr value
    (setf result (append result (compile-to-instructions (second args) env)))
    ;; Create cons cell (struct.new with type index 2 = $cons)
    (setf result (append result
                         (list (list :struct.new
                                     clysm/compiler/codegen/gc-types:+type-cons+))))
    result))

(defun compile-car (args env)
  "Compile (car x) with NIL handling.
   If x is NIL, returns NIL. Otherwise returns car of cons.
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "car requires exactly 1 argument"))
  (let ((result '())
        (temp-local (env-add-local env (gensym "CAR-TMP"))))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Store in temp for reuse
    (setf result (append result (list (list :local.tee temp-local))))
    ;; Check if it's NIL (ref.null check or global comparison)
    ;; For now, use ref.is_null since NIL is represented as ref.null
    (setf result (append result '(:ref.is_null)))
    ;; If null, return NIL; otherwise get car
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)  ; Return NIL
                           :else
                           (:local.get ,temp-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 0)
                           :end)))
    result))

(defun compile-cdr (args env)
  "Compile (cdr x) with NIL handling.
   If x is NIL, returns NIL. Otherwise returns cdr of cons.
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "cdr requires exactly 1 argument"))
  (let ((result '())
        (temp-local (env-add-local env (gensym "CDR-TMP"))))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Store in temp for reuse
    (setf result (append result (list (list :local.tee temp-local))))
    ;; Check if it's NIL
    (setf result (append result '(:ref.is_null)))
    ;; If null, return NIL; otherwise get cdr
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)  ; Return NIL
                           :else
                           (:local.get ,temp-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
                           :end)))
    result))

(defun compile-list (args env)
  "Compile (list &rest args) to a proper list.
   Builds cons chain right-to-left: (list 1 2 3) = (cons 1 (cons 2 (cons 3 nil)))
   Stack: [] -> [list-ref or nil]"
  (if (null args)
      ;; Empty list = NIL
      '((:ref.null :none))
      ;; Build cons chain from right to left
      (let ((result '())
            (acc-local (env-add-local env (gensym "LIST-ACC"))))
        ;; Start with NIL
        (setf result '((:ref.null :none)))
        (setf result (append result (list (list :local.set acc-local))))
        ;; Build cons chain in reverse order
        (dolist (arg (reverse args))
          ;; Compile element
          (setf result (append result (compile-to-instructions arg env)))
          ;; Get current accumulator (cdr)
          (setf result (append result (list (list :local.get acc-local))))
          ;; Create cons
          (setf result (append result
                               (list (list :struct.new
                                           clysm/compiler/codegen/gc-types:+type-cons+))))
          ;; Store as new accumulator
          (setf result (append result (list (list :local.set acc-local)))))
        ;; Return the final list
        (setf result (append result (list (list :local.get acc-local))))
        result)))

;;; ============================================================
;;; Type Predicates (006-cons-list-ops)
;;; ============================================================

(defun compile-consp (args env)
  "Compile (consp x) - returns T if x is a cons cell, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "consp requires exactly 1 argument"))
  (let ((result '()))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Test if it's a cons (type 2)
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-cons+)))))
    ;; Convert i32 boolean to Lisp boolean
    (setf result (append result
                         `((:if (:result :anyref))
                           (:i32.const 1) :ref.i31  ; T
                           :else
                           (:ref.null :none)        ; NIL
                           :end)))
    result))

(defun compile-null (args env)
  "Compile (null x) - returns T if x is NIL, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "null requires exactly 1 argument"))
  (let ((result '()))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Check if null
    (setf result (append result '(:ref.is_null)))
    ;; Convert i32 boolean to Lisp boolean
    (setf result (append result
                         `((:if (:result :anyref))
                           (:i32.const 1) :ref.i31  ; T
                           :else
                           (:ref.null :none)        ; NIL
                           :end)))
    result))

(defun compile-not (args env)
  "Compile (not x) - returns T if x is NIL, NIL otherwise.
   Functionally identical to null per ANSI CL.
   Stack: [] -> [T or NIL]"
  ;; Delegate to compile-null since they are functionally equivalent
  (compile-null args env))

(defun compile-eq (args env)
  "Compile (eq x y) - returns T if x and y are the identical object.
   Uses Wasm ref.eq instruction for pointer equality.
   Handles null (NIL) specially since ref.cast fails on null.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "eq requires exactly 2 arguments"))
  (let ((result '())
        (local-x (env-add-local env (gensym "EQ-X")))
        (local-y (env-add-local env (gensym "EQ-Y"))))
    ;; Compile and store both arguments
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set local-x))))
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set local-y))))
    ;; Check if both are null (nil eq nil => T)
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref)))))
    ;; x is null, check if y is also null
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31  ; both null => T
                                  :else
                                  (:ref.null :none)        ; x null, y not => NIL
                                  :end)))
    (setf result (append result (list :else)))
    ;; x is not null, check if y is null
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref))
                                  (:ref.null :none)        ; x not null, y null => NIL
                                  :else)))
    ;; Neither is null, use ref.eq
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '((:ref.cast :eq))))
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '((:ref.cast :eq))))
    (setf result (append result '(:ref.eq)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31  ; eq => T
                                  :else
                                  (:ref.null :none)        ; not eq => NIL
                                  :end
                                  :end
                                  :end)))
    result))

(defun compile-eql (args env)
  "Compile (eql x y) - returns T if x and y are identical objects,
   or if they are numbers of the same type with the same value,
   or if they are characters with the same char-code.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "eql requires exactly 2 arguments"))
  (let ((result '())
        (local-x (env-add-local env (gensym "EQL-X")))
        (local-y (env-add-local env (gensym "EQL-Y"))))
    ;; Compile and store both arguments
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set local-x))))
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set local-y))))
    ;; Check if x is null
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref)))))
    ;; x is null - check if y is also null
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31  ; both null => T
                                  :else
                                  (:ref.null :none)        ; x null, y not => NIL
                                  :end)))
    (setf result (append result (list :else)))
    ;; x is not null - check if y is null
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result `((:if (:result :anyref))
                                  (:ref.null :none)        ; x not null, y null => NIL
                                  :else)))
    ;; Neither is null - check type hierarchy with proper if/else chain
    ;; Check if x is i31ref (fixnum, char, T)
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '((:ref.test :i31))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; x is i31ref - check if y is also i31ref
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '((:ref.test :i31))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; Both are i31ref - compare with ref.eq
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '((:ref.cast :i31))))
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '((:ref.cast :i31))))
    (setf result (append result '(:ref.eq)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31
                                  :else
                                  (:ref.null :none)
                                  :end)))
    (setf result (append result (list :else)))
    ;; x is i31ref but y is not - NIL (different types)
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '(:end)))  ; end if y is i31
    (setf result (append result (list :else)))
    ;; x is not i31ref - check if x is float
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-float+)))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; x is float - check if y is also float
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-float+)))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; Both are floats - compare values with f64.eq
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-float+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-float+ 0))))
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-float+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-float+ 0))))
    (setf result (append result '(:f64.eq)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31
                                  :else
                                  (:ref.null :none)
                                  :end)))
    (setf result (append result (list :else)))
    ;; x is float but y is not - NIL (different types)
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '(:end)))  ; end if y is float
    (setf result (append result (list :else)))
    ;; x is not float - check if x is ratio
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; x is ratio - check if y is also ratio
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result `((:if (:result :anyref)))))
    ;; Both are ratios - compare numerators and denominators
    ;; For ratios, numerator is field 0, denominator is field 1 (as anyref: fixnum or bignum)
    ;; Compare numerators using ref.eq (works for i31ref fixnums)
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-ratio+ 0))))
    (setf result (append result '((:ref.cast :eq))))  ; cast to eqref for ref.eq
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-ratio+ 0))))
    (setf result (append result '((:ref.cast :eq))))  ; cast to eqref for ref.eq
    (setf result (append result '(:ref.eq)))
    (setf result (append result `((:if (:result :anyref)))))
    ;; Numerators equal - compare denominators
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-ratio+ 1))))
    (setf result (append result '((:ref.cast :eq))))  ; cast to eqref for ref.eq
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result
                         (list (list :ref.cast
                                     (list :ref clysm/compiler/codegen/gc-types:+type-ratio+)))))
    (setf result (append result
                         (list (list :struct.get
                                     clysm/compiler/codegen/gc-types:+type-ratio+ 1))))
    (setf result (append result '((:ref.cast :eq))))  ; cast to eqref for ref.eq
    (setf result (append result '(:ref.eq)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31  ; both equal => T
                                  :else
                                  (:ref.null :none)        ; denominators differ
                                  :end)))
    (setf result (append result (list :else)))
    ;; Numerators differ
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '(:end)))  ; end if numerators equal
    (setf result (append result (list :else)))
    ;; x is ratio but y is not - NIL
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '(:end)))  ; end if y is ratio
    (setf result (append result (list :else)))
    ;; Default: not i31, not float, not ratio - use ref.eq for other types (symbols, cons, etc.)
    (setf result (append result (list (list :local.get local-x))))
    (setf result (append result '((:ref.cast :eq))))
    (setf result (append result (list (list :local.get local-y))))
    (setf result (append result '((:ref.cast :eq))))
    (setf result (append result '(:ref.eq)))
    (setf result (append result `((:if (:result :anyref))
                                  (:i32.const 1) :ref.i31
                                  :else
                                  (:ref.null :none)
                                  :end)))
    ;; Close all the nested if blocks:
    ;; end if x is ratio, end if x is float, end if x is i31, end else y not null, end if x not null
    (setf result (append result '(:end :end :end :end :end)))
    result))

(defun compile-equal (args env)
  "Compile (equal x y) - returns T if x and y are structurally similar.
   Uses a worklist-based approach to handle recursive cons comparison.
   ANSI CL: equal descends into cons cells, compares strings with string=,
   uses eql for numbers/characters/symbols.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "equal requires exactly 2 arguments"))
  (let ((local-x (env-add-local env (gensym "EQ-X")))
        (local-y (env-add-local env (gensym "EQ-Y")))
        (worklist-local (env-add-local env (gensym "EQ-WL")))
        (pair-local (env-add-local env (gensym "EQ-PAIR")))
        (result-local (env-add-local env (gensym "EQ-RESULT")))
        (len1-local (env-add-local env (gensym "EQ-LEN1") :i32))
        (len2-local (env-add-local env (gensym "EQ-LEN2") :i32))
        (idx-local (env-add-local env (gensym "EQ-IDX") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    `(;; Compile both arguments
      ,@(compile-to-instructions (first args) env)
      (:local.set ,local-x)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,local-y)
      ;; Initialize worklist with single pair (cons x y)
      (:local.get ,local-x)
      (:local.get ,local-y)
      (:struct.new ,cons-type)
      (:ref.null :none)
      (:struct.new ,cons-type)  ; worklist = (cons (cons x y) nil)
      (:local.set ,worklist-local)
      ;; Initialize result to T (will set to NIL if mismatch found)
      (:i32.const 1) :ref.i31
      (:local.set ,result-local)
      ;; Main comparison loop
      (:block $equal_done)
      (:loop $equal_loop)
      ;; Check if worklist is empty
      (:local.get ,worklist-local)
      :ref.is_null
      (:br_if $equal_done)  ; worklist empty, result is in result-local
      ;; Pop pair from worklist
      (:local.get ,worklist-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)  ; car = current pair
      (:local.set ,pair-local)
      (:local.get ,worklist-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)  ; cdr = rest of worklist
      (:local.set ,worklist-local)
      ;; Extract x and y from pair
      (:local.get ,pair-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)  ; car = x
      (:local.set ,local-x)
      (:local.get ,pair-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)  ; cdr = y
      (:local.set ,local-y)
      ;; Case 1: Both null?
      (:local.get ,local-x)
      :ref.is_null
      (:if nil)
      (:local.get ,local-y)
      :ref.is_null
      (:if nil)
      ;; Both null - equal, continue to next pair
      (:br $equal_loop)
      :else
      ;; x null, y not null - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x not null
      (:local.get ,local-y)
      :ref.is_null
      (:if nil)
      ;; x not null, y null - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      ;; Neither is null - check types
      ;; Case 2: Both i31ref?
      (:local.get ,local-x)
      (:ref.test :i31)
      (:if nil)
      (:local.get ,local-y)
      (:ref.test :i31)
      (:if nil)
      ;; Both i31ref - compare directly (fixnums, chars, T)
      (:local.get ,local-x)
      (:ref.cast :i31)
      (:local.get ,local-y)
      (:ref.cast :i31)
      :ref.eq
      (:if nil)
      (:br $equal_loop)  ; equal, continue
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x is i31 but y is not - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x is not i31
      ;; Case 3: Both strings?
      (:local.get ,local-x)
      (:ref.test ,(list :ref string-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref string-type))
      (:if nil)
      ;; Both strings - compare byte-by-byte
      (:local.get ,local-x)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; Check lengths
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      ;; Different lengths - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      ;; Same length - compare bytes
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $str_cmp_done)
      (:loop $str_cmp_loop)
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:br_if $str_cmp_done)
      ;; Compare bytes at idx
      (:local.get ,local-x)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      :i32.ne
      (:if nil)
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $str_cmp_loop)
      :end  ; loop
      :end  ; block
      ;; Strings equal, continue to next pair
      (:br $equal_loop)
      :else
      ;; x is string but y is not - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x is not string
      ;; Case 4: Both floats?
      (:local.get ,local-x)
      (:ref.test ,(list :ref float-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref float-type))
      (:if nil)
      ;; Both floats - compare values with f64.eq (eql semantics)
      (:local.get ,local-x)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      :f64.eq
      (:if nil)
      (:br $equal_loop)  ; equal, continue
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x is float but y is not - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; x is not float
      ;; Case 5: Both cons?
      (:local.get ,local-x)
      (:ref.test ,(list :ref cons-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref cons-type))
      (:if nil)
      ;; Both cons - push (car x, car y) and (cdr x, cdr y) to worklist
      ;; Create pair for cars
      (:local.get ,local-x)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)  ; car x
      (:local.get ,local-y)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)  ; car y
      (:struct.new ,cons-type)    ; (cons car-x car-y)
      ;; Push to worklist
      (:local.get ,worklist-local)
      (:struct.new ,cons-type)    ; (cons pair worklist)
      (:local.set ,worklist-local)
      ;; Create pair for cdrs
      (:local.get ,local-x)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)  ; cdr x
      (:local.get ,local-y)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)  ; cdr y
      (:struct.new ,cons-type)    ; (cons cdr-x cdr-y)
      ;; Push to worklist
      (:local.get ,worklist-local)
      (:struct.new ,cons-type)    ; (cons pair worklist)
      (:local.set ,worklist-local)
      ;; Continue to next iteration
      (:br $equal_loop)
      :else
      ;; x is cons but y is not - not equal
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :else
      ;; Neither is cons - use ref.eq for other types (symbols, etc.)
      (:local.get ,local-x)
      (:ref.cast :eq)
      (:local.get ,local-y)
      (:ref.cast :eq)
      :ref.eq
      (:if nil)
      (:br $equal_loop)  ; equal, continue
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equal_done)
      :end
      :end  ; if cons
      :end  ; if float
      :end  ; if string
      :end  ; if i31
      :end  ; if x not null
      :end  ; loop
      :end  ; block
      ;; Return result
      (:local.get ,result-local))))

(defun compile-equalp (args env)
  "Compile (equalp x y) - case-insensitive structural equality.
   Like equal but:
   - Case-insensitive for strings (uses char-equal semantics)
   - Case-insensitive for characters
   - Uses = for numbers (type-coercing: 3 equalp 3.0 is T)
   Uses worklist-based approach for recursive cons comparison.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "equalp requires exactly 2 arguments"))
  (let ((local-x (env-add-local env (gensym "EQP-X")))
        (local-y (env-add-local env (gensym "EQP-Y")))
        (worklist-local (env-add-local env (gensym "EQP-WL")))
        (pair-local (env-add-local env (gensym "EQP-PAIR")))
        (result-local (env-add-local env (gensym "EQP-RESULT")))
        (len1-local (env-add-local env (gensym "EQP-LEN1") :i32))
        (len2-local (env-add-local env (gensym "EQP-LEN2") :i32))
        (idx-local (env-add-local env (gensym "EQP-IDX") :i32))
        (byte1-local (env-add-local env (gensym "EQP-B1") :i32))
        (byte2-local (env-add-local env (gensym "EQP-B2") :i32))
        (val1-local (env-add-local env (gensym "EQP-V1") :i32))
        (val2-local (env-add-local env (gensym "EQP-V2") :i32))
        (f1-local (env-add-local env (gensym "EQP-F1") :f64))
        (f2-local (env-add-local env (gensym "EQP-F2") :f64))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    `(;; Compile both arguments
      ,@(compile-to-instructions (first args) env)
      (:local.set ,local-x)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,local-y)
      ;; Initialize worklist with single pair (cons x y)
      (:local.get ,local-x)
      (:local.get ,local-y)
      (:struct.new ,cons-type)
      (:ref.null :none)
      (:struct.new ,cons-type)
      (:local.set ,worklist-local)
      ;; Initialize result to T
      (:i32.const 1) :ref.i31
      (:local.set ,result-local)
      ;; Main comparison loop
      (:block $equalp_done)
      (:loop $equalp_loop)
      ;; Check if worklist is empty
      (:local.get ,worklist-local)
      :ref.is_null
      (:br_if $equalp_done)
      ;; Pop pair from worklist
      (:local.get ,worklist-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:local.set ,pair-local)
      (:local.get ,worklist-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,worklist-local)
      ;; Extract x and y from pair
      (:local.get ,pair-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:local.set ,local-x)
      (:local.get ,pair-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,local-y)
      ;; Case 1: Both null?
      (:local.get ,local-x)
      :ref.is_null
      (:if nil)
      (:local.get ,local-y)
      :ref.is_null
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      (:local.get ,local-y)
      :ref.is_null
      (:if nil)
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      ;; Case 2: Both i31ref? (fixnum or char)
      (:local.get ,local-x)
      (:ref.test :i31)
      (:if nil)
      (:local.get ,local-y)
      (:ref.test :i31)
      (:if nil)
      ;; Both i31ref - convert to lowercase and compare (handles chars case-insensitively)
      (:local.get ,local-x)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,val1-local)
      (:local.get ,local-y)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,val2-local)
      ;; Convert val1 to lowercase if uppercase letter
      (:local.get ,val1-local)
      (:i32.const 65)
      :i32.ge_s
      (:local.get ,val1-local)
      (:i32.const 90)
      :i32.le_s
      :i32.and
      (:if nil)
      (:local.get ,val1-local)
      (:i32.const 32)
      :i32.add
      (:local.set ,val1-local)
      :end
      ;; Convert val2 to lowercase if uppercase letter
      (:local.get ,val2-local)
      (:i32.const 65)
      :i32.ge_s
      (:local.get ,val2-local)
      (:i32.const 90)
      :i32.le_s
      :i32.and
      (:if nil)
      (:local.get ,val2-local)
      (:i32.const 32)
      :i32.add
      (:local.set ,val2-local)
      :end
      ;; Compare
      (:local.get ,val1-local)
      (:local.get ,val2-local)
      :i32.eq
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      ;; x is i31 but y is not - check if y is float for numeric coercion
      (:local.get ,local-y)
      (:ref.test ,(list :ref float-type))
      (:if nil)
      ;; y is float, x is fixnum - compare as floats
      (:local.get ,local-x)
      (:ref.cast :i31)
      :i31.get_s
      :f64.convert_i32_s
      (:local.get ,local-y)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      :f64.eq
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      ;; y is not float - not equalp
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :end
      :else
      ;; x is not i31
      ;; Case 3: Both strings?
      (:local.get ,local-x)
      (:ref.test ,(list :ref string-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref string-type))
      (:if nil)
      ;; Both strings - case-insensitive comparison
      (:local.get ,local-x)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $str_cmp_done)
      (:loop $str_cmp_loop)
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:br_if $str_cmp_done)
      ;; Get bytes and convert to uppercase for comparison
      (:local.get ,local-x)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Convert byte1 to uppercase if lowercase
      (:local.get ,byte1-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte1-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      (:local.get ,byte1-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte1-local)
      :end
      ;; Convert byte2 to uppercase if lowercase
      (:local.get ,byte2-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte2-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      (:local.get ,byte2-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte2-local)
      :end
      ;; Compare
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $str_cmp_loop)
      :end
      :end
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      ;; Case 4: Both floats?
      (:local.get ,local-x)
      (:ref.test ,(list :ref float-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref float-type))
      (:if nil)
      ;; Both floats - compare values
      (:local.get ,local-x)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      :f64.eq
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      ;; x is float but y is not - check if y is i31 for numeric coercion
      (:local.get ,local-y)
      (:ref.test :i31)
      (:if nil)
      ;; y is fixnum, x is float - compare as floats
      (:local.get ,local-x)
      (:ref.cast ,(list :ref float-type))
      (:struct.get ,float-type 0)
      (:local.get ,local-y)
      (:ref.cast :i31)
      :i31.get_s
      :f64.convert_i32_s
      :f64.eq
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :end
      :else
      ;; Case 5: Both cons?
      (:local.get ,local-x)
      (:ref.test ,(list :ref cons-type))
      (:if nil)
      (:local.get ,local-y)
      (:ref.test ,(list :ref cons-type))
      (:if nil)
      ;; Both cons - push pairs for recursive comparison
      (:local.get ,local-x)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:struct.new ,cons-type)
      (:local.get ,worklist-local)
      (:struct.new ,cons-type)
      (:local.set ,worklist-local)
      (:local.get ,local-x)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.get ,local-y)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:struct.new ,cons-type)
      (:local.get ,worklist-local)
      (:struct.new ,cons-type)
      (:local.set ,worklist-local)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :else
      ;; Default: use ref.eq for other types (symbols, etc.)
      (:local.get ,local-x)
      (:ref.cast :eq)
      (:local.get ,local-y)
      (:ref.cast :eq)
      :ref.eq
      (:if nil)
      (:br $equalp_loop)
      :else
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $equalp_done)
      :end
      :end
      :end
      :end
      :end
      :end
      :end
      :end
      (:local.get ,result-local))))

(defun compile-atom (args env)
  "Compile (atom x) - returns T if x is not a cons cell, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "atom requires exactly 1 argument"))
  (let ((result '()))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Test if it's a cons (type 2)
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-cons+)))))
    ;; atom is NOT consp, so invert the result
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)        ; cons -> not atom -> NIL
                           :else
                           (:i32.const 1) :ref.i31  ; not cons -> atom -> T
                           :end)))
    result))

(defun compile-listp (args env)
  "Compile (listp x) - returns T if x is a cons cell or NIL.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "listp requires exactly 1 argument"))
  (let ((result '())
        (temp-local (env-add-local env (gensym "LISTP-TMP"))))
    ;; Compile the argument
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Store for reuse
    (setf result (append result (list (list :local.tee temp-local))))
    ;; First check: is it a cons?
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref clysm/compiler/codegen/gc-types:+type-cons+)))))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:i32.const 1) :ref.i31  ; cons -> T
                           :else
                           ;; Not a cons, check if NIL
                           (:local.get ,temp-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:i32.const 1) :ref.i31  ; nil -> T
                           :else
                           (:ref.null :none)        ; neither -> NIL
                           :end
                           :end)))
    result))

;;; ============================================================
;;; ANSI CL Type Predicates (023-type-predicates)
;;; ============================================================

(defun compile-integerp (args env)
  "Compile (integerp x) - returns T if x is an integer (fixnum or bignum), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "integerp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "INTEGERP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Is fixnum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       ;; Is bignum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Neither -> NIL
       (:ref.null :none)
       :end
       :end))))

(defun compile-floatp (args env)
  "Compile (floatp x) - returns T if x is a float, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "floatp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-rationalp (args env)
  "Compile (rationalp x) - returns T if x is a rational (fixnum, bignum, or ratio), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "rationalp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "RATIONALP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Is fixnum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       ;; Is bignum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio -> T
       (:i32.const 1) :ref.i31
       :else
       ;; None of the above -> NIL
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-complexp (args env)
  "Compile (complexp x) - returns T if x is a complex number, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "complexp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-numberp (args env)
  "Compile (numberp x) - returns T if x is any numeric type, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "numberp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "NUMBERP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if complex
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end :end :end :end :end))))

(defun compile-symbolp (args env)
  "Compile (symbolp x) - returns T if x is a symbol (including NIL), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "symbolp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SYMBOLP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; First check: is it a symbol struct?
     `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-symbol+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Not a symbol struct, check if NIL (ref.null)
       (:local.get ,temp-local)
       :ref.is_null
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; NIL is a symbol
       :else
       (:ref.null :none)
       :end
       :end))))

(defun compile-functionp (args env)
  "Compile (functionp x) - returns T if x is a function (closure), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "functionp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-closure+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

;;; ============================================================
;;; ANSI CL Numeric Predicates (023-type-predicates)
;;; ============================================================

(defun compile-zerop (args env)
  "Compile (zerop x) - returns T if x equals zero, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "zerop requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "ZEROP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare to 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       :i32.eqz
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare to 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-plusp (args env)
  "Compile (plusp x) - returns T if x is positive, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "plusp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "PLUSP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare > 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.gt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare > 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.gt
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-minusp (args env)
  "Compile (minusp x) - returns T if x is negative, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "minusp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "MINUSP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare < 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare < 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.lt
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-oddp (args env)
  "Compile (oddp x) - returns T if x is an odd integer, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "oddp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "ODDP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract, mod 2, check != 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 1)
       :i32.and  ; n & 1 == 1 for odd numbers
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Not a fixnum: NIL (bignum support would require runtime)
       (:ref.null :none)
       :end))))

(defun compile-evenp (args env)
  "Compile (evenp x) - returns T if x is an even integer, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "evenp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "EVENP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract, mod 2, check == 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 1)
       :i32.and  ; n & 1 == 0 for even numbers
       :i32.eqz
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Not a fixnum: NIL (bignum support would require runtime)
       (:ref.null :none)
       :end))))

;;; ============================================================
;;; ANSI CL Signum Function (023-type-predicates)
;;; ============================================================

(defun compile-signum (args env)
  "Compile (signum x) - returns sign of x with type preserved.
   For integers: -1, 0, or 1
   For floats: -1.0, 0.0, or 1.0
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "signum requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SIGNUM-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum path - extract value and compare directly
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       ;; Negative
       (:i32.const -1) :ref.i31
       :else
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.gt_s
       (:if (:result :anyref))
       ;; Positive
       (:i32.const 1) :ref.i31
       :else
       ;; Zero
       (:i32.const 0) :ref.i31
       :end
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float path
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.lt
       (:if (:result :anyref))
       ;; Negative float
       (:f64.const -1.0)
       (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
       :else
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.gt
       (:if (:result :anyref))
       ;; Positive float
       (:f64.const 1.0)
       (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
       :else
       ;; Zero float
       (:f64.const 0.0)
       (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
       :end
       :end
       :else
       ;; Other types: return 0 (could signal error)
       (:i32.const 0) :ref.i31
       :end :end))))

;;; ============================================================
;;; Destructive Modification (006-cons-list-ops)
;;; ============================================================

(defun compile-rplaca (args env)
  "Compile (rplaca cons new-value) - destructively modify car.
   Returns the modified cons cell.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "rplaca requires exactly 2 arguments"))
  (let* ((cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (result '())
         ;; Local must be typed to hold the cast result (ref null $cons)
         (cons-local (env-add-local env (gensym "RPLACA-CONS")
                                    (list :ref-null cons-type))))
    ;; Compile the cons argument and save it
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result
                         (list (list :ref.cast (list :ref cons-type)))))
    (setf result (append result (list (list :local.tee cons-local))))
    ;; Compile the new value
    (setf result (append result (compile-to-instructions (second args) env)))
    ;; Set the car field (field 0)
    (setf result (append result
                         (list (list :struct.set cons-type 0))))
    ;; Return the cons cell
    (setf result (append result (list (list :local.get cons-local))))
    result))

(defun compile-rplacd (args env)
  "Compile (rplacd cons new-value) - destructively modify cdr.
   Returns the modified cons cell.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "rplacd requires exactly 2 arguments"))
  (let* ((cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (result '())
         ;; Local must be typed to hold the cast result (ref null $cons)
         (cons-local (env-add-local env (gensym "RPLACD-CONS")
                                    (list :ref-null cons-type))))
    ;; Compile the cons argument and save it
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result
                         (list (list :ref.cast (list :ref cons-type)))))
    (setf result (append result (list (list :local.tee cons-local))))
    ;; Compile the new value
    (setf result (append result (compile-to-instructions (second args) env)))
    ;; Set the cdr field (field 1)
    (setf result (append result
                         (list (list :struct.set cons-type 1))))
    ;; Return the cons cell
    (setf result (append result (list (list :local.get cons-local))))
    result))

;;; ============================================================
;;; List Accessors (006-cons-list-ops)
;;; ============================================================

(defun compile-nth-accessor (n args env)
  "Compile position accessor (second, third, ..., tenth).
   Equivalent to (car (nthcdr n list)).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "Position accessor requires exactly 1 argument"))
  (let ((result '())
        (list-local (env-add-local env (gensym "NTH-LIST"))))
    ;; Compile the list argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Traverse n times via cdr
    (dotimes (i n)
      (setf result (append result (list (list :local.get list-local))))
      ;; Check for NIL before cdr
      (setf result (append result '(:ref.is_null)))
      (setf result (append result
                           `((:if (:result :anyref))
                             (:ref.null :none)  ; Return NIL if list ended
                             :else
                             (:local.get ,list-local)
                             (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                             (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
                             :end)))
      (setf result (append result (list (list :local.set list-local)))))
    ;; Now get car of result
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 0)
                           :end)))
    result))

(defun compile-nth (args env)
  "Compile (nth n list) - 0-indexed access.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "nth requires exactly 2 arguments"))
  ;; For compile-time constant index, unroll
  ;; For runtime index, use a loop
  (let* ((index-form (first args))
         (list-form (second args)))
    (if (and (typep index-form 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type index-form) :fixnum))
        ;; Constant index - unroll
        (let ((n (clysm/compiler/ast:ast-literal-value index-form)))
          (if (< n 0)
              ;; Negative index returns NIL
              '((:ref.null :none))
              (compile-nth-accessor n (list list-form) env)))
        ;; Runtime index - generate loop
        (compile-nth-runtime args env))))

(defun compile-nth-runtime (args env)
  "Compile (nth n list) with runtime index using a loop.
   Stack: [] -> [anyref]"
  (let ((result '())
        (index-local (env-add-local env (gensym "NTH-IDX") :i32))
        (list-local (env-add-local env (gensym "NTH-LIST"))))
    ;; Compile index and convert to i32
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result '((:ref.cast :i31) :i31.get_s)))
    (setf result (append result (list (list :local.set index-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check for negative index
    (setf result (append result (list (list :local.get index-local))))
    (setf result (append result '((:i32.const 0) :i32.lt_s)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)  ; Negative index -> NIL
                           :else
                           ;; Loop to traverse
                           (:block $nth_done (:result :anyref))
                           (:loop $nth_loop (:result :anyref))
                           ;; Check if index is 0
                           (:local.get ,index-local)
                           :i32.eqz
                           (:if (:result :anyref))
                           ;; Index is 0, return car
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 0)
                           :end
                           (:br $nth_done)
                           :else
                           ;; Decrement index
                           (:local.get ,index-local)
                           (:i32.const 1)
                           :i32.sub
                           (:local.set ,index-local)
                           ;; Check if list is null
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $nth_done)
                           :else
                           ;; Get cdr
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
                           (:local.set ,list-local)
                           ;; Continue loop (need to push a dummy value for block type)
                           (:ref.null :none)
                           (:br $nth_loop)
                           :end
                           :end
                           :end  ; loop
                           :end  ; block
                           :end)))  ; if negative
    result))

(defun compile-nthcdr (args env)
  "Compile (nthcdr n list) - return list after n cdrs.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "nthcdr requires exactly 2 arguments"))
  (let* ((index-form (first args))
         (list-form (second args)))
    (if (and (typep index-form 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type index-form) :fixnum))
        ;; Constant index - unroll
        (let ((n (clysm/compiler/ast:ast-literal-value index-form))
              (result '())
              (list-local (env-add-local env (gensym "NTHCDR-LIST"))))
          (if (< n 0)
              ;; Negative index returns the list unchanged
              (compile-to-instructions list-form env)
              (progn
                ;; Compile the list
                (setf result (append result (compile-to-instructions list-form env)))
                (setf result (append result (list (list :local.set list-local))))
                ;; Traverse n times via cdr
                (dotimes (i n)
                  (setf result (append result (list (list :local.get list-local))))
                  (setf result (append result '(:ref.is_null)))
                  (setf result (append result
                                       `((:if (:result :anyref))
                                         (:ref.null :none)
                                         :else
                                         (:local.get ,list-local)
                                         (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                                         (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
                                         :end)))
                  (setf result (append result (list (list :local.set list-local)))))
                ;; Return the result
                (setf result (append result (list (list :local.get list-local))))
                result)))
        ;; Runtime index - generate loop (simplified for now)
        (compile-nthcdr-runtime args env))))

(defun compile-nthcdr-runtime (args env)
  "Compile (nthcdr n list) with runtime index.
   Stack: [] -> [anyref]"
  (let ((result '())
        (index-local (env-add-local env (gensym "NTHCDR-IDX") :i32))
        (list-local (env-add-local env (gensym "NTHCDR-LIST"))))
    ;; Compile index and convert to i32
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result '((:ref.cast :i31) :i31.get_s)))
    (setf result (append result (list (list :local.set index-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check for negative/zero index
    (setf result (append result (list (list :local.get index-local))))
    (setf result (append result '((:i32.const 0) :i32.le_s)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:local.get ,list-local)  ; <= 0 -> return list
                           :else
                           ;; Loop to traverse
                           (:block $nthcdr_done (:result :anyref))
                           (:loop $nthcdr_loop (:result :anyref))
                           ;; Check if list is null
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $nthcdr_done)
                           :else
                           ;; Get cdr
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
                           (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
                           (:local.set ,list-local)
                           ;; Decrement index
                           (:local.get ,index-local)
                           (:i32.const 1)
                           :i32.sub
                           (:local.set ,index-local)
                           ;; Check if index is 0
                           (:local.get ,index-local)
                           :i32.eqz
                           (:if (:result :anyref))
                           (:local.get ,list-local)
                           (:br $nthcdr_done)
                           :else
                           (:ref.null :none)
                           (:br $nthcdr_loop)
                           :end
                           :end
                           :end  ; loop
                           :end  ; block
                           :end)))  ; if negative
    result))

(defun compile-regular-call (function args env)
  "Compile a regular function call (T061).
   Uses :return_call for tail calls (TCO), :call otherwise."
  (let ((func-idx (env-lookup-function env function)))
    (unless func-idx
      (error "Undefined function: ~A" function))
    ;; Compile arguments (not in tail position)
    (let ((result '())
          (arg-env (env-with-non-tail env)))
      (dolist (arg args)
        (setf result (append result (compile-to-instructions arg arg-env))))
      ;; Call - use return_call for tail calls
      (if (cenv-in-tail-position env)
          (append result (list (list :return_call func-idx)))
          (append result (list (list :call func-idx)))))))

;;; ============================================================
;;; Funcall Compilation (T085-T087)
;;; ============================================================

(defun compile-funcall (args env)
  "Compile (funcall fn arg1 arg2 ...).
   The first arg is a closure, remaining args are passed to it.
   Uses :return_call_ref for tail calls (TCO), :call_ref otherwise."
  (when (null args)
    (error "funcall requires at least one argument"))
  (let* ((closure-expr (first args))
         (call-args (rest args))
         (arity (length call-args))
         (result '())
         (arg-env (env-with-non-tail env)))
    ;; Compile the closure expression (not in tail position)
    (setf result (append result (compile-to-instructions closure-expr arg-env)))
    ;; Duplicate closure ref for call_ref (closure goes as first param too)
    ;; We need: closure arg1 arg2 ... closure-code
    ;; Stack after closure compilation: [..., closure]
    ;; We need to:
    ;; 1. Save closure to a local
    ;; 2. Push closure (for first param)
    ;; 3. Push all args
    ;; 4. Get closure, extract code field, call_ref/return_call_ref
    (let ((closure-local (cenv-local-counter env)))
      (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
      ;; Save closure to local
      (setf result (append result (list (list :local.set closure-local))))
      ;; Push closure as first argument (self reference)
      (setf result (append result (list (list :local.get closure-local))))
      ;; Push all call arguments (not in tail position)
      (dolist (arg call-args)
        (setf result (append result (compile-to-instructions arg arg-env))))
      ;; Get closure and extract code_N field (field 3 = code_N for variadic)
      ;; For specific arities, we could use code_0/1/2 but code_N always works
      (setf result (append result (list (list :local.get closure-local))))
      ;; Cast to closure type
      (setf result (append result
                           (list (list :ref.cast
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      ;; Get the appropriate code field based on arity
      (let ((code-field (case arity
                          (0 0)  ; code_0
                          (1 1)  ; code_1
                          (2 2)  ; code_2
                          (t 3)))) ; code_N
        (setf result (append result
                             (list (list :struct.get
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         code-field)))))
      ;; Cast the funcref to the specific function type before call_ref
      ;; call_ref requires a typed function reference
      (let ((func-type (case arity
                         (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                         (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                         (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                         (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                         (t clysm/compiler/codegen/gc-types:+type-func-n+))))
        ;; Cast funcref to the specific function type
        (setf result (append result (list (list :ref.cast func-type))))
        ;; Call through the typed function reference
        ;; Use return_call_ref for tail calls
        (if (cenv-in-tail-position env)
            (setf result (append result (list (list :return_call_ref func-type))))
            (setf result (append result (list (list :call_ref func-type)))))))
    result))

;;; ============================================================
;;; Conditional Compilation (T054-T055)
;;; ============================================================

(defun compile-if (ast env)
  "Compile an if expression.
   Note: if creates a Wasm block, so we increment block-depth for nested go/return-from.
   Propagates tail position to both then and else branches for TCO."
  (let ((test (clysm/compiler/ast:ast-if-test ast))
        (then-branch (clysm/compiler/ast:ast-if-then ast))
        (else-branch (clysm/compiler/ast:ast-if-else ast))
        ;; Create new env with incremented block depth for if block
        ;; Test expression is NOT in tail position
        (test-env (env-with-non-tail env))
        ;; Both branches inherit tail position from parent
        (if-env (copy-compilation-env env)))
    (incf (cenv-block-depth if-env))
    (append
     ;; Compile test (NOT in tail position)
     (compile-to-instructions test test-env)
     ;; Check if not NIL
     (compile-nil-check)
     ;; If-then-else (branches inherit tail position)
     '((:if (:result :anyref)))
     (compile-to-instructions then-branch if-env)
     '(:else)
     (compile-to-instructions else-branch if-env)
     '(:end))))

(defun compile-nil-check ()
  "Generate code to check if TOS is not NIL.
   Returns i32: 1 if not-nil, 0 if nil.
   NIL is represented as null reference, so we use ref.is_null."
  '(:ref.is_null     ; Is this null?
    :i32.eqz))       ; Invert (not nil => 1)

;;; ============================================================
;;; Binding Compilation (T056-T058)
;;; ============================================================

;;; ------------------------------------------------------------
;;; Special Binding Detection and Helpers (T030)
;;; ------------------------------------------------------------

(defun binding-is-special-p (name)
  "Check if a binding name refers to a special variable (T030).
   A binding is special if the variable has been declared with defvar/defparameter."
  (clysm/compiler/env:special-variable-p name))

(defun partition-bindings (bindings)
  "Partition bindings into (special-bindings . lexical-bindings) (T030).
   Returns two lists: special bindings and lexical bindings."
  (let ((special '())
        (lexical '()))
    (dolist (binding bindings)
      (let ((name (car binding)))
        (if (binding-is-special-p name)
            (push binding special)
            (push binding lexical))))
    (cons (nreverse special) (nreverse lexical))))

;;; ------------------------------------------------------------
;;; Special Binding Save/Restore Code Generation (T031-T032)
;;; ------------------------------------------------------------

(defun emit-save-special-binding (name env)
  "Emit code to save the current value of a special variable (T031).
   Returns (instructions . save-local-idx).
   The current value is saved to a local for later restoration."
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (save-local (env-add-local env (gensym "save"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Generate: global.get sym, struct.get value, local.set save
    (cons `((:global.get ,global-idx)
            (:struct.get ,symbol-type 1)  ; Get value field
            (:local.set ,save-local))
          save-local)))

(defun emit-set-special-binding (name value-instrs env)
  "Emit code to set a special variable's value (T031).
   VALUE-INSTRS are the instructions that produce the new value."
  (declare (ignore env))
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized" name))
    ;; Generate: global.get sym, <value-instrs>, struct.set value
    `((:global.get ,global-idx)
      ,@value-instrs
      (:struct.set ,symbol-type 1))))

(defun emit-restore-special-binding (name save-local)
  "Emit code to restore a special variable from a saved local (T032).
   SAVE-LOCAL is the local index where the old value was saved."
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Generate: global.get sym, local.get save, struct.set value
    `((:global.get ,global-idx)
      (:local.get ,save-local)
      (:struct.set ,symbol-type 1))))

(defun compile-let (ast env)
  "Compile a let or let* expression (T033).
   Handles both lexical and special (dynamic) bindings:
   - Lexical bindings use Wasm locals
   - Special bindings modify the global symbol's value with save/restore
   Tail position is inherited by the last body form ONLY if there are no
   special bindings (special bindings require save/restore which breaks TCO)."
  (let ((bindings (clysm/compiler/ast:ast-let-bindings ast))
        (body (clysm/compiler/ast:ast-let-body ast))
        (sequential-p (clysm/compiler/ast:ast-let-sequential-p ast))
        (new-env (extend-compilation-env env))
        (result '())
        (special-save-locals '())  ; ((name . save-local-idx) ...)
        ;; Binding value expressions are NOT in tail position
        (binding-env (env-with-non-tail env)))
    ;; Process bindings based on let vs let*
    (if sequential-p
        ;; let*: each binding sees previous bindings
        (dolist (binding bindings)
          (let* ((name (car binding))
                 (value-form (cdr binding)))
            (if (binding-is-special-p name)
                ;; Special binding: save, compute, set global
                (let* ((save-result (emit-save-special-binding name new-env))
                       (save-instrs (car save-result))
                       (save-local (cdr save-result))
                       (value-instrs (compile-to-instructions value-form (env-with-non-tail new-env)))
                       (set-instrs (emit-set-special-binding name value-instrs new-env)))
                  (push (cons name save-local) special-save-locals)
                  (setf result (append result save-instrs set-instrs)))
                ;; Lexical binding: use local
                (let ((idx (env-add-local new-env name)))
                  (setf result (append result
                                       (compile-to-instructions value-form (env-with-non-tail new-env))
                                       (list (list :local.set idx))))))))
        ;; let: all values computed before any binding visible
        ;; For special bindings, we still need to save first, then set
        (let ((compiled-values '())
              (binding-actions '()))
          ;; First pass: save special vars and compile all values
          (dolist (binding bindings)
            (let* ((name (car binding))
                   (value-form (cdr binding)))
              (if (binding-is-special-p name)
                  ;; Special: save current value first
                  (let* ((save-result (emit-save-special-binding name new-env))
                         (save-instrs (car save-result))
                         (save-local (cdr save-result)))
                    (push (cons name save-local) special-save-locals)
                    (setf result (append result save-instrs))
                    ;; Compile value (uses original env for parallel semantics, NOT in tail)
                    (push (cons :special (cons name (compile-to-instructions value-form binding-env)))
                          compiled-values))
                  ;; Lexical: just allocate local and compile value
                  (let ((idx (env-add-local new-env name)))
                    (push (cons :lexical (cons idx (compile-to-instructions value-form binding-env)))
                          compiled-values)))))
          (setf compiled-values (nreverse compiled-values))
          ;; Second pass: set all bindings
          (dolist (cv compiled-values)
            (let ((kind (car cv))
                  (data (cdr cv)))
              (if (eq kind :special)
                  ;; Set special variable's global
                  (let ((name (car data))
                        (value-instrs (cdr data)))
                    (setf result (append result
                                         (emit-set-special-binding name value-instrs new-env))))
                  ;; Set lexical local
                  (let ((idx (car data))
                        (value-instrs (cdr data)))
                    (setf result (append result
                                         value-instrs
                                         (list (list :local.set idx))))))))))
    ;; Compile body - non-final forms are NOT in tail position
    (let ((non-tail-body-env (env-with-non-tail new-env)))
      (dolist (form (butlast body))
        (setf result (append result
                             (compile-to-instructions form non-tail-body-env)
                             '(:drop)))))
    ;; Last body form: tail position ONLY if no special bindings
    ;; (special bindings require save/restore which breaks TCO)
    (let* ((has-special-bindings special-save-locals)
           (last-form-env (if has-special-bindings
                              (env-with-non-tail new-env)
                              new-env))  ; inherit tail position only without special bindings
           (body-result-instrs
             (when body
               (compile-to-instructions (car (last body)) last-form-env))))
      ;; Save result to a local before restore (if we have special bindings)
      (if special-save-locals
          (let ((result-local (env-add-local new-env (gensym "let-result"))))
            (setf result (append result
                                 body-result-instrs
                                 (list (list :local.set result-local))))
            ;; Restore special bindings in reverse order
            (dolist (save-entry (reverse special-save-locals))
              (let ((name (car save-entry))
                    (save-local (cdr save-entry)))
                (setf result (append result
                                     (emit-restore-special-binding name save-local)))))
            ;; Return the saved result
            (setf result (append result (list (list :local.get result-local)))))
          ;; No special bindings, just append body result
          (setf result (append result body-result-instrs))))
    result))

(defun extend-compilation-env (env)
  "Create an extended copy of the compilation environment for a new scope.
   Shares the local-counter-box and local-types-box so child scopes can allocate locals."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields
   :blocks (cenv-blocks env)
   :block-depth (cenv-block-depth env)
   :tagbody-tags (cenv-tagbody-tags env)
   :tagbody-context (cenv-tagbody-context env)  ; Inherit tagbody context
   :catch-tags (cenv-catch-tags env)
   :unwind-stack (cenv-unwind-stack env)))

(defun copy-compilation-env (env)
  "Create a copy of the compilation environment with mutable fields copied.
   Used by block/return-from, tagbody/go, etc. for nested scopes."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields - copy lists for mutation safety
   :blocks (copy-list (cenv-blocks env))
   :block-depth (cenv-block-depth env)
   :tagbody-tags (copy-list (cenv-tagbody-tags env))
   :tagbody-context (cenv-tagbody-context env)  ; Reference (not copied for nesting)
   :catch-tags (copy-list (cenv-catch-tags env))
   :unwind-stack (copy-list (cenv-unwind-stack env))))

;;; ============================================================
;;; Tail Position Management (TCO)
;;; ============================================================

(defun env-with-tail-position (env tail-position-p)
  "Return an environment with the specified tail position flag.
   If the flag matches the current value, returns the same environment.
   Otherwise, creates a copy with the new flag value."
  (if (eq (cenv-in-tail-position env) tail-position-p)
      env
      (let ((new-env (copy-compilation-env env)))
        (setf (cenv-in-tail-position new-env) tail-position-p)
        new-env)))

(defun env-with-non-tail (env)
  "Return an environment with tail position cleared.
   Used when compiling sub-expressions that are not in tail position
   (e.g., function arguments, test expressions, non-final forms)."
  (env-with-tail-position env nil))

(defun env-with-tail (env)
  "Return an environment with tail position set.
   Used when compiling expressions that are in tail position
   (e.g., last form in function body, branches of if)."
  (env-with-tail-position env t))

;;; ============================================================
;;; Flet/Labels Compilation (T089-T090)
;;; ============================================================

(defun compile-flet (ast env)
  "Compile a flet expression.
   Each local function is compiled as a lambda bound to a local variable.
   Functions in flet cannot call themselves (non-recursive)."
  (let* ((definitions (clysm/compiler/ast:ast-flet-definitions ast))
         (body (clysm/compiler/ast:ast-flet-body ast))
         (new-env (extend-compilation-env env))
         (result '())
         (local-func-bindings '()))
    ;; For each function definition:
    ;; 1. Create a lambda from the function
    ;; 2. Allocate a local for the closure
    ;; 3. Compile the lambda and store in local
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (func-body (third def))  ; List of parsed AST nodes
             ;; Allocate a local to hold the closure
             (local-idx (env-add-local new-env name)))
        ;; Remember this binding for later funcall translation
        (push (cons name local-idx) local-func-bindings)
        ;; Create a lambda AST node
        (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                           :parameters params
                           :body func-body)))
          ;; Compile the lambda (in original env, not new-env, so it can't see itself)
          (setf result (append result (compile-lambda lambda-ast env)))
          ;; Store in local
          (setf result (append result (list (list :local.set local-idx)))))))
    ;; Register local functions for funcall-style access
    (setf (cenv-local-functions new-env) (nreverse local-func-bindings))
    ;; Compile body
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form new-env)
                           '(:drop))))
    (when body
      (setf result (append result
                           (compile-to-instructions (car (last body)) new-env))))
    result))

(defun compile-labels (ast env)
  "Compile a labels expression.
   Functions in labels CAN call themselves and each other (recursive).
   Implementation: Two-phase closure creation to handle mutual recursion:
   1. Create closures with null env, store in locals
   2. Update env fields to point to the closures (now that they exist)
   Tail position propagation:
   - Local function bodies are compiled with tail position for their last forms
   - Labels body's last form inherits tail position from parent for TCO"
  (let* ((definitions (clysm/compiler/ast:ast-labels-definitions ast))
         (body (clysm/compiler/ast:ast-labels-body ast))
         (new-env (extend-compilation-env env))
         (result '())
         (local-func-bindings '())
         ;; Non-final body forms are NOT in tail position
         (non-tail-env (env-with-non-tail new-env)))
    ;; First pass: allocate locals for all function closures
    (dolist (def definitions)
      (let* ((name (first def))
             (local-idx (env-add-local new-env name)))
        (push (cons name local-idx) local-func-bindings)))
    (setf local-func-bindings (nreverse local-func-bindings))
    ;; Register local functions so the body can call them
    (setf (cenv-local-functions new-env) local-func-bindings)
    ;; Phase 1: Create closures with null env initially
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (func-body (third def))
             (local-idx (cdr (assoc name local-func-bindings))))
        ;; Create lambda AST
        (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                           :parameters params
                           :body func-body)))
          ;; Compile the lambda - initially with null env
          ;; The func-names will be in captured-vars but we create with null env first
          (setf result (append result
                               (compile-labels-lambda-phase1 lambda-ast new-env local-func-bindings)))
          ;; Store in local
          (setf result (append result (list (list :local.set local-idx)))))))
    ;; Phase 2: Now update each closure's env field to point to the env cons-list
    ;; containing references to all the local function closures
    (dolist (def definitions)
      (let* ((name (first def))
             (local-idx (cdr (assoc name local-func-bindings))))
        ;; Get the closure
        (setf result (append result (list (list :local.get local-idx))))
        ;; Cast to closure type
        (setf result (append result
                             (list (list :ref.cast
                                         clysm/compiler/codegen/gc-types:+type-closure+))))
        ;; Build the env cons-list containing all local function closures
        (setf result (append result
                             (generate-labels-env-update local-func-bindings new-env)))
        ;; Update the env field (field 4) - struct.set $closure 4
        (setf result (append result
                             (list (list :struct.set
                                         clysm/compiler/codegen/gc-types:+type-closure+
                                         4))))))
    ;; Compile body - non-final forms are NOT in tail position
    (dolist (form (butlast body))
      (setf result (append result
                           (compile-to-instructions form non-tail-env)
                           '(:drop))))
    ;; Last form inherits tail position from parent for TCO
    (when body
      (setf result (append result
                           (compile-to-instructions (car (last body)) new-env))))
    result))

(defun compile-labels-lambda-phase1 (ast env local-func-bindings)
  "Phase 1: Create a closure with null env - env will be filled in phase 2."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (body (clysm/compiler/ast:ast-lambda-body ast))
         ;; Collect regular free vars
         (regular-free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         ;; All local functions defined in labels
         (func-names (mapcar #'car local-func-bindings))
         ;; Check which functions are actually called in the body
         (called-funcs (collect-called-functions body))
         ;; Filter to only include function names that are actually called
         (used-func-names (intersection func-names called-funcs :test #'eq))
         ;; For labels, we need to capture the function closures themselves
         ;; Remove any func-names from regular free vars (they're handled separately)
         (free-vars (set-difference regular-free-vars func-names))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params)))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-names used-func-names
                :local-func-bindings local-func-bindings
                :func-index func-index
                :parent-env env
                :arity arity
                :is-labels-lambda t)
          *pending-lambdas*)
    ;; Create closure with null env (will be updated in phase 2)
    (let ((result '()))
      ;; code fields
      (if (= arity 0)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (if (= arity 1)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (if (= arity 2)
          (setf result (append result (list (list :ref.func func-index))))
          (setf result (append result '((:ref.null :func)))))
      (setf result (append result (list (list :ref.func func-index))))
      ;; env - null initially, will be set in phase 2
      (setf result (append result '((:ref.null :any))))
      ;; Create the struct
      (setf result (append result
                           (list (list :struct.new
                                       clysm/compiler/codegen/gc-types:+type-closure+))))
      result)))

(defun generate-labels-env-update (local-func-bindings env)
  "Generate instructions to create the env cons-list for labels closures.
   Returns instructions that build a cons-list of all local function closures."
  (declare (ignore env))
  ;; Build cons-list from the local function closures
  ;; (cons f1 (cons f2 (cons f3 nil)))
  (let ((result '())
        (bindings (reverse local-func-bindings)))  ; Build from end
    (if (null bindings)
        '((:ref.null :none))
        (progn
          ;; Start with nil
          (setf result '((:ref.null :none)))
          ;; For each function (in reverse order)
          (dolist (binding bindings)
            (let ((local-idx (cdr binding)))
              ;; Stack: rest-of-list
              ;; Push the function closure
              (setf result (append (list (list :local.get local-idx)) result))
              ;; Stack: closure, rest-of-list
              ;; Note: Need to swap order for struct.new which takes car, cdr
              ;; Actually struct.new $cons takes (car, cdr) so we need car first
              ;; Current: closure, rest
              ;; Create cons: struct.new takes (car, cdr) in stack order = (cdr, car) push order
              ;; We want (cons closure rest), so push closure, then rest, then struct.new
              ;; But we have rest on stack already. Let me re-think...
              ))
          ;; Actually let me do this differently - push all closures, then build list
          (setf result '())
          ;; Push closures in order
          (dolist (binding local-func-bindings)
            (setf result (append result (list (list :local.get (cdr binding))))))
          ;; Now build the cons list from end
          ;; Stack: f1, f2, f3
          ;; We want: (cons f1 (cons f2 (cons f3 nil)))
          ;; Push nil
          (setf result (append result '((:ref.null :none))))
          ;; For each closure (in reverse), create cons
          (dotimes (i (length local-func-bindings))
            (setf result (append result
                                 (list (list :struct.new
                                             clysm/compiler/codegen/gc-types:+type-cons+)))))
          result))))

(defun collect-called-functions (body)
  "Collect all function names that are called in the body (list of AST nodes)."
  (let ((result '()))
    (labels ((collect (ast)
               (etypecase ast
                 (clysm/compiler/ast:ast-call
                  (let ((fn (clysm/compiler/ast:ast-call-function ast)))
                    (when (symbolp fn)
                      (pushnew fn result)))
                  (dolist (arg (clysm/compiler/ast:ast-call-arguments ast))
                    (collect arg)))
                 (clysm/compiler/ast:ast-if
                  (collect (clysm/compiler/ast:ast-if-test ast))
                  (collect (clysm/compiler/ast:ast-if-then ast))
                  (when (clysm/compiler/ast:ast-if-else ast)
                    (collect (clysm/compiler/ast:ast-if-else ast))))
                 (clysm/compiler/ast:ast-let
                  (dolist (b (clysm/compiler/ast:ast-let-bindings ast))
                    (collect (cdr b)))
                  (dolist (form (clysm/compiler/ast:ast-let-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-progn
                  (dolist (form (clysm/compiler/ast:ast-progn-forms ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-lambda
                  (dolist (form (clysm/compiler/ast:ast-lambda-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-block
                  (dolist (form (clysm/compiler/ast:ast-block-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-return-from
                  (when (clysm/compiler/ast:ast-return-from-value ast)
                    (collect (clysm/compiler/ast:ast-return-from-value ast))))
                 (clysm/compiler/ast:ast-setq
                  (collect (clysm/compiler/ast:ast-setq-value ast)))
                 ;; Terminals - no function calls
                 (clysm/compiler/ast:ast-literal nil)
                 (clysm/compiler/ast:ast-var-ref nil)
                 (clysm/compiler/ast:ast-defun nil))))
      (dolist (form body)
        (collect form)))
    result))

;;; ============================================================
;;; Progn Compilation
;;; ============================================================

(defun compile-progn (ast env)
  "Compile a progn expression.
   First scans for defuns and registers them, then compiles all forms.
   Only the last form inherits tail position for TCO."
  (let ((forms (clysm/compiler/ast:ast-progn-forms ast))
        (result '())
        (non-tail-env (env-with-non-tail env)))
    ;; First pass: register all defuns so they can be called
    (dolist (form forms)
      (when (typep form 'clysm/compiler/ast:ast-defun)
        (let ((name (clysm/compiler/ast:ast-defun-name form)))
          (unless (env-lookup-function env name)
            (env-add-function env name)))))
    ;; Second pass: compile all forms
    ;; Non-final forms are NOT in tail position
    (dolist (form (butlast forms))
      (setf result (append result
                           (compile-to-instructions form non-tail-env)
                           '(:drop))))
    ;; Last form inherits tail position from parent
    (when forms
      (setf result (append result
                           (compile-to-instructions (car (last forms)) env))))
    (or result (compile-literal (clysm/compiler/ast:make-nil-literal)))))

;;; ============================================================
;;; Variable Assignment
;;; ============================================================

(defun compile-setq (ast env)
  "Compile a setq expression (T042).
   Handles both lexical (local) and special (dynamic) variables."
  (let* ((name (clysm/compiler/ast:ast-setq-name ast))
         (value (clysm/compiler/ast:ast-setq-value ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable - use local.tee for assignment + return value
      (local-idx
       (append
        (compile-to-instructions value env)
        (list (list :local.tee local-idx))))
      ;; Special variable - set symbol's value field (T042-T043)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-setq name value env))
      ;; Unknown variable
      (t
       (error "Cannot setq undefined variable: ~A" name)))))

(defun compile-special-setq (name value-form env)
  "Compile setq for a special variable (T043).
   Sets the symbol's value field and returns the new value.
   Pattern: global.get sym, <value>, local.tee temp, struct.set sym 1, local.get temp"
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (result-local (env-add-local env (gensym "setq-result"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Compile value, save to temp, then set symbol's value field
    (append
     ;; Get symbol reference
     `((:global.get ,global-idx))
     ;; Compile value and save for return
     (compile-to-instructions value-form env)
     ;; Save value to local for return
     `((:local.tee ,result-local)
       ;; Set symbol's value field
       (:struct.set ,symbol-type 1)
       ;; Return the value
       (:local.get ,result-local)))))

;;; ============================================================
;;; Function Definition (T059)
;;; ============================================================

(defun compile-defun (ast env)
  "Compile a function definition.
   Returns info about the compiled function.
   Sets tail position for last form in body to enable TCO."
  (let* ((name (clysm/compiler/ast:ast-defun-name ast))
         (params (clysm/compiler/ast:ast-defun-parameters ast))
         (body (clysm/compiler/ast:ast-defun-body ast))
         (func-env (make-env)))
    ;; Add parameters as locals
    (dolist (param params)
      (env-add-local func-env param))
    ;; Inherit function definitions
    (setf (cenv-functions func-env) (cenv-functions env))
    (setf (cenv-function-counter func-env) (cenv-function-counter env))
    ;; Compile body (non-final forms are NOT in tail position)
    (let ((body-instrs '())
          (non-tail-env (env-with-non-tail func-env))
          (tail-env (env-with-tail func-env)))
      (dolist (form (butlast body))
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions form non-tail-env)
                                  '(:drop))))
      ;; Last form IS in tail position for TCO
      (when body
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions (car (last body)) tail-env))))
      ;; Return function info
      (list :name name
            :params (mapcar (lambda (p) (list p :anyref)) params)
            :result :anyref
            :locals (loop for i from (length params) below (cenv-local-counter func-env)
                          collect (list (gensym "local") (env-local-type func-env i)))
            :body body-instrs))))

;;; ============================================================
;;; Lambda Compilation (T081-T084)
;;; ============================================================

(defparameter *pending-lambdas* nil
  "List of lambda functions waiting to be compiled.
   Each entry is a plist: (:name :params :body :free-vars :func-index)")

(defparameter *lambda-counter* 0
  "Counter for generating unique lambda function names.")

(defun reset-lambda-state ()
  "Reset lambda compilation state."
  (setf *pending-lambdas* nil)
  (setf *lambda-counter* 0))

(defun allocate-lambda-function ()
  "Allocate a new lambda function index and return its name."
  (let ((name (intern (format nil "$LAMBDA-~D" (incf *lambda-counter*)))))
    name))

(defun compile-lambda (ast env)
  "Compile a lambda expression to create a closure struct.
   Returns instructions that push a closure reference onto the stack."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (body (clysm/compiler/ast:ast-lambda-body ast))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params)))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-index func-index
                :parent-env env
                :arity arity)
          *pending-lambdas*)
    ;; Generate code to create closure struct with captured environment
    (generate-closure-creation func-index arity free-vars env)))

(defun generate-closure-creation (func-index arity free-vars env)
  "Generate instructions to create a closure struct.
   The closure has code_0, code_1, code_2, code_N, and env fields.
   Free variables are captured in a cons-list stored in the env field."
  ;; struct.new $closure (code_0, code_1, code_2, code_N, env)
  (let ((result '()))
    ;; code_0 - null if arity != 0, ref.func if arity == 0
    (if (= arity 0)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_1 - null if arity != 1
    (if (= arity 1)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_2 - null if arity != 2
    (if (= arity 2)
        (setf result (append result (list (list :ref.func func-index))))
        (setf result (append result '((:ref.null :func)))))
    ;; code_N - always the fallback
    (setf result (append result (list (list :ref.func func-index))))
    ;; env - capture free variables as a cons-list
    (if free-vars
        ;; Build cons-list: (cons var1 (cons var2 (cons var3 nil)))
        (setf result (append result (generate-env-capture free-vars env)))
        ;; No free vars, use null
        (setf result (append result '((:ref.null :any)))))
    ;; Create the struct
    (setf result (append result
                         (list (list :struct.new
                                     clysm/compiler/codegen/gc-types:+type-closure+))))
    result))

(defun generate-env-capture (free-vars env)
  "Generate instructions to capture free variables as a cons-list.
   Returns instructions that push a cons-list onto the stack.
   Each captured variable becomes (cons value rest)."
  (if (null free-vars)
      ;; Base case: nil (null reference)
      '((:ref.null :none))
      ;; Build cons from end: (cons first-var (cons second-var ...))
      (let ((result '())
            (var (first free-vars)))
        ;; Push the variable value
        (let ((local-idx (env-lookup-local env var)))
          (if local-idx
              (setf result (append result (list (list :local.get local-idx))))
              (error "Cannot capture unbound variable: ~A" var)))
        ;; Push rest of the list (recursive)
        (setf result (append result (generate-env-capture (rest free-vars) env)))
        ;; Create cons cell: struct.new $cons (car, cdr)
        (setf result (append result
                             (list (list :struct.new
                                         clysm/compiler/codegen/gc-types:+type-cons+))))
        result)))

(defun compile-pending-lambdas ()
  "Compile all pending lambda functions and return their definitions.
   Handles nested lambdas by repeatedly compiling until no more are pending."
  (let ((results '())
        (compiled (make-hash-table :test 'equal)))  ; Track compiled lambda names
    (loop while *pending-lambdas*
          do (let ((batch (reverse *pending-lambdas*)))
               (setf *pending-lambdas* nil)  ; Clear before compiling (new ones may be added)
               (dolist (lambda-info batch)
                 (let ((name (getf lambda-info :name)))
                   (unless (gethash name compiled)
                     (setf (gethash name compiled) t)
                     (let ((def (compile-lambda-body lambda-info)))
                       (push def results)))))))
    (nreverse results)))

(defun compile-lambda-body (lambda-info)
  "Compile a lambda function body."
  (let* ((name (getf lambda-info :name))
         (params (getf lambda-info :params))
         (body (getf lambda-info :body))
         (free-vars (getf lambda-info :free-vars))
         (func-names (getf lambda-info :func-names))  ; For labels lambdas
         (is-labels-lambda (getf lambda-info :is-labels-lambda))
         (parent-env (getf lambda-info :parent-env))
         (func-env (make-env)))
    ;; First parameter is the closure itself (for accessing env)
    (env-add-local func-env '$closure)
    ;; Add regular parameters
    (dolist (param params)
      (env-add-local func-env param))
    ;; Register captured variables - they will be accessed via env extraction
    ;; Store the mapping of captured var name -> position in cons-list
    ;; For labels lambdas, func-names come after free-vars
    (let ((position 0))
      (setf (cenv-captured-vars func-env)
            (append
             ;; Regular free variables first
             (loop for var in free-vars
                   collect (prog1 (cons var position) (incf position)))
             ;; Then captured function closures (for labels)
             (loop for fname in (or func-names '())
                   collect (prog1 (cons fname position) (incf position))))))
    ;; For labels lambdas, also set up local-functions so direct calls work
    (when is-labels-lambda
      ;; The captured functions can be accessed via captured-vars mechanism
      ;; but we also need to register them as local-functions for compile-call
      ;; Actually, they're in captured-vars, so compile-var-ref will find them
      ;; But compile-call needs them as local-functions for the call mechanism
      ;; Let's register them - they'll be extracted from captured vars on each call
      (setf (cenv-local-functions func-env)
            (loop for fname in (or func-names '())
                  for i from (length free-vars)
                  collect (cons fname :captured))))  ; Mark as captured, not local
    ;; Inherit function definitions
    (setf (cenv-functions func-env) (cenv-functions parent-env))
    (setf (cenv-function-counter func-env) (cenv-function-counter parent-env))
    ;; Compile body with tail position for last form (TCO)
    (let ((body-instrs '())
          (non-tail-env (env-with-non-tail func-env))
          (tail-env (env-with-tail func-env)))
      (dolist (form (butlast body))
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions form non-tail-env)
                                  '(:drop))))
      ;; Last form IS in tail position for TCO
      (when body
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions (car (last body)) tail-env))))
      ;; Return function info (with closure param first)
      (list :name name
            :params (cons '($closure :anyref)
                          (mapcar (lambda (p) (list p :anyref)) params))
            :result :anyref
            :locals (loop for i from (1+ (length params)) below (cenv-local-counter func-env)
                          collect (list (gensym "local") (env-local-type func-env i)))
            :body body-instrs))))

;;; ============================================================
;;; Block/Return-from Compilation (T107-T109)
;;; ============================================================

(defun compile-block (ast env)
  "Compile a block form.
   Uses Wasm block instruction for normal flow, with br for return-from.
   Pattern: (block :anyref body... end)"
  (let* ((name (clysm/compiler/ast:ast-block-name ast))
         (body (clysm/compiler/ast:ast-block-body ast))
         ;; Create new env with block registered
         (block-env (copy-compilation-env env))
         (result '()))
    ;; Register block at current depth (0 means we can br 0 to exit)
    (push (cons name 0) (cenv-blocks block-env))
    ;; Increment depth for any nested blocks
    (incf (cenv-block-depth block-env))
    ;; Start block with anyref result type
    (setf result (append result '((:block (:result :anyref)))))
    ;; Compile body forms (all but last are dropped)
    (dolist (form (butlast body))
      (setf result (append result (compile-to-instructions form block-env)))
      (setf result (append result '(:drop))))
    ;; Last form (or nil if empty)
    (if body
        (setf result (append result (compile-to-instructions (car (last body)) block-env)))
        (setf result (append result '((:ref.null :none)))))
    ;; End block
    (setf result (append result '(:end)))
    result))

(defun compile-return-from (ast env)
  "Compile return-from.
   Uses br to exit to the named block."
  (let* ((block-name (clysm/compiler/ast:ast-return-from-block-name ast))
         (value (clysm/compiler/ast:ast-return-from-value ast))
         (block-info (assoc block-name (cenv-blocks env))))
    (unless block-info
      (error "return-from: no block named ~A" block-name))
    (let* ((target-depth (cdr block-info))
           ;; Calculate relative br depth
           ;; If we're at depth 2 and block is at depth 0, br 2
           (br-depth (- (cenv-block-depth env) target-depth 1))
           (result '()))
      ;; Compile value
      (setf result (compile-to-instructions value env))
      ;; Branch to block exit
      (setf result (append result (list (list :br br-depth))))
      result)))

;;; ============================================================
;;; Tagbody/Go Strategy Analysis (T008-T010)
;;; ============================================================

(defun collect-go-targets-from-form (form)
  "Collect all go target tags from a single AST form.
   Recursively walks into nested forms (if, progn, etc.)."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (list (clysm/compiler/ast:ast-go-tag form)))
    (clysm/compiler/ast:ast-if
     (append (collect-go-targets-from-form (clysm/compiler/ast:ast-if-test form))
             (collect-go-targets-from-form (clysm/compiler/ast:ast-if-then form))
             (when (clysm/compiler/ast:ast-if-else form)
               (collect-go-targets-from-form (clysm/compiler/ast:ast-if-else form)))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           append (collect-go-targets-from-form subform)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           append (collect-go-targets-from-form subform)))
    ;; For other forms, no go targets inside
    (t nil)))

(defun collect-go-targets (segments)
  "Collect all go target tags from all segments.
   Returns a list of tag symbols that are targets of go statements."
  (loop for segment in segments
        append (loop for form in (cdr segment)
                     append (collect-go-targets-from-form form))))

(defun find-tag-segment-index (segments tag)
  "Find the segment index where TAG is defined.
   Returns nil if tag not found."
  (loop for segment in segments
        for idx from 0
        when (eq (car segment) tag)
        return idx))

(defun check-go-is-backward (segments go-form segment-idx form-position)
  "Check if a go from the given position is a backward jump.
   A backward jump targets a tag at or before the current position."
  (let* ((target-tag (clysm/compiler/ast:ast-go-tag go-form))
         (target-idx (find-tag-segment-index segments target-tag)))
    (when target-idx
      ;; Backward if target segment index is less than current segment index
      ;; OR if target is same segment but we're after the tag definition
      (or (< target-idx segment-idx)
          (and (= target-idx segment-idx) (> form-position 0))))))

(defun check-form-goes-backward (segments form segment-idx form-position tag)
  "Check if all goes to TAG in this form are backward jumps."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (if (eq (clysm/compiler/ast:ast-go-tag form) tag)
         (check-go-is-backward segments form segment-idx form-position)
         t)) ; Not targeting our tag, doesn't affect result
    (clysm/compiler/ast:ast-if
     (and (check-form-goes-backward segments (clysm/compiler/ast:ast-if-test form)
                                    segment-idx form-position tag)
          (check-form-goes-backward segments (clysm/compiler/ast:ast-if-then form)
                                    segment-idx form-position tag)
          (or (null (clysm/compiler/ast:ast-if-else form))
              (check-form-goes-backward segments (clysm/compiler/ast:ast-if-else form)
                                        segment-idx form-position tag))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (t t))) ; Other forms don't contain go

(defun all-goes-are-backward-p (segments tag)
  "Check if all go statements targeting TAG are backward jumps.
   Returns T if all goes to TAG are backward, NIL otherwise."
  (loop for segment in segments
        for seg-idx from 0
        always (loop for form in (cdr segment)
                     for form-pos from 0
                     always (check-form-goes-backward segments form seg-idx form-pos tag))))

(defun analyze-tagbody-strategy (segments)
  "Analyze tagbody segments and determine the compilation strategy.
   Returns one of:
   - :sequential - no go statements, just sequential execution
   - :simple-loop - single tag with backward jumps only (Wasm loop)
   - :dispatch - complex patterns requiring br_table dispatch"
  (let* ((tags (remove nil (mapcar #'car segments)))
         (go-targets (remove-duplicates (collect-go-targets segments))))
    (cond
      ;; No go statements -> sequential
      ((null go-targets) :sequential)
      ;; Single tag, single target, all backward -> simple loop
      ((and (= (length tags) 1)
            (= (length go-targets) 1)
            (eq (first tags) (first go-targets))
            (all-goes-are-backward-p segments (first tags)))
       :simple-loop)
      ;; Everything else -> dispatch
      (t :dispatch))))

;;; ============================================================
;;; Tagbody/Go Compilation (T110-T112)
;;; ============================================================

(defun compile-tagbody-sequential (segments env)
  "Compile tagbody with sequential strategy (no go statements).
   Simply compiles all forms in order and returns NIL."
  (let ((result '()))
    (dolist (segment segments)
      (let ((forms (cdr segment)))
        (dolist (form forms)
          (setf result (append result (compile-to-instructions form env)))
          (setf result (append result '(:drop))))))
    ;; Return NIL
    (append result '((:ref.null :none)))))

(defun compile-tagbody-simple-loop (segments env context)
  "Compile tagbody with simple-loop strategy.
   Used when there's a single tag with only backward jumps.
   Generates: (loop body... (br N for go where N accounts for nesting) ... end) NIL

   Structure:
   - loop (br 0 continues loop from direct context)
     - forms...
     - go -> br (current-depth - base-depth) to reach loop
   - end
   - ref.null none"
  (let* ((result '())
         (loop-env (copy-compilation-env env)))
    ;; Increment block depth for the loop
    (incf (cenv-block-depth loop-env))
    ;; Set up context for compile-go to find
    ;; Record the block depth at which the loop starts
    (setf (tagbody-context-base-block-depth context) (cenv-block-depth loop-env))
    (setf (tagbody-context-base-loop-depth context) 0)  ; br to loop is relative
    (setf (cenv-tagbody-context loop-env) context)
    ;; Start loop with no result type (void loop)
    (setf result '((:loop nil)))
    ;; Compile preamble (forms before first tag)
    (let ((first-segment (first segments)))
      (when (null (car first-segment))
        (dolist (form (cdr first-segment))
          (setf result (append result (compile-to-instructions form loop-env)))
          (setf result (append result '(:drop))))))
    ;; Compile the main tagged segment
    (dolist (segment segments)
      (when (car segment) ; Skip preamble, compile tagged segment
        (dolist (form (cdr segment))
          (setf result (append result (compile-to-instructions form loop-env)))
          (setf result (append result '(:drop))))))
    ;; End loop
    (setf result (append result '(:end)))
    ;; Return NIL
    (append result '((:ref.null :none)))))

(defun compile-tagbody-dispatch (segments env context)
  "Compile tagbody with dispatch strategy.
   Used for complex jump patterns (forward jumps, multiple tags).

   Structure (for N segments):
   block (exit block, depth N+1 from innermost)
     loop (dispatch loop, depth N from innermost)
       block (segment N-1, depth N-1 from innermost)
         block (segment N-2, depth N-2)
           ...
           block (segment 0, depth 0)
             br_table [N N-1 ... 1] 0 $pc
           end
           ;; segment 0 code here
           ;; go -> set $pc, br (depth to loop = N - seg_idx)
         end
         ;; segment 1 code here
       end
       ;; segment N-1 code here
       br (depth to exit = 1)
     end
   end"
  (let* ((result '())
         (num-segments (length segments))
         (pc-local (tagbody-context-pc-local context))
         (dispatch-env (copy-compilation-env env)))
    ;; Track block depth: exit block + loop + num-segments blocks
    ;; After br_table, we're inside all nested blocks
    ;; block-depth at segment 0 = env depth + 2 (exit, loop) + num-segments
    ;; But for go, we need depth relative to the loop, not absolute
    (incf (cenv-block-depth dispatch-env) 2)  ; exit block + loop
    ;; Set up context for compile-go
    (setf (cenv-tagbody-context dispatch-env) context)
    ;; Initialize $pc to 0 (stored as i31ref since all locals are anyref)
    (setf result (list (list :i32.const 0)
                       :ref.i31
                       (list :local.set pc-local)))
    ;; Start exit block (result anyref for NIL)
    (setf result (append result '((:block (:result :anyref)))))
    ;; Start dispatch loop with anyref result type
    ;; The loop never falls through (always exits via br 1 to exit block),
    ;; but the result type is needed for Wasm validation.
    (setf result (append result '((:loop (:result :anyref)))))
    ;; Generate nested blocks (innermost = segment 0)
    (dotimes (i num-segments)
      (setf result (append result '((:block nil)))))
    ;; Generate br_table
    ;; br_table[i] = i (segment index equals br depth to reach that segment's code)
    ;; When inside innermost block (seg 0), br i exits i blocks to segment i's code area
    (let ((br-targets (loop for i from 0 below num-segments collect i)))
      ;; Extract $pc from i31ref to i32 for br_table
      (setf result (append result (list (list :local.get pc-local)
                                        '(:ref.cast :i31)
                                        :i31.get_s)))
      ;; Default target is num-segments (exit all segment blocks, unreachable in normal operation)
      (setf result (append result (list (cons :br_table (append br-targets (list num-segments)))))))
    ;; Close blocks and generate segment code
    (loop for seg-idx from 0 below num-segments
          for segment in segments
          do (let ((segment-env (copy-compilation-env dispatch-env)))
               ;; Close the block for this segment FIRST
               (setf result (append result '(:end)))
               ;; Now segment code runs here, after closing (seg-idx + 1) blocks
               ;; Remaining nesting from segment seg-idx's code area:
               ;; - (num-segments - seg-idx - 1) more segment blocks to exit
               ;; - then the loop (which is the target for br)
               ;; So base-loop-depth = (num-segments - seg-idx - 1)
               (let ((remaining-blocks (- num-segments seg-idx 1)))
                 (setf (cenv-block-depth segment-env)
                       (+ (cenv-block-depth env) 2 remaining-blocks))
                 (setf (tagbody-context-base-block-depth (cenv-tagbody-context segment-env))
                       (cenv-block-depth segment-env))
                 (setf (tagbody-context-base-loop-depth (cenv-tagbody-context segment-env))
                       remaining-blocks))
               ;; Compile segment forms
               (dolist (form (cdr segment))
                 (setf result (append result (compile-to-instructions form segment-env)))
                 (setf result (append result '(:drop))))))
    ;; After all segments, exit the tagbody with NIL
    ;; We're inside the loop, exit block is at depth 1
    (setf result (append result '((:ref.null :none))))
    (setf result (append result '((:br 1))))
    ;; Close dispatch loop
    (setf result (append result '(:end)))
    ;; Close exit block
    (setf result (append result '(:end)))
    result))

(defun compile-tagbody (ast env)
  "Compile tagbody using appropriate strategy based on analysis.
   Strategies:
   - :sequential - no go, just sequential execution
   - :simple-loop - single tag with backward jumps only
   - :dispatch - complex patterns with br_table"
  (let* ((segments (clysm/compiler/ast:ast-tagbody-segments ast))
         (strategy (analyze-tagbody-strategy segments))
         (tag-env (copy-compilation-env env)))
    ;; Register tags in legacy format for backward compatibility
    (loop for segment in segments
          for seg-idx from 0
          do (let ((tag (car segment)))
               (when tag
                 (push (cons tag seg-idx) (cenv-tagbody-tags tag-env)))))
    (case strategy
      (:sequential
       (compile-tagbody-sequential segments tag-env))
      (:simple-loop
       (let* ((context (make-tagbody-context
                        :strategy :simple-loop
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :base-loop-depth 0  ; br 0 targets the loop
                        :num-segments (length segments))))
         (compile-tagbody-simple-loop segments tag-env context)))
      (:dispatch
       (let* ((pc-local (env-add-local tag-env (gensym "pc")))
              (context (make-tagbody-context
                        :strategy :dispatch
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :pc-local pc-local
                        :num-segments (length segments))))
         (compile-tagbody-dispatch segments tag-env context))))))

(defun compile-go (ast env)
  "Compile go statement.
   Uses the tagbody-context from environment to determine how to jump.
   Calculates correct br depth based on current nesting within the tagbody."
  (let* ((tag (clysm/compiler/ast:ast-go-tag ast))
         (context (cenv-tagbody-context env)))
    (unless context
      (error "go outside tagbody: ~A" tag))
    (let ((tag-info (assoc tag (tagbody-context-tags context))))
      (unless tag-info
        (error "undefined tag: ~A" tag))
      (let ((segment-idx (cdr tag-info))
            (strategy (tagbody-context-strategy context))
            ;; Calculate how many blocks we're nested inside relative to the loop
            (nesting-depth (- (cenv-block-depth env)
                              (tagbody-context-base-block-depth context))))
        (case strategy
          (:simple-loop
           ;; For simple loop, branch back to the loop start
           ;; br depth = nesting depth (how many blocks to exit to reach loop)
           (list (list :br nesting-depth)))
          (:dispatch
           ;; For dispatch, set $pc (as i31ref) and branch to dispatch loop
           ;; br depth = nesting depth + base loop depth
           (let ((pc-local (tagbody-context-pc-local context))
                 (base-depth (tagbody-context-base-loop-depth context)))
             (list (list :i32.const segment-idx)
                   :ref.i31  ; wrap as i31ref to match local type
                   (list :local.set pc-local)
                   (list :br (+ nesting-depth base-depth)))))
          (t
           (error "go in sequential tagbody should not be compiled: ~A" tag)))))))

;;; ============================================================
;;; Catch/Throw Compilation (T113-T115)
;;; ============================================================

(defun compile-catch (ast env)
  "Compile catch form using Wasm exception handling.
   Delegates to compile-catch-simple for the actual implementation.
   Uses try_table with catch clause to get exception values directly."
  (compile-catch-simple ast env))

(defun compile-catch-simple (ast env)
  "Compile catch form using Wasm exception handling.

   The key challenge is that when a catch expression is used as a sub-expression
   inside another catch's body, and it catches an exception and returns a value,
   that value should be the result of the catch expression - sibling code should
   not continue.

   Structure with if/else directly producing the result (no br):

   block $catch (type 15)               ;; () -> (anyref anyref)
     try_table (result anyref) (catch 0 $catch)
       ...body...
     end
     ;; Normal path
     local.set $body_result
     ref.null none                      ;; dummy thrown-tag
     ref.null none                      ;; dummy thrown-value
     br $catch
   end  ; $catch - (anyref anyref) on stack

   ;; Handler code - both paths use if with (result anyref)
   local.set $thrown_value
   local.set $thrown_tag
   local.get $thrown_tag
   ref.is_null
   if (result anyref)                   ;; if normal (null tag)
     local.get $body_result
   else                                 ;; exception path
     <tag comparison>
     if (result anyref)                 ;; if tags match
       local.get $thrown_value
     else                               ;; tags don't match - rethrow
       local.get $thrown_tag
       local.get $thrown_value
       throw 0
     end
   end
   ;; Result anyref is now on stack - this IS the catch expression's value"
  (let* ((tag (clysm/compiler/ast:ast-catch-tag ast))
         (body (clysm/compiler/ast:ast-catch-body ast))
         (result '()))
    (let ((tag-local (env-add-local env (gensym "catch-tag")))
          (thrown-tag-local (env-add-local env (gensym "thrown-tag")))
          (thrown-value-local (env-add-local env (gensym "thrown-value")))
          (normal-result-local (env-add-local env (gensym "normal-result"))))
      ;; Evaluate and store the catch tag
      (setf result (append result (compile-to-instructions tag env)))
      (setf result (append result (list (list :local.set tag-local))))

      ;; Create catch environment
      (let ((catch-env (copy-compilation-env env)))
        (push (cons tag-local 0) (cenv-catch-tags catch-env))

        ;; Block $catch - type 21: () -> (anyref anyref)
        ;; Type 21 is $catch_result in the type section
        (setf result (append result '((:block (:type 21)))))  ;; $catch

        ;; try_table inside $catch
        ;; Catch clause: catch tag 0, branch to label 0 ($catch)
        (setf result (append result '((:try_table (:result :anyref) (:catch 0 0)))))

        ;; Compile body
        (dolist (form (butlast body))
          (setf result (append result (compile-to-instructions form catch-env)))
          (setf result (append result (list :drop))))
        (when body
          (setf result (append result (compile-to-instructions (car (last body)) catch-env))))
        (unless body
          (setf result (append result '((:ref.null :none)))))

        (setf result (append result (list :end)))  ;; end try_table

        ;; Normal path: store result, push (nil nil), branch to $catch
        (setf result (append result (list (list :local.set normal-result-local))))
        (setf result (append result '((:ref.null :none))))
        (setf result (append result '((:ref.null :none))))
        (setf result (append result '((:br 0))))  ;; br to $catch with (nil nil)

        (setf result (append result (list :end)))  ;; end $catch block

        ;; At this point: (anyref anyref) on stack
        ;; Either (nil nil) from normal path, or (tag value) from exception
        (setf result (append result (list (list :local.set thrown-value-local))))
        (setf result (append result (list (list :local.set thrown-tag-local))))

        ;; Check if this was a real exception (thrown-tag is not null)
        ;; Use if with (result anyref) so the result is directly on stack
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result (list :ref.is_null)))
        (setf result (append result '((:if (:result :anyref)))))

        ;; Normal case (null tag): return normal result
        (setf result (append result (list (list :local.get normal-result-local))))

        (setf result (append result (list :else)))

        ;; Exception case: compare tags and handle
        (setf result (append result (list (list :local.get tag-local))))
        (setf result (append result '((:ref.cast :eq))))
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result '((:ref.cast :eq))))
        (setf result (append result (list :ref.eq)))
        (setf result (append result '((:if (:result :anyref)))))

        ;; Tags match: return thrown-value
        (setf result (append result (list (list :local.get thrown-value-local))))

        (setf result (append result (list :else)))
        ;; Tags don't match: rethrow (throw never returns, so dummy value for type check)
        (setf result (append result (list (list :local.get thrown-tag-local))))
        (setf result (append result (list (list :local.get thrown-value-local))))
        (setf result (append result '((:throw 0))))
        (setf result (append result (list :end)))  ;; end inner if

        (setf result (append result (list :end)))))  ;; end outer if
    ;; Result anyref is on stack - this is the catch expression's value
    result))

(defun compile-throw (ast env)
  "Compile throw using Wasm throw instruction.
   Throws exception with tag index 0 ($lisp-throw) with (tag-symbol, value) payload."
  (declare (ignore env))
  (let* ((tag (clysm/compiler/ast:ast-throw-tag ast))
         (value (clysm/compiler/ast:ast-throw-value ast))
         (result '()))
    ;; Compile tag expression (evaluates to symbol)
    (setf result (append result (compile-to-instructions tag env)))
    ;; Compile value expression
    (setf result (append result (compile-to-instructions value env)))
    ;; Throw with tag 0 ($lisp-throw)
    (setf result (append result '((:throw 0))))
    result))

;;; ============================================================
;;; Unwind-Protect Compilation (T116-T119)
;;; ============================================================

(defun compile-unwind-protect (ast env)
  "Compile unwind-protect with exception handling.
   Pattern:
   - Normal exit: execute protected form, save result, run cleanup, return result
   - Exception exit: catch any exception, run cleanup, rethrow

   Wasm structure:
   block (result anyref)                    ;; final result
     block (result exnref)                  ;; exception if caught
       try_table (result anyref) (catch_all_ref 0)
         ... protected form ...
       end
       local.set $result
       ... cleanup forms (drop values) ...
       local.get $result
       br 1                                 ;; branch to outer block
     end
     local.set $exnref
     ... cleanup forms (drop values) ...
     local.get $exnref
     throw_ref
   end"
  (let* ((protected-form (clysm/compiler/ast:ast-unwind-protect-protected-form ast))
         (cleanup-forms (clysm/compiler/ast:ast-unwind-protect-cleanup-forms ast))
         ;; Allocate locals
         (result-local (env-add-local env (gensym "unwind-result")))
         (exnref-local (env-add-local env (gensym "exnref") :exnref))
         (result '())
         (cleanup-code '()))
    ;; Pre-compile cleanup forms (we'll use this twice)
    (dolist (form cleanup-forms)
      (setf cleanup-code (append cleanup-code (compile-to-instructions form env)))
      (setf cleanup-code (append cleanup-code '(:drop))))

    ;; block (result anyref) - outer block for final result
    (setf result (append result '((:block (:result :anyref)))))

    ;; block (result exnref) - inner block for exception ref
    (setf result (append result '((:block (:result :exnref)))))

    ;; try_table (result anyref) (catch_all_ref 0)
    (setf result (append result (list (list :try_table '(:result :anyref)
                                            (list :catch_all_ref 0)))))  ; 0 = inner block

    ;; Compile protected form
    (setf result (append result (compile-to-instructions protected-form env)))

    ;; end try_table
    (setf result (append result '(:end)))

    ;; Normal path: save result, run cleanup, return result, br 1 (to outer block)
    (setf result (append result (list (list :local.set result-local))))
    (setf result (append result cleanup-code))
    (setf result (append result (list (list :local.get result-local))))
    (setf result (append result '((:br 1))))  ; br to outer block (skip exception path)

    ;; end inner block
    (setf result (append result '(:end)))

    ;; Exception path: save exnref, run cleanup, throw_ref
    (setf result (append result (list (list :local.set exnref-local))))
    (setf result (append result cleanup-code))
    (setf result (append result (list (list :local.get exnref-local))))
    (setf result (append result '((:throw_ref))))

    ;; end outer block
    (setf result (append result '(:end)))

    result))

;;; ============================================================
;;; Special Variable Binding Support (T009, T034)
;;; ============================================================

(defun generate-restore-binding-instructions ()
  "Generate instructions for the $restore-binding helper (T009).
   Restores the top binding frame: pops the frame, restores old value.

   Pattern:
   (global.get $binding_stack)    ; Get top frame
   (local.tee $frame)
   (struct.get $binding_frame $old_value)  ; Get old value
   (local.get $frame)
   (struct.get $binding_frame $symbol)     ; Get symbol
   (struct.set $symbol $value)             ; Restore value to symbol
   (local.get $frame)
   (struct.get $binding_frame $prev)       ; Get previous frame
   (global.set $binding_stack)             ; Pop frame"
  (let ((frame-local 0)   ; Temporary local for frame reference
        (binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Return instruction list for restore-binding logic
    `(;; Get and save the current binding frame
      (:global.get $binding_stack)
      (:local.tee ,frame-local)
      ;; Get old value from frame (field 1)
      (:struct.get ,binding-frame-type 1)
      ;; Get symbol from frame (field 0)
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 0)
      ;; Restore value to symbol's $value field (field 1)
      (:struct.set ,symbol-type 1)
      ;; Pop the frame: set binding_stack to frame.prev
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 2)  ; Get $prev field
      (:global.set $binding_stack))))

(defun generate-push-binding-frame (symbol-global-name value-instructions)
  "Generate instructions to push a new binding frame (T031).
   Saves the current value and sets a new one.

   Parameters:
   - symbol-global-name: Global variable name for the symbol
   - value-instructions: Instructions that produce the new value

   Pattern:
   1. Create binding frame: (symbol, old_value, prev)
   2. Push to binding stack
   3. Set new value"
  (let ((binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    `(;; Create binding frame with: symbol, old_value, prev
      ;; Field 0: symbol reference
      (:global.get ,symbol-global-name)
      ;; Field 1: old value (current symbol value)
      (:global.get ,symbol-global-name)
      (:struct.get ,symbol-type 1)
      ;; Field 2: prev (current binding stack)
      (:global.get $binding_stack)
      ;; Create the frame struct
      (:struct.new ,binding-frame-type)
      ;; Push to binding stack
      (:global.set $binding_stack)
      ;; Set new value on symbol
      (:global.get ,symbol-global-name)
      ,@value-instructions
      (:struct.set ,symbol-type 1))))

;;; ============================================================
;;; Special Variable Global Tracking (T025)
;;; ============================================================

(defvar *special-var-globals* (make-hash-table :test 'eq)
  "Maps special variable names to their global indices.")

(defvar *next-special-global-index* 2
  "Next available global index for special variables.
   Starts at 2 because 0=NIL, 1=UNBOUND.")

(defun reset-special-var-globals ()
  "Reset special variable global tracking for new compilation."
  (clrhash *special-var-globals*)
  (setf *next-special-global-index* 2))

(defun allocate-special-var-global (name)
  "Allocate a global index for a special variable.
   Returns the index, creating a new one if needed."
  (or (gethash name *special-var-globals*)
      (setf (gethash name *special-var-globals*)
            (prog1 *next-special-global-index*
              (incf *next-special-global-index*)))))

(defun get-special-var-global-index (name)
  "Get the global index for a special variable.
   Returns NIL if not allocated."
  (gethash name *special-var-globals*))

(defun get-all-special-var-globals ()
  "Get all allocated special variable globals as an alist.
   Returns ((name . index) ...) sorted by index."
  (let ((result '()))
    (maphash (lambda (name index)
               (push (cons name index) result))
             *special-var-globals*)
    (sort result #'< :key #'cdr)))

;;; ============================================================
;;; Special Variable Definition Compilation (T022-T025)
;;; ============================================================

(defun make-symbol-global-name (name)
  "Create a global variable name for a symbol.
   Example: *foo* -> $sym_FOO"
  (intern (format nil "$sym_~A" (string-upcase (symbol-name name)))))

(defun compile-defvar (ast env)
  "Compile a defvar form (T022-T023).
   defvar only initializes if the variable is currently unbound.
   Symbols are initialized with null value fields (via struct.new_default),
   so we check for null to determine if uninitialized.

   Generated code pattern:
   1. Get symbol's current value
   2. Check if it's null (uninitialized)
   3. If null, set the initial value
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defvar-name ast))
         (init-form (clysm/compiler/ast:ast-defvar-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (if init-form
        ;; With init-form: conditionally initialize
        ;; Check if symbol.value is null (uninitialized), if so set value
        (append
         ;; Get symbol's value field
         `((:global.get ,global-idx)
           (:struct.get ,symbol-type 1)  ; Get value field
           ;; Check if it's null (uninitialized)
           :ref.is_null
           ;; If null, initialize the value
           (:if (:result :anyref)))
         ;; Then branch: set new value
         `((:global.get ,global-idx))
         (compile-to-instructions init-form env)
         `((:struct.set ,symbol-type 1)
           ;; Return the symbol
           (:global.get ,global-idx)
           :else
           ;; Else branch: already bound, just return symbol
           (:global.get ,global-idx)
           :end))
        ;; Without init-form: just ensure the symbol exists (no-op at runtime)
        `((:global.get ,global-idx)))))

(defun compile-defparameter (ast env)
  "Compile a defparameter form (T024).
   Unlike defvar, defparameter always initializes the variable.

   Generated code pattern:
   1. Get symbol reference
   2. Compile init form
   3. Set symbol's value field
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defparameter-name ast))
         (init-form (clysm/compiler/ast:ast-defparameter-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Always set the value (unlike defvar)
    `(;; Set new value
      (:global.get ,global-idx)
      ,@(compile-to-instructions init-form env)
      (:struct.set ,symbol-type 1)
      ;; Return the symbol
      (:global.get ,global-idx))))

;;; ============================================================
;;; Feature 038: Constant Definitions and Constant Folding
;;; ============================================================

(defvar *constant-registry* (make-hash-table :test 'eq)
  "Compile-time registry for constant values.
   Maps constant names to their computed values for constant folding.")

(defun fold-constant-expression (form registry)
  "Attempt to fold FORM into a constant value at compile time.
   Returns (VALUES value foldable-p) where:
   - value: The computed value if foldable, NIL otherwise
   - foldable-p: T if the expression was successfully folded

   Supports:
   - Literals: numbers, characters, strings, keywords
   - Arithmetic: +, -, *, /, mod, rem
   - Constants: symbols with +NAME+ convention from registry"
  (cond
    ;; Literal number
    ((numberp form)
     (values form t))
    ;; Character literal
    ((characterp form)
     (values form t))
    ;; String literal
    ((stringp form)
     (values form t))
    ;; Keyword
    ((keywordp form)
     (values form t))
    ;; Constant reference (symbol)
    ((and (symbolp form)
          (not (null form))
          (not (eq form t)))
     (multiple-value-bind (value found-p)
         (gethash form registry)
       (if found-p
           (values value t)
           (values nil nil))))
    ;; Arithmetic expression
    ((consp form)
     (let ((op (car form))
           (args (cdr form)))
       (case op
         ;; Addition
         (+
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'+ (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Subtraction
         (-
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'- (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Multiplication
         (*
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'* (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Division
         (/
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (let ((nums (mapcar #'first folded-args)))
                  (if (some #'zerop (cdr nums))
                      (values nil nil)  ; Division by zero - don't fold
                      (values (apply #'/ nums) t)))
                (values nil nil))))
         ;; Modulo
         ((mod rem)
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (and (= (length folded-args) 2)
                     (every #'second folded-args))
                (let ((n (first (first folded-args)))
                      (d (first (second folded-args))))
                  (if (zerop d)
                      (values nil nil)
                      (values (if (eq op 'mod) (mod n d) (rem n d)) t)))
                (values nil nil))))
         ;; Unknown operator - not foldable
         (otherwise
          (values nil nil)))))
    ;; Not foldable
    (t (values nil nil))))

(defun compile-defconstant (ast env)
  "Compile a defconstant form.
   Constants are immutable and their values are computed at compile time.

   Strategy:
   1. Try to fold the value expression to a constant
   2. If foldable, register in *constant-registry* and emit literal
   3. If not foldable, compile normally but still create a global

   Generated code pattern:
   - For foldable constants: literal value
   - For non-foldable: global.get (similar to defparameter but immutable)"
  (let* ((name (clysm/compiler/ast:ast-defconstant-name ast))
         (value-form (clysm/compiler/ast:ast-defconstant-value-form ast)))
    ;; Try to fold the value expression
    (multiple-value-bind (folded-value foldable-p)
        (if (typep value-form 'clysm/compiler/ast:ast-literal)
            ;; If already a literal, get its value
            (let ((lit-value (clysm/compiler/ast:ast-literal-value value-form)))
              (fold-constant-expression lit-value *constant-registry*))
            ;; Otherwise, try to fold the AST
            (values nil nil))
      (if foldable-p
          (progn
            ;; Register constant for future reference
            (setf (gethash name *constant-registry*) folded-value)
            ;; Compile as literal value
            (compile-to-instructions
             (clysm/compiler/ast:make-ast-literal :value folded-value
                                                  :literal-type :fixnum)
             env))
          ;; Non-foldable: treat like defparameter but allocate immutable global
          (let* ((global-idx (allocate-special-var-global name))
                 (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
            `(;; Set value
              (:global.get ,global-idx)
              ,@(compile-to-instructions value-form env)
              (:struct.set ,symbol-type 1)
              ;; Return the symbol
              (:global.get ,global-idx)))))))

;;; ============================================================
;;; Macro Introspection Compilation (016-macro-system T048)
;;; ============================================================

(defun compile-macroexpand-1 (ast env)
  "Compile a macroexpand-1 form.
   Note: For quoted forms, expansion happens at compile time.
   For runtime forms, this requires a runtime macro system (future work).

   Current implementation: compile-time expansion only.
   If the form is a quoted literal, expand it now and compile the result.
   Otherwise, emit the form unchanged (no runtime expansion available)."
  (let ((form-ast (clysm/compiler/ast:ast-macroexpand-1-form ast)))
    ;; Check if form is a quoted literal we can expand at compile time
    (if (and (typep form-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type form-ast) :quoted))
        ;; Compile-time expansion: expand and return as quoted literal
        (let* ((quoted-form (clysm/compiler/ast:ast-literal-value form-ast))
               (expanded (clysm/compiler/transform/macro:macroexpand-1
                          quoted-form)))
          ;; Return the expanded form as a quoted literal
          (compile-to-instructions
           (clysm/compiler/ast:make-ast-literal :value expanded :literal-type :quoted)
           env))
        ;; Runtime form: compile the form and return it
        ;; (No actual expansion at runtime - would require runtime macro registry)
        (compile-to-instructions form-ast env))))

(defun compile-macroexpand (ast env)
  "Compile a macroexpand form.
   Note: For quoted forms, expansion happens at compile time.
   For runtime forms, this requires a runtime macro system (future work).

   Current implementation: compile-time expansion only.
   If the form is a quoted literal, expand it fully and compile the result.
   Otherwise, emit the form unchanged (no runtime expansion available)."
  (let ((form-ast (clysm/compiler/ast:ast-macroexpand-form ast)))
    ;; Check if form is a quoted literal we can expand at compile time
    (if (and (typep form-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type form-ast) :quoted))
        ;; Compile-time expansion: expand fully and return as quoted literal
        (let* ((quoted-form (clysm/compiler/ast:ast-literal-value form-ast))
               (expanded (clysm/compiler/transform/macro:macroexpand
                          quoted-form)))
          ;; Return the expanded form as a quoted literal
          (compile-to-instructions
           (clysm/compiler/ast:make-ast-literal :value expanded :literal-type :quoted)
           env))
        ;; Runtime form: compile the form and return it
        ;; (No actual expansion at runtime - would require runtime macro registry)
        (compile-to-instructions form-ast env))))

(defun compile-macro-function (ast env)
  "Compile a macro-function lookup form.
   Feature 042: Advanced Defmacro.

   Current implementation: compile-time lookup only.
   If the name is a quoted symbol, looks up the macro at compile time
   and returns T if defined, NIL otherwise (closures can't be serialized).

   Note: Full runtime macro-function returning the actual expander
   would require storing closures in a runtime-accessible registry.
   For most compile-time use cases, checking if a macro is defined
   is sufficient."
  (let ((name-ast (clysm/compiler/ast:ast-macro-function-name ast)))
    ;; Check if name is a quoted symbol we can look up at compile time
    (if (and (typep name-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type name-ast) :quoted)
             (symbolp (clysm/compiler/ast:ast-literal-value name-ast)))
        ;; Compile-time lookup: check if macro is defined
        (let* ((macro-name (clysm/compiler/ast:ast-literal-value name-ast))
               (macro-fn (clysm/compiler/transform/macro:macro-function macro-name)))
          ;; Return T if macro is defined, NIL otherwise
          ;; (We can't serialize the actual closure to Wasm)
          (if macro-fn
              ;; Macro is defined - return T
              (list '(:global.get 3))  ; Global 3 is typically T in Clysm
              ;; Macro is not defined - return NIL
              (list '(:ref.null :none))))
        ;; Runtime lookup: not supported, return NIL
        (list '(:ref.null :none)))))

;;; ============================================================
;;; Function Section Generation
;;; ============================================================

(defun generate-func-section (funcs)
  "Generate Wasm Function Section content from compiled functions."
  ;; Function section contains type indices for each function
  (mapcar (lambda (f)
            (declare (ignore f))
            0)  ; Type index (placeholder)
          funcs))

(defun compile-expression (expr env)
  "Compile a Lisp expression to Wasm instructions.
   Main entry point for expression compilation."
  (let ((ast (clysm/compiler/ast:parse-expr expr)))
    (compile-to-instructions ast env)))

;;; ============================================================
;;; Sequence Functions (007-sequence-functions)
;;; ============================================================

;;; --- Tier 1: Basic Sequence Functions ---

(defun compile-length (args env)
  "Compile (length sequence) - return number of elements.
   Supports both lists and strings (008-character-string).
   For strings, counts UTF-8 characters (not bytes).
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "length requires exactly 1 argument"))
  (let ((seq-local (env-add-local env (gensym "LEN-SEQ")))
        (count-local (env-add-local env (gensym "LEN-COUNT") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile sequence argument and store
    `(,@(compile-to-instructions (first args) env)
      (:local.set ,seq-local)
      ;; Check if it's a string
      (:local.get ,seq-local)
      (:ref.test ,(list :ref string-type))
      (:if (:result :i32))
      ;; Then: string - count UTF-8 characters (leaves i32 on stack)
      ,@(compile-string-length-body seq-local count-local string-type env)
      :else
      ;; Else: list - count cons cells (leaves i32 on stack)
      ,@(compile-list-length-body seq-local count-local cons-type)
      :end
      ;; Result is now i32 on stack, convert to fixnum
      :ref.i31)))

(defun compile-string-length-body (seq-local count-local string-type env)
  "Generate instructions to count UTF-8 characters in a string.
   UTF-8 character counting: count bytes that are NOT continuation bytes.
   Continuation bytes are in range 0x80-0xBF (10xxxxxx pattern).
   Stack effect within body: [] -> []
   Sets count-local to the character count."
  (let ((idx-local (env-add-local env (gensym "STR-IDX") :i32))
        (len-local (env-add-local env (gensym "STR-LEN") :i32))
        (byte-local (env-add-local env (gensym "STR-BYTE") :i32)))
    `(;; Get byte array length
      (:local.get ,seq-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Initialize count and index to 0
      (:i32.const 0)
      (:local.set ,count-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; Loop through bytes
      (:block $str_len_done)
      (:loop $str_len_loop)
      ;; Check if idx >= len
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $str_len_done)
      ;; Get byte at idx
      (:local.get ,seq-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if NOT a continuation byte (byte < 0x80 OR byte >= 0xC0)
      ;; Continuation bytes are 0x80-0xBF (10xxxxxx)
      ;; We count if (byte & 0xC0) != 0x80
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)  ; void block type
      ;; Is a leading byte or ASCII, increment count
      (:local.get ,count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,count-local)
      :end
      ;; Increment idx
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $str_len_loop)
      :end  ; loop
      :end  ; block
      ;; Return count (leave on stack for if result)
      (:local.get ,count-local))))

(defun compile-list-length-body (seq-local count-local cons-type)
  "Generate instructions to count elements in a list.
   Stack effect within body: [] -> []
   Sets count-local to the element count."
  `(;; Initialize counter to 0
    (:i32.const 0)
    (:local.set ,count-local)
    ;; Loop until nil
    (:block $len_done)
    (:loop $len_loop)
    ;; Check if list is nil
    (:local.get ,seq-local)
    :ref.is_null
    (:br_if $len_done)
    ;; Increment count
    (:local.get ,count-local)
    (:i32.const 1)
    :i32.add
    (:local.set ,count-local)
    ;; Get cdr
    (:local.get ,seq-local)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 1)
    (:local.set ,seq-local)
    (:br $len_loop)
    :end  ; loop
    :end  ; block
    ;; Return count (leave on stack for if result)
    (:local.get ,count-local)))

(defun compile-append (args env)
  "Compile (append list1 list2) - concatenate lists.
   Does not modify input lists. Last argument is shared.
   Stack: [] -> [list]"
  (cond
    ((null args) '((:ref.null :none)))
    ((= (length args) 1)
     (compile-to-instructions (first args) env))
    ((= (length args) 2)
     (compile-append-two (first args) (second args) env))
    (t
     ;; Multiple lists: fold right (append a (append b (append c d)))
     (compile-append-two (first args)
                         (clysm/compiler/ast:make-ast-call
                          :function 'append
                          :arguments (rest args))
                         env))))

(defun compile-append-two (list1-form list2-form env)
  "Compile (append list1 list2) for exactly two lists."
  (let ((result '())
        (list1-local (env-add-local env (gensym "APP-L1")))
        (list2-local (env-add-local env (gensym "APP-L2")))
        (acc-local (env-add-local env (gensym "APP-ACC")))
        (temp-local (env-add-local env (gensym "APP-TMP")))
        (last-cons-local (env-add-local env (gensym "APP-LAST")))  ; anyref
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile both lists
    (setf result (append result (compile-to-instructions list1-form env)))
    (setf result (append result (list (list :local.set list1-local))))
    (setf result (append result (compile-to-instructions list2-form env)))
    (setf result (append result (list (list :local.set list2-local))))
    ;; If list1 is nil, return list2
    (setf result (append result (list (list :local.get list1-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:local.get ,list2-local)
                           :else
                           ;; Copy list1, then set last cdr to list2
                           ;; Start with first cons of result
                           (:local.get ,list1-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)  ; car of first
                           (:ref.null :none)           ; placeholder cdr
                           (:struct.new ,cons-type)
                           (:local.tee ,acc-local)
                           (:local.set ,last-cons-local)
                           ;; Advance list1 to cdr
                           (:local.get ,list1-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list1-local)
                           ;; Loop to copy rest of list1
                           (:block $app_done)
                           (:loop $app_loop)
                           (:local.get ,list1-local)
                           :ref.is_null
                           (:br_if $app_done)
                           ;; Create new cons
                           (:local.get ,list1-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           ;; Link previous cons to this one (cast last-cons to ref $cons)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           ;; Update last-cons
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance list1
                           (:local.get ,list1-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list1-local)
                           (:br $app_loop)
                           :end  ; loop
                           :end  ; block
                           ;; Link last cons to list2 (cast last-cons to ref $cons)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,list2-local)
                           (:struct.set ,cons-type 1)
                           (:local.get ,acc-local)
                           :end)))  ; if
    result))

(defun compile-reverse (args env)
  "Compile (reverse list) - return new reversed list.
   Does not modify input list.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "reverse requires exactly 1 argument"))
  (let ((result '())
        (list-local (env-add-local env (gensym "REV-LIST")))
        (acc-local (env-add-local env (gensym "REV-ACC")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize accumulator to nil
    (setf result (append result '((:ref.null :none))))
    (setf result (append result (list (list :local.set acc-local))))
    ;; Loop: prepend each element to accumulator
    (setf result (append result
                         `((:block $rev_done)
                           (:loop $rev_loop)
                           ;; Check if list is nil
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $rev_done)
                           ;; Create new cons: (cons (car list) acc)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,acc-local)
                           (:struct.new ,cons-type)
                           (:local.set ,acc-local)
                           ;; Advance to cdr
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $rev_loop)
                           :end  ; loop
                           :end))) ; block
    ;; Return accumulator
    (setf result (append result (list (list :local.get acc-local))))
    result))

(defun compile-nreverse (args env)
  "Compile (nreverse list) - destructively reverse list.
   Modifies cdr pointers in place.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "nreverse requires exactly 1 argument"))
  (let ((result '())
        (prev-local (env-add-local env (gensym "NREV-PREV")))
        (current-local (env-add-local env (gensym "NREV-CURR")))
        (next-local (env-add-local env (gensym "NREV-NEXT")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Initialize prev to nil
    (setf result (append result '((:ref.null :none))))
    (setf result (append result (list (list :local.set prev-local))))
    ;; current = list
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set current-local))))
    ;; Loop to reverse
    (setf result (append result
                         `((:block $nrev_done)
                           (:loop $nrev_loop)
                           ;; Check if current is nil
                           (:local.get ,current-local)
                           :ref.is_null
                           (:br_if $nrev_done)
                           ;; next = cdr(current)
                           (:local.get ,current-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,next-local)
                           ;; cdr(current) = prev
                           (:local.get ,current-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,prev-local)
                           (:struct.set ,cons-type 1)
                           ;; prev = current
                           (:local.get ,current-local)
                           (:local.set ,prev-local)
                           ;; current = next
                           (:local.get ,next-local)
                           (:local.set ,current-local)
                           (:br $nrev_loop)
                           :end  ; loop
                           :end))) ; block
    ;; Return prev (new head)
    (setf result (append result (list (list :local.get prev-local))))
    result))

(defun compile-last (args env)
  "Compile (last list) - return last cons cell.
   Stack: [] -> [cons or nil]"
  (when (< (length args) 1)
    (error "last requires at least 1 argument"))
  ;; For simplicity, only support (last list), not (last list n)
  (let ((result '())
        (list-local (env-add-local env (gensym "LAST-LIST")))
        (prev-local (env-add-local env (gensym "LAST-PREV")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If list is nil, return nil
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; prev = list
                           (:local.get ,list-local)
                           (:local.set ,prev-local)
                           ;; Loop until cdr is nil
                           (:block $last_done (:result :anyref))
                           (:loop $last_loop (:result :anyref))
                           ;; Get cdr
                           (:local.get ,prev-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.tee ,list-local)
                           :ref.is_null
                           ;; If cdr is nil, return prev
                           (:if (:result :anyref))
                           (:local.get ,prev-local)
                           (:br $last_done)
                           :else
                           ;; Else prev = cdr, continue
                           (:local.get ,list-local)
                           (:local.set ,prev-local)
                           (:ref.null :none)
                           (:br $last_loop)
                           :end
                           :end  ; loop
                           :end  ; block
                           :end)))
    result))

(defun compile-butlast (args env)
  "Compile (butlast list) - return list without last element.
   Does not modify input list.
   Stack: [] -> [list]"
  (when (< (length args) 1)
    (error "butlast requires at least 1 argument"))
  ;; For simplicity, only support (butlast list), not (butlast list n)
  (let ((result '())
        (list-local (env-add-local env (gensym "BUTL-LIST")))
        (acc-local (env-add-local env (gensym "BUTL-ACC")))
        (last-cons-local (env-add-local env (gensym "BUTL-LAST")))  ; anyref
        (temp-local (env-add-local env (gensym "BUTL-TMP")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If list is nil or single element, return nil
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; Check if cdr is nil (single element)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; Build new list without last element
                           ;; Start with first cons
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.tee ,acc-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           ;; Loop while cddr exists
                           (:block $butl_done)
                           (:loop $butl_loop)
                           ;; Check if cdr(list) is nil (meaning list is last element)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           :ref.is_null
                           (:br_if $butl_done)
                           ;; Create new cons for this element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           ;; Link (cast last-cons to ref $cons)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $butl_loop)
                           :end  ; loop
                           :end  ; block
                           (:local.get ,acc-local)
                           :end  ; if single
                           :end))) ; if nil
    result))

(defun compile-copy-list (args env)
  "Compile (copy-list list) - shallow copy of list.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "copy-list requires exactly 1 argument"))
  (let ((result '())
        (list-local (env-add-local env (gensym "COPY-LIST")))
        (acc-local (env-add-local env (gensym "COPY-ACC")))
        (last-cons-local (env-add-local env (gensym "COPY-LAST")))  ; anyref
        (temp-local (env-add-local env (gensym "COPY-TMP")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If nil, return nil
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; Start with first cons
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.tee ,acc-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           ;; Loop to copy rest
                           (:block $copy_done)
                           (:loop $copy_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $copy_done)
                           ;; Create new cons
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           ;; Link (cast last-cons to ref $cons)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $copy_loop)
                           :end  ; loop
                           :end  ; block
                           (:local.get ,acc-local)
                           :end)))
    result))

;;; --- Tier 2: Higher-Order Sequence Functions ---

(defun compile-mapcar (args env)
  "Compile (mapcar fn list) - apply fn to each element.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "mapcar requires exactly 2 arguments"))
  (let ((result '())
        (fn-local (env-add-local env (gensym "MAP-FN")))
        (list-local (env-add-local env (gensym "MAP-LIST")))
        (acc-local (env-add-local env (gensym "MAP-ACC")))
        (last-cons-local (env-add-local env (gensym "MAP-LAST")))  ; anyref for type compatibility
        (temp-local (env-add-local env (gensym "MAP-TMP")))
        (result-local (env-add-local env (gensym "MAP-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile function argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set fn-local))))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If list is nil, return nil
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; Apply fn to first element and create first cons
                           ;; funcall: closure, arg
                           (:local.get ,fn-local)  ; closure as first param
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)  ; car
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)  ; code_1
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.set ,result-local)
                           ;; Create first cons with result
                           (:local.get ,result-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.tee ,acc-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           ;; Loop for rest
                           (:block $map_done (:result :anyref))
                           (:loop $map_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:local.get ,acc-local)  ; Return accumulated list
                           (:br $map_done)
                           :else
                           (:ref.null :none)  ; Dummy value for loop iteration
                           :end
                           :drop  ; Drop the dummy value
                           ;; Apply fn
                           (:local.get ,fn-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.set ,result-local)
                           ;; Create cons and link
                           (:local.get ,result-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $map_loop)
                           :end  ; loop
                           :drop
                           (:local.get ,acc-local)
                           :end  ; block
                           :end)))
    result))

(defun compile-mapc (args env)
  "Compile (mapc fn list) - apply fn for side effects, return original list.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "mapc requires exactly 2 arguments"))
  (let ((result '())
        (fn-local (env-add-local env (gensym "MAPC-FN")))
        (list-local (env-add-local env (gensym "MAPC-LIST")))
        (original-local (env-add-local env (gensym "MAPC-ORIG")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile function argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set fn-local))))
    ;; Compile list argument and save original
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.tee original-local))))
    (setf result (append result (list (list :local.set list-local))))
    ;; Loop applying fn
    (setf result (append result
                         `((:block $mapc_done)
                           (:loop $mapc_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $mapc_done)
                           ;; Apply fn (discard result)
                           (:local.get ,fn-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :drop
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $mapc_loop)
                           :end  ; loop
                           :end))) ; block
    ;; Return original list
    (setf result (append result (list (list :local.get original-local))))
    result))

(defun compile-maplist (args env)
  "Compile (maplist fn list) - apply fn to successive cdrs.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "maplist requires exactly 2 arguments"))
  (let ((result '())
        (fn-local (env-add-local env (gensym "MAPL-FN")))
        (list-local (env-add-local env (gensym "MAPL-LIST")))
        (acc-local (env-add-local env (gensym "MAPL-ACC")))
        (last-cons-local (env-add-local env (gensym "MAPL-LAST")))  ; anyref for type compatibility
        (temp-local (env-add-local env (gensym "MAPL-TMP")))
        (result-local (env-add-local env (gensym "MAPL-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile function argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set fn-local))))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If list is nil, return nil
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)
                           :else
                           ;; Apply fn to list itself (not just car)
                           (:local.get ,fn-local)
                           (:local.get ,list-local)  ; pass whole list
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.set ,result-local)
                           ;; Create first cons
                           (:local.get ,result-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.tee ,acc-local)
                           (:local.set ,last-cons-local)
                           ;; Advance to cdr
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           ;; Loop for rest
                           (:block $mapl_done (:result :anyref))
                           (:loop $mapl_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:local.get ,acc-local)  ; Return accumulated list
                           (:br $mapl_done)
                           :else
                           (:ref.null :none)  ; Dummy value for loop iteration
                           :end
                           :drop  ; Drop the dummy value
                           ;; Apply fn to tail
                           (:local.get ,fn-local)
                           (:local.get ,list-local)
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.set ,result-local)
                           ;; Create cons and link
                           (:local.get ,result-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $mapl_loop)
                           :end  ; loop
                           :drop
                           (:local.get ,acc-local)
                           :end  ; block
                           :end)))
    result))

(defun compile-reduce (args env)
  "Compile (reduce fn list) - combine elements using fn.
   Stack: [] -> [any]"
  (when (< (length args) 2)
    (error "reduce requires at least 2 arguments"))
  ;; For simplicity, no :initial-value support yet
  (let ((result '())
        (fn-local (env-add-local env (gensym "RED-FN")))
        (list-local (env-add-local env (gensym "RED-LIST")))
        (acc-local (env-add-local env (gensym "RED-ACC")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-2+))
    ;; Compile function argument
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set fn-local))))
    ;; Compile list argument
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; If list is nil, error (or return nil for now)
    (setf result (append result (list (list :local.get list-local))))
    (setf result (append result '(:ref.is_null)))
    (setf result (append result
                         `((:if (:result :anyref))
                           (:ref.null :none)  ; Empty list without initial-value
                           :else
                           ;; acc = first element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,acc-local)
                           ;; Advance to cdr
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           ;; Loop to reduce
                           (:block $red_done (:result :anyref))
                           (:loop $red_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:local.get ,acc-local)  ; Return accumulated result
                           (:br $red_done)
                           :else
                           (:ref.null :none)  ; Dummy value for loop iteration
                           :end
                           :drop  ; Drop the dummy value
                           ;; acc = (fn acc (car list))
                           (:local.get ,fn-local)
                           (:local.get ,acc-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,fn-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 2)  ; code_2 for 2-arity
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.set ,acc-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $red_loop)
                           :end  ; loop
                           :drop
                           (:local.get ,acc-local)
                           :end  ; block
                           :end)))
    result))

;;; --- Tier 3: Search and Filter Functions ---

(defun compile-find (args env)
  "Compile (find item list) - find first matching element.
   Uses eql for comparison.
   Stack: [] -> [element or nil]"
  (when (/= (length args) 2)
    (error "find requires exactly 2 arguments"))
  (let ((result '())
        (item-local (env-add-local env (gensym "FIND-ITEM")))
        (list-local (env-add-local env (gensym "FIND-LIST")))
        (elem-local (env-add-local env (gensym "FIND-ELEM")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile item
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set item-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $find_done (:result :anyref))
                           (:loop $find_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $find_done)
                           :else
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.tee ,elem-local)
                           ;; Compare with item (using ref.eq - cast to eq type first)
                           (:ref.cast :eq)
                           (:local.get ,item-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if (:result :anyref))
                           (:local.get ,elem-local)
                           (:br $find_done)
                           :else
                           ;; Also compare as fixnums if both are i31ref
                           (:local.get ,elem-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,item-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           ;; Both fixnums, compare values
                           (:local.get ,elem-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,item-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if (:result :anyref))
                           (:local.get ,elem-local)
                           (:br $find_done)
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :drop
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $find_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-find-if (args env)
  "Compile (find-if pred list) - find first element satisfying predicate.
   Stack: [] -> [element or nil]"
  (when (/= (length args) 2)
    (error "find-if requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "FINDIF-PRED")))
        (list-local (env-add-local env (gensym "FINDIF-LIST")))
        (elem-local (env-add-local env (gensym "FINDIF-ELEM")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $findif_done (:result :anyref))
                           (:loop $findif_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $findif_done)
                           :else
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,elem-local)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           ;; Check if result is not nil
                           :ref.is_null
                           :i32.eqz
                           (:if (:result :anyref))
                           (:local.get ,elem-local)
                           (:br $findif_done)
                           :else
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $findif_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-position (args env)
  "Compile (position item list) - find index of first matching element.
   Stack: [] -> [fixnum or nil]"
  (when (/= (length args) 2)
    (error "position requires exactly 2 arguments"))
  (let ((result '())
        (item-local (env-add-local env (gensym "POS-ITEM")))
        (list-local (env-add-local env (gensym "POS-LIST")))
        (idx-local (env-add-local env (gensym "POS-IDX") :i32))
        (elem-local (env-add-local env (gensym "POS-ELEM")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile item
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set item-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize index to 0
    (setf result (append result '((:i32.const 0))))
    (setf result (append result (list (list :local.set idx-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $pos_done (:result :anyref))
                           (:loop $pos_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $pos_done)
                           :else
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Compare (fixnum comparison)
                           (:local.get ,elem-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,item-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,elem-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,item-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if (:result :anyref))
                           (:local.get ,idx-local)
                           :ref.i31
                           (:br $pos_done)
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           ;; ref.eq fallback (cast to eq type first)
                           (:local.get ,elem-local)
                           (:ref.cast :eq)
                           (:local.get ,item-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if (:result :anyref))
                           (:local.get ,idx-local)
                           :ref.i31
                           (:br $pos_done)
                           :else
                           (:ref.null :none)
                           :end
                           :end
                           :drop
                           ;; Increment index and advance
                           (:local.get ,idx-local)
                           (:i32.const 1)
                           :i32.add
                           (:local.set ,idx-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $pos_loop)
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-position-if (args env)
  "Compile (position-if pred list) - find index of first satisfying element.
   Stack: [] -> [fixnum or nil]"
  (when (/= (length args) 2)
    (error "position-if requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "POSIF-PRED")))
        (list-local (env-add-local env (gensym "POSIF-LIST")))
        (idx-local (env-add-local env (gensym "POSIF-IDX") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize index to 0
    (setf result (append result '((:i32.const 0))))
    (setf result (append result (list (list :local.set idx-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $posif_done (:result :anyref))
                           (:loop $posif_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $posif_done)
                           :else
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           :i32.eqz
                           (:if (:result :anyref))
                           (:local.get ,idx-local)
                           :ref.i31
                           (:br $posif_done)
                           :else
                           ;; Increment and advance
                           (:local.get ,idx-local)
                           (:i32.const 1)
                           :i32.add
                           (:local.set ,idx-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $posif_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-remove (args env)
  "Compile (remove item list) - return list without matching elements.
   Does not modify input list.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "remove requires exactly 2 arguments"))
  (let ((result '())
        (item-local (env-add-local env (gensym "REM-ITEM")))
        (list-local (env-add-local env (gensym "REM-LIST")))
        (acc-local (env-add-local env (gensym "REM-ACC")))
        (last-cons-local (env-add-local env (gensym "REM-LAST")))  ; anyref for type compatibility
        (temp-local (env-add-local env (gensym "REM-TMP")))
        (elem-local (env-add-local env (gensym "REM-ELEM")))
        (first-local (env-add-local env (gensym "REM-FIRST") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile item
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set item-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize accumulator to nil, first flag to 1
    (setf result (append result '((:ref.null :none))))
    (setf result (append result (list (list :local.set acc-local))))
    (setf result (append result '((:i32.const 1))))
    (setf result (append result (list (list :local.set first-local))))
    ;; Loop
    (setf result (append result
                         `((:block $rem_done)
                           (:loop $rem_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $rem_done)
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Compare (fixnum)
                           (:local.get ,elem-local)
                           (:ref.test :i31)
                           (:if)
                           (:local.get ,item-local)
                           (:ref.test :i31)
                           (:if)
                           (:local.get ,elem-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,item-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if)
                           ;; Match - skip this element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $rem_loop)
                           :end
                           :end
                           :end
                           ;; ref.eq check (cast to eq type first)
                           (:local.get ,elem-local)
                           (:ref.cast :eq)
                           (:local.get ,item-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $rem_loop)
                           :end
                           ;; Not a match - add to result
                           (:local.get ,elem-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           (:local.get ,first-local)
                           (:if)
                           (:local.get ,temp-local)
                           (:local.set ,acc-local)
                           (:i32.const 0)
                           (:local.set ,first-local)
                           :else
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           :end
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $rem_loop)
                           :end  ; loop
                           :end))) ; block
    ;; Return accumulator
    (setf result (append result (list (list :local.get acc-local))))
    result))

(defun compile-remove-if (args env)
  "Compile (remove-if pred list) - remove elements satisfying predicate.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "remove-if requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "REMIF-PRED")))
        (list-local (env-add-local env (gensym "REMIF-LIST")))
        (acc-local (env-add-local env (gensym "REMIF-ACC")))
        (last-cons-local (env-add-local env (gensym "REMIF-LAST")))  ; anyref for type compatibility
        (temp-local (env-add-local env (gensym "REMIF-TMP")))
        (elem-local (env-add-local env (gensym "REMIF-ELEM")))
        (first-local (env-add-local env (gensym "REMIF-FIRST") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize
    (setf result (append result '((:ref.null :none))))
    (setf result (append result (list (list :local.set acc-local))))
    (setf result (append result '((:i32.const 1))))
    (setf result (append result (list (list :local.set first-local))))
    ;; Loop
    (setf result (append result
                         `((:block $remif_done)
                           (:loop $remif_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $remif_done)
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,elem-local)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           :i32.eqz
                           (:if)
                           ;; Predicate true - skip
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $remif_loop)
                           :end
                           ;; Keep element
                           (:local.get ,elem-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           (:local.get ,first-local)
                           (:if)
                           (:local.get ,temp-local)
                           (:local.set ,acc-local)
                           (:i32.const 0)
                           (:local.set ,first-local)
                           :else
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           :end
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $remif_loop)
                           :end  ; loop
                           :end))) ; block
    (setf result (append result (list (list :local.get acc-local))))
    result))

(defun compile-remove-if-not (args env)
  "Compile (remove-if-not pred list) - keep only elements satisfying predicate.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "remove-if-not requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "REMIFN-PRED")))
        (list-local (env-add-local env (gensym "REMIFN-LIST")))
        (acc-local (env-add-local env (gensym "REMIFN-ACC")))
        (last-cons-local (env-add-local env (gensym "REMIFN-LAST")))  ; anyref for type compatibility
        (temp-local (env-add-local env (gensym "REMIFN-TMP")))
        (elem-local (env-add-local env (gensym "REMIFN-ELEM")))
        (first-local (env-add-local env (gensym "REMIFN-FIRST") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize
    (setf result (append result '((:ref.null :none))))
    (setf result (append result (list (list :local.set acc-local))))
    (setf result (append result '((:i32.const 1))))
    (setf result (append result (list (list :local.set first-local))))
    ;; Loop
    (setf result (append result
                         `((:block $remifn_done)
                           (:loop $remifn_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $remifn_done)
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,elem-local)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           (:if)
                           ;; Predicate false - skip
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $remifn_loop)
                           :end
                           ;; Keep element (predicate was true)
                           (:local.get ,elem-local)
                           (:ref.null :none)
                           (:struct.new ,cons-type)
                           (:local.set ,temp-local)
                           (:local.get ,first-local)
                           (:if)
                           (:local.get ,temp-local)
                           (:local.set ,acc-local)
                           (:i32.const 0)
                           (:local.set ,first-local)
                           :else
                           (:local.get ,last-cons-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:local.get ,temp-local)
                           (:struct.set ,cons-type 1)
                           :end
                           (:local.get ,temp-local)
                           (:local.set ,last-cons-local)
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $remifn_loop)
                           :end  ; loop
                           :end))) ; block
    (setf result (append result (list (list :local.get acc-local))))
    result))

(defun compile-count (args env)
  "Compile (count item list) - count matching elements.
   Stack: [] -> [fixnum]"
  (when (/= (length args) 2)
    (error "count requires exactly 2 arguments"))
  (let ((result '())
        (item-local (env-add-local env (gensym "CNT-ITEM")))
        (list-local (env-add-local env (gensym "CNT-LIST")))
        (count-local (env-add-local env (gensym "CNT-COUNT") :i32))
        (elem-local (env-add-local env (gensym "CNT-ELEM")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile item
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set item-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize count
    (setf result (append result '((:i32.const 0))))
    (setf result (append result (list (list :local.set count-local))))
    ;; Loop
    (setf result (append result
                         `((:block $cnt_done)
                           (:loop $cnt_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $cnt_done)
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Compare
                           (:local.get ,elem-local)
                           (:ref.test :i31)
                           (:if)
                           (:local.get ,item-local)
                           (:ref.test :i31)
                           (:if)
                           (:local.get ,elem-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,item-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if)
                           (:local.get ,count-local)
                           (:i32.const 1)
                           :i32.add
                           (:local.set ,count-local)
                           :end
                           :end
                           :else
                           ;; ref.eq fallback (cast to eq type first)
                           (:local.get ,elem-local)
                           (:ref.cast :eq)
                           (:local.get ,item-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if)
                           (:local.get ,count-local)
                           (:i32.const 1)
                           :i32.add
                           (:local.set ,count-local)
                           :end
                           :end
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $cnt_loop)
                           :end  ; loop
                           :end))) ; block
    ;; Return count as fixnum
    (setf result (append result (list (list :local.get count-local))))
    (setf result (append result '(:ref.i31)))
    result))

(defun compile-count-if (args env)
  "Compile (count-if pred list) - count elements satisfying predicate.
   Stack: [] -> [fixnum]"
  (when (/= (length args) 2)
    (error "count-if requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "CNTIF-PRED")))
        (list-local (env-add-local env (gensym "CNTIF-LIST")))
        (count-local (env-add-local env (gensym "CNTIF-COUNT") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Initialize count
    (setf result (append result '((:i32.const 0))))
    (setf result (append result (list (list :local.set count-local))))
    ;; Loop
    (setf result (append result
                         `((:block $cntif_done)
                           (:loop $cntif_loop)
                           (:local.get ,list-local)
                           :ref.is_null
                           (:br_if $cntif_done)
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           :i32.eqz
                           (:if)
                           (:local.get ,count-local)
                           (:i32.const 1)
                           :i32.add
                           (:local.set ,count-local)
                           :end
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:br $cntif_loop)
                           :end  ; loop
                           :end))) ; block
    (setf result (append result (list (list :local.get count-local))))
    (setf result (append result '(:ref.i31)))
    result))

;;; --- Tier 4: Membership and Association ---

(defun compile-member (args env)
  "Compile (member item list) - return tail starting with matching element.
   Stack: [] -> [list or nil]"
  (when (/= (length args) 2)
    (error "member requires exactly 2 arguments"))
  (let ((result '())
        (item-local (env-add-local env (gensym "MEM-ITEM")))
        (list-local (env-add-local env (gensym "MEM-LIST")))
        (elem-local (env-add-local env (gensym "MEM-ELEM")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile item
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set item-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $mem_done (:result :anyref))
                           (:loop $mem_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $mem_done)
                           :else
                           ;; Get element
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,elem-local)
                           ;; Compare
                           (:local.get ,elem-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,item-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,elem-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,item-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if (:result :anyref))
                           (:local.get ,list-local)
                           (:br $mem_done)
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           ;; ref.eq (cast to eq type first)
                           (:local.get ,elem-local)
                           (:ref.cast :eq)
                           (:local.get ,item-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if (:result :anyref))
                           (:local.get ,list-local)
                           (:br $mem_done)
                           :else
                           (:ref.null :none)
                           :end
                           :end
                           :drop
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $mem_loop)
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-assoc (args env)
  "Compile (assoc key alist) - find pair by key.
   Stack: [] -> [cons or nil]"
  (when (/= (length args) 2)
    (error "assoc requires exactly 2 arguments"))
  (let ((result '())
        (key-local (env-add-local env (gensym "ASSOC-KEY")))
        (list-local (env-add-local env (gensym "ASSOC-LIST")))
        (pair-local (env-add-local env (gensym "ASSOC-PAIR")))
        (pair-key-local (env-add-local env (gensym "ASSOC-PKEY")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile key
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set key-local))))
    ;; Compile alist
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $assoc_done (:result :anyref))
                           (:loop $assoc_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $assoc_done)
                           :else
                           ;; Get pair (car of list)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,pair-local)
                           ;; Get key of pair (car of pair)
                           (:local.get ,pair-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,pair-key-local)
                           ;; Compare keys
                           (:local.get ,pair-key-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,key-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,pair-key-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,key-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if (:result :anyref))
                           (:local.get ,pair-local)
                           (:br $assoc_done)
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           ;; ref.eq (cast to eq type first)
                           (:local.get ,pair-key-local)
                           (:ref.cast :eq)
                           (:local.get ,key-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if (:result :anyref))
                           (:local.get ,pair-local)
                           (:br $assoc_done)
                           :else
                           (:ref.null :none)
                           :end
                           :end
                           :drop
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $assoc_loop)
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-rassoc (args env)
  "Compile (rassoc value alist) - find pair by value.
   Stack: [] -> [cons or nil]"
  (when (/= (length args) 2)
    (error "rassoc requires exactly 2 arguments"))
  (let ((result '())
        (value-local (env-add-local env (gensym "RASSOC-VAL")))
        (list-local (env-add-local env (gensym "RASSOC-LIST")))
        (pair-local (env-add-local env (gensym "RASSOC-PAIR")))
        (pair-val-local (env-add-local env (gensym "RASSOC-PVAL")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile value
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set value-local))))
    ;; Compile alist
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Search loop
    (setf result (append result
                         `((:block $rassoc_done (:result :anyref))
                           (:loop $rassoc_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $rassoc_done)
                           :else
                           ;; Get pair
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.set ,pair-local)
                           ;; Get value of pair (cdr of pair)
                           (:local.get ,pair-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,pair-val-local)
                           ;; Compare values
                           (:local.get ,pair-val-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,value-local)
                           (:ref.test :i31)
                           (:if (:result :anyref))
                           (:local.get ,pair-val-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           (:local.get ,value-local)
                           (:ref.cast :i31)
                           :i31.get_s
                           :i32.eq
                           (:if (:result :anyref))
                           (:local.get ,pair-local)
                           (:br $rassoc_done)
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           (:ref.null :none)
                           :end
                           :else
                           ;; ref.eq (cast to eq type first)
                           (:local.get ,pair-val-local)
                           (:ref.cast :eq)
                           (:local.get ,value-local)
                           (:ref.cast :eq)
                           :ref.eq
                           (:if (:result :anyref))
                           (:local.get ,pair-local)
                           (:br $rassoc_done)
                           :else
                           (:ref.null :none)
                           :end
                           :end
                           :drop
                           ;; Advance
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $rassoc_loop)
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

;;; --- Tier 4: Quantifier Predicates ---

(defun compile-every (args env)
  "Compile (every pred list) - true if all elements satisfy predicate.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "every requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "EVERY-PRED")))
        (list-local (env-add-local env (gensym "EVERY-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check loop - return NIL if any fails, T if all pass
    (setf result (append result
                         `((:block $every_done (:result :anyref))
                           (:loop $every_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Empty/exhausted - return T
                           (:i32.const 1)
                           :ref.i31
                           (:br $every_done)
                           :else
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Predicate returned NIL - return NIL
                           (:ref.null :none)
                           (:br $every_done)
                           :else
                           ;; Continue
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $every_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-some (args env)
  "Compile (some pred list) - return first non-nil result.
   Stack: [] -> [value or NIL]"
  (when (/= (length args) 2)
    (error "some requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "SOME-PRED")))
        (list-local (env-add-local env (gensym "SOME-LIST")))
        (pred-result-local (env-add-local env (gensym "SOME-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check loop
    (setf result (append result
                         `((:block $some_done (:result :anyref))
                           (:loop $some_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Exhausted - return NIL
                           (:ref.null :none)
                           (:br $some_done)
                           :else
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           (:local.tee ,pred-result-local)
                           :ref.is_null
                           :i32.eqz
                           (:if (:result :anyref))
                           ;; Found - return result
                           (:local.get ,pred-result-local)
                           (:br $some_done)
                           :else
                           ;; Continue
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $some_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-notany (args env)
  "Compile (notany pred list) - true if no element satisfies predicate.
   Equivalent to (not (some pred list)).
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "notany requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "NOTANY-PRED")))
        (list-local (env-add-local env (gensym "NOTANY-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check loop
    (setf result (append result
                         `((:block $notany_done (:result :anyref))
                           (:loop $notany_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Exhausted - return T (none matched)
                           (:i32.const 1)
                           :ref.i31
                           (:br $notany_done)
                           :else
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           :i32.eqz
                           (:if (:result :anyref))
                           ;; Found match - return NIL
                           (:ref.null :none)
                           (:br $notany_done)
                           :else
                           ;; Continue
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $notany_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

(defun compile-notevery (args env)
  "Compile (notevery pred list) - true if some element fails predicate.
   Equivalent to (not (every pred list)).
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "notevery requires exactly 2 arguments"))
  (let ((result '())
        (pred-local (env-add-local env (gensym "NOTEV-PRED")))
        (list-local (env-add-local env (gensym "NOTEV-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    ;; Compile predicate
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result (list (list :local.set pred-local))))
    ;; Compile list
    (setf result (append result (compile-to-instructions (second args) env)))
    (setf result (append result (list (list :local.set list-local))))
    ;; Check loop
    (setf result (append result
                         `((:block $notev_done (:result :anyref))
                           (:loop $notev_loop (:result :anyref))
                           (:local.get ,list-local)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Exhausted - all passed, return NIL
                           (:ref.null :none)
                           (:br $notev_done)
                           :else
                           ;; Apply predicate
                           (:local.get ,pred-local)
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 0)
                           (:local.get ,pred-local)
                           (:ref.cast ,closure-type)
                           (:struct.get ,closure-type 1)
                           (:ref.cast ,func-type)
                           (:call_ref ,func-type)
                           :ref.is_null
                           (:if (:result :anyref))
                           ;; Predicate failed - return T
                           (:i32.const 1)
                           :ref.i31
                           (:br $notev_done)
                           :else
                           ;; Continue
                           (:local.get ,list-local)
                           (:ref.cast ,(list :ref cons-type))
                           (:struct.get ,cons-type 1)
                           (:local.set ,list-local)
                           (:ref.null :none)
                           (:br $notev_loop)
                           :end
                           :end
                           :end  ; loop
                           :end))) ; block
    result))

;;; ============================================================
;;; Character Functions (008-character-string)
;;; ============================================================

(defun compile-char-code (args env)
  "Compile (char-code char) - return Unicode codepoint.
   Characters are stored as i31ref with the codepoint value.
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "char-code requires exactly 1 argument"))
  ;; Character is already stored as i31ref with codepoint
  ;; Just return it as-is (it's the same representation as fixnum)
  (compile-to-instructions (first args) env))

(defun compile-code-char (args env)
  "Compile (code-char code) - return character for codepoint.
   Returns NIL for invalid codepoints (negative or > #x10FFFF or surrogates).
   Stack: [] -> [character or NIL]"
  (when (/= (length args) 1)
    (error "code-char requires exactly 1 argument"))
  (let ((result '())
        (code-local (env-add-local env (gensym "CODE") :i32)))
    ;; Compile code argument and save as i32
    (setf result (append result (compile-to-instructions (first args) env)))
    (setf result (append result
                         `((:ref.cast :i31)
                           :i31.get_s
                           (:local.set ,code-local)
                           ;; Check validity: 0 <= code <= #x10FFFF and not surrogate
                           (:block $code_char_done (:result :anyref))
                           ;; Check < 0 (invalid)
                           (:local.get ,code-local)
                           (:i32.const 0)
                           :i32.lt_s
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $code_char_done)
                           :else
                           ;; Check > #x10FFFF (invalid)
                           (:local.get ,code-local)
                           (:i32.const #x10FFFF)
                           :i32.gt_s
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $code_char_done)
                           :else
                           ;; Check surrogate range #xD800-#xDFFF (invalid)
                           (:local.get ,code-local)
                           (:i32.const #xD800)
                           :i32.ge_s
                           (:local.get ,code-local)
                           (:i32.const #xDFFF)
                           :i32.le_s
                           :i32.mul  ;; AND of both conditions
                           (:if (:result :anyref))
                           (:ref.null :none)
                           (:br $code_char_done)
                           :else
                           ;; Valid - return as character (i31ref)
                           (:local.get ,code-local)
                           :ref.i31
                           :end
                           :end
                           :end
                           :end)))
    result))

(defun compile-char= (args env)
  "Compile (char= char1 char2) - character equality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.eq
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char/= (args env)
  "Compile (char/= char1 char2) - character inequality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char/= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.ne
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char< (args env)
  "Compile (char< char1 char2) - character less than.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char< requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.lt_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char> (args env)
  "Compile (char> char1 char2) - character greater than.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char> requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.gt_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char<= (args env)
  "Compile (char<= char1 char2) - character less than or equal.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char<= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.le_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char>= (args env)
  "Compile (char>= char1 char2) - character greater than or equal.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char>= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.ge_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char-to-lower (args env local)
  "Helper: compile code to convert character to lowercase.
   Stores result in local. Returns instruction list.
   Input: character codepoint in local.
   Result: lowercase codepoint in local."
  (declare (ignore args env))
  `(;; If 'A' <= c <= 'Z', add 32
    (:local.get ,local)
    (:i32.const 65)  ;; 'A'
    :i32.ge_s
    (:local.get ,local)
    (:i32.const 90)  ;; 'Z'
    :i32.le_s
    :i32.mul  ;; AND
    (:if (:result :i32))
    (:local.get ,local)
    (:i32.const 32)
    :i32.add
    :else
    (:local.get ,local)
    :end))

(defun compile-char-equal (args env)
  "Compile (char-equal char1 char2) - case-insensitive character equality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-equal requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     ;; Get first char, convert to lowercase
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     ;; Get second char, convert to lowercase
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       ;; Compare
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-lessp (args env)
  "Compile (char-lessp char1 char2) - case-insensitive char<.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-lessp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-greaterp (args env)
  "Compile (char-greaterp char1 char2) - case-insensitive char>.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-greaterp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.gt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-not-lessp (args env)
  "Compile (char-not-lessp char1 char2) - case-insensitive char>=.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-not-lessp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.ge_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-not-greaterp (args env)
  "Compile (char-not-greaterp char1 char2) - case-insensitive char<=.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-not-greaterp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.le_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-upcase (args env)
  "Compile (char-upcase char) - convert to uppercase.
   Stack: [] -> [character]"
  (when (/= (length args) 1)
    (error "char-upcase requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       ;; If 'a' <= c <= 'z', subtract 32
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.mul  ;; AND - result is 0 or 1 (i32)
       (:if (:result :anyref))
       (:local.get ,c-local)
       (:i32.const 32)
       :i32.sub
       :ref.i31
       :else
       (:local.get ,c-local)
       :ref.i31
       :end))))

(defun compile-char-downcase (args env)
  "Compile (char-downcase char) - convert to lowercase.
   Stack: [] -> [character]"
  (when (/= (length args) 1)
    (error "char-downcase requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       ;; If 'A' <= c <= 'Z', add 32
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.mul  ;; AND - result is 0 or 1 (i32)
       (:if (:result :anyref))
       (:local.get ,c-local)
       (:i32.const 32)
       :i32.add
       :ref.i31
       :else
       (:local.get ,c-local)
       :ref.i31
       :end))))

(defun compile-characterp (args env)
  "Compile (characterp obj) - check if object is a character.
   Since characters and fixnums share i31ref representation,
   we can't distinguish them at runtime without type tags.
   For MVP, returns T for any i31ref (which includes both chars and fixnums).
   TODO: Add proper type tagging to distinguish chars from fixnums.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "characterp requires exactly 1 argument"))
  ;; Test if value is a valid i31ref (not null and ref.test succeeds)
  ;; Note: This will also return T for fixnums since they share i31ref representation
  (append
   (compile-to-instructions (first args) env)
   '((:ref.test :i31)
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31  ;; T (represented as fixnum 1)
     :else
     (:ref.null :none)  ;; NIL
     :end)))

(defun compile-alpha-char-p (args env)
  "Compile (alpha-char-p char) - test if alphabetic.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "alpha-char-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $alpha_done (:result :anyref))
       ;; Check uppercase A-Z
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alpha_done)
       :else
       ;; Check lowercase a-z
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-digit-char-p (args env)
  "Compile (digit-char-p char [radix]) - test if digit, return weight or NIL.
   Default radix is 10.
   Stack: [] -> [fixnum or NIL]"
  (when (or (< (length args) 1) (> (length args) 2))
    (error "digit-char-p requires 1 or 2 arguments"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32))
        (radix-local (env-add-local env (gensym "RADIX") :i32))
        (digit-local (env-add-local env (gensym "DIGIT") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local))
     ;; Compile radix (default 10)
     (if (second args)
         (append (compile-to-instructions (second args) env)
                 `((:ref.cast :i31) :i31.get_s (:local.set ,radix-local)))
         `((:i32.const 10) (:local.set ,radix-local)))
     `((:block $digit_done (:result :anyref))
       ;; Check '0'-'9' first
       (:local.get ,c-local)
       (:i32.const 48)  ;; '0'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 57)  ;; '9'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; It's a decimal digit
       (:local.get ,c-local)
       (:i32.const 48)
       :i32.sub
       (:local.set ,digit-local)
       ;; Check if digit < radix
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Check 'A'-'Z' for hex digits
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; Uppercase hex: A=10, B=11, ...
       (:local.get ,c-local)
       (:i32.const 55)  ;; 'A' - 10
       :i32.sub
       (:local.set ,digit-local)
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Check 'a'-'z' for hex digits
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; Lowercase hex: a=10, b=11, ...
       (:local.get ,c-local)
       (:i32.const 87)  ;; 'a' - 10
       :i32.sub
       (:local.set ,digit-local)
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Not a digit
       (:ref.null :none)
       :end
       :end
       :end
       :end))))

(defun compile-alphanumericp (args env)
  "Compile (alphanumericp char) - test if alphanumeric.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "alphanumericp requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $alphanum_done (:result :anyref))
       ;; Check uppercase A-Z
       (:local.get ,c-local)
       (:i32.const 65)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alphanum_done)
       :else
       ;; Check lowercase a-z
       (:local.get ,c-local)
       (:i32.const 97)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alphanum_done)
       :else
       ;; Check digits 0-9
       (:local.get ,c-local)
       (:i32.const 48)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 57)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end
       :end))))

(defun compile-upper-case-p (args env)
  "Compile (upper-case-p char) - test if uppercase letter.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "upper-case-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-lower-case-p (args env)
  "Compile (lower-case-p char) - test if lowercase letter.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "lower-case-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-stringp (args env)
  "Compile (stringp obj) - test if object is a string.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "stringp requires exactly 1 argument"))
  (let ((string-type clysm/compiler/codegen/gc-types:+type-string+))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.test ,(list :ref string-type))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-string-char (args env)
  "Compile (char string index) or (schar string index) - get character at index.
   For UTF-8 strings, we need to iterate to find the byte offset.
   Stack: [] -> [character]"
  (when (/= (length args) 2)
    (error "char/schar requires exactly 2 arguments"))
  (let ((str-local (env-add-local env (gensym "STR")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-idx-local (env-add-local env (gensym "BYTE-IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHAR-COUNT") :i32))
        (byte-len-local (env-add-local env (gensym "BYTE-LEN") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile string and index
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str-local)
      ,@(compile-to-instructions (second args) env)
      ;; Extract index from i31ref fixnum
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,idx-local)
      ;; Get byte length
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,byte-len-local)
      ;; Initialize: byte-idx=0, char-count=0
      (:i32.const 0)
      (:local.set ,byte-idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Loop to find the byte position of the idx-th character
      ;; We look for the start of each character and check if it's the one we want
      (:block $find_done)
      (:loop $find_loop)
      ;; Check if byte-idx >= byte-len (out of bounds)
      (:local.get ,byte-idx-local)
      (:local.get ,byte-len-local)
      :i32.ge_u
      (:br_if $find_done)
      ;; Get byte at current position
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if this byte is a leading byte (not a continuation byte)
      ;; Continuation bytes: 10xxxxxx (0x80-0xBF)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      ;; This is a leading byte (start of a character)
      ;; Check if char-count == idx (this is the character we want)
      (:local.get ,char-count-local)
      (:local.get ,idx-local)
      :i32.eq
      (:br_if $find_done)
      ;; Not the one we want, increment char count and continue
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Increment byte-idx
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,byte-idx-local)
      (:br $find_loop)
      :end  ; loop
      :end  ; block
      ;; Now byte-idx points to the start of the idx-th character
      ;; Decode the UTF-8 character at this position
      ;; Get the first byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Determine character length and decode
      ;; ASCII: 0xxxxxxx (0x00-0x7F) -> 1 byte
      (:local.get ,byte-local)
      (:i32.const #x80)
      :i32.lt_u
      (:if (:result :i32))
      ;; ASCII byte, codepoint = byte
      (:local.get ,byte-local)
      :else
      ;; Multi-byte sequence
      ;; 2-byte: 110xxxxx (0xC0-0xDF)
      (:local.get ,byte-local)
      (:i32.const #xE0)
      :i32.lt_u
      (:if (:result :i32))
      ;; 2-byte sequence: 110xxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x1F)  ; mask lower 5 bits
      :i32.and
      (:i32.const 6)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)  ; mask lower 6 bits
      :i32.and
      :i32.or
      :else
      ;; 3-byte: 1110xxxx (0xE0-0xEF)
      (:local.get ,byte-local)
      (:i32.const #xF0)
      :i32.lt_u
      (:if (:result :i32))
      ;; 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x0F)  ; mask lower 4 bits
      :i32.and
      (:i32.const 12)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 6)
      :i32.shl
      :i32.or
      ;; Get third byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 2)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      :i32.or
      :else
      ;; 4-byte: 11110xxx (0xF0-0xF7)
      ;; 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x07)  ; mask lower 3 bits
      :i32.and
      (:i32.const 18)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 12)
      :i32.shl
      :i32.or
      ;; Get third byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 2)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 6)
      :i32.shl
      :i32.or
      ;; Get fourth byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 3)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      :i32.or
      :end  ; close 3-byte vs 4-byte if
      :end  ; close 2-byte vs 3-byte+ if
      :end  ; close ASCII vs multi-byte if
      ;; Result is the codepoint as i32, convert to character (i31ref)
      :ref.i31)))

;;; String Comparison Functions (008-character-string)
;;; For UTF-8 strings, byte-by-byte comparison is valid for equality
;;; and lexicographic ordering since UTF-8 preserves codepoint order.

(defun compile-string= (args env)
  "Compile (string= str1 str2) - test string equality.
   Returns T if strings are equal, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "string= requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; Assume equal (T), will set to NIL if we find difference
      (:i32.const 1) :ref.i31
      (:local.set ,result-local)
      ;; Initialize idx
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; First check if lengths differ
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      ;; Lengths differ, set result to NIL
      (:ref.null :none)
      (:local.set ,result-local)
      :else
      ;; Same length, compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= len (all bytes compared equal)
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:br_if $cmp_done)
      ;; Compare bytes at idx
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      :i32.ne
      (:if nil)
      ;; Bytes differ, set result to NIL and exit
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal, continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      :end  ; length check if
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string/= (args env)
  "Compile (string/= str1 str2) - test string inequality.
   Returns the index of first differing character, or NIL if equal.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string/= requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (min-len-local (env-add-local env (gensym "MINLEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; min-len = min(len1, len2)
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.lt_u
      (:if (:result :i32))
      (:local.get ,len1-local)
      :else
      (:local.get ,len2-local)
      :end
      (:local.set ,min-len-local)
      ;; Initialize: assume equal (NIL result), idx=0, char-count=0
      (:ref.null :none)
      (:local.set ,result-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= min-len
      (:local.get ,idx-local)
      (:local.get ,min-len-local)
      :i32.ge_u
      (:if nil)
      ;; Reached end of shorter string
      ;; If lengths differ, set result to char-count
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end
      (:br $cmp_done)
      :end
      ;; Get byte from str1
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Compare bytes
      (:local.get ,byte-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      :i32.ne
      (:if nil)
      ;; Bytes differ, set result to char-count and exit
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal - if leading byte, increment char-count
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string< (args env)
  "Compile (string< str1 str2) - test if str1 < str2 lexicographically.
   Returns index of first difference if str1 < str2, NIL otherwise.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :lt))

(defun compile-string> (args env)
  "Compile (string> str1 str2) - test if str1 > str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :gt))

(defun compile-string<= (args env)
  "Compile (string<= str1 str2) - test if str1 <= str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :le))

(defun compile-string>= (args env)
  "Compile (string>= str1 str2) - test if str1 >= str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :ge))

;;; ============================================================
;;; Case-insensitive string comparison functions
;;; ============================================================

(defun compile-string-equal (args env)
  "Compile (string-equal str1 str2) - case-insensitive string equality.
   Returns T if strings are equal ignoring case, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "string-equal requires exactly 2 arguments"))
  (compile-string-compare-ci args env :equal))

(defun compile-string-lessp (args env)
  "Compile (string-lessp str1 str2) - case-insensitive string less-than.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-lessp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :lt))

(defun compile-string-greaterp (args env)
  "Compile (string-greaterp str1 str2) - case-insensitive string greater-than.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-greaterp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :gt))

(defun compile-string-not-lessp (args env)
  "Compile (string-not-lessp str1 str2) - case-insensitive string >=.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-lessp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :ge))

(defun compile-string-not-greaterp (args env)
  "Compile (string-not-greaterp str1 str2) - case-insensitive string <=.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-greaterp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :le))

(defun compile-string-not-equal (args env)
  "Compile (string-not-equal str1 str2) - case-insensitive string inequality.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-equal requires exactly 2 arguments"))
  (compile-string-compare-ci args env :not-equal))

(defun compile-upcase-byte (byte-local temp-local)
  "Generate instructions to convert a byte in byte-local to uppercase.
   Modifies byte-local in place. Uses temp-local as scratch.
   Stack: [] -> []"
  `(;; Check if byte is lowercase a-z (97-122)
    (:local.get ,byte-local)
    (:i32.const 97)  ;; 'a'
    :i32.ge_u
    (:local.get ,byte-local)
    (:i32.const 122) ;; 'z'
    :i32.le_u
    :i32.and
    (:if nil)
    ;; Convert to uppercase by subtracting 32
    (:local.get ,byte-local)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte-local)
    :end))

(defun compile-string-compare-ci (args env comparison)
  "Case-insensitive string comparison.
   COMPARISON is one of :equal, :not-equal, :lt, :gt, :le, :ge"
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte1-local (env-add-local env (gensym "BYTE1") :i32))
        (byte2-local (env-add-local env (gensym "BYTE2") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths (byte lengths)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; Initialize
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      (:ref.null :none)  ;; Default result is NIL
      (:local.set ,result-local)
      ;; Compare byte by byte with case folding
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if we've reached the end of str1
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:if nil)
      ;; str1 ended - check str2
      (:local.get ,idx-local)
      (:local.get ,len2-local)
      :i32.ge_u
      (:if nil)
      ;; Both ended at same length - strings are equal
      ,@(compile-string-compare-ci-at-end-equal comparison char-count-local result-local)
      :else
      ;; str1 ended but str2 continues - str1 < str2
      ,@(compile-string-compare-ci-at-end-prefix comparison :str1-shorter char-count-local result-local)
      :end
      (:br $cmp_done)
      :end
      ;; str1 not ended - check if str2 ended
      (:local.get ,idx-local)
      (:local.get ,len2-local)
      :i32.ge_u
      (:if nil)
      ;; str2 ended but str1 continues - str1 > str2
      ,@(compile-string-compare-ci-at-end-prefix comparison :str2-shorter char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Both have more bytes - get bytes and convert to uppercase
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Convert both to uppercase for comparison
      ,@(compile-upcase-byte byte1-local byte2-local)  ;; uses byte2-local as temp (we'll restore it)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ,@(compile-upcase-byte byte2-local byte1-local)
      ;; Get byte1 again (may have been modified as temp)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      ,@(compile-upcase-byte byte1-local byte2-local)
      ;; Compare uppercased bytes
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      ;; Bytes differ - determine result
      ,@(compile-string-compare-ci-at-diff comparison byte1-local byte2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Count leading byte for character count
      ;; Leading bytes have top 2 bits != 10 (i.e., not continuation byte)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:i32.const 192)  ;; 0xC0
      :i32.and
      (:i32.const 128)  ;; 0x80
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Next byte
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      (:local.get ,result-local))))

(defun compile-string-compare-ci-at-end-equal (comparison char-count-local result-local)
  "Generate code for when both strings ended at the same point (equal)."
  (case comparison
    (:equal
     ;; Strings are equal - return T
     `((:i32.const 1) :ref.i31 (:local.set ,result-local)))
    (:not-equal
     ;; Strings are equal - return NIL (already set)
     nil)
    ((:lt :gt)
     ;; Strings are equal - return NIL for < and > (already set)
     nil)
    ((:le :ge)
     ;; Strings are equal - return char-count for <= and >=
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))))

(defun compile-string-compare-ci-at-end-prefix (comparison which-shorter char-count-local result-local)
  "Generate code when one string is a prefix of the other."
  (case comparison
    (:equal
     ;; Not equal - return NIL (already set)
     nil)
    (:not-equal
     ;; Not equal - return char-count
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))
    (:lt
     ;; str1 < str2 iff str1 is shorter
     (if (eq which-shorter :str1-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:gt
     ;; str1 > str2 iff str2 is shorter
     (if (eq which-shorter :str2-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:le
     ;; str1 <= str2 iff str1 is shorter or equal (str1 shorter means true)
     (if (eq which-shorter :str1-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:ge
     ;; str1 >= str2 iff str2 is shorter or equal (str2 shorter means true)
     (if (eq which-shorter :str2-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))))

(defun compile-string-compare-ci-at-diff (comparison byte1-local byte2-local char-count-local result-local)
  "Generate code for when bytes differ at comparison point."
  (case comparison
    (:equal
     ;; Not equal - return NIL (already set)
     nil)
    (:not-equal
     ;; Not equal - return char-count
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))
    (:lt
     ;; str1 < str2 iff byte1 < byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:gt
     ;; str1 > str2 iff byte1 > byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:le
     ;; str1 <= str2 iff byte1 <= byte2 (since they differ, this means byte1 < byte2)
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:ge
     ;; str1 >= str2 iff byte1 >= byte2 (since they differ, this means byte1 > byte2)
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))))

(defun compile-string-compare (args env comparison)
  "Generic string comparison for <, >, <=, >=.
   COMPARISON is one of :lt, :gt, :le, :ge"
  (when (/= (length args) 2)
    (error "String comparison requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (min-len-local (env-add-local env (gensym "MINLEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (byte1-local (env-add-local env (gensym "BYTE1") :i32))
        (byte2-local (env-add-local env (gensym "BYTE2") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; min-len = min(len1, len2)
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.lt_u
      (:if (:result :i32))
      (:local.get ,len1-local)
      :else
      (:local.get ,len2-local)
      :end
      (:local.set ,min-len-local)
      ;; Initialize: assume false (NIL), idx=0, char-count=0
      (:ref.null :none)
      (:local.set ,result-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= min-len
      (:local.get ,idx-local)
      (:local.get ,min-len-local)
      :i32.ge_u
      (:if nil)
      ;; Reached end of shorter string
      ;; Result depends on comparison type and relative lengths
      ,@(compile-string-compare-at-end-v2 comparison len1-local len2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Get bytes from both strings
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Compare bytes
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      ;; Bytes differ - determine result based on comparison
      ,@(compile-string-compare-at-diff-v2 comparison byte1-local byte2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal - if leading byte, increment char-count
      (:local.get ,byte1-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string-compare-at-end-v2 (comparison len1-local len2-local char-count-local result-local)
  "Generate code for string comparison result when one string is a prefix of the other.
   Sets result-local to char-count (as i31) if condition is true, leaves it as NIL otherwise."
  (let ((condition-instr (case comparison
                           (:lt :i32.lt_u)  ; str1 < str2 iff len1 < len2
                           (:gt :i32.gt_u)  ; str1 > str2 iff len1 > len2
                           (:le :i32.le_u)  ; str1 <= str2 iff len1 <= len2
                           (:ge :i32.ge_u)))) ; str1 >= str2 iff len1 >= len2
    `((:local.get ,len1-local)
      (:local.get ,len2-local)
      ,condition-instr
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end)))

(defun compile-string-compare-at-end (comparison len1-local len2-local char-count-local)
  "Generate code for string comparison result when one string is a prefix of the other."
  (case comparison
    (:lt
     ;; str1 < str2 iff len1 < len2 (str1 is proper prefix)
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.lt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31  ; return mismatch position
       :else
       (:ref.null :none)
       :end))
    (:gt
     ;; str1 > str2 iff len1 > len2 (str2 is proper prefix)
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.gt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:le
     ;; str1 <= str2 iff len1 <= len2
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.le_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:ge
     ;; str1 >= str2 iff len1 >= len2
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.ge_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-string-compare-at-diff-v2 (comparison byte1-local byte2-local char-count-local result-local)
  "Generate code for string comparison result when bytes differ.
   Sets result-local to char-count (as i31) if condition is true, leaves it as NIL otherwise."
  (let ((condition-instr (case comparison
                           (:lt :i32.lt_u)  ; str1 < str2 iff byte1 < byte2
                           (:gt :i32.gt_u)  ; str1 > str2 iff byte1 > byte2
                           (:le :i32.lt_u)  ; str1 <= str2 iff byte1 < byte2 (NOT <=, since bytes differ!)
                           (:ge :i32.gt_u)))) ; str1 >= str2 iff byte1 > byte2 (NOT >=, since bytes differ!)
    `((:local.get ,byte1-local)
      (:local.get ,byte2-local)
      ,condition-instr
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end)))

(defun compile-string-compare-at-diff (comparison byte1-local byte2-local char-count-local)
  "Generate code for string comparison result when bytes differ."
  (case comparison
    (:lt
     ;; str1 < str2 iff byte1 < byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:gt
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:le
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.le_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:ge
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.ge_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))))

;;; ============================================================
;;; String Generation/Conversion (008-character-string Phase 6)
;;; ============================================================

(defun extract-keyword-from-ast (ast)
  "Extract keyword symbol from AST node (AST-VAR-REF with keyword name)."
  (and (typep ast 'clysm/compiler/ast:ast-var-ref)
       (let ((name (clysm/compiler/ast:ast-var-ref-name ast)))
         (and (keywordp name) name))))

(defun compile-make-string (args env)
  "Compile (make-string size &key initial-element) - create new string.
   Stack: [] -> [string]
   Note: Currently only supports ASCII characters. Multi-byte UTF-8 will be added later."
  (when (< (length args) 1)
    (error "make-string requires at least 1 argument (size)"))
  (let* ((size-arg (first args))
         (rest-args (rest args))
         ;; Parse keyword arguments
         (initial-element nil))
    ;; Look for :initial-element keyword
    (loop while rest-args do
      (let* ((key-ast (first rest-args))
             (key (extract-keyword-from-ast key-ast))
             (val (second rest-args)))
        (cond
          ((eq key :initial-element)
           (setf initial-element val))
          (t (error "Unknown keyword argument to make-string: ~A" key-ast)))
        (setf rest-args (cddr rest-args))))
    ;; Generate code - simple version that assumes ASCII (1 byte per char)
    (let ((size-local (env-add-local env (gensym "SIZE") :i32))
          (char-local (env-add-local env (gensym "CHAR") :i32))
          (str-local (env-add-local env (gensym "STR")))
          (idx-local (env-add-local env (gensym "IDX") :i32))
          (string-type clysm/compiler/codegen/gc-types:+type-string+))
      `(;; Get size
        ,@(compile-to-instructions size-arg env)
        (:ref.cast :i31)
        :i31.get_s
        (:local.set ,size-local)
        ;; Get initial char code (default #\Null = 0)
        ,@(if initial-element
              `(,@(compile-to-instructions initial-element env)
                (:ref.cast :i31)
                :i31.get_s
                (:local.set ,char-local))
              `((:i32.const 0)
                (:local.set ,char-local)))
        ;; Create array with size bytes (assuming ASCII)
        (:local.get ,size-local)
        (:array.new_default ,string-type)
        (:local.set ,str-local)
        ;; Fill array with initial character
        (:i32.const 0)
        (:local.set ,idx-local)
        ;; Loop to fill
        (:block $ms_done)
        (:loop $ms_loop)
        ;; Check if done
        (:local.get ,idx-local)
        (:local.get ,size-local)
        :i32.ge_u
        (:br_if $ms_done)
        ;; Store char byte
        (:local.get ,str-local)
        (:ref.cast ,(list :ref string-type))
        (:local.get ,idx-local)
        (:local.get ,char-local)
        (:array.set ,string-type)
        ;; Increment idx
        (:local.get ,idx-local)
        (:i32.const 1)
        :i32.add
        (:local.set ,idx-local)
        (:br $ms_loop)
        :end  ; loop
        :end  ; block
        ;; Return string
        (:local.get ,str-local)))))

(defun compile-string (args env)
  "Compile (string x) - convert character or symbol to string.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string requires exactly 1 argument"))
  (let ((str-local (env-add-local env (gensym "STR")))
        (char-local (env-add-local env (gensym "CHAR") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    ;; If arg is a character, create a 1-char string
    ;; If arg is already a string, return it
    ;; TODO: Handle symbols (get their print name)
    `(,@(compile-to-instructions (first args) env)
      (:local.set ,str-local)
      ;; Check if already a string
      (:local.get ,str-local)
      (:ref.test ,(list :ref string-type))
      (:if (:result :anyref))
      ;; Already a string - return as-is
      (:local.get ,str-local)
      :else
      ;; Assume it's a character - convert to 1-char string
      (:local.get ,str-local)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,char-local)
      ;; Create 1-element array
      (:i32.const 1)
      (:array.new_default ,string-type)
      (:local.set ,str-local)
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:i32.const 0)
      (:local.get ,char-local)
      (:array.set ,string-type)
      (:local.get ,str-local)
      :end)))

(defun compile-string-upcase (args env)
  "Compile (string-upcase string) - convert to uppercase.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-upcase requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy and convert
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $up_done)
      (:loop $up_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $up_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if lowercase (a-z = 97-122)
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if (:result :i32))
      ;; Convert to uppercase (subtract 32)
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.sub
      :else
      (:local.get ,byte-local)
      :end
      (:local.set ,byte-local)
      ;; Store in destination
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $up_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-string-downcase (args env)
  "Compile (string-downcase string) - convert to lowercase.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-downcase requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy and convert
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $down_done)
      (:loop $down_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $down_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if uppercase (A-Z = 65-90)
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:if (:result :i32))
      ;; Convert to lowercase (add 32)
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.add
      :else
      (:local.get ,byte-local)
      :end
      (:local.set ,byte-local)
      ;; Store in destination
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $down_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-string-capitalize (args env)
  "Compile (string-capitalize string) - capitalize first letter of each word.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-capitalize requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (word-start-local (env-add-local env (gensym "WORDSTART") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Initialize
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 1)  ; Start of string is word start
      (:local.set ,word-start-local)
      (:block $cap_done)
      (:loop $cap_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $cap_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if alphabetic
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      :i32.or
      (:if nil)
      ;; Is alphabetic
      ;; Check if word start
      (:local.get ,word-start-local)
      (:if nil)
      ;; Word start - uppercase
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      ;; Is lowercase, convert to uppercase
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte-local)
      :end
      :else
      ;; Not word start - lowercase
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:if nil)
      ;; Is uppercase, convert to lowercase
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.add
      (:local.set ,byte-local)
      :end
      :end
      ;; Clear word-start flag
      (:i32.const 0)
      (:local.set ,word-start-local)
      :else
      ;; Not alphabetic - set word-start flag
      (:i32.const 1)
      (:local.set ,word-start-local)
      :end
      ;; Store byte
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cap_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

;;; ============================================================
;;; Substring and Concatenation (008-character-string Phase 7)
;;; ============================================================

(defun compile-subseq (args env)
  "Compile (subseq string start &optional end) - get substring.
   Stack: [] -> [string]"
  (when (< (length args) 2)
    (error "subseq requires at least 2 arguments"))
  (let* ((string-arg (first args))
         (start-arg (second args))
         (end-arg (if (>= (length args) 3) (third args) nil))
         (src-local (env-add-local env (gensym "SRC")))
         (dst-local (env-add-local env (gensym "DST")))
         (start-local (env-add-local env (gensym "START") :i32))
         (end-local (env-add-local env (gensym "END") :i32))
         (byte-len-local (env-add-local env (gensym "BYTELEN") :i32))
         (src-byte-len-local (env-add-local env (gensym "SRCBYTELEN") :i32))
         (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
         (src-idx-local (env-add-local env (gensym "SRCIDX") :i32))
         (dst-idx-local (env-add-local env (gensym "DSTIDX") :i32))
         (byte-local (env-add-local env (gensym "BYTE") :i32))
         (start-byte-local (env-add-local env (gensym "STARTBYTE") :i32))
         (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions string-arg env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get start index
      ,@(compile-to-instructions start-arg env)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,start-local)
      ;; Get end index (or string length)
      ,@(if end-arg
            `(,@(compile-to-instructions end-arg env)
              (:ref.cast :i31)
              :i31.get_s
              (:local.set ,end-local))
            `(;; Calculate string char length for default end
              (:local.get ,src-local)
              (:ref.cast ,(list :ref string-type))
              (:array.len)
              (:local.set ,src-byte-len-local)
              ;; Count characters (UTF-8 aware)
              (:i32.const 0)
              (:local.set ,char-count-local)
              (:i32.const 0)
              (:local.set ,src-idx-local)
              (:block $len_done)
              (:loop $len_loop)
              (:local.get ,src-idx-local)
              (:local.get ,src-byte-len-local)
              :i32.ge_u
              (:br_if $len_done)
              (:local.get ,src-local)
              (:ref.cast ,(list :ref string-type))
              (:local.get ,src-idx-local)
              (:array.get_u ,string-type)
              (:i32.const #xC0)
              :i32.and
              (:i32.const #x80)
              :i32.ne
              (:if nil)
              (:local.get ,char-count-local)
              (:i32.const 1)
              :i32.add
              (:local.set ,char-count-local)
              :end
              (:local.get ,src-idx-local)
              (:i32.const 1)
              :i32.add
              (:local.set ,src-idx-local)
              (:br $len_loop)
              :end
              :end
              (:local.get ,char-count-local)
              (:local.set ,end-local)))
      ;; Find byte position of start char
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,src-byte-len-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      (:i32.const 0)
      (:local.set ,src-idx-local)
      (:block $find_start_done)
      (:loop $find_start_loop)
      (:local.get ,src-idx-local)
      (:local.get ,src-byte-len-local)
      :i32.ge_u
      (:br_if $find_start_done)
      (:local.get ,char-count-local)
      (:local.get ,start-local)
      :i32.ge_u
      (:br_if $find_start_done)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,src-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      (:local.get ,src-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,src-idx-local)
      (:br $find_start_loop)
      :end
      :end
      (:local.get ,src-idx-local)
      (:local.set ,start-byte-local)
      ;; Find byte position of end char and calculate byte length
      (:block $find_end_done)
      (:loop $find_end_loop)
      (:local.get ,src-idx-local)
      (:local.get ,src-byte-len-local)
      :i32.ge_u
      (:br_if $find_end_done)
      (:local.get ,char-count-local)
      (:local.get ,end-local)
      :i32.ge_u
      (:br_if $find_end_done)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,src-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      (:local.get ,src-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,src-idx-local)
      (:br $find_end_loop)
      :end
      :end
      (:local.get ,src-idx-local)
      (:local.get ,start-byte-local)
      :i32.sub
      (:local.set ,byte-len-local)
      ;; Create destination array
      (:local.get ,byte-len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy bytes
      (:i32.const 0)
      (:local.set ,dst-idx-local)
      (:block $copy_done)
      (:loop $copy_loop)
      (:local.get ,dst-idx-local)
      (:local.get ,byte-len-local)
      :i32.ge_u
      (:br_if $copy_done)
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,dst-idx-local)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,start-byte-local)
      (:local.get ,dst-idx-local)
      :i32.add
      (:array.get_u ,string-type)
      (:array.set ,string-type)
      (:local.get ,dst-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,dst-idx-local)
      (:br $copy_loop)
      :end
      :end
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-concatenate (args env)
  "Compile (concatenate 'string str1 str2 ...) - concatenate strings.
   Stack: [] -> [string]"
  (when (< (length args) 1)
    (error "concatenate requires at least 1 argument (result type)"))
  ;; Check that first arg is 'string
  (let ((type-arg (first args))
        (string-args (rest args)))
    ;; For now, only support 'string result type
    ;; Skip type check at compile time - we'll just produce a string
    (if (null string-args)
        ;; No strings to concatenate - return empty string
        `((:i32.const 0)
          (:array.new_default ,clysm/compiler/codegen/gc-types:+type-string+))
        ;; Concatenate strings
        (let ((str-locals (loop for i from 0 below (length string-args)
                                collect (env-add-local env (gensym "STR"))))
              (len-local (env-add-local env (gensym "TOTALLEN") :i32))
              (result-local (env-add-local env (gensym "RESULT")))
              (dst-idx-local (env-add-local env (gensym "DSTIDX") :i32))
              (src-idx-local (env-add-local env (gensym "SRCIDX") :i32))
              (src-len-local (env-add-local env (gensym "SRCLEN") :i32))
              (string-type clysm/compiler/codegen/gc-types:+type-string+))
          `(;; Compile each string and store in locals
            ,@(loop for arg in string-args
                    for local in str-locals
                    append `(,@(compile-to-instructions arg env)
                             (:ref.cast ,(list :ref string-type))
                             (:local.set ,local)))
            ;; Calculate total length
            (:i32.const 0)
            (:local.set ,len-local)
            ,@(loop for local in str-locals
                    append `((:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:array.len)
                             (:local.get ,len-local)
                             :i32.add
                             (:local.set ,len-local)))
            ;; Create result array
            (:local.get ,len-local)
            (:array.new_default ,string-type)
            (:local.set ,result-local)
            ;; Copy each string
            (:i32.const 0)
            (:local.set ,dst-idx-local)
            ,@(loop for local in str-locals
                    append `(;; Get source length
                             (:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:array.len)
                             (:local.set ,src-len-local)
                             ;; Copy bytes from this string
                             (:i32.const 0)
                             (:local.set ,src-idx-local)
                             (:block $concat_copy_done)
                             (:loop $concat_copy_loop)
                             (:local.get ,src-idx-local)
                             (:local.get ,src-len-local)
                             :i32.ge_u
                             (:br_if $concat_copy_done)
                             (:local.get ,result-local)
                             (:ref.cast ,(list :ref string-type))
                             (:local.get ,dst-idx-local)
                             (:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:local.get ,src-idx-local)
                             (:array.get_u ,string-type)
                             (:array.set ,string-type)
                             (:local.get ,src-idx-local)
                             (:i32.const 1)
                             :i32.add
                             (:local.set ,src-idx-local)
                             (:local.get ,dst-idx-local)
                             (:i32.const 1)
                             :i32.add
                             (:local.set ,dst-idx-local)
                             (:br $concat_copy_loop)
                             :end
                             :end))
            ;; Return result
            (:local.get ,result-local))))))

;;; ============================================================
;;; Multiple Values Compilation (025-multiple-values)
;;; ============================================================

(defun compile-values (ast env)
  "Compile a values form to Wasm instructions.

   T015: (values) - Returns NIL with mv-count=0
   T016: (values x) - Returns x with mv-count=1
   T017: (values x y z...) - Returns x with y,z,... in mv-buffer, mv-count=n

   Global layout:
     Global 2 ($mv_count): i32 - number of values
     Global 3 ($mv_buffer): ref $mv_array - array holding secondary values

   Primary value stays on stack, secondary values go in mv-buffer."
  (let* ((forms (clysm/compiler/ast:ast-values-forms ast))
         (count (length forms))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22))  ; Type index for $mv_array
    (cond
      ;; T015: Zero values - return NIL, set count to 0
      ((zerop count)
       `((:i32.const 0)
         (:global.set ,mv-count-global)
         (:ref.null :none)))

      ;; T016: Single value - return it directly, set count to 1
      ((= count 1)
       `((:i32.const 1)
         (:global.set ,mv-count-global)
         ,@(compile-to-instructions (first forms) env)))

      ;; T017: Multiple values - first on stack, rest in mv-buffer
      (t
       (let ((first-local (env-add-local env (gensym "MV-FIRST"))))
         `(;; Set mv-count to number of values
           (:i32.const ,count)
           (:global.set ,mv-count-global)
           ;; Compile first value and save to local
           ,@(compile-to-instructions (first forms) env)
           (:local.set ,first-local)
           ;; Store secondary values (2nd, 3rd, ...) in mv-buffer
           ,@(loop for form in (rest forms)
                   for idx from 0
                   append `(;; Get mv-buffer
                            (:global.get ,mv-buffer-global)
                            ;; Buffer index
                            (:i32.const ,idx)
                            ;; Compile the value
                            ,@(compile-to-instructions form env)
                            ;; Store in array
                            (:array.set ,mv-array-type)))
           ;; Return first value
           (:local.get ,first-local)))))))

(defun compile-multiple-value-bind (ast env)
  "Compile a multiple-value-bind form to Wasm instructions.

   (multiple-value-bind (a b c) values-form body...)

   T024-T027: Binds variables from values returned by values-form.
   - First variable gets primary value (on stack)
   - Remaining variables get values from mv-buffer
   - Variables beyond mv-count get NIL

   Algorithm:
   1. Execute values-form (puts primary on stack, sets mv-count and mv-buffer)
   2. Bind first var to primary value
   3. For each remaining var, check mv-count and bind from mv-buffer or NIL
   4. Compile body in extended environment

   Note: env-add-local mutates env by adding bindings, so after calling it
   for all vars, env already contains all the bindings."
  (let* ((vars (clysm/compiler/ast:ast-mvb-vars ast))
         (values-form (clysm/compiler/ast:ast-mvb-values-form ast))
         (body (clysm/compiler/ast:ast-mvb-body ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (var-count (length vars)))
    (cond
      ;; No variables - just execute values-form (discarding values) and body
      ((zerop var-count)
       `(,@(compile-to-instructions values-form env)
         :drop
         ,@(compile-mvb-body body env)))

      ;; One variable - bind to primary value
      ((= var-count 1)
       (let* ((var (first vars))
              (local-idx (env-add-local env var)))
         `(;; Execute values form - puts primary value on stack
           ,@(compile-to-instructions values-form env)
           ;; Store in local
           (:local.set ,local-idx)
           ;; Compile body (env already has the binding from env-add-local)
           ,@(compile-mvb-body body env))))

      ;; Multiple variables
      (t
       (let* ((local-indices (loop for var in vars
                                   collect (env-add-local env var)))
              (first-local (first local-indices)))
         ;; After the loop, env already has all bindings from env-add-local
         `(;; Execute values form - puts primary value on stack
           ,@(compile-to-instructions values-form env)
           ;; Store first value in first local
           (:local.set ,first-local)
           ;; Bind remaining variables from mv-buffer
           ,@(loop for var in (rest vars)
                   for local in (rest local-indices)
                   for idx from 0
                   append `(;; Check if mv-count > (idx + 1)
                            (:global.get ,mv-count-global)
                            (:i32.const ,(1+ idx))
                            :i32.gt_s
                            (:if (:result :anyref))
                            ;; True: get from mv-buffer
                            (:global.get ,mv-buffer-global)
                            (:i32.const ,idx)
                            (:array.get ,mv-array-type)
                            :else
                            ;; False: use NIL
                            (:ref.null :none)
                            :end
                            ;; Store in local
                            (:local.set ,local)))
           ;; Compile body
           ,@(compile-mvb-body body env)))))))

(defun compile-mvb-body (body env)
  "Compile a list of body forms for MVB, returning value of last form."
  (if (null body)
      '((:ref.null :none))  ; Empty body returns NIL
      (loop for form in body
            for i from 0
            for is-last = (= i (1- (length body)))
            if is-last
              append (compile-to-instructions form env)
            else
              append `(,@(compile-to-instructions form env) :drop))))

;;; ============================================================
;;; multiple-value-list (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-list (ast env)
  "Compile (multiple-value-list form) to Wasm instructions.

   Collects all values from form into a proper list.
   Uses a loop to iterate over mv-buffer and build list in reverse,
   then cons primary value at front.

   Algorithm:
   1. Execute form, save primary value in local
   2. If mv-count = 0, return NIL
   3. Build list by consing values from mv-buffer[count-2] down to [0]
   4. Cons primary value onto front
   5. Return the list"
  (let* ((form (clysm/compiler/ast:ast-mvl-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (primary-local (env-add-local env (gensym "MVL-PRIMARY")))
         (count-local (env-add-local env (gensym "MVL-COUNT") :i32))
         (idx-local (env-add-local env (gensym "MVL-IDX") :i32))
         (result-local (env-add-local env (gensym "MVL-RESULT"))))
    `(;; Execute form and save primary value
      ,@(compile-to-instructions form env)
      (:local.set ,primary-local)
      ;; Get mv-count and save
      (:global.get ,mv-count-global)
      (:local.set ,count-local)
      ;; Check if count = 0 -> return NIL
      (:local.get ,count-local)
      (:i32.const 0)
      :i32.eq
      (:if (:result :anyref))
      (:ref.null :none)
      :else
      ;; Start with NIL as result
      (:ref.null :none)
      (:local.set ,result-local)
      ;; idx = count - 2 (start from last secondary value)
      (:local.get ,count-local)
      (:i32.const 2)
      :i32.sub
      (:local.set ,idx-local)
      ;; Loop while idx >= 0 (void block/loop)
      (:block)
      (:loop)
      ;; Check if idx < 0, exit if so
      (:local.get ,idx-local)
      (:i32.const 0)
      :i32.lt_s
      (:br_if 1)  ; Exit loop if idx < 0
      ;; Cons mv-buffer[idx] with result using struct.new
      (:global.get ,mv-buffer-global)
      (:local.get ,idx-local)
      (:array.get ,mv-array-type)
      (:local.get ,result-local)
      (:struct.new ,cons-type)
      (:local.set ,result-local)
      ;; idx = idx - 1
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.sub
      (:local.set ,idx-local)
      ;; Continue loop
      (:br 0)
      :end  ; end loop
      :end  ; end block
      ;; Now cons primary value onto front using struct.new
      (:local.get ,primary-local)
      (:local.get ,result-local)
      (:struct.new ,cons-type)
      :end)))

;;; ============================================================
;;; nth-value (025-multiple-values)
;;; ============================================================

(defun compile-nth-value (ast env)
  "Compile (nth-value n form) to Wasm instructions.

   Returns the nth value from form, or NIL if n >= mv-count.

   Algorithm:
   1. Evaluate index expression
   2. Execute form
   3. If index = 0, return primary value
   4. If index >= mv-count, return NIL
   5. Otherwise, return mv-buffer[index - 1]"
  (let* ((index-ast (clysm/compiler/ast:ast-nth-value-index ast))
         (form (clysm/compiler/ast:ast-nth-value-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (index-local (env-add-local env (gensym "NTH-IDX") :i32))
         (primary-local (env-add-local env (gensym "NTH-PRIMARY"))))
    `(;; Evaluate and save index
      ,@(compile-to-instructions index-ast env)
      ;; Cast to i31 and unwrap to i32
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,index-local)
      ;; Execute form
      ,@(compile-to-instructions form env)
      (:local.set ,primary-local)
      ;; Check if index = 0 -> return primary
      (:local.get ,index-local)
      (:i32.const 0)
      :i32.eq
      (:if (:result :anyref))
      (:local.get ,primary-local)
      :else
      ;; Check if index >= mv-count -> return NIL
      (:local.get ,index-local)
      (:global.get ,mv-count-global)
      :i32.ge_s
      (:if (:result :anyref))
      (:ref.null :none)
      :else
      ;; Return mv-buffer[index - 1]
      (:global.get ,mv-buffer-global)
      (:local.get ,index-local)
      (:i32.const 1)
      :i32.sub
      (:array.get ,mv-array-type)
      :end
      :end)))

;;; ============================================================
;;; values-list (025-multiple-values)
;;; ============================================================

(defun compile-values-list (ast env)
  "Compile (values-list list) to Wasm instructions.

   Spreads a list as multiple values.

   Algorithm:
   1. If list is NIL, set mv-count=0, return NIL
   2. Otherwise, car is primary value, iterate cdr to fill mv-buffer
   3. Set mv-count = list length, return primary value"
  (let* ((form (clysm/compiler/ast:ast-values-list-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (list-local (env-add-local env (gensym "VL-LIST")))
         (count-local (env-add-local env (gensym "VL-COUNT") :i32))
         (idx-local (env-add-local env (gensym "VL-IDX") :i32))
         (current-local (env-add-local env (gensym "VL-CURRENT")))
         (primary-local (env-add-local env (gensym "VL-PRIMARY"))))
    `(;; Evaluate list expression
      ,@(compile-to-instructions form env)
      (:local.set ,list-local)
      ;; Check if nil
      (:local.get ,list-local)
      :ref.is_null
      (:if (:result :anyref))
      ;; NIL case: mv-count=0, return NIL
      (:i32.const 0)
      (:global.set ,mv-count-global)
      (:ref.null :none)
      :else
      ;; Get car as primary using struct.get
      (:local.get ,list-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:local.set ,primary-local)
      ;; Start counting and iterating
      (:i32.const 1)
      (:local.set ,count-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; current = cdr(list) using struct.get
      (:local.get ,list-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,current-local)
      ;; Loop while current != nil (void block/loop)
      (:block)
      (:loop)
      ;; Check if current is nil
      (:local.get ,current-local)
      :ref.is_null
      (:br_if 1)  ; Exit if nil
      ;; Store car(current) in mv-buffer[idx]
      (:global.get ,mv-buffer-global)
      (:local.get ,idx-local)
      (:local.get ,current-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:array.set ,mv-array-type)
      ;; count++, idx++
      (:local.get ,count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,count-local)
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      ;; current = cdr(current) using struct.get
      (:local.get ,current-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,current-local)
      ;; Continue loop
      (:br 0)
      :end  ; end loop
      :end  ; end block
      ;; Set mv-count
      (:local.get ,count-local)
      (:global.set ,mv-count-global)
      ;; Return primary
      (:local.get ,primary-local)
      :end)))

;;; ============================================================
;;; multiple-value-prog1 (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-prog1 (ast env)
  "Compile (multiple-value-prog1 first-form body...) to Wasm instructions.

   Saves all values from first-form, executes body for side effects,
   then restores and returns the saved values.

   Algorithm:
   1. Execute first-form, save primary value and mv-count
   2. Copy mv-buffer contents to local array
   3. Execute body forms (drop results)
   4. Restore mv-count from local
   5. Copy local array back to mv-buffer
   6. Return saved primary value"
  (let* ((first-form (clysm/compiler/ast:ast-mvp1-first-form ast))
         (body (clysm/compiler/ast:ast-mvp1-body ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (primary-local (env-add-local env (gensym "MVP1-PRIMARY")))
         (count-local (env-add-local env (gensym "MVP1-COUNT") :i32)))
    `(;; Execute first form
      ,@(compile-to-instructions first-form env)
      (:local.set ,primary-local)
      ;; Save mv-count
      (:global.get ,mv-count-global)
      (:local.set ,count-local)
      ;; Execute body forms for side effects
      ,@(loop for form in body
              append `(,@(compile-to-instructions form env) :drop))
      ;; Restore mv-count
      (:local.get ,count-local)
      (:global.set ,mv-count-global)
      ;; Return primary value (note: secondary values in mv-buffer may be stale,
      ;; but for simple cases this is acceptable - full preservation would need
      ;; a local array copy which is complex in WasmGC)
      (:local.get ,primary-local))))

;;; ============================================================
;;; multiple-value-call (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-call (ast env)
  "Compile (multiple-value-call fn form1 form2 ...) to Wasm instructions.

   NOTE: This is a simplified implementation that only works with + and
   a single form for now. Full dynamic apply is complex and deferred.

   For (multiple-value-call #'+ (values 1 2 3)):
   - Collects all values and sums them using a loop

   TODO: Full implementation requires dynamic function application."
  (let* ((function-ast (clysm/compiler/ast:ast-mvc-function ast))
         (forms (clysm/compiler/ast:ast-mvc-forms ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 22)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (sum-local (env-add-local env (gensym "MVC-SUM") :i32))
         (idx-local (env-add-local env (gensym "MVC-IDX") :i32))
         (count-local (env-add-local env (gensym "MVC-COUNT") :i32)))
    ;; For now, only handle single-form case with + function
    ;; This computes sum of all values
    (when (and (= 1 (length forms))
               (typep function-ast 'clysm/compiler/ast:ast-call)
               (eq 'function (clysm/compiler/ast:ast-call-function function-ast)))
      `(;; Execute the form
        ,@(compile-to-instructions (first forms) env)
        ;; Cast to i31 and unwrap to i32 for sum
        (:ref.cast :i31)
        :i31.get_s
        (:local.set ,sum-local)
        ;; Get count
        (:global.get ,mv-count-global)
        (:local.set ,count-local)
        ;; Initialize idx = 0
        (:i32.const 0)
        (:local.set ,idx-local)
        ;; Loop to add secondary values (void block/loop)
        (:block)
        (:loop)
        ;; Check if idx >= count - 1
        (:local.get ,idx-local)
        (:local.get ,count-local)
        (:i32.const 1)
        :i32.sub
        :i32.ge_s
        (:br_if 1)
        ;; Add mv-buffer[idx] to sum
        (:global.get ,mv-buffer-global)
        (:local.get ,idx-local)
        (:array.get ,mv-array-type)
        (:ref.cast :i31)
        :i31.get_s
        (:local.get ,sum-local)
        :i32.add
        (:local.set ,sum-local)
        ;; idx++
        (:local.get ,idx-local)
        (:i32.const 1)
        :i32.add
        (:local.set ,idx-local)
        ;; Continue
        (:br 0)
        :end
        :end
        ;; Return sum as i31ref
        (:local.get ,sum-local)
        :ref.i31))))

;;; ============================================================
;;; CLOS: defclass Compilation (026-clos-foundation)
;;; ============================================================

;;; Class ID allocation counter for dispatch
(defvar *class-id-counter* 0
  "Counter for allocating unique class IDs.")

(defun reset-class-id-counter ()
  "Reset the class ID counter. Called when starting a new compilation."
  (setf *class-id-counter* 0))

(defun allocate-class-id ()
  "Allocate a new unique class ID."
  (prog1 *class-id-counter*
    (incf *class-id-counter*)))

;;; Class registry for compile-time class tracking
(defvar *class-registry* (make-hash-table :test 'eq)
  "Compile-time registry mapping class names to class info.")

(defstruct class-info
  "Compile-time information about a CLOS class."
  (name nil :type symbol)
  (superclass nil :type (or null symbol))
  (slots nil :type list)       ; List of slot-info structs
  (class-id nil :type (or null fixnum))
  (finalized-p nil :type boolean))

(defstruct slot-info
  "Compile-time information about a slot."
  (name nil :type symbol)
  (initarg nil :type (or null keyword))
  (accessor nil :type (or null symbol))
  (initform nil :type t)
  (initform-p nil :type boolean)
  (index nil :type (or null fixnum)))  ; Slot index in instance

(defun reset-class-registry ()
  "Reset the class registry. Called when starting a new compilation."
  (clrhash *class-registry*)
  (reset-class-id-counter))

(defun register-compile-time-class (name superclass slots)
  "Register a class at compile time.
   NAME is the class symbol.
   SUPERCLASS is the superclass name or NIL.
   SLOTS is a list of ast-slot-definition structs.

   Handles single inheritance:
   - Inherits slots from superclass (prepended to own slots)
   - Child slots with same name shadow parent slots
   - Recomputes slot indices"
  (let* ((class-id (allocate-class-id))
         ;; Get parent slots if superclass exists
         (parent-slots
           (when superclass
             (let ((parent-info (find-compile-time-class superclass)))
               (if parent-info
                   (class-info-slots parent-info)
                   (error "Superclass ~S not defined" superclass)))))
         ;; Get own slot names for shadowing check
         (own-slot-names
           (mapcar #'clysm/compiler/ast:ast-slot-definition-name slots))
         ;; Filter out shadowed parent slots
         (inherited-slots
           (remove-if (lambda (pslot)
                        (member (slot-info-name pslot) own-slot-names))
                      parent-slots))
         ;; Convert own slots to slot-info (index will be set later)
         (own-slot-infos
           (mapcar (lambda (slot)
                     (make-slot-info
                      :name (clysm/compiler/ast:ast-slot-definition-name slot)
                      :initarg (clysm/compiler/ast:ast-slot-definition-initarg slot)
                      :accessor (clysm/compiler/ast:ast-slot-definition-accessor slot)
                      :initform (clysm/compiler/ast:ast-slot-definition-initform slot)
                      :initform-p (clysm/compiler/ast:ast-slot-definition-initform-p slot)
                      :index 0))  ; Temporary, will be recomputed
                   slots))
         ;; Combine: inherited first, then own slots
         (all-slots (append inherited-slots own-slot-infos))
         ;; Recompute indices
         (slot-infos
           (loop for slot in all-slots
                 for index from 0
                 collect (make-slot-info
                          :name (slot-info-name slot)
                          :initarg (slot-info-initarg slot)
                          :accessor (slot-info-accessor slot)
                          :initform (slot-info-initform slot)
                          :initform-p (slot-info-initform-p slot)
                          :index index)))
         (info (make-class-info
                :name name
                :superclass superclass
                :slots slot-infos
                :class-id class-id
                :finalized-p t)))
    (setf (gethash name *class-registry*) info)
    info))

(defun find-compile-time-class (name)
  "Find a class in the compile-time registry."
  (gethash name *class-registry*))

(defun compile-defclass (ast env)
  "Compile a defclass form.
   Registers the class at compile time and returns NIL.
   The actual class global will be emitted in the global section.

   For now, this is a compile-time only operation - the class metadata
   is registered for use by make-instance and accessor codegen, but
   no runtime code is emitted in the expression position."
  (declare (ignore env))
  (let* ((name (clysm/compiler/ast:ast-defclass-name ast))
         (superclass (clysm/compiler/ast:ast-defclass-superclass ast))
         (slots (clysm/compiler/ast:ast-defclass-slots ast)))
    ;; Register the class at compile time
    (register-compile-time-class name superclass slots)
    ;; Also register with the existing CLOS infrastructure for interpreter compatibility
    ;; This uses the existing compile-time CLOS in clysm/clos/
    (let ((form `(defclass ,name ,(if superclass (list superclass) nil)
                   ,(mapcar (lambda (slot)
                              (let ((slot-name (clysm/compiler/ast:ast-slot-definition-name slot))
                                    (initarg (clysm/compiler/ast:ast-slot-definition-initarg slot))
                                    (accessor (clysm/compiler/ast:ast-slot-definition-accessor slot))
                                    (initform (clysm/compiler/ast:ast-slot-definition-initform slot))
                                    (initform-p (clysm/compiler/ast:ast-slot-definition-initform-p slot)))
                                (append (list slot-name)
                                        (when initarg (list :initarg initarg))
                                        (when accessor (list :accessor accessor))
                                        (when initform-p (list :initform initform)))))
                            slots))))
      (clysm/clos/defclass:define-class* form))
    ;; Return NIL (defclass returns class name, but as a quoted symbol we return NIL for now)
    ;; In a full implementation, this would return a reference to the class object
    (list '(:ref.null :none))))

;;; ============================================================
;;; CLOS: make-instance Compilation (026-clos-foundation)
;;; ============================================================

(defun compile-make-instance (ast env)
  "Compile a make-instance form.
   Creates an instance of the specified class with the given initargs.

   The compilation emits code that:
   1. Looks up the class (error if not found)
   2. Creates a slot vector with the correct size
   3. Fills slots based on initargs, initforms, or UNBOUND
   4. Creates the $instance struct

   For now, this is a placeholder that returns NIL.
   Full WasmGC codegen will be added in the next iteration."
  (declare (ignore env))
  (let* ((class-name (clysm/compiler/ast:ast-make-instance-class-name ast))
         (initargs (clysm/compiler/ast:ast-make-instance-initargs ast))
         (class-info (find-compile-time-class class-name)))
    ;; Validate class exists
    (unless class-info
      (error "Cannot make instance of undefined class: ~S" class-name))
    ;; For now, use the interpreter CLOS to create the instance
    ;; This is a compile-time evaluation - not ideal but works for MVP
    ;; In a full implementation, we would emit WasmGC instructions
    (let ((initarg-plist (loop for (key . val-ast) in initargs
                               collect key
                               collect (if (typep val-ast 'clysm/compiler/ast:ast-literal)
                                           (clysm/compiler/ast:ast-literal-value val-ast)
                                           0))))  ; Default to 0 for non-literals
      (declare (ignore initarg-plist))
      ;; For MVP, just return a null reference
      ;; TODO: Emit actual struct.new $instance instructions
      (list '(:ref.null :none)))))

;;; ============================================================
;;; CLOS: Generic Function Registry (026-clos-foundation)
;;; ============================================================

(defvar *generic-function-registry* (make-hash-table :test 'eq)
  "Compile-time registry mapping generic function names to GF info.")

(defstruct gf-info
  "Compile-time information about a generic function."
  (name nil :type symbol)
  (methods nil :type list)        ; List of method-info structs
  (lambda-list nil :type list))   ; Expected lambda-list

(defstruct method-info
  "Compile-time information about a method."
  (specializers nil :type list)   ; List of class names
  (qualifier nil :type (or null keyword))
  (lambda-list nil :type list)
  (body nil :type list))          ; Body as list of AST nodes

(defun reset-generic-function-registry ()
  "Reset the generic function registry."
  (clrhash *generic-function-registry*))

(defun find-generic-function (name)
  "Find a generic function in the compile-time registry."
  (gethash name *generic-function-registry*))

(defun register-generic-function (name)
  "Register a new generic function."
  (let ((gf (make-gf-info :name name)))
    (setf (gethash name *generic-function-registry*) gf)
    gf))

(defun ensure-compile-time-gf (name)
  "Ensure a compile-time generic function exists, creating if necessary."
  (or (find-generic-function name)
      (register-generic-function name)))

(defun add-method-to-gf (gf method-info)
  "Add a method to a generic function."
  (push method-info (gf-info-methods gf)))

;;; ============================================================
;;; CLOS: defmethod Compilation (026-clos-foundation)
;;; ============================================================

(defun compile-defmethod (ast env)
  "Compile a defmethod form.
   Registers the method with the generic function.
   Returns NIL for now (method registration is compile-time only)."
  (declare (ignore env))
  (let* ((name (clysm/compiler/ast:ast-defmethod-name ast))
         (qualifier (clysm/compiler/ast:ast-defmethod-qualifier ast))
         (specializers (clysm/compiler/ast:ast-defmethod-specializers ast))
         (lambda-list (clysm/compiler/ast:ast-defmethod-lambda-list ast))
         (body (clysm/compiler/ast:ast-defmethod-body ast)))
    ;; Ensure generic function exists
    (let ((gf (ensure-compile-time-gf name)))
      ;; Create method info
      (let ((method (make-method-info
                     :specializers specializers
                     :qualifier qualifier
                     :lambda-list lambda-list
                     :body body)))
        ;; Add method to GF
        (add-method-to-gf gf method)))
    ;; Also register with existing CLOS infrastructure
    (let ((form `(defmethod ,name ,@(when qualifier (list qualifier))
                   ,(mapcar (lambda (param spec)
                              (if (eq spec t)
                                  param
                                  (list param spec)))
                            lambda-list specializers)
                   ,@(mapcar (lambda (body-ast)
                               (if (typep body-ast 'clysm/compiler/ast:ast-literal)
                                   (clysm/compiler/ast:ast-literal-value body-ast)
                                   nil))  ; Simplified for now
                             body))))
      (clysm/clos/defmethod:define-method* form))
    ;; Return NIL
    (list '(:ref.null :none))))
