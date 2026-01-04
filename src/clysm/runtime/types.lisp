;;;; runtime/types.lisp - WasmGC Type Definitions for Lisp Objects
;;;;
;;;; Defines the runtime type system mapping Common Lisp objects to WasmGC
;;;; structures. Uses i31ref for fixnums and structs/arrays for heap objects.

(in-package #:clysm)

;;; ============================================================
;;; Type Registry
;;; ============================================================
;;;
;;; The type registry manages named type indices for Lisp objects.
;;; Type indices are assigned when types are registered with a module.

(defstruct (type-registry (:constructor %make-type-registry))
  "Registry of Lisp object types and their Wasm type indices."
  ;; Core function types (for closure dispatch)
  (func-0 nil :type (or null integer))    ; () -> anyref
  (func-1 nil :type (or null integer))    ; (anyref) -> anyref
  (func-2 nil :type (or null integer))    ; (anyref, anyref) -> anyref
  (func-n nil :type (or null integer))    ; (anyref, array) -> anyref

  ;; Core struct types
  (cons nil :type (or null integer))      ; $cons
  (symbol nil :type (or null integer))    ; $symbol
  (closure nil :type (or null integer))   ; $closure

  ;; Array types
  (env nil :type (or null integer))       ; $env (closure environment)
  (string nil :type (or null integer))    ; $string (i8 array)
  (vector nil :type (or null integer))    ; $vector (anyref array)

  ;; Special marker types
  (nil-type nil :type (or null integer))  ; $nil singleton type
  (unbound nil :type (or null integer)))  ; $unbound sentinel type

(defun make-type-registry ()
  "Create an empty type registry."
  (%make-type-registry))

;;; ============================================================
;;; Function Types for Closure Dispatch
;;; ============================================================
;;;
;;; Closures support arity-specific dispatch for efficiency.
;;; Each closure can have specialized code for 0, 1, 2 args
;;; plus a general N-arg handler.

(defun make-func-0-type ()
  "Create function type: (env) -> anyref
ENV is the closure environment, result is anyref."
  (make-wasm-type
   (make-functype '((:ref :null 0))  ; env: (ref null $env)
                  '(:anyref))
   :name "func_0"))

(defun make-func-1-type ()
  "Create function type: (env, arg1) -> anyref"
  (make-wasm-type
   (make-functype '((:ref :null 0) :anyref)
                  '(:anyref))
   :name "func_1"))

(defun make-func-2-type ()
  "Create function type: (env, arg1, arg2) -> anyref"
  (make-wasm-type
   (make-functype '((:ref :null 0) :anyref :anyref)
                  '(:anyref))
   :name "func_2"))

(defun make-func-n-type ()
  "Create function type: (env, args-array) -> anyref
For functions with more than 2 arguments."
  (make-wasm-type
   (make-functype '((:ref :null 0) (:ref :null 1))  ; env, args-array
                  '(:anyref))
   :name "func_N"))

;;; ============================================================
;;; Core Struct Types
;;; ============================================================

(defun make-cons-type ()
  "Create the cons cell struct type.
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))"
  (make-wasm-type
   (make-structtype
    (list (make-field :anyref :mutable t)    ; car
          (make-field :anyref :mutable t)))  ; cdr
   :name "cons"))

(defun make-symbol-type ()
  "Create the symbol struct type.
(type $symbol (struct
  (field $name (ref $string))      ; print name
  (field $value (mut anyref))      ; global value
  (field $function (mut anyref))   ; function definition
  (field $plist (mut anyref))))    ; property list"
  (make-wasm-type
   (make-structtype
    (list (make-field :anyref :mutable nil)   ; name (immutable)
          (make-field :anyref :mutable t)     ; value
          (make-field :anyref :mutable t)     ; function
          (make-field :anyref :mutable t)))   ; plist
   :name "symbol"))

(defun make-closure-type (registry)
  "Create the closure struct type.
(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (ref null $env))))
Uses function type indices from REGISTRY."
  (let ((func-0-idx (type-registry-func-0 registry))
        (func-1-idx (type-registry-func-1 registry))
        (func-2-idx (type-registry-func-2 registry))
        (func-n-idx (type-registry-func-n registry))
        (env-idx (type-registry-env registry)))
    (make-wasm-type
     (make-structtype
      (list (make-field `(:ref :null ,func-0-idx) :mutable nil)  ; code_0
            (make-field `(:ref :null ,func-1-idx) :mutable nil)  ; code_1
            (make-field `(:ref :null ,func-2-idx) :mutable nil)  ; code_2
            (make-field `(:ref :null ,func-n-idx) :mutable nil)  ; code_N
            (make-field `(:ref :null ,env-idx) :mutable nil)))   ; env
     :name "closure")))

;;; ============================================================
;;; Array Types
;;; ============================================================

(defun make-env-type ()
  "Create the closure environment array type.
(type $env (array (mut anyref)))"
  (make-wasm-type
   (make-arraytype :anyref :mutable t)
   :name "env"))

(defun make-string-type ()
  "Create the string type (array of i8).
(type $string (array (mut i8)))
Note: Using i8 for ASCII. Unicode support may need i32."
  (make-wasm-type
   (make-arraytype :i32 :mutable t)  ; Using i32 for now (easier indexing)
   :name "string"))

(defun make-vector-type ()
  "Create the general vector type.
(type $vector (array (mut anyref)))"
  (make-wasm-type
   (make-arraytype :anyref :mutable t)
   :name "vector"))

;;; ============================================================
;;; Special Singleton Types
;;; ============================================================

(defun make-nil-type ()
  "Create the NIL singleton type.
NIL is a special symbol whose car and cdr point to itself.
(type $nil (struct
  (field $car (ref $nil))
  (field $cdr (ref $nil))
  (field $name (ref $string))
  (field $value (ref $nil))
  (field $function anyref)
  (field $plist anyref)))
Note: Simplified - uses anyref for self-references initially."
  (make-wasm-type
   (make-structtype
    (list (make-field :anyref :mutable nil)    ; car (points to self)
          (make-field :anyref :mutable nil)    ; cdr (points to self)
          (make-field :anyref :mutable nil)    ; name
          (make-field :anyref :mutable nil)    ; value (points to self)
          (make-field :anyref :mutable nil)    ; function (nil)
          (make-field :anyref :mutable nil)))  ; plist (nil)
   :name "nil"))

(defun make-unbound-type ()
  "Create the UNBOUND sentinel type.
Used to detect unbound variables.
(type $unbound (struct))"
  (make-wasm-type
   (make-structtype nil)  ; Empty struct
   :name "unbound"))

;;; ============================================================
;;; Type Registration
;;; ============================================================

(defun register-core-types (module)
  "Register all core Lisp types with MODULE.
Returns a TYPE-REGISTRY with the assigned type indices.

Types are registered in dependency order:
1. Array types (env, string, vector) - no dependencies
2. Function types - reference env array
3. Struct types - reference function types and arrays
4. Singleton types"
  (let ((registry (make-type-registry)))
    ;; Phase 1: Array types (no dependencies)
    (setf (type-registry-env registry)
          (module-add-type module (make-env-type)))
    (setf (type-registry-string registry)
          (module-add-type module (make-string-type)))
    (setf (type-registry-vector registry)
          (module-add-type module (make-vector-type)))

    ;; Phase 2: Function types (reference env)
    ;; Note: We need to patch the func types to reference the actual env index
    (let ((env-idx (type-registry-env registry)))
      (setf (type-registry-func-0 registry)
            (module-add-type module
                             (make-wasm-type
                              (make-functype `((:ref :null ,env-idx))
                                             '(:anyref))
                              :name "func_0")))
      (setf (type-registry-func-1 registry)
            (module-add-type module
                             (make-wasm-type
                              (make-functype `((:ref :null ,env-idx) :anyref)
                                             '(:anyref))
                              :name "func_1")))
      (setf (type-registry-func-2 registry)
            (module-add-type module
                             (make-wasm-type
                              (make-functype `((:ref :null ,env-idx) :anyref :anyref)
                                             '(:anyref))
                              :name "func_2")))
      (let ((args-idx (type-registry-vector registry)))
        (setf (type-registry-func-n registry)
              (module-add-type module
                               (make-wasm-type
                                (make-functype `((:ref :null ,env-idx)
                                                 (:ref :null ,args-idx))
                                               '(:anyref))
                                :name "func_N")))))

    ;; Phase 3: Struct types
    (setf (type-registry-cons registry)
          (module-add-type module (make-cons-type)))
    (setf (type-registry-symbol registry)
          (module-add-type module (make-symbol-type)))
    (setf (type-registry-closure registry)
          (module-add-type module (make-closure-type registry)))

    ;; Phase 4: Singleton types
    (setf (type-registry-nil-type registry)
          (module-add-type module (make-nil-type)))
    (setf (type-registry-unbound registry)
          (module-add-type module (make-unbound-type)))

    registry))

;;; ============================================================
;;; Type Index Accessors
;;; ============================================================

(defun cons-type-index (registry)
  "Get the type index for cons cells."
  (type-registry-cons registry))

(defun symbol-type-index (registry)
  "Get the type index for symbols."
  (type-registry-symbol registry))

(defun closure-type-index (registry)
  "Get the type index for closures."
  (type-registry-closure registry))

(defun env-type-index (registry)
  "Get the type index for closure environments."
  (type-registry-env registry))

(defun string-type-index (registry)
  "Get the type index for strings."
  (type-registry-string registry))

(defun vector-type-index (registry)
  "Get the type index for vectors."
  (type-registry-vector registry))

(defun nil-type-index (registry)
  "Get the type index for NIL."
  (type-registry-nil-type registry))

(defun unbound-type-index (registry)
  "Get the type index for UNBOUND."
  (type-registry-unbound registry))

;;; ============================================================
;;; Struct Field Indices
;;; ============================================================

;; Cons cell fields
(defconstant +cons-car+ 0 "Field index for cons car")
(defconstant +cons-cdr+ 1 "Field index for cons cdr")

;; Symbol fields
(defconstant +symbol-name+ 0 "Field index for symbol name")
(defconstant +symbol-value+ 1 "Field index for symbol value")
(defconstant +symbol-function+ 2 "Field index for symbol function")
(defconstant +symbol-plist+ 3 "Field index for symbol plist")

;; Closure fields
(defconstant +closure-code-0+ 0 "Field index for 0-arg code")
(defconstant +closure-code-1+ 1 "Field index for 1-arg code")
(defconstant +closure-code-2+ 2 "Field index for 2-arg code")
(defconstant +closure-code-n+ 3 "Field index for N-arg code")
(defconstant +closure-env+ 4 "Field index for closure environment")

;; NIL fields (acts as both cons and symbol)
(defconstant +nil-car+ 0)
(defconstant +nil-cdr+ 1)
(defconstant +nil-name+ 2)
(defconstant +nil-value+ 3)
(defconstant +nil-function+ 4)
(defconstant +nil-plist+ 5)

;;; ============================================================
;;; Instruction Emitters for Object Operations
;;; ============================================================

(defun emit-struct.new (type-index)
  "Emit struct.new instruction for TYPE-INDEX."
  (append (list #xFB #x00)  ; struct.new prefix
          (encode-uleb128 type-index)))

(defun emit-struct.get (type-index field-index)
  "Emit struct.get instruction."
  (append (list #xFB #x02)  ; struct.get prefix
          (encode-uleb128 type-index)
          (encode-uleb128 field-index)))

(defun emit-struct.set (type-index field-index)
  "Emit struct.set instruction."
  (append (list #xFB #x05)  ; struct.set prefix
          (encode-uleb128 type-index)
          (encode-uleb128 field-index)))

(defun emit-array.new (type-index)
  "Emit array.new instruction."
  (append (list #xFB #x06)  ; array.new prefix
          (encode-uleb128 type-index)))

(defun emit-array.new-fixed (type-index length)
  "Emit array.new_fixed instruction."
  (append (list #xFB #x08)  ; array.new_fixed prefix
          (encode-uleb128 type-index)
          (encode-uleb128 length)))

(defun emit-array.get (type-index)
  "Emit array.get instruction."
  (append (list #xFB #x0B)  ; array.get prefix
          (encode-uleb128 type-index)))

(defun emit-array.set (type-index)
  "Emit array.set instruction."
  (append (list #xFB #x0E)  ; array.set prefix
          (encode-uleb128 type-index)))

(defun emit-array.len ()
  "Emit array.len instruction."
  (list #xFB #x0F))

(defun emit-ref.i31 ()
  "Emit ref.i31 instruction (wrap i32 to i31ref)."
  (list #xFB #x1C))

(defun emit-i31.get-s ()
  "Emit i31.get_s instruction (extract signed i32 from i31ref)."
  (list #xFB #x1D))

(defun emit-i31.get-u ()
  "Emit i31.get_u instruction (extract unsigned i32 from i31ref)."
  (list #xFB #x1E))

(defun emit-ref.test (type-index)
  "Emit ref.test instruction."
  (append (list #xFB #x14)
          (encode-uleb128 type-index)))

(defun emit-ref.cast (type-index)
  "Emit ref.cast instruction."
  (append (list #xFB #x15)
          (encode-uleb128 type-index)))

;;; ============================================================
;;; High-Level Object Creation Emitters
;;; ============================================================

(defun emit-make-cons (registry)
  "Emit instructions to create a cons cell.
Stack: [car cdr] -> [cons]"
  (emit-struct.new (cons-type-index registry)))

(defun emit-car (registry)
  "Emit instructions to get car of a cons.
Stack: [cons] -> [car]"
  (emit-struct.get (cons-type-index registry) +cons-car+))

(defun emit-cdr (registry)
  "Emit instructions to get cdr of a cons.
Stack: [cons] -> [cdr]"
  (emit-struct.get (cons-type-index registry) +cons-cdr+))

(defun emit-rplaca (registry)
  "Emit instructions to set car of a cons.
Stack: [cons new-car] -> []"
  (emit-struct.set (cons-type-index registry) +cons-car+))

(defun emit-rplacd (registry)
  "Emit instructions to set cdr of a cons.
Stack: [cons new-cdr] -> []"
  (emit-struct.set (cons-type-index registry) +cons-cdr+))

(defun emit-fixnum-from-i32 ()
  "Emit instructions to convert i32 to fixnum (i31ref).
Stack: [i32] -> [i31ref]"
  (emit-ref.i31))

(defun emit-fixnum-to-i32 ()
  "Emit instructions to convert fixnum (i31ref) to i32.
Stack: [i31ref] -> [i32]"
  (emit-i31.get-s))

(defun emit-consp (registry)
  "Emit instructions to test if value is a cons.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (cons-type-index registry)))

(defun emit-symbolp (registry)
  "Emit instructions to test if value is a symbol.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (symbol-type-index registry)))

(defun emit-functionp (registry)
  "Emit instructions to test if value is a closure.
Stack: [anyref] -> [i32 (0 or 1)]"
  (emit-ref.test (closure-type-index registry)))
