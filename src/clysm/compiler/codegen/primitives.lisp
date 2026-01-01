;;;; primitives.lisp - Layer 1 Primitives Registry
;;;; Feature 001-runtime-library-system
;;;;
;;;; This file defines the primitives registry that maps primitive operation names
;;;; to their Wasm code emitters. Layer 2 runtime library functions use these
;;;; primitives as building blocks.
;;;;
;;;; Primitives are operations that compile directly to Wasm instructions,
;;;; such as: cons, car, cdr, +, -, consp, null, etc.

(in-package #:clysm)

;;; ============================================================================
;;; Primitive Structure
;;; ============================================================================

(defstruct (primitive (:constructor make-primitive)
                      (:copier nil))
  "A Layer 1 primitive operation that compiles directly to Wasm.

Slots:
  NAME         - Symbol naming the primitive (e.g., CONS, CAR, +)
  WASM-EMITTER - Function that emits Wasm bytecode for this primitive
  SIGNATURE    - List of (param-types return-types) for type checking
  INLINE-P     - T if this primitive should be inlined at call sites
  CATEGORY     - Keyword categorizing the primitive (:memory, :predicate, etc.)"
  (name        nil :type symbol :read-only t)
  (wasm-emitter nil :type (or function null))
  (signature   nil :type list)
  (inline-p    nil :type boolean)
  (category    nil :type keyword :read-only t))

;;; ============================================================================
;;; Primitives Registry
;;; ============================================================================

(defvar *primitives-registry* (make-hash-table :test #'eq)
  "Hash table mapping primitive names (symbols) to primitive structures.
Used by the compiler to resolve calls to Layer 1 operations.")

;;; ============================================================================
;;; Registry Operations
;;; ============================================================================

(defun register-primitive (name &key wasm-emitter signature inline-p category)
  "Register a primitive operation in the registry.

Arguments:
  NAME         - Symbol naming the primitive
  WASM-EMITTER - Function (args env) -> wasm-bytecode that emits code
  SIGNATURE    - List of (param-types return-types)
  INLINE-P     - Whether to inline this primitive
  CATEGORY     - Keyword category (:memory :predicate :arithmetic :ffi)

Returns: The created primitive structure."
  (let ((primitive (make-primitive :name name
                                   :wasm-emitter wasm-emitter
                                   :signature signature
                                   :inline-p inline-p
                                   :category (or category :other))))
    (setf (gethash name *primitives-registry*) primitive)
    primitive))

(defun get-primitive (name)
  "Look up a primitive by name.
Returns the primitive structure or NIL if not found."
  (gethash name *primitives-registry*))

(defun registered-primitive-p (name)
  "Test if NAME is a registered primitive in the registry.
Returns T if the primitive exists, NIL otherwise.
Note: primitive-p is reserved for the defstruct predicate."
  (not (null (gethash name *primitives-registry*))))

(defun list-primitives (&optional category)
  "List all registered primitives, optionally filtered by CATEGORY.

Arguments:
  CATEGORY - If provided, only list primitives in this category

Returns: List of primitive names (symbols)."
  (let ((result nil))
    (maphash (lambda (name primitive)
               (when (or (null category)
                         (eq (primitive-category primitive) category))
                 (push name result)))
             *primitives-registry*)
    (sort result #'string< :key #'symbol-name)))

(defun clear-primitives-registry ()
  "Clear all registered primitives. Mainly useful for testing."
  (clrhash *primitives-registry*))

;;; ============================================================================
;;; Primitive Categories
;;; ============================================================================
;;; Primitives are organized into categories per the contracts/primitives.md spec:
;;;
;;; :memory    - cons, car, cdr, rplaca, rplacd
;;; :predicate - consp, null, numberp, stringp, symbolp
;;; :arithmetic - +, -, *, /, mod, <, >, =, <=, >=
;;; :ffi       - %host-write-char, %host-write-string, %host-read-char, %host-read-line

;;; ============================================================================
;;; Primitive Registration (Phase 2)
;;; ============================================================================
;;; T018-T021: Register primitives with references to existing compile functions
;;; from func-section.lisp. The compile-* functions are loaded before this file.

(defun %register-memory-primitives ()
  "Register memory primitives: cons, car, cdr, rplaca, rplacd.
T018: Memory primitives per contracts/primitives.md
See also: resources/HyperSpec/Body/f_cons.htm, f_car_c.htm, f_rplaca.htm"
  (register-primitive 'cons
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-cons
                      :signature '((any any) cons)
                      :inline-p t
                      :category :memory)
  (register-primitive 'car
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-car
                      :signature '((cons) any)
                      :inline-p t
                      :category :memory)
  (register-primitive 'cdr
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-cdr
                      :signature '((cons) any)
                      :inline-p t
                      :category :memory)
  (register-primitive 'rplaca
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-rplaca
                      :signature '((cons any) cons)
                      :inline-p t
                      :category :memory)
  (register-primitive 'rplacd
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-rplacd
                      :signature '((cons any) cons)
                      :inline-p t
                      :category :memory))

(defun %register-predicate-primitives ()
  "Register type predicates: consp, null, numberp, stringp, symbolp.
T019: Type predicates per contracts/primitives.md
See also: resources/HyperSpec/Body/f_consp.htm, f_null.htm, f_nump.htm"
  (register-primitive 'consp
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-consp
                      :signature '((any) boolean)
                      :inline-p t
                      :category :predicate)
  (register-primitive 'null
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-null
                      :signature '((any) boolean)
                      :inline-p t
                      :category :predicate)
  (register-primitive 'numberp
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-numberp
                      :signature '((any) boolean)
                      :inline-p t
                      :category :predicate)
  (register-primitive 'stringp
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-stringp
                      :signature '((any) boolean)
                      :inline-p t
                      :category :predicate)
  (register-primitive 'symbolp
                      :wasm-emitter #'clysm/compiler/codegen/func-section::compile-symbolp
                      :signature '((any) boolean)
                      :inline-p t
                      :category :predicate))

(defun %register-arithmetic-primitives ()
  "Register arithmetic primitives: +, -, *, /, mod, <, >, =, <=, >=.
T020: Arithmetic primitives per contracts/primitives.md
See also: resources/HyperSpec/Body/f_pl.htm, f__.htm, f_st.htm, f_sl.htm

Note: Arithmetic and comparison ops use compile-arithmetic-op and compile-comparison-op
helper functions from func-section.lisp. We create wrapper lambdas for the registry."
  ;; Arithmetic operations - use compile-arithmetic-op
  (register-primitive '+
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.add args env 0))
                      :signature '((&rest number) number)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '-
                      :wasm-emitter (lambda (args env)
                                      (if (= (length args) 1)
                                          ;; Unary minus: negate
                                          (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.sub (cons 0 args) env nil)
                                          ;; Binary/variadic minus
                                          (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.sub args env nil)))
                      :signature '((number &rest number) number)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '*
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.mul args env 1))
                      :signature '((&rest number) number)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '/
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.div_s args env nil))
                      :signature '((number &rest number) number)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive 'mod
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.rem_s args env nil))
                      :signature '((number number) number)
                      :inline-p t
                      :category :arithmetic)
  ;; Comparison operations - use compile-comparison-op
  (register-primitive '<
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-comparison-op :i32.lt_s args env))
                      :signature '((number number) boolean)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '>
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-comparison-op :i32.gt_s args env))
                      :signature '((number number) boolean)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '=
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-comparison-op :i32.eq args env))
                      :signature '((number number) boolean)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '<=
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-comparison-op :i32.le_s args env))
                      :signature '((number number) boolean)
                      :inline-p t
                      :category :arithmetic)
  (register-primitive '>=
                      :wasm-emitter (lambda (args env)
                                      (clysm/compiler/codegen/func-section::compile-comparison-op :i32.ge_s args env))
                      :signature '((number number) boolean)
                      :inline-p t
                      :category :arithmetic))

(defun %register-ffi-primitives ()
  "Register FFI primitives: %host-write-char, %host-write-string, etc.
T021: FFI primitives per contracts/primitives.md"
  ;; FFI primitives use the FFI call mechanism rather than direct compile functions
  ;; These are registered as marker entries - actual compilation goes through FFI layer
  (register-primitive '%host-write-char
                      :wasm-emitter nil  ; Uses FFI call mechanism
                      :signature '((fixnum fixnum) void)
                      :inline-p nil
                      :category :ffi)
  (register-primitive '%host-write-string
                      :wasm-emitter nil  ; Uses FFI call mechanism
                      :signature '((fixnum string) void)
                      :inline-p nil
                      :category :ffi)
  (register-primitive '%host-read-char
                      :wasm-emitter nil  ; Uses FFI call mechanism
                      :signature '((fixnum) fixnum)
                      :inline-p nil
                      :category :ffi)
  (register-primitive '%host-read-line
                      :wasm-emitter nil  ; Uses FFI call mechanism
                      :signature '((fixnum) (or string null))
                      :inline-p nil
                      :category :ffi))

(defun initialize-primitives-registry ()
  "Initialize the primitives registry with all Layer 1 primitives.
Called during compiler initialization."
  (clear-primitives-registry)
  (%register-memory-primitives)
  (%register-predicate-primitives)
  (%register-arithmetic-primitives)
  (%register-ffi-primitives))
