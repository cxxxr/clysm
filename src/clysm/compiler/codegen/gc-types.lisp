;;;; gc-types.lisp - WasmGC type definitions
;;;; Defines the type system for Clysm's Lisp objects in WasmGC

(in-package #:clysm/compiler/codegen/gc-types)

;;; ============================================================
;;; Type Indices for Standard Clysm Types
;;; ============================================================

(defconstant +type-nil+ 0 "Type index for NIL singleton")
(defconstant +type-unbound+ 1 "Type index for UNBOUND sentinel")
(defconstant +type-cons+ 2 "Type index for cons cells")
(defconstant +type-symbol+ 3 "Type index for symbols")
(defconstant +type-string+ 4 "Type index for strings")
(defconstant +type-closure+ 5 "Type index for closures")
(defconstant +type-instance+ 6 "Type index for CLOS instances")
(defconstant +type-standard-class+ 7 "Type index for standard classes")

;;; ============================================================
;;; WasmGC Type Structures
;;; ============================================================

(defstruct (gc-type (:constructor nil))
  "Base structure for WasmGC types"
  (index nil :type (or null fixnum)))

(defstruct (wasm-struct-type (:include gc-type)
                             (:constructor make-wasm-struct-type))
  "WasmGC struct type definition"
  (name nil :type symbol)
  (fields nil :type list)
  (super nil :type (or null wasm-struct-type)))

(defstruct (wasm-array-type (:include gc-type)
                            (:constructor make-wasm-array-type))
  "WasmGC array type definition"
  (name nil :type symbol)
  (element-type nil :type keyword)
  (mutable t :type boolean))

(defstruct (wasm-field)
  "WasmGC struct field definition"
  (name nil :type symbol)
  (type nil :type keyword)  ; :i32, :i64, :anyref, :i31ref, etc.
  (mutable t :type boolean))

;;; ============================================================
;;; Type Environment
;;; ============================================================

(defstruct (type-environment (:constructor %make-type-environment))
  "Environment tracking all defined types"
  (types (make-hash-table) :type hash-table)
  (next-index 0 :type fixnum))

(defun make-type-environment ()
  "Create a new type environment with standard types pre-registered"
  (let ((env (%make-type-environment)))
    ;; Register standard types
    (register-type env :nil)
    (register-type env :unbound)
    (register-type env :cons)
    (register-type env :symbol)
    (register-type env :string)
    (register-type env :closure)
    env))

(defun register-type (env type-key)
  "Register a type and return its index"
  (let ((types (type-environment-types env)))
    (or (gethash type-key types)
        (let ((idx (type-environment-next-index env)))
          (setf (gethash type-key types) idx)
          (incf (type-environment-next-index env))
          idx))))

(defun get-type-index (env type-key)
  "Get the index for a registered type"
  (gethash type-key (type-environment-types env)))

;;; ============================================================
;;; Fixnum Representation
;;; ============================================================

(defun fixnum-representation ()
  "Return the Wasm representation for fixnums"
  :i31ref)

;;; ============================================================
;;; Type Constructors (T033-T037)
;;; ============================================================

(defun make-nil-type ()
  "Create NIL singleton type (T033)
   NIL is represented as a struct with a single i32 tag field
   to distinguish it from other null references."
  (make-wasm-struct-type
   :name '$nil
   :index +type-nil+
   :fields (list (make-wasm-field :name 'tag :type :i32 :mutable nil))))

(defun make-unbound-type ()
  "Create UNBOUND sentinel type (T034)
   Used to detect unbound variables at runtime."
  (make-wasm-struct-type
   :name '$unbound
   :index +type-unbound+
   :fields (list (make-wasm-field :name 'tag :type :i32 :mutable nil))))

(defun make-cons-type ()
  "Create CONS cell type (T035)
   Cons cells have car and cdr fields, both anyref."
  (make-wasm-struct-type
   :name '$cons
   :index +type-cons+
   :fields (list (make-wasm-field :name 'car :type :anyref :mutable t)
                 (make-wasm-field :name 'cdr :type :anyref :mutable t))))

(defun make-symbol-type ()
  "Create symbol type (T036)
   Symbols have name, value, and function slots."
  (make-wasm-struct-type
   :name '$symbol
   :index +type-symbol+
   :fields (list (make-wasm-field :name 'name :type :anyref :mutable nil)    ; string ref
                 (make-wasm-field :name 'value :type :anyref :mutable t)     ; value cell
                 (make-wasm-field :name 'function :type :anyref :mutable t)  ; function cell
                 (make-wasm-field :name 'plist :type :anyref :mutable t))))  ; property list

(defun make-string-type ()
  "Create string type (T037)
   Strings are arrays of i8 (UTF-8 bytes)."
  (make-wasm-array-type
   :name '$string
   :index +type-string+
   :element-type :i8
   :mutable nil))

(defun make-closure-type ()
  "Create closure type
   Closures have a function reference and environment."
  (make-wasm-struct-type
   :name '$closure
   :index +type-closure+
   :fields (list (make-wasm-field :name 'code :type :funcref :mutable nil)
                 (make-wasm-field :name 'env :type :anyref :mutable nil))))

;;; ============================================================
;;; Struct Fields Accessor
;;; ============================================================

(defun struct-fields (struct-type)
  "Get the fields of a struct type"
  (wasm-struct-type-fields struct-type))

;;; ============================================================
;;; Type Section Generation (T038)
;;; ============================================================

(defun generate-type-definitions ()
  "Generate all WasmGC type definitions for the Type Section.
   Returns a list of type definitions in order of their indices."
  (list (make-nil-type)
        (make-unbound-type)
        (make-cons-type)
        (make-symbol-type)
        (make-string-type)
        (make-closure-type)))

(defun emit-type-to-binary (type stream)
  "Emit a type definition to the binary stream.
   Uses WasmGC encoding for struct and array types."
  (etypecase type
    (wasm-struct-type
     (emit-struct-type type stream))
    (wasm-array-type
     (emit-array-type type stream))))

(defun emit-struct-type (struct-type stream)
  "Emit a WasmGC struct type definition"
  ;; struct type encoding: 0x5F (sub), 0x5C (no super), field_count, fields...
  ;; Simplified: 0x5F (struct), field_count, fields...
  (write-byte #x5F stream)  ; struct type
  (let ((fields (wasm-struct-type-fields struct-type)))
    (clysm/backend/leb128:encode-unsigned-leb128-to-stream (length fields) stream)
    (dolist (field fields)
      (emit-field-type field stream))))

(defun emit-array-type (array-type stream)
  "Emit a WasmGC array type definition"
  ;; array type encoding: 0x5E, element_type, mutability
  (write-byte #x5E stream)  ; array type
  (emit-value-type (wasm-array-type-element-type array-type) stream)
  (write-byte (if (wasm-array-type-mutable array-type) 1 0) stream))

(defun emit-field-type (field stream)
  "Emit a struct field type"
  (emit-value-type (wasm-field-type field) stream)
  (write-byte (if (wasm-field-mutable field) 1 0) stream))

(defun emit-value-type (type-keyword stream)
  "Emit a Wasm value type byte"
  (write-byte
   (ecase type-keyword
     (:i32 #x7F)
     (:i64 #x7E)
     (:f32 #x7D)
     (:f64 #x7C)
     (:i8 #x78)      ; i8 for array elements
     (:i16 #x77)     ; i16 for array elements
     (:anyref #x6F)  ; externref in core, anyref in GC
     (:funcref #x70)
     (:i31ref #x6C)  ; i31ref (WasmGC)
     (:eqref #x6D)   ; eqref (WasmGC)
     (:structref #x6B)) ; structref (WasmGC)
   stream))
