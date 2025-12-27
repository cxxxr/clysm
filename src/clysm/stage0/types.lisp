;;;; types.lisp - WasmGC type definitions for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements US4: Runtime Initialization - WasmGC type definitions
;;;;
;;;; This file generates the type section for Stage 0 Wasm binary,
;;;; defining all 24+ WasmGC types needed for Lisp object representation.

(in-package #:clysm/stage0)

;;; ============================================================
;;; Type Indices (matching gc-types.lisp from main compiler)
;;; ============================================================

(defconstant +type-nil+ 0 "Type index for NIL singleton")
(defconstant +type-unbound+ 1 "Type index for UNBOUND sentinel")
(defconstant +type-cons+ 2 "Type index for cons cells")
(defconstant +type-symbol+ 3 "Type index for symbols")
(defconstant +type-string+ 4 "Type index for strings")
(defconstant +type-closure+ 5 "Type index for closures")
(defconstant +type-instance+ 6 "Type index for CLOS instances")
(defconstant +type-standard-class+ 7 "Type index for standard classes")
(defconstant +type-func-0+ 8 "Type index for 0-arg function type")
(defconstant +type-func-1+ 9 "Type index for 1-arg function type")
(defconstant +type-func-2+ 10 "Type index for 2-arg function type")
(defconstant +type-func-3+ 11 "Type index for 3-arg function type")
(defconstant +type-func-n+ 12 "Type index for N-arg function type")
(defconstant +type-binding-frame+ 13 "Type index for binding frame")
(defconstant +type-bignum+ 14 "Type index for arbitrary-precision integers")
(defconstant +type-ratio+ 15 "Type index for exact rational numbers")
(defconstant +type-float+ 16 "Type index for IEEE 754 doubles")
(defconstant +type-complex+ 17 "Type index for complex numbers")
(defconstant +type-limb-array+ 18 "Type index for bignum limb array")
(defconstant +type-stream+ 19 "Type index for stream objects")
(defconstant +type-mv-array+ 20 "Type index for multiple values buffer")
(defconstant +type-slot-vector+ 21 "Type index for CLOS slot storage")
(defconstant +type-keyword-array+ 22 "Type index for CLOS initarg keywords")
(defconstant +type-closure-array+ 23 "Type index for CLOS initforms")
(defconstant +type-macro-environment+ 24 "Type index for macro environment")
(defconstant +type-hash-entry+ 25 "Type index for hash table entry")
(defconstant +type-hash-table+ 26 "Type index for hash table")
(defconstant +type-bucket-array+ 27 "Type index for hash table buckets")

(defconstant +total-types+ 28 "Total number of type definitions")

;;; ============================================================
;;; WasmGC Type Encoding Constants
;;; ============================================================

;; Wasm value types
(defconstant +wasm-i32+ #x7F)
(defconstant +wasm-i64+ #x7E)
(defconstant +wasm-f32+ #x7D)
(defconstant +wasm-f64+ #x7C)

;; WasmGC reference types
(defconstant +wasm-anyref+ #x6F)
(defconstant +wasm-funcref+ #x70)
(defconstant +wasm-externref+ #x6F)
(defconstant +wasm-i31ref+ #x6C)
(defconstant +wasm-structref+ #x6B)
(defconstant +wasm-arrayref+ #x6A)

;; WasmGC composite type prefixes
(defconstant +wasm-func+ #x60)     ; function type
(defconstant +wasm-struct+ #x5F)   ; struct type
(defconstant +wasm-array+ #x5E)    ; array type

;; GC prefix for type references
(defconstant +gc-prefix+ #xFB)

;; Mutability flags
(defconstant +immutable+ #x00)
(defconstant +mutable+ #x01)

;; Wasm section IDs
(defconstant +section-custom+ 0)
(defconstant +section-type+ 1)
(defconstant +section-import+ 2)
(defconstant +section-function+ 3)
(defconstant +section-table+ 4)
(defconstant +section-memory+ 5)
(defconstant +section-global+ 6)
(defconstant +section-export+ 7)
(defconstant +section-start+ 8)
(defconstant +section-element+ 9)
(defconstant +section-code+ 10)
(defconstant +section-data+ 11)

;; Nullability for ref types
(defconstant +nullable+ #x63)      ; ref null type
(defconstant +non-null+ #x64)      ; ref type

;;; ============================================================
;;; LEB128 Encoding Utilities
;;; ============================================================

(defun encode-unsigned-leb128 (value)
  "Encode unsigned integer as LEB128 bytes"
  (let ((result '()))
    (loop
      (let ((byte (logand value #x7F)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn
              (push byte result)
              (return (nreverse result)))
            (push (logior byte #x80) result))))))

(defun encode-signed-leb128 (value)
  "Encode signed integer as LEB128 bytes"
  (let ((result '())
        (more t))
    (loop while more do
      (let* ((byte (logand value #x7F))
             (value-next (ash value -7))
             (sign-bit (logand byte #x40)))
        (setf value value-next)
        (cond
          ;; Positive number, all remaining bits are 0
          ((and (zerop value) (zerop sign-bit))
           (push byte result)
           (setf more nil))
          ;; Negative number, all remaining bits are 1
          ((and (= value -1) (not (zerop sign-bit)))
           (push byte result)
           (setf more nil))
          ;; More bytes needed
          (t
           (push (logior byte #x80) result)))))
    (nreverse result)))

;;; ============================================================
;;; Type Section Generation
;;; ============================================================

(defun generate-nil-type ()
  "Generate NIL singleton struct type: struct $nil {}"
  ;; Empty struct with no fields
  (list +wasm-struct+
        0))  ; 0 fields

(defun generate-unbound-type ()
  "Generate UNBOUND sentinel struct type: struct $unbound {}"
  ;; Empty struct with no fields
  (list +wasm-struct+
        0))  ; 0 fields

(defun generate-cons-type ()
  "Generate cons cell struct type: struct $cons { car: anyref, cdr: anyref }"
  (list +wasm-struct+
        2                       ; 2 fields
        +wasm-anyref+ +mutable+ ; car: (mut anyref)
        +wasm-anyref+ +mutable+)) ; cdr: (mut anyref)

(defun generate-symbol-type ()
  "Generate symbol struct type with name, value, function, plist"
  (list +wasm-struct+
        4                       ; 4 fields
        +wasm-anyref+ +immutable+ ; name: anyref (string)
        +wasm-anyref+ +mutable+   ; value: (mut anyref)
        +wasm-anyref+ +mutable+   ; function: (mut anyref)
        +wasm-anyref+ +mutable+)) ; plist: (mut anyref)

(defun generate-string-type ()
  "Generate string array type: array i8"
  (list +wasm-array+
        +wasm-i32+ +immutable+)) ; element type: i8 (using i32 for simplicity)

(defun generate-closure-type ()
  "Generate closure struct with multi-arity code slots and environment"
  ;; Closure uses nullable func refs for arity dispatch
  (list +wasm-struct+
        5                         ; 5 fields
        +wasm-funcref+ +immutable+ ; code_0: funcref (0-arg)
        +wasm-funcref+ +immutable+ ; code_1: funcref (1-arg)
        +wasm-funcref+ +immutable+ ; code_2: funcref (2-arg)
        +wasm-funcref+ +immutable+ ; code_N: funcref (N-arg)
        +wasm-anyref+ +mutable+))  ; env: (mut anyref)

(defun generate-instance-type ()
  "Generate CLOS instance struct: struct { class: ref, slots: ref }"
  (list +wasm-struct+
        2                       ; 2 fields
        +wasm-anyref+ +immutable+ ; class reference
        +wasm-anyref+ +immutable+)) ; slot vector reference

(defun generate-standard-class-type ()
  "Generate standard-class struct"
  (list +wasm-struct+
        5                       ; 5 fields
        +wasm-anyref+ +immutable+ ; name
        +wasm-anyref+ +immutable+ ; superclass
        +wasm-i32+ +immutable+    ; slot_count
        +wasm-anyref+ +immutable+ ; slot-names
        +wasm-anyref+ +immutable+)) ; initargs

(defun generate-func-type (arity)
  "Generate function type for given arity"
  (let ((params (loop repeat arity collect +wasm-anyref+)))
    (append (list +wasm-func+
                  (length params))  ; param count
            params
            (list 1 +wasm-anyref+)))) ; 1 result: anyref

(defun generate-func-n-type ()
  "Generate N-arg function type (takes list of args)"
  (list +wasm-func+
        1 +wasm-anyref+        ; 1 param: args list
        1 +wasm-anyref+))      ; 1 result: anyref

(defun generate-binding-frame-type ()
  "Generate binding frame for dynamic variables"
  (list +wasm-struct+
        3                       ; 3 fields
        +wasm-anyref+ +immutable+ ; symbol
        +wasm-anyref+ +mutable+   ; old-value
        +wasm-anyref+ +immutable+)) ; next frame

(defun generate-bignum-type ()
  "Generate bignum struct: struct { sign: i32, limbs: ref }"
  (list +wasm-struct+
        2
        +wasm-i32+ +immutable+    ; sign
        +wasm-anyref+ +immutable+)) ; limbs array

(defun generate-ratio-type ()
  "Generate ratio struct: struct { numerator: anyref, denominator: anyref }"
  (list +wasm-struct+
        2
        +wasm-anyref+ +immutable+  ; numerator
        +wasm-anyref+ +immutable+)) ; denominator

(defun generate-float-type ()
  "Generate float struct: struct { value: f64 }"
  (list +wasm-struct+
        1
        +wasm-f64+ +immutable+))  ; IEEE 754 double

(defun generate-complex-type ()
  "Generate complex struct: struct { real: anyref, imag: anyref }"
  (list +wasm-struct+
        2
        +wasm-anyref+ +immutable+  ; real part
        +wasm-anyref+ +immutable+)) ; imaginary part

(defun generate-limb-array-type ()
  "Generate limb array for bignums: array i32"
  (list +wasm-array+
        +wasm-i32+ +immutable+))

(defun generate-stream-type ()
  "Generate stream struct"
  (list +wasm-struct+
        4
        +wasm-i32+ +immutable+    ; direction
        +wasm-anyref+ +mutable+   ; buffer
        +wasm-i32+ +mutable+      ; position
        +wasm-anyref+ +immutable+)) ; handle

(defun generate-mv-array-type ()
  "Generate multiple values buffer: array anyref"
  (list +wasm-array+
        +wasm-anyref+ +mutable+))

(defun generate-slot-vector-type ()
  "Generate CLOS slot storage: array anyref"
  (list +wasm-array+
        +wasm-anyref+ +mutable+))

(defun generate-keyword-array-type ()
  "Generate CLOS initarg keywords: array anyref"
  (list +wasm-array+
        +wasm-anyref+ +immutable+))

(defun generate-closure-array-type ()
  "Generate CLOS initform closures: array anyref"
  (list +wasm-array+
        +wasm-anyref+ +immutable+))

(defun generate-macro-environment-type ()
  "Generate macro environment struct"
  (list +wasm-struct+
        2
        +wasm-anyref+ +immutable+  ; local-macros
        +wasm-anyref+ +immutable+)) ; parent

(defun generate-hash-entry-type ()
  "Generate hash table entry struct"
  (list +wasm-struct+
        3
        +wasm-anyref+ +immutable+  ; key
        +wasm-anyref+ +mutable+    ; value
        +wasm-anyref+ +mutable+))  ; next (nullable hash-entry ref)

(defun generate-hash-table-type ()
  "Generate hash table struct"
  (list +wasm-struct+
        4
        +wasm-i32+ +immutable+     ; size (bucket count)
        +wasm-i32+ +mutable+       ; count (entry count)
        +wasm-anyref+ +immutable+  ; test function
        +wasm-anyref+ +immutable+)) ; buckets array

(defun generate-bucket-array-type ()
  "Generate hash table bucket array: array anyref"
  (list +wasm-array+
        +wasm-anyref+ +mutable+))

(defun generate-all-types ()
  "Generate list of all type definitions in order"
  (list
   (generate-nil-type)            ; 0
   (generate-unbound-type)        ; 1
   (generate-cons-type)           ; 2
   (generate-symbol-type)         ; 3
   (generate-string-type)         ; 4
   (generate-closure-type)        ; 5
   (generate-instance-type)       ; 6
   (generate-standard-class-type) ; 7
   (generate-func-type 0)         ; 8
   (generate-func-type 1)         ; 9
   (generate-func-type 2)         ; 10
   (generate-func-type 3)         ; 11
   (generate-func-n-type)         ; 12
   (generate-binding-frame-type)  ; 13
   (generate-bignum-type)         ; 14
   (generate-ratio-type)          ; 15
   (generate-float-type)          ; 16
   (generate-complex-type)        ; 17
   (generate-limb-array-type)     ; 18
   (generate-stream-type)         ; 19
   (generate-mv-array-type)       ; 20
   (generate-slot-vector-type)    ; 21
   (generate-keyword-array-type)  ; 22
   (generate-closure-array-type)  ; 23
   (generate-macro-environment-type) ; 24
   (generate-hash-entry-type)     ; 25
   (generate-hash-table-type)     ; 26
   (generate-bucket-array-type))) ; 27

(defun generate-type-section ()
  "Generate complete type section as byte vector.
   Returns vector of bytes for Wasm type section (section ID 1)."
  (let* ((types (generate-all-types))
         (type-bytes '())
         (type-count (length types)))
    ;; Encode each type definition
    (dolist (type-def types)
      (dolist (byte type-def)
        (push byte type-bytes)))
    ;; Reverse to get correct order
    (setf type-bytes (nreverse type-bytes))
    ;; Build section: ID + size + count + type definitions
    (let* ((count-bytes (encode-unsigned-leb128 type-count))
           (content-size (+ (length count-bytes) (length type-bytes)))
           (size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      ;; Section ID
      (push 1 section)
      ;; Section size
      (dolist (b size-bytes) (push b section))
      ;; Type count
      (dolist (b count-bytes) (push b section))
      ;; Type definitions
      (dolist (b type-bytes) (push b section))
      ;; Return as byte vector
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Result Types for Stage 0 Compilation
;;; ============================================================

(defstruct stage0-result
  "Result of Stage 0 compilation operation"
  (success-p nil :type boolean)
  (wasm-bytes nil :type (or null vector))
  (error-message nil :type (or null string))
  (form-count 0 :type integer))

(defstruct compile-context
  "Context for tracking compilation state"
  (forms nil :type list)
  (current-module nil :type (or null string))
  (compiled-count 0 :type integer)
  (failed-count 0 :type integer))
