;;;; marshalling.lisp - FFI type marshalling code generation (T043-T052)
;;;;
;;;; Generates Wasm instructions for converting between Lisp types and Wasm types.
;;;; This module is used by import-gen.lisp and export-gen.lisp to wrap FFI calls.
;;;;
;;;; Type Mapping (Lisp -> Wasm -> Host):
;;;;   :fixnum  - i31ref -> i32 (via i31.get_s, sign-extended)
;;;;   :float   - (ref $float) -> f64 (via struct.get)
;;;;   :string  - (ref $string) -> externref (WasmGC array, no linear memory)
;;;;   :boolean - anyref -> i32 (nil=0, non-nil=1)
;;;;   :anyref  - anyref -> anyref (passthrough)
;;;;   :void    - N/A (no value)
;;;;
;;;; Type Mapping (Host -> Wasm -> Lisp):
;;;;   i32 -> :fixnum  - ref.i31 to create i31ref
;;;;   f64 -> :float   - struct.new $float to box
;;;;   externref -> :string - cast to (ref $string)
;;;;   i32 -> :boolean - 0=nil, non-zero=t
;;;;   anyref -> :anyref - passthrough

(in-package #:clysm/ffi)

;;; ============================================================
;;; T043: Fixnum Marshalling (i31ref <-> i32)
;;; ============================================================

(defun marshal-fixnum-to-i32 ()
  "Generate instructions to convert i31ref to i32 for host.
   Stack: [i31ref] -> [i32]"
  ;; i31.get_s sign-extends the 31-bit integer to i32
  '(i31.get_s))

(defun marshal-i32-to-fixnum ()
  "Generate instructions to convert i32 from host to i31ref.
   Stack: [i32] -> [i31ref]"
  ;; ref.i31 creates an i31ref from the low 31 bits of i32
  '(ref.i31))

;;; ============================================================
;;; T045-T046: Float Marshalling ((ref $float) <-> f64)
;;; ============================================================

(defun marshal-float-to-f64 ()
  "Generate instructions to extract f64 from $float struct.
   Stack: [(ref $float)] -> [f64]"
  ;; $float is a struct containing a single f64 field
  ;; struct.get $float 0 extracts the value
  '((struct.get $float 0)))

(defun marshal-f64-to-float ()
  "Generate instructions to wrap f64 in $float struct.
   Stack: [f64] -> [(ref $float)]"
  ;; struct.new $float creates a new float box
  '((struct.new $float)))

;;; ============================================================
;;; T047-T048: String Marshalling ((ref $string) <-> externref)
;;; ============================================================
;;; NOTE: Constitution I requires WasmGC-First - NO linear memory!
;;; Strings are WasmGC arrays, passed as externref to host.

(defun marshal-string-to-externref ()
  "Generate instructions to pass string as externref to host.
   Stack: [(ref $string)] -> [externref]

   Since $string is already a WasmGC reference type, we can pass it
   directly as externref. The host sees it as an opaque reference."
  ;; extern.convert_any converts any GC reference to externref
  '(extern.convert_any))

(defun marshal-externref-to-string ()
  "Generate instructions to receive externref from host as string.
   Stack: [externref] -> [(ref $string)]

   The host should pass back the same externref it received,
   which we can convert back to our internal string type."
  ;; any.convert_extern converts externref back to anyref
  ;; Then we need to cast to the expected string type
  '(any.convert_extern
    (ref.cast (ref $string))))

;;; ============================================================
;;; T049-T050: Boolean Marshalling (t/nil <-> i32 1/0)
;;; ============================================================

(defun marshal-boolean-to-i32 ()
  "Generate instructions to convert Lisp boolean to i32.
   Stack: [anyref] -> [i32]

   Lisp semantics: nil = false, anything else = true
   Wasm result: nil -> 0, non-nil -> 1"
  ;; Check if the value is null (nil)
  ;; If null, push 0; otherwise push 1
  '(ref.is_null              ; anyref -> i32 (1 if null, 0 if non-null)
    i32.eqz))                ; Invert: 1 if non-null (true), 0 if null (false)

(defun marshal-i32-to-boolean ()
  "Generate instructions to convert i32 to Lisp boolean.
   Stack: [i32] -> [anyref]

   Host semantics: 0 = false, non-zero = true
   Lisp result: 0 -> nil, non-zero -> t"
  ;; If i32 is 0, return nil (null); otherwise return symbol t
  ;; We use select to choose between nil and t based on the condition
  '(if (result (ref null any))
       (ref.null any)           ; 0 -> nil
       (global.get $sym-t)))    ; non-zero -> t

;;; ============================================================
;;; T041 (implementation): Anyref Passthrough
;;; ============================================================

(defun marshal-anyref-to-anyref ()
  "Generate instructions for anyref passthrough (no conversion).
   Stack: [anyref] -> [anyref]"
  ;; No conversion needed
  nil)

;;; ============================================================
;;; T051: Type Error Handling
;;; ============================================================

(defun signal-ffi-type-error (expected-type actual-value)
  "Signal an FFI-TYPE-ERROR condition."
  (error 'ffi-type-error
         :expected-type expected-type
         :actual-value actual-value))

;;; ============================================================
;;; Main Marshalling Interface (T043-T050)
;;; ============================================================

(defun marshal-to-wasm (marshal-type)
  "Generate instructions to marshal a Lisp value to Wasm type for host.
   MARSHAL-TYPE: The marshal type (:fixnum, :float, :string, :boolean, :anyref)
   Returns: List of Wasm instructions"
  (case marshal-type
    (:fixnum (marshal-fixnum-to-i32))
    (:float (marshal-float-to-f64))
    (:string (marshal-string-to-externref))
    (:boolean (marshal-boolean-to-i32))
    (:anyref (marshal-anyref-to-anyref))
    (:void (error 'ffi-type-error
                  :expected-type "value type"
                  :actual-value :void))
    (otherwise
     (error 'ffi-type-error
            :expected-type "valid marshal type"
            :actual-value marshal-type))))

(defun marshal-from-wasm (marshal-type)
  "Generate instructions to marshal a Wasm value from host to Lisp.
   MARSHAL-TYPE: The marshal type (:fixnum, :float, :string, :boolean, :anyref)
   Returns: List of Wasm instructions"
  (case marshal-type
    (:fixnum (marshal-i32-to-fixnum))
    (:float (marshal-f64-to-float))
    (:string (marshal-externref-to-string))
    (:boolean (marshal-i32-to-boolean))
    (:anyref (marshal-anyref-to-anyref))
    (:void (error 'ffi-type-error
                  :expected-type "value type"
                  :actual-value :void))
    (otherwise
     (error 'ffi-type-error
            :expected-type "valid marshal type"
            :actual-value marshal-type))))

;;; ============================================================
;;; Wasm Type Mapping (for Type Section generation)
;;; ============================================================
;;; Note: marshal-type-to-wasm-type is already defined in export-gen.lisp
;;; We keep this here for completeness and may consolidate later.

;;; ============================================================
;;; T052: Integration with Import/Export Generation
;;; ============================================================

(defun generate-marshal-instructions (param-types return-type direction)
  "Generate complete marshalling instructions for a function signature.
   PARAM-TYPES: List of marshal types for parameters
   RETURN-TYPE: Marshal type for return value
   DIRECTION: :to-host (calling host) or :from-host (host calling us)

   Returns: (values param-marshal-instrs return-marshal-instrs)"
  (case direction
    (:to-host
     ;; Calling a host function: marshal params TO wasm, unmarshal return FROM wasm
     (values (mapcar #'marshal-to-wasm param-types)
             (unless (eq return-type :void)
               (marshal-from-wasm return-type))))
    (:from-host
     ;; Host calling us: unmarshal params FROM wasm, marshal return TO wasm
     (values (mapcar #'marshal-from-wasm param-types)
             (unless (eq return-type :void)
               (marshal-to-wasm return-type))))
    (otherwise
     (error "Invalid direction: ~A (expected :to-host or :from-host)" direction))))

;;; ============================================================
;;; T070: Inline Marshalling Optimization for Common Types
;;; ============================================================
;;;
;;; Performance optimization: For commonly-used types, provide inline
;;; instruction sequences instead of function calls.

(defun marshal-to-wasm-inline (marshal-type)
  "Generate optimized inline instructions for marshalling to Wasm.
   T070: Performance optimization for common types.

   This function returns instructions that can be directly spliced into
   generated code without requiring additional function calls.

   For :fixnum: i31.get_s is a single instruction
   For :anyref: No instructions needed (passthrough)
   For :boolean: ref.is_null + i32.eqz (2 instructions)"
  (case marshal-type
    (:fixnum
     ;; Single instruction: extract i32 from i31ref
     '((i31.get_s)))
    (:anyref
     ;; No conversion needed - direct passthrough
     nil)
    (:boolean
     ;; Inline: check null and invert
     '((ref.is_null) (i32.eqz)))
    ;; Fall back to standard marshalling for complex types
    (otherwise
     (marshal-to-wasm marshal-type))))

(defun marshal-from-wasm-inline (marshal-type)
  "Generate optimized inline instructions for unmarshalling from Wasm.
   T070: Performance optimization for common types.

   For :fixnum: ref.i31 is a single instruction
   For :anyref: No instructions needed (passthrough)
   For :boolean: select between nil and t (inline if block)"
  (case marshal-type
    (:fixnum
     ;; Single instruction: wrap i32 in i31ref
     '((ref.i31)))
    (:anyref
     ;; No conversion needed - direct passthrough
     nil)
    ;; Fall back to standard unmarshalling for complex types
    (otherwise
     (marshal-from-wasm marshal-type))))

(defun should-inline-marshal-p (marshal-type)
  "Return T if the marshal type benefits from inline optimization.
   T070: Used to decide whether to use inline marshalling."
  (member marshal-type '(:fixnum :anyref)))

(defun marshal-instruction-count (marshal-type direction)
  "Return the number of instructions needed for marshalling.
   T070: Useful for estimating code size impact.
   DIRECTION: :to-wasm or :from-wasm"
  (let ((instrs (case direction
                  (:to-wasm (marshal-to-wasm-inline marshal-type))
                  (:from-wasm (marshal-from-wasm-inline marshal-type))
                  (otherwise nil))))
    (if instrs (length instrs) 0)))
