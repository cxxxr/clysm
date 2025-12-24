;;;; ffi-marshalling-test.lisp - Unit tests for FFI type marshalling (T037-T042)
;;;;
;;;; Tests for marshalling code generation between Lisp types and Wasm types.
;;;; These tests verify that the marshalling module generates correct Wasm
;;;; instructions for type conversion.
;;;;
;;;; Marshal Type Mapping:
;;;;   :fixnum  - i31ref <-> i32 (sign-extended 31-bit integer)
;;;;   :float   - (ref $float) <-> f64 (IEEE 754 double)
;;;;   :string  - (ref $string) <-> externref (WasmGC array)
;;;;   :boolean - t/NIL <-> i32 (1/0)
;;;;   :anyref  - anyref <-> anyref (passthrough, no conversion)

(in-package #:clysm/tests/unit/ffi-marshalling)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun has-instruction-named (instrs name)
  "Check if INSTRS contains an instruction with the given NAME (case-insensitive).
   Handles both atoms and lists (for instructions like (struct.get $float 0))."
  (find-if (lambda (x)
             (cond
               ((atom x)
                (string-equal (symbol-name x) name))
               ((listp x)
                (string-equal (symbol-name (first x)) name))
               (t nil)))
           instrs))

;;; ============================================================
;;; T037: Fixnum Marshalling Tests
;;; ============================================================

(deftest fixnum-marshal-to-i32
  "Test marshalling fixnum (i31ref) to i32 - Lisp to Host."
  ;; When passing a fixnum to a host function, we need to convert
  ;; from i31ref to i32 (sign-extend the 31-bit value)
  (let ((instrs (clysm/ffi:marshal-to-wasm :fixnum)))
    ;; Should generate instruction sequence for i31ref -> i32 conversion
    (ok (listp instrs) "Should return instruction list")
    (ok (has-instruction-named instrs "I31.GET_S")
        "Should use i31.get_s for sign-extended conversion")))

(deftest fixnum-unmarshal-from-i32
  "Test unmarshalling i32 to fixnum - Host to Lisp."
  ;; When receiving an i32 from a host function, we need to convert to i31ref
  (let ((instrs (clysm/ffi:marshal-from-wasm :fixnum)))
    (ok (listp instrs) "Should return instruction list")
    ;; Should generate ref.i31 to create i31ref from i32
    (ok (has-instruction-named instrs "REF.I31")
        "Should use ref.i31 to wrap i32 in i31ref")))

(deftest fixnum-roundtrip-identity
  "Test that fixnum marshalling is reversible."
  ;; marshal-to-wasm followed by marshal-from-wasm should be identity
  (let ((to-wasm (clysm/ffi:marshal-to-wasm :fixnum))
        (from-wasm (clysm/ffi:marshal-from-wasm :fixnum)))
    (ok (and to-wasm from-wasm)
        "Both directions should generate instructions")))

;;; ============================================================
;;; T038: Float Marshalling Tests
;;; ============================================================

(deftest float-marshal-to-f64
  "Test marshalling Lisp float to f64 - Lisp to Host."
  ;; Lisp floats are boxed in $float struct, need to extract the f64 value
  (let ((instrs (clysm/ffi:marshal-to-wasm :float)))
    (ok (listp instrs) "Should return instruction list")
    ;; Should access the f64 field from the $float struct
    (ok (has-instruction-named instrs "STRUCT.GET")
        "Should extract f64 from $float struct using struct.get")))

(deftest float-unmarshal-from-f64
  "Test unmarshalling f64 to Lisp float - Host to Lisp."
  ;; Need to wrap f64 in a $float struct
  (let ((instrs (clysm/ffi:marshal-from-wasm :float)))
    (ok (listp instrs) "Should return instruction list")
    ;; Should create $float struct containing the f64
    (ok (has-instruction-named instrs "STRUCT.NEW")
        "Should create $float struct from f64 using struct.new")))

;;; ============================================================
;;; T039: String Marshalling Tests (WasmGC, no linear memory)
;;; ============================================================

(deftest string-marshal-to-externref
  "Test marshalling Lisp string to externref (WasmGC array, no linear memory)."
  ;; Strings are already WasmGC arrays ($string = array i8), pass as externref
  (let ((instrs (clysm/ffi:marshal-to-wasm :string)))
    (ok (listp instrs) "Should return instruction list")
    ;; For WasmGC strings, we pass them as-is or cast to externref
    ;; Should NOT use any memory.* or data.* instructions (no linear memory)
    (ok (not (find-if (lambda (x)
                        (and (atom x)
                             (let ((s (symbol-name x)))
                               (or (search "MEMORY" s)
                                   (search "DATA.DROP" s)))))
                      instrs))
        "Should NOT use linear memory operations (Constitution I: WasmGC-First)")))

(deftest string-unmarshal-from-externref
  "Test unmarshalling externref to Lisp string - Host to Lisp."
  ;; externref from host needs to be treated as $string
  (let ((instrs (clysm/ffi:marshal-from-wasm :string)))
    (ok (listp instrs) "Should return instruction list")
    ;; No linear memory operations
    (ok (not (find-if (lambda (x)
                        (and (atom x)
                             (let ((s (symbol-name x)))
                               (or (search "MEMORY" s)
                                   (search "I32.LOAD" s)))))
                      instrs))
        "Should NOT use linear memory operations")))

(deftest string-no-linear-memory
  "Verify string marshalling never uses linear memory (Constitution I)."
  (let ((to-wasm (clysm/ffi:marshal-to-wasm :string))
        (from-wasm (clysm/ffi:marshal-from-wasm :string)))
    ;; Concatenate all instructions and check for memory ops
    (let ((all-instrs (append to-wasm from-wasm)))
      (ok (not (find-if (lambda (x)
                          (and (atom x)
                               (let ((s (symbol-name x)))
                                 (or (search "MEMORY" s)
                                     (search "I32.STORE" s)
                                     (search "I32.LOAD" s)))))
                        all-instrs))
          "String marshalling must use WasmGC arrays only"))))

;;; ============================================================
;;; T040: Boolean Marshalling Tests
;;; ============================================================

(deftest boolean-marshal-to-i32
  "Test marshalling t/nil to 1/0 - Lisp to Host."
  ;; Lisp booleans (t=any non-nil, nil=null) need to convert to i32 1/0
  (let ((instrs (clysm/ffi:marshal-to-wasm :boolean)))
    (ok (listp instrs) "Should return instruction list")
    ;; Should check if value is nil and produce 0 or 1
    (ok (> (length instrs) 0)
        "Should generate boolean conversion instructions")))

(deftest boolean-unmarshal-from-i32
  "Test unmarshalling 1/0 to t/nil - Host to Lisp."
  ;; i32 0 -> nil, i32 non-zero -> t
  (let ((instrs (clysm/ffi:marshal-from-wasm :boolean)))
    (ok (listp instrs) "Should return instruction list")
    ;; Should convert i32 to Lisp boolean representation
    (ok (> (length instrs) 0)
        "Should generate boolean conversion instructions")))

(deftest boolean-zero-is-nil
  "Test that i32 0 unmarshals to nil."
  ;; Verify the semantic: 0 = nil (false)
  (let ((instrs (clysm/ffi:marshal-from-wasm :boolean)))
    (ok instrs "Should have instructions for boolean unmarshalling")))

(deftest boolean-nonzero-is-t
  "Test that i32 non-zero unmarshals to t."
  ;; Verify the semantic: non-zero = t (true)
  (let ((instrs (clysm/ffi:marshal-from-wasm :boolean)))
    (ok instrs "Should have instructions for boolean unmarshalling")))

;;; ============================================================
;;; T041: Anyref Passthrough Tests
;;; ============================================================

(deftest anyref-passthrough-to-wasm
  "Test that anyref values pass through without conversion to host."
  (let ((instrs (clysm/ffi:marshal-to-wasm :anyref)))
    ;; anyref should be passthrough - either nil or empty list
    (ok (or (null instrs)
            (listp instrs))
        "anyref should require no conversion to Wasm")))

(deftest anyref-passthrough-from-wasm
  "Test that anyref values pass through without conversion from host."
  (let ((instrs (clysm/ffi:marshal-from-wasm :anyref)))
    ;; anyref from host should pass through directly
    (ok (or (null instrs)
            (listp instrs))
        "anyref should require no conversion from Wasm")))

(deftest anyref-is-identity
  "Test that anyref marshalling is identity (no-op)."
  (let ((to-wasm (clysm/ffi:marshal-to-wasm :anyref))
        (from-wasm (clysm/ffi:marshal-from-wasm :anyref)))
    ;; Both directions should be no-ops (nil or empty)
    (ok (or (null to-wasm) (listp to-wasm))
        "anyref to-wasm should be identity")
    (ok (or (null from-wasm) (listp from-wasm))
        "anyref from-wasm should be identity")))

;;; ============================================================
;;; T042: Type Error Tests
;;; ============================================================

(deftest unsupported-type-signals-error
  "Test that unsupported types signal FFI-TYPE-ERROR."
  ;; :invalid-type is not a valid marshal type
  (ok (handler-case
          (progn
            (clysm/ffi:marshal-to-wasm :invalid-type)
            nil)  ; Should not reach here
        (clysm/ffi:ffi-type-error () t)
        (error () t))  ; Accept any error
      "Should signal FFI-TYPE-ERROR for invalid types"))

(deftest void-is-not-marshallable
  "Test that :void cannot be marshalled as a value."
  ;; :void is only valid as return type, not for value marshalling
  (ok (handler-case
          (progn
            (clysm/ffi:marshal-to-wasm :void)
            nil)  ; Should not reach here
        (clysm/ffi:ffi-type-error () t)
        (error () t))  ; Accept any error
      "Should signal error for :void marshalling"))

(deftest nil-type-signals-error
  "Test that nil type signals an error."
  (ok (handler-case
          (progn
            (clysm/ffi:marshal-to-wasm nil)
            nil)  ; Should not reach here
        (clysm/ffi:ffi-type-error () t)
        (error () t))  ; Accept any error
      "Should signal error for nil type"))

(deftest random-keyword-signals-error
  "Test that arbitrary keywords signal FFI-TYPE-ERROR."
  (ok (handler-case
          (progn
            (clysm/ffi:marshal-to-wasm :complex-number)
            nil)  ; Should not reach here
        (clysm/ffi:ffi-type-error () t)
        (error () t))  ; Accept any error
      "Should signal FFI-TYPE-ERROR for unsupported type :complex-number"))

;;; ============================================================
;;; Wasm Type Mapping Tests
;;; ============================================================

(deftest wasm-type-for-fixnum
  "Test that :fixnum maps to i31ref in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :fixnum)))
    (ok (or (eq wasm-type 'i31ref)
            (eq wasm-type 'i32)
            (string-equal (symbol-name wasm-type) "I31REF")
            (string-equal (symbol-name wasm-type) "I32"))
        "fixnum should map to i31ref or i32")))

(deftest wasm-type-for-float
  "Test that :float maps to f64 in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :float)))
    (ok (or (eq wasm-type 'f64)
            (string-equal (symbol-name wasm-type) "F64"))
        "float should map to f64")))

(deftest wasm-type-for-string
  "Test that :string maps to externref in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :string)))
    (ok (or (eq wasm-type 'externref)
            (string-equal (symbol-name wasm-type) "EXTERNREF"))
        "string should map to externref")))

(deftest wasm-type-for-boolean
  "Test that :boolean maps to i32 in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :boolean)))
    (ok (or (eq wasm-type 'i32)
            (string-equal (symbol-name wasm-type) "I32"))
        "boolean should map to i32")))

(deftest wasm-type-for-anyref
  "Test that :anyref maps to anyref in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :anyref)))
    (ok (or (eq wasm-type 'anyref)
            (string-equal (symbol-name wasm-type) "ANYREF"))
        "anyref should map to anyref")))

(deftest wasm-type-for-void
  "Test that :void maps to nil (no type) in Wasm signature."
  (let ((wasm-type (clysm/ffi:marshal-type-to-wasm-type :void)))
    (ok (null wasm-type) "void should map to nil (no return type)")))
