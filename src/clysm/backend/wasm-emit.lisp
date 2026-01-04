;;;; backend/wasm-emit.lisp - Wasm Binary Emitter
;;;;
;;;; Emits complete Wasm binary modules from wasm-module structures.

(in-package #:clysm)

;;; ============================================================
;;; Module Header
;;; ============================================================

(defparameter *wasm-magic* '(#x00 #x61 #x73 #x6D)
  "Wasm binary magic number: \\0asm")

(defparameter *wasm-version* '(#x01 #x00 #x00 #x00)
  "Wasm binary version: 1 (little-endian)")

(defun emit-module-header ()
  "Emit the Wasm module header (magic + version)."
  (append *wasm-magic* *wasm-version*))

;;; ============================================================
;;; Complete Module Emission
;;; ============================================================

(defun emit-wasm-binary (module)
  "Emit a complete Wasm binary from MODULE.
Returns a list of bytes."
  ;; Ensure module is finalized
  (module-finalize module)

  ;; Emit all sections in order
  (let ((bytes (emit-module-header)))
    ;; Section 1: Type
    (when-let (section (emit-type-section (wasm-module-types module)))
      (setf bytes (append bytes section)))

    ;; Section 2: Import
    (when-let (section (emit-import-section (wasm-module-imports module)))
      (setf bytes (append bytes section)))

    ;; Section 3: Function
    (when-let (section (emit-function-section (wasm-module-funcs module)))
      (setf bytes (append bytes section)))

    ;; Section 4: Table
    (when-let (section (emit-table-section (wasm-module-tables module)))
      (setf bytes (append bytes section)))

    ;; Section 5: Memory
    (when-let (section (emit-memory-section (wasm-module-memories module)))
      (setf bytes (append bytes section)))

    ;; Section 6: Global
    (when-let (section (emit-global-section (wasm-module-globals module)))
      (setf bytes (append bytes section)))

    ;; Section 7: Export
    (when-let (section (emit-export-section (wasm-module-exports module)))
      (setf bytes (append bytes section)))

    ;; Section 8: Start
    (when-let (section (emit-start-section (wasm-module-start module)))
      (setf bytes (append bytes section)))

    ;; Section 10: Code
    (when-let (section (emit-code-section (wasm-module-funcs module)))
      (setf bytes (append bytes section)))

    ;; Custom section: name (for debugging)
    (when-let (section (emit-name-section module))
      (setf bytes (append bytes section)))

    bytes))

(defun emit-wasm-to-file (module pathname)
  "Emit MODULE to a Wasm binary file at PATHNAME."
  (let ((bytes (emit-wasm-binary module)))
    (write-file-bytes pathname bytes)
    pathname))

(defun emit-wasm-to-vector (module)
  "Emit MODULE as a byte vector."
  (bytes-to-vector (emit-wasm-binary module)))

;;; ============================================================
;;; Wasm Instruction Opcodes
;;; ============================================================

(defparameter *wasm-opcodes*
  '(;; Control instructions
    (:unreachable . #x00)
    (:nop . #x01)
    (:block . #x02)
    (:loop . #x03)
    (:if . #x04)
    (:else . #x05)
    (:end . #x0B)
    (:br . #x0C)
    (:br_if . #x0D)
    (:br_table . #x0E)
    (:return . #x0F)
    (:call . #x10)
    (:call_indirect . #x11)

    ;; Tail call (extension)
    (:return_call . #x12)
    (:return_call_indirect . #x13)

    ;; Reference instructions
    (:ref.null . #xD0)
    (:ref.is_null . #xD1)
    (:ref.func . #xD2)
    (:ref.eq . #xD3)

    ;; Parametric instructions
    (:drop . #x1A)
    (:select . #x1B)

    ;; Variable instructions
    (:local.get . #x20)
    (:local.set . #x21)
    (:local.tee . #x22)
    (:global.get . #x23)
    (:global.set . #x24)

    ;; Memory instructions
    (:i32.load . #x28)
    (:i64.load . #x29)
    (:f32.load . #x2A)
    (:f64.load . #x2B)
    (:i32.store . #x36)
    (:i64.store . #x37)
    (:f32.store . #x38)
    (:f64.store . #x39)

    ;; Numeric constants
    (:i32.const . #x41)
    (:i64.const . #x42)
    (:f32.const . #x43)
    (:f64.const . #x44)

    ;; i32 comparison
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

    ;; i64 comparison
    (:i64.eqz . #x50)
    (:i64.eq . #x51)
    (:i64.ne . #x52)
    (:i64.lt_s . #x53)
    (:i64.lt_u . #x54)
    (:i64.gt_s . #x55)
    (:i64.gt_u . #x56)
    (:i64.le_s . #x57)
    (:i64.le_u . #x58)
    (:i64.ge_s . #x59)
    (:i64.ge_u . #x5A)

    ;; i32 arithmetic
    (:i32.clz . #x67)
    (:i32.ctz . #x68)
    (:i32.popcnt . #x69)
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
    (:i32.shl . #x74)
    (:i32.shr_s . #x75)
    (:i32.shr_u . #x76)
    (:i32.rotl . #x77)
    (:i32.rotr . #x78)

    ;; i64 arithmetic
    (:i64.add . #x7C)
    (:i64.sub . #x7D)
    (:i64.mul . #x7E)

    ;; Conversion
    (:i32.wrap_i64 . #xA7)
    (:i64.extend_i32_s . #xAC)
    (:i64.extend_i32_u . #xAD))
  "Basic Wasm opcodes.")

;;; GC opcodes (0xFB prefix)
(defparameter *wasm-gc-opcodes*
  '((:struct.new . #x00)
    (:struct.new_default . #x01)
    (:struct.get . #x02)
    (:struct.get_s . #x03)
    (:struct.get_u . #x04)
    (:struct.set . #x05)
    (:array.new . #x06)
    (:array.new_default . #x07)
    (:array.new_fixed . #x08)
    (:array.new_data . #x09)
    (:array.new_elem . #x0A)
    (:array.get . #x0B)
    (:array.get_s . #x0C)
    (:array.get_u . #x0D)
    (:array.set . #x0E)
    (:array.len . #x0F)
    (:array.fill . #x10)
    (:array.copy . #x11)
    (:array.init_data . #x12)
    (:array.init_elem . #x13)
    (:ref.test . #x14)
    (:ref.cast . #x15)
    (:br_on_cast . #x18)
    (:br_on_cast_fail . #x19)
    (:any.convert_extern . #x1A)
    (:extern.convert_any . #x1B)
    (:ref.i31 . #x1C)
    (:i31.get_s . #x1D)
    (:i31.get_u . #x1E))
  "WasmGC opcodes (prefixed with 0xFB).")

(defun opcode (name)
  "Get the opcode byte(s) for an instruction name."
  (or (cdr (assoc name *wasm-opcodes*))
      (let ((gc-op (cdr (assoc name *wasm-gc-opcodes*))))
        (when gc-op
          (list #xFB gc-op)))
      (error "Unknown opcode: ~S" name)))

;;; ============================================================
;;; Instruction Encoding Helpers
;;; ============================================================

(defun emit-op (op &rest args)
  "Emit an instruction with optional immediate arguments."
  (let ((code (opcode op)))
    (append (ensure-list code)
            (mappend #'ensure-list args))))

(defun emit-i32.const (value)
  "Emit i32.const instruction."
  (append (list (opcode :i32.const))
          (encode-sleb128 value)))

(defun emit-i64.const (value)
  "Emit i64.const instruction."
  (append (list (opcode :i64.const))
          (encode-sleb128 value)))

(defun emit-local.get (index)
  "Emit local.get instruction."
  (append (list (opcode :local.get))
          (encode-uleb128 index)))

(defun emit-local.set (index)
  "Emit local.set instruction."
  (append (list (opcode :local.set))
          (encode-uleb128 index)))

(defun emit-call (func-index)
  "Emit call instruction."
  (append (list (opcode :call))
          (encode-uleb128 func-index)))

(defun emit-end ()
  "Emit end instruction."
  (list (opcode :end)))

;;; ============================================================
;;; Block Type Encoding
;;; ============================================================

(defun encode-blocktype (blocktype)
  "Encode a block type.
NIL = void, valtype = single result, negative = type index."
  (cond
    ((null blocktype)
     ;; Empty block type (void)
     (list #x40))
    ((keywordp blocktype)
     ;; Single value type
     (encode-valtype blocktype))
    ((integerp blocktype)
     ;; Type index (s33)
     (encode-s33 blocktype))
    (t
     (error "Invalid block type: ~S" blocktype))))
