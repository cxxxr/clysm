;;;; wat.lisp - WebAssembly Text format output (S-expression format with source comments)

(in-package #:clysm/wasm)

;;; Opcode → Mnemonic Mapping

(defparameter *opcode-names* (make-hash-table :test 'eql)
  "Opcode byte → WAT mnemonic string mapping.")

(defparameter *gc-opcode-names* (make-hash-table :test 'eql)
  "GC instruction opcode (after 0xfb prefix) → mnemonic mapping.")

(defun init-opcode-names ()
  "Initialize opcode name tables."
  (clrhash *opcode-names*)
  (clrhash *gc-opcode-names*)

  ;; Control Instructions
  (setf (gethash #x00 *opcode-names*) "unreachable")
  (setf (gethash #x01 *opcode-names*) "nop")
  (setf (gethash #x02 *opcode-names*) "block")
  (setf (gethash #x03 *opcode-names*) "loop")
  (setf (gethash #x04 *opcode-names*) "if")
  (setf (gethash #x05 *opcode-names*) "else")
  (setf (gethash #x0b *opcode-names*) "end")
  (setf (gethash #x0c *opcode-names*) "br")
  (setf (gethash #x0d *opcode-names*) "br_if")
  (setf (gethash #x0e *opcode-names*) "br_table")
  (setf (gethash #x0f *opcode-names*) "return")
  (setf (gethash #x10 *opcode-names*) "call")
  (setf (gethash #x11 *opcode-names*) "call_indirect")
  (setf (gethash #x12 *opcode-names*) "return_call")
  (setf (gethash #x13 *opcode-names*) "return_call_indirect")

  ;; Parametric Instructions
  (setf (gethash #x1a *opcode-names*) "drop")
  (setf (gethash #x1b *opcode-names*) "select")
  (setf (gethash #x1c *opcode-names*) "select")  ; typed

  ;; Variable Instructions
  (setf (gethash #x20 *opcode-names*) "local.get")
  (setf (gethash #x21 *opcode-names*) "local.set")
  (setf (gethash #x22 *opcode-names*) "local.tee")
  (setf (gethash #x23 *opcode-names*) "global.get")
  (setf (gethash #x24 *opcode-names*) "global.set")

  ;; Table Instructions
  (setf (gethash #x25 *opcode-names*) "table.get")
  (setf (gethash #x26 *opcode-names*) "table.set")

  ;; Memory Instructions
  (setf (gethash #x28 *opcode-names*) "i32.load")
  (setf (gethash #x29 *opcode-names*) "i64.load")
  (setf (gethash #x2a *opcode-names*) "f32.load")
  (setf (gethash #x2b *opcode-names*) "f64.load")
  (setf (gethash #x2c *opcode-names*) "i32.load8_s")
  (setf (gethash #x2d *opcode-names*) "i32.load8_u")
  (setf (gethash #x2e *opcode-names*) "i32.load16_s")
  (setf (gethash #x2f *opcode-names*) "i32.load16_u")
  (setf (gethash #x30 *opcode-names*) "i64.load8_s")
  (setf (gethash #x31 *opcode-names*) "i64.load8_u")
  (setf (gethash #x32 *opcode-names*) "i64.load16_s")
  (setf (gethash #x33 *opcode-names*) "i64.load16_u")
  (setf (gethash #x34 *opcode-names*) "i64.load32_s")
  (setf (gethash #x35 *opcode-names*) "i64.load32_u")
  (setf (gethash #x36 *opcode-names*) "i32.store")
  (setf (gethash #x37 *opcode-names*) "i64.store")
  (setf (gethash #x38 *opcode-names*) "f32.store")
  (setf (gethash #x39 *opcode-names*) "f64.store")
  (setf (gethash #x3a *opcode-names*) "i32.store8")
  (setf (gethash #x3b *opcode-names*) "i32.store16")
  (setf (gethash #x3c *opcode-names*) "i64.store8")
  (setf (gethash #x3d *opcode-names*) "i64.store16")
  (setf (gethash #x3e *opcode-names*) "i64.store32")
  (setf (gethash #x3f *opcode-names*) "memory.size")
  (setf (gethash #x40 *opcode-names*) "memory.grow")

  ;; Numeric Constants
  (setf (gethash #x41 *opcode-names*) "i32.const")
  (setf (gethash #x42 *opcode-names*) "i64.const")
  (setf (gethash #x43 *opcode-names*) "f32.const")
  (setf (gethash #x44 *opcode-names*) "f64.const")

  ;; i32 Comparison
  (setf (gethash #x45 *opcode-names*) "i32.eqz")
  (setf (gethash #x46 *opcode-names*) "i32.eq")
  (setf (gethash #x47 *opcode-names*) "i32.ne")
  (setf (gethash #x48 *opcode-names*) "i32.lt_s")
  (setf (gethash #x49 *opcode-names*) "i32.lt_u")
  (setf (gethash #x4a *opcode-names*) "i32.gt_s")
  (setf (gethash #x4b *opcode-names*) "i32.gt_u")
  (setf (gethash #x4c *opcode-names*) "i32.le_s")
  (setf (gethash #x4d *opcode-names*) "i32.le_u")
  (setf (gethash #x4e *opcode-names*) "i32.ge_s")
  (setf (gethash #x4f *opcode-names*) "i32.ge_u")

  ;; i64 Comparison
  (setf (gethash #x50 *opcode-names*) "i64.eqz")
  (setf (gethash #x51 *opcode-names*) "i64.eq")
  (setf (gethash #x52 *opcode-names*) "i64.ne")
  (setf (gethash #x53 *opcode-names*) "i64.lt_s")
  (setf (gethash #x54 *opcode-names*) "i64.lt_u")
  (setf (gethash #x55 *opcode-names*) "i64.gt_s")
  (setf (gethash #x56 *opcode-names*) "i64.gt_u")
  (setf (gethash #x57 *opcode-names*) "i64.le_s")
  (setf (gethash #x58 *opcode-names*) "i64.le_u")
  (setf (gethash #x59 *opcode-names*) "i64.ge_s")
  (setf (gethash #x5a *opcode-names*) "i64.ge_u")

  ;; f32 Comparison
  (setf (gethash #x5b *opcode-names*) "f32.eq")
  (setf (gethash #x5c *opcode-names*) "f32.ne")
  (setf (gethash #x5d *opcode-names*) "f32.lt")
  (setf (gethash #x5e *opcode-names*) "f32.gt")
  (setf (gethash #x5f *opcode-names*) "f32.le")
  (setf (gethash #x60 *opcode-names*) "f32.ge")

  ;; f64 Comparison
  (setf (gethash #x61 *opcode-names*) "f64.eq")
  (setf (gethash #x62 *opcode-names*) "f64.ne")
  (setf (gethash #x63 *opcode-names*) "f64.lt")
  (setf (gethash #x64 *opcode-names*) "f64.gt")
  (setf (gethash #x65 *opcode-names*) "f64.le")
  (setf (gethash #x66 *opcode-names*) "f64.ge")

  ;; i32 Arithmetic
  (setf (gethash #x67 *opcode-names*) "i32.clz")
  (setf (gethash #x68 *opcode-names*) "i32.ctz")
  (setf (gethash #x69 *opcode-names*) "i32.popcnt")
  (setf (gethash #x6a *opcode-names*) "i32.add")
  (setf (gethash #x6b *opcode-names*) "i32.sub")
  (setf (gethash #x6c *opcode-names*) "i32.mul")
  (setf (gethash #x6d *opcode-names*) "i32.div_s")
  (setf (gethash #x6e *opcode-names*) "i32.div_u")
  (setf (gethash #x6f *opcode-names*) "i32.rem_s")
  (setf (gethash #x70 *opcode-names*) "i32.rem_u")
  (setf (gethash #x71 *opcode-names*) "i32.and")
  (setf (gethash #x72 *opcode-names*) "i32.or")
  (setf (gethash #x73 *opcode-names*) "i32.xor")
  (setf (gethash #x74 *opcode-names*) "i32.shl")
  (setf (gethash #x75 *opcode-names*) "i32.shr_s")
  (setf (gethash #x76 *opcode-names*) "i32.shr_u")
  (setf (gethash #x77 *opcode-names*) "i32.rotl")
  (setf (gethash #x78 *opcode-names*) "i32.rotr")

  ;; i64 Arithmetic
  (setf (gethash #x79 *opcode-names*) "i64.clz")
  (setf (gethash #x7a *opcode-names*) "i64.ctz")
  (setf (gethash #x7b *opcode-names*) "i64.popcnt")
  (setf (gethash #x7c *opcode-names*) "i64.add")
  (setf (gethash #x7d *opcode-names*) "i64.sub")
  (setf (gethash #x7e *opcode-names*) "i64.mul")
  (setf (gethash #x7f *opcode-names*) "i64.div_s")
  (setf (gethash #x80 *opcode-names*) "i64.div_u")
  (setf (gethash #x81 *opcode-names*) "i64.rem_s")
  (setf (gethash #x82 *opcode-names*) "i64.rem_u")
  (setf (gethash #x83 *opcode-names*) "i64.and")
  (setf (gethash #x84 *opcode-names*) "i64.or")
  (setf (gethash #x85 *opcode-names*) "i64.xor")
  (setf (gethash #x86 *opcode-names*) "i64.shl")
  (setf (gethash #x87 *opcode-names*) "i64.shr_s")
  (setf (gethash #x88 *opcode-names*) "i64.shr_u")
  (setf (gethash #x89 *opcode-names*) "i64.rotl")
  (setf (gethash #x8a *opcode-names*) "i64.rotr")

  ;; f32 Arithmetic
  (setf (gethash #x8b *opcode-names*) "f32.abs")
  (setf (gethash #x8c *opcode-names*) "f32.neg")
  (setf (gethash #x8d *opcode-names*) "f32.ceil")
  (setf (gethash #x8e *opcode-names*) "f32.floor")
  (setf (gethash #x8f *opcode-names*) "f32.trunc")
  (setf (gethash #x90 *opcode-names*) "f32.nearest")
  (setf (gethash #x91 *opcode-names*) "f32.sqrt")
  (setf (gethash #x92 *opcode-names*) "f32.add")
  (setf (gethash #x93 *opcode-names*) "f32.sub")
  (setf (gethash #x94 *opcode-names*) "f32.mul")
  (setf (gethash #x95 *opcode-names*) "f32.div")
  (setf (gethash #x96 *opcode-names*) "f32.min")
  (setf (gethash #x97 *opcode-names*) "f32.max")
  (setf (gethash #x98 *opcode-names*) "f32.copysign")

  ;; f64 Arithmetic
  (setf (gethash #x99 *opcode-names*) "f64.abs")
  (setf (gethash #x9a *opcode-names*) "f64.neg")
  (setf (gethash #x9b *opcode-names*) "f64.ceil")
  (setf (gethash #x9c *opcode-names*) "f64.floor")
  (setf (gethash #x9d *opcode-names*) "f64.trunc")
  (setf (gethash #x9e *opcode-names*) "f64.nearest")
  (setf (gethash #x9f *opcode-names*) "f64.sqrt")
  (setf (gethash #xa0 *opcode-names*) "f64.add")
  (setf (gethash #xa1 *opcode-names*) "f64.sub")
  (setf (gethash #xa2 *opcode-names*) "f64.mul")
  (setf (gethash #xa3 *opcode-names*) "f64.div")
  (setf (gethash #xa4 *opcode-names*) "f64.min")
  (setf (gethash #xa5 *opcode-names*) "f64.max")
  (setf (gethash #xa6 *opcode-names*) "f64.copysign")

  ;; Conversions
  (setf (gethash #xa7 *opcode-names*) "i32.wrap_i64")
  (setf (gethash #xa8 *opcode-names*) "i32.trunc_f32_s")
  (setf (gethash #xa9 *opcode-names*) "i32.trunc_f32_u")
  (setf (gethash #xaa *opcode-names*) "i32.trunc_f64_s")
  (setf (gethash #xab *opcode-names*) "i32.trunc_f64_u")
  (setf (gethash #xac *opcode-names*) "i64.extend_i32_s")
  (setf (gethash #xad *opcode-names*) "i64.extend_i32_u")
  (setf (gethash #xae *opcode-names*) "i64.trunc_f32_s")
  (setf (gethash #xaf *opcode-names*) "i64.trunc_f32_u")
  (setf (gethash #xb0 *opcode-names*) "i64.trunc_f64_s")
  (setf (gethash #xb1 *opcode-names*) "i64.trunc_f64_u")
  (setf (gethash #xb2 *opcode-names*) "f32.convert_i32_s")
  (setf (gethash #xb3 *opcode-names*) "f32.convert_i32_u")
  (setf (gethash #xb4 *opcode-names*) "f32.convert_i64_s")
  (setf (gethash #xb5 *opcode-names*) "f32.convert_i64_u")
  (setf (gethash #xb6 *opcode-names*) "f32.demote_f64")
  (setf (gethash #xb7 *opcode-names*) "f64.convert_i32_s")
  (setf (gethash #xb8 *opcode-names*) "f64.convert_i32_u")
  (setf (gethash #xb9 *opcode-names*) "f64.convert_i64_s")
  (setf (gethash #xba *opcode-names*) "f64.convert_i64_u")
  (setf (gethash #xbb *opcode-names*) "f64.promote_f32")

  ;; Reinterpret
  (setf (gethash #xbc *opcode-names*) "i32.reinterpret_f32")
  (setf (gethash #xbd *opcode-names*) "i64.reinterpret_f64")
  (setf (gethash #xbe *opcode-names*) "f32.reinterpret_i32")
  (setf (gethash #xbf *opcode-names*) "f64.reinterpret_i64")

  ;; Sign Extension
  (setf (gethash #xc0 *opcode-names*) "i32.extend8_s")
  (setf (gethash #xc1 *opcode-names*) "i32.extend16_s")
  (setf (gethash #xc2 *opcode-names*) "i64.extend8_s")
  (setf (gethash #xc3 *opcode-names*) "i64.extend16_s")
  (setf (gethash #xc4 *opcode-names*) "i64.extend32_s")

  ;; Reference Instructions
  (setf (gethash #xd0 *opcode-names*) "ref.null")
  (setf (gethash #xd1 *opcode-names*) "ref.is_null")
  (setf (gethash #xd2 *opcode-names*) "ref.func")
  (setf (gethash #xd3 *opcode-names*) "ref.eq")
  (setf (gethash #xd4 *opcode-names*) "ref.as_non_null")

  ;; GC Instructions (after 0xfb prefix)
  (setf (gethash 0 *gc-opcode-names*) "struct.new")
  (setf (gethash 1 *gc-opcode-names*) "struct.new_default")
  (setf (gethash 2 *gc-opcode-names*) "struct.get")
  (setf (gethash 3 *gc-opcode-names*) "struct.get_s")
  (setf (gethash 4 *gc-opcode-names*) "struct.get_u")
  (setf (gethash 5 *gc-opcode-names*) "struct.set")
  (setf (gethash 6 *gc-opcode-names*) "array.new")
  (setf (gethash 7 *gc-opcode-names*) "array.new_default")
  (setf (gethash 8 *gc-opcode-names*) "array.new_fixed")
  (setf (gethash 9 *gc-opcode-names*) "array.new_data")
  (setf (gethash 10 *gc-opcode-names*) "array.new_elem")
  (setf (gethash 11 *gc-opcode-names*) "array.get")
  (setf (gethash 12 *gc-opcode-names*) "array.get_s")
  (setf (gethash 13 *gc-opcode-names*) "array.get_u")
  (setf (gethash 14 *gc-opcode-names*) "array.set")
  (setf (gethash 15 *gc-opcode-names*) "array.len")
  (setf (gethash 16 *gc-opcode-names*) "array.fill")
  (setf (gethash 17 *gc-opcode-names*) "array.copy")
  (setf (gethash 18 *gc-opcode-names*) "array.init_data")
  (setf (gethash 19 *gc-opcode-names*) "array.init_elem")
  (setf (gethash 20 *gc-opcode-names*) "ref.test")
  (setf (gethash 21 *gc-opcode-names*) "ref.test_null")
  (setf (gethash 22 *gc-opcode-names*) "ref.cast")
  (setf (gethash 23 *gc-opcode-names*) "ref.cast_null")
  (setf (gethash 24 *gc-opcode-names*) "br_on_cast")
  (setf (gethash 25 *gc-opcode-names*) "br_on_cast_fail")
  (setf (gethash 26 *gc-opcode-names*) "any.convert_extern")
  (setf (gethash 27 *gc-opcode-names*) "extern.convert_any")
  (setf (gethash 28 *gc-opcode-names*) "ref.i31")
  (setf (gethash 29 *gc-opcode-names*) "i31.get_s")
  (setf (gethash 30 *gc-opcode-names*) "i31.get_u")

  (values))

;; Initialize on load
(init-opcode-names)

(defun opcode-name (opcode)
  "Get WAT mnemonic for an opcode."
  (or (gethash opcode *opcode-names*)
      (format nil "unknown_0x~2,'0x" opcode)))

(defun gc-opcode-name (gc-opcode)
  "Get WAT mnemonic for a GC instruction opcode."
  (or (gethash gc-opcode *gc-opcode-names*)
      (format nil "unknown_gc_~d" gc-opcode)))

;;; Value Type Names

(defun value-type-name (type-byte)
  "Convert type byte to WAT type name."
  (case type-byte
    (#x7f "i32")
    (#x7e "i64")
    (#x7d "f32")
    (#x7c "f64")
    (#x7b "v128")
    (#x70 "funcref")
    (#x6f "externref")
    (#x6e "anyref")
    (#x6d "eqref")
    (#x6c "i31ref")
    (#x6b "structref")
    (#x6a "arrayref")
    (#x40 "")  ; void
    (t (format nil "(type ~d)" type-byte))))

;;; Stack Signature Table for S-expression Conversion

(defparameter *opcode-stack-signature* (make-hash-table :test 'eql)
  "opcode → (consume . produce) mapping for stack simulation.")

(defun init-stack-signatures ()
  "Initialize stack signatures for all opcodes."
  (clrhash *opcode-stack-signature*)

  ;; Format: (consume . produce) where consume = stack args, produce = stack results

  ;; Constants: consume 0, produce 1
  (dolist (op '(#x41 #x42 #x43 #x44))  ; i32/i64/f32/f64.const
    (setf (gethash op *opcode-stack-signature*) '(0 . 1)))

  ;; Variable get: consume 0, produce 1
  (dolist (op '(#x20 #x23))  ; local.get, global.get
    (setf (gethash op *opcode-stack-signature*) '(0 . 1)))

  ;; Variable set: consume 1, produce 0
  (dolist (op '(#x21 #x24))  ; local.set, global.set
    (setf (gethash op *opcode-stack-signature*) '(1 . 0)))

  ;; Variable tee: consume 1, produce 1
  (setf (gethash #x22 *opcode-stack-signature*) '(1 . 1))  ; local.tee

  ;; Unary operators: consume 1, produce 1
  (dolist (op '(#x45 #x50  ; eqz
                #x67 #x68 #x69  ; clz, ctz, popcnt (i32)
                #x79 #x7a #x7b  ; clz, ctz, popcnt (i64)
                #x8b #x8c #x8d #x8e #x8f #x90 #x91  ; f32 unary
                #x99 #x9a #x9b #x9c #x9d #x9e #x9f  ; f64 unary
                #xa7 #xa8 #xa9 #xaa #xab  ; i32 conversions
                #xac #xad #xae #xaf #xb0 #xb1  ; i64 conversions
                #xb2 #xb3 #xb4 #xb5 #xb6  ; f32 conversions
                #xb7 #xb8 #xb9 #xba #xbb  ; f64 conversions
                #xbc #xbd #xbe #xbf  ; reinterpret
                #xc0 #xc1 #xc2 #xc3 #xc4  ; sign extension
                #xd1))  ; ref.is_null
    (setf (gethash op *opcode-stack-signature*) '(1 . 1)))

  ;; Binary operators: consume 2, produce 1
  (dolist (op '(#x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f  ; i32 compare
                #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a  ; i64 compare
                #x5b #x5c #x5d #x5e #x5f #x60  ; f32 compare
                #x61 #x62 #x63 #x64 #x65 #x66  ; f64 compare
                #x6a #x6b #x6c #x6d #x6e #x6f #x70  ; i32 arithmetic
                #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78  ; i32 bitwise
                #x7c #x7d #x7e #x7f #x80 #x81 #x82  ; i64 arithmetic
                #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8a  ; i64 bitwise
                #x92 #x93 #x94 #x95 #x96 #x97 #x98  ; f32 binary
                #xa0 #xa1 #xa2 #xa3 #xa4 #xa5 #xa6  ; f64 binary
                #xd3))  ; ref.eq
    (setf (gethash op *opcode-stack-signature*) '(2 . 1)))

  ;; Memory load: consume 1 (address), produce 1
  (dolist (op '(#x28 #x29 #x2a #x2b  ; load
                #x2c #x2d #x2e #x2f  ; load8/16
                #x30 #x31 #x32 #x33 #x34 #x35))  ; i64 load variants
    (setf (gethash op *opcode-stack-signature*) '(1 . 1)))

  ;; Memory store: consume 2 (address, value), produce 0
  (dolist (op '(#x36 #x37 #x38 #x39  ; store
                #x3a #x3b  ; store8/16 (i32)
                #x3c #x3d #x3e))  ; store8/16/32 (i64)
    (setf (gethash op *opcode-stack-signature*) '(2 . 0)))

  ;; Drop: consume 1, produce 0
  (setf (gethash #x1a *opcode-stack-signature*) '(1 . 0))

  ;; Select: consume 3, produce 1
  (setf (gethash #x1b *opcode-stack-signature*) '(3 . 1))

  ;; Memory size: consume 0, produce 1
  (setf (gethash #x3f *opcode-stack-signature*) '(0 . 1))

  ;; Memory grow: consume 1, produce 1
  (setf (gethash #x40 *opcode-stack-signature*) '(1 . 1))

  ;; ref.null: consume 0, produce 1
  (setf (gethash #xd0 *opcode-stack-signature*) '(0 . 1))

  ;; ref.func: consume 0, produce 1
  (setf (gethash #xd2 *opcode-stack-signature*) '(0 . 1))

  (values))

(init-stack-signatures)

(defun get-stack-signature (opcode)
  "Get stack signature (consume . produce) for opcode."
  (or (gethash opcode *opcode-stack-signature*)
      '(0 . 1)))  ; Default: produce 1 value

;;; Instruction Analysis

(defun instr-opcode (instr)
  "Extract opcode from instruction (handles both atom and list forms)."
  (if (consp instr)
      (car instr)
      instr))

(defun instr-args (instr)
  "Extract arguments from instruction."
  (if (consp instr)
      (cdr instr)
      nil))

(defun block-instr-p (opcode)
  "Check if opcode starts a block structure."
  (member opcode '(#x02 #x03 #x04)))  ; block, loop, if

(defun end-instr-p (opcode)
  "Check if opcode ends a block."
  (= opcode #x0b))

(defun else-instr-p (opcode)
  "Check if opcode is else."
  (= opcode #x05))

;;; S-expression Tree Building

(defstruct wat-node
  "Node in WAT expression tree."
  opcode
  args
  children
  source-form)

(defun build-expression-tree (instructions)
  "Convert flat instruction list to S-expression tree.
   Uses stack simulation to reconstruct nesting."
  (let ((stack nil)
        (block-stack nil))
    (dolist (instr instructions)
      (let* ((opcode (instr-opcode instr))
             (args (instr-args instr)))
        (cond
          ;; End of block
          ((end-instr-p opcode)
           (when block-stack
             (let* ((block-content (nreverse (pop block-stack)))
                    (block-node (car stack)))
               (when (wat-node-p block-node)
                 (setf (wat-node-children block-node) block-content)))))

          ;; Else in if block
          ((else-instr-p opcode)
           ;; Mark else position in current block
           nil)

          ;; Start of new block
          ((block-instr-p opcode)
           (let ((node (make-wat-node :opcode opcode :args args)))
             (push node stack)
             (push nil block-stack)))

          ;; Regular instruction
          (t
           (let ((sig (get-stack-signature opcode))
                 (node (make-wat-node :opcode opcode :args args)))
             ;; Pop consumed values from stack
             (let ((consumed nil))
               (dotimes (i (car sig))
                 (when stack
                   (push (pop stack) consumed)))
               (setf (wat-node-children node) consumed))
             ;; Push result if produces value
             (when (plusp (cdr sig))
               (push node stack)))))))
    ;; Return top of stack
    (car stack)))

;;; WAT Formatting

(defun format-wat-indent (stream indent)
  "Write indentation."
  (dotimes (i indent)
    (write-char #\Space stream)))

(defun format-wat-node (node stream indent)
  "Format a WAT node as S-expression."
  (when (wat-node-p node)
    (let* ((opcode (wat-node-opcode node))
           (args (wat-node-args node))
           (children (wat-node-children node))
           (name (opcode-name opcode)))
      ;; Format based on structure
      (cond
        ;; Block instructions with children
        ((and (block-instr-p opcode) children)
         (format-wat-indent stream indent)
         (format stream "(~a" name)
         (when args
           (format stream " ~a" (format-block-type (first args))))
         (format stream "~%")
         (dolist (child children)
           (format-wat-node child stream (+ indent 2)))
         (format-wat-indent stream indent)
         (format stream ")~%"))

        ;; Instructions with stack children (folded form)
        ((and children (every #'wat-node-p children))
         (format-wat-indent stream indent)
         (format stream "(~a~{ ~a~}" name args)
         (format stream "~%")
         (dolist (child (reverse children))
           (format-wat-node child stream (+ indent 2)))
         (format-wat-indent stream indent)
         (format stream ")~%"))

        ;; Simple instruction
        (t
         (format-wat-indent stream indent)
         (format stream "(~a~{ ~a~})~%" name args))))))

(defun format-block-type (type)
  "Format block type annotation."
  (cond
    ((null type) "")
    ((= type #x40) "")  ; void
    ((numberp type)
     (format nil "(result ~a)" (value-type-name type)))
    (t "")))

;;; Flat Format (simpler, wabt-compatible)

(defun format-instruction-flat (instr stream indent)
  "Format single instruction in flat (non-folded) format."
  (let ((opcode (instr-opcode instr))
        (args (instr-args instr)))
    (format-wat-indent stream indent)
    (let ((name (opcode-name opcode)))
      (cond
        ;; Block type instructions
        ((block-instr-p opcode)
         (format stream "~a~@[ ~a~]~%"
                 name
                 (when args (format-block-type (first args)))))

        ;; Memory instructions (align, offset)
        ((and (>= opcode #x28) (<= opcode #x3e))
         (if (and args (= (length args) 2))
             (let ((align (first args))
                   (offset (second args)))
               (format stream "~a~@[ offset=~d~]~@[ align=~d~]~%"
                       name
                       (when (plusp offset) offset)
                       (when (/= align 2) (ash 1 align))))
             (format stream "~a~{ ~a~}~%" name args)))

        ;; call_indirect (type, table)
        ((= opcode #x11)
         (format stream "~a (type ~d)~%" name (first args)))

        ;; Regular instruction
        (t
         (format stream "~a~{ ~a~}~%" name args))))))

;;; Module Section Formatters

(defun format-type-section (module stream)
  "Format type section."
  (let ((idx 0))
    (dolist (type (wasm-module-types module))
      (when (func-type-p type)
        (format stream "  (type (;~d;) (func" idx)
        (when (func-type-params type)
          (dolist (p (func-type-params type))
            (format stream " (param ~a)" (value-type-name p))))
        (when (func-type-results type)
          (dolist (r (func-type-results type))
            (format stream " (result ~a)" (value-type-name r))))
        (format stream "))~%")
        (incf idx)))))

(defun format-memory-section (module stream)
  "Format memory section."
  (dolist (mem (wasm-module-memories module))
    (format stream "  (memory ~d~@[ ~d~])~%"
            (wasm-memory-min mem)
            (wasm-memory-max mem))))

(defun format-table-section (module stream)
  "Format table section."
  (dolist (table (wasm-module-tables module))
    (format stream "  (table ~d~@[ ~d~] ~a)~%"
            (wasm-table-min table)
            (wasm-table-max table)
            (value-type-name (wasm-table-element-type table)))))

(defun format-global-section (module stream)
  "Format global section."
  (let ((idx 0))
    (dolist (global (wasm-module-globals module))
      (format stream "  (global (;~d;) (~a ~a) "
              idx
              (if (wasm-global-mutable global) "mut" "")
              (value-type-name (wasm-global-type global)))
      ;; Format init expression
      (dolist (instr (wasm-global-init global))
        (let ((opcode (instr-opcode instr))
              (args (instr-args instr)))
          (format stream "(~a~{ ~a~})" (opcode-name opcode) args)))
      (format stream ")~%")
      (incf idx))))

(defun format-export-section (module stream)
  "Format export section."
  (dolist (export (wasm-module-exports module))
    (let ((kind-name (case (wasm-export-kind export)
                       (0 "func")
                       (1 "table")
                       (2 "memory")
                       (3 "global")
                       (t "unknown"))))
      (format stream "  (export ~s (~a ~d))~%"
              (wasm-export-name export)
              kind-name
              (wasm-export-idx export)))))

(defun format-data-section (module stream)
  "Format data section."
  (dolist (data (wasm-module-data module))
    (format stream "  (data ")
    ;; Offset expression
    (dolist (instr (wasm-data-offset data))
      (let ((opcode (instr-opcode instr))
            (args (instr-args instr)))
        (format stream "(~a~{ ~a~}) " (opcode-name opcode) args)))
    ;; Data bytes as string
    (format stream "~s)~%"
            (map 'string #'code-char (wasm-data-data data)))))

(defun format-function-body (func stream source-form)
  "Format a function body."
  ;; Source comment
  (when source-form
    (format stream "  ;; Lisp: ~S~%" source-form))
  ;; Function header
  (format stream "  (func (;~d;) (type ~d)"
          0  ; Will be filled by caller
          (wasm-func-type-idx func))
  ;; Locals
  (dolist (local (wasm-func-locals func))
    (dotimes (i (car local))
      (format stream " (local ~a)" (value-type-name (cdr local)))))
  (format stream "~%")
  ;; Body (flat format for now)
  (dolist (instr (wasm-func-body func))
    (format-instruction-flat instr stream 4))
  (format stream "  )~%"))

;;; Main API

(defun module-to-wat (module &key (stream *standard-output*)
                                  (source-info nil)
                                  (style :flat))
  "Convert wasm-module to WAT text format.
   STYLE can be :flat (default) or :folded."
  (format stream "(module~%")
  (format-type-section module stream)
  (format-memory-section module stream)
  (format-table-section module stream)
  (format-global-section module stream)
  ;; Functions with optional source comments
  (let ((idx (wasm-module-import-func-count module)))
    (dolist (func (wasm-module-functions module))
      (let ((src (when source-info
                   (nth (- idx (wasm-module-import-func-count module))
                        source-info))))
        (when src
          (format stream "  ;; Lisp: ~S~%" src))
        (format stream "  (func (;~d;) (type ~d)"
                idx (wasm-func-type-idx func))
        ;; Locals
        (dolist (local (wasm-func-locals func))
          (dotimes (i (car local))
            (format stream " (local ~a)" (value-type-name (cdr local)))))
        (format stream "~%")
        ;; Body
        (ecase style
          (:flat
           (dolist (instr (wasm-func-body func))
             (format-instruction-flat instr stream 4)))
          (:folded
           (let ((tree (build-expression-tree (wasm-func-body func))))
             (when tree
               (format-wat-node tree stream 4)))))
        (format stream "  )~%")
        (incf idx))))
  (format-export-section module stream)
  (format-data-section module stream)
  (format stream ")~%"))

(defun save-module-as-wat (module filename &key source-info (style :flat))
  "Save module as WAT file."
  (with-open-file (out filename :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
    (module-to-wat module :stream out
                          :source-info source-info
                          :style style))
  filename)
