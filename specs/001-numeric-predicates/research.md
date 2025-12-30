# Research: Phase 14B - Numeric Type Predicates Enhancement

**Feature**: 001-numeric-predicates
**Date**: 2025-12-30

## Findings Summary

### 1. Existing Implementation Analysis

**Already Implemented** (no work needed):
- [zerop](resources/HyperSpec/Body/f_zerop.htm) - `func-section.lisp:4414-4456`
- [plusp](resources/HyperSpec/Body/f_minusp.htm) - `func-section.lisp:4458-4501`
- [minusp](resources/HyperSpec/Body/f_minusp.htm) - `func-section.lisp:4503-4546`
- [oddp](resources/HyperSpec/Body/f_evenpc.htm) - `func-section.lisp:4548-4574`
- [evenp](resources/HyperSpec/Body/f_evenpc.htm) - `func-section.lisp:4576-4603`

**Functions to Implement** (10 total):
- [logbitp](resources/HyperSpec/Body/f_logbtp.htm) - bit testing
- [logtest](resources/HyperSpec/Body/f_logtes.htm) - bit intersection test
- [byte](resources/HyperSpec/Body/f_by_by.htm) - byte specifier constructor
- [byte-size](resources/HyperSpec/Body/f_by_by.htm) - byte specifier accessor
- [byte-position](resources/HyperSpec/Body/f_by_by.htm) - byte specifier accessor
- [ldb](resources/HyperSpec/Body/f_ldb.htm) - load byte
- [dpb](resources/HyperSpec/Body/f_dpb.htm) - deposit byte
- [mask-field](resources/HyperSpec/Body/f_mask_f.htm) - mask byte field
- [deposit-field](resources/HyperSpec/Body/f_deposi.htm) - deposit byte field

### 2. Byte Specifier Encoding Decision

**Decision**: Fixnum encoding using `(logior (ash size 6) position)`

**Rationale**:
1. **Zero allocation** - byte specifiers are immediate i31ref values
2. **Compile-time folding** - `(byte 8 0)` becomes constant `512` (8 << 6)
3. **Precedent** - SBCL, CCL, CMUCL all use fixnum encoding
4. **Wasm efficiency** - No heap allocation, no type test needed

**Alternatives Considered**:

| Option | Pros | Cons | Rejected Because |
|--------|------|------|------------------|
| Cons cell `(size . position)` | Simple, readable | Allocates, needs type test | Overhead for common operation |
| WasmGC struct `$byte-specifier` | Type-safe | New type index, allocates | Adds complexity, slower |
| Two-word encoding | Larger range | Uses more registers | Overkill for 32-bit integers |

**Encoding Details**:
```
Byte specifier = (size << 6) | position
- Bits 0-5:  position (0-63)
- Bits 6-11: size (0-63)
- Total: 12 bits, fits easily in i31ref (31 bits)

Examples:
  (byte 8 0)  = (8 << 6) | 0  = 512
  (byte 4 4)  = (4 << 6) | 4  = 260
  (byte 1 31) = (1 << 6) | 31 = 95
```

### 3. Implementation Patterns from Existing Code

**Predicate Pattern** (`compile-evenp`, line 4576):
```lisp
(defun compile-evenp (args env)
  (let ((temp-local (env-add-local env (gensym "EVENP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum path
       (:local.get ,temp-local) (:ref.cast :i31) :i31.get_s
       (:i32.const 1) :i32.and :i32.eqz
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else (:ref.null :none) :end
       :else (:ref.null :none) :end))))
```

**Bit Operation Pattern** (`compile-ash`, line 2837):
```lisp
(defun compile-ash (args env)
  ;; Extract both args, test count sign, branch to shl or shr_s
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s)
   ;; Direction-based shift
   `((:if (:result :i32)) :i32.shl :else ... :i32.shr_s :end)
   '(:ref.i31)))
```

### 4. Wasm Instruction Mapping

| CL Function | Wasm Instructions |
|-------------|-------------------|
| `logbitp` | `i32.shr_u`, `i32.and 1`, `i32.eqz` (inverted) |
| `logtest` | `i32.and`, `i32.eqz` (inverted) |
| `byte` | `i32.shl 6`, `i32.or` |
| `byte-size` | `i32.shr_u 6` |
| `byte-position` | `i32.and 63` |
| `ldb` | `i32.shr_u position`, `i32.and mask` |
| `dpb` | `i32.and ~field-mask`, `i32.or (val << pos)` |
| `mask-field` | `i32.and field-mask` |
| `deposit-field` | `i32.and ~field-mask`, `i32.or (val & field-mask)` |

### 5. Edge Case Handling

**logbitp with large index**: Per ANSI CL, for non-negative integer n greater than integer-length, return the sign bit (false for positive integers).
```lisp
;; Implementation: if index >= 31, return sign bit test
;; (logbitp 50 5) => NIL (5 is positive, conceptually infinite 0s)
;; (logbitp 50 -1) => T (-1 is negative, conceptually infinite 1s)
```

**Empty byte specifier (size 0)**:
- `(ldb (byte 0 n) x)` => always 0
- `(dpb val (byte 0 n) x)` => x unchanged

**Byte specifier bounds**: Size and position up to 63 are supported within i31ref fixnum range.

### 6. Registration Requirements

New functions must be added to:
1. `func-section.lisp` dispatch table (~line 920)
2. Export list for Stage 1 generation
3. Feature registry (`feature-registry.lisp`)

## Next Steps

1. Write unit tests following TDD (Constitution VII)
2. Implement `compile-logbitp` and `compile-logtest`
3. Implement byte specifier functions
4. Implement byte manipulation functions
5. Validate Wasm output
6. Update feature registry
