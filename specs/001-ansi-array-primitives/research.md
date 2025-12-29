# Research: ANSI CL Array/Sequence Primitives

**Feature**: 001-ansi-array-primitives
**Date**: 2025-12-29
**Status**: Complete

## Executive Summary

This research establishes the implementation approach for array/sequence primitives in Clysm. Key findings:

1. **Type Reuse**: Type 22 (`$mv_array`) can be reused for simple-vectors; no new type needed
2. **Existing Pattern**: `(:array.get type)` and `(:array.set type)` instructions already work correctly
3. **Setf Infrastructure**: Setf expander for `aref` exists but lacks `%setf-aref` primitive
4. **String Access**: Type 4 (`$string`) with `array.get_u` for unsigned byte access

## WasmGC Array Instructions

### Available Instructions (from compiler.lisp)

| Instruction | Opcode | Stack Effect | Usage |
|-------------|--------|--------------|-------|
| `array.get` | 0xFB 0x0B | `[arrayref, i32] -> [anyref]` | Get element from anyref array |
| `array.get_u` | 0xFB 0x0D | `[arrayref, i32] -> [i32]` | Get unsigned element (for i8/i16 arrays) |
| `array.set` | 0xFB 0x0E | `[arrayref, i32, value] -> []` | Set element in array |
| `array.len` | 0xFB 0x0F | `[arrayref] -> [i32]` | Get array length |
| `array.new_fixed` | 0xFB 0x08 | `[v1..vn] -> [arrayref]` | Create array with n elements from stack |
| `array.new_default` | 0xFB 0x07 | `[i32] -> [arrayref]` | Create array with default values |

**Source**: `src/clysm/compiler/compiler.lisp:876-899`

### Type Indices

| Index | Type | Element Type | Usage |
|-------|------|--------------|-------|
| 4 | `$string` | `i8` (mutable) | String characters (UTF-8 bytes) |
| 22 | `$mv_array` | `anyref` (mutable) | Multiple values buffer, **reuse for simple-vector** |

**Source**: `src/clysm/compiler/codegen/gc-types.lisp`

## Existing Implementation Patterns

### Pattern 1: Multiple-Value-Bind (from func-section.lisp:12550-12552)

```lisp
;; Access mv-buffer at index
(:global.get ,mv-buffer-global)  ; Get array reference
(:i32.const ,idx)                 ; Push index
(:array.get ,mv-array-type)       ; Get element (type 22)
```

**Decision**: Use this exact pattern for `aref`/`svref` on simple-vectors.

### Pattern 2: String Character Access (from compiler.lisp:893-898)

```lisp
;; Get unsigned byte from string
(:array.get_u 4)  ; Type 4 = $string
```

**Decision**: Use for `schar`, then wrap result in i31ref for character representation.

### Pattern 3: Setf Expanders (from setf-expanders.lisp:294-310)

```lisp
(defun make-aref-setf-expander ()
  "Create setf expander for AREF.
   Feature 043: Uses %setf-aref primitive to avoid infinite recursion."
  (lambda (form env)
    ...
    `(%setf-aref ,array-temp ,store ,@index-temps)))
```

**Decision**: Implement `%setf-aref` primitive that compiles to `array.set`.

## Implementation Approach

### 1. aref / svref

**Compile Pattern**:
```
(aref vec idx)
→
compile(vec)           ; pushes arrayref
compile(idx)           ; pushes i31ref
(:ref.cast :i31)       ; ensure fixnum
:i31.get_s             ; extract i32 index
(:array.get 22)        ; get element
```

**Rationale**: Simple-vectors use type 22 (same as mv_array). Index must be converted from Lisp fixnum to Wasm i32.

### 2. schar

**Compile Pattern**:
```
(schar str idx)
→
compile(str)           ; pushes string ref
compile(idx)           ; pushes i31ref
(:ref.cast :i31)       ; ensure fixnum
:i31.get_s             ; extract i32 index
(:array.get_u 4)       ; get unsigned byte
:ref.i31               ; wrap byte as i31ref (character)
```

**Rationale**: Strings are UTF-8 byte arrays. For ASCII, byte value equals code point. Unicode multi-byte handling is complex and may need runtime support for character-by-character indexing.

**Alternative Considered**: Character indexing vs byte indexing. ANSI CL specifies character indexing. For Phase 13D-1, assume ASCII-compatible strings (1 byte = 1 char). Document as assumption.

### 3. elt

**Compile Pattern** (type dispatch):
```
(elt seq idx)
→
compile(seq)                      ; pushes anyref
(:local.tee ,seq-local)           ; save for type tests
(:ref.test (:ref 22))             ; test: simple-vector?
(:if (:result :anyref))
  ;; Vector path
  (:local.get ,seq-local)
  (:ref.cast (:ref 22))
  compile(idx) -> i32
  (:array.get 22)
:else
  (:local.get ,seq-local)
  (:ref.test (:ref 4))            ; test: string?
  (:if (:result :anyref))
    ;; String path
    ...(:array.get_u 4)...
  :else
    ;; List path: runtime call to nth
    (:call ,nth-func-idx)
  :end
:end
```

**Rationale**: `elt` is generic across sequence types. Type dispatch at runtime using `ref.test`.

### 4. coerce

**Compile-Time Expansion**:
```lisp
(coerce seq 'vector)
→ (if (vectorp seq) seq (%list-to-vector seq))

(coerce seq 'list)
→ (if (listp seq) seq (%vector-to-list seq))
```

**Rationale**: `coerce` result type is known at compile time in most cases. Generate inline code with runtime helpers for actual conversion.

### 5. Setf Forms

**%setf-aref Compile Pattern**:
```
(%setf-aref array value idx)
→
compile(array)         ; arrayref
compile(idx)           ; convert to i32
compile(value)         ; value to store
(:array.set 22)        ; set element
compile(value)         ; return the value (setf returns stored value)
```

## Dependencies Identified

1. **Type 22 ($mv_array)**: Already defined, reuse for simple-vectors
2. **Setf Expander Registry**: Already has `aref` expander, needs `svref`, `schar`, `elt`
3. **Runtime `nth` Function**: Needed for `elt` on lists - may already exist or need implementation
4. **List-to-Vector Conversion**: Runtime helper for `coerce`

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| UTF-8 multi-byte characters in `schar` | Character indexing incorrect for non-ASCII | Document as limitation; ASCII-only for Phase 13D-1 |
| Type 22 conflicts with mv-buffer | Unlikely since both are `(array (mut anyref))` | Verify no semantic conflicts |
| `elt` on lists performance | O(n) access | Accept for correctness; optimize later |

## Recommendations

1. **Implement in Priority Order**: aref/svref (P1) → coerce (P2) → schar (P3) → elt (P4)
2. **Test with defstruct**: Primary goal is defstruct accessor compilation
3. **ASCII Assumption**: Document `schar` limitation for UTF-8 multi-byte
4. **Runtime Helpers**: Create `%list-to-vector` and `%vector-to-list` for coerce

## HyperSpec References

Per Constitution IX, all ANSI CL references:

- [aref](../../resources/HyperSpec/Body/f_aref.htm) - Array element access
- [svref](../../resources/HyperSpec/Body/f_svref.htm) - Simple-vector element access
- [schar](../../resources/HyperSpec/Body/f_schar.htm) - Simple string character access
- [elt](../../resources/HyperSpec/Body/f_elt.htm) - Generic sequence element access
- [coerce](../../resources/HyperSpec/Body/f_coerce.htm) - Type coercion
- [simple-vector](../../resources/HyperSpec/Body/t_smp_ve.htm) - Simple vector type
- [sequence](../../resources/HyperSpec/Body/t_seq.htm) - Sequence type specifier
