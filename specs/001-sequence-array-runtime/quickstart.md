# Quickstart: Sequence and Array Runtime Migration

**Feature**: 001-sequence-array-runtime
**Date**: 2026-01-04

## Overview

This feature migrates `subseq` and `adjust-array` from inline Wasm codegen to the Lisp runtime library, following the established pattern from previous runtime migrations.

## After Implementation

### Using subseq

```lisp
;; String subseq - UTF-8 aware, indices are character positions
(subseq "hello" 1 3)    ; => "el"
(subseq "日本語" 1)     ; => "本語"
(subseq "café" 0 4)     ; => "café"

;; List subseq
(subseq '(a b c d e) 1 3)  ; => (b c)
(subseq '(1 2 3) 1 1)      ; => nil (empty)

;; Vector subseq
(subseq #(1 2 3 4 5) 2)    ; => #(3 4 5)
(subseq #(a b c) 0 2)      ; => #(a b)
```

### Using adjust-array

```lisp
;; Grow array with initial element
(adjust-array #(1 2 3) 5 :initial-element 0)
; => #(1 2 3 0 0)

;; Shrink array
(adjust-array #(1 2 3 4 5) 3)
; => #(1 2 3)

;; Dimensions as list
(adjust-array #(a b c) '(4) :initial-element 'x)
; => #(a b c x)
```

## Verifying the Migration

### 1. Check Runtime Registration

```lisp
;; In SBCL REPL after loading clysm
(gethash 'subseq clysm::*runtime-function-table*)
; => (:$subseq-rt)

(gethash 'adjust-array clysm::*runtime-function-table*)
; => (:$adjust-array-rt)
```

### 2. Run Unit Tests

```bash
sbcl --eval "(asdf:test-system :clysm)"
```

Expected: All tests pass, including new sequence-array-runtime tests.

### 3. Verify Stage 1 Compilation

```bash
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm
```

Expected: Exit code 0 from both commands.

### 4. Verify Dead Code Removal

```bash
grep -c "compile-subseq\|compile-adjust-array" src/clysm/compiler/codegen/func-section.lisp
```

Expected: 0 (no matches)

## Files Modified

| File | Change |
|------|--------|
| `src/clysm/lib/sequence-runtime.lisp` | Added subseq-rt, adjust-array-rt |
| `src/clysm/compiler/codegen/func-section.lisp` | Removed compile-subseq, compile-adjust-array; added registration |
| `tests/unit/sequence-array-runtime-test.lisp` | New test file |

## Common Issues

### UTF-8 String Indices

Indices refer to **character positions**, not byte positions:

```lisp
;; "日" is 3 bytes in UTF-8, but index 0
(subseq "日本語" 0 1)  ; => "日" (1 character, 3 bytes)
```

### Multidimensional Arrays

adjust-array currently only supports 1D arrays:

```lisp
;; This will signal an error
(adjust-array (make-array '(2 3)) '(3 4))
; => error: Only 1D arrays supported
```

## Related Documentation

- [subseq](resources/HyperSpec/Body/f_subseq.htm) - ANSI CL specification
- [adjust-array](resources/HyperSpec/Body/f_adjust.htm) - ANSI CL specification
- `001-string-runtime-migration` - UTF-8 helper functions
- `001-sequence-runtime-migration` - Runtime dispatch pattern
