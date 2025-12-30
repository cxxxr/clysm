# Quickstart: Phase 15C - ANSI Array Operations

## Overview

This guide helps you implement the 9 ANSI Common Lisp array functions for WasmGC codegen.

## Prerequisites

1. Understand existing array infrastructure:
   ```bash
   # Read current array primitives
   cat src/clysm/compiler/codegen/func-section.lisp | grep -A 20 "compile-aref"
   ```

2. Review WasmGC types:
   ```bash
   cat src/clysm/compiler/codegen/gc-types.lisp | grep "type-"
   ```

## Implementation Steps

### Step 1: Add $mdarray Type (gc-types.lisp)

```lisp
;; After +type-bucket-array+ (27)
(defconstant +type-mdarray+ 28 "Type index for multidimensional array wrapper")
```

Add to `emit-type-section`:
```lisp
;; $mdarray struct: dimensions list + storage array + adjustable flag
(emit-struct-type
  `((ref ,+type-cons+)      ; $dimensions
    (ref ,+type-mv-array+)  ; $storage
    i32))                   ; $adjustable
```

### Step 2: Implement Metadata Queries (func-section.lisp)

Start with the simplest function:

```lisp
(defun compile-array-total-size (args env)
  "Stack: [] -> [i31ref]"
  (let ((array-code (compile-form (first args) env)))
    (append
      array-code
      ;; Type dispatch: $mdarray or $mv_array?
      `((ref.test ,+type-mdarray+)
        (if (result i31ref)
          ;; $mdarray: get storage array length
          (block
            (ref.cast (ref ,+type-mdarray+))
            (struct.get ,+type-mdarray+ 1)  ; $storage
            (array.len)
            (i31.new))
          ;; $mv_array: direct length
          (block
            (ref.cast (ref ,+type-mv-array+))
            (array.len)
            (i31.new)))))))
```

### Step 3: Register in *builtin-compilers*

```lisp
(defparameter *builtin-compilers*
  (list
    ;; ... existing entries ...
    (cons 'array-rank #'compile-array-rank)
    (cons 'array-dimension #'compile-array-dimension)
    (cons 'array-dimensions #'compile-array-dimensions)
    (cons 'array-total-size #'compile-array-total-size)
    (cons 'array-row-major-index #'compile-array-row-major-index)
    (cons 'row-major-aref #'compile-row-major-aref)
    (cons '%setf-row-major-aref #'compile-setf-row-major-aref)
    (cons 'adjustable-array-p #'compile-adjustable-array-p)
    (cons 'adjust-array #'compile-adjust-array)))
```

### Step 4: Add Setf Expander (setf-expanders.lisp)

```lisp
(register-setf-expander
  'row-major-aref
  (make-row-major-aref-setf-expander))
```

## Testing

### Run Unit Tests

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
```

### Validate Generated Wasm

```bash
sbcl --load build/stage0-complete.lisp
wasm-tools validate dist/clysm-stage0.wasm
```

### Integration Test Example

```lisp
;; In tests/integration/array-ops-test.lisp
(deftest test-array-rank
  (let ((result (compile-and-run '(array-rank (make-array '(2 3 4))))))
    (ok (= result 3))))
```

## Common Patterns

### Type Dispatch Pattern

```lisp
;; Check if value is $mdarray
(ref.test +type-mdarray+)
(if (result ...)
  ;; Handle multidimensional array
  (block ...)
  ;; Handle simple-vector
  (block ...))
```

### Accessing $mdarray Fields

```lisp
;; Field 0: $dimensions (cons list)
(struct.get +type-mdarray+ 0)

;; Field 1: $storage (mv_array)
(struct.get +type-mdarray+ 1)

;; Field 2: $adjustable (i32)
(struct.get +type-mdarray+ 2)
```

### Converting Fixnum to i32

```lisp
;; i31ref -> i32
(i31.get_s)

;; i32 -> i31ref
(i31.new)
```

## Debugging Tips

1. **Print WAT for inspection**:
   ```lisp
   (let ((wasm (compile-to-wasm '(array-rank #(1 2 3)))))
     (wasm-tools-print wasm))
   ```

2. **Check stack types**:
   Ensure each function leaves exactly one value on stack.

3. **Test simple cases first**:
   Start with simple-vectors before testing multidimensional arrays.

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/gc-types.lisp` | Add `+type-mdarray+` |
| `src/clysm/compiler/codegen/func-section.lisp` | Add 9 compile-* functions |
| `src/clysm/lib/setf-expanders.lisp` | Add row-major-aref expander |
| `tests/unit/array-ops-test.lisp` | Unit tests |
| `tests/contract/array-ops-wasm-test.lisp` | Wasm validation tests |
| `tests/integration/array-ops-test.lisp` | Runtime tests |

## HyperSpec References

- [array-rank](../../resources/HyperSpec/Body/f_ar_ran.htm)
- [array-dimension](../../resources/HyperSpec/Body/f_ar_dim.htm)
- [array-dimensions](../../resources/HyperSpec/Body/f_ar_di1.htm)
- [array-total-size](../../resources/HyperSpec/Body/f_ar_tot.htm)
- [array-row-major-index](../../resources/HyperSpec/Body/f_ar_row.htm)
- [row-major-aref](../../resources/HyperSpec/Body/f_row_ma.htm)
- [adjustable-array-p](../../resources/HyperSpec/Body/f_adjust.htm)
- [adjust-array](../../resources/HyperSpec/Body/f_adj_ar.htm)
