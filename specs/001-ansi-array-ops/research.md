# Research: Phase 15C - ANSI Array Operations

**Date**: 2025-12-31
**Status**: Complete

## 1. Multidimensional Array Representation

### Decision: Wrapper Struct with Dimension Metadata

Represent multidimensional arrays as a new WasmGC struct type containing:
1. Dimension list (stored as cons list for ANSI CL compatibility)
2. Storage array (reuse existing `$mv_array` type 20)
3. Adjustable flag (i31ref 0 or 1)

### Rationale

- **Separation of concerns**: Metadata is distinct from element storage
- **Backward compatibility**: Simple-vectors remain raw `$mv_array` with no wrapper
- **Type dispatch**: Use `ref.test` to distinguish `$mdarray` from `$mv_array`
- **Memory efficiency**: Only multidimensional arrays incur the struct overhead

### Alternatives Considered

1. **Inline metadata in array**: Store rank and dimensions in first N slots
   - Rejected: Complicates `aref` index calculation, breaks simple-vector semantics

2. **Property list on symbol**: Store metadata externally keyed by array identity
   - Rejected: Requires hash lookup, breaks with array copying

3. **Uniform wrapper for all arrays**: Always use `$mdarray` even for 1D
   - Rejected: Performance regression for common case (simple-vectors)

## 2. WasmGC Type Definition

### Decision: Type Index 28 for `$mdarray`

```wat
(type $mdarray (struct
  (field $dimensions (ref $cons))       ;; List of dimension sizes as fixnums
  (field $storage (ref $mv_array))      ;; Underlying 1D storage
  (field $adjustable i32)               ;; 0 = fixed, 1 = adjustable
))
```

### Rationale

- Type index 28 follows existing allocation (27 is `$bucket-array`)
- `$dimensions` as cons list enables direct return from `array-dimensions`
- Separate `$storage` field allows efficient `row-major-aref` without redirection
- `i32` for `$adjustable` avoids boxing/unboxing overhead

## 3. Row-Major Index Computation

### Decision: Loop-based Multiplication

Algorithm for `array-row-major-index`:
```
index = 0
multiplier = 1
for dim from (rank-1) downto 0:
    subscript = args[dim]
    index += subscript * multiplier
    multiplier *= dimensions[dim]
return index
```

### Rationale

- O(rank) computation, matching SBCL implementation
- Avoids recursion (Wasm stack depth concerns)
- Computed dimensions can be cached if performance critical

### Alternatives Considered

1. **Precomputed stride table**: Store cumulative products
   - Rejected: Extra memory per array, benefit minimal for rank < 8

## 4. Simple-Vector vs General Array Dispatch

### Decision: Runtime Type Test

```wat
;; Check if value is multidimensional array
ref.test (ref $mdarray)
if
  ;; Extract metadata from $mdarray struct
else
  ;; Treat as simple-vector (raw $mv_array)
  ;; rank=1, dimension=array.len, total-size=array.len
end
```

### Rationale

- Single code path handles both array types
- No breaking changes to existing simple-vector code
- Type test is O(1) with `ref.test`

## 5. Adjustable Array Implementation

### Decision: Copy-on-Resize

For `adjust-array`:
1. Create new `$mv_array` with new total size
2. Copy existing elements using row-major order
3. Fill new positions with `:initial-element` (default: NIL)
4. Create new `$mdarray` struct with updated dimensions
5. Return new array (not mutation in place)

### Rationale

- WasmGC arrays are fixed-size after creation
- Cannot mutate struct fields in immutable structs
- ANSI CL allows returning a different array object

### Scope Limitation

Displaced arrays (`:displaced-to`) and fill-pointers deferred to future phase.

## 6. Error Handling

### Decision: Signal Type-Error via Exception

Use existing Clysm exception mechanism:
```wat
;; Out-of-bounds check
i32.lt_u
if
  ;; Signal type-error
  ;; (throw $tag-type-error)
end
```

### Rationale

- Consistent with existing error handling in `aref`
- ANSI CL specifies type-error for invalid subscripts

## 7. Test Strategy

### Decision: Three-Tier Testing

1. **Unit tests**: Each function compiles without error
2. **Contract tests**: Generated Wasm passes `wasm-tools validate`
3. **Integration tests**: Runtime execution matches SBCL behavior

### Key Test Cases

| Function | Test Cases |
|----------|------------|
| array-rank | simple-vector=1, 2D=2, 3D=3, 0D=0 |
| array-dimension | each axis of 2x3x4 array |
| array-dimensions | returns correct list |
| array-total-size | product of dimensions |
| array-row-major-index | known index calculations |
| row-major-aref | element access by linear index |
| (setf row-major-aref) | element mutation |
| adjustable-array-p | T for adjustable, NIL for fixed |
| adjust-array | resize preserves elements |

## 8. Function Compilation Order

### Decision: Dependency-Based Order

1. **Phase 1**: Type definition (`$mdarray` struct in gc-types.lisp)
2. **Phase 2**: Metadata queries (array-rank, array-dimension, array-dimensions, array-total-size)
3. **Phase 3**: Row-major access (array-row-major-index, row-major-aref, setf row-major-aref)
4. **Phase 4**: Adjustability (adjustable-array-p, adjust-array)

### Rationale

- Later phases depend on earlier implementations
- Enables incremental testing and validation
