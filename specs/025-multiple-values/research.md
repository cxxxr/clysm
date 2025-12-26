# Research: ANSI Common Lisp Multiple Values Support

**Feature**: 025-multiple-values
**Date**: 2025-12-26

## Research Topics

### 1. WasmGC Global Initialization for Array Types

**Decision**: Use `array.new_default` instruction to initialize mv-buffer with NIL values

**Rationale**:
- WasmGC arrays require initialization expressions in globals
- `array.new_default` creates an array of specified length with default values (null for anyref)
- Since our values are anyref and NIL is a valid default, this is the cleanest approach
- The buffer size (20) satisfies ANSI CL `multiple-values-limit` minimum

**Alternatives Considered**:
1. `array.new_fixed` - Requires listing all 20 elements in init expression (verbose)
2. `array.new_elem` - Requires element segment (unnecessary complexity)
3. Linear memory buffer - Violates Constitution Principle I (WasmGC-First)

**Implementation**:
```wat
;; Type definition (added to type section)
(type $mv-array (array (mut anyref)))

;; Global initialization
(global $mv-count (mut i32) (i32.const 1))
(global $mv-buffer (mut (ref $mv-array)) (array.new_default $mv-array (i32.const 20)))
```

### 2. Global Index Allocation Strategy

**Decision**: Reserve indices 2 and 3 for mv-count and mv-buffer; shift special variables to index 4+

**Rationale**:
- Current allocation: 0=NIL, 1=UNBOUND, 2+=special variables
- Multiple values are fundamental and should be initialized before any user code
- Fixed indices simplify codegen (hardcoded references in compile-values, etc.)
- Special variables already use dynamic allocation via `allocate-global`

**Alternatives Considered**:
1. Dynamic allocation for mv globals - Adds complexity, no benefit for fixed resources
2. Place after special vars - Would require tracking and potentially variable indices

**Implementation**:
- Update `*global-counter*` initial value from 2 to 4 in objects.lisp
- Add `*mv-count-global-index*` = 2 and `*mv-buffer-global-index*` = 3 constants

### 3. Multiple Values in Tail Position

**Decision**: Values in tail position are transparent; enclosing function preserves mv-count and mv-buffer state

**Rationale**:
- ANSI CL specifies that `(values 1 2 3)` in tail position should return all 3 values from the function
- Primary value is already on stack (normal Wasm return)
- Secondary values persist in globals until next values-producing form
- No special handling needed in tail call optimization

**Alternatives Considered**:
1. Copy buffer on function entry - Unnecessary overhead for common case
2. Stack-based values - Not feasible in Wasm without extensive trampolining

### 4. Arithmetic Functions Multiple Values

**Decision**: Implement remainder as second value for floor/truncate/ceiling/round

**Rationale**:
- ANSI CL requires these functions to return (quotient remainder) as multiple values
- Existing implementation in func-section.lisp only returns quotient
- Pattern: compute both values, store remainder in mv-buffer[0], set mv-count=2, return quotient

**Implementation Pattern**:
```lisp
(defun compile-floor (args env)
  ;; Compile dividend and divisor
  ;; Generate: (local.tee $quot) ... (i32.sub) for remainder
  ;; Store remainder: (global.get $mv-buffer) (i32.const 0) remainder (array.set)
  ;; Set count: (i32.const 2) (global.set $mv-count)
  ;; Return quotient on stack
  )
```

### 5. multiple-value-bind Implementation Strategy

**Decision**: Compile as inline code that reads globals directly, not as macro expansion

**Rationale**:
- Macro expansion to let + setq would require temporary variables
- Direct codegen is more efficient and cleaner
- Pattern: evaluate form, store primary in local, read secondaries from buffer, bind all

**Alternatives Considered**:
1. Macro to (let ((#:tmp form)) (let ((a #:tmp) (b (nth-value 1 #:tmp)) ...) ...))
   - Requires re-evaluating form or complex temporary handling
2. Runtime function call - Adds overhead, defeats purpose of inline values

**Implementation**:
```lisp
(defun compile-multiple-value-bind (args env)
  (destructuring-bind ((vars value-form &rest body) args)
    ;; 1. Compile value-form
    ;; 2. Store primary value in first local
    ;; 3. For each secondary var:
    ;;    (global.get $mv-count) (i32.const index) (i32.gt_u)
    ;;    (if (then (global.get $mv-buffer) (i32.const index-1) (array.get))
    ;;        (else (global.get $nil)))
    ;; 4. Bind all vars in extended env
    ;; 5. Compile body
    ))
```

### 6. values-list Type Checking

**Decision**: Use ref.test to verify list type; signal type-error via existing condition system

**Rationale**:
- ANSI CL requires type-error when argument is not a list
- clysm already has condition system (Feature 014)
- Pattern: test if cons or nil, else signal type-error

**Implementation**:
```lisp
;; Pseudocode for values-list codegen
(if (or (ref.test $cons arg) (ref.eq arg $nil))
    (loop for elem in list
          for i from 0
          do (if (= i 0) (return elem) (array.set $mv-buffer (1- i) elem)))
    (signal-type-error 'values-list arg 'list))
```

### 7. multiple-value-call Argument Collection

**Decision**: Collect all values from each form into a temporary list, then apply function

**Rationale**:
- `(multiple-value-call #'f (values 1 2) (values 3 4))` must call `(f 1 2 3 4)`
- Cannot know total argument count at compile time if forms are dynamic
- Collect phase: evaluate each form, append all values to accumulator
- Apply phase: spread accumulated values as arguments

**Alternatives Considered**:
1. Fixed argument count analysis - Only works for literal values forms
2. Stack manipulation - Wasm stack doesn't support dynamic arg count

**Implementation**:
- Use runtime list accumulator
- For each value-form: push primary, then buffer[0..count-2] to accumulator
- Call function with apply semantics

## Type System Integration

### New Type Indices

| Index | Type Name | Definition |
|-------|-----------|------------|
| 18 | $mv-array | `(array (mut anyref))` |

Note: Type index 18 assumes indices 0-17 are already allocated per gc-types.lisp.
Actual index will be determined by type section order.

## Wasm Instructions Used

| Instruction | Purpose |
|-------------|---------|
| `global.get/set` | Access mv-count and mv-buffer |
| `array.new_default` | Initialize mv-buffer with 20 null slots |
| `array.get/set` | Read/write secondary values |
| `array.len` | Optional: verify buffer bounds |
| `ref.test` | Type checking for values-list |
| `i32.const/add/sub/gt_u` | Index arithmetic and count comparison |

## Compatibility Notes

- Single-value code is unaffected (mv-count defaults to 1, buffer untouched)
- JIT-compiled code uses same globals (shared runtime state)
- Interpreter (Tier 1) can use same buffer via host imports
