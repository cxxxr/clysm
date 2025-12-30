# Research: ANSI CL Sequence Operations

**Branch**: `001-ansi-sequence-operations` | **Date**: 2025-12-29

## Research Tasks

### 1. WasmGC Array Instructions Availability

**Question**: Which WasmGC array instructions are available for sequence operations?

**Decision**: Use existing `array.new_fixed`, `array.new_default`, `array.len`, `array.get`, `array.set` instructions. Add `array.copy` support to the compiler.

**Rationale**:
- `array.copy` is part of the WasmGC proposal and is essential for efficient `subseq` and `copy-seq` implementation
- The compiler (`compiler.lisp:856-893`) already supports `array.new_fixed`, `array.new_default`, `array.len`, `array.get`, `array.set`, `array.get_u`
- `array.copy` needs to be added to the instruction emission logic

**Alternatives considered**:
- Loop-based copy using `array.get`/`array.set`: Rejected because `array.copy` is more efficient and idiomatic WasmGC

### 2. Existing Sequence Type Infrastructure

**Question**: What sequence types exist and how are they represented in WasmGC?

**Decision**: Leverage existing type definitions from `gc-types.lisp`:

| Sequence Type | WasmGC Type Index | Structure |
|---------------|-------------------|-----------|
| String | `+type-string+` (4) | `(array (mut i8))` - UTF-8 bytes |
| Vector (general) | `+type-anyref-array+` (21) | `(array (mut anyref))` |
| List | `+type-cons+` (2) | `(struct (field car anyref) (field cdr anyref))` |

**Rationale**: Existing infrastructure is well-established and used throughout the compiler. No new types needed.

### 3. Existing Compile Functions Pattern

**Question**: What pattern should new compile functions follow?

**Decision**: Follow the established pattern in `func-section.lisp`:

```lisp
(defun compile-<operation> (ast env)
  "Compile <operation> expression to Wasm instructions.
   <docstring with details>"
  ;; 1. Extract arguments from AST
  ;; 2. Compile sub-expressions (recursively)
  ;; 3. Generate type dispatch (if polymorphic)
  ;; 4. Emit Wasm instructions
  ;; 5. Return instruction list
  )
```

**Rationale**: Consistent with existing `compile-aref`, `compile-svref`, `compile-schar`, etc. patterns from Phase 13D-1.

### 4. Type Dispatch Strategy

**Question**: How should polymorphic sequence operations dispatch on sequence type?

**Decision**: Use runtime type checking with `ref.test` and conditional branching:

```wat
;; Check if string
(local.get $seq)
(ref.test (ref $string))
(if (result anyref)
  (then ;; string case)
  (else
    ;; Check if vector
    (local.get $seq)
    (ref.test (ref $anyref-array))
    (if (result anyref)
      (then ;; vector case)
      (else ;; list case))))
```

**Rationale**: Matches existing type dispatch patterns in the compiler (see `compile-elt` implementation).

**Alternatives considered**:
- Compile-time type inference: Rejected because sequence types are often dynamically determined
- Generic function dispatch (CLOS): Rejected as overkill for primitive operations

### 5. Bounds Checking Strategy

**Question**: How should bounds errors be signaled?

**Decision**: Use existing condition system infrastructure:
1. Generate runtime bounds check with `array.len` comparison
2. On failure, call error signaling function for `bounding-indices-bad-error`
3. Pattern matches `compile-aref` bounds checking

**Rationale**: Consistent with existing array access bounds checking.

### 6. (setf subseq) Implementation

**Question**: Does `(setf subseq)` already have a setf expander?

**Decision**: Yes, `setf-expanders.lisp:587` already defines:
```lisp
(defsetf* subseq (sequence start &optional end) (new-value)
  ...)
```

**Action**: Ensure the generated code can be compiled by adding appropriate codegen support.

### 7. UTF-8 String Semantics

**Question**: Should `subseq` on strings use byte or character indices?

**Decision**: Use byte-level indices internally (matching WasmGC `array` semantics), with Lisp-level UTF-8 handling for character semantics.

**Rationale**:
- The spec assumption states: "UTF-8 encoding for strings is maintained; subseq on strings works at byte level for Wasm"
- Character-level semantics are handled by existing UTF-8 layer in `lib/utf8.lisp`
- This is consistent with how strings are currently represented (type index 4 = byte array)

## Implementation Recommendations

1. **Add `array.copy` instruction support** to `compiler.lisp` bytecode emission
2. **Implement in priority order**: subseq → concatenate → make-string → make-array extensions → copy-seq
3. **Reuse existing patterns** from Phase 13D-1 array primitives
4. **No new WasmGC types needed** - use existing type indices
5. **Follow TDD strictly** - write failing tests before implementation

## Unresolved Questions

None - all technical questions resolved through codebase research.
