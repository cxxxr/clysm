# Research: ANSI Sequence Generic Functions

**Feature**: 001-ansi-sequence-functions (Phase 15B)
**Date**: 2025-12-31

## Executive Summary

This research consolidates findings for implementing 21 ANSI CL sequence generic functions. The implementation follows established patterns from Phase 15A (list-ops.lisp) and leverages existing infrastructure in the clysm compiler.

## Key Decisions

### 1. Implementation Pattern

**Decision**: Follow the `*`-suffix naming convention used in list-ops.lisp

**Rationale**:
- Consistent with existing codebase (`member*`, `assoc*`, etc.)
- Avoids shadowing SBCL built-ins during host compilation
- Clear distinction between clysm implementations and host implementations

**Alternatives Considered**:
- Direct naming without suffix: Would conflict with SBCL built-ins
- `clysm-` prefix: More verbose, inconsistent with existing code

### 2. Sequence Type Dispatch Strategy

**Decision**: Use `etypecase` with explicit branches for list, vector, and string

**Rationale**:
- Direct type dispatch matches WasmGC's `ref.test`/`ref.cast` pattern
- Clear code structure for each sequence type
- Enables type-specific optimizations (e.g., direct array indexing for vectors)

**Alternatives Considered**:
- Generic sequence protocol (CLOS): Higher overhead, not needed for these functions
- Single implementation with accessor functions: Less efficient, harder to optimize

### 3. Keyword Argument Handling

**Decision**: Use `&key` with explicit defaults matching ANSI spec

**Rationale**:
- ANSI CL specifies: `:test #'eql`, `:key #'identity` as defaults
- `:start 0`, `:end nil` (nil means sequence length)
- `:from-end nil`, `:count nil` (nil means unlimited)

**Code Pattern**:
```lisp
(defun count* (item sequence &key (test #'eql) (key #'identity)
                                   (start 0) end from-end)
  ...)
```

### 4. Bounds Checking Strategy

**Decision**: Centralized bounds validation utility function

**Rationale**:
- DRY principle: 21 functions need identical bounds checking
- Consistent error signaling across all functions
- Simplifies individual function implementations

**Implementation**:
```lisp
(defun validate-bounding-indices (start end length)
  "Validate START and END bounds for a sequence of given LENGTH.
   Returns validated (START . END) cons with END defaulted if NIL.
   Signals error if bounds are invalid."
  (let ((real-end (or end length)))
    (unless (and (<= 0 start real-end length))
      (error "Invalid bounding indices: start=~A end=~A length=~A"
             start real-end length))
    (cons start real-end)))
```

### 5. :from-end Processing Strategy

**Decision**: Process in forward order, track matches, select based on :from-end at end

**Rationale**:
- Avoids complex reverse iteration for lists
- Works uniformly across all sequence types
- For `:count` with `:from-end`, collect all matches then select last N

**Alternatives Considered**:
- True reverse iteration: Complex for lists (requires pre-building index)
- Two-pass algorithm: First count, then process (inefficient)

### 6. Destructive vs Non-Destructive Variants

**Decision**: Share core logic via internal helper, wrap for destructive/non-destructive behavior

**Rationale**:
- `substitute` vs `nsubstitute`: Same algorithm, different mutation policy
- `remove-duplicates` vs `delete-duplicates`: Same detection, different result construction

**Pattern**:
```lisp
;; Internal implementation
(defun %substitute-impl (newitem olditem sequence test key start end count from-end copy-p)
  ...)

;; Non-destructive wrapper
(defun substitute* (newitem olditem sequence ...)
  (%substitute-impl ... t))  ; copy-p = t

;; Destructive wrapper
(defun nsubstitute* (newitem olditem sequence ...)
  (%substitute-impl ... nil))  ; copy-p = nil
```

## HyperSpec References

| Function Family | HyperSpec Path |
|-----------------|----------------|
| count, count-if, count-if-not | [f_countc.htm](resources/HyperSpec/Body/f_countc.htm) |
| find, find-if, find-if-not | [f_find_.htm](resources/HyperSpec/Body/f_find_.htm) |
| position, position-if, position-if-not | [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm) |
| mismatch | [f_mismat.htm](resources/HyperSpec/Body/f_mismat.htm) |
| search | [f_search.htm](resources/HyperSpec/Body/f_search.htm) |
| substitute, substitute-if, nsubstitute, nsubstitute-if | [f_substc.htm](resources/HyperSpec/Body/f_substc.htm) |
| remove-duplicates, delete-duplicates | [f_rm_dup.htm](resources/HyperSpec/Body/f_rm_dup.htm) |
| replace | [f_replac.htm](resources/HyperSpec/Body/f_replac.htm) |
| fill | [f_fill.htm](resources/HyperSpec/Body/f_fill.htm) |

## Implementation Groups

Based on shared patterns, functions are grouped for implementation:

### Group 1: Single-Item Search (count/find/position)
All share: item, sequence, :test, :key, :start, :end, :from-end
- `count*`, `count-if*`, `count-if-not*`
- `find*`, `find-if*`, `find-if-not*`
- `position*`, `position-if*`, `position-if-not*`

### Group 2: Two-Sequence Operations (mismatch/search)
Both share: sequence1, sequence2, :test, :key, :start1, :end1, :start2, :end2, :from-end
- `mismatch*`
- `search*`

### Group 3: Element Substitution (substitute/nsubstitute)
All share: newitem, olditem/predicate, sequence, :test, :key, :start, :end, :count, :from-end
- `substitute*`, `substitute-if*`, `substitute-if-not*`
- `nsubstitute*`, `nsubstitute-if*`, `nsubstitute-if-not*`

### Group 4: Duplicate Removal
Both share: sequence, :test, :key, :start, :end, :from-end
- `remove-duplicates*`
- `delete-duplicates*`

### Group 5: Bulk Modification
- `fill*`: sequence, item, :start, :end
- `replace*`: sequence1, sequence2, :start1, :end1, :start2, :end2

## Testing Strategy

### Unit Test Organization
Per-function test files in `tests/unit/sequences/`:
- Each function gets comprehensive tests for all sequence types
- Edge case coverage per spec edge cases section

### ANSI Compliance Testing
Use existing ANSI CL test suite infrastructure to measure compliance rate.
Target: 60%+ on sequences category.

## Dependencies

### Existing Infrastructure (Phase 15A)
- `length*`: Sequence length calculation
- `subseq*`: Subsequence extraction
- `copy-seq*`: Sequence copying
- `elt*`: Element access by index

### New Infrastructure (This Phase)
- `validate-bounding-indices`: Bounds checking utility
- Sequence iteration macros (optional optimization)

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| :from-end performance for large lists | Medium | Low | Document O(n) behavior, optimize if needed |
| :test-not deprecation confusion | Low | Low | Support per ANSI spec, document deprecation |
| Edge case misses | Medium | Medium | Comprehensive test suite from spec edge cases |

## Conclusion

All NEEDS CLARIFICATION items have been resolved. The implementation can proceed following the established patterns from Phase 15A with the decisions documented above.
