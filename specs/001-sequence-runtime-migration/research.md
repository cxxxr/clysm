# Research: Sequence Runtime Migration

**Branch**: `001-sequence-runtime-migration`
**Date**: 2026-01-01

## Executive Summary

The 12 targeted functions ([remove](resources/HyperSpec/Body/f_rm_rm.htm)/[count](resources/HyperSpec/Body/f_countc.htm)/[substitute](resources/HyperSpec/Body/f_sbs_s.htm)/[delete](resources/HyperSpec/Body/f_rm_rm.htm) families) account for ~642 lines in `func-section.lisp` (3.5% of 18,327 lines). To achieve the 20% reduction target (~3,665 lines), additional functions must be included in subsequent phases.

## Decision 1: Naming Convention

**Decision**: Use `-rt` suffix for runtime function names (e.g., `remove-rt`, `count-rt`)

**Rationale**: Consistent with existing pattern in `list-runtime.lisp` (`member-rt`, `assoc-rt`, `find-rt`, `position-rt`)

**Alternatives Considered**:
- `%remove` prefix: Rejected - commonly used for internal/primitive functions
- `runtime-remove`: Rejected - verbose, inconsistent with existing code

## Decision 2: Sequence Type Scope

**Decision**: Lists only for initial implementation

**Rationale**:
- Layer 1 primitives (car, cdr, cons) are list-specific
- String/vector support requires additional primitives (char, length, svref)
- List operations cover the majority of sequence usage in the compiler itself
- Deferred scope allows faster delivery of core functionality

**Alternatives Considered**:
- All sequence types: Rejected - requires expanding Layer 1 primitive set
- Lists + strings: Rejected - strings require char/schar primitives not yet in Layer 1

## Decision 3: Keyword Argument Handling

**Decision**: Parse keyword arguments at compile time; pass as regular positional arguments to runtime functions

**Rationale**:
- Avoids runtime keyword parsing overhead
- Matches existing `list-runtime.lisp` pattern
- Compile-time validation of keyword arguments

**Function Signature Pattern**:
```lisp
(defun remove-rt (item list test key start end count from-end)
  "Runtime implementation of REMOVE for lists."
  ...)
```

**Compiler Registration**:
```lisp
(register-runtime-function 'remove :$remove-rt nil)  ; nil = variadic for keyword handling
```

**Alternatives Considered**:
- Runtime &key parsing: Rejected - adds overhead, complicates primitive-only constraint
- Separate functions per keyword combo: Rejected - combinatorial explosion

## Decision 4: Delete Family Strategy

**Decision**: Implement delete family as separate functions that destructively modify cons cells

**Rationale**:
- ANSI CL specifies delete MAY modify the list destructively
- Sharing implementation with remove loses the efficiency benefit
- Destructive operations use `rplaca`/`rplacd` which are Layer 1 primitives

**Alternatives Considered**:
- Wrapper around remove: Rejected - loses destructive efficiency
- Copy-on-write: Rejected - adds complexity without benefit

## Decision 5: :from-end Processing

**Decision**: Implement `:from-end` by reversing, processing, then reversing result

**Rationale**:
- Simpler implementation using existing `nreverse` primitive
- Correct semantics for `:count` with `:from-end`
- Acceptable performance for typical list sizes

**Alternatives Considered**:
- Two-pass algorithm: Rejected - more complex, marginal performance benefit
- Recursive accumulator: Rejected - stack depth concerns for long lists

## Code Reduction Analysis

### Phase 1: Targeted 12 Functions

| Function | Lines | Notes |
|----------|-------|-------|
| compile-remove | ~105 | Lines 11485-11589 |
| compile-remove-if | ~86 | Lines 11591-11676 |
| compile-remove-if-not | ~86 | Lines 11678-11763 |
| compile-substitute | ~85 | Lines 11764-11848 |
| compile-substitute-if | ~90 | Lines 11849-11938 |
| compile-count | ~80 | Lines 11939-12018 |
| compile-count-if | ~60 | Lines 12019-12078 |
| delete family | ~0 | Not yet implemented inline |
| **Subtotal** | **~592** | 3.2% reduction |
| Dispatch clauses | ~30 | Case statements |
| **Total Phase 1** | **~622** | 3.4% reduction |

### Phase 2 Candidates (for 20% target)

To reach 20% (~3,665 lines), additional migrations required:

| Function Group | Est. Lines | Priority |
|----------------|------------|----------|
| find/find-if | ~160 | HIGH - already have `find-rt` in list-runtime |
| position/position-if | ~180 | HIGH - already have `position-rt` in list-runtime |
| member/member-if/member-if-not | ~180 | HIGH - already have `member-rt` |
| assoc/rassoc variants | ~340 | HIGH - already have `assoc-rt`, `rassoc-rt` |
| union/intersection/set-difference | ~300 | MEDIUM |
| every/some/notany/notevery | ~240 | MEDIUM |
| mapcar/mapc/maplist | ~250 | LOW - performance critical |
| reduce | ~80 | LOW - complex semantics |
| **Potential Additional** | **~1,730** | |

**Recommendation**: Expand Phase 1 to include find/position/member/assoc families (already have runtime implementations in `list-runtime.lisp`). This brings total reduction to ~1,300 lines (7.1%).

## Implementation Pattern (from list-runtime.lisp)

```lisp
;;;; sequence-runtime.lisp - Runtime library sequence functions
;;;; Feature: 001-sequence-runtime-migration
;;;;
;;;; HyperSpec references:
;;;;   [remove](resources/HyperSpec/Body/f_rm_rm.htm)
;;;;   [count](resources/HyperSpec/Body/f_countc.htm)

(in-package #:clysm)

(defun remove-rt (item list test key start end count from-end)
  "Return a copy of LIST without elements equal to ITEM.
   Uses car/cdr/consp primitives only."
  (let ((test-fn (or test #'eql))
        (result nil)
        (index 0)
        (matched 0))
    ;; Simple implementation for lists
    (loop for rest = list then (cdr rest)
          while (consp rest)
          for elem = (car rest)
          do (let ((at-pos (and (or (null start) (>= index start))
                                (or (null end) (< index end))))
                   (matches (funcall test-fn item
                                     (if key (funcall key elem) elem))))
               (cond
                 ((and at-pos matches (or (null count) (< matched count)))
                  (incf matched))
                 (t (push elem result)))
               (incf index)))
    (nreverse result)))
```

## Risk Analysis

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Performance regression | MEDIUM | HIGH | Benchmark before/after, profile hot paths |
| ANSI incompatibility | LOW | HIGH | Test against SBCL behavior |
| Primitive insufficiency | LOW | MEDIUM | Extend Layer 1 if needed |
| Compile-time keyword parsing bugs | MEDIUM | MEDIUM | Thorough unit tests |

## Dependencies

1. **Existing Infrastructure** (verified available):
   - `*runtime-function-table*` in `func-section.lisp:69`
   - `register-runtime-function` in `func-section.lisp:78`
   - `compile-runtime-call` in `func-section.lisp:90`
   - `runtime-function-p` in `func-section.lisp:86`

2. **Layer 1 Primitives** (verified in registered-primitive-p):
   - `car`, `cdr`, `cons`, `consp`, `null`, `atom`, `listp`
   - `eq`, `eql`, `funcall`
   - `rplaca`, `rplacd` (for delete family)

3. **Test Infrastructure**:
   - rove framework in `tests/`
   - Existing pattern in `tests/unit/io-runtime/`

## References

- [CLHS remove](resources/HyperSpec/Body/f_rm_rm.htm)
- [CLHS count](resources/HyperSpec/Body/f_countc.htm)
- [CLHS substitute](resources/HyperSpec/Body/f_sbs_s.htm)
- [CLHS delete](resources/HyperSpec/Body/f_rm_rm.htm) (same page as remove)
- Existing implementation: `src/clysm/lib/list-runtime.lisp`
