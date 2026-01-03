# Research: Equality Predicate Type-Dispatch Consolidation

**Date**: 2026-01-03

## Overview

Analysis of the current equality predicate implementations to identify consolidation opportunities and design the unified type-dispatch infrastructure.

## Current Implementation Analysis

### Function Locations and Sizes

| Function | Lines | Location |
|----------|-------|----------|
| compile-eq | 48 | func-section.lisp:4339-4386 |
| compile-eql | 144 | func-section.lisp:4387-4530 |
| compile-equal | 274 | func-section.lisp:4531-4804 |
| compile-equalp | 374 | func-section.lisp:4805-5178 |
| **Total** | **840** | - |

### Common Patterns Identified

#### 1. Argument Handling (all functions)
```lisp
(when (/= (length args) 2)
  (error "X requires exactly 2 arguments"))
(let ((local-x (env-add-local env (gensym "X-X")))
      (local-y (env-add-local env (gensym "X-Y"))))
  (compile-to-instructions (first args) env)
  (:local.set local-x)
  (compile-to-instructions (second args) env)
  (:local.set local-y)
  ...)
```

#### 2. Null Check Pattern (all functions)
```lisp
(:local.get local-x)
:ref.is_null
(:if (:result :anyref))
  (:local.get local-y)
  :ref.is_null
  (:if (:result :anyref))
    ;; both null => T
  :else
    ;; x null, y not => NIL
  :end
:else
  (:local.get local-y)
  :ref.is_null
  (:if (:result :anyref))
    ;; x not null, y null => NIL
  :else
    ;; type dispatch
  :end
:end
```

#### 3. Type Test Pattern (repeated per type)
```lisp
(:local.get local-x)
(:ref.test TYPE)
(:if ...)
  (:local.get local-y)
  (:ref.test TYPE)
  (:if ...)
    ;; same type comparison
  :else
    ;; different types
  :end
:else
  ;; try next type
:end
```

### Equality Semantics Matrix

| Type | eq | eql | equal | equalp |
|------|-----|-----|-------|--------|
| null | ref.eq | ref.eq | ref.eq | ref.eq |
| i31ref (fixnum/char/T) | ref.eq | ref.eq | ref.eq | ref.eq* |
| float | ref.eq | f64.eq | f64.eq | numeric= |
| ratio | ref.eq | num+den eq | num+den eq | numeric= |
| string | ref.eq | ref.eq | byte-by-byte | case-insensitive |
| cons | ref.eq | ref.eq | recursive | recursive |
| symbol | ref.eq | ref.eq | ref.eq | ref.eq |
| other | ref.eq | ref.eq | ref.eq | ref.eq |

*Note: equalp for i31ref characters uses case-insensitive comparison

## Design Decisions

### Decision 1: Type Dispatch Infrastructure

**Choice**: Table-driven type dispatch with equality-level parameterization

**Rationale**: Each WasmGC type index maps to a comparison strategy. The comparison logic varies by equality level but the type dispatch structure is identical across all four predicates.

**Alternatives Rejected**:
- Macro expansion: Would increase compile-time code size
- Runtime dispatch table: Adds runtime overhead, against WasmGC-First principle

### Decision 2: Equality Level Representation

**Choice**: Keyword parameter `:eq`, `:eql`, `:equal`, `:equalp`

**Rationale**: Clear, self-documenting, matches ANSI CL naming. Can be efficiently dispatched at compile-time.

**Alternatives Rejected**:
- Numeric levels (0-3): Less readable, error-prone
- Bitmask flags: Over-engineered for 4 distinct levels

### Decision 3: Type Comparison Generators

**Choice**: Per-type comparison generator functions

| Generator | Types | Description |
|-----------|-------|-------------|
| `emit-null-comparison` | null | Always use ref.eq |
| `emit-i31-comparison` | fixnum, char, T | ref.eq or case-insensitive |
| `emit-float-comparison` | float | f64.eq or numeric= |
| `emit-ratio-comparison` | ratio | component comparison or numeric= |
| `emit-string-comparison` | string | byte-by-byte or case-insensitive |
| `emit-cons-comparison` | cons | worklist push (equal/equalp only) |
| `emit-default-comparison` | other | ref.eq |

**Rationale**: Separates concerns, allows testing individual comparison logic.

### Decision 4: Worklist vs Direct Recursion

**Choice**: Keep worklist-based approach for equal/equalp

**Rationale**: Required per Constitution IV (Wasm制御フロー活用) - Wasm has recursion limits, worklist-based iteration is safer for deep structures.

### Decision 5: Code Organization

**Choice**: New helper functions in func-section.lisp, above equality predicates

**Rationale**:
- Keeps related code together
- Avoids adding new files (minimal change)
- Helper functions can be reused by other predicates

## Estimated Line Count

| Component | Lines |
|-----------|-------|
| `compile-type-dispatch` (shared dispatch) | ~40 |
| `emit-null-comparison` | ~15 |
| `emit-i31-comparison` | ~25 |
| `emit-float-comparison` | ~20 |
| `emit-ratio-comparison` | ~30 |
| `emit-string-comparison` | ~50 |
| `emit-cons-comparison` | ~30 |
| `emit-default-comparison` | ~10 |
| `compile-equality-predicate` | ~80 |
| 4x wrappers (compile-eq/eql/equal/equalp) | ~20 |
| **Total** | **~320** |

Target: < 400 lines. Estimated: ~320 lines. **Margin: 80 lines (25%)**

## Dependencies

| Dependency | Status | Notes |
|------------|--------|-------|
| primitive-dispatch.lisp | Stable | register-primitive-compiler API |
| gc-types constants | Stable | +type-cons+, +type-float+, etc. |
| instruction-collector | Stable | with-instruction-collector, emit, emit* |
| env-add-local | Stable | Local variable allocation |

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Wasm output differs | Medium | High | Capture baseline Wasm before refactoring, binary diff |
| Test coverage gaps | Low | Medium | Run all equality tests, add edge cases if needed |
| Compile-time regression | Low | Low | Profile Stage 1 build before/after |

## References

- [eq](resources/HyperSpec/Body/f_eq.htm) - Object identity
- [eql](resources/HyperSpec/Body/f_eql.htm) - Object identity + numeric/char equality
- [equal](resources/HyperSpec/Body/f_equal.htm) - Structural similarity
- [equalp](resources/HyperSpec/Body/f_equalp.htm) - Case-insensitive structural equality
