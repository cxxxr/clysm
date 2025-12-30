# Research: Phase 15A - ANSI List Operations Extension

**Date**: 2025-12-30
**Branch**: `001-ansi-list-ops`

## Overview

No NEEDS CLARIFICATION items in Technical Context. This research documents implementation patterns, best practices, and ANSI CL compliance decisions for the 21 list operation functions.

## Research Topics

### 1. Existing Implementation Patterns in Clysm

**Research Question**: How are similar list operations implemented in the current codebase?

**Findings**:
- `cons`, `car`, `cdr` compile to `struct.new 2`, `struct.get 2 0`, `struct.get 2 1`
- `rplaca`, `rplacd` use `struct.set 2 0/1` for destructive modification
- Basic tests exist in `tests/unit/cons-test.lisp` (T079, T080 for nth/nthcdr)
- `assoc` with `:test/:key` has tests in `tests/unit/list-ops/assoc-test.lisp`
- Type predicates (`consp`, `null`, `atom`, `listp`) use `ref.test` and `ref.is_null`

**Decision**: Follow existing patterns. Use loop-based iteration for list traversal to avoid Wasm recursion limits.

**Rationale**: Consistency with existing codebase. WasmGC has limited call stack depth.

**Alternatives Considered**: Recursive implementation rejected due to stack depth limits.

### 2. Keyword Argument Handling (:test, :key)

**Research Question**: How should :test and :key keyword arguments be compiled to Wasm?

**Findings**:
- Existing functions (e.g., `assoc`) already handle keyword arguments
- Keyword args compile to closure objects passed to the function
- Default values: `:test` → `#'eql`, `:key` → `#'identity`
- Call pattern: `(funcall test (funcall key element) item)`

**Decision**: Use existing keyword argument infrastructure. Transform at compile time.

**Rationale**: Reuse existing infrastructure. ANSI CL specifies these defaults.

**Alternatives Considered**: Hardcoded defaults rejected as it limits extensibility.

### 3. NIL Handling in List Operations

**Research Question**: How should functions handle NIL as list terminator and result?

**Findings**:
- Per Constitution II, NIL is a singleton struct (not Wasm null)
- Check with `ref.eq` against global NIL reference
- `ref.is_null` insufficient (Constitution violation)
- Empty list operations should return NIL consistently

**Decision**: Use `ref.eq` for NIL checks. Return global NIL reference for empty results.

**Rationale**: Constitution compliance. Consistent with existing implementation.

**Alternatives Considered**: None - Constitution is non-negotiable.

### 4. Destructive vs Non-Destructive Operations

**Research Question**: How to differentiate destructive (N-prefix) from non-destructive operations?

**Findings**:
- `nbutlast`: Modifies list in place using `rplacd`
- `butlast`: Creates fresh cons cells for result
- Per ANSI CL, destructive ops return the modified structure
- Constitution allows `struct.set` for destructive modification

**Decision**:
- Destructive: Use `struct.set 2 1` (rplacd pattern)
- Non-destructive: Use `struct.new 2` for each new cell

**Rationale**: ANSI CL compliance. WasmGC supports mutable struct fields.

**Alternatives Considered**: Copy-on-write rejected as unnecessarily complex.

### 5. Set Operation Result Order

**Research Question**: What order should set operation results be in?

**Findings**:
- ANSI CL explicitly states order is unspecified for `intersection`, `union`, `set-difference`
- Implementation may return elements in any order
- Tests should use set-equality checks, not list equality

**Decision**: Build results by consing onto accumulator (reverse order from first list).

**Rationale**: Simplest implementation. ANSI CL permits any order.

**Alternatives Considered**: Preserving input order adds complexity with no benefit.

### 6. Skipping Non-Cons Elements in Alists

**Research Question**: How should `assoc`/`rassoc` handle non-cons alist entries?

**Findings**:
- ANSI CL specifies: "It is permissible for alist to contain non-conses; they are ignored."
- Implementation should skip elements that fail `consp` test
- Use `ref.test (ref $cons)` to check before accessing CAR/CDR

**Decision**: Skip non-cons elements silently using `ref.test` guard.

**Rationale**: ANSI CL compliance.

**Alternatives Considered**: Signaling error rejected as non-compliant.

### 7. PUSHNEW Macro Expansion

**Research Question**: How should PUSHNEW be expanded?

**Findings**:
- PUSHNEW is a macro, not a function
- Existing SETF infrastructure (feature 028) provides place modification
- Expansion: `(pushnew x place :k1 v1)` → `(setf place (adjoin x place :k1 v1))`
- Must evaluate `place` subforms correctly (multiple-evaluation avoidance)

**Decision**: Simple expansion to SETF + ADJOIN. Use `get-setf-expansion*` for complex places.

**Rationale**: Leverage existing SETF machinery. ANSI CL compliant.

**Alternatives Considered**: Direct implementation rejected as duplicating SETF logic.

## HyperSpec Reference Summary

| Function | HyperSpec | Key Notes |
|----------|-----------|-----------|
| [last](resources/HyperSpec/Body/f_last.htm) | f_last.htm | Returns last n cons cells, default n=1 |
| [butlast](resources/HyperSpec/Body/f_butlas.htm) | f_butlas.htm | Returns fresh list of all but last n |
| [nbutlast](resources/HyperSpec/Body/f_butlas.htm) | f_butlas.htm | Destructive version of butlast |
| [nth](resources/HyperSpec/Body/f_nth.htm) | f_nth.htm | Zero-indexed element access |
| [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm) | f_nthcdr.htm | Apply cdr n times |
| [member](resources/HyperSpec/Body/f_mem_m.htm) | f_mem_m.htm | Search with :test/:key, returns tail |
| [member-if](resources/HyperSpec/Body/f_mem_m.htm) | f_mem_m.htm | Predicate version of member |
| [member-if-not](resources/HyperSpec/Body/f_mem_m.htm) | f_mem_m.htm | Negated predicate version |
| [assoc](resources/HyperSpec/Body/f_assocc.htm) | f_assocc.htm | Alist lookup by key |
| [assoc-if](resources/HyperSpec/Body/f_assocc.htm) | f_assocc.htm | Predicate version of assoc |
| [rassoc](resources/HyperSpec/Body/f_rassoc.htm) | f_rassoc.htm | Reverse alist lookup (by value) |
| [rassoc-if](resources/HyperSpec/Body/f_rassoc.htm) | f_rassoc.htm | Predicate version of rassoc |
| [pairlis](resources/HyperSpec/Body/f_pairli.htm) | f_pairli.htm | Construct alist from parallel lists |
| [acons](resources/HyperSpec/Body/f_acons.htm) | f_acons.htm | Prepend key-value to alist |
| [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm) | f_cp_ali.htm | Shallow copy alist structure |
| [intersection](resources/HyperSpec/Body/f_intera.htm) | f_intera.htm | Set intersection, order unspecified |
| [union](resources/HyperSpec/Body/f_unionc.htm) | f_unionc.htm | Set union, order unspecified |
| [set-difference](resources/HyperSpec/Body/f_set_di.htm) | f_set_di.htm | Elements in first but not second |
| [subsetp](resources/HyperSpec/Body/f_subset.htm) | f_subset.htm | Subset predicate |
| [adjoin](resources/HyperSpec/Body/f_adjoin.htm) | f_adjoin.htm | Add if not member |
| [pushnew](resources/HyperSpec/Body/m_push.htm) | m_push.htm | Macro: setf + adjoin |

## Conclusion

All research questions resolved. No blockers identified. Implementation can proceed following TDD methodology with the patterns documented above.

**Key Implementation Principles**:
1. Loop-based iteration (avoid recursion)
2. Use existing keyword argument infrastructure
3. NIL checks via `ref.eq` (Constitution II)
4. Destructive ops use `struct.set`, non-destructive use `struct.new`
5. Set operation order unspecified (simplest implementation)
6. PUSHNEW expands to SETF + ADJOIN
