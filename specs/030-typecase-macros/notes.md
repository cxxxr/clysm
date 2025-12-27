# Implementation Notes: Type Dispatch Macros

## Edge Cases Discovered

### 1. ctypecase and check-type expand to LOOP, not LET

**Issue**: Initial integration tests expected all typecase macros to expand with `LET` as the outer form.

**Reality**: `ctypecase` and `check-type` require a `LOOP` as the outer form for proper re-validation after the `store-value` restart is invoked.

**Expansion patterns**:
- `typecase` → `(let ((#:key keyform)) (if ...))`
- `etypecase` → `(let ((#:key keyform)) (if ... (error 'type-error ...)))`
- `ctypecase` → `(loop (if ... (return ...) ...) (restart-case (error ...) (store-value ...)))`
- `check-type` → `(loop (when (pred place) (return nil)) (restart-case (error ...) (store-value ...)))`

### 2. Compound types implemented during Foundational phase

**Issue**: Tasks.md had separate tasks T067-T071 for implementing each compound type specifier.

**Reality**: All compound types (or, and, not, member, satisfies, eql) were already implemented in `type-specifier-to-predicate` during Phase 2 (Foundational), making Phase 7 primarily a verification phase.

### 3. No runtime typep needed

**Design decision**: Per research.md findings, typecase macros expand directly to predicate calls at macro-expansion time. There is no need for a runtime `typep` function - the expansion uses primitive predicates (integerp, symbolp, etc.) directly.

**Benefit**: Simpler implementation, better performance (compile-time type dispatch).

### 4. flatten helper required for tests

**Issue**: Integration tests used `flatten` without importing it.

**Fix**: Added `(:import-from #:alexandria #:flatten)` to the `clysm/tests/integration/typecase-ansi` package definition.

## Implementation Summary

| Macro | Outer Form | Error Signaling | Restart |
|-------|------------|-----------------|---------|
| typecase | LET | None (returns NIL) | None |
| etypecase | LET | type-error | None |
| ctypecase | LOOP | type-error | store-value |
| check-type | LOOP | type-error | store-value |

## Test Coverage

- Unit tests: 5 test files in `tests/unit/typecase/`
- Contract tests: `tests/contract/typecase-wasm-test.lisp`
- Integration tests: `tests/integration/typecase-ansi-test.lisp`

All tests pass. Feature implementation complete.
