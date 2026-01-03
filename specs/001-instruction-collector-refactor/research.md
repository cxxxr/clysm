# Research: Instruction Collector Refactor

**Date**: 2026-01-03
**Branch**: 001-instruction-collector-refactor

## Research Questions

### RQ1: Current Append Patterns in func-section.lisp

**Finding**: Two primary instruction accumulation patterns identified:

1. **Explicit append loop** (~615 occurrences):
   ```lisp
   (let ((result '()))
     (setf result (append result (compile-to-instructions arg env)))
     (setf result (append result '((:local.set n))))
     result)
   ```
   This is O(n²) because each `append` traverses the entire result list.

2. **Quasiquote splicing** (~60 occurrences):
   ```lisp
   `(,@(compile-to-instructions (first args) env)
     (:local.set ,local-x)
     ,@(compile-to-instructions (second args) env))
   ```
   This is O(n) but verbose and harder to conditionally emit.

**Decision**: Both patterns can be unified under `with-instruction-collector` macro.
**Rationale**: Single consistent pattern improves maintainability and guarantees O(n) performance.

### RQ2: Optimal Macro Design for Instruction Collection

**Alternatives Considered**:

| Approach | Pros | Cons |
|----------|------|------|
| A. `collect` with special variable | Simple implementation | Dynamic scope pollution, hard to nest |
| B. `with-collector` returning function | Explicit, composable | Verbose call syntax |
| C. **`with-instruction-collector` with `macrolet`** | Local `emit`/`emit*` macros, clean syntax | Slightly more complex macro expansion |
| D. Loop-based `collect` | Familiar to Lisp programmers | Doesn't fit imperative emit pattern |

**Decision**: Option C - `with-instruction-collector` with local `macrolet`
**Rationale**:
- Local macros (`emit`, `emit*`) avoid symbol conflicts
- Clean syntax: `(emit :local.get 0)` vs `(funcall emit (list :local.get 0))`
- Lexical scoping enables safe nesting
- O(n) via `push` during body, `nreverse` at return

### RQ3: Macro Implementation Strategy

**Decision**: Use `macrolet` to define local `emit` and `emit*` macros within the body.

**Implementation Sketch**:
```lisp
(defmacro with-instruction-collector (&body body)
  (let ((acc (gensym "INSTRUCTIONS")))
    `(let ((,acc '()))
       (macrolet ((emit (&rest instruction)
                    `(push (list ,@instruction) ,',acc))
                  (emit* (instructions)
                    `(dolist (instr ,instructions)
                       (push instr ,',acc))))
         ,@body
         (nreverse ,acc)))))
```

**Rationale**:
- `gensym` prevents variable capture
- `macrolet` provides hygenic local syntax
- `push` is O(1), `nreverse` is O(n) at end
- Total: O(n) vs previous O(n²)

### RQ4: Migration Order Strategy

**Decision**: Migrate by function size, largest first.

**Top 10 Functions by Size** (from grep analysis):

| Function | Start Line | Est. Lines | Append Calls |
|----------|------------|------------|--------------|
| compile-equalp | 4881 | ~374 | High |
| compile-primitive-call | 1173 | ~363 | High |
| compile-equal | ~4500 | ~300 | High |
| compile-quoted-list | ~700 | ~150 | Medium |
| compile-function-call | ~1100 | ~100 | Medium |

**Rationale**:
- Largest functions yield most line reduction
- High-impact validation: if complex functions work, simple ones will too
- Enables incremental testing with meaningful checkpoints

### RQ5: Test Strategy

**Decision**: Three-tier testing approach.

1. **Unit Tests** (`tests/unit/instruction-collector-test.lisp`):
   - Basic emit/emit* functionality
   - Empty body returns nil
   - Nested collectors are independent
   - Error on emit outside collector

2. **Contract Tests** (`tests/contract/instruction-collector/`):
   - Before/after Wasm output comparison for migrated functions
   - Byte-identical verification

3. **Integration Tests** (`tests/integration/`):
   - Full compilation pipeline with migrated functions
   - Stage 1 generation verification

**Rationale**: TDD requires tests first; three tiers ensure macro correctness, migration correctness, and system stability.

### RQ6: Rollback Strategy

**Decision**: Git-based incremental commits with test verification at each step.

**Process**:
1. Commit macro implementation with tests
2. Migrate one function, commit after tests pass
3. If tests fail, `git revert` the migration commit
4. Debug and retry

**Rationale**: Fine-grained commits enable safe rollback without losing all progress.

## Summary

| Question | Decision | Confidence |
|----------|----------|------------|
| Pattern identification | 615 append + 60 quasiquote | High |
| Macro design | macrolet with emit/emit* | High |
| Implementation | push + nreverse | High |
| Migration order | Size-based, largest first | High |
| Test strategy | Unit → Contract → Integration | High |
| Rollback | Git-based incremental | High |

**All NEEDS CLARIFICATION items resolved. Ready for Phase 1.**
