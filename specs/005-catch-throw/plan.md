# Implementation Plan: Catch/Throw Dynamic Exception Handling

**Branch**: `005-catch-throw` | **Date**: 2025-12-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/005-catch-throw/spec.md`

## Summary

Implement Common Lisp `catch/throw` dynamic exception handling using the WebAssembly Exception Handling proposal. This enables non-local exits across function boundaries with runtime tag matching via `eq` comparison.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) + alexandria, rove (testing)
**Primary Dependencies**: Existing clysm compiler infrastructure
**Storage**: N/A
**Testing**: rove + integration tests via wasmtime
**Target Platform**: WebAssembly with Exception Handling proposal
**Project Type**: Single project
**Performance Goals**: 10,000 nested throws without stack overflow
**Constraints**: Requires wasmtime `-W exceptions=y` flag

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- [x] TDD: Tests written first, must fail before implementation
- [x] Nix-First: All tests runnable via `nix flake check`
- [x] Single project structure (no new packages)
- [x] Leverages existing patterns from tagbody/go implementation

## Project Structure

### Documentation (this feature)

```text
specs/005-catch-throw/
├── plan.md              # This file
├── research.md          # Wasm EH decision
├── data-model.md        # Entity definitions
├── spec.md              # Feature specification
├── checklists/
│   └── requirements.md  # Validation checklist
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/
└── clysm/
    └── compiler/
        ├── codegen/
        │   ├── func-section.lisp  # Main: compile-catch, compile-throw
        │   └── module.lisp        # Exception tag declaration
        └── ast.lisp               # AST definitions (already present)

tests/
├── unit/
│   └── catch-test.lisp            # New: unit tests
└── integration/
    ├── control-flow-test.lisp     # Existing: catch/throw tests
    └── helpers.lisp               # Update: add --wasm exceptions flag
```

**Structure Decision**: Single project extending existing compiler structure

## Implementation Phases

### Phase 1: Infrastructure Setup

**Goal**: Enable Wasm exception handling in the test infrastructure

1. **T001**: Update `tests/helpers.lisp` to add `--wasm exceptions` flag to wasmtime invocation
2. **T002**: Verify existing catch/throw tests fail (expected - current stub implementation)
3. **T003**: Add exception tag declaration `$lisp-throw` to module generation in `module.lisp`

**Checkpoint**: wasmtime accepts exception handling code

### Phase 2: Basic Catch Without Throw (US1 partial)

**Goal**: Catch form with normal body completion (no throw)

4. **T004**: Write unit test for catch compiling to `try_table` structure
5. **T005**: Implement `compile-catch` generating `try_table` with proper block structure
6. **T006**: Run `test-simple-catch` - should pass

**Checkpoint**: `(catch 'foo 42)` returns 42

### Phase 3: Basic Throw Within Same Function (US1 complete)

**Goal**: Throw transfers control to matching catch

7. **T007**: Write unit test for throw generating `throw` instruction
8. **T008**: Implement `compile-throw` generating `(throw $lisp-throw tag value)`
9. **T009**: Implement exception handler in `compile-catch` to:
   - Extract thrown tag and value from exnref
   - Compare tags using eq
   - Return value on match
   - Rethrow on mismatch
10. **T010**: Run `test-catch-throw` - should pass

**Checkpoint**: `(catch 'foo (throw 'foo 42) 0)` returns 42

### Phase 4: Cross-Function Throw (US2)

**Goal**: Throw from called function unwinds to catch

11. **T011**: Run `test-throw-from-function` - should pass (no code changes if Phase 3 correct)
12. **T012**: Write integration test for deeply nested function calls with throw
13. **T013**: Verify 100 function call levels work per SC-003

**Checkpoint**: Throw works across any call depth

### Phase 5: Nested Catch with Tag Selection (US3)

**Goal**: Multiple catches with different tags work correctly

14. **T014**: Run `test-nested-catch` - should pass
15. **T015**: Run `test-catch-wrong-tag` - should pass
16. **T016**: Write test for 3+ nested catches with different tags
17. **T017**: Ensure rethrow mechanism propagates correctly

**Checkpoint**: `(catch 'outer (catch 'inner (throw 'outer 42)))` returns 42 to outer

### Phase 6: Unwind-Protect Integration (US4)

**Goal**: Cleanup forms run during throw unwinding

18. **T018**: Update `compile-unwind-protect` to use `catch_all` handler
19. **T019**: Run `test-unwind-protect-with-throw` - should pass
20. **T020**: Write test for multiple nested unwind-protects during throw
21. **T021**: Verify cleanup order (innermost first)

**Checkpoint**: Cleanup always runs before catch receives value

### Phase 7: Edge Cases and Error Handling

**Goal**: Handle error conditions per spec

22. **T022**: Write test for throw with no matching catch
23. **T023**: Implement runtime error for unhandled throw (let exception propagate to host)
24. **T024**: Write test for NIL value in throw
25. **T025**: Write test for empty catch body

**Checkpoint**: All edge cases handled correctly

### Phase 8: Performance Verification

**Goal**: Meet success criteria

26. **T026**: Write stress test for 10,000 nested throws (SC-002)
27. **T027**: Verify no stack overflow
28. **T028**: Run full test suite via `nix flake check`

**Checkpoint**: All success criteria met

## Key Implementation Details

### Exception Tag Declaration

```lisp
;; In module.lisp, add to module definition
(:tag $lisp-throw (:param :anyref :anyref))
```

### Catch Compilation Pattern

```lisp
(defun compile-catch (ast env)
  "Compile catch form using try_table."
  (let* ((tag (ast-catch-tag ast))
         (body (ast-catch-body ast))
         (tag-local (env-add-local env (gensym "catch-tag")))
         (result '()))
    ;; Evaluate and store catch tag
    (setf result (append result (compile-to-instructions tag env)))
    (setf result (append result (list (list :local.set tag-local))))
    ;; try_table structure
    (setf result (append result
      '((:block (:result :anyref))  ; $catch-exit
         (:block (:result :exnref)) ; $handler
           (:try_table (:catch $lisp-throw 0))
             ;; body compilation
             (:br 1)  ; normal exit to $catch-exit
           :end)
         ;; handler: exnref on stack
         ;; Extract, compare, return or rethrow
        :end)))
    result))
```

### Throw Compilation Pattern

```lisp
(defun compile-throw (ast env)
  "Compile throw using Wasm throw instruction."
  (let* ((tag (ast-throw-tag ast))
         (value (ast-throw-value ast))
         (result '()))
    (setf result (append result (compile-to-instructions tag env)))
    (setf result (append result (compile-to-instructions value env)))
    (setf result (append result '((:throw $lisp-throw))))
    result))
```

## Dependencies

| From | To | Type |
|------|-----|------|
| Phase 2 | Phase 1 | Blocking |
| Phase 3 | Phase 2 | Blocking |
| Phase 4 | Phase 3 | Validation |
| Phase 5 | Phase 3 | Extension |
| Phase 6 | Phase 5 | Extension |
| Phase 7 | Phase 6 | Extension |
| Phase 8 | Phase 7 | Validation |

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Wasmtime EH support issues | Already verified: wasmtime 39.0.1 has `-W exceptions=y` |
| exnref extraction complexity | Use Wasm GC instructions for struct access |
| Performance regression | Stress test in Phase 8 |
| Unwind-protect interaction | Dedicated Phase 6 with explicit tests |

## Success Criteria Verification

| Criterion | Test | Phase |
|-----------|------|-------|
| SC-001: Existing tests pass | test-catch-throw, test-nested-catch, test-throw-from-function, test-catch-wrong-tag | 3-5 |
| SC-002: 10,000 nested throws | stress test | 8 |
| SC-003: 100 function levels | integration test | 4 |
| SC-004: unwind-protect 100% | test-unwind-protect-with-throw | 6 |
| SC-005: Unhandled throw error | edge case test | 7 |
