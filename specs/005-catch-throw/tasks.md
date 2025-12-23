# Tasks: Catch/Throw Dynamic Exception Handling

**Input**: Design documents from `/specs/005-catch-throw/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: REQUIRED per Constitution VII (TDD non-negotiable). Tests must be written first and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Main implementation: `src/clysm/compiler/codegen/func-section.lisp`
- Module generation: `src/clysm/compiler/codegen/module.lisp`
- Unit tests: `tests/unit/catch-test.lisp`
- Integration tests: `tests/integration/control-flow-test.lisp`
- Test helpers: `tests/helpers.lisp`

---

## Phase 1: Setup

**Purpose**: Enable Wasm exception handling in the test infrastructure

- [x] T001 Update `run-wasm-bytes` function to add `--wasm exceptions` flag to wasmtime invocation in tests/helpers.lisp
- [x] T002 Verify wasmtime accepts `--wasm exceptions` flag by running a simple test
- [x] T003 Add exception tag declaration `$lisp-throw` with `(param anyref anyref)` to module generation in src/clysm/compiler/compiler.lisp

**Checkpoint**: wasmtime accepts exception handling code

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Data Structures

- [x] T004 Review existing `catch-tags` field in `compilation-env` struct and document its usage in src/clysm/compiler/codegen/func-section.lisp
- [x] T005 Ensure `copy-compilation-env` properly handles catch-tags for nested catch support in src/clysm/compiler/codegen/func-section.lisp

### Wasm IR Support

- [x] T006 [P] Verify Wasm IR emitter supports `:try_table` instruction in src/clysm/compiler/compiler.lisp
- [x] T007 [P] Verify Wasm IR emitter supports `:throw` instruction in src/clysm/compiler/compiler.lisp
- [x] T008 [P] Verify Wasm IR emitter supports `:throw_ref` instruction in src/clysm/compiler/compiler.lisp
- [x] T009 Add missing Wasm EH instructions to emitter if needed in src/clysm/compiler/compiler.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Catch/Throw (Priority: P1) 🎯 MVP

**Goal**: Implement basic catch/throw within same function - the fundamental non-local exit mechanism

**Independent Test**: Compile `(catch 'tag (throw 'tag 42))` and verify it returns 42

### Tests for User Story 1 (TDD - Write First)

- [x] T010 [P] [US1] Write unit test `test-catch-generates-try-table` verifying catch compiles to try_table structure in tests/unit/catch-test.lisp
- [x] T011 [P] [US1] Write unit test `test-throw-generates-throw-instruction` verifying throw compiles to throw instruction in tests/unit/catch-test.lisp
- [x] T012 [P] [US1] Write unit test `test-catch-normal-completion` verifying catch returns body value when no throw in tests/unit/catch-test.lisp
- [x] T013 [P] [US1] Write unit test `test-catch-throw-transfer` verifying throw transfers control to catch in tests/unit/catch-test.lisp

### Implementation for User Story 1

- [x] T014 [US1] Implement `compile-catch` generating `try_table` with `(:catch $lisp-throw)` clause in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US1] Implement exception handler block structure: outer block for result, inner block for exnref in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [US1] Implement tag evaluation and storage in local variable within compile-catch in src/clysm/compiler/codegen/func-section.lisp
- [x] T017 [US1] Implement body compilation within try_table with normal exit via `br` in src/clysm/compiler/codegen/func-section.lisp
- [x] T018 [US1] Implement `compile-throw` generating `(throw $lisp-throw tag value)` in src/clysm/compiler/codegen/func-section.lisp
- [x] T019 [US1] Implement exception handler: extract thrown-tag and value from exnref in src/clysm/compiler/codegen/func-section.lisp
- [x] T020 [US1] Implement tag comparison using eq and return value on match in src/clysm/compiler/codegen/func-section.lisp
- [x] T021 [US1] Implement rethrow via `throw_ref` when tags don't match in src/clysm/compiler/codegen/func-section.lisp
- [x] T022 [US1] Run existing test `test-simple-catch` in tests/integration/control-flow-test.lisp - should pass
- [x] T023 [US1] Run existing test `test-catch-throw` in tests/integration/control-flow-test.lisp - should pass

**Checkpoint**: User Story 1 (MVP) complete - basic catch/throw works within same function

---

## Phase 4: User Story 2 - Cross-Function Throw (Priority: P1)

**Goal**: Throw from called function unwinds to catch in caller

**Independent Test**: Define helper function that throws, wrap call in catch, verify throw propagates

### Tests for User Story 2 (TDD - Write First)

- [x] T024 [P] [US2] Write integration test `test-throw-deep-nesting` for 10 nested function calls with throw at deepest level in tests/integration/control-flow-test.lisp
- [x] T025 [P] [US2] Write integration test `test-throw-many-levels` verifying SC-003 (recursive function call unwinding) in tests/integration/control-flow-test.lisp

### Implementation for User Story 2

- [x] T026 [US2] Run existing test `test-throw-from-function` in tests/integration/control-flow-test.lisp - should pass (no code changes if US1 correct)
- [x] T027 [US2] Run test `test-throw-deep-nesting` - verify cross-function unwinding works
- [x] T028 [US2] Run test `test-throw-many-levels` - verify SC-003 compliance

**Checkpoint**: User Story 2 complete - cross-function throw works at any depth

---

## Phase 5: User Story 3 - Nested Catch with Tag Selection (Priority: P2)

**Goal**: Multiple catches with different tags work correctly - throw finds correct catch by tag

**Independent Test**: Nest catches with different tags, verify throw finds correct one

### Tests for User Story 3 (TDD - Write First)

- [x] T029 [P] [US3] Write integration test `test-three-nested-catches` with 3 different tags in tests/integration/control-flow-test.lisp
- [x] T030 [P] [US3] Write integration test `test-throw-skips-inner-catch` verifying inner catch with wrong tag is skipped in tests/integration/control-flow-test.lisp
- [x] T031 [P] [US3] Write integration test `test-code-after-inner-catch-not-executed` when throw targets outer in tests/integration/control-flow-test.lisp

### Implementation for User Story 3

- [x] T032 [US3] Run existing test `test-nested-catch` in tests/integration/control-flow-test.lisp - should pass
- [x] T033 [US3] Run existing test `test-catch-wrong-tag` in tests/integration/control-flow-test.lisp - should pass
- [x] T034 [US3] Run test `test-three-nested-catches` - verify complex nesting works
- [x] T035 [US3] Run test `test-throw-skips-inner-catch` - verify rethrow mechanism works
- [x] T036 [US3] Run test `test-code-after-inner-catch-not-executed` - verify proper control flow

**Checkpoint**: User Story 3 complete - nested catches with tag selection work correctly

---

## Phase 6: User Story 4 - Unwind-Protect Integration (Priority: P3)

**Goal**: Cleanup forms in unwind-protect execute during throw unwinding

**Independent Test**: Wrap throw in unwind-protect, verify cleanup executes before catch receives value

### Tests for User Story 4 (TDD - Write First)

- [x] T037 [P] [US4] Write integration test `test-throw-unwind-cleanup-order` for multiple nested unwind-protects with throw in tests/integration/control-flow-test.lisp
- [x] T038 [P] [US4] Write integration test `test-throw-through-multiple-unwind-protects` verifying all cleanups run in tests/integration/control-flow-test.lisp

### Implementation for User Story 4

- [x] T039 [US4] Update `compile-unwind-protect` to use `catch_all` handler for catching exceptions in src/clysm/compiler/codegen/func-section.lisp
- [x] T040 [US4] Implement cleanup execution followed by `throw_ref` in unwind-protect handler in src/clysm/compiler/codegen/func-section.lisp
- [x] T041 [US4] Run existing test `test-unwind-protect-with-throw` in tests/integration/control-flow-test.lisp - should pass
- [x] T042 [US4] Run test `test-throw-unwind-cleanup-order` - verify cleanup order (innermost first)
- [x] T043 [US4] Run test `test-throw-through-multiple-unwind-protects` - verify SC-004 (100% cleanup execution)

**Checkpoint**: User Story 4 complete - unwind-protect cleanup always runs during throw

---

## Phase 7: Edge Cases & Error Handling

**Purpose**: Handle error conditions per specification

### Tests for Edge Cases (TDD - Write First)

- [x] T044 [P] Write integration test `test-throw-no-matching-catch` verifying runtime error in tests/integration/control-flow-test.lisp
- [x] T045 [P] Write integration test `test-throw-nil-value` verifying NIL can be thrown in tests/integration/control-flow-test.lisp
- [x] T046 [P] Write integration test `test-catch-empty-body` verifying returns NIL in tests/integration/control-flow-test.lisp
- [x] T047 [P] Write integration test `test-catch-tag-runtime-eval` verifying tags evaluated at runtime in tests/integration/control-flow-test.lisp

### Implementation for Edge Cases

- [x] T048 Let unhandled throw propagate to host as runtime error (SC-005) - no code changes needed
- [x] T049 Run test `test-throw-no-matching-catch` - verify error behavior
- [x] T050 Run test `test-throw-nil-value` - verify NIL handling
- [x] T051 Run test `test-catch-empty-body` - verify empty body returns NIL
- [x] T052 Run test `test-catch-tag-runtime-eval` - verify runtime tag evaluation

**Checkpoint**: All edge cases handled correctly

---

## Phase 8: Performance & Polish

**Purpose**: Meet success criteria and final verification

### Performance Tests

- [x] T053 [P] Write stress test `test-many-nested-throws` (5000 iterations) for SC-002 in tests/integration/control-flow-test.lisp
- [x] T054 Run test `test-many-nested-throws` - verify no stack overflow

### Final Verification

- [x] T055 [P] Verify generated WAT for catch uses try_table (not legacy try/catch) by manual inspection
- [x] T056 [P] Verify all error messages are clear and actionable
- [x] T057 Run full test suite via `nix flake check` to ensure Nix-First compliance
- [x] T058 Code cleanup: Remove any debug code, ensure consistent formatting in src/clysm/compiler/codegen/func-section.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - MVP, basic catch/throw
- **User Story 2 (Phase 4)**: Depends on User Story 1 (validates cross-function)
- **User Story 3 (Phase 5)**: Depends on User Story 1 (nested catches need basic working)
- **User Story 4 (Phase 6)**: Depends on User Story 1 (unwind-protect needs throw working)
- **Edge Cases (Phase 7)**: Depends on all user stories
- **Polish (Phase 8)**: Depends on Edge Cases

### User Story Dependencies

| Story | Can Start After | Dependencies |
|-------|-----------------|--------------|
| US1 (Basic Catch/Throw) | Phase 2 | None - independent |
| US2 (Cross-Function) | US1 | Validates US1 works cross-function |
| US3 (Nested Catch) | US1 | Extends tag selection |
| US4 (Unwind-Protect) | US1 | Integrates cleanup with throw |

### Within Each User Story (TDD Order)

1. Write tests FIRST - must FAIL
2. Implement minimum code to pass
3. Refactor if needed
4. Verify tests pass
5. Move to next task

### Parallel Opportunities

**Phase 2 (Foundational)**:
```bash
# These can run in parallel:
T006, T007, T008  # Verify Wasm IR emitter support
```

**Phase 3 (User Story 1)**:
```bash
# Tests can run in parallel:
T010, T011, T012, T013
```

**Phase 4-6 (User Stories 2-4)**:
```bash
# Tests within each story can run in parallel:
# US2: T024, T025
# US3: T029, T030, T031
# US4: T037, T038
```

**Phase 7 (Edge Cases)**:
```bash
# All edge case tests can run in parallel:
T044, T045, T046, T047
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T009)
3. Complete Phase 3: User Story 1 (T010-T023) - **MVP DONE**
4. **STOP and VALIDATE**: Test basic catch/throw works
5. Demo/deploy if ready

### Incremental Delivery

| Milestone | Phases | Capability |
|-----------|--------|------------|
| Foundation | 1-2 | Wasm EH infrastructure ready |
| MVP | 1-3 | Basic catch/throw within function |
| Cross-Function | 1-4 | Throw works across any call depth |
| Nested Catches | 1-5 | Multiple catches with tag selection |
| Full Feature | 1-6 | Unwind-protect integration |
| Production | 1-8 | Verified and polished |

---

## Notes

- Constitution requires TDD - all tests must be written and fail before implementation
- rove is the testing framework - use `(deftest ...)` syntax
- Main implementation file: `src/clysm/compiler/codegen/func-section.lisp`
- Wasm EH requires `--wasm exceptions` flag in wasmtime
- Existing tests in `tests/integration/control-flow-test.lisp` serve as acceptance criteria
- Commit after each logical group of tasks
- Verify `nix flake check` passes before marking phase complete
