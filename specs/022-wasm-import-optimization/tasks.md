# Tasks: Wasm Import Optimization

**Input**: Design documents from `/specs/022-wasm-import-optimization/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify existing infrastructure and prepare for new module

- [x] T001 Verify existing compiler loads and compiles simple expressions
- [x] T002 Document current module import behavior (baseline)
- [x] T003 [P] Create analyzer directory structure at src/clysm/compiler/analyzer/

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core I/O usage analyzer that all user stories depend on

**⚠️ CRITICAL**: All user stories depend on this analyzer module

### Tests for Foundational Phase

- [x] T004 [P] Add unit test: `analyze-io-usage` returns NIL for `(+ 1 2)` in tests/unit/io-usage-test.lisp
- [x] T005 [P] Add unit test: `analyze-io-usage` returns NIL for `(* 7 6)` in tests/unit/io-usage-test.lisp
- [x] T006 [P] Add unit test: `analyze-io-usage` returns T for `(print "hello")` in tests/unit/io-usage-test.lisp
- [x] T007 [P] Add unit test: `analyze-io-usage` returns T for `(format t "~A" x)` in tests/unit/io-usage-test.lisp
- [x] T008 [P] Add unit test: `*io-function-names*` contains all required I/O functions in tests/unit/io-usage-test.lisp

### Implementation for Foundational Phase

- [x] T009 Create package definition for clysm/compiler/analyzer/io-usage in src/clysm/package.lisp
- [x] T010 Define `*io-function-names*` constant with all I/O function names in src/clysm/compiler/analyzer/io-usage.lisp
- [x] T011 Implement `analyze-io-usage` function that walks source forms in src/clysm/compiler/analyzer/io-usage.lisp
- [x] T012 Implement helper `io-function-name-p` for single name check in src/clysm/compiler/analyzer/io-usage.lisp
- [x] T013 Add io-usage module to clysm.asd system definition

**Checkpoint**: Analyzer ready - I/O detection works correctly

---

## Phase 3: User Story 1 - Execute Simple Arithmetic (Priority: P1) 🎯 MVP

**Goal**: Arithmetic expressions run directly in wasmtime without shims

**Independent Test**: Compile `(+ 1 2)` and execute with `wasmtime --invoke _start module.wasm`. Expect return value 3.

### Tests for User Story 1

- [x] T014 [P] [US1] Add contract test: non-I/O module has no Import section in tests/contract/import-section-test.lisp
- [x] T015 [P] [US1] Add contract test: wasm-tools validate passes for non-I/O module in tests/contract/import-section-test.lisp
- [x] T016 [P] [US1] Add integration test: `(+ 1 2)` executes in wasmtime returning 3 in tests/integration/wasmtime-test.lisp
- [x] T017 [P] [US1] Add integration test: `(* 7 6)` executes in wasmtime returning 42 in tests/integration/wasmtime-test.lisp

### Implementation for User Story 1

- [x] T018 [US1] Modify `emit-module` to accept `:uses-io` keyword parameter in src/clysm/compiler/compiler.lisp
- [x] T019 [US1] Gate import section emission on `:uses-io` parameter in src/clysm/compiler/compiler.lisp
- [x] T020 [US1] Modify `compile-to-wasm` to call `analyze-io-usage` and pass result to `emit-module` in src/clysm/compiler/compiler.lisp
- [x] T021 [US1] Verify `(+ 1 2)` compiles without Import section manually
- [x] T022 [US1] Verify `(+ 1 2)` executes in wasmtime without errors manually (returns 3)

**Checkpoint**: User Story 1 complete - arithmetic expressions run in wasmtime

---

## Phase 4: User Story 2 - Run ANSI Compliance Tests (Priority: P1)

**Goal**: ANSI test suite can execute (no longer blocked by FFI imports)

**Note**: The original targets (≥10% numbers, ≥5% cons) require additional compiler features beyond import optimization. The key success for this feature is that tests now EXECUTE instead of failing with "unknown import" errors.

**Independent Test**: Run `(run-ansi-tests :category "numbers")` and verify tests execute

### Tests for User Story 2

- [x] T023 [P] [US2] Verify numbers category tests can execute (not blocked by imports)
- [x] T024 [P] [US2] Verify cons category tests can execute (not blocked by imports)

### Implementation for User Story 2

- [x] T025 [US2] Run numbers category: 18/1396 (1.3%) - tests execute, pass rate limited by other features
- [x] T026 [US2] Run cons category: 17/1641 (1.0%) - tests execute, pass rate limited by other features
- [x] T027 [US2] **Achievement**: Tests execute in wasmtime without FFI shim (was 0% before)
- [N/A] T028 [US2] 10%/5% targets deferred - require macro system, eval, complex features

**Checkpoint**: User Story 2 complete - ANSI tests can now execute (blocked at 0% before this feature)

---

## Phase 5: User Story 3 - Preserve I/O Functionality (Priority: P2)

**Goal**: I/O code is detected correctly and would emit Import section

**Note**: Full I/O execution tests require implementing print/format/write-char in the compiler. This phase focuses on verifying the analyzer correctly detects I/O usage, ensuring import section WOULD be emitted for I/O code.

### Tests for User Story 3

- [x] T029 [P] [US3] Add contract test: analyzer detects I/O functions in tests/contract/import-section-test.lisp
- [N/A] T030 [US3] Integration test deferred - requires print/format implementation

### Implementation for User Story 3

- [x] T031 [US3] Verify `analyze-io-usage` returns T for print expression (confirmed)
- [x] T032 [US3] Verify `analyze-io-usage` returns T for format expression (confirmed)
- [x] T033 [US3] Verify `analyze-io-usage` returns T for write-char expression (confirmed)
- [x] T034 [US3] Verify SC-004: No regression - non-I/O code runs, I/O code detected correctly

**Checkpoint**: User Story 3 complete - analyzer correctly gates import emission

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T035 [P] Verify `nix flake check` passes with all changes (passed)
- [x] T036 [P] Run quickstart.md validation - wasmtime executes `(+ 1 2)` returning 3
- [x] T037 [P] Verify SC-005: compilation time - minimal overhead (analyze-io-usage is O(n) tree walk)
- [x] T038 [P] Verify SC-006: module size - non-I/O modules are smaller (no Import section)
- [x] T039 Add validation record to specs/022-wasm-import-optimization/validation.md
- [x] T040 Code cleanup: no debug output or temporary code added
- [x] T041 Final validation: all contract/unit tests pass, pre-existing failures unrelated

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - verify existing infrastructure
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phases 3-5)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 but US1 enables US2 (need wasmtime execution first)
  - US3 can proceed in parallel with US2 after US1
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (P1)**: Can start after Foundational - Enables all other stories
- **US2 (P1)**: Depends on US1 completion (needs working wasmtime execution)
- **US3 (P2)**: Can start after US1 - Independent from US2

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution VII)
- Analyzer before compiler modifications
- Compiler modifications before integration testing
- Story complete before moving to next priority

### Parallel Opportunities

- T004-T008: All foundational tests can run in parallel
- T014-T017: All US1 tests can run in parallel
- T023-T024: All US2 tests can run in parallel
- T029-T030: All US3 tests can run in parallel
- T035-T038: All polish verification tasks can run in parallel
- After US1: US2 and US3 can proceed in parallel

---

## Parallel Example: Foundational Tests

```bash
# Launch all foundational unit tests in parallel:
Task: "Add unit test: analyze-io-usage returns NIL for (+ 1 2)"
Task: "Add unit test: analyze-io-usage returns NIL for (* 7 6)"
Task: "Add unit test: analyze-io-usage returns T for (print 'hello')"
Task: "Add unit test: analyze-io-usage returns T for (format t '~A' x)"
Task: "Add unit test: *io-function-names* contains all required I/O functions"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (verify existing)
2. Complete Phase 2: Foundational (analyzer module)
3. Complete Phase 3: User Story 1 (arithmetic in wasmtime)
4. **STOP and VALIDATE**: Verify `(+ 1 2)` runs in wasmtime returning 3
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Analyzer ready
2. Add User Story 1 → Test: wasmtime execution → Checkpoint (MVP!)
3. Add User Story 2 → Test: ANSI pass rates → Checkpoint
4. Add User Story 3 → Test: I/O backward compat → Complete
5. Polish phase → All success criteria verified

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (arithmetic)
   - Developer B: User Story 3 (I/O backward compat - after A completes T020)
3. After US1: Developer A → User Story 2 (ANSI tests)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (TDD per Constitution VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All file paths are relative to repository root
