# Tasks: Eval/JIT Compile System

**Input**: Design documents from `/specs/017-eval-jit-compile/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution Principle VII (TDD - 非交渉)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story?] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1, US2, US3, US4, US5)
- Exact file paths included

## Path Conventions

- **Source**: `src/clysm/eval/` (existing module)
- **Tests**: `tests/unit/`, `tests/integration/`, `tests/contract/`

---

## Phase 1: Setup (Package Exports)

**Purpose**: Ensure package exports are complete for new functionality

- [x] T001 Add tiered-function exports to src/clysm/package.lisp
- [x] T002 [P] Add compile test package definition to tests/package.lisp

---

## Phase 2: Foundational (Data Structures)

**Purpose**: Core data structures that ALL user stories depend on

**⚠️ CRITICAL**: Must complete before any user story implementation

- [x] T003 Define tiered-function struct in src/clysm/eval/compile.lisp
- [x] T004 [P] Define tiered-function-wrapper macro in src/clysm/eval/compile.lisp
- [x] T005 [P] Add reset-invocation-counts utility for testing in src/clysm/eval/compile.lisp
- [x] T006 Add reset-function-slots utility for testing in src/clysm/eval/jit.lisp

**Checkpoint**: Foundation ready - tiered-function struct and utilities available

---

## Phase 3: User Story 1 - Compile Anonymous Lambda (Priority: P1) 🎯 MVP

**Goal**: `(compile nil '(lambda (x) (+ x 1)))` returns a callable function

**Independent Test**: Call `(compile* nil '(lambda (x) (+ x 1)))`, invoke result with 5, verify returns 6

### Tests for User Story 1

> **TDD: Write tests FIRST, ensure they FAIL before implementation**

- [x] T007 [P] [US1] Unit test for compile* with nil name in tests/unit/compile-test.lisp
- [x] T008 [P] [US1] Unit test for compile* with single-param lambda in tests/unit/compile-test.lisp
- [x] T009 [P] [US1] Unit test for compile* with multi-param lambda in tests/unit/compile-test.lisp
- [x] T010 [P] [US1] Unit test for compile* with zero-param lambda in tests/unit/compile-test.lisp
- [x] T011 [P] [US1] Unit test for compile* error on non-lambda in tests/unit/compile-test.lisp
- [x] T012 [US1] Integration test for anonymous lambda compilation in tests/integration/compile-test.lisp

### Implementation for User Story 1

- [x] T013 [US1] Implement validate-lambda-expr in src/clysm/eval/compile.lisp
- [x] T014 [US1] Update compile* to return wrapped tiered-function in src/clysm/eval/compile.lisp
- [x] T015 [US1] Implement make-tiered-wrapper function in src/clysm/eval/compile.lisp
- [x] T016 [US1] Verify compile* returns callable function for all lambda forms

**Checkpoint**: `(compile nil '(lambda ...))` works, returns callable function

---

## Phase 4: User Story 2 - Tier 1 Interpreted Execution (Priority: P1)

**Goal**: Newly compiled functions execute through Tier 1 interpreter by default

**Independent Test**: Compile a function, call it <10 times, verify executed through interpreter

### Tests for User Story 2

- [x] T017 [P] [US2] Unit test: new function starts at :tier-1 in tests/unit/compile-test.lisp
- [x] T018 [P] [US2] Unit test: function below threshold stays in Tier 1 in tests/unit/compile-test.lisp
- [x] T019 [P] [US2] Integration test for all special forms via Tier 1 in tests/integration/compile-test.lisp

### Implementation for User Story 2

- [x] T020 [US2] Ensure tiered-function initializes with tier=:tier-1 in src/clysm/eval/compile.lisp
- [x] T021 [US2] Verify interpreter handles all special forms (if, let, let*, lambda, progn, setq, block, return-from, tagbody, go, flet, labels) in src/clysm/eval/interpreter.lisp
- [x] T022 [US2] Add tier accessor function get-current-tier in src/clysm/eval/compile.lisp

**Checkpoint**: Tier 1 execution verified for all special forms

---

## Phase 5: User Story 3 - Automatic Tier Promotion (Priority: P2)

**Goal**: Functions exceeding threshold (default 10) automatically promote to Tier 2

**Independent Test**: Call function 11 times, verify tier changes from :tier-1 to :tier-2

### Tests for User Story 3

- [x] T023 [P] [US3] Unit test: invocation counter increments in tests/unit/compile-test.lisp
- [x] T024 [P] [US3] Unit test: should-promote-to-tier-2-p returns T after threshold in tests/unit/compile-test.lisp
- [x] T025 [P] [US3] Unit test: configurable threshold works in tests/unit/compile-test.lisp
- [x] T026 [P] [US3] Contract test: promotion produces valid Wasm in tests/contract/tier-promotion-test.lisp
- [x] T027 [US3] Integration test for automatic tier promotion in tests/integration/tier-promotion-test.lisp

### Implementation for User Story 3

- [x] T028 [US3] Implement invocation counting in tiered-wrapper in src/clysm/eval/compile.lisp
- [x] T029 [US3] Implement threshold check in wrapper invocation in src/clysm/eval/compile.lisp
- [x] T030 [US3] Implement attempt-tier-promotion with graceful degradation in src/clysm/eval/compile.lisp
- [x] T031 [US3] Update tiered-function tier slot on successful promotion in src/clysm/eval/compile.lisp
- [x] T032 [US3] Implement promotion-failed-p tracking to avoid retry in src/clysm/eval/compile.lisp

**Checkpoint**: Hot spot detection and automatic promotion working

---

## Phase 6: User Story 4 - Compile Named Functions (Priority: P2)

**Goal**: `(compile 'my-fn '(lambda ...))` registers function in symbol's function slot

**Independent Test**: Compile named function, lookup via function slot, verify callable

### Tests for User Story 4

- [x] T033 [P] [US4] Unit test: named compile registers in *function-slots* in tests/unit/compile-test.lisp
- [x] T034 [P] [US4] Unit test: get-function-slot retrieves registered function in tests/unit/compile-test.lisp
- [x] T035 [P] [US4] Unit test: recompile updates function slot (hot-patch) in tests/unit/compile-test.lisp
- [x] T036 [US4] Integration test for named function compile and call in tests/integration/compile-test.lisp

### Implementation for User Story 4

- [x] T037 [US4] Update compile* to register in *function-slots* when name provided in src/clysm/eval/compile.lisp
- [x] T038 [US4] Ensure hot-patch updates both function slot and tiered-function in src/clysm/eval/compile.lisp
- [x] T039 [US4] Add lookup-compiled-function utility in src/clysm/eval/compile.lisp

**Checkpoint**: Named function compilation and hot-patching working

---

## Phase 7: User Story 5 - Dynamic Module Linking (Priority: P3)

**Goal**: JIT-compiled Wasm modules link to runtime imports at instantiation

**Independent Test**: JIT-compile function using cons/car/cdr, verify correct execution

### Tests for User Story 5

- [x] T040 [P] [US5] Unit test: register-runtime-import adds to table in tests/unit/jit-test.lisp
- [x] T041 [P] [US5] Unit test: get-runtime-import retrieves function in tests/unit/jit-test.lisp
- [x] T042 [P] [US5] Contract test: JIT module receives imports in tests/contract/module-linking-test.lisp
- [x] T043 [US5] Integration test for JIT function calling runtime import in tests/integration/jit-test.lisp

### Implementation for User Story 5

- [x] T044 [US5] Enhance instantiate-wasm to bind runtime imports in src/clysm/eval/jit.lisp
- [x] T045 [US5] Add missing runtime imports (append, length, etc.) in src/clysm/eval/jit.lisp
- [x] T046 [US5] Implement import resolution for JIT-compiled modules in src/clysm/eval/jit.lisp

**Checkpoint**: JIT modules correctly link to runtime imports

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Integration, error handling, and validation

- [x] T047 [P] Add edge case test: compile invalid syntax in tests/unit/compile-test.lisp
- [x] T048 [P] Add edge case test: closure environment capture in tests/integration/compile-test.lisp
- [x] T049 [P] Add edge case test: Tier 2 compilation failure graceful degradation in tests/integration/tier-promotion-test.lisp
- [x] T050 [P] Add edge case test: recursive function tier promotion in tests/integration/tier-promotion-test.lisp
- [x] T051 [P] Add edge case test: hot-patch during execution in tests/integration/compile-test.lisp
- [x] T052 Verify all tests pass via nix flake check
- [x] T053 Run quickstart.md validation examples
- [x] T054 Update CLAUDE.md with feature completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - can start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phases 3-7 (User Stories)**: All depend on Phase 2 completion
- **Phase 8 (Polish)**: Depends on all user stories complete

### User Story Dependencies

| Story | Priority | Dependencies | Can Parallel With |
|-------|----------|--------------|-------------------|
| US1 (Anonymous Lambda) | P1 | Phase 2 only | US2 |
| US2 (Tier 1 Execution) | P1 | Phase 2 only | US1 |
| US3 (Tier Promotion) | P2 | US1 (needs compile*) | US4 |
| US4 (Named Functions) | P2 | US1 (needs compile*) | US3 |
| US5 (Module Linking) | P3 | US3 (needs JIT path) | - |

### Within Each User Story

1. Tests FIRST (TDD) - must FAIL before implementation
2. Core implementation
3. Integration verification
4. Checkpoint validation

### Parallel Opportunities

```text
Phase 1: T001, T002 can run in parallel
Phase 2: T003 first, then T004-T006 in parallel
Phase 3: T007-T012 tests in parallel, then T013-T016 sequential
Phase 4: T017-T019 tests in parallel, then T020-T022 sequential
Phase 5: T023-T027 tests in parallel, then T028-T032 sequential
Phase 6: T033-T036 tests in parallel, then T037-T039 sequential
Phase 7: T040-T043 tests in parallel, then T044-T046 sequential
Phase 8: T047-T051 in parallel, then T052-T054 sequential
```

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together (they're in same file but independent test cases):
tests/unit/compile-test.lisp - test-compile-nil-name
tests/unit/compile-test.lisp - test-compile-single-param
tests/unit/compile-test.lisp - test-compile-multi-param
tests/unit/compile-test.lisp - test-compile-zero-param
tests/unit/compile-test.lisp - test-compile-error-non-lambda
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (Anonymous Lambda)
4. Complete Phase 4: User Story 2 (Tier 1 Execution)
5. **STOP and VALIDATE**: `(compile nil '(lambda (x) (+ x 1)))` works
6. Deploy/demo if ready

### Incremental Delivery

1. **MVP**: Setup + Foundational + US1 + US2 → Basic compile works
2. **+Optimization**: Add US3 → Automatic tier promotion
3. **+Named Functions**: Add US4 → defun-like semantics
4. **+Full JIT**: Add US5 → Complete module linking
5. Each story adds value without breaking previous stories

---

## Task Count Summary

| Phase | Tasks | Parallel |
|-------|-------|----------|
| Setup | 2 | 2 |
| Foundational | 4 | 3 |
| US1 (P1) | 10 | 6 |
| US2 (P1) | 6 | 3 |
| US3 (P2) | 10 | 5 |
| US4 (P2) | 7 | 4 |
| US5 (P3) | 7 | 4 |
| Polish | 8 | 5 |
| **Total** | **54** | **32** |

---

## Notes

- [P] tasks = different files or independent test cases
- [Story] label maps task to user story for traceability
- TDD enforced per Constitution Principle VII
- Graceful degradation on Tier 2 failure per research.md decision
- Single-threaded assumption simplifies hot-patching
- Commit after each task or logical group
- Run `nix flake check` after each phase
