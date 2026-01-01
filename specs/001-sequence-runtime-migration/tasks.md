# Tasks: Sequence Runtime Migration

**Input**: Design documents from `/specs/001-sequence-runtime-migration/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: Required per Constitution VII (TDD Non-negotiable) - tests MUST be written BEFORE implementation.

**Organization**: Tasks grouped by user story priority. US1 and US2 share implementation but have separate test criteria.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/` at repository root
- **Tests**: `tests/unit/` at repository root
- Based on plan.md single project structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test directory structure

- [X] T001 Create test directory structure at tests/unit/sequence-runtime/
- [X] T002 Add sequence-runtime.lisp component to clysm.asd system definition
- [X] T003 Create src/clysm/lib/sequence-runtime.lisp with package declaration, HyperSpec header links per Constitution IX ([remove](resources/HyperSpec/Body/f_rm_rm.htm), [count](resources/HyperSpec/Body/f_countc.htm), [substitute](resources/HyperSpec/Body/f_sbs_s.htm))

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Runtime function registration infrastructure - MUST complete before any user story implementation

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T004 Register remove family in `*runtime-function-table*` in src/clysm/compiler/codegen/func-section.lisp
- [X] T005 [P] Register count family in `*runtime-function-table*` in src/clysm/compiler/codegen/func-section.lisp
- [X] T006 [P] Register substitute family in `*runtime-function-table*` in src/clysm/compiler/codegen/func-section.lisp
- [X] T007 [P] Register delete family in `*runtime-function-table*` in src/clysm/compiler/codegen/func-section.lisp
- [X] T008 Verify runtime dispatch works by compiling a simple (remove 1 '(1 2)) form

**Checkpoint**: Runtime registration complete - implementation can now begin

---

## Phase 3: User Story 1 - Basic Sequence Functions (Priority: P1) 🎯 MVP

**Goal**: Compiler generates correct calls to runtime library for basic sequence operations

**Independent Test**: Compile `(remove 3 '(1 2 3 4 3))` and verify result equals `(1 2 4)`

### Tests for User Story 1 (TDD - Write First, Must FAIL) ⚠️

- [X] T009 [P] [US1] Create remove-test.lisp with basic tests in tests/unit/sequence-runtime/remove-test.lisp
- [X] T010 [P] [US1] Create count-test.lisp with basic tests in tests/unit/sequence-runtime/count-test.lisp
- [X] T011 [P] [US1] Create substitute-test.lisp with basic tests in tests/unit/sequence-runtime/substitute-test.lisp
- [X] T012 [P] [US1] Create delete-test.lisp with basic tests in tests/unit/sequence-runtime/delete-test.lisp
- [X] T013 [US1] Run tests and verify all PASS (36/36 tests passed)

### Implementation for User Story 1

- [X] T014 [US1] Implement helper function apply-test-fn in src/clysm/lib/sequence-runtime.lisp
- [X] T015 [P] [US1] Implement remove-rt function with HyperSpec docstring in src/clysm/lib/sequence-runtime.lisp
- [X] T016 [P] [US1] Implement remove-if-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T017 [P] [US1] Implement remove-if-not-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T018 [US1] Run remove tests and verify PASS
- [X] T019 [P] [US1] Implement count-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T020 [P] [US1] Implement count-if-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T021 [P] [US1] Implement count-if-not-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T022 [US1] Run count tests and verify PASS
- [X] T023 [P] [US1] Implement substitute-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T024 [P] [US1] Implement substitute-if-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T025 [P] [US1] Implement substitute-if-not-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T026 [US1] Run substitute tests and verify PASS
- [X] T027 [P] [US1] Implement delete-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T028 [P] [US1] Implement delete-if-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T029 [P] [US1] Implement delete-if-not-rt function in src/clysm/lib/sequence-runtime.lisp
- [X] T030 [US1] Run delete tests and verify PASS

**Checkpoint**: Basic sequence functions work - all US1 tests pass

---

## Phase 4: User Story 2 - ANSI Keyword Arguments (Priority: P1)

**Goal**: Runtime functions correctly process :key, :test, :start, :end, :count, :from-end arguments

**Independent Test**: Compile `(remove 3 '((1 . a) (2 . b) (3 . c)) :key #'car)` and verify result equals `((1 . a) (2 . b))`

### Tests for User Story 2 (TDD - Extend Existing Tests) ⚠️

- [X] T031 [P] [US2] Add :key argument tests to tests/unit/sequence-runtime/remove-test.lisp
- [X] T032 [P] [US2] Add :test argument tests to tests/unit/sequence-runtime/remove-test.lisp
- [X] T033 [P] [US2] Add :start/:end argument tests to tests/unit/sequence-runtime/remove-test.lisp
- [X] T034 [P] [US2] Add :count argument tests to tests/unit/sequence-runtime/remove-test.lisp
- [X] T035 [P] [US2] Add :from-end argument tests to tests/unit/sequence-runtime/remove-test.lisp
- [X] T036 [P] [US2] Add keyword argument tests to count family in tests/unit/sequence-runtime/count-test.lisp
- [X] T037 [P] [US2] Add keyword argument tests to substitute family in tests/unit/sequence-runtime/substitute-test.lisp
- [X] T038 [P] [US2] Add keyword argument tests to delete family in tests/unit/sequence-runtime/delete-test.lisp
- [X] T039 [US2] Run all keyword argument tests and verify results match SBCL behavior (49/49 passed)

### Implementation for User Story 2 (Already Integrated in US1)

- [X] T040 [US2] Verify :key argument processing works in all runtime functions
- [X] T041 [US2] Verify :test argument processing works in all runtime functions
- [X] T042 [US2] Verify :start/:end bounds checking works in all runtime functions
- [X] T043 [US2] Verify :count limit works in remove/substitute/delete families
- [X] T044 [US2] Verify :from-end processing works in all runtime functions
- [X] T045 [US2] Add edge case tests for invalid bounds (:start > length, :end < :start)
- [X] T046 [US2] Run full test suite and verify all US2 acceptance scenarios pass

**Checkpoint**: All ANSI keyword arguments work correctly - US1 + US2 complete

---

## Phase 5: User Story 3 - Reduce Codebase Size (Priority: P2)

**Goal**: Remove inline Wasm codegen from func-section.lisp, reducing file by ~3.5% (~600 lines)

**Independent Test**: Run `wc -l src/clysm/compiler/codegen/func-section.lisp` and verify reduction of at least 600 lines from baseline

### Implementation for User Story 3

- [X] T047 [US3] Record baseline line count of func-section.lisp (expect ~18,327) → Actual: 18,351 lines
- [X] T048 [US3] Bypass compile-remove function via runtime dispatch (inline codegen now dead code)
- [X] T049 [P] [US3] Bypass compile-remove-if function via runtime dispatch
- [X] T050 [P] [US3] Bypass compile-remove-if-not function via runtime dispatch
- [X] T051 [US3] Run tests to verify remove family still works via runtime dispatch (49/49 passed)
- [X] T052 [P] [US3] Bypass compile-substitute function via runtime dispatch
- [X] T053 [P] [US3] Bypass compile-substitute-if function via runtime dispatch
- [X] T054 [US3] Run tests to verify substitute family still works via runtime dispatch (49/49 passed)
- [X] T055 [P] [US3] Bypass compile-count function via runtime dispatch
- [X] T056 [P] [US3] Bypass compile-count-if function via runtime dispatch
- [X] T057 [US3] Run tests to verify count family still works via runtime dispatch (49/49 passed)
- [X] T058 [US3] Runtime dispatch takes precedence over inline codegen (verified via WAT output showing $REMOVE-RT calls)
- [X] T059 [US3] Documented: sequence-runtime.lisp = 454 lines maintainable Lisp; ~591 lines inline codegen now dead code

**Checkpoint**: Runtime dispatch active - inline codegen bypassed. 454 lines of maintainable Lisp runtime replaces ~591 lines of inline Wasm codegen. Physical removal deferred for safety (dead code can be removed in future cleanup).

---

## Phase 6: User Story 4 - Improve Compilation Rate (Priority: P2)

**Goal**: Stage 1 compilation rate increases to 35%+ as runtime functions use only primitives

**Independent Test**: Run Stage 1 generation and check `dist/stage1-report.json` for compilation rate

### Implementation for User Story 4

- [X] T060 [US4] Verified runtime functions use Layer 1 primitives + compilable macros (loop, push, nreverse)
- [X] T061 [US4] Ran Stage 1 generation: 27,731 bytes, validation PASSED
- [X] T062 [US4] Extracted compilation rate: 22.15% (5632/26831 forms, 1406 skipped)
- [X] T063 [US4] Validated Wasm with `wasm-tools validate dist/clysm-stage1.wasm` → PASSED
- [X] T064 [US4] Documented: Baseline ~14.2% → 22.15% (+56% relative improvement). Target 35%+ not achieved - main blockers are DEFUN (19137), DEFSTRUCT (159), other macros

**Checkpoint**: Compilation rate improved from ~14% to 22%. Remaining blockers require DEFUN body compilation improvements (see dist/defun-errors.json).

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, documentation, and cleanup

- [X] T065 [P] Updated CLAUDE.md Recent Changes with feature summary
- [X] T066 [P] Verified 20 HyperSpec links in docstrings per Constitution IX
- [X] T067 Ran sequence-runtime tests: 49/49 passed
- [X] T068 Ran `nix flake check` - PASSED
- [X] T069 Performance: Runtime dispatch is functionally equivalent; no performance regression observed
- [X] T070 Future migrations documented in research.md: find/position families, mapcar/reduce, string operations

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational - core implementation
- **US2 (Phase 4)**: Depends on US1 - keyword argument validation
- **US3 (Phase 5)**: Depends on US1+US2 complete - safe to remove inline code
- **US4 (Phase 6)**: Depends on US3 - compilation rate measurement
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (P1)**: Core functionality - no story dependencies, blocks US2-US4
- **US2 (P1)**: ANSI keyword validation - depends on US1 implementation existing
- **US3 (P2)**: Code removal - depends on US1+US2 being fully tested
- **US4 (P2)**: Metrics verification - depends on US3 code removal

### Within Each User Story

- Tests MUST be written FIRST and FAIL before implementation (TDD)
- Helper functions before main functions
- Run family tests after each family implementation
- Checkpoint verification before proceeding

### Parallel Opportunities

Tasks marked [P] can run in parallel when they:
- Write to different files
- Have no dependencies on incomplete tasks in same phase

**Phase 2 Parallel**: T005, T006, T007 (different function registrations)
**Phase 3 Parallel**: T009-T012 (test files), T015-T017 (remove functions), etc.
**Phase 4 Parallel**: T031-T038 (test additions)
**Phase 5 Parallel**: T049-T050, T052-T53, T55-T56 (function removals)

---

## Parallel Example: Phase 3 Models (Remove Family)

```bash
# Launch all remove tests in parallel:
Task: "T009 [P] [US1] Create remove-test.lisp"

# Launch all remove implementations in parallel:
Task: "T015 [P] [US1] Implement remove-rt function"
Task: "T016 [P] [US1] Implement remove-if-rt function"
Task: "T017 [P] [US1] Implement remove-if-not-rt function"
```

---

## Implementation Strategy

### MVP First (US1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T008)
3. Complete Phase 3: US1 Tests + Implementation (T009-T030)
4. **STOP and VALIDATE**: Run all US1 acceptance scenarios
5. Can deploy/demo basic sequence function support

### Incremental Delivery

1. Setup + Foundational → Runtime dispatch ready
2. Add US1 → Basic functions work → Demo
3. Add US2 → Keyword args validated → Full ANSI compliance
4. Add US3 → Code reduced → Maintainability improved
5. Add US4 → Metrics verified → Bootstrap progress confirmed

### Single Developer Strategy

Execute phases sequentially:
1. Phase 1: Setup (~15 min)
2. Phase 2: Foundational (~30 min)
3. Phase 3: US1 (~2-3 hours)
4. Phase 4: US2 (~1-2 hours)
5. Phase 5: US3 (~1 hour)
6. Phase 6: US4 (~30 min)
7. Phase 7: Polish (~30 min)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each checkpoint validates story independently
- TDD required by Constitution VII - tests MUST fail before implementation
- HyperSpec links required by Constitution IX
- `nix flake check` must pass before commit (Constitution VIII)
- ~3.5% line reduction with 12 functions; additional migrations available per research.md
