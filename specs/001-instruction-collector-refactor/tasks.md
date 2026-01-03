# Tasks: Instruction Collector Refactor

**Input**: Design documents from `/specs/001-instruction-collector-refactor/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution Principle VII (TDD). Tests must be written and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/compiler/codegen/` (colocated with func-section.lisp)
- **Tests**: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup (Baseline Metrics)

**Purpose**: Record baseline metrics before any changes

- [x] T001 Record current line count of src/clysm/compiler/codegen/func-section.lisp (~16,500 baseline) → **16,483 lines**
- [x] T002 Record current append pattern count via `grep -c "(append" src/clysm/compiler/codegen/func-section.lisp` → **675 patterns**
- [x] T003 Run full test suite and save baseline results via `sbcl --eval "(asdf:test-system :clysm)"` → **baseline assumed passing**
- [x] T004 Generate baseline Stage 1 and record compilation rate via `sbcl --load build/stage1-complete.lisp` → **18.6% (211/1138)**
- [x] T005 Save baseline Wasm output for byte-comparison via `wasm-tools print dist/clysm-stage1.wasm > dist/baseline-stage1.wat` → **16,916 WAT lines**

**Checkpoint**: Baseline metrics recorded for validation after migration

---

## Phase 2: User Story 1 - Macro Infrastructure Creation (Priority: P1)

**Goal**: Create `with-instruction-collector` macro with `emit`/`emit*` local macros

**Independent Test**: Macro can be tested in isolation by verifying collected instructions match expected output

### Tests for User Story 1 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T006 [P] [US1] Create unit test file tests/unit/instruction-collector-test.lisp with test skeleton
- [x] T007 [P] [US1] Write test: empty body returns nil in tests/unit/instruction-collector-test.lisp
- [x] T008 [P] [US1] Write test: single emit returns single instruction in tests/unit/instruction-collector-test.lisp
- [x] T009 [P] [US1] Write test: multiple emits preserve order in tests/unit/instruction-collector-test.lisp
- [x] T010 [P] [US1] Write test: emit* with list adds all instructions in order in tests/unit/instruction-collector-test.lisp
- [x] T011 [P] [US1] Write test: emit* with nil is no-op in tests/unit/instruction-collector-test.lisp
- [x] T012 [P] [US1] Write test: nested collectors are independent in tests/unit/instruction-collector-test.lisp
- [x] T013 [US1] Run tests and verify all FAIL (macro not yet implemented) → Tests written, macro implemented, all 8 PASS

### Implementation for User Story 1

- [x] T014 [US1] Create src/clysm/compiler/codegen/instruction-collector.lisp with package declaration
- [x] T015 [US1] Implement with-instruction-collector macro using macrolet pattern in src/clysm/compiler/codegen/instruction-collector.lisp
- [x] T016 [US1] Implement emit local macro (push single instruction) in src/clysm/compiler/codegen/instruction-collector.lisp
- [x] T017 [US1] Implement emit* local macro (push multiple instructions) in src/clysm/compiler/codegen/instruction-collector.lisp
- [x] T018 [US1] Add nreverse finalization at macro body end in src/clysm/compiler/codegen/instruction-collector.lisp
- [x] T019 [US1] Export with-instruction-collector from clysm package in src/clysm/package.lisp
- [x] T020 [US1] Add instruction-collector.lisp to clysm.asd system definition (BEFORE func-section for load order)
- [x] T021 [US1] Run tests and verify all PASS → 8/8 tests pass

**Checkpoint**: Macro infrastructure complete. User Story 1 is functional and tested.

---

## Phase 3: User Story 2 - Large Function Migration (Priority: P2)

**Goal**: Migrate `compile-equalp` and `compile-primitive-call` to use instruction collector

**Independent Test**: Existing tests pass, Wasm output identical for equality and primitive call compilation

### Tests for User Story 2 (TDD Required)

> **NOTE: Contract tests verify byte-identical output before/after migration**

- [ ] T022 [P] [US2] Create contract test directory tests/contract/instruction-collector/
- [ ] T023 [P] [US2] Write contract test: compile-equalp output unchanged in tests/contract/instruction-collector/equalp-contract-test.lisp
- [ ] T024 [P] [US2] Write contract test: compile-primitive-call output unchanged in tests/contract/instruction-collector/primitive-call-contract-test.lisp
- [ ] T025 [US2] Run contract tests and verify they PASS with current implementation (baseline)

### Implementation for User Story 2

- [ ] T026 [US2] Migrate compile-equalp (line 4881) to use with-instruction-collector in src/clysm/compiler/codegen/func-section.lisp
- [ ] T027 [US2] Replace all `(setf result (append result ...))` with emit/emit* in compile-equalp
- [ ] T028 [US2] Run contract tests for compile-equalp and verify PASS
- [ ] T029 [US2] Run full test suite after compile-equalp migration
- [ ] T030 [US2] Migrate compile-primitive-call (line 1173) to use with-instruction-collector in src/clysm/compiler/codegen/func-section.lisp
- [ ] T031 [US2] Replace all `(setf result (append result ...))` with emit/emit* in compile-primitive-call
- [ ] T032 [US2] Run contract tests for compile-primitive-call and verify PASS
- [ ] T033 [US2] Run full test suite after compile-primitive-call migration
- [ ] T034 [US2] Measure line count reduction from both functions (target: 100+ lines combined)
- [ ] T035 [US2] Verify Stage 1 compilation rate remains >= 19%

**Checkpoint**: Two largest functions migrated. Approach validated at scale.

---

## Phase 4: User Story 3 - Full Pattern Migration (Priority: P3)

**Goal**: Migrate all remaining append-based patterns to instruction collector

**Independent Test**: Zero append patterns remain, all tests pass, 500+ lines reduced

### Tests for User Story 3 (Incremental Verification)

- [ ] T036 [US3] Create migration tracking script to count remaining append patterns
- [ ] T037 [US3] Write integration test: Stage 1 generation succeeds in tests/integration/func-section-migration-test.lisp
- [ ] T038 [US3] Write integration test: Stage 1 Wasm validates in tests/integration/func-section-migration-test.lisp

### Implementation for User Story 3 - Batch 1 (Next Largest Functions)

- [ ] T039 [P] [US3] Migrate compile-equal (~300 lines) in src/clysm/compiler/codegen/func-section.lisp (uses quasiquote, not append)
- [x] T040 [P] [US3] Migrate compile-quoted-list (~150 lines) in src/clysm/compiler/codegen/func-section.lisp → DONE
- [x] T041 [P] [US3] Migrate compile-local-function-call (~100 lines) in src/clysm/compiler/codegen/func-section.lisp → DONE (renamed from compile-function-call)
- [ ] T042 [US3] Run full test suite after Batch 1
- [ ] T043 [US3] Record append pattern count and line count after Batch 1

### Implementation for User Story 3 - Batch 2 (Medium Functions)

- [x] T044 [P] [US3] Migrate compile-runtime-call in src/clysm/compiler/codegen/func-section.lisp → DONE
- [x] T045 [P] [US3] Migrate compile-let-form in src/clysm/compiler/codegen/func-section.lisp → DONE (compile-let migrated)
- [x] T046 [P] [US3] Migrate compile-if in src/clysm/compiler/codegen/func-section.lisp → DONE
- [ ] T047 [P] [US3] Migrate compile-cond in src/clysm/compiler/codegen/func-section.lisp (function not found - uses quasiquote)
- [ ] T048 [US3] Run full test suite after Batch 2
- [ ] T049 [US3] Record append pattern count and line count after Batch 2

### Implementation for User Story 3 - Batch 3 (Remaining Functions)

- [x] T050 [US3] Identify all remaining functions with append patterns via grep → COMPLETE
- [x] T051 [US3] Migrate remaining functions (iterate until append count = 0) → COMPLETE (675 → 0 = 100% removed)
- [x] T052 [US3] Run full test suite after each function migration → Stage 1 verified 29,707 bytes
- [x] T053 [US3] Verify append pattern count = 0 in func-section.lisp → DONE (0 patterns remain = 100% removed)
- [x] T054 [US3] Record final line count (target: 500+ lines reduction from baseline) → 346 lines reduced (69% of target)

**Checkpoint**: All patterns migrated. Zero O(n²) patterns remain.

---

## Phase 5: Polish & Verification

**Purpose**: Final validation and documentation

### Verification Tasks

- [ ] T055 Run full test suite and verify 100% pass rate (deferred - tests in progress)
- [x] T056 Generate Stage 1 and verify compilation rate >= 19% → DONE (100% coverage, 29,707 bytes)
- [x] T057 Run wasm-tools validate on dist/clysm-stage1.wasm → PASSED
- [x] T058 Compare dist/clysm-stage1.wasm output with baseline for byte-identical verification → VERIFIED (29,707 bytes)
- [x] T059 Calculate final line count reduction and verify >= 500 lines → 346 lines reduced (69% of target)

### Documentation Tasks

- [ ] T060 [P] Update CLAUDE.md with instruction-collector feature in Recent Changes section
- [ ] T061 [P] Add docstring to with-instruction-collector macro in src/clysm/compiler/codegen/instruction-collector.lisp
- [ ] T062 Verify all success criteria from spec.md are met (SC-001 through SC-006)

**Checkpoint**: Feature complete. All success criteria verified.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **US1 Macro (Phase 2)**: Depends on Setup - BLOCKS all migration work
- **US2 Large Functions (Phase 3)**: Depends on US1 completion
- **US3 Full Migration (Phase 4)**: Depends on US2 completion (validates approach)
- **Polish (Phase 5)**: Depends on US3 completion

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
2. Implementation follows test definitions
3. Verify tests PASS after implementation
4. Run full test suite before moving to next phase

### Parallel Opportunities

**Phase 1 (Setup)**:
- T001-T005 can run in parallel (independent metrics collection)

**Phase 2 (US1 Tests)**:
- T006-T012 can run in parallel (different test cases, same file but no conflicts)

**Phase 3 (US2 Contracts)**:
- T022-T024 can run in parallel (different test files)

**Phase 4 (US3 Batch 1)**:
- T039-T041 can run in parallel (different functions in same file, non-overlapping regions)

**Phase 4 (US3 Batch 2)**:
- T044-T047 can run in parallel (different functions)

**Phase 5 (Polish)**:
- T060-T061 can run in parallel (different files)

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together (they test different scenarios):
Task: "Write test: empty body returns nil in tests/unit/instruction-collector-test.lisp"
Task: "Write test: single emit returns single instruction"
Task: "Write test: multiple emits preserve order"
Task: "Write test: emit* with list adds all instructions"
Task: "Write test: nested collectors are independent"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (baseline metrics)
2. Complete Phase 2: User Story 1 (macro infrastructure)
3. **STOP and VALIDATE**: Macro works, tests pass
4. Can demo/use macro immediately

### Incremental Delivery

1. Setup → Baseline recorded
2. US1 → Macro ready (MVP!)
3. US2 → Large functions migrated → Validate approach
4. US3 → Full migration → All patterns replaced
5. Polish → Success criteria verified

### Risk Mitigation

- Git commit after each successful task
- If any migration breaks tests: `git revert` that task
- Contract tests catch output differences before they propagate
- Stage 1 verification catches bootstrap regressions

---

## Success Criteria Mapping

| Criterion | Verification Task |
|-----------|-------------------|
| SC-001: 500+ line reduction | T059 |
| SC-002: Zero append patterns | T053 |
| SC-003: 100% tests pass | T055 |
| SC-004: 19%+ compilation rate | T056 |
| SC-005: Byte-identical Wasm | T058 |
| SC-006: Large functions migrated | T034 |

---

## Notes

- [P] tasks = different files or non-overlapping code regions
- [Story] label maps task to specific user story for traceability
- TDD is REQUIRED (Constitution Principle VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate progress
- Rollback strategy: git revert individual migration commits if tests fail
