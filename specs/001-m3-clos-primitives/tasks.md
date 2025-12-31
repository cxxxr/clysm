# Tasks: Phase 13D M3 - CLOS Primitives for Wasm

**Input**: Design documents from `/specs/001-m3-clos-primitives/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: TDD required per Constitution VII - tests written before implementation.

**Organization**: Tasks grouped by user story. Note: US3 (CLOS primitives) is foundational and enables US1/US2, so it appears in Phase 2.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/compiler/codegen/` - codegen modifications
- **Tests**: `tests/unit/`, `tests/contract/`, `tests/integration/`
- **Build**: `build/` - Stage 1 generation scripts

---

## Phase 1: Setup

**Purpose**: Verify prerequisites and add missing opcodes

- [ ] T001 Verify array opcodes exist in `src/clysm/compiler/codegen/func-section.lisp` `*wasm-opcodes*`
- [ ] T002 [P] Add `(:array.get . (#xFB #x0B))` to `*wasm-opcodes*` if missing in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T003 [P] Add `(:array.set . (#xFB #x0E))` to `*wasm-opcodes*` if missing in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T004 [P] Add `(:array.new_default . (#xFB #x1B))` to `*wasm-opcodes*` if missing in `src/clysm/compiler/codegen/func-section.lisp`

---

## Phase 2: Foundational - CLOS Primitives (US3)

**Purpose**: Core CLOS primitive codegen that MUST be complete before DEFSTRUCT/DEFINE-CONDITION can work

**Goal**: Compile individual CLOS operations to efficient Wasm opcodes

**Independent Test**: `(slot-value* instance 'x)` compiles to `struct.get` + `array.get` sequence

**⚠️ CRITICAL**: No DEFSTRUCT/DEFINE-CONDITION work can begin until this phase is complete

### Tests for CLOS Primitives (US3)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T005 [P] [US3] Create contract test for slot-value* codegen in `tests/contract/clos-primitives-wasm-test.lisp`
- [ ] T006 [P] [US3] Create contract test for (setf slot-value*) codegen in `tests/contract/clos-primitives-wasm-test.lisp`
- [ ] T007 [P] [US3] Create contract test for make-instance* codegen in `tests/contract/clos-primitives-wasm-test.lisp`
- [ ] T008 [P] [US3] Create contract test for standard-instance-p codegen in `tests/contract/clos-primitives-wasm-test.lisp`
- [ ] T009 [US3] Run tests - verify all 4 tests FAIL (primitives not yet implemented)

### Implementation for CLOS Primitives (US3)

- [ ] T010 [US3] Add `compile-slot-value-read` handler in `src/clysm/compiler/codegen/func-section.lisp` (FR-001)
- [ ] T011 [US3] Add `compile-slot-value-write` handler in `src/clysm/compiler/codegen/func-section.lisp` (FR-002)
- [ ] T012 [US3] Add `compile-make-instance` handler in `src/clysm/compiler/codegen/func-section.lisp` (FR-003)
- [ ] T013 [US3] Add `compile-standard-instance-p` handler in `src/clysm/compiler/codegen/func-section.lisp` (FR-004)
- [ ] T014 [US3] Add slot index resolution helper `resolve-slot-index` in `src/clysm/compiler/codegen/func-section.lisp` (FR-005)
- [ ] T015 [US3] Register primitive handlers in `compile-call` case statement in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T016 [US3] Run tests - verify all 4 contract tests PASS
- [ ] T017 [US3] Run `wasm-tools validate` on test output - verify exit code 0 (FR-008)

**Checkpoint**: CLOS primitives compile to valid Wasm - US1/US2 can now proceed

---

## Phase 3: User Story 1 - DEFSTRUCT Compilation (Priority: P1) 🎯 MVP

**Goal**: DEFSTRUCT forms compile successfully to valid Wasm

**Independent Test**: `(defstruct point x y)` compiles and passes `wasm-tools validate`

### Tests for User Story 1

- [ ] T018 [P] [US1] Create unit test for simple DEFSTRUCT in `tests/unit/defstruct-compile-test.lisp`
- [ ] T019 [P] [US1] Create unit test for DEFSTRUCT with :conc-name in `tests/unit/defstruct-compile-test.lisp`
- [ ] T020 [P] [US1] Create unit test for DEFSTRUCT with :include in `tests/unit/defstruct-compile-test.lisp`
- [ ] T021 [P] [US1] Create unit test for DEFSTRUCT with :read-only slots in `tests/unit/defstruct-compile-test.lisp`
- [ ] T022 [P] [US1] Create integration test for DEFSTRUCT end-to-end in `tests/integration/defstruct-e2e-test.lisp`
- [ ] T023 [US1] Run tests - verify all DEFSTRUCT tests FAIL

### Implementation for User Story 1

- [ ] T024 [US1] Verify DEFSTRUCT macro expansion produces slot-value*/make-instance* calls (should work - no changes needed)
- [ ] T025 [US1] Test `(defstruct point x y)` compilation - verify it now compiles (depends on Phase 2)
- [ ] T026 [US1] Test DEFSTRUCT with :conc-name option compiles correctly
- [ ] T027 [US1] Test DEFSTRUCT with :include option compiles with inherited slot access
- [ ] T028 [US1] Test DEFSTRUCT with :read-only slots - verify no setter generated
- [ ] T029 [US1] Run all DEFSTRUCT tests - verify PASS
- [ ] T030 [US1] Run `wasm-tools validate` on DEFSTRUCT output - verify exit code 0

### Verification for User Story 1

- [ ] T031 [US1] Generate Stage 1 with `sbcl --load build/stage1-complete.lisp`
- [ ] T032 [US1] Check `dist/stage1-report.json` - verify DEFSTRUCT failures < 100 (SC-002)
- [ ] T033 [US1] Run `wasm-tools validate dist/clysm-stage1.wasm` - verify exit code 0 (SC-005)

**Checkpoint**: DEFSTRUCT compiles - largest blocker (1953 failures) resolved

---

## Phase 4: User Story 2 - DEFINE-CONDITION Compilation (Priority: P2)

**Goal**: DEFINE-CONDITION forms compile successfully to valid Wasm

**Independent Test**: Simple condition definition compiles with slot access working

### Tests for User Story 2

- [ ] T034 [P] [US2] Create unit test for simple DEFINE-CONDITION in `tests/unit/condition-compile-test.lisp`
- [ ] T035 [P] [US2] Create unit test for DEFINE-CONDITION with parent in `tests/unit/condition-compile-test.lisp`
- [ ] T036 [P] [US2] Create unit test for DEFINE-CONDITION with :report in `tests/unit/condition-compile-test.lisp`
- [ ] T037 [US2] Run tests - verify all DEFINE-CONDITION tests FAIL

### Implementation for User Story 2

- [ ] T038 [US2] Verify DEFINE-CONDITION macro expansion produces slot-value*/make-instance* calls
- [ ] T039 [US2] Test simple DEFINE-CONDITION compiles (should work with Phase 2 primitives)
- [ ] T040 [US2] Test DEFINE-CONDITION with parent condition compiles with inherited slots
- [ ] T041 [US2] Test DEFINE-CONDITION with :report function compiles
- [ ] T042 [US2] Run all DEFINE-CONDITION tests - verify PASS

### Verification for User Story 2

- [ ] T043 [US2] Re-generate Stage 1 with `sbcl --load build/stage1-complete.lisp`
- [ ] T044 [US2] Check `dist/stage1-report.json` - verify DEFINE-CONDITION failures < 50 (SC-003)

**Checkpoint**: DEFINE-CONDITION compiles - condition system available for Stage 1

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: Final verification and cleanup

- [ ] T045 Run full test suite `sbcl --eval "(asdf:test-system :clysm)"` - verify no regressions (SC-006)
- [ ] T046 Check `dist/stage1-report.json` - verify compilation rate >= 25% (SC-001)
- [ ] T047 [P] Add edge case tests for nil instance handling in `tests/unit/clos-edge-test.lisp`
- [ ] T048 [P] Add edge case tests for invalid slot name in `tests/unit/clos-edge-test.lisp`
- [ ] T049 Update existing `tests/contract/clos-wasm-test.lisp` - remove placeholder tests, add real ones
- [ ] T050 Run quickstart.md verification checklist

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (US3 - CLOS Primitives) ← BLOCKS all user stories
    ↓
┌───┴───┐
↓       ↓
Phase 3: US1 (DEFSTRUCT)    Phase 4: US2 (DEFINE-CONDITION)
    ↓       ↓
    └───┬───┘
        ↓
Phase 5: Polish
```

### User Story Dependencies

- **US3 (CLOS Primitives)**: Foundational - MUST complete first, enables US1 and US2
- **US1 (DEFSTRUCT)**: Depends on US3 completion - Can start after Phase 2
- **US2 (DEFINE-CONDITION)**: Depends on US3 completion - Can start after Phase 2, parallel with US1

### Within Each Phase

- Tests MUST be written and FAIL before implementation
- Contract tests before unit tests
- Handlers before integration in `compile-call`
- Verify tests PASS after implementation
- Validate Wasm output after each major change

### Parallel Opportunities

**Phase 1** (Setup):
- T002, T003, T004 can run in parallel (different opcode additions)

**Phase 2** (US3 - Tests):
- T005, T006, T007, T008 can run in parallel (different test cases)

**Phase 2** (US3 - Implementation):
- T010, T011, T012, T013 can run in parallel initially (different handlers)
- T015 depends on T010-T013 completion

**Phase 3** (US1 - Tests):
- T018, T019, T020, T021, T022 can run in parallel (different test files)

**Phase 4** (US2 - Tests):
- T034, T035, T036 can run in parallel (different test cases)

**Between Phases 3 & 4**:
- US1 and US2 can be worked in parallel after Phase 2 completes

---

## Parallel Example: Phase 2 (CLOS Primitives)

```bash
# Launch all US3 contract tests together:
Task: "T005 [P] [US3] Create contract test for slot-value* codegen"
Task: "T006 [P] [US3] Create contract test for (setf slot-value*) codegen"
Task: "T007 [P] [US3] Create contract test for make-instance* codegen"
Task: "T008 [P] [US3] Create contract test for standard-instance-p codegen"

# After tests written, launch handlers in parallel:
Task: "T010 [US3] Add compile-slot-value-read handler"
Task: "T011 [US3] Add compile-slot-value-write handler"
Task: "T012 [US3] Add compile-make-instance handler"
Task: "T013 [US3] Add compile-standard-instance-p handler"
```

---

## Implementation Strategy

### MVP First (US3 + US1 Only)

1. Complete Phase 1: Setup (opcodes)
2. Complete Phase 2: US3 (CLOS primitives) - CRITICAL foundation
3. Complete Phase 3: US1 (DEFSTRUCT)
4. **STOP and VALIDATE**: Check compilation rate, DEFSTRUCT failures
5. If >= 25% rate achieved, MVP complete

### Incremental Delivery

1. Setup + US3 primitives → Foundation ready
2. Add US1 (DEFSTRUCT) → 1953 failures resolved → Major milestone
3. Add US2 (DEFINE-CONDITION) → 302 failures resolved → Full feature
4. Polish → Edge cases, documentation

### Success Metrics

| Metric | Before | Target | Verification |
|--------|--------|--------|--------------|
| Compilation Rate | 14.26% | >= 25% | `dist/stage1-report.json` |
| DEFSTRUCT Failures | 1953 | < 100 | `dist/stage1-report.json` |
| DEFINE-CONDITION Failures | 302 | < 50 | `dist/stage1-report.json` |
| Wasm Validation | N/A | Exit 0 | `wasm-tools validate` |
| Test Regressions | 0 | 0 | `asdf:test-system` |

---

## Notes

- [P] tasks = different files, no dependencies on incomplete parallel tasks
- [Story] label maps task to specific user story for traceability
- US3 appears in Phase 2 (Foundational) because it enables US1/US2
- TDD required: Write tests first, verify they fail, then implement
- Each phase ends with validation checkpoint
- HyperSpec references: [slot-value](resources/HyperSpec/Body/f_slt_va.htm), [make-instance](resources/HyperSpec/Body/f_mk_ins.htm), [defstruct](resources/HyperSpec/Body/m_defstr.htm)
