# Tasks: ANSI Test Execution

**Input**: Design documents from `/specs/021-ansi-test-execution/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/ansi-test/`, `tests/unit/ansi-test/` at repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify existing infrastructure and project structure

- [x] T001 Verify 020-ansi-test infrastructure is loaded and functional
- [x] T002 Run existing tests to confirm baseline behavior in tests/unit/ansi-test/
- [x] T003 [P] Document current pass/skip/fail counts for numbers category (baseline)
- [x] T004 [P] Document current pass/skip/fail counts for cons category (baseline)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core improvements that enable all user stories to pass

**⚠️ CRITICAL**: All user stories depend on these improvements

### Tests for Foundational Phase

- [x] T005 [P] Add unit test for detecting `loop` as unsupported form in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T006 [P] Add unit test for detecting `let` as unsupported form in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T007 [P] Add unit test for detecting ANSI test macros (`eqt`, `equalt`) in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T008 [P] Add unit test for detecting ANSI test globals (`*universe*`) in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T009 [P] Add unit test for expected-value classification (symbol→unverifiable) in tests/unit/ansi-test/classifier-test.lisp
- [x] T010 [P] Add unit test for expected-value classification (cons→unverifiable) in tests/unit/ansi-test/classifier-test.lisp
- [x] T011 [P] Add unit test for T/NIL comparison with wasmtime output in tests/unit/ansi-test/classifier-test.lisp

### Implementation for Foundational Phase

- [x] T012 Add control flow forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (loop, for, while, do, do*, dotimes, dolist, let, let*, flet, labels)
- [x] T013 Add macro forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (macrolet, symbol-macrolet)
- [x] T014 Add MV forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (multiple-value-bind, multiple-value-list, values)
- [x] T015 Add side-effect forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (incf, decf, push, pop, setf, setq)
- [x] T016 Add error handling forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (handler-case, handler-bind, ignore-errors)
- [x] T017 Add ANSI test framework forms to *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp (signals-error, eqt, equalt, notnot, expand-in-current-env, def-fold-test, *universe*, *numbers*, *symbols*, *conses*)
- [x] T018 Add `expected-value-verifiable-p` function to classify expected values in src/clysm/ansi-test/classifier.lisp
- [x] T019 Update `compare-values` to handle T/NIL correctly with wasmtime output in src/clysm/ansi-test/classifier.lisp
- [x] T020 Update `classify-result` to check expected-value-verifiable-p before comparison in src/clysm/ansi-test/classifier.lisp

**Checkpoint**: Foundation ready - improved skip detection and result classification in place

---

## Blocking Issue: FFI Imports Required for All Modules

**Status**: BLOCKED - All Clysm-compiled Wasm modules include `clysm:io` FFI imports unconditionally

**Discovery**: 2025-12-25 during Phase 3 implementation

**Root Cause**: The Clysm compiler generates Wasm modules that always import `clysm:io::write-char`, `clysm:io::write-string`, `clysm:io::read-char`, `clysm:io::read-line` even for expressions that don't use I/O.

**Impact**:
- All test execution fails at wasmtime instantiation (unknown import error)
- Node.js runner with host-shim has type signature mismatches
- Pass rate is 0% for all categories due to compile-error skips

**Resolution Options** (out of scope for 021):
1. Fix compiler to only emit used imports (compiler change)
2. Update host-shim to match actual import signatures (FFI fix)
3. Provide wasmtime-compatible stub module for unused imports

**Completed Work**:
- [x] Foundational tests T005-T011 pass
- [x] Skip registry expanded with ~60 additional unsupported forms
- [x] `expected-value-verifiable-p` function implemented
- [x] `classify-result` checks verifiability before comparison
- [x] Node.js added to flake.nix for future shim support
- [x] Runner modified to attempt Node.js with host-shim

**Recommendation**: Create follow-up feature 022-ffi-import-optimization to address this

---

## Phase 3-7: DEFERRED (Blocked by FFI Imports)

The following phases are deferred pending resolution of the FFI import issue.

## Phase 3: User Story 1 - Execute Basic Arithmetic Tests (Priority: P1) 🎯 MVP

**Status**: BLOCKED

**Goal**: Tests like `(+ 1 2) = 3` show PASS status when running numbers category

**Independent Test**: Run `(run-ansi-tests :category "numbers")` and verify at least 10% pass rate

### Tests for User Story 1

- [ ] T021 [P] [US1] Add integration test verifying `(+ 1 2)` test passes in tests/integration/ansi-test/execution-test.lisp
- [ ] T022 [P] [US1] Add integration test verifying `(- 5 3)` test passes in tests/integration/ansi-test/execution-test.lisp
- [ ] T023 [P] [US1] Add integration test verifying `(* 2 3)` test passes in tests/integration/ansi-test/execution-test.lisp
- [ ] T024 [US1] Add integration test verifying numbers category achieves ≥10% pass rate in tests/integration/ansi-test/execution-test.lisp

### Implementation for User Story 1

- [ ] T025 [US1] Verify arithmetic tests compile correctly with Clysm compiler (diagnostic task)
- [ ] T026 [US1] Fix any issues with fixnum result comparison in src/clysm/ansi-test/classifier.lisp
- [ ] T027 [US1] Ensure tests returning fixnums are correctly marked PASS in src/clysm/ansi-test/classifier.lisp
- [ ] T028 [US1] Run full numbers category and verify pass rate meets SC-001 (≥10%)

**Checkpoint**: User Story 1 complete - arithmetic tests passing with measurable rate

---

## Phase 4: User Story 2 - Execute Predicate Tests (Priority: P1)

**Goal**: Tests like `(null nil) = T` and `(atom 1) = T` show PASS status

**Independent Test**: Run predicate tests and verify basic predicates pass

### Tests for User Story 2

- [ ] T029 [P] [US2] Add integration test verifying `(null nil)` returns T in tests/integration/ansi-test/execution-test.lisp
- [ ] T030 [P] [US2] Add integration test verifying `(atom 1)` returns T in tests/integration/ansi-test/execution-test.lisp
- [ ] T031 [P] [US2] Add integration test verifying `(consp (cons 1 2))` returns T in tests/integration/ansi-test/execution-test.lisp

### Implementation for User Story 2

- [ ] T032 [US2] Verify T return value is correctly detected from wasmtime output in src/clysm/ansi-test/classifier.lisp
- [ ] T033 [US2] Verify NIL return value is correctly detected (sentinel -2147483648) in src/clysm/ansi-test/classifier.lisp
- [ ] T034 [US2] Ensure boolean-returning tests are correctly classified PASS/FAIL in src/clysm/ansi-test/classifier.lisp

**Checkpoint**: User Story 2 complete - predicate tests passing

---

## Phase 5: User Story 3 - Execute Cons/List Operation Tests (Priority: P1)

**Goal**: Cons category achieves ≥5% pass rate with accurate skip classification

**Independent Test**: Run `(run-ansi-tests :category "cons")` and verify at least 5% pass rate

### Tests for User Story 3

- [ ] T035 [P] [US3] Add integration test verifying cons category achieves ≥5% pass rate in tests/integration/ansi-test/execution-test.lisp
- [ ] T036 [P] [US3] Add unit test verifying cons-result tests are marked unverifiable in tests/unit/ansi-test/classifier-test.lisp

### Implementation for User Story 3

- [ ] T037 [US3] Ensure tests expecting fixnum from cons operations (like car/cdr) pass in src/clysm/ansi-test/classifier.lisp
- [ ] T038 [US3] Ensure tests expecting cons cells are marked SKIP with "unverifiable" in src/clysm/ansi-test/classifier.lisp
- [ ] T039 [US3] Run full cons category and verify pass rate meets SC-002 (≥5%)

**Checkpoint**: User Story 3 complete - cons tests with measurable pass rate

---

## Phase 6: User Story 4 - View Category Pass Rate Summary (Priority: P2)

**Goal**: Summary displays in format "category: PASS/TOTAL (X.X%) passed"

**Independent Test**: Run any category and verify summary output format

### Tests for User Story 4

- [ ] T040 [P] [US4] Add contract test verifying summary format in tests/contract/ansi-test/report-format-test.lisp
- [ ] T041 [P] [US4] Add test verifying 0% category displays correctly in tests/contract/ansi-test/report-format-test.lisp

### Implementation for User Story 4

- [ ] T042 [US4] Verify current summary format in runner.lisp matches "category: PASS/TOTAL (X.X%)" in src/clysm/ansi-test/runner.lisp
- [ ] T043 [US4] Fix any formatting issues with pass rate display in src/clysm/ansi-test/runner.lisp

**Checkpoint**: User Story 4 complete - summary format verified

---

## Phase 7: User Story 5 - Classify Skip Reasons Accurately (Priority: P2)

**Goal**: Each skipped test has actionable reason (compile-error, unsupported-form, unverifiable)

**Independent Test**: Examine skip reasons and verify they follow the taxonomy

### Tests for User Story 5

- [ ] T044 [P] [US5] Add unit test verifying "unsupported-form: FORMAT" reason format in tests/unit/ansi-test/skip-registry-test.lisp
- [ ] T045 [P] [US5] Add unit test verifying "compile-error:" reason format in tests/unit/ansi-test/classifier-test.lisp
- [ ] T046 [P] [US5] Add unit test verifying "unverifiable:" reason format in tests/unit/ansi-test/classifier-test.lisp

### Implementation for User Story 5

- [ ] T047 [US5] Verify skip reason format follows data-model.md taxonomy in src/clysm/ansi-test/skip-registry.lisp
- [ ] T048 [US5] Ensure all skip paths produce categorized reasons (no empty skip-reason) in src/clysm/ansi-test/classifier.lisp
- [ ] T049 [US5] Verify SC-006: All skipped tests have documented reasons

**Checkpoint**: User Story 5 complete - actionable skip reasons

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T050 [P] Verify nix flake check passes with all changes
- [ ] T051 [P] Run quickstart.md validation (execute examples and verify output)
- [ ] T052 [P] Verify single category execution completes within 2 minutes (FR-010/SC-005)
- [ ] T053 Add summary of pass rates to specs/021-ansi-test-execution/ as validation record
- [ ] T054 Code cleanup: Remove any debug output or temporary code
- [ ] T055 Final validation: Run both numbers and cons categories, verify all success criteria

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - verify existing infrastructure
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phases 3-7)**: All depend on Foundational phase completion
  - US1 (arithmetic), US2 (predicates), US3 (cons) can proceed in parallel after Foundational
  - US4 (summary) and US5 (skip reasons) can proceed in parallel with US1-3
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (P1)**: Can start after Foundational - No dependencies on other stories
- **US2 (P1)**: Can start after Foundational - No dependencies on other stories
- **US3 (P1)**: Can start after Foundational - No dependencies on other stories
- **US4 (P2)**: Can start after Foundational - Runs against US1-3 output
- **US5 (P2)**: Can start after Foundational - Validates skip reasons from all tests

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution VII)
- Classifier improvements before runner improvements
- Core implementation before integration testing
- Story complete before moving to next priority

### Parallel Opportunities

- T003, T004: Baseline documentation can run in parallel
- T005-T011: All foundational tests can run in parallel
- T021-T023, T029-T031, T035-T036, T040-T041, T044-T046: Tests within stories can run in parallel
- After Foundational: US1, US2, US3 can be developed in parallel
- T050, T051: Polish tasks can run in parallel

---

## Parallel Example: Foundational Tests

```bash
# Launch all foundational tests in parallel:
Task: "Add unit test for detecting loop as unsupported form"
Task: "Add unit test for detecting let as unsupported form"
Task: "Add unit test for detecting ANSI test macros"
Task: "Add unit test for expected-value classification (symbol→unverifiable)"
Task: "Add unit test for T/NIL comparison with wasmtime output"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (verify baseline)
2. Complete Phase 2: Foundational (skip registry + classifier improvements)
3. Complete Phase 3: User Story 1 (arithmetic tests)
4. **STOP and VALIDATE**: Verify ≥10% numbers pass rate
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test: numbers ≥10% → Checkpoint (MVP!)
3. Add User Story 2 → Test: predicates passing → Checkpoint
4. Add User Story 3 → Test: cons ≥5% → Checkpoint
5. Add User Stories 4+5 → Test: formatting and skip reasons → Complete

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (arithmetic)
   - Developer B: User Story 2 (predicates)
   - Developer C: User Story 3 (cons)
3. Stories complete and validate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (TDD per Constitution VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All file paths are relative to repository root
