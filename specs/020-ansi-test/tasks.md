# Tasks: ANSI Common Lisp Test Suite Integration

**Input**: Design documents from `/specs/020-ansi-test/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/cli.md

**TDD Required**: Per Constitution Principle VII, all components MUST have tests first (Red-Green-Refactor cycle).

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1-US5) this task belongs to
- File paths relative to repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization, git submodule, ASDF system definition

- [x] T001 Add pfdietz/ansi-test as git submodule at ansi-test/
- [x] T002 Update flake.nix with ansi-test input (pinned commit)
- [x] T003 [P] Create baselines/ directory with .gitkeep
- [x] T004 [P] Create src/clysm/ansi-test/ directory structure
- [x] T005 Define clysm/ansi-test package in src/clysm/ansi-test/package.lisp
- [x] T006 Add clysm/ansi-test system definition to clysm.asd

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures and utilities that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Test Infrastructure (MUST be first for TDD)

- [x] T007 Create test package for ansi-test harness in tests/unit/ansi-test/package.lisp

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T008 [P] Unit test for test-case struct in tests/unit/ansi-test/data-model-test.lisp
- [x] T009 [P] Unit test for test-result struct in tests/unit/ansi-test/data-model-test.lisp
- [x] T010 [P] Unit test for category-result struct in tests/unit/ansi-test/data-model-test.lisp
- [x] T011 [P] Unit test for skip-registry struct in tests/unit/ansi-test/data-model-test.lisp

### Implementation

- [x] T012 Implement test-case defstruct in src/clysm/ansi-test/data-model.lisp
- [x] T013 Implement test-result defstruct in src/clysm/ansi-test/data-model.lisp
- [x] T014 Implement category-result defstruct with pass-rate accessor in src/clysm/ansi-test/data-model.lisp
- [x] T015 Implement report-summary and coverage-report defstructs in src/clysm/ansi-test/data-model.lisp
- [x] T016 Implement skip-registry defstruct with *default-skip-registry* in src/clysm/ansi-test/skip-registry.lisp
- [x] T017 Verify all T008-T011 tests pass (Green phase)

**Checkpoint**: Data structures ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Run Full Test Suite and View Results (Priority: P1) 🎯 MVP

**Goal**: Developer runs full ANSI test suite and sees pass/fail/skip counts per category with overall compliance percentage

**Independent Test**: Run `(run-ansi-tests)` and verify output shows category results and overall pass rate

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T018 [P] [US1] Unit test for parse-deftest in tests/unit/ansi-test/loader-test.lisp
- [x] T019 [P] [US1] Unit test for load-category-tests in tests/unit/ansi-test/loader-test.lisp
- [x] T020 [P] [US1] Unit test for execute-single-test in tests/unit/ansi-test/runner-test.lisp
- [x] T021 [P] [US1] Unit test for classify-result (PASS/FAIL/SKIP) in tests/unit/ansi-test/classifier-test.lisp
- [x] T022 [US1] Integration test for run-ansi-tests with mock category in tests/integration/ansi-test/runner-test.lisp

### Implementation

- [x] T023 [US1] Implement parse-deftest function in src/clysm/ansi-test/loader.lisp
- [x] T024 [US1] Implement load-file-tests to parse .lsp files in src/clysm/ansi-test/loader.lisp
- [x] T025 [US1] Implement load-category-tests to load all tests from category dir in src/clysm/ansi-test/loader.lisp
- [x] T026 [US1] Implement list-categories to discover all category directories in src/clysm/ansi-test/loader.lisp
- [x] T027 [US1] Implement classify-result for PASS/FAIL/SKIP in src/clysm/ansi-test/classifier.lisp
- [x] T028 [US1] Implement compare-values for expected vs actual in src/clysm/ansi-test/classifier.lisp
- [x] T029 [US1] Implement execute-single-test using existing compile-and-run in src/clysm/ansi-test/runner.lisp
- [x] T030 [US1] Implement run-category to execute all tests in a category in src/clysm/ansi-test/runner.lisp
- [x] T031 [US1] Implement run-ansi-tests main entry point in src/clysm/ansi-test/runner.lisp
- [x] T032 [US1] Implement progress display (category completion, pass rate) in src/clysm/ansi-test/runner.lisp
- [x] T033 [US1] Verify all T018-T022 tests pass (Green phase)

**Checkpoint**: User Story 1 complete - can run full test suite and see results

---

## Phase 4: User Story 2 - Run Tests for Specific Category (Priority: P1)

**Goal**: Developer specifies category name and only those tests execute

**Independent Test**: Run `(run-ansi-tests :category "cons")` and verify only cons tests execute

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T034 [P] [US2] Unit test for category filtering in tests/unit/ansi-test/runner-test.lisp
- [x] T035 [P] [US2] Unit test for category-not-found-error in tests/unit/ansi-test/runner-test.lisp

### Implementation

- [x] T036 [US2] Add :category keyword parameter to run-ansi-tests in src/clysm/ansi-test/runner.lisp
- [x] T037 [US2] Implement category validation with helpful error in src/clysm/ansi-test/runner.lisp
- [x] T038 [US2] Define category-not-found-error condition in src/clysm/ansi-test/conditions.lisp
- [x] T039 [US2] Verify T034-T035 tests pass (Green phase)

**Checkpoint**: User Story 2 complete - can run single category tests

---

## Phase 5: User Story 3 - Generate Coverage Report (Priority: P2)

**Goal**: Developer generates Markdown report with category-by-category results and summary table

**Independent Test**: Run tests, call `(generate-report report :output-path "report.md")`, verify Markdown is valid

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T040 [P] [US3] Contract test for Markdown format in tests/contract/ansi-test/report-format-test.lisp
- [x] T041 [P] [US3] Unit test for format-category-results in tests/unit/ansi-test/reporter-test.lisp
- [x] T042 [P] [US3] Unit test for format-summary-table in tests/unit/ansi-test/reporter-test.lisp

### Implementation

- [x] T043 [US3] Implement format-summary-table for overall stats in src/clysm/ansi-test/reporter.lisp
- [x] T044 [US3] Implement format-category-results with pass rate in src/clysm/ansi-test/reporter.lisp
- [x] T045 [US3] Implement format-skip-reasons-summary in src/clysm/ansi-test/reporter.lisp
- [x] T046 [US3] Implement generate-report main function in src/clysm/ansi-test/reporter.lisp
- [x] T047 [US3] Add :report-path parameter to run-ansi-tests in src/clysm/ansi-test/runner.lisp
- [x] T048 [US3] Verify T040-T042 tests pass (Green phase)

**Checkpoint**: User Story 3 complete - can generate Markdown compliance reports

---

## Phase 6: User Story 4 - Automatic Skip of Unsupported Features (Priority: P2)

**Goal**: Tests using unsupported forms (FORMAT, DEFGENERIC, etc.) are auto-detected and skipped with reason

**Independent Test**: Run test containing FORMAT, verify it shows SKIP with reason "unsupported-form: FORMAT"

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T049 [P] [US4] Unit test for contains-unsupported-form-p in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T050 [P] [US4] Unit test for detect-skip-reason in tests/unit/ansi-test/skip-registry-test.lisp
- [x] T051 [P] [US4] Unit test for skip reason tracking in tests/unit/ansi-test/classifier-test.lisp

### Implementation

- [x] T052 [US4] Implement contains-unsupported-form-p with AST walk in src/clysm/ansi-test/skip-registry.lisp
- [x] T053 [US4] Implement detect-skip-reason for form-level detection in src/clysm/ansi-test/skip-registry.lisp
- [x] T054 [US4] Implement category-level skip detection in src/clysm/ansi-test/skip-registry.lisp
- [x] T055 [US4] Integrate skip detection into classify-result in src/clysm/ansi-test/classifier.lisp
- [x] T056 [US4] Add skip reason aggregation to coverage-report in src/clysm/ansi-test/reporter.lisp
- [x] T057 [US4] Verify T049-T051 tests pass (Green phase)

**Checkpoint**: User Story 4 complete - unsupported forms auto-detected and skipped

---

## Phase 7: User Story 5 - CI Integration for Regression Detection (Priority: P3)

**Goal**: CI compares results against baseline and flags regressions (5%+ pass rate drop)

**Independent Test**: Create baseline, simulate regression, verify comparison detects it

### Tests (TDD - Write FIRST, verify FAIL)

- [x] T058 [P] [US5] Unit test for serialize-report-to-json in tests/unit/ansi-test/baseline-test.lisp
- [x] T059 [P] [US5] Unit test for deserialize-baseline-from-json in tests/unit/ansi-test/baseline-test.lisp
- [x] T060 [P] [US5] Unit test for compare-baseline regression detection in tests/unit/ansi-test/baseline-test.lisp
- [x] T061 [US5] Integration test for update-baseline in tests/integration/ansi-test/baseline-test.lisp

### Implementation

- [x] T062 [US5] Implement baseline-comparison defstruct in src/clysm/ansi-test/data-model.lisp
- [x] T063 [US5] Implement serialize-report-to-json in src/clysm/ansi-test/baseline.lisp
- [x] T064 [US5] Implement deserialize-baseline-from-json in src/clysm/ansi-test/baseline.lisp
- [x] T065 [US5] Implement compare-baseline with 5% threshold in src/clysm/ansi-test/baseline.lisp
- [x] T066 [US5] Implement update-baseline function in src/clysm/ansi-test/baseline.lisp
- [x] T067 [US5] Implement format-comparison-output in src/clysm/ansi-test/baseline.lisp
- [x] T068 [US5] Verify T058-T061 tests pass (Green phase)

**Checkpoint**: User Story 5 complete - CI can detect regressions

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: CLI wrapper, documentation, performance optimization

- [x] T069 [P] Create ansi-test.sh shell wrapper for CI in scripts/ansi-test.sh
- [x] T070 [P] Add timeout handling (30s default) to execute-single-test in src/clysm/ansi-test/runner.lisp
- [x] T071 [P] Add :parallel parameter for worker pool to run-ansi-tests in src/clysm/ansi-test/runner.lisp
- [x] T072 [P] Add :timeout parameter to run-ansi-tests in src/clysm/ansi-test/runner.lisp
- [x] T073 Create GitHub Actions workflow for ANSI compliance in .github/workflows/ansi-compliance.yml
- [x] T074 Verify nix flake check passes with new test infrastructure
- [x] T075 Run quickstart.md validation scenarios

**Note**: T076-T079 (pass rate verification) removed from this feature. These depend on Clysm compiler implementation progress, not the test harness infrastructure. Track separately as compiler milestones.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Phase 2 completion
  - US1 & US2 (both P1) can be done sequentially or US1 first
  - US3 & US4 (both P2) can start after US1 is complete
  - US5 (P3) can start after US3 is complete (needs report structure)
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

```text
Phase 2 (Foundational)
    │
    ▼
US1 (Run Full Suite) ──────────────────────────────┐
    │                                              │
    ▼                                              │
US2 (Category Filter) ─ extends US1                │
    │                                              │
    ├──────────────────────────────────────────────┤
    ▼                                              ▼
US3 (Report Generation)                    US4 (Auto Skip)
    │                                              │
    └──────────────────┬───────────────────────────┘
                       ▼
               US5 (Regression Detection)
                       │
                       ▼
               Phase 8 (Polish)
```

### Within Each User Story

1. Tests MUST be written and FAIL first (TDD Red phase)
2. Implementation makes tests pass (Green phase)
3. Story complete before moving to next priority

### Parallel Opportunities

**Phase 1 (parallel)**:
```bash
T003 # Create baselines/ directory
T004 # Create src/clysm/ansi-test/ structure
```

**Phase 2 Tests (parallel)**:
```bash
T008, T009, T010, T011 # All data model tests (after T007 package creation)
```

**US1 Tests (parallel)**:
```bash
T018, T019, T020, T021 # All loader/runner/classifier tests
```

**US3 Tests (parallel)**:
```bash
T040, T041, T042 # Report format tests
```

**US4 Tests (parallel)**:
```bash
T049, T050, T051 # Skip detection tests
```

**US5 Tests (parallel)**:
```bash
T058, T059, T060 # Baseline tests
```

**Phase 8 (parallel)**:
```bash
T069, T070, T071, T072 # CLI, timeout, parallel, timeout params
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2)

1. Complete Phase 1: Setup (git submodule, flake.nix, ASDF)
2. Complete Phase 2: Foundational (data structures)
3. Complete Phase 3: User Story 1 (run full suite)
4. Complete Phase 4: User Story 2 (category filter)
5. **STOP and VALIDATE**: Run `(run-ansi-tests :category "cons")` and verify output
6. Demo/Deploy MVP

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 → Test full suite → **MVP v0.1**
3. Add US2 → Test category filtering → **MVP v0.2**
4. Add US3 + US4 → Reports + Auto-skip → **v0.3**
5. Add US5 → CI regression detection → **v1.0**
6. Polish → Performance + CLI → **v1.0 release**

---

## Summary

| Phase | Tasks | Parallel Opportunities |
|-------|-------|----------------------|
| Phase 1: Setup | 6 | 2 |
| Phase 2: Foundational | 11 | 4 |
| Phase 3: US1 (P1) | 16 | 4 |
| Phase 4: US2 (P1) | 6 | 2 |
| Phase 5: US3 (P2) | 9 | 3 |
| Phase 6: US4 (P2) | 9 | 3 |
| Phase 7: US5 (P3) | 11 | 3 |
| Phase 8: Polish | 11 | 4 |
| **Total** | **79** | **25** |

---

## Notes

- TDD is REQUIRED per Constitution Principle VII
- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story is independently completable and testable
- Verify tests FAIL before implementing (Red phase)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
