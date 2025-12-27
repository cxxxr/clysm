# Tasks: Stage 1 Compiler Generation

**Input**: Design documents from `/specs/039-stage1-compiler-gen/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per constitution (VII. TDD Non-Negotiable)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and package structure

- [x] T001 Create stage1 package definition in src/clysm/stage1/package.lisp
- [x] T002 [P] Add stage1 module to clysm.asd system definition
- [x] T003 [P] Create scripts directory structure for run-stage1-gen.sh and diff-stages.sh

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Define SourceModule struct in src/clysm/stage1/types.lisp
- [x] T005 [P] Define SourceForm struct in src/clysm/stage1/types.lisp
- [x] T006 [P] Define CompilationResult struct in src/clysm/stage1/types.lisp
- [x] T007 Implement read-source-forms function to parse Lisp files in src/clysm/stage1/reader.lisp
- [x] T008 [P] Implement compilable-form-p predicate for form filtering in src/clysm/stage1/reader.lisp
- [x] T009 Create base error condition hierarchy (stage1-error) in src/clysm/stage1/conditions.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Stage 0 Runtime Verification (Priority: P1) 🎯 MVP

**Goal**: Verify Stage 0 Wasm binary executes basic Lisp forms on wasmtime

**Independent Test**: Run Stage 0 on wasmtime with test expressions (arithmetic, defun, conditionals) and verify correct results

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T010 [P] [US1] Unit test for wasmtime runner wrapper in tests/unit/stage1/runner-test.lisp
- [x] T011 [P] [US1] Contract test for Stage 0 Wasm binary loading in tests/contract/stage1-load-test.lisp
- [x] T012 [P] [US1] Integration test for arithmetic expression (+ 1 2) in tests/integration/stage1-arith-test.lisp
- [x] T013 [P] [US1] Integration test for defun/call sequence in tests/integration/stage1-defun-test.lisp
- [x] T014 [P] [US1] Integration test for unsupported form error handling in tests/integration/stage1-error-test.lisp

### Implementation for User Story 1

- [x] T015 [US1] Implement wasmtime-available-p check in src/clysm/stage1/runner.lisp
- [x] T016 [US1] Implement load-stage0 function to load Wasm binary in src/clysm/stage1/runner.lisp
- [x] T017 [US1] Implement run-form function to execute single form via wasmtime in src/clysm/stage1/runner.lisp
- [x] T018 [US1] Implement capture-result function to extract evaluation result in src/clysm/stage1/runner.lisp
- [x] T019 [US1] Implement error-from-wasm function to convert Wasm errors to conditions in src/clysm/stage1/runner.lisp
- [x] T020 [US1] Create verify-stage0.sh script for CLI verification in scripts/verify-stage0.sh
- [x] T021 [US1] Extend host-shim/verify-stage0.js with actual compile invocation support

**Checkpoint**: Stage 0 can execute basic forms on wasmtime

---

## Phase 4: User Story 2 - Source File Reading Protocol (Priority: P1)

**Goal**: Enable Stage 0 to read Lisp source files from filesystem via FFI

**Independent Test**: Have Stage 0 read a test Lisp file and return its contents as string

### Tests for User Story 2

- [x] T022 [P] [US2] Unit test for FFI file reading function in tests/unit/stage1/file-reader-test.lisp
- [x] T023 [P] [US2] Contract test for clysm.fs.read-source FFI in tests/contract/stage1-fs-test.lisp
- [x] T024 [P] [US2] Integration test for reading all 41 modules in tests/integration/stage1-modules-test.lisp

### Implementation for User Story 2

- [x] T025 [US2] Implement read-source FFI callback in host-shim/stage1-host.js
- [x] T026 [US2] Implement list-modules FFI callback in host-shim/stage1-host.js
- [x] T027 [US2] Implement get-module-paths function in src/clysm/stage1/reader.lisp
- [x] T028 [US2] Implement read-all-modules function in src/clysm/stage1/reader.lisp
- [x] T029 [US2] Add file-not-found error handling with condition in src/clysm/stage1/conditions.lisp
- [x] T030 [US2] Add encoding-error handling for non-UTF-8 files in src/clysm/stage1/conditions.lisp

**Checkpoint**: Stage 0 can read and parse all 41 compiler source modules

---

## Phase 5: User Story 3 - Compilation Progress Measurement (Priority: P2)

**Goal**: Measure and track compilation progress with per-module and overall statistics

**Independent Test**: Run Stage 0 compilation on source files and view progress statistics (forms attempted, compiled, failed, coverage %)

### Tests for User Story 3

- [x] T031 [P] [US3] Unit test for ModuleStats struct in tests/unit/stage1/progress-test.lisp
- [x] T032 [P] [US3] Unit test for Summary and FailureGroup aggregation in tests/unit/stage1/progress-test.lisp
- [x] T033 [P] [US3] Contract test for progress report JSON format in tests/contract/stage1-report-test.lisp
- [x] T034 [P] [US3] Integration test for progress report generation timing (<5s) in tests/integration/stage1-timing-test.lisp

### Implementation for User Story 3

- [x] T035 [US3] Define ModuleStats struct in src/clysm/stage1/progress.lisp
- [x] T036 [US3] Define FailureGroup struct in src/clysm/stage1/progress.lisp
- [x] T037 [US3] Define Summary struct in src/clysm/stage1/progress.lisp
- [x] T038 [US3] Define ProgressReport struct in src/clysm/stage1/progress.lisp
- [x] T039 [US3] Implement record-form-result function in src/clysm/stage1/progress.lisp
- [x] T040 [US3] Implement aggregate-module-stats function in src/clysm/stage1/progress.lisp
- [x] T041 [US3] Implement compute-coverage function in src/clysm/stage1/progress.lisp
- [x] T042 [US3] Implement group-failures-by-operator function in src/clysm/stage1/progress.lisp
- [x] T043 [US3] Implement generate-progress-report function in src/clysm/stage1/progress.lisp
- [x] T044 [US3] Implement write-report-json function for JSON output in src/clysm/stage1/progress.lisp
- [x] T045 [US3] Add report-start/report-form-result/report-module-complete FFI callbacks in host-shim/stage1-host.js

**Checkpoint**: Compilation progress is tracked with detailed statistics

---

## Phase 6: User Story 4 - Stage 1 Binary Generation (Priority: P2)

**Goal**: Generate Stage 1 Wasm binary from compiled Clysm source

**Independent Test**: Run Stage 0 self-compilation and validate output binary with wasm-tools

### Tests for User Story 4

- [x] T046 [P] [US4] Unit test for binary accumulator in tests/unit/stage1/generator-test.lisp
- [x] T047 [P] [US4] Contract test for Stage 1 Wasm validation in tests/contract/stage1-validate-test.lisp
- [x] T048 [P] [US4] Integration test for partial Stage 1 generation in tests/integration/stage1-gen-test.lisp

### Implementation for User Story 4

- [x] T049 [US4] Implement compile-all-forms function in src/clysm/stage1/generator.lisp
- [x] T050 [US4] Implement accumulate-wasm-bytes function in src/clysm/stage1/generator.lisp
- [x] T051 [US4] Implement write-stage1-binary function in src/clysm/stage1/generator.lisp
- [x] T052 [US4] Implement validate-stage1 function using wasm-tools in src/clysm/stage1/generator.lisp
- [x] T053 [US4] Add write-bytes FFI callback in host-shim/stage1-host.js
- [x] T054 [US4] Create run-stage1-gen.sh script for CLI generation in scripts/run-stage1-gen.sh
- [x] T055 [US4] Create build/stage1-gen.lisp entry point script

**Checkpoint**: Stage 1 binary is generated and validates with wasm-tools

---

## Phase 7: User Story 5 - Blocker Analysis and Reporting (Priority: P2)

**Goal**: Identify and document blockers preventing full self-compilation

**Independent Test**: Run analysis on failed forms and generate blocker report with categorized list

### Tests for User Story 5

- [x] T056 [P] [US5] Unit test for blocker categorization in tests/unit/stage1/blocker-test.lisp
- [x] T057 [P] [US5] Unit test for impact calculation in tests/unit/stage1/blocker-test.lisp
- [x] T058 [P] [US5] Contract test for blocker report JSON format in tests/contract/stage1-blocker-test.lisp

### Implementation for User Story 5

- [x] T059 [US5] Define BlockerInfo struct in src/clysm/stage1/blocker.lisp
- [x] T060 [US5] Implement categorize-failures function in src/clysm/stage1/blocker.lisp
- [x] T061 [US5] Implement calculate-impact function in src/clysm/stage1/blocker.lisp
- [x] T062 [US5] Implement rank-blockers function in src/clysm/stage1/blocker.lisp
- [x] T063 [US5] Implement generate-blocker-report function in src/clysm/stage1/blocker.lisp
- [x] T064 [US5] Implement generate-future-work-plan function in src/clysm/stage1/blocker.lisp
- [x] T065 [US5] Implement write-blocker-report-json function in src/clysm/stage1/blocker.lisp

**Checkpoint**: Blockers are identified with impact and priority recommendations

---

## Phase 8: User Story 6 - Diff Analysis Between Stages (Priority: P3)

**Goal**: Compare Stage 0 and Stage 1 binaries to understand differences

**Independent Test**: Compare two Wasm binaries and view diff report with size, exports, and type differences

### Tests for User Story 6

- [x] T066 [P] [US6] Unit test for binary metadata extraction in tests/unit/stage1/diff-test.lisp
- [x] T067 [P] [US6] Unit test for diff computation in tests/unit/stage1/diff-test.lisp
- [x] T068 [P] [US6] Contract test for diff report JSON format in tests/contract/stage1-diff-test.lisp

### Implementation for User Story 6

- [x] T069 [US6] Define BinaryInfo struct in src/clysm/stage1/diff.lisp
- [x] T070 [US6] Define DiffDetails struct in src/clysm/stage1/diff.lisp
- [x] T071 [US6] Define DiffReport struct in src/clysm/stage1/diff.lisp
- [x] T072 [US6] Implement extract-binary-info function using wasm-tools in src/clysm/stage1/diff.lisp
- [x] T073 [US6] Implement compute-diff function in src/clysm/stage1/diff.lisp
- [x] T074 [US6] Implement compare-exports function in src/clysm/stage1/diff.lisp
- [x] T075 [US6] Implement compare-types function in src/clysm/stage1/diff.lisp
- [x] T076 [US6] Implement generate-diff-report function in src/clysm/stage1/diff.lisp
- [x] T077 [US6] Implement write-diff-report-json function in src/clysm/stage1/diff.lisp
- [x] T078 [US6] Create diff-stages.sh script for CLI comparison in scripts/diff-stages.sh

**Checkpoint**: Diff analysis correctly identifies export and type section differences

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [x] T079 [P] Add comprehensive error messages for all stage1-error subtypes in src/clysm/stage1/conditions.lisp
- [x] T080 [P] Add logging throughout stage1 module for debugging in src/clysm/stage1/logging.lisp
- [x] T081 Update CLAUDE.md with Feature 039 completion summary
- [x] T082 [P] Run quickstart.md validation to verify all workflows
- [x] T083 Final integration test: full Stage 1 generation with 25% coverage target in tests/integration/stage1-full-test.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 and can proceed in parallel after Foundational
  - US3, US4, US5 are P2 and depend on US1+US2 completion for meaningful execution
  - US6 is P3 and depends on US4 (needs Stage 1 binary to compare)
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

```text
        ┌──────────────────┐
        │   Foundational   │
        │    (Phase 2)     │
        └────────┬─────────┘
                 │
    ┌────────────┴────────────┐
    │                         │
┌───▼───┐                 ┌───▼───┐
│  US1  │                 │  US2  │
│(P1)   │                 │(P1)   │
└───┬───┘                 └───┬───┘
    │                         │
    └────────────┬────────────┘
                 │
    ┌────────────┼────────────┐
    │            │            │
┌───▼───┐   ┌───▼───┐   ┌───▼───┐
│  US3  │   │  US4  │   │  US5  │
│(P2)   │   │(P2)   │   │(P2)   │
└───────┘   └───┬───┘   └───────┘
                │
            ┌───▼───┐
            │  US6  │
            │(P3)   │
            └───────┘
```

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Structs/types before functions
- Core implementation before FFI callbacks
- Story complete before moving to next priority

### Parallel Opportunities

- T002, T003 can run in parallel (different files)
- T004, T005, T006 can run in parallel (same file but independent structs)
- All tests within a story marked [P] can run in parallel
- US1 and US2 can be worked on in parallel after Foundational
- US3, US4, US5 can be worked on in parallel after US1+US2

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Unit test for wasmtime runner wrapper in tests/unit/stage1/runner-test.lisp"
Task: "Contract test for Stage 0 Wasm binary loading in tests/contract/stage1-load-test.lisp"
Task: "Integration test for arithmetic expression in tests/integration/stage1-arith-test.lisp"
Task: "Integration test for defun/call sequence in tests/integration/stage1-defun-test.lisp"
Task: "Integration test for unsupported form error handling in tests/integration/stage1-error-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 + 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Stage 0 Runtime Verification)
4. Complete Phase 4: User Story 2 (Source File Reading)
5. **STOP and VALIDATE**: Test US1+US2 independently
6. Commit/demo if ready - Stage 0 runs and reads files!

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 + US2 → Test independently → Commit (MVP: Stage 0 works!)
3. Add US3 → Test independently → Commit (Progress tracking)
4. Add US4 → Test independently → Commit (Stage 1 binary generated!)
5. Add US5 → Test independently → Commit (Blockers identified)
6. Add US6 → Test independently → Commit (Diff analysis)
7. Polish → Final commit

### Success Criteria Mapping

| Success Criterion | Verified By |
|-------------------|-------------|
| SC-001: Basic Lisp expressions | US1 integration tests |
| SC-002: Read all 41 modules | US2 tests (T024) |
| SC-003: 25% coverage | US3 + US4 (T083) |
| SC-004: Valid Stage 1 | US4 tests (T047) |
| SC-005: Top 5 blockers | US5 tests (T058) |
| SC-006: Reports <5s | US3 tests (T034) |
| SC-007: Failure context | US3 implementation |
| SC-008: Diff analysis | US6 tests (T068) |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Verify tests fail before implementing (TDD per constitution)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Total: 83 tasks (9 setup/foundational, 74 user story tasks)
