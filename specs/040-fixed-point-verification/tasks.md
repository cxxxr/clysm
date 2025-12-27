# Tasks: Fixed-Point Verification (Phase 13B)

**Input**: Design documents from `/specs/040-fixed-point-verification/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution Principle VII (TDD required)

**Organization**: Tasks grouped by user story for independent implementation and testing

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and prerequisite verification

- [ ] T001 Verify Stage 1 binary exists at dist/clysm-stage1.wasm
- [ ] T002 Verify wasmtime runtime is available (wasmtime --version)
- [ ] T003 [P] Verify wasm-tools is available (wasm-tools --version)
- [ ] T004 [P] Verify Node.js is available for host shims (node --version)
- [ ] T005 Create tests/unit/fixpoint/ directory structure
- [ ] T006 [P] Create tests/contract/ fixpoint test placeholder
- [ ] T007 [P] Create tests/integration/ fixpoint test placeholder

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures and package definitions that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T008 Extend src/clysm/stage1/types.lisp with verification-result struct
- [ ] T009 [P] Extend src/clysm/stage1/types.lisp with byte-diff-info struct
- [ ] T010 [P] Extend src/clysm/stage1/types.lisp with verification-history-entry struct
- [ ] T011 Extend src/clysm/stage1/conditions.lisp with fixpoint-error condition hierarchy
- [ ] T012 Create src/clysm/stage2/package.lisp with clysm/stage2 package definition
- [ ] T013 Export new symbols from src/clysm/stage1/package.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Stage 2 Generation (Priority: P1) 🎯 MVP

**Goal**: Run Stage 1 Wasm binary on wasmtime to compile Clysm source and produce Stage 2 binary

**Independent Test**: Execute `./scripts/run-stage2-gen.sh` and verify `dist/clysm-stage2.wasm` is created and valid

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T014 [P] [US1] Unit test for wasmtime invocation in tests/unit/fixpoint/runner-test.lisp
- [ ] T015 [P] [US1] Unit test for Stage 2 generation in tests/unit/fixpoint/stage2-gen-test.lisp
- [ ] T016 [P] [US1] Contract test for Stage 2 binary validity in tests/contract/fixpoint-stage2-test.lisp

### Implementation for User Story 1

- [ ] T017 [US1] Implement run-form in src/clysm/stage1/runner.lisp to invoke wasmtime via host shim
- [ ] T018 [US1] Extend host-shim/stage1-host.js with --mode compile support
- [ ] T019 [US1] Implement compileToStage2() function in host-shim/stage1-host.js
- [ ] T020 [US1] Create src/clysm/stage2/generator.lisp with generate-stage2 function
- [ ] T021 [US1] Implement module reading and compilation orchestration in src/clysm/stage2/generator.lisp
- [ ] T022 [US1] Handle partial compilation failure (FR-010) in src/clysm/stage2/generator.lisp
- [ ] T023 [US1] Create build/stage2-gen.lisp CLI entry point
- [ ] T024 [US1] Create scripts/run-stage2-gen.sh shell wrapper

**Checkpoint**: Stage 2 generation works - `./scripts/run-stage2-gen.sh` produces valid Wasm binary

---

## Phase 4: User Story 2 - Fixed-Point Verification (Priority: P1)

**Goal**: Compare Stage 1 and Stage 2 byte-by-byte to verify identical match (fixed-point)

**Independent Test**: Run binary comparison on two files and verify correct identical/different status

### Tests for User Story 2

- [ ] T025 [P] [US2] Unit test for binaries-identical-p in tests/unit/fixpoint/byte-compare-test.lisp
- [ ] T026 [P] [US2] Unit test for verify-fixpoint in tests/unit/fixpoint/verifier-test.lisp
- [ ] T027 [P] [US2] Integration test for full verification in tests/integration/fixpoint-verify-test.lisp

### Implementation for User Story 2

- [ ] T028 [US2] Implement binaries-identical-p in src/clysm/stage1/diff.lisp
- [ ] T029 [US2] Return first-diff-offset from binaries-identical-p in src/clysm/stage1/diff.lisp
- [ ] T030 [US2] Create src/clysm/stage1/fixpoint.lisp with fixpoint-status type
- [ ] T031 [US2] Implement status-to-exit-code mapping in src/clysm/stage1/fixpoint.lisp
- [ ] T032 [US2] Create src/clysm/stage2/verifier.lisp with verify-fixpoint function
- [ ] T033 [US2] Implement dependency checking (wasmtime, Stage 1) in src/clysm/stage2/verifier.lisp
- [ ] T034 [US2] Implement Stage 1 validation (FR-008) using wasm-tools in src/clysm/stage2/verifier.lisp
- [ ] T035 [US2] Integrate Stage 2 generation + comparison in src/clysm/stage2/verifier.lisp
- [ ] T036 [US2] Output human-readable result (ACHIEVED/NOT ACHIEVED) in src/clysm/stage2/verifier.lisp

**Checkpoint**: `(clysm/stage2:verify-fixpoint)` returns correct verification-result struct

---

## Phase 5: User Story 3 - Diff Analysis Report (Priority: P2)

**Goal**: Generate detailed diff report showing byte offsets, section mismatches, export differences

**Independent Test**: Create two different Wasm files and verify report identifies differences correctly

### Tests for User Story 3

- [ ] T037 [P] [US3] Unit test for byte-diff-info generation in tests/unit/fixpoint/byte-diff-test.lisp
- [ ] T038 [P] [US3] Unit test for section-level diff in tests/unit/fixpoint/section-diff-test.lisp
- [ ] T039 [P] [US3] Contract test for diff report format in tests/contract/fixpoint-diff-test.lisp

### Implementation for User Story 3

- [ ] T040 [US3] Extend src/clysm/stage1/diff.lisp with generate-byte-diff-report function
- [ ] T041 [US3] Implement total-diff-bytes calculation in src/clysm/stage1/diff.lisp
- [ ] T042 [US3] Implement size-mismatch detection in src/clysm/stage1/diff.lisp
- [ ] T043 [US3] Extend export comparison in src/clysm/stage1/diff.lisp for added/removed/modified
- [ ] T044 [US3] Extend type comparison in src/clysm/stage1/diff.lisp for signature differences
- [ ] T045 [US3] Extend scripts/diff-stages.sh with --byte-level option

**Checkpoint**: `./scripts/diff-stages.sh --byte-level` shows detailed byte/section diff

---

## Phase 6: User Story 4 - Automated CI Verification (Priority: P2)

**Goal**: CI pipeline integration with exit codes and JSON output

**Independent Test**: Run verification script and verify exit code matches status (0/1/2/3)

### Tests for User Story 4

- [ ] T046 [P] [US4] Unit test for JSON output format in tests/unit/fixpoint/json-output-test.lisp
- [ ] T047 [P] [US4] Integration test for exit codes in tests/integration/fixpoint-exitcode-test.lisp

### Implementation for User Story 4

- [ ] T048 [US4] Implement JSON output (FR-006) in src/clysm/stage2/verifier.lisp
- [ ] T049 [US4] Add output-format parameter (:text/:json) to verify-fixpoint in src/clysm/stage2/verifier.lisp
- [ ] T050 [US4] Implement timing capture (stage2-gen-time-ms, comparison-time-ms) in src/clysm/stage2/verifier.lisp
- [ ] T051 [US4] Create scripts/verify-fixpoint.sh main verification script
- [ ] T052 [US4] Implement --json option in scripts/verify-fixpoint.sh
- [ ] T053 [US4] Implement --skip-generate option in scripts/verify-fixpoint.sh
- [ ] T054 [US4] Implement exit code mapping (FR-007: 0/1/2/3) in scripts/verify-fixpoint.sh
- [ ] T055 [US4] Add --help option with usage information in scripts/verify-fixpoint.sh

**Checkpoint**: `./scripts/verify-fixpoint.sh --json` returns proper JSON and exit code

---

## Phase 7: User Story 5 - Iterative Improvement Tracking (Priority: P3)

**Goal**: Track verification history across iterations to show progress toward fixed-point

**Independent Test**: Run verification multiple times and verify history log is appended

### Tests for User Story 5

- [ ] T056 [P] [US5] Unit test for history entry append in tests/unit/fixpoint/history-test.lisp
- [ ] T057 [P] [US5] Unit test for progress summary in tests/unit/fixpoint/progress-test.lisp

### Implementation for User Story 5

- [ ] T058 [US5] Implement append-to-history in src/clysm/stage1/fixpoint.lisp
- [ ] T059 [US5] Implement JSON Lines format for dist/verification-history.jsonl
- [ ] T060 [US5] Implement read-history-log in src/clysm/stage1/fixpoint.lisp
- [ ] T061 [US5] Implement compute-progress-summary in src/clysm/stage1/fixpoint.lisp
- [ ] T062 [US5] Add --history option to scripts/verify-fixpoint.sh
- [ ] T063 [US5] Implement trend detection (improving/regressing) in src/clysm/stage1/fixpoint.lisp

**Checkpoint**: `./scripts/verify-fixpoint.sh --history` appends to log; history can be queried

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, cleanup, and cross-story improvements

- [ ] T064 [P] Update quickstart.md with actual command outputs
- [ ] T065 [P] Add docstrings to all new functions in src/clysm/stage1/*.lisp
- [ ] T066 [P] Add docstrings to all new functions in src/clysm/stage2/*.lisp
- [ ] T067 Run all unit tests and ensure pass
- [ ] T068 Run all contract tests and ensure pass
- [ ] T069 Run all integration tests and ensure pass
- [ ] T070 Validate scripts work in nix develop environment
- [ ] T071 Update CLAUDE.md with Feature 040 summary

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - 🎯 MVP target
- **User Story 2 (Phase 4)**: Depends on Foundational AND User Story 1 (needs Stage 2 generation)
- **User Story 3 (Phase 5)**: Depends on Foundational - Can run parallel to US1/US2
- **User Story 4 (Phase 6)**: Depends on US1 + US2 (needs full verification workflow)
- **User Story 5 (Phase 7)**: Depends on US4 (needs verification results to track)
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

```
                    ┌───────────────┐
                    │    Setup      │
                    │   (Phase 1)   │
                    └───────┬───────┘
                            │
                            ▼
                    ┌───────────────┐
                    │  Foundational │
                    │   (Phase 2)   │
                    └───────┬───────┘
                            │
            ┌───────────────┼───────────────┐
            │               │               │
            ▼               ▼               ▼
    ┌───────────────┐ ┌───────────┐ ┌───────────┐
    │ US1: Stage 2  │ │ US3: Diff │ │  (other)  │
    │   Generation  │ │  Report   │ │           │
    │   (Phase 3)   │ │ (Phase 5) │ │           │
    └───────┬───────┘ └───────────┘ └───────────┘
            │
            ▼
    ┌───────────────┐
    │ US2: Fixed-   │
    │ Point Verify  │
    │   (Phase 4)   │
    └───────┬───────┘
            │
            ▼
    ┌───────────────┐
    │ US4: CI       │
    │ Integration   │
    │   (Phase 6)   │
    └───────┬───────┘
            │
            ▼
    ┌───────────────┐
    │ US5: History  │
    │   Tracking    │
    │   (Phase 7)   │
    └───────────────┘
```

### Parallel Opportunities

**Within Phase 1 (Setup)**:
- T002, T003, T004 can run in parallel
- T005, T006, T007 can run in parallel

**Within Phase 2 (Foundational)**:
- T008, T009, T010 can run in parallel (same file but different structs)
- T011, T012, T013 depend on T008-T010

**Within User Stories**:
- All [P] marked tests can run in parallel
- Implementation tasks typically sequential within each story

**Across User Stories**:
- US3 (Diff Report) can start in parallel with US1/US2 after Foundational
- US1 and US3 have no interdependency

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Unit test for wasmtime invocation in tests/unit/fixpoint/runner-test.lisp"
Task: "Unit test for Stage 2 generation in tests/unit/fixpoint/stage2-gen-test.lisp"
Task: "Contract test for Stage 2 binary validity in tests/contract/fixpoint-stage2-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 + 2 Only)

1. Complete Phase 1: Setup (verify prerequisites)
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Stage 2 Generation)
4. Complete Phase 4: User Story 2 (Fixed-Point Verification)
5. **STOP and VALIDATE**: Run `(clysm/stage2:verify-fixpoint)` manually
6. Deploy/demo if verification works

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add User Story 1 → Test with `./scripts/run-stage2-gen.sh` → Can produce Stage 2
3. Add User Story 2 → Test manually in REPL → Can verify fixed-point
4. Add User Story 4 → `./scripts/verify-fixpoint.sh` works → CI-ready
5. Add User Story 3 → Detailed diff reports available
6. Add User Story 5 → Progress tracking enabled

### Recommended Execution Order

1. **T001-T013**: Setup + Foundational (must be sequential mostly)
2. **T014-T024**: US1 tests + implementation (Stage 2 gen)
3. **T025-T036**: US2 tests + implementation (verification)
4. **T046-T055**: US4 tests + implementation (CI script)
5. **T037-T045**: US3 tests + implementation (diff report)
6. **T056-T063**: US5 tests + implementation (history)
7. **T064-T071**: Polish

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Constitution requires TDD - write tests first, verify they fail
- FR-007 exit codes: 0 (achieved), 1 (not achieved), 2 (compile error), 3 (missing dep)
- FR-008 requires wasm-tools validation before Stage 1 execution
- FR-010 requires graceful handling of partial compilation failure
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
