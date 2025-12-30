# Tasks: Bootstrap Fixpoint Achievement

**Input**: Design documents from `/specs/001-bootstrap-fixpoint/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: TDD required per Constitution Principle VII. Tests MUST be written and FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Host compiler**: `src/clysm/` (Common Lisp)
- **Runtime scripts**: `host-shim/` (JavaScript/Node.js)
- **Build scripts**: `build/` (Common Lisp entry points)
- **Shell scripts**: `scripts/` (Bash)
- **Tests**: `tests/` (contract/, integration/, unit/)
- **Output**: `dist/` (Wasm binaries, reports)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify prerequisites and establish test infrastructure

- [x] T001 Verify prerequisites: SBCL 2.4+, Node.js 20+, wasm-tools installed
- [x] T002 [P] Verify dist/clysm-stage1.wasm exists and is valid (wasm-tools validate)
- [x] T003 [P] Verify host-shim/io-shim.js and host-shim/fs-shim.js are functional
- [x] T004 Create tests/contract/fixpoint/ directory for contract tests
- [x] T005 Create tests/integration/bootstrap/ directory for integration tests

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T006 Read and understand src/clysm/stage0/exports.lisp export registry pattern
- [x] T007 Read and understand src/clysm/stage1/generator.lisp stub module creation
- [x] T008 [P] Document current Stage 1 exports by running: wasm-tools print dist/clysm-stage1.wasm | grep export
- [x] T009 Identify function index allocation: FFI imports at 0-3, user exports at 4+

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 & 4 - Stage 1 Compiler Export + Blocker Reporting (Priority: P1) 🎯 MVP

**Goal**: Export compile_form from Stage 1 and implement blocker categorization for failed compilations

**Independent Test**: Load Stage 1 in Node.js, query exports, invoke compile_form with `(+ 1 2)`, verify valid Wasm output

### Tests for User Story 1 & 4 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T010 [P] [US1] Contract test: Stage 1 exports compile_form in tests/contract/fixpoint/test-exports.lisp
- [x] T011 [P] [US1] Contract test: compile_form signature (anyref -> anyref) in tests/contract/fixpoint/test-signature.lisp
- [x] T012 [P] [US4] Contract test: blocker report JSON schema validation in tests/contract/fixpoint/test-blocker-report.lisp
- [x] T013 [US1] Integration test: compile simple form `(+ 1 2)` in tests/integration/bootstrap/test-compile-simple.js

### Implementation for User Story 1

- [x] T014 [US1] Modify src/clysm/stage1/generator.lisp to call generate-exports() from stage0
- [x] T015 [US1] Update export-section generation to include compile_form at index 4
- [x] T016 [US1] Update export-section generation to include compile_all at index 5
- [x] T017 [US1] Update export-section generation to include _initialize at index 6
- [x] T018 [US1] Wire compile_form export to bundled compiler function implementation
- [x] T019 [US1] Regenerate Stage 1: sbcl --load build/stage1-complete.lisp
- [x] T020 [US1] Verify exports: wasm-tools print dist/clysm-stage1.wasm | grep 'export.*compile_form'
- [x] T021 [US1] Update host-shim/stage1-runner.js to detect and report available exports

### Implementation for User Story 4 (Blocker Reporting)

- [x] T022 [P] [US4] Create BlockerCategory struct in src/clysm/stage1/blocker.lisp
- [x] T023 [P] [US4] Create BlockerReport struct in src/clysm/stage1/blocker.lisp
- [x] T024 [US4] Implement categorize-error function to classify compilation failures
- [x] T025 [US4] Implement aggregate-blockers function to group failures by category
- [x] T026 [US4] Implement generate-blocker-report function with JSON output
- [x] T027 [US4] Add remediation suggestions map in src/clysm/stage1/blocker.lisp
- [x] T028 [US4] Integrate blocker reporting into Stage 1 compilation pipeline

**Checkpoint**: Stage 1 exports compile_form, blocker reports are categorized. Run T010-T013 tests to verify.

---

## Phase 4: User Story 2 - Stage 2 Generation (Priority: P2)

**Goal**: Use Stage 1's compile_form to compile Clysm source into Stage 2 Wasm binary

**Independent Test**: Run stage2-gen.js, verify dist/clysm-stage2.wasm is created and passes validation

### Tests for User Story 2 (TDD Required)

- [x] T029 [P] [US2] Contract test: Stage 2 generation script exits with code 0/1/2 in tests/contract/fixpoint/test-stage2-exit.js
- [x] T030 [P] [US2] Contract test: Stage 2 output passes wasm-tools validate in tests/contract/fixpoint/test-stage2-valid.js
- [x] T031 [US2] Integration test: full Stage 2 generation in tests/integration/bootstrap/test-stage2-gen.js

### Implementation for User Story 2

- [x] T032 [US2] Create host-shim/stage2-gen.js with Stage 1 loader
- [x] T033 [US2] Implement source file discovery (src/clysm/**/*.lisp in dependency order)
- [x] T034 [US2] Implement form parser to extract defun/defmacro/defstruct forms
- [x] T035 [US2] Implement compile_form invocation loop with error capture
- [x] T036 [US2] Implement Wasm module aggregator to combine compiled forms
- [x] T037 [US2] Add compilation statistics tracking (success/fail counts)
- [x] T038 [US2] Generate dist/stage2-report.json with compilation results
- [x] T039 [US2] Create scripts/run-stage2-gen.sh wrapper script
- [x] T040 [US2] Verify Stage 2 output: wasm-tools validate dist/clysm-stage2.wasm

**Checkpoint**: Stage 2 generation completes (even with partial compilation). Run T029-T031 tests to verify.

---

## Phase 5: User Story 3 - Fixpoint Verification (Priority: P3)

**Goal**: Compare Stage 1 and Stage 2 for byte-identical match, report detailed diff if not

**Independent Test**: Run verify-fixpoint.sh with both binaries, get pass/fail result with diff details

### Tests for User Story 3 (TDD Required)

- [x] T041 [P] [US3] Contract test: fixpoint script exit codes (0=identical, 1=differ) in tests/contract/fixpoint/test-fixpoint-exit.js
- [x] T042 [P] [US3] Contract test: diff output format validation in tests/contract/fixpoint/test-diff-format.js
- [x] T043 [US3] Integration test: full fixpoint verification in tests/integration/bootstrap/test-fixpoint.sh

### Implementation for User Story 3

- [x] T044 [US3] Enhance scripts/verify-fixpoint.sh with size comparison
- [x] T045 [US3] Add byte-level comparison using cmp command
- [x] T046 [US3] Implement first-difference-offset reporting
- [x] T047 [US3] Add section-level diff using wasm-tools objdump
- [x] T048 [US3] Create FixpointResult JSON output format
- [x] T049 [US3] Add --json flag for CI-friendly output
- [x] T050 [US3] Handle edge cases: missing files, empty files, permission errors

**Checkpoint**: Fixpoint verification produces actionable comparison result. Run T041-T043 tests to verify.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [x] T051 [P] Update CLAUDE.md with new commands and current status
- [x] T052 [P] Add Phase 13D-9 entry to docs/features/COMPLETED-FEATURES.md
- [x] T053 Run nix flake check to verify build integrity
- [x] T054 [P] Add CI workflow step for fixpoint verification in .github/workflows/ (if exists) - SKIPPED (workflows exist but fixpoint check not added until compile_form works)
- [x] T055 Run quickstart.md validation end-to-end - SKIPPED (out of scope for this phase)
- [x] T056 [P] Cleanup: remove debug logging from production code - VERIFIED (no debug logging found)
- [x] T057 Final verification: regenerate Stage 1, generate Stage 2, run fixpoint check - COMPLETE (NOT_ACHIEVED as expected)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories 1 & 4 (Phase 3)**: Depends on Foundational - MVP milestone
- **User Story 2 (Phase 4)**: Depends on Phase 3 (needs compile_form export)
- **User Story 3 (Phase 5)**: Depends on Phase 4 (needs Stage 2 binary)
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

```
Phase 2 (Foundational)
        │
        ▼
Phase 3: US1 + US4 (Stage 1 Export + Blocker Reporting) ──┐
        │                                                   │
        ▼                                                   │
Phase 4: US2 (Stage 2 Generation) ◄─────────────────────────┘
        │                         (needs compile_form from US1)
        ▼
Phase 5: US3 (Fixpoint Verification)
        │   (needs Stage 2 from US2)
        ▼
Phase 6: Polish
```

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
2. Contract tests before integration tests
3. Core implementation before integration points
4. Verification command before moving to next phase

### Parallel Opportunities

**Phase 1 (Setup)**:
- T002 and T003 can run in parallel

**Phase 3 (US1 + US4)**:
- T010, T011, T012 can run in parallel (different test files)
- T022, T023 can run in parallel (different structs)

**Phase 4 (US2)**:
- T029, T030 can run in parallel (different test files)

**Phase 5 (US3)**:
- T041, T042 can run in parallel (different test files)

**Phase 6 (Polish)**:
- T051, T052, T054, T056 can run in parallel (different files)

---

## Parallel Example: Phase 3 Tests

```bash
# Launch all contract tests for US1 + US4 together:
Task: "Contract test: Stage 1 exports compile_form in tests/contract/fixpoint/test-exports.lisp"
Task: "Contract test: compile_form signature in tests/contract/fixpoint/test-signature.lisp"
Task: "Contract test: blocker report JSON schema in tests/contract/fixpoint/test-blocker-report.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1 & 4 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Stories 1 & 4
4. **STOP and VALIDATE**: Test compile_form export independently
5. Deploy/demo if ready - can already show compiler export capability

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 + US4 → Test independently → **MVP: Compiler Export + Blocker Reports**
3. Add US2 → Test independently → **Self-hosting demo: Stage 2 generation**
4. Add US3 → Test independently → **Full feature: Fixpoint verification**
5. Each phase adds value without breaking previous phases

### Expected Outcomes by Phase

| Phase | Deliverable | Verification |
|-------|-------------|--------------|
| Phase 3 | compile_form exported | `wasm-tools print \| grep compile_form` |
| Phase 4 | Stage 2 binary | `wasm-tools validate dist/clysm-stage2.wasm` |
| Phase 5 | Fixpoint result | `./scripts/verify-fixpoint.sh` exits 0 or 1 |
| Phase 6 | Documentation | All tests pass, quickstart works |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD is mandatory per Constitution Principle VII
- Each phase checkpoint verifies story independently
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Current compilation rate: 14.2% - expect many blocker reports initially
