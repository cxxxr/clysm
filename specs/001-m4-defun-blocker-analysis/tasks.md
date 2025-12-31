# Tasks: Phase 13D M4 - DEFUN Blocker Analysis and Resolution

**Input**: Design documents from `/specs/001-m4-defun-blocker-analysis/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution Principle VII (TDD Non-negotiable)

**Organization**: Tasks grouped by user story for independent implementation and testing

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/`, `build/` at repository root
- Files follow existing Clysm structure per plan.md

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create new files and test infrastructure needed by all user stories

- [ ] T001 Create error analysis package in src/clysm/stage0/error-analysis.lisp
- [ ] T002 [P] Create contract test directory structure at tests/contract/error-analysis/
- [ ] T003 [P] Create unit test directory structure at tests/unit/lambda-list/
- [ ] T004 Register new test systems in clysm.asd

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T005 Define error-log-entry struct in src/clysm/stage0/error-analysis.lisp
- [ ] T006 [P] Define error-pattern-category struct in src/clysm/stage0/error-analysis.lisp
- [ ] T007 [P] Define compilation-report struct in src/clysm/stage0/error-analysis.lisp
- [ ] T008 Export error analysis symbols from package definition in src/clysm/stage0/error-analysis.lisp
- [ ] T009 Add error-analysis dependency to stage1 system in build/stage1-complete.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Detailed DEFUN Error Logging (Priority: P1) 🎯 MVP

**Goal**: Log detailed error information for each DEFUN compilation failure including function name, error type, and failing subform

**Independent Test**: Run Stage 1 compilation and verify each DEFUN failure includes detailed error context

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T010 [P] [US1] Contract test for error-log-entry JSON schema in tests/contract/error-analysis/test-error-log-entry.lisp
- [ ] T011 [P] [US1] Unit test for make-error-log-entry in tests/unit/lambda-list/test-error-logging.lisp
- [ ] T012 [P] [US1] Contract test for defun-errors.json output format in tests/contract/error-analysis/test-defun-errors-json.lisp

### Implementation for User Story 1

- [ ] T013 [US1] Implement make-error-log-entry function in src/clysm/stage0/error-analysis.lisp
- [ ] T014 [US1] Implement extract-failing-subform helper in src/clysm/stage0/error-analysis.lisp
- [ ] T015 [US1] Implement collect-defun-error wrapper for handler-case in src/clysm/stage0/error-analysis.lisp
- [ ] T016 [US1] Implement write-defun-errors-json function in src/clysm/stage0/error-analysis.lisp
- [ ] T017 [US1] Integrate error collection into compile-form loop in build/stage1-complete.lisp
- [ ] T018 [US1] Add defun-errors.json output generation at end of Stage 1 in build/stage1-complete.lisp

**Checkpoint**: DEFUN failures now generate detailed logs in dist/defun-errors.json

---

## Phase 4: User Story 2 - Error Pattern Classification (Priority: P1)

**Goal**: Automatically categorize failures by error pattern for batch analysis

**Independent Test**: Run compilation and verify output report categorizes errors into distinct patterns with counts

### Tests for User Story 2

- [ ] T019 [P] [US2] Contract test for error-pattern-category JSON schema in tests/contract/error-analysis/test-error-pattern.lisp
- [ ] T020 [P] [US2] Unit test for normalize-error-pattern in tests/unit/lambda-list/test-pattern-classification.lisp
- [ ] T021 [P] [US2] Contract test for error_patterns section in stage1-report.json in tests/contract/error-analysis/test-stage1-report.lisp

### Implementation for User Story 2

- [ ] T022 [US2] Implement normalize-error-pattern function in src/clysm/stage0/error-analysis.lisp
- [ ] T023 [US2] Implement classify-error-pattern function in src/clysm/stage0/error-analysis.lisp
- [ ] T024 [US2] Implement aggregate-patterns function to group errors by pattern in src/clysm/stage0/error-analysis.lisp
- [ ] T025 [US2] Implement compute-pattern-priority function (HIGH/MEDIUM/LOW) in src/clysm/stage0/error-analysis.lisp
- [ ] T026 [US2] Add error_patterns section to report generation in build/stage1-complete.lisp
- [ ] T027 [US2] Add defun_failures and defun_failure_reduction to summary in build/stage1-complete.lisp

**Checkpoint**: Stage 1 report now includes error_patterns section with top patterns covering ≥80% of failures

---

## Phase 5: User Story 3 - Complex Lambda-List Support (Priority: P2)

**Goal**: Handle &aux and &allow-other-keys in DEFUN compilation

**Independent Test**: Compile DEFUN forms containing each lambda-list feature and verify successful compilation

### Tests for User Story 3

- [ ] T028 [P] [US3] Unit test for &aux parameter compilation in tests/unit/lambda-list/test-aux-params.lisp
- [ ] T029 [P] [US3] Unit test for &aux with complex init forms in tests/unit/lambda-list/test-aux-init-forms.lisp
- [ ] T030 [P] [US3] Unit test for &allow-other-keys handling in tests/unit/lambda-list/test-allow-other-keys.lisp

### Implementation for User Story 3

- [ ] T031 [US3] Debug &aux local index allocation in src/clysm/compiler/codegen/func-section.lisp:8740-8758
- [ ] T032 [US3] Fix &aux init form compilation for complex expressions in src/clysm/compiler/codegen/func-section.lisp:8902-8916
- [ ] T033 [US3] Add allow-other-keys flag check to keyword argument validation in src/clysm/compiler/codegen/func-section.lisp:8865-8900
- [ ] T034 [US3] Handle nested destructuring parse errors gracefully in src/clysm/compiler/ast.lisp

**Checkpoint**: DEFUNs with &aux and &allow-other-keys compile successfully

---

## Phase 6: User Story 4 - Module Compilation Achievement (Priority: P2)

**Goal**: Achieve 100% compilation for backend/ and reader/ modules

**Independent Test**: Run Stage 1 and verify all forms in backend/ and reader/ compile or are intentionally skipped

### Tests for User Story 4

- [ ] T035 [P] [US4] Integration test for backend/leb128.lisp full compilation in tests/integration/test-backend-compile.lisp
- [ ] T036 [P] [US4] Integration test for reader/tokenizer.lisp full compilation in tests/integration/test-reader-compile.lisp

### Implementation for User Story 4

- [ ] T037 [US4] Analyze top error patterns in backend/ modules using error analysis output
- [ ] T038 [US4] Fix identified blockers for backend/leb128.lisp in relevant compiler files
- [ ] T039 [US4] Fix identified blockers for backend/sections.lisp in relevant compiler files
- [ ] T040 [US4] Fix identified blockers for backend/wasm-emit.lisp in relevant compiler files
- [ ] T041 [US4] Fix identified blockers for backend/wat-print.lisp in relevant compiler files
- [ ] T042 [US4] Analyze top error patterns in reader/ modules using error analysis output
- [ ] T043 [US4] Fix identified blockers for reader/tokenizer.lisp in relevant compiler files
- [ ] T044 [US4] Fix identified blockers for reader/parser.lisp in relevant compiler files
- [ ] T045 [US4] Fix identified blockers for reader/reader.lisp in relevant compiler files
- [ ] T046 [US4] Add skip markers for intentionally unsupported forms in backend/ and reader/

**Checkpoint**: backend/ and reader/ modules reach 100% compilation (compiled + intentionally skipped)

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [ ] T047 [P] Run full Stage 1 generation and verify compilation rate ≥35%
- [ ] T048 [P] Verify DEFUN failures ≤15,000
- [ ] T049 [P] Validate stage1-report.json against JSON schema in specs/001-m4-defun-blocker-analysis/contracts/
- [ ] T050 [P] Validate defun-errors.json against JSON schema in specs/001-m4-defun-blocker-analysis/contracts/
- [ ] T051 Run wasm-tools validate on dist/clysm-stage1.wasm
- [ ] T052 Run quickstart.md verification commands
- [ ] T053 Update CLAUDE.md with feature completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (P1) and US2 (P1) can proceed in parallel
  - US3 (P2) and US4 (P2) can proceed in parallel after US1/US2
  - US4 benefits from US1/US2 completion (uses error analysis output)
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

```text
                    ┌─────────────────┐
                    │  Foundational   │
                    │    (Phase 2)    │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
              ▼              ▼              │
        ┌──────────┐  ┌──────────┐         │
        │   US1    │  │   US2    │         │
        │ Logging  │  │ Patterns │         │
        │   (P1)   │  │   (P1)   │         │
        └────┬─────┘  └────┬─────┘         │
             │             │               │
             └──────┬──────┘               │
                    │ (provides error data)│
                    ▼                      ▼
        ┌──────────────────┐       ┌──────────┐
        │       US4        │       │   US3    │
        │ Module Compile   │       │ Lambda   │
        │      (P2)        │       │   (P2)   │
        └──────────────────┘       └──────────┘
```

- **US1 (Logging)**: Can start immediately after Foundational
- **US2 (Patterns)**: Can start immediately after Foundational (parallel with US1)
- **US3 (Lambda-List)**: Can start after Foundational (independent)
- **US4 (Modules)**: Benefits from US1/US2 completion for error analysis data

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Core infrastructure before integration
- Story complete before moving to next priority

### Parallel Opportunities

**Phase 1 (Setup)**: T002, T003 can run in parallel
**Phase 2 (Foundational)**: T006, T007 can run in parallel
**US1 Tests**: T010, T011, T012 can run in parallel
**US2 Tests**: T019, T020, T021 can run in parallel
**US3 Tests**: T028, T029, T030 can run in parallel
**US4 Tests**: T035, T036 can run in parallel
**Polish**: T047, T048, T049, T050 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for US1 together (TDD - write tests first):
Task: "T010 Contract test for error-log-entry JSON schema"
Task: "T011 Unit test for make-error-log-entry"
Task: "T012 Contract test for defun-errors.json output format"

# After tests fail, implement sequentially:
# T013 → T014 → T015 → T016 → T017 → T018
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Error Logging)
4. Complete Phase 4: User Story 2 (Pattern Classification)
5. **STOP and VALIDATE**: Run Stage 1 and verify error reports work
6. At this point you can analyze errors to guide US3/US4

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 + US2 → Error analysis working → Can run analysis
3. Add US3 → Lambda-list fixed → More DEFUNs compile
4. Add US4 → Target modules 100% → Success criteria met

### Success Criteria Checkpoints

| Checkpoint | Criteria | How to Verify |
|------------|----------|---------------|
| After US1 | dist/defun-errors.json exists with entries | `cat dist/defun-errors.json \| jq '.total_entries'` |
| After US2 | Top 10 patterns cover ≥80% failures | `cat dist/stage1-report.json \| jq '[.error_patterns[:10] \| .[].percentage] \| add'` |
| After US3 | &aux and &allow-other-keys compile | Run unit tests |
| After US4 | backend/ and reader/ at 100% | `cat dist/stage1-report.json \| jq '.modules[] \| select(.path \| contains("backend/") or contains("reader/")) \| select(.failed > 0)'` returns empty |
| Final | Coverage ≥35%, DEFUN ≤15,000 | `cat dist/stage1-report.json \| jq '.summary'` |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Verify tests fail before implementing (TDD per Constitution VII)
- Commit after each task or logical group
- US4 is data-driven: use US1/US2 output to identify specific blockers
