# Tasks: Lexical Environment Function Export System

**Input**: Design documents from `/specs/001-lexenv-function-export/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, quickstart.md ✓

**Tests**: Required per Constitution (VII. TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Scope Summary

Based on research.md, scope is refined:
- **3 functions to export**: `env-add-local`, `loop-keyword-eq`, `numeric-literal-p`
- **Skipped**: `make-lexical-env` (already exported), `env-lookup` (not in error patterns)
- **Expected impact**: 138 errors eliminated, coverage ~22%

---

## Phase 1: Setup (Baseline Capture)

**Purpose**: Capture baseline metrics before any changes

- [X] T001 Capture baseline Stage 1 report by running `sbcl --load build/stage1-complete.lisp` and saving `dist/stage1-report.json` snapshot
- [X] T002 Verify baseline metrics: 21.43% coverage, 118 ENV-ADD-LOCAL errors, 10 LOOP-KEYWORD-EQ errors, 10 NUMERIC-LITERAL-P errors

---

## Phase 2: Foundational (Test Infrastructure)

**Purpose**: Create test framework for exported functions (TDD requirement)

**⚠️ CRITICAL**: Tests must be written and fail before implementation

- [X] T003 Create test file `tests/unit/lexenv-export-test.lisp` with rove test framework structure
- [X] T004 [P] Write test for `clysm:env-add-local` accessibility in `tests/unit/lexenv-export-test.lisp`
- [X] T005 [P] Write test for `clysm:loop-keyword-eq` accessibility in `tests/unit/lexenv-export-test.lisp`
- [X] T006 [P] Write test for `clysm:numeric-literal-p` accessibility in `tests/unit/lexenv-export-test.lisp`
- [X] T007 Run tests and verify all 3 export tests FAIL (functions not yet exported)

**Checkpoint**: Test infrastructure ready, all tests fail as expected

---

## Phase 3: User Story 1 - Export Internal Functions (Priority: P1) 🎯 MVP

**Goal**: Export 3 internal compiler functions to the public clysm package

**Independent Test**: Run `(find-symbol "ENV-ADD-LOCAL" :clysm)` and verify returns `:EXTERNAL`

### Tests for User Story 1 ⚠️

> Tests already written in Phase 2 (T004-T006) - they should now PASS after implementation

### Implementation for User Story 1

- [X] T008 [US1] Add `:import-from #:clysm/compiler/codegen/func-section #:env-add-local` to clysm package definition in `src/clysm/package.lisp`
- [X] T009 [US1] Add `:import-from #:clysm/lib/macros #:loop-keyword-eq` to clysm package definition in `src/clysm/package.lisp`
- [X] T010 [US1] Add `:import-from #:clysm/compiler/ast #:numeric-literal-p` to clysm package definition in `src/clysm/package.lisp`
- [X] T011 [US1] Add `:export #:env-add-local #:loop-keyword-eq #:numeric-literal-p` to clysm package definition in `src/clysm/package.lisp`
- [X] T012 [US1] Verify source packages export these symbols (check `func-section`, `macros`, `ast` package definitions)
- [X] T013 [US1] Run export tests from T004-T006 and verify all PASS

**Checkpoint**: All 3 functions accessible via `clysm:function-name` syntax

---

## Phase 4: User Story 2 - Register in Runtime Table (Priority: P2)

**Goal**: Register exported functions in `*runtime-function-table*` for Wasm dispatch

**Independent Test**: `(runtime-function-p 'clysm:env-add-local)` returns entry with arity

### Tests for User Story 2 ⚠️

- [X] T014 [P] [US2] Write test for `env-add-local` runtime table entry (arity NIL/variadic) in `tests/unit/lexenv-export-test.lisp`
- [X] T015 [P] [US2] Write test for `loop-keyword-eq` runtime table entry (arity 2) in `tests/unit/lexenv-export-test.lisp`
- [X] T016 [P] [US2] Write test for `numeric-literal-p` runtime table entry (arity 1) in `tests/unit/lexenv-export-test.lisp`
- [X] T017 [US2] Run runtime table tests and verify all 3 FAIL

### Implementation for User Story 2

- [X] T018 [US2] Create `register-lexenv-runtime-functions` function in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T019 [US2] Register `env-add-local` with `:$env-add-local-rt` and arity NIL (variadic due to optional param) in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T020 [US2] Register `loop-keyword-eq` with `:$loop-keyword-eq-rt` and arity 2 in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T021 [US2] Register `numeric-literal-p` with `:$numeric-literal-p-rt` and arity 1 in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T022 [US2] Call `register-lexenv-runtime-functions` during module initialization in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T023 [US2] Run runtime table tests from T014-T016 and verify all PASS

**Checkpoint**: All 3 functions have valid entries in `*runtime-function-table*`

---

## Phase 5: User Story 3 - Verify Error Reduction (Priority: P3)

**Goal**: Regenerate Stage 1 and verify error elimination

**Independent Test**: `jq '.error_patterns[] | select(.pattern | contains("ENV-ADD-LOCAL"))' dist/stage1-report.json` returns 0 count

### Tests for User Story 3 ⚠️

- [X] T024 [P] [US3] Verify ENV-ADD-LOCAL undefined function errors = 0 in `dist/stage1-report.json`
- [X] T025 [P] [US3] Verify LOOP-KEYWORD-EQ undefined function errors = 0 in `dist/stage1-report.json`
- [X] T026 [P] [US3] Verify NUMERIC-LITERAL-P undefined function errors = 0 in `dist/stage1-report.json`
- [X] T027 [P] [US3] Verify Stage 1 Wasm validation passes

### Implementation for User Story 3

- [X] T028 [US3] Regenerate Stage 1 by running `sbcl --load build/stage1-complete.lisp`
- [X] T029 [US3] Validate generated Wasm with `wasm-tools validate dist/clysm-stage1.wasm`
- [X] T030 [US3] Extract error counts from `dist/stage1-report.json` for ENV-ADD-LOCAL pattern
- [X] T031 [US3] Extract error counts from `dist/stage1-report.json` for LOOP-KEYWORD-EQ pattern
- [X] T032 [US3] Extract error counts from `dist/stage1-report.json` for NUMERIC-LITERAL-P pattern
- [X] T033 [US3] Verified all target error patterns eliminated
- [X] T034 [US3] Run all unit tests and verify PASS

**Checkpoint**: All targeted error patterns eliminated, Wasm validates

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Documentation and final validation

- [X] T035 [P] Update `CLAUDE.md` Recent Changes section with feature summary
- [X] T036 [P] Run unit tests and verify PASS
- [X] T037 Run quickstart.md validation checklist
- [X] T038 Update tasks.md with completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - baseline capture
- **Foundational (Phase 2)**: Depends on Phase 1 - test infrastructure
- **US1 (Phase 3)**: Depends on Phase 2 - package exports
- **US2 (Phase 4)**: Depends on US1 - runtime table registration
- **US3 (Phase 5)**: Depends on US1 + US2 - verification
- **Polish (Phase 6)**: Depends on US3 - documentation

### User Story Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (Tests)
    ↓
Phase 3: US1 - Export Functions ──┐
    ↓                             │
Phase 4: US2 - Register in Table ←┘
    ↓
Phase 5: US3 - Verify Errors
    ↓
Phase 6: Polish
```

**Note**: US2 depends on US1 (must export before registering). US3 depends on both (verification requires full implementation).

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Implementation follows test specifications
- All tests PASS before checkpoint

### Parallel Opportunities

**Phase 2 (Foundational)**:
- T004, T005, T006 can run in parallel (different test cases)

**Phase 4 (US2)**:
- T014, T015, T016 can run in parallel (different test cases)

**Phase 5 (US3)**:
- T024, T025, T026, T027 can run in parallel (different test cases)

**Phase 6 (Polish)**:
- T035, T036 can run in parallel (different files)

---

## Parallel Example: User Story 2

```bash
# Launch all tests for User Story 2 together:
Task: "Write test for env-add-local runtime table entry in tests/unit/lexenv-export-test.lisp"
Task: "Write test for loop-keyword-eq runtime table entry in tests/unit/lexenv-export-test.lisp"
Task: "Write test for numeric-literal-p runtime table entry in tests/unit/lexenv-export-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (baseline)
2. Complete Phase 2: Foundational (tests)
3. Complete Phase 3: User Story 1 (exports)
4. **STOP and VALIDATE**: Functions accessible via `clysm:function-name`
5. Continue to US2 for runtime dispatch

### Incremental Delivery

1. Setup + Foundational → Tests ready
2. Add US1 → Functions exported → Partial value (symbol access)
3. Add US2 → Runtime table → Full dispatch support
4. Add US3 → Verification → Confirm goals met
5. Each story builds on previous without breaking

### Single Developer Strategy

Execute in strict order: Phase 1 → 2 → 3 → 4 → 5 → 6

TDD cycle per story:
1. Write tests (verify FAIL)
2. Implement
3. Run tests (verify PASS)
4. Proceed to next story

---

## Notes

- All 38 tasks follow TDD pattern per Constitution VII
- Package exports must preserve `:import-from` before `:export` order
- Runtime table registration uses established pattern from `001-io-list-runtime`
- Success criteria: ENV-ADD-LOCAL errors = 0, Wasm validation passes
- Target coverage 25%+ may require additional features beyond this scope
