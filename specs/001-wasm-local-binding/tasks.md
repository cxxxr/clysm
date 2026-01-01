# Tasks: Wasm Local Instruction Binding

**Input**: Design documents from `/specs/001-wasm-local-binding/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: Required per Constitution Principle VII (TDD) - tests must be written and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Tests in `tests/unit/` for unit tests, `tests/contract/` for contract tests

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify baseline state before any modifications

- [ ] T001 Verify current Stage 1 report exists at dist/stage1-report.json with baseline coverage (19%)
- [ ] T002 Confirm wasm-tools validate command available in environment
- [ ] T003 [P] Run existing test suite to confirm baseline passes: `sbcl --eval "(asdf:test-system :clysm)"`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 Understand existing runtime function table registration pattern in src/clysm/compiler/codegen/func-section.lisp:69-88
- [ ] T005 [P] Identify all backquote expressions containing `:local.set` in func-section.lisp
- [ ] T006 [P] Identify all backquote expressions containing `:local.tee` in func-section.lisp
- [ ] T007 Document current AST-TAGBODY compilation path in func-section.lisp:442-443 and 9611-9615

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Fix Local Variable Instruction Compilation (Priority: P1) 🎯 MVP

**Goal**: Fix LOCAL.SET/LOCAL.TEE unbound variable errors (60 compilation failures → 0)

**Independent Test**: Compile functions using local variable assignments, verify generated Wasm passes validation

### Tests for User Story 1 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T008 [P] [US1] Create unit test for LOCAL.SET opcode emission in tests/unit/local-instruction-test.lisp
- [ ] T009 [P] [US1] Create unit test for LOCAL.TEE opcode emission in tests/unit/local-instruction-test.lisp
- [ ] T010 [US1] Run tests to confirm they FAIL (red phase): LOCAL.SET/LOCAL.TEE not properly handled

### Implementation for User Story 1

- [ ] T011 [US1] Add keyword literal handling for `:local.set` in AST compilation path in src/clysm/compiler/codegen/func-section.lisp
- [ ] T012 [US1] Add keyword literal handling for `:local.tee` in AST compilation path in src/clysm/compiler/codegen/func-section.lisp
- [ ] T013 [US1] Verify backquote expressions preserve keyword quoting during macro expansion
- [ ] T014 [US1] Run tests to confirm they PASS (green phase)
- [ ] T015 [US1] Verify P221 error pattern (LOCAL.SET unbound) eliminated via quick Stage 1 test
- [ ] T016 [US1] Verify P987 error pattern (LOCAL.TEE unbound) eliminated via quick Stage 1 test

**Checkpoint**: User Story 1 complete - LOCAL.SET/LOCAL.TEE compilation works

---

## Phase 4: User Story 2 - Export ADVANCE-TOKEN for Parser Integration (Priority: P2)

**Goal**: Export ADVANCE-TOKEN function, eliminating 22 compilation failures (P027)

**Independent Test**: Verify ADVANCE-TOKEN is exported, registered in runtime table, parser functions compile

### Tests for User Story 2 (TDD Required)

- [ ] T017 [P] [US2] Create unit test for ADVANCE-TOKEN package export in tests/unit/advance-token-export-test.lisp
- [ ] T018 [P] [US2] Create unit test for ADVANCE-TOKEN runtime table registration in tests/unit/advance-token-export-test.lisp
- [ ] T019 [US2] Run tests to confirm they FAIL (red phase)

### Implementation for User Story 2

- [ ] T020 [US2] Add ADVANCE-TOKEN export to clysm package in src/clysm/package.lisp
- [ ] T021 [US2] Register ADVANCE-TOKEN in *runtime-function-table* with arity 1 in src/clysm/compiler/codegen/func-section.lisp
- [ ] T022 [US2] Run tests to confirm they PASS (green phase)
- [ ] T023 [US2] Verify P027 error pattern (ADVANCE-TOKEN undefined) eliminated via Stage 1 test

**Checkpoint**: User Story 2 complete - ADVANCE-TOKEN available for parser integration

---

## Phase 5: User Story 3 - Export EMIT-MODULE-HEADER for Wasm Generation (Priority: P3)

**Goal**: Export EMIT-MODULE-HEADER function, eliminating 10 compilation failures (P143)

**Independent Test**: Verify EMIT-MODULE-HEADER is exported from clysm package and functions compile

### Tests for User Story 3 (TDD Required)

- [ ] T024 [P] [US3] Create unit test for EMIT-MODULE-HEADER package export in tests/unit/emit-header-export-test.lisp
- [ ] T025 [P] [US3] Create unit test for EMIT-MODULE-HEADER runtime table registration in tests/unit/emit-header-export-test.lisp
- [ ] T026 [US3] Run tests to confirm they FAIL (red phase)

### Implementation for User Story 3

- [ ] T027 [US3] Add EMIT-MODULE-HEADER re-export to clysm package in src/clysm/package.lisp
- [ ] T028 [US3] Register EMIT-MODULE-HEADER in *runtime-function-table* with arity 0 in src/clysm/compiler/codegen/func-section.lisp
- [ ] T029 [US3] Run tests to confirm they PASS (green phase)
- [ ] T030 [US3] Verify P143 error pattern (EMIT-MODULE-HEADER undefined) eliminated via Stage 1 test

**Checkpoint**: User Story 3 complete - EMIT-MODULE-HEADER available for Wasm generation

---

## Phase 6: User Story 4 - Handle AST-TAGBODY Structure Serialization (Priority: P3)

**Goal**: Handle AST-TAGBODY structures properly during compilation, eliminating 9 errors (P943)

**Independent Test**: Compile functions with TAGBODY forms, verify no #S(...) structures in error output

### Tests for User Story 4 (TDD Required)

- [ ] T031 [P] [US4] Create unit test for AST-TAGBODY literal handling in tests/unit/ast-tagbody-test.lisp
- [ ] T032 [US4] Run test to confirm it FAILS (red phase)

### Implementation for User Story 4

- [ ] T033 [US4] Add AST-TAGBODY case to literal/structure compilation handler in src/clysm/compiler/codegen/func-section.lisp
- [ ] T034 [US4] Ensure AST-TAGBODY structures are properly transformed or serialized
- [ ] T035 [US4] Run test to confirm it PASSES (green phase)
- [ ] T036 [US4] Verify P943 error pattern (#S(AST-TAGBODY...)) eliminated via Stage 1 test

**Checkpoint**: User Story 4 complete - AST-TAGBODY structures handled properly

---

## Phase 7: User Story 5 - Verify Compilation Rate Improvement (Priority: P1)

**Goal**: Confirm compilation coverage improves from 19% to at least 25%

**Independent Test**: Run full Stage 1 generation and compare coverage_pct field

### Tests for User Story 5

- [ ] T037 [P] [US5] Create contract test for Stage 1 coverage threshold in tests/contract/stage1-coverage-test.lisp
- [ ] T038 [US5] Run coverage test to verify threshold

### Implementation for User Story 5

- [ ] T039 [US5] Run full Stage 1 generation: `sbcl --load build/stage1-complete.lisp`
- [ ] T040 [US5] Extract coverage_pct from dist/stage1-report.json and verify >= 25%
- [ ] T041 [US5] Run `wasm-tools validate dist/clysm-stage1.wasm` and verify exit code 0
- [ ] T042 [US5] Verify all 5 error patterns (P221, P987, P027, P143, P943) have count = 0 in report

**Checkpoint**: All success criteria verified - feature complete

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [ ] T043 [P] Run full test suite to confirm no regressions: `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T044 [P] Update CLAUDE.md with new feature documentation
- [ ] T045 Code review: verify no unnecessary changes, clean diff
- [ ] T046 Run quickstart.md validation commands to confirm documentation accuracy

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 and US5 are P1 priority - implement first
  - US2, US3, US4 are P2/P3 - implement after US1
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Phase 2 - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Phase 2 - Independent of US1
- **User Story 3 (P3)**: Can start after Phase 2 - Independent of US1, US2
- **User Story 4 (P3)**: Can start after Phase 2 - Independent of other stories
- **User Story 5 (P1)**: MUST be done LAST - depends on US1-US4 completion for verification

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Implementation tasks in dependency order
- Verification before checkpoint

### Parallel Opportunities

- T003-T007: Setup and Foundational tasks can run in parallel
- T008-T009: US1 tests can be written in parallel
- T017-T018: US2 tests can be written in parallel
- T024-T025: US3 tests can be written in parallel
- US1, US2, US3, US4 can proceed in parallel once Foundational is complete (if team capacity allows)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Create unit test for LOCAL.SET opcode emission in tests/unit/local-instruction-test.lisp"
Task: "Create unit test for LOCAL.TEE opcode emission in tests/unit/local-instruction-test.lisp"

# Then implementation in sequence:
Task: "Add keyword literal handling for :local.set in src/clysm/compiler/codegen/func-section.lisp"
Task: "Add keyword literal handling for :local.tee in src/clysm/compiler/codegen/func-section.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (LOCAL.SET/LOCAL.TEE fix)
4. **STOP and VALIDATE**: Test US1 independently - 60 errors should be eliminated
5. Continue to US2-US4 or deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → 60 errors fixed (MVP!)
3. Add User Story 2 → Test independently → 22 more errors fixed
4. Add User Story 3 → Test independently → 10 more errors fixed
5. Add User Story 4 → Test independently → 9 more errors fixed
6. User Story 5 → Full verification → Coverage ≥ 25%

### Error Pattern Progress Tracking

| After Story | P221 | P987 | P027 | P143 | P943 | Total Fixed | Coverage Est |
|-------------|------|------|------|------|------|-------------|--------------|
| Baseline    | 40   | 20   | 22   | 10   | 9    | 0           | 19%          |
| US1         | 0    | 0    | 22   | 10   | 9    | 60          | ~22%         |
| US2         | 0    | 0    | 0    | 10   | 9    | 82          | ~23%         |
| US3         | 0    | 0    | 0    | 0    | 9    | 92          | ~24%         |
| US4         | 0    | 0    | 0    | 0    | 0    | 101         | ≥25%         |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD required: Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Use `jq` to query dist/stage1-report.json for error pattern counts
