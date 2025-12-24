# Tasks: Macro System (Lisp-4)

**Input**: Design documents from `/specs/016-macro-system/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution VII (TDD Non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Macro system extends existing compiler infrastructure

---

## Phase 1: Setup (Verification)

**Purpose**: Verify existing infrastructure and understand current state

- [x] T001 Verify existing macro.lisp structure in src/clysm/compiler/transform/macro.lisp
- [x] T002 [P] Verify existing macros.lisp structure in src/clysm/lib/macros.lisp
- [x] T003 [P] Verify backquote token handling in src/clysm/reader/tokenizer.lisp
- [x] T004 [P] Verify quasiquote parsing in src/clysm/reader/parser.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Add expansion depth limit (1000) to macroexpand* in src/clysm/compiler/transform/macro.lisp
- [x] T006 [P] Add macro-expansion-depth-exceeded condition in src/clysm/compiler/transform/macro.lisp
- [x] T007 [P] Add unit test for expansion depth limit in tests/unit/macro-test.lisp
- [x] T008 Verify depth limit test fails, then implement in macroexpand* in src/clysm/compiler/transform/macro.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Define and Use Custom Macros (Priority: P1) 🎯 MVP

**Goal**: Developers can define macros with defmacro and use them in code

**Independent Test**: `(defmacro my-when (test &body body) \`(if ,test (progn ,@body)))` then `(my-when t 1 2 3)` returns 3

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T009 [P] [US1] Unit test for &key lambda list parsing in tests/unit/macro-test.lisp
- [x] T010 [P] [US1] Unit test for defmacro with &key in tests/unit/macro-test.lisp
- [x] T011 [P] [US1] Integration test for user-defined macro compilation in tests/integration/macro-test.lisp

### Implementation for User Story 1

- [x] T012 [US1] Add &key parsing to parse-lambda-list in src/clysm/compiler/transform/macro.lisp
- [x] T013 [US1] Update compile-defmacro to handle &key bindings in src/clysm/compiler/transform/macro.lisp
- [x] T014 [US1] Add keyword argument validation and error messages in src/clysm/compiler/transform/macro.lisp
- [x] T015 [US1] Verify all US1 tests pass

**Checkpoint**: User Story 1 complete - defmacro with full lambda lists works

---

## Phase 4: User Story 2 - Use Backquote Syntax for Template Code (Priority: P1)

**Goal**: Backquote/unquote/unquote-splicing syntax works correctly in all edge cases

**Independent Test**: `` `(a ,b ,@c) `` where b=1, c=(2 3) produces `(a 1 2 3)`

### Tests for User Story 2 ⚠️

- [x] T016 [P] [US2] Unit test for nested backquote in tests/unit/backquote-test.lisp
- [x] T017 [P] [US2] Unit test for ,@ inside nested backquote in tests/unit/backquote-test.lisp
- [x] T018 [P] [US2] Unit test for backquote with dotted lists in tests/unit/backquote-test.lisp
- [x] T019 [P] [US2] Integration test for macro using complex backquote in tests/integration/macro-test.lisp

### Implementation for User Story 2

- [x] T020 [US2] Review and fix nested backquote handling in expand-bq in src/clysm/compiler/transform/macro.lisp
- [x] T021 [US2] Add dotted list support to backquote expansion in src/clysm/compiler/transform/macro.lisp
- [x] T022 [US2] Verify all US2 tests pass

**Checkpoint**: User Story 2 complete - all backquote patterns work correctly

---

## Phase 5: User Story 3 - Use Standard Control Flow Macros (Priority: P2)

**Goal**: Standard control macros (when, unless, cond, case) work correctly

**Independent Test**: `(case 'b (a 1) (b 2) (c 3))` returns 2

### Tests for User Story 3 ⚠️

- [x] T023 [P] [US3] Unit test for case macro expansion in tests/unit/macro-test.lisp
- [x] T024 [P] [US3] Integration test for case with multiple keys in tests/integration/macro-test.lisp
- [x] T025 [P] [US3] Integration test for case with otherwise/t clause in tests/integration/macro-test.lisp

### Implementation for User Story 3

- [x] T026 [US3] Implement make-case-expander in src/clysm/lib/macros.lisp
- [x] T027 [US3] Register case macro in install-standard-macros in src/clysm/lib/macros.lisp
- [x] T028 [US3] Verify existing when/unless/cond still pass tests
- [x] T029 [US3] Verify all US3 tests pass

**Checkpoint**: User Story 3 complete - all control flow macros work

---

## Phase 6: User Story 4 - Use Iteration Macros (Priority: P2)

**Goal**: Iteration macros (dolist, dotimes, do, prog1, prog2) work correctly

**Independent Test**: `(do ((i 0 (+ i 1))) ((>= i 5) i))` returns 5

### Tests for User Story 4 ⚠️

- [x] T030 [P] [US4] Unit test for do macro expansion in tests/unit/macro-test.lisp
- [x] T031 [P] [US4] Unit test for prog1 macro expansion in tests/unit/macro-test.lisp
- [x] T032 [P] [US4] Unit test for prog2 macro expansion in tests/unit/macro-test.lisp
- [x] T033 [P] [US4] Integration test for do with parallel bindings in tests/integration/macro-test.lisp
- [x] T034 [P] [US4] Integration test for prog1/prog2 return values in tests/integration/macro-test.lisp

### Implementation for User Story 4

- [x] T035 [US4] Implement make-do-expander in src/clysm/lib/macros.lisp
- [x] T036 [US4] Implement make-prog1-expander in src/clysm/lib/macros.lisp
- [x] T037 [US4] Implement make-prog2-expander in src/clysm/lib/macros.lisp
- [x] T038 [US4] Register do, prog1, prog2 in install-standard-macros in src/clysm/lib/macros.lisp
- [x] T039 [US4] Verify existing dolist/dotimes still pass tests
- [x] T040 [US4] Verify all US4 tests pass

**Checkpoint**: User Story 4 complete - all iteration macros work

---

## Phase 7: User Story 5 - Inspect Macro Expansions (Priority: P3)

**Goal**: macroexpand-1 and macroexpand available as compiled functions for debugging

**Independent Test**: `(macroexpand-1 '(when t 1))` returns `(if t (progn 1) nil)`

### Tests for User Story 5 ⚠️

- [x] T041 [P] [US5] Unit test for macroexpand-1 as special form in tests/unit/macro-test.lisp
- [x] T042 [P] [US5] Unit test for macroexpand as special form in tests/unit/macro-test.lisp
- [x] T043 [P] [US5] Integration test for macroexpand-1 at runtime in tests/integration/macro-test.lisp

### Implementation for User Story 5

- [x] T044 [US5] Add ast-macroexpand-1 node in src/clysm/compiler/ast.lisp
- [x] T045 [US5] Add ast-macroexpand node in src/clysm/compiler/ast.lisp
- [x] T046 [US5] Handle macroexpand-1 in parse-expr in src/clysm/compiler/ast.lisp
- [x] T047 [US5] Handle macroexpand in parse-expr in src/clysm/compiler/ast.lisp
- [x] T048 [US5] Implement codegen for macroexpand-1 (runtime call to expansion) in src/clysm/compiler/codegen/
- [x] T049 [US5] Verify all US5 tests pass (host-side implementation complete)

**Checkpoint**: User Story 5 complete - macroexpand-1/macroexpand work at compile-time and runtime

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T050 [P] Run all unit tests in tests/unit/
- [x] T051 [P] Run all integration tests in tests/integration/
- [x] T052 Verify SC-001: All standard macros compile and execute correctly
- [x] T053 Verify SC-002: (when t 1 2 3) evaluates to 3
- [x] T054 Verify SC-003: Nested backquotes at any depth work
- [x] T055 Verify SC-004: macroexpand-1 shows correct expansions
- [x] T056 Verify SC-005: User-defined macros work like built-ins
- [x] T057 Verify SC-006: Clear error messages for macro errors
- [x] T058 Run quickstart.md validation scenarios

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 - can proceed in parallel
  - US3 and US4 are both P2 - can proceed in parallel after US1/US2
  - US5 is P3 - proceed after US3/US4
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 3 (P2)**: Can start after Foundational - Independent but benefits from US1/US2 completion
- **User Story 4 (P2)**: Can start after Foundational - Independent but benefits from US1/US2 completion
- **User Story 5 (P3)**: Can start after Foundational - Needs macros from US3/US4 for meaningful testing

### Within Each User Story

- Tests MUST be written and FAIL before implementation (Constitution VII)
- Infrastructure code before macro expanders
- Unit tests before integration tests
- Verify all story tests pass before checkpoint

### Parallel Opportunities

- T001-T004: All setup tasks can run in parallel
- T006-T007: Foundational tasks marked [P]
- T009-T011: US1 tests can run in parallel
- T016-T019: US2 tests can run in parallel
- T023-T025: US3 tests can run in parallel
- T030-T034: US4 tests can run in parallel
- T041-T043: US5 tests can run in parallel
- US1 and US2 can be worked on in parallel (both P1)
- US3 and US4 can be worked on in parallel (both P2)

---

## Parallel Example: User Story 4

```bash
# Launch all tests for User Story 4 together:
Task: "Unit test for do macro expansion in tests/unit/macro-test.lisp"
Task: "Unit test for prog1 macro expansion in tests/unit/macro-test.lisp"
Task: "Unit test for prog2 macro expansion in tests/unit/macro-test.lisp"
Task: "Integration test for do with parallel bindings in tests/integration/macro-test.lisp"
Task: "Integration test for prog1/prog2 return values in tests/integration/macro-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 + 2 Only)

1. Complete Phase 1: Setup (verification)
2. Complete Phase 2: Foundational (expansion depth limit)
3. Complete Phase 3: User Story 1 (defmacro with &key)
4. Complete Phase 4: User Story 2 (backquote edge cases)
5. **STOP and VALIDATE**: Test defmacro and backquote independently
6. Deploy/demo if ready - macros are usable!

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 + US2 → Test independently → MVP ready!
3. Add US3 → Control flow macros (case) → Demo
4. Add US4 → Iteration macros (do, prog1, prog2) → Demo
5. Add US5 → Macro introspection → Full feature complete

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (defmacro &key)
   - Developer B: User Story 2 (backquote edge cases)
3. After US1/US2 complete:
   - Developer A: User Story 3 (case macro)
   - Developer B: User Story 4 (do, prog1, prog2)
4. Developer A: User Story 5 (macroexpand functions)
5. Team: Polish phase together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (Constitution VII TDD)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Most work extends existing files rather than creating new ones
