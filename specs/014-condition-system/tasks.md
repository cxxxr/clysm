# Tasks: ANSI Common Lisp Condition System

**Input**: Design documents from `/specs/014-condition-system/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDD required per constitution (Section VII). Tests written before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/` at repository root
- **Tests**: `tests/` at repository root
- **New module**: `src/clysm/conditions/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and condition system module structure

- [x] T001 Create conditions module directory at src/clysm/conditions/
- [x] T002 Create package definition for clysm/conditions in src/clysm/conditions/package.lisp
- [x] T003 [P] Add conditions module to ASDF system definition in clysm.asd
- [x] T004 [P] Create runtime support file at src/clysm/runtime/condition-runtime.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational Phase

- [x] T005 [P] Test condition class hierarchy in tests/unit/condition-types-test.lisp
- [x] T006 [P] Test handler stack operations in tests/unit/handler-test.lisp
- [x] T007 [P] Test restart stack operations in tests/unit/restart-test.lisp

### Implementation for Foundational Phase

- [x] T008 Define base condition CLOS class in src/clysm/conditions/types.lisp
- [x] T009 Define serious-condition class in src/clysm/conditions/types.lisp
- [x] T010 Define error class (subclass of serious-condition) in src/clysm/conditions/types.lisp
- [x] T011 [P] Define warning class in src/clysm/conditions/types.lisp
- [x] T012 [P] Define simple-condition mixin with format-control/format-arguments in src/clysm/conditions/types.lisp
- [x] T013 Define simple-error class (simple-condition + error) in src/clysm/conditions/types.lisp
- [x] T014 [P] Define simple-warning class (simple-condition + warning) in src/clysm/conditions/types.lisp
- [x] T015 Define type-error with datum and expected-type slots in src/clysm/conditions/types.lisp
- [x] T016 [P] Define cell-error with name slot in src/clysm/conditions/types.lisp
- [x] T017 [P] Define unbound-variable (subclass of cell-error) in src/clysm/conditions/types.lisp
- [x] T018 [P] Define undefined-function (subclass of cell-error) in src/clysm/conditions/types.lisp
- [x] T019 [P] Define control-error class in src/clysm/conditions/types.lisp
- [x] T020 Implement handler struct and handler-cluster struct in src/clysm/runtime/condition-runtime.lisp
- [x] T021 Implement restart struct and restart-cluster struct in src/clysm/runtime/condition-runtime.lisp
- [x] T022 Implement *handler-clusters* dynamic variable in src/clysm/runtime/condition-runtime.lisp
- [x] T023 Implement *restart-clusters* dynamic variable in src/clysm/runtime/condition-runtime.lisp
- [x] T024 Implement handler stack push/pop with unwind-protect in src/clysm/runtime/condition-runtime.lisp
- [x] T025 Implement restart stack push/pop with unwind-protect in src/clysm/runtime/condition-runtime.lisp
- [x] T026 Implement find-handler function for condition type matching in src/clysm/runtime/condition-runtime.lisp
- [x] T027 Implement signal-internal function for handler dispatch loop in src/clysm/conditions/signaling.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Error Signaling and Handling (Priority: P1) 🎯 MVP

**Goal**: Developers can catch and handle errors using handler-case with type-based dispatch

**Independent Test**: Signal an error with `error` function, catch with `handler-case`, verify handler executes and returns value

### Tests for User Story 1

- [x] T028 [P] [US1] Test handler-case catches type-error in tests/integration/condition-test.lisp
- [x] T029 [P] [US1] Test nested handler-case finds innermost handler in tests/integration/condition-test.lisp
- [x] T030 [P] [US1] Test handler-case returns body value when no error in tests/integration/condition-test.lisp

### Implementation for User Story 1

- [x] T031 [US1] Implement error function for signaling errors in src/clysm/conditions/signaling.lisp
- [x] T032 [US1] Implement make-condition function in src/clysm/conditions/types.lisp
- [x] T033 [US1] Implement handler-case macro expansion in src/clysm/conditions/handlers.lisp
- [N/A] T034 [US1] Add AST node for handler-case (macro-based impl)
- [N/A] T035 [US1] Implement handler-case codegen (macro-based impl)
- [x] T036 [US1] Handle unhandled errors (trap with message) in src/clysm/conditions/signaling.lisp

**Checkpoint**: User Story 1 complete - handler-case works for catching typed errors

---

## Phase 4: User Story 2 - Restart-Based Recovery (Priority: P1)

**Goal**: Developers can establish restarts with restart-case and invoke them from handlers

**Independent Test**: Establish restart with `restart-case`, signal error, invoke restart from handler, verify recovery value

### Tests for User Story 2

- [x] T037 [P] [US2] Test restart-case establishes invocable restart in tests/integration/condition-test.lisp
- [x] T038 [P] [US2] Test compute-restarts returns all visible restarts in tests/unit/restart-test.lisp
- [x] T039 [P] [US2] Test find-restart locates restart by name in tests/unit/restart-test.lisp
- [x] T040 [P] [US2] Test invoke-restart transfers control via catch/throw in tests/integration/condition-test.lisp

### Implementation for User Story 2

- [x] T041 [US2] Implement find-restart function in src/clysm/conditions/restarts.lisp
- [x] T042 [US2] Implement compute-restarts function in src/clysm/conditions/restarts.lisp
- [x] T043 [US2] Implement invoke-restart function (uses throw) in src/clysm/conditions/restarts.lisp
- [x] T044 [US2] Implement restart-case macro expansion in src/clysm/conditions/restarts.lisp
- [N/A] T045 [US2] Add AST node for restart-case (macro-based impl)
- [N/A] T046 [US2] Implement restart-case codegen (macro-based impl)
- [x] T047 [US2] Implement invoke-restart-interactively in src/clysm/conditions/restarts.lisp

**Checkpoint**: User Story 2 complete - restart-case and invoke-restart work

---

## Phase 5: User Story 3 - Warning and Non-Fatal Conditions (Priority: P2)

**Goal**: Developers can signal warnings that output messages but continue execution

**Independent Test**: Call `warn`, verify warning outputs to *error-output*, then use muffle-warning to suppress

### Tests for User Story 3

- [x] T048 [P] [US3] Test warn outputs message and continues in tests/integration/condition-test.lisp
- [x] T049 [P] [US3] Test muffle-warning suppresses warning output in tests/integration/condition-test.lisp
- [x] T050 [P] [US3] Test simple-warning format arguments in tests/unit/condition-types-test.lisp

### Implementation for User Story 3

- [x] T051 [US3] Implement warn function in src/clysm/conditions/signaling.lisp
- [x] T052 [US3] Implement muffle-warning restart in src/clysm/conditions/standard.lisp
- [x] T053 [US3] Add muffle-warning restart establishment in warn in src/clysm/conditions/signaling.lisp
- [x] T054 [US3] Implement default warning output to *error-output* in src/clysm/conditions/signaling.lisp

**Checkpoint**: User Story 3 complete - warnings work with muffle-warning

---

## Phase 6: User Story 4 - Continuable Errors with cerror (Priority: P2)

**Goal**: Developers can signal continuable errors that allow continue restart

**Independent Test**: Call `cerror`, invoke continue restart, verify execution continues

### Tests for User Story 4

- [x] T055 [P] [US4] Test cerror establishes continue restart in tests/integration/condition-test.lisp
- [x] T056 [P] [US4] Test continue restart allows execution to proceed in tests/integration/condition-test.lisp

### Implementation for User Story 4

- [x] T057 [US4] Implement cerror function in src/clysm/conditions/signaling.lisp
- [x] T058 [US4] Implement continue restart in src/clysm/conditions/standard.lisp
- [x] T059 [US4] Add continue restart establishment in cerror in src/clysm/conditions/signaling.lisp

**Checkpoint**: User Story 4 complete - cerror with continue restart works

---

## Phase 7: User Story 5 - Handler-Bind for Non-Transferring Handlers (Priority: P2)

**Goal**: Developers can establish handlers that inspect conditions without automatic transfer

**Independent Test**: Establish handler-bind that logs and returns normally, verify condition propagates to outer handler

### Tests for User Story 5

- [x] T060 [P] [US5] Test handler-bind handler that declines (returns normally) in tests/integration/condition-test.lisp
- [x] T061 [P] [US5] Test handler-bind handler that invokes restart in tests/integration/condition-test.lisp
- [x] T062 [P] [US5] Test multiple handler-bind handlers tried innermost first in tests/integration/condition-test.lisp

### Implementation for User Story 5

- [x] T063 [US5] Implement handler-bind macro expansion in src/clysm/conditions/handlers.lisp
- [N/A] T064 [US5] Add AST node for handler-bind in src/clysm/compiler/ast.lisp (macro-based impl)
- [N/A] T065 [US5] Implement handler-bind codegen (let + unwind-protect) (macro-based impl)
- [x] T066 [US5] Update signal-internal to handle declining handlers in src/clysm/conditions/signaling.lisp

**Checkpoint**: User Story 5 complete - handler-bind with declining handlers works

---

## Phase 8: User Story 6 - Standard Restarts (Priority: P3)

**Goal**: Standard restarts (abort, use-value, store-value) are available

**Independent Test**: Establish each standard restart, find and invoke it, verify expected behavior

### Tests for User Story 6

- [x] T067 [P] [US6] Test abort restart performs non-local exit in tests/integration/condition-test.lisp
- [x] T068 [P] [US6] Test use-value restart substitutes value in tests/integration/condition-test.lisp
- [x] T069 [P] [US6] Test store-value restart stores value in tests/integration/condition-test.lisp

### Implementation for User Story 6

- [x] T070 [US6] Implement abort function and restart in src/clysm/conditions/standard.lisp
- [x] T071 [US6] Implement use-value function and restart in src/clysm/conditions/standard.lisp
- [x] T072 [US6] Implement store-value function and restart in src/clysm/conditions/standard.lisp

**Checkpoint**: User Story 6 complete - all standard restarts available

---

## Phase 9: User Story 7 - With-Simple-Restart Convenience Macro (Priority: P3)

**Goal**: Developers can use with-simple-restart for common restart patterns

**Independent Test**: Wrap code in with-simple-restart, invoke restart, verify returns (values nil t)

### Tests for User Story 7

- [x] T073 [P] [US7] Test with-simple-restart normal completion returns (values result nil) in tests/integration/condition-test.lisp
- [x] T074 [P] [US7] Test with-simple-restart invoked returns (values nil t) in tests/integration/condition-test.lisp

### Implementation for User Story 7

- [x] T075 [US7] Implement with-simple-restart macro in src/clysm/conditions/restarts.lisp
- [x] T076 [US7] Add with-simple-restart to package exports in src/clysm/conditions/package.lisp

**Checkpoint**: User Story 7 complete - with-simple-restart macro works

---

## Phase 10: Polish & Cross-Cutting Concerns

**Purpose**: Edge cases, integration validation, and cleanup

- [x] T077 [P] Test handler unbinding during handler execution (prevent infinite recursion) in tests/integration/condition-test.lisp
- [x] T078 [P] Test invoke-restart with non-visible restart signals control-error in tests/integration/condition-test.lisp
- [x] T079 [P] Test unwind-protect cleanup during restart invocation in tests/integration/condition-test.lisp
- [x] T080 Implement restart-bind macro in src/clysm/conditions/restarts.lisp
- [x] T081 Implement signal function in src/clysm/conditions/signaling.lisp
- [x] T082 Add all condition system exports to package in src/clysm/conditions/package.lisp
- [x] T083 Run nix flake check to validate all tests pass
- [x] T084 Run quickstart.md examples to validate documentation

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 priority - can run in parallel
  - US3, US4, US5 are P2 priority - can run in parallel after US1/US2
  - US6, US7 are P3 priority - can run in parallel
- **Polish (Phase 10)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies on other stories - MVP
- **User Story 2 (P1)**: No dependencies on other stories - can parallel with US1
- **User Story 3 (P2)**: Uses US2's muffle-warning restart
- **User Story 4 (P2)**: Uses US2's restart infrastructure
- **User Story 5 (P2)**: Uses US1's handler infrastructure
- **User Story 6 (P3)**: Uses US2's restart infrastructure
- **User Story 7 (P3)**: Uses US2's restart-case

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Foundation types → Runtime support → Macros → Codegen
- Core implementation before integration tests

### Parallel Opportunities

**Phase 2 (Foundational)**:
```text
Parallel: T005, T006, T007 (tests)
Parallel: T011, T012, T014, T016, T017, T018, T019 (condition types)
Sequential: T020 → T021 → T022-T027 (runtime)
```

**Phase 3-9 (User Stories)**:
```text
US1 and US2 can run in parallel (both P1)
US3, US4, US5 can run in parallel (all P2, after US1/US2)
US6, US7 can run in parallel (both P3)
```

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "T028 [P] [US1] Test handler-case catches type-error"
Task: "T029 [P] [US1] Test nested handler-case finds innermost handler"
Task: "T030 [P] [US1] Test handler-case returns body value when no error"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (handler-case + error)
4. **STOP and VALIDATE**: Test handler-case independently
5. Deploy/demo if ready - basic error handling works!

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add User Story 1 → MVP: Basic error handling
3. Add User Story 2 → Restarts: Recovery patterns
4. Add User Story 3-5 → Warnings, cerror, handler-bind
5. Add User Story 6-7 → Standard restarts, convenience macros
6. Polish → Edge cases and integration

### Parallel Team Strategy

With multiple developers after Foundational:
- Developer A: User Story 1 (handler-case)
- Developer B: User Story 2 (restart-case)
- Then US3/4/5 and US6/7 in parallel waves

---

## Summary

| Metric | Count |
|--------|-------|
| Total Tasks | 84 |
| Phase 1 (Setup) | 4 |
| Phase 2 (Foundational) | 23 |
| User Story 1 | 9 |
| User Story 2 | 11 |
| User Story 3 | 7 |
| User Story 4 | 5 |
| User Story 5 | 7 |
| User Story 6 | 6 |
| User Story 7 | 4 |
| Phase 10 (Polish) | 8 |
| Parallel Tasks [P] | 42 |

**MVP Scope**: User Story 1 (Phase 3) = 9 tasks after foundational

**Independent Test Criteria**:
- US1: `(handler-case (error 'type-error ...) (type-error (c) :caught))` → :caught
- US2: `(restart-case (error ...) (use-value (v) v))` with `(invoke-restart 'use-value 42)` → 42
- US3: `(warn "msg")` outputs and continues; `(handler-bind ((warning #'muffle-warning)) (warn "msg"))` suppresses
- US4: `(cerror "Continue" "Error")` with `(invoke-restart 'continue)` continues
- US5: `(handler-bind ((error (lambda (c) nil))) (handler-case (error ...) (error () :outer)))` → :outer
- US6: `(restart-case ... (abort () (throw 'top nil)))` with `(invoke-restart 'abort)` exits
- US7: `(with-simple-restart (skip "Skip") 42)` → (values 42 nil); invoking skip → (values nil t)
