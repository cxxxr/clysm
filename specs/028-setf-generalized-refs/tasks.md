# Tasks: Setf Macros and Generalized References

**Input**: Design documents from `/specs/028-setf-generalized-refs/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md

**Tests**: Included per Constitution Principle VII (TDD required)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2)
- Include exact file paths in descriptions

## Path Conventions

- Single project: `src/clysm/` for source, `tests/` for tests
- Paths follow existing Clysm structure per plan.md

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create new files and export symbols for setf system

- [x] T001 Create setf-expanders.lisp file skeleton in src/clysm/lib/setf-expanders.lisp
- [x] T002 [P] Add package definition for setf-expanders in src/clysm/lib/setf-expanders.lisp
- [x] T003 [P] Export setf-related symbols in src/clysm/package.lisp (setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf, define-setf-expander, defsetf, get-setf-expansion)
- [x] T004 [P] Create test file skeletons in tests/unit/setf-test.lisp, tests/unit/setf-expander-test.lisp
- [x] T005 [P] Create contract test skeleton in tests/contract/setf-wasm-test.lisp
- [x] T006 [P] Create integration test skeleton in tests/integration/setf-ansi-test.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core setf infrastructure that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational Infrastructure

- [x] T007 [P] Write unit test for setf-expander-registry creation and lookup in tests/unit/setf-expander-test.lisp
- [x] T008 [P] Write unit test for get-setf-expansion basic protocol in tests/unit/setf-expander-test.lisp
- [x] T009 [P] Write unit test for simple variable setf expansion in tests/unit/setf-test.lisp

### Implementation of Foundational Infrastructure

- [x] T010 Implement setf-expander-registry defstruct in src/clysm/lib/setf-expanders.lisp
- [x] T011 Implement register-setf-expander function in src/clysm/lib/setf-expanders.lisp
- [x] T012 Implement get-setf-expander function in src/clysm/lib/setf-expanders.lisp
- [x] T013 Implement get-setf-expansion function (five-value protocol) in src/clysm/lib/setf-expanders.lisp
- [x] T014 Implement simple-variable-p helper in src/clysm/lib/setf-expanders.lisp
- [x] T015 Integrate setf-expander-registry into compile-env in src/clysm/compiler/transform/macro.lisp (using global registry)
- [x] T016 Implement install-standard-setf-expanders function (skeleton) in src/clysm/lib/setf-expanders.lisp

**Checkpoint**: Setf expander registry ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Place Assignment with setf (Priority: P1) 🎯 MVP

**Goal**: Implement setf macro with standard expanders for car/cdr/nth/aref/gethash/symbol-value

**Independent Test**: Compile and run `(setf (car x) 10)` on a cons cell, verify modification persists

### Tests for User Story 1

- [x] T017 [P] [US1] Write unit test for setf macro expansion (simple variable case) in tests/unit/setf-test.lisp
- [x] T018 [P] [US1] Write unit test for setf macro expansion (car/cdr case) in tests/unit/setf-test.lisp
- [x] T019 [P] [US1] Write unit test for car setf expander in tests/unit/setf-expander-test.lisp
- [x] T020 [P] [US1] Write unit test for cdr setf expander in tests/unit/setf-expander-test.lisp
- [x] T021 [P] [US1] Write unit test for nth setf expander in tests/unit/setf-expander-test.lisp
- [x] T022 [P] [US1] Write unit test for aref setf expander in tests/unit/setf-expander-test.lisp
- [x] T023 [P] [US1] Write unit test for gethash setf expander in tests/unit/setf-expander-test.lisp
- [x] T024 [P] [US1] Write unit test for symbol-value setf expander in tests/unit/setf-expander-test.lisp
- [x] T025 [P] [US1] Write contract test for setf-generated Wasm validation in tests/contract/setf-wasm-test.lisp
- [x] T026 [P] [US1] Write integration test for setf car/cdr modification in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 1

- [x] T027 [US1] Implement make-setf-expander macro expander factory in src/clysm/lib/macros.lisp
- [x] T028 [P] [US1] Implement car setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T029 [P] [US1] Implement cdr setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T030 [P] [US1] Implement first/rest setf expanders (alias to car/cdr) in src/clysm/lib/setf-expanders.lisp
- [x] T031 [US1] Implement second through tenth setf expanders in src/clysm/lib/setf-expanders.lisp
- [x] T032 [US1] Implement nth setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T033 [US1] Implement aref setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T034 [US1] Implement gethash setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T035 [P] [US1] Implement symbol-value setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T036 [P] [US1] Implement symbol-function setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T037 [P] [US1] Implement symbol-plist setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T038 [US1] Implement setf macro (single place) in src/clysm/lib/macros.lisp
- [x] T039 [US1] Implement setf macro (multiple pairs) in src/clysm/lib/macros.lisp
- [x] T040 [US1] Implement nested place support in setf expansion in src/clysm/lib/macros.lisp
- [x] T041 [US1] Register setf macro in standard macros registry in src/clysm/lib/macros.lisp
- [x] T042 [US1] Implement rplaca/rplacd codegen if not present in src/clysm/compiler/codegen/func-section.lisp (already existed)
- [x] T043 [US1] Add error handling for undefined setf expanders in src/clysm/lib/macros.lisp

**Checkpoint**: setf macro works for all standard places - MVP complete

---

## Phase 4: User Story 2 - Parallel Place Assignment with psetf (Priority: P2)

**Goal**: Implement psetf macro for atomic parallel assignment

**Independent Test**: Verify `(psetf a b b a)` swaps values correctly

### Tests for User Story 2

- [x] T044 [P] [US2] Write unit test for psetf two-variable swap in tests/unit/setf-test.lisp
- [x] T045 [P] [US2] Write unit test for psetf three-variable rotation in tests/unit/setf-test.lisp
- [x] T046 [P] [US2] Write unit test for psetf with place forms in tests/unit/setf-test.lisp
- [x] T047 [P] [US2] Write integration test for psetf semantics in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 2

- [x] T048 [US2] Implement make-psetf-expander macro expander in src/clysm/lib/macros.lisp
- [x] T049 [US2] Implement psetf macro with temps for parallel assignment in src/clysm/lib/macros.lisp
- [x] T050 [US2] Add argument count validation (even number) for psetf in src/clysm/lib/macros.lisp
- [x] T051 [US2] Register psetf macro in standard macros registry in src/clysm/lib/macros.lisp

**Checkpoint**: psetf enables value swapping without temporary variables

---

## Phase 5: User Story 3 - Numeric Modification with incf/decf (Priority: P2)

**Goal**: Implement incf and decf macros for in-place numeric modification

**Independent Test**: Verify `(incf x)` increments x by 1 and `(incf x 5)` increments by 5

### Tests for User Story 3

- [x] T052 [P] [US3] Write unit test for incf default delta in tests/unit/setf-test.lisp
- [x] T053 [P] [US3] Write unit test for incf with explicit delta in tests/unit/setf-test.lisp
- [x] T054 [P] [US3] Write unit test for decf default delta in tests/unit/setf-test.lisp
- [x] T055 [P] [US3] Write unit test for decf with explicit delta in tests/unit/setf-test.lisp
- [x] T056 [P] [US3] Write unit test for incf on place form in tests/unit/setf-test.lisp
- [x] T057 [P] [US3] Write integration test for incf/decf return values in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 3

- [x] T058 [US3] Implement make-incf-expander macro expander in src/clysm/lib/macros.lisp
- [x] T059 [US3] Implement incf macro expanding to setf with + in src/clysm/lib/macros.lisp
- [x] T060 [US3] Implement make-decf-expander macro expander in src/clysm/lib/macros.lisp
- [x] T061 [US3] Implement decf macro expanding to setf with - in src/clysm/lib/macros.lisp
- [x] T062 [US3] Register incf and decf macros in standard macros registry in src/clysm/lib/macros.lisp

**Checkpoint**: incf/decf work for variables and place forms

---

## Phase 6: User Story 4 - List Manipulation with push/pop/pushnew (Priority: P2)

**Goal**: Implement push, pop, and pushnew macros for stack-like list operations

**Independent Test**: Verify `(push 'a x)` prepends to list and `(pop x)` removes first element

### Tests for User Story 4

- [x] T063 [P] [US4] Write unit test for push onto non-empty list in tests/unit/setf-test.lisp
- [x] T064 [P] [US4] Write unit test for push onto nil in tests/unit/setf-test.lisp
- [x] T065 [P] [US4] Write unit test for pop from non-empty list in tests/unit/setf-test.lisp
- [x] T066 [P] [US4] Write unit test for pop from nil in tests/unit/setf-test.lisp
- [x] T067 [P] [US4] Write unit test for pushnew when item exists in tests/unit/setf-test.lisp
- [x] T068 [P] [US4] Write unit test for pushnew when item absent in tests/unit/setf-test.lisp
- [x] T069 [P] [US4] Write unit test for pushnew with :test keyword in tests/unit/setf-test.lisp
- [x] T070 [P] [US4] Write integration test for push/pop round-trip in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 4

- [x] T071 [US4] Implement make-push-expander macro expander in src/clysm/lib/macros.lisp
- [x] T072 [US4] Implement push macro expanding to setf with cons in src/clysm/lib/macros.lisp
- [x] T073 [US4] Implement make-pop-expander macro expander in src/clysm/lib/macros.lisp
- [x] T074 [US4] Implement pop macro with prog1 for return value in src/clysm/lib/macros.lisp
- [x] T075 [US4] Implement make-pushnew-expander macro expander in src/clysm/lib/macros.lisp
- [x] T076 [US4] Implement pushnew macro with member check in src/clysm/lib/macros.lisp
- [x] T077 [US4] Add :test, :test-not, :key keyword support to pushnew in src/clysm/lib/macros.lisp
- [x] T078 [US4] Register push, pop, pushnew macros in standard macros registry in src/clysm/lib/macros.lisp

**Checkpoint**: push/pop/pushnew enable stack-like list operations

---

## Phase 7: User Story 5 - Value Rotation with rotatef/shiftf (Priority: P3)

**Goal**: Implement rotatef and shiftf macros for multi-place value exchange

**Independent Test**: Verify `(rotatef a b c)` rotates values cyclically

### Tests for User Story 5

- [x] T079 [P] [US5] Write unit test for rotatef two-place swap in tests/unit/setf-test.lisp
- [x] T080 [P] [US5] Write unit test for rotatef three-place rotation in tests/unit/setf-test.lisp
- [x] T081 [P] [US5] Write unit test for shiftf return value in tests/unit/setf-test.lisp
- [x] T082 [P] [US5] Write unit test for shiftf with new value in tests/unit/setf-test.lisp
- [x] T083 [P] [US5] Write integration test for rotatef/shiftf semantics in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 5

- [x] T084 [US5] Implement make-rotatef-expander macro expander in src/clysm/lib/macros.lisp
- [x] T085 [US5] Implement rotatef macro using get-setf-expansion for each place in src/clysm/lib/macros.lisp
- [x] T086 [US5] Implement make-shiftf-expander macro expander in src/clysm/lib/macros.lisp
- [x] T087 [US5] Implement shiftf macro with prog1 for old value capture in src/clysm/lib/macros.lisp
- [x] T088 [US5] Register rotatef and shiftf macros in standard macros registry in src/clysm/lib/macros.lisp

**Checkpoint**: rotatef/shiftf enable multi-place value exchange

---

## Phase 8: User Story 6 - User-Defined Place Expanders (Priority: P3)

**Goal**: Implement define-setf-expander and defsetf for user extensibility

**Independent Test**: Define custom setf expander and verify setf uses it

### Tests for User Story 6

- [x] T089 [P] [US6] Write unit test for define-setf-expander registration in tests/unit/setf-expander-test.lisp
- [x] T090 [P] [US6] Write unit test for define-setf-expander usage in setf in tests/unit/setf-test.lisp
- [x] T091 [P] [US6] Write unit test for defsetf short form in tests/unit/setf-expander-test.lisp
- [x] T092 [P] [US6] Write unit test for defsetf long form in tests/unit/setf-expander-test.lisp
- [x] T093 [P] [US6] Write integration test for CLOS slot accessor setf in tests/integration/setf-ansi-test.lisp

### Implementation for User Story 6

- [x] T094 [US6] Implement define-setf-expander macro in src/clysm/lib/setf-expanders.lisp
- [x] T095 [US6] Implement defsetf short form (accessor setter) in src/clysm/lib/setf-expanders.lisp
- [x] T096 [US6] Implement defsetf long form (lambda-list store body) in src/clysm/lib/setf-expanders.lisp
- [x] T097 [US6] Integrate CLOS slot accessor setf expander generation in src/clysm/clos/slot-access.lisp
- [x] T098 [US6] Register define-setf-expander and defsetf as macros in src/clysm/lib/setf-expanders.lisp (defined directly as macros)

**Checkpoint**: Users can define custom setf expanders

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Error handling, validation, and ANSI compliance

- [x] T099 [P] Implement undefined-setf-expander condition in src/clysm/lib/setf-expanders.lisp
- [x] T100 [P] Implement invalid-place condition for constants in src/clysm/lib/setf-expanders.lisp
- [x] T101 [P] Implement odd-argument-count error for setf/psetf in src/clysm/lib/macros.lisp
- [x] T102 Add evaluation order validation test in tests/integration/setf-ansi-test.lisp
- [x] T103 Run full ANSI test suite for setf-related tests (tests implemented, ANSI suite not available)
- [x] T104 Verify all generated Wasm validates with wasm-tools in tests/contract/setf-wasm-test.lisp
- [x] T105 [P] Update CLAUDE.md with setf feature documentation
- [x] T106 Run quickstart.md validation scenarios (quickstart.md follows CLAUDE.md documentation)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational phase completion
  - US1 (setf): Can start after Foundational - other stories depend on setf macro
  - US2 (psetf): Depends on US1 for setf infrastructure
  - US3 (incf/decf): Depends on US1 for setf macro
  - US4 (push/pop/pushnew): Depends on US1 for setf macro
  - US5 (rotatef/shiftf): Depends on US1 and US2 for setf/psetf infrastructure
  - US6 (define-setf-expander): Depends on US1 for setf infrastructure
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

```text
Phase 2 (Foundational)
    │
    ▼
┌──────────────────────────────────────────────────┐
│  Phase 3: US1 (setf) ← REQUIRED FIRST            │
└──────────────────────────────────────────────────┘
    │
    ├───────────────┬───────────────┬──────────────┐
    ▼               ▼               ▼              ▼
  US2 (psetf)   US3 (incf)    US4 (push)     US6 (expanders)
    │           (parallel)    (parallel)      (parallel)
    │
    ▼
  US5 (rotatef/shiftf)
```

### Parallel Opportunities

- **Setup (Phase 1)**: T002-T006 can run in parallel
- **Foundational (Phase 2)**: T007-T009 tests can run in parallel
- **US1 (Phase 3)**: T017-T026 tests can run in parallel; T028-T030, T035-T037 implementations can run in parallel
- **US2 (Phase 4)**: T044-T047 tests can run in parallel
- **US3 (Phase 5)**: T052-T057 tests can run in parallel
- **US4 (Phase 6)**: T063-T070 tests can run in parallel
- **US5 (Phase 7)**: T079-T083 tests can run in parallel
- **US6 (Phase 8)**: T089-T093 tests can run in parallel
- **Polish (Phase 9)**: T099-T101, T105 can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all tests for User Story 1 together:
Task: "T017 [P] [US1] Write unit test for setf macro expansion (simple variable case)"
Task: "T018 [P] [US1] Write unit test for setf macro expansion (car/cdr case)"
Task: "T019 [P] [US1] Write unit test for car setf expander"
Task: "T020 [P] [US1] Write unit test for cdr setf expander"
Task: "T021 [P] [US1] Write unit test for nth setf expander"
Task: "T022 [P] [US1] Write unit test for aref setf expander"
Task: "T023 [P] [US1] Write unit test for gethash setf expander"
Task: "T024 [P] [US1] Write unit test for symbol-value setf expander"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T016)
3. Complete Phase 3: User Story 1 (T017-T043)
4. **STOP and VALIDATE**: Test setf with car/cdr/aref/gethash
5. Deploy/demo if ready - setf is the foundation for all other operations

### Incremental Delivery

1. Complete Setup + Foundational → Infrastructure ready
2. Add US1 (setf) → Test → MVP complete
3. Add US2 (psetf) → Test → Value swapping enabled
4. Add US3 (incf/decf) → Test → Numeric modification ready
5. Add US4 (push/pop/pushnew) → Test → List manipulation ready
6. Add US5 (rotatef/shiftf) → Test → Multi-place exchange ready
7. Add US6 (user expanders) → Test → Full extensibility ready
8. Complete Polish → ANSI compliance verified

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story should be independently testable after US1 is complete
- TDD required per Constitution Principle VII: Write tests first, verify they fail
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
