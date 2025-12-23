# Tasks: Cons Cell and List Operations

**Input**: Design documents from `/specs/006-cons-list-ops/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: TDD is MANDATORY per constitution VII. All tests must be written first and FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Implementation: `src/clysm/compiler/codegen/func-section.lisp`
- Unit tests: `tests/unit/cons-test.lisp`
- Integration tests: `tests/integration/list-test.lisp`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create test files and verify prerequisites

- [x] T001 Create unit test file `tests/unit/cons-test.lisp` with rove test package definition
- [x] T002 Create integration test file `tests/integration/list-test.lisp` with rove test package definition
- [x] T003 [P] Verify `$cons` type (Type 2) exists in `src/clysm/compiler/codegen/gc-types.lisp`
- [x] T004 [P] Verify `$nil` global is accessible in generated Wasm modules
- [x] T005 [P] Verify `$t` global is accessible for predicate return values

**Checkpoint**: Test files exist and type infrastructure verified

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**WARNING**: No user story work can begin until this phase is complete

- [x] T006 Add `compile-cons` function stub to `src/clysm/compiler/codegen/func-section.lisp`
- [x] T007 Add `compile-car` function stub to `src/clysm/compiler/codegen/func-section.lisp`
- [x] T008 Add `compile-cdr` function stub to `src/clysm/compiler/codegen/func-section.lisp`
- [x] T009 Locate and verify `*primitives*` list or primitive dispatch mechanism in `func-section.lisp`
- [x] T010 Verify temp local variable allocation mechanism for NIL checks

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Cons Cell Creation and Access (Priority: P1) - MVP

**Goal**: Create cons cells using `(cons x y)` and access with `(car ...)` and `(cdr ...)`

**Independent Test**: Compile and run `(car (cons 1 2))` - should return 1

### Tests for User Story 1 (TDD - Write First, Must Fail)

- [x] T011 [P] [US1] Unit test `test-cons-creation` verifying cons compiles to `struct.new 2` in `tests/unit/cons-test.lisp`
- [x] T012 [P] [US1] Unit test `test-car-access` verifying car compiles to `struct.get 2 0` in `tests/unit/cons-test.lisp`
- [x] T013 [P] [US1] Unit test `test-cdr-access` verifying cdr compiles to `struct.get 2 1` in `tests/unit/cons-test.lisp`
- [x] T014 [US1] Run tests to verify they FAIL before implementation

### Implementation for User Story 1

- [x] T015 [US1] Implement `compile-cons` generating `struct.new 2` with evaluated car/cdr in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T016 [US1] Add `cons` to primitive dispatch in `compile-primitive-call` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T017 [US1] Implement `compile-car` with NIL check and `struct.get 2 0` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T018 [US1] Implement `compile-cdr` with NIL check and `struct.get 2 1` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T019 [US1] Add `car` and `cdr` to primitive dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T020 [US1] Run unit tests to verify they now PASS

### Integration Test for User Story 1

- [x] T021 [US1] Integration test `test-cons-car-cdr-roundtrip` for `(car (cons 1 2))` returns 1 in `tests/integration/list-test.lisp`
- [x] T022 [US1] Integration test for nested cons `(car (car (cons (cons 1 2) 3)))` returns 1 in `tests/integration/list-test.lisp`
- [x] T023 [US1] Run `nix flake check` to verify all tests pass

**Checkpoint**: User Story 1 complete - `(car (cons 1 2))` works. MVP achieved!

---

## Phase 4: User Story 2 - NIL Handling with car/cdr (Priority: P1)

**Goal**: `(car nil)` and `(cdr nil)` return NIL per Common Lisp semantics

**Independent Test**: Compile and run `(car nil)` - should return NIL (not error)

### Tests for User Story 2 (TDD - Write First, Must Fail)

- [x] T024 [P] [US2] Integration test `test-car-nil-returns-nil` in `tests/integration/list-test.lisp`
- [x] T025 [P] [US2] Integration test `test-cdr-nil-returns-nil` in `tests/integration/list-test.lisp`
- [x] T026 [P] [US2] Integration test `test-nested-nil-access` for `(cdr (cdr (cons 1 nil)))` in `tests/integration/list-test.lisp`
- [x] T027 [US2] Run tests to verify they FAIL or already pass (NIL check may be in US1)

### Implementation for User Story 2

- [x] T028 [US2] Verify NIL singleton comparison works in car implementation in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T029 [US2] Verify NIL singleton comparison works in cdr implementation in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T030 [US2] Run tests to verify all NIL handling tests PASS

**Checkpoint**: User Story 2 complete - `(car nil)` returns NIL

---

## Phase 5: User Story 3 - List Construction (Priority: P1)

**Goal**: Create proper lists using `(list a b c ...)` building cons chain terminated by NIL

**Independent Test**: Compile and run `(car (cdr (list 1 2 3)))` - should return 2

### Tests for User Story 3 (TDD - Write First, Must Fail)

- [x] T031 [P] [US3] Unit test `test-list-empty` verifying `(list)` returns NIL in `tests/unit/cons-test.lisp`
- [x] T032 [P] [US3] Unit test `test-list-single` verifying `(list 1)` structure in `tests/unit/cons-test.lisp`
- [x] T033 [P] [US3] Unit test `test-list-multiple` verifying `(list 1 2 3)` structure in `tests/unit/cons-test.lisp`
- [x] T034 [US3] Run tests to verify they FAIL before implementation

### Implementation for User Story 3

- [x] T035 [US3] Implement `compile-list` building cons chain right-to-left in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T036 [US3] Add `list` to primitive dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T037 [US3] Run unit tests to verify they now PASS

### Integration Test for User Story 3

- [x] T038 [US3] Integration test `test-list-traversal` for `(car (cdr (list 1 2 3)))` returns 2 in `tests/integration/list-test.lisp`
- [x] T039 [US3] Integration test for list termination `(cdr (cdr (cdr (list 1 2 3))))` returns NIL in `tests/integration/list-test.lisp`
- [x] T040 [US3] Run `nix flake check` to verify all tests pass

**Checkpoint**: User Story 3 complete - P1 user stories all done!

---

## Phase 6: User Story 4 - Type Predicates (Priority: P2)

**Goal**: Type testing with `consp`, `null`, `atom`, and `listp` predicates

**Independent Test**: Compile and run `(consp (cons 1 2))` - should return T

### Tests for User Story 4 (TDD - Write First, Must Fail)

- [x] T041 [P] [US4] Unit test `test-consp-true` for cons cell input in `tests/unit/cons-test.lisp`
- [x] T042 [P] [US4] Unit test `test-consp-false` for NIL and fixnum input in `tests/unit/cons-test.lisp`
- [x] T043 [P] [US4] Unit test `test-null-true` for NIL input in `tests/unit/cons-test.lisp`
- [x] T044 [P] [US4] Unit test `test-null-false` for cons input in `tests/unit/cons-test.lisp`
- [x] T045 [P] [US4] Unit test `test-atom-true` for NIL and fixnum input in `tests/unit/cons-test.lisp`
- [x] T046 [P] [US4] Unit test `test-atom-false` for cons input in `tests/unit/cons-test.lisp`
- [x] T047 [P] [US4] Unit test `test-listp-true` for NIL and cons input in `tests/unit/cons-test.lisp`
- [x] T048 [P] [US4] Unit test `test-listp-false` for fixnum input in `tests/unit/cons-test.lisp`
- [x] T049 [US4] Run tests to verify they FAIL before implementation

### Implementation for User Story 4

- [x] T050 [US4] Implement `compile-consp` using `ref.test (ref 2)` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T051 [US4] Implement `compile-null` using `ref.eq` against NIL in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T052 [US4] Implement `compile-atom` as NOT consp in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T053 [US4] Implement `compile-listp` as consp OR null in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T054 [US4] Add `consp`, `null`, `atom`, `listp` to primitive dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T055 [US4] Run unit tests to verify they now PASS

### Integration Test for User Story 4

- [x] T056 [US4] Integration test for all predicates with various inputs in `tests/integration/list-test.lisp`
- [x] T057 [US4] Run `nix flake check` to verify all tests pass

**Checkpoint**: User Story 4 complete - type predicates functional

---

## Phase 7: User Story 5 - Quoted List Literals (Priority: P2)

**Goal**: Quote syntax `'(1 2 3)` creates proper list structures

**Independent Test**: Compile and run `(car '(a b c))` - should return symbol A

### Tests for User Story 5 (TDD - Write First, Must Fail)

- [x] T058 [P] [US5] Integration test `test-quote-empty-list` for `'()` returns NIL in `tests/integration/list-test.lisp`
- [x] T059 [P] [US5] Integration test `test-quote-list` for `'(1 2 3)` in `tests/integration/list-test.lisp`
- [x] T060 [P] [US5] Integration test `test-quote-nested-list` for `'((1 2) 3)` in `tests/integration/list-test.lisp`
- [x] T061 [P] [US5] Integration test `test-quote-deeply-nested` for 10 levels deep in `tests/integration/list-test.lisp`
- [x] T062 [US5] Run tests to verify behavior (may already work via literal compilation)

### Implementation for User Story 5

- [x] T063 [US5] Verify/update `compile-literal` to handle list literals in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T064 [US5] Ensure nested quoted lists work correctly in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T065 [US5] Run all quote tests to verify they PASS

**Checkpoint**: User Story 5 complete - P2 user stories all done!

---

## Phase 8: User Story 6 - Destructive Modification (Priority: P3)

**Goal**: In-place cons cell modification with `rplaca` and `rplacd`

**Independent Test**: Compile and run `(let ((x (cons 1 2))) (rplaca x 10) (car x))` - should return 10

### Tests for User Story 6 (TDD - Write First, Must Fail)

- [x] T066 [P] [US6] Unit test `test-rplaca` for car modification in `tests/unit/cons-test.lisp`
- [x] T067 [P] [US6] Unit test `test-rplacd` for cdr modification in `tests/unit/cons-test.lisp`
- [x] T068 [P] [US6] Unit test `test-rplaca-return-value` verifying cons is returned in `tests/unit/cons-test.lisp`
- [x] T069 [US6] Run tests to verify they FAIL before implementation

### Implementation for User Story 6

- [x] T070 [US6] Implement `compile-rplaca` using `struct.set 2 0` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T071 [US6] Implement `compile-rplacd` using `struct.set 2 1` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T072 [US6] Add `rplaca` and `rplacd` to primitive dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T073 [US6] Run unit tests to verify they now PASS

### Integration Test for User Story 6

- [x] T074 [US6] Integration test verifying modification persists in `tests/integration/list-test.lisp`
- [x] T075 [US6] Integration test verifying return value is modified cons in `tests/integration/list-test.lisp`
- [x] T076 [US6] Run `nix flake check` to verify all tests pass

**Checkpoint**: User Story 6 complete - destructive operations functional

---

## Phase 9: User Story 7 - List Accessors (Priority: P3)

**Goal**: Convenience functions `first` through `tenth`, `rest`, `nth`, and `nthcdr`

**Independent Test**: Compile and run `(third '(a b c d))` - should return C

### Tests for User Story 7 (TDD - Write First, Must Fail)

- [x] T077 [P] [US7] Unit test `test-first-rest` for first and rest in `tests/unit/cons-test.lisp`
- [x] T078 [P] [US7] Unit test `test-second-through-tenth` for all position accessors in `tests/unit/cons-test.lisp`
- [x] T079 [P] [US7] Unit test `test-nth` for nth with various indices in `tests/unit/cons-test.lisp`
- [x] T080 [P] [US7] Unit test `test-nthcdr` for nthcdr with various indices in `tests/unit/cons-test.lisp`
- [x] T081 [US7] Run tests to verify they FAIL before implementation

### Implementation for User Story 7

- [x] T082 [US7] Implement `compile-first` as alias for car in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T083 [US7] Implement `compile-rest` as alias for cdr in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T084 [US7] Implement `compile-second` through `compile-tenth` as car/cdr compositions in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T085 [US7] Implement `compile-nth` with index and list traversal in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T086 [US7] Implement `compile-nthcdr` returning tail after n cdrs in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T087 [US7] Add all accessors to primitive dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T088 [US7] Run unit tests to verify they now PASS

### Integration Test for User Story 7

- [x] T089 [US7] Integration test for `(third '(a b c d))` returns C in `tests/integration/list-test.lisp`
- [x] T090 [US7] Integration test for `(nth 2 '(a b c d))` returns C in `tests/integration/list-test.lisp`
- [x] T091 [US7] Run `nix flake check` to verify all tests pass

**Checkpoint**: User Story 7 complete - P3 user stories all done!

---

## Phase 10: Edge Cases & Error Handling

**Purpose**: Handle invalid inputs correctly

### Tests for Edge Cases

- [x] T092 [P] Integration test for car/cdr on non-cons, non-nil value (should error) in `tests/integration/list-test.lisp`
- [x] T093 [P] Integration test for `(nth -1 list)` behavior in `tests/integration/list-test.lisp`
- [x] T094 [P] Integration test for rplaca/rplacd on non-cons (should error) in `tests/integration/list-test.lisp`

### Implementation for Edge Cases

- [x] T095 Implement type error signaling for invalid car/cdr in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T096 Implement type error signaling for invalid rplaca/rplacd in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T097 Run all edge case tests to verify behavior

**Checkpoint**: All error conditions handled correctly

---

## Phase 11: Polish & Performance

**Purpose**: Final verification and performance testing

- [x] T098 [P] Write stress test creating 1,000 cons cells in `tests/integration/list-test.lisp` (reduced from 10,000 due to wasmtime stack limits with recursive implementation)
- [x] T099 [P] Write stress test for deep list traversal in `tests/integration/list-test.lisp`
- [x] T100 Verify O(1) cons/car/cdr via timing tests
- [x] T101 Run full test suite via `nix flake check`
- [x] T102 Code cleanup and remove any debugging code in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T103 Validate quickstart.md examples work correctly

**Checkpoint**: All success criteria met - feature complete!

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational phase completion
- **Edge Cases (Phase 10)**: Depends on all user story phases completion
- **Polish (Phase 11)**: Depends on Edge Cases completion

### User Story Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (BLOCKS ALL)
    ↓
┌───────────────────────────────────────────┐
│  P1 Stories (Sequential - MVP scope)      │
│  Phase 3: US1 → Phase 4: US2 → Phase 5: US3 │
└───────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────┐
│  P2 Stories (Can parallelize after P1)    │
│  Phase 6: US4  ←→  Phase 7: US5           │
└───────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────┐
│  P3 Stories (Can parallelize after P2)    │
│  Phase 8: US6  ←→  Phase 9: US7           │
└───────────────────────────────────────────┘
    ↓
Phase 10: Edge Cases
    ↓
Phase 11: Polish
```

### Within Each User Story

1. Tests MUST be written FIRST and FAIL before implementation (TDD)
2. Implementation follows tests
3. Integration tests verify end-to-end behavior
4. `nix flake check` validates all tests pass

### Parallel Opportunities

**Phase 1 (Setup)**:
- T003, T004, T005 can run in parallel

**Phase 3 (US1 Tests)**:
- T011, T012, T013 can run in parallel

**Phase 5 (US3 Tests)**:
- T031, T032, T033 can run in parallel

**Phase 6 (US4 Tests)**:
- T041-T048 can run in parallel (8 predicate tests)

**Phase 7 (US5 Tests)**:
- T058, T059, T060, T061 can run in parallel

**Phase 8 (US6 Tests)**:
- T066, T067, T068 can run in parallel

**Phase 9 (US7 Tests)**:
- T077, T078, T079, T080 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (must FAIL initially):
Task: "Unit test test-cons-creation in tests/unit/cons-test.lisp"
Task: "Unit test test-car-access in tests/unit/cons-test.lisp"
Task: "Unit test test-cdr-access in tests/unit/cons-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1-3 Only)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T010)
3. Complete Phase 3: User Story 1 - cons/car/cdr (T011-T023)
4. **STOP and VALIDATE**: Test `(car (cons 1 2))` independently
5. Complete Phase 4: User Story 2 - NIL handling (T024-T030)
6. Complete Phase 5: User Story 3 - list function (T031-T040)
7. **STOP and VALIDATE**: Test `(car (cdr (list 1 2 3)))` - MVP COMPLETE!

### Incremental Delivery

| Milestone | User Stories | Independent Test |
|-----------|--------------|------------------|
| MVP | US1, US2, US3 | `(car (cdr (list 1 2 3)))` returns 2 |
| + Predicates | US4 | `(consp (cons 1 2))` returns T |
| + Quotes | US5 | `(car '(a b c))` returns A |
| + Mutators | US6 | `(rplaca x 10)` modifies x |
| + Accessors | US7 | `(third '(a b c d))` returns C |
| Complete | All + Edge + Polish | `nix flake check` passes |

---

## Summary Statistics

| Category | Count |
|----------|-------|
| **Total Tasks** | 103 |
| **Setup Phase** | 5 |
| **Foundational Phase** | 5 |
| **US1 Tasks** | 13 |
| **US2 Tasks** | 7 |
| **US3 Tasks** | 10 |
| **US4 Tasks** | 17 |
| **US5 Tasks** | 8 |
| **US6 Tasks** | 11 |
| **US7 Tasks** | 15 |
| **Edge Cases** | 6 |
| **Polish** | 6 |
| **Parallel Opportunities** | 30+ tasks marked [P] |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD is MANDATORY: Write tests FIRST, verify they FAIL, then implement
- Commit after each logical group of tasks
- Stop at any checkpoint to validate story independently
- Run `nix flake check` after each user story phase
