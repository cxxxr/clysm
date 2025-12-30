# Tasks: Phase 16A - ANSI Character Functions

**Input**: Design documents from `/specs/001-ansi-char-functions/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/
**TDD Required**: Yes (Constitution Principle VII)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## ANSI CL References

All implementations must reference HyperSpec per Constitution Principle IX:
- [graphic-char-p](../../resources/HyperSpec/Body/f_graphi.htm)
- [standard-char-p](../../resources/HyperSpec/Body/f_std_ch.htm)
- [both-case-p](../../resources/HyperSpec/Body/f_upper_.htm)
- [char-name](../../resources/HyperSpec/Body/f_char_n.htm)
- [name-char](../../resources/HyperSpec/Body/f_name_c.htm)
- [digit-char](../../resources/HyperSpec/Body/f_digit_.htm)
- [char-int](../../resources/HyperSpec/Body/f_char_i.htm)

---

## Phase 1: Setup

**Purpose**: Prepare test infrastructure for TDD workflow

- [ ] T001 Create test file skeleton in tests/unit/character-functions.lisp
- [ ] T002 [P] Create contract test skeleton in tests/contract/character-wasm.lisp

**Checkpoint**: Test infrastructure ready for TDD workflow

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Register builtin function dispatchers in compiler

**⚠️ CRITICAL**: No user story implementation can proceed without builtin registration

- [ ] T003 Add graphic-char-p to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T004 [P] Add standard-char-p to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T005 [P] Add both-case-p to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T006 [P] Add char-name to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T007 [P] Add name-char to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T008 [P] Add digit-char to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp
- [ ] T009 [P] Add char-int to `*builtin-function-compilers*` in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: All builtin dispatchers registered - user story implementation can begin

---

## Phase 3: User Story 1 - Character Type Classification (Priority: P1) 🎯 MVP

**Goal**: Implement character classification predicates for text processing logic

**Independent Test**: Call each predicate with various character inputs (printable, control, alphabetic) and verify boolean returns against ANSI CL specification

### Tests for User Story 1 (TDD - Write First, Must Fail)

- [ ] T010 [P] [US1] Write unit tests for graphic-char-p in tests/unit/character-functions.lisp
- [ ] T011 [P] [US1] Write unit tests for standard-char-p in tests/unit/character-functions.lisp
- [ ] T012 [P] [US1] Write unit tests for both-case-p in tests/unit/character-functions.lisp
- [ ] T013 [US1] Verify all US1 tests fail before implementation in tests/unit/character-functions.lisp

### Implementation for User Story 1

- [ ] T014 [P] [US1] Implement compile-graphic-char-p (range check 32-126) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T015 [P] [US1] Implement compile-standard-char-p (96-char set check) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T016 [P] [US1] Implement compile-both-case-p (A-Z or a-z check) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T017 [US1] Verify all US1 tests pass after implementation

**Checkpoint**: Character classification predicates fully functional and tested independently

---

## Phase 4: User Story 2 - Character Name Conversion (Priority: P2)

**Goal**: Implement bidirectional character ↔ name conversion for reader/printer

**Independent Test**: Convert named characters to names and back, verify round-trip consistency for all 9 standard names

### Tests for User Story 2 (TDD - Write First, Must Fail)

- [ ] T018 [P] [US2] Write unit tests for char-name in tests/unit/character-functions.lisp
- [ ] T019 [P] [US2] Write unit tests for name-char (case-insensitive) in tests/unit/character-functions.lisp
- [ ] T020 [US2] Verify all US2 tests fail before implementation in tests/unit/character-functions.lisp

### Implementation for User Story 2

- [ ] T021 [P] [US2] Implement compile-char-name (9-name lookup returning $string or NIL) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T022 [US2] Implement compile-name-char (case-insensitive string comparison) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T023 [US2] Verify all US2 tests pass after implementation
- [ ] T024 [US2] Verify round-trip: (name-char (char-name x)) = x for all named characters

**Checkpoint**: Character name conversion fully functional and tested independently

---

## Phase 5: User Story 3 - Digit-Character Conversion (Priority: P2)

**Goal**: Implement weight → digit character conversion for numeric formatting

**Independent Test**: Convert weights 0-35 to characters in various radices (10, 16, 36) and verify output

### Tests for User Story 3 (TDD - Write First, Must Fail)

- [ ] T025 [P] [US3] Write unit tests for digit-char with radix 10 in tests/unit/character-functions.lisp
- [ ] T026 [P] [US3] Write unit tests for digit-char with radix 16 (hex) in tests/unit/character-functions.lisp
- [ ] T027 [P] [US3] Write unit tests for digit-char edge cases (weight >= radix, negative) in tests/unit/character-functions.lisp
- [ ] T028 [US3] Verify all US3 tests fail before implementation in tests/unit/character-functions.lisp

### Implementation for User Story 3

- [ ] T029 [US3] Implement compile-digit-char (inverse of digit-char-p) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T030 [US3] Verify all US3 tests pass after implementation
- [ ] T031 [US3] Verify round-trip: (digit-char (digit-char-p x)) = x for digit characters

**Checkpoint**: Digit-character conversion fully functional and tested independently

---

## Phase 6: User Story 4 - Character Integer Conversion (Priority: P3)

**Goal**: Implement character → integer conversion for ANSI compliance

**Independent Test**: Convert characters to integers and verify uniqueness/consistency

### Tests for User Story 4 (TDD - Write First, Must Fail)

- [ ] T032 [P] [US4] Write unit tests for char-int in tests/unit/character-functions.lisp
- [ ] T033 [US4] Verify all US4 tests fail before implementation in tests/unit/character-functions.lisp

### Implementation for User Story 4

- [ ] T034 [US4] Implement compile-char-int (identity, equivalent to char-code) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T035 [US4] Verify all US4 tests pass after implementation

**Checkpoint**: All 6 character functions implemented and tested

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Validation, integration, and compliance verification

- [ ] T036 [P] Write contract tests for Wasm output validation in tests/contract/character-wasm.lisp
- [ ] T037 Run wasm-tools validate on generated Wasm bytecode
- [ ] T038 Verify 80%+ character category compliance rate (SC-002)
- [ ] T039 Run full test suite to verify no regression (SC-006)
- [ ] T040 [P] Update CLAUDE.md with new feature entry

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - can start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phase 3-6 (User Stories)**: All depend on Phase 2 completion
- **Phase 7 (Polish)**: Depends on all user stories complete

### User Story Dependencies

- **US1 (P1)**: Can start after Phase 2 - No dependencies on other stories
- **US2 (P2)**: Can start after Phase 2 - No dependencies on US1
- **US3 (P2)**: Can start after Phase 2 - No dependencies on US1/US2
- **US4 (P3)**: Can start after Phase 2 - No dependencies on other stories

### Within Each User Story (TDD Order)

1. Write tests FIRST (must FAIL)
2. Verify tests fail
3. Implement function
4. Verify tests PASS
5. Verify round-trip (if applicable)

### Parallel Opportunities

**Phase 2 (All parallel after T003):**
```
T004, T005, T006, T007, T008, T009
```

**US1 Tests (All parallel):**
```
T010, T011, T012
```

**US1 Implementation (All parallel):**
```
T014, T015, T016
```

**US2 Tests (All parallel):**
```
T018, T019
```

**US3 Tests (All parallel):**
```
T025, T026, T027
```

---

## Parallel Example: User Story 1

```bash
# Phase 2: Register all builtins in parallel
Task: "Add graphic-char-p to *builtin-function-compilers*"
Task: "Add standard-char-p to *builtin-function-compilers*"
Task: "Add both-case-p to *builtin-function-compilers*"

# US1 Tests: Write all tests in parallel
Task: "Write unit tests for graphic-char-p"
Task: "Write unit tests for standard-char-p"
Task: "Write unit tests for both-case-p"

# US1 Implementation: All functions in parallel
Task: "Implement compile-graphic-char-p"
Task: "Implement compile-standard-char-p"
Task: "Implement compile-both-case-p"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Register builtins for US1 functions (T003-T005)
3. Complete Phase 3: User Story 1 (Character Classification)
4. **STOP and VALIDATE**: Test US1 independently
5. Character classification predicates available for use

### Incremental Delivery

1. Setup + Foundational → Infrastructure ready
2. Add US1 → Test independently → Character classification works (MVP!)
3. Add US2 → Test independently → char-name/name-char works
4. Add US3 → Test independently → digit-char works
5. Add US4 → Test independently → Full ANSI compliance

### Parallel Team Strategy

With multiple developers after Phase 2 completes:

- Developer A: User Story 1 (Classification)
- Developer B: User Story 2 (Name Conversion)
- Developer C: User Story 3 + 4 (Digit + Int Conversion)

---

## Notes

- [P] tasks = different functions/tests, no file conflicts
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- TDD required: Write tests first, verify they fail, then implement
- All functions follow existing pattern from compile-char-upcase, compile-alpha-char-p
- Commit after each task or logical group
