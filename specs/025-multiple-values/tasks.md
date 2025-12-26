# Tasks: ANSI Common Lisp Multiple Values Support

**Input**: Design documents from `/specs/025-multiple-values/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD Non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root

---

## Phase 1: Setup

**Purpose**: No additional setup needed - using existing clysm project structure

*Phase skipped - project already initialized*

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T001 Add $mv-array type definition to type section in src/clysm/compiler/codegen/gc-types.lisp
- [X] T002 Add *mv-count-global-index* constant (= 2) in src/clysm/runtime/objects.lisp
- [X] T003 Add *mv-buffer-global-index* constant (= 3) in src/clysm/runtime/objects.lisp
- [X] T004 Implement make-mv-count-global function in src/clysm/runtime/objects.lisp
- [X] T005 Implement make-mv-buffer-global function in src/clysm/runtime/objects.lisp
- [X] T006 Update *global-counter* initial value from 2 to 4 in src/clysm/runtime/objects.lisp
- [X] T007 Update reset-global-counter to reset to 4 in src/clysm/runtime/objects.lisp
- [X] T008 Update compiler.lisp to include mv-count and mv-buffer globals in module initialization in src/clysm/compiler/compiler.lisp
- [X] T009 [P] Contract test: verify mv globals exist in compiled output in tests/contract/mv-wasm-test.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Return Multiple Values (Priority: P1) 🎯 MVP

**Goal**: Implement `values` special form that returns multiple values

**Independent Test**: Compile `(values 1 2 3)` and verify primary value (1) is returned, secondary values accessible

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T010 [P] [US1] Unit test for (values) returning NIL with count=0 in tests/unit/multiple-values-test.lisp
- [X] T011 [P] [US1] Unit test for (values 42) returning 42 with count=1 in tests/unit/multiple-values-test.lisp
- [X] T012 [P] [US1] Unit test for (values 1 2 3) returning 1 with count=3 in tests/unit/multiple-values-test.lisp
- [X] T013 [P] [US1] Contract test for values Wasm codegen in tests/contract/mv-wasm-test.lisp

### Implementation for User Story 1

- [X] T014 [US1] Add values to special form dispatch in compile-special-form in src/clysm/compiler/codegen/func-section.lisp
- [X] T015 [US1] Implement compile-values function for zero arguments case in src/clysm/compiler/codegen/func-section.lisp
- [X] T016 [US1] Implement compile-values function for single argument case in src/clysm/compiler/codegen/func-section.lisp
- [X] T017 [US1] Implement compile-values function for multiple arguments case in src/clysm/compiler/codegen/func-section.lisp
- [X] T018 [US1] Verify tests pass and Wasm validates with wasm-tools validate

**Checkpoint**: `values` form works - can return multiple values from functions

---

## Phase 4: User Story 2 - Bind Multiple Values (Priority: P1)

**Goal**: Implement `multiple-value-bind` to capture returned values into variables

**Independent Test**: Compile `(multiple-value-bind (a b) (values 1 2) (+ a b))` and verify result is 3

### Tests for User Story 2

- [X] T019 [P] [US2] Unit test for basic mvb binding two values in tests/unit/multiple-values-test.lisp
- [X] T020 [P] [US2] Unit test for mvb with fewer values than variables (NIL fill) in tests/unit/multiple-values-test.lisp
- [X] T021 [P] [US2] Unit test for mvb with more values than variables (extras ignored) in tests/unit/multiple-values-test.lisp
- [X] T022 [P] [US2] Unit test for mvb with empty variable list in tests/unit/multiple-values-test.lisp
- [X] T023 [P] [US2] Contract test for mvb Wasm codegen in tests/contract/mv-wasm-test.lisp

### Implementation for User Story 2

- [X] T024 [US2] Add multiple-value-bind to special form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T025 [US2] Implement compile-multiple-value-bind: evaluate value form and store primary in src/clysm/compiler/codegen/func-section.lisp
- [X] T026 [US2] Implement compile-multiple-value-bind: read secondary values from buffer with count check in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [US2] Implement compile-multiple-value-bind: bind variables and compile body in src/clysm/compiler/codegen/func-section.lisp
- [X] T028 [US2] Verify tests pass and Wasm validates

**Checkpoint**: Can receive multiple values from function calls

---

## Phase 5: User Story 8 - Arithmetic Functions (Priority: P1)

**Goal**: Update `floor`, `truncate`, `ceiling`, `round` to return (quotient remainder) as two values

**Independent Test**: Compile `(multiple-value-list (floor 7 3))` and verify result is `(2 1)`

### Tests for User Story 8

- [X] T029 [P] [US8] Unit test for floor returning quotient and remainder in tests/unit/multiple-values-test.lisp
- [X] T030 [P] [US8] Unit test for truncate with negative numbers in tests/unit/multiple-values-test.lisp
- [X] T031 [P] [US8] Unit test for ceiling returning quotient and remainder in tests/unit/multiple-values-test.lisp
- [X] T032 [P] [US8] Unit test for round returning quotient and remainder in tests/unit/multiple-values-test.lisp

### Implementation for User Story 8

- [X] T033 [US8] Update compile-floor to compute remainder and store in mv-buffer[0] in src/clysm/compiler/codegen/func-section.lisp
- [X] T034 [US8] Update compile-floor to set mv-count=2 and return quotient in src/clysm/compiler/codegen/func-section.lisp
- [X] T035 [P] [US8] Implement compile-truncate with multiple values in src/clysm/compiler/codegen/func-section.lisp
- [X] T036 [P] [US8] Implement compile-ceiling with multiple values in src/clysm/compiler/codegen/func-section.lisp
- [X] T037 [P] [US8] Implement compile-round with multiple values in src/clysm/compiler/codegen/func-section.lisp
- [X] T038 [US8] Verify tests pass and all arithmetic functions return two values

**Checkpoint**: All P1 user stories complete - arithmetic functions return multiple values

---

## Phase 6: User Story 3 - Collect as List (Priority: P2)

**Goal**: Implement `multiple-value-list` to collect all returned values into a list

**Independent Test**: Compile `(multiple-value-list (values 1 2 3))` and verify result is `(1 2 3)`

### Tests for User Story 3

- [X] T039 [P] [US3] Unit test for mvl with zero values in tests/unit/multiple-values-test.lisp
- [X] T040 [P] [US3] Unit test for mvl with multiple values in tests/unit/multiple-values-test.lisp
- [X] T041 [P] [US3] Unit test for mvl with floor result in tests/unit/multiple-values-test.lisp

### Implementation for User Story 3

- [X] T042 [US3] Add multiple-value-list to special form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T043 [US3] Implement compile-multiple-value-list: evaluate form and get primary value in src/clysm/compiler/codegen/func-section.lisp
- [X] T044 [US3] Implement compile-multiple-value-list: loop over mv-buffer building list in src/clysm/compiler/codegen/func-section.lisp
- [X] T045 [US3] Verify tests pass

**Checkpoint**: Can collect multiple values into lists

---

## Phase 7: User Story 4 - Access by Index (Priority: P2)

**Goal**: Implement `nth-value` to access a specific value by index

**Independent Test**: Compile `(nth-value 1 (values 'a 'b 'c))` and verify result is `B`

### Tests for User Story 4

- [X] T046 [P] [US4] Unit test for nth-value index 0 (primary) in tests/unit/multiple-values-test.lisp
- [X] T047 [P] [US4] Unit test for nth-value index 2 (secondary) in tests/unit/multiple-values-test.lisp
- [X] T048 [P] [US4] Unit test for nth-value index out of range (returns NIL) in tests/unit/multiple-values-test.lisp

### Implementation for User Story 4

- [X] T049 [US4] Add nth-value to special form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T050 [US4] Implement compile-nth-value: evaluate index and form in src/clysm/compiler/codegen/func-section.lisp
- [X] T051 [US4] Implement compile-nth-value: check index against mv-count and return value or NIL in src/clysm/compiler/codegen/func-section.lisp
- [X] T052 [US4] Verify tests pass

**Checkpoint**: Can access specific values by index

---

## Phase 8: User Story 5 - Spread List as Values (Priority: P2)

**Goal**: Implement `values-list` to convert a list into multiple values

**Independent Test**: Compile `(multiple-value-list (values-list '(1 2 3)))` and verify result is `(1 2 3)`

### Tests for User Story 5

- [X] T053 [P] [US5] Unit test for values-list with nil in tests/unit/multiple-values-test.lisp
- [X] T054 [P] [US5] Unit test for values-list with proper list in tests/unit/multiple-values-test.lisp
- [X] T055 [P] [US5] Unit test for values-list type error with non-list in tests/unit/multiple-values-test.lisp

### Implementation for User Story 5

- [X] T056 [US5] Add values-list to function dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T057 [US5] Implement compile-values-list: type check for list or nil in src/clysm/compiler/codegen/func-section.lisp
- [X] T058 [US5] Implement compile-values-list: iterate list storing in buffer and counting in src/clysm/compiler/codegen/func-section.lisp
- [X] T059 [US5] Implement compile-values-list: signal type-error if not list in src/clysm/compiler/codegen/func-section.lisp
- [X] T060 [US5] Verify tests pass

**Checkpoint**: Can spread lists as multiple values

---

## Phase 9: User Story 6 - Preserve Values (Priority: P2)

**Goal**: Implement `multiple-value-prog1` to preserve values through side-effect forms

**Independent Test**: Compile `(multiple-value-prog1 (values 1 2) (print 'done))` and verify returns 1, 2

### Tests for User Story 6

- [X] T061 [P] [US6] Unit test for mvp1 preserving multiple values in tests/unit/multiple-values-test.lisp
- [X] T062 [P] [US6] Unit test for mvp1 with single value in tests/unit/multiple-values-test.lisp

### Implementation for User Story 6

- [X] T063 [US6] Add multiple-value-prog1 to special form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T064 [US6] Implement compile-multiple-value-prog1: save primary value and mv-count in locals in src/clysm/compiler/codegen/func-section.lisp
- [X] T065 [US6] Implement compile-multiple-value-prog1: save buffer contents to local array in src/clysm/compiler/codegen/func-section.lisp
- [X] T066 [US6] Implement compile-multiple-value-prog1: execute body forms for side effects in src/clysm/compiler/codegen/func-section.lisp
- [X] T067 [US6] Implement compile-multiple-value-prog1: restore mv-count and buffer, return primary in src/clysm/compiler/codegen/func-section.lisp
- [X] T068 [US6] Verify tests pass

**Checkpoint**: Can preserve multiple values through evaluation

---

## Phase 10: User Story 7 - Pass Values to Functions (Priority: P3)

**Goal**: Implement `multiple-value-call` to pass all values as function arguments

**Independent Test**: Compile `(multiple-value-call #'+ (values 1 2) (values 3 4))` and verify result is 10

### Tests for User Story 7

- [X] T069 [P] [US7] Unit test for mvc with single form in tests/unit/multiple-values-test.lisp
- [X] T070 [P] [US7] Unit test for mvc with multiple forms in tests/unit/multiple-values-test.lisp
- [X] T071 [P] [US7] Unit test for mvc with floor results in tests/unit/multiple-values-test.lisp

### Implementation for User Story 7

- [X] T072 [US7] Add multiple-value-call to special form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T073 [US7] Implement compile-multiple-value-call: evaluate function reference in src/clysm/compiler/codegen/func-section.lisp
- [X] T074 [US7] Implement compile-multiple-value-call: collect all values from each form into accumulator list in src/clysm/compiler/codegen/func-section.lisp
- [X] T075 [US7] Implement compile-multiple-value-call: apply function with accumulated arguments in src/clysm/compiler/codegen/func-section.lisp
- [X] T076 [US7] Verify tests pass

**Checkpoint**: All user stories complete

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: Integration tests, ANSI compliance verification, cleanup

- [X] T077 [P] Integration test: full ANSI compliance scenarios in tests/integration/mv-ansi-test.lisp
- [X] T078 [P] Integration test: edge cases from spec.md in tests/integration/mv-ansi-test.lisp
- [X] T079 [P] Integration test: backward compatibility - single-value code unchanged in tests/integration/mv-ansi-test.lisp
- [X] T080 Verify all generated Wasm validates with wasm-tools validate
- [X] T081 Run quickstart.md verification commands
- [X] T082 Update CLAUDE.md with 025-multiple-values completion notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Skipped - using existing project
- **Foundational (Phase 2)**: No dependencies - can start immediately - **BLOCKS all user stories**
- **User Stories (Phase 3-10)**: All depend on Foundational phase completion
- **Polish (Phase 11)**: Depends on all user stories being complete

### User Story Dependencies

```
Phase 2 (Foundational)
    │
    ├── Phase 3 (US1: values) ───────────────────┬──────────────────────────┐
    │       │                                     │                          │
    │       ├── Phase 4 (US2: mvb) ──────────────┤                          │
    │       │                                     │                          │
    │       └── Phase 5 (US8: floor/etc) ────────┼── Phase 6 (US3: mvl)     │
    │                                             │       │                  │
    │                                             │       ├── Phase 7 (US4)  │
    │                                             │       │                  │
    │                                             │       ├── Phase 8 (US5)  │
    │                                             │       │                  │
    │                                             │       ├── Phase 9 (US6)  │
    │                                             │       │                  │
    │                                             │       └── Phase 10 (US7) │
    │                                             │                          │
    └─────────────────────────────────────────────┴──────────────────────────┘
                                                                              │
                                                              Phase 11 (Polish)
```

- **US1 (values)**: Foundational only - MVP target
- **US2 (mvb)**: Depends on US1 (needs values infrastructure)
- **US8 (arithmetic)**: Depends on US1 (needs values infrastructure)
- **US3 (mvl)**: Depends on US1+US2 (needs both for testing)
- **US4-7**: Can proceed after US1, independent of each other

### Within Each User Story

1. Tests MUST be written and FAIL before implementation
2. Implementation tasks execute sequentially within a story
3. Story complete when all tests pass

### Parallel Opportunities

- T001-T008: Sequential (dependencies)
- T009: Can run after T008
- T010-T013: All parallel (different test cases)
- T014-T017: Sequential (building up values implementation)
- T019-T023: All parallel (different test cases)
- T029-T032: All parallel (different arithmetic functions)
- T035-T037: All parallel (different arithmetic functions)
- T039-T041, T046-T048, T053-T055, T061-T062, T069-T071, T077-T079: Each group parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: T010 "Unit test for (values) returning NIL"
Task: T011 "Unit test for (values 42)"
Task: T012 "Unit test for (values 1 2 3)"
Task: T013 "Contract test for values Wasm codegen"

# After tests fail, implement sequentially:
Task: T014 "Add values to special form dispatch"
Task: T015 "Implement zero arguments case"
Task: T016 "Implement single argument case"
Task: T017 "Implement multiple arguments case"
Task: T018 "Verify tests pass"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 2: Foundational (T001-T009)
2. Complete Phase 3: User Story 1 (T010-T018)
3. **STOP and VALIDATE**: `(values 1 2 3)` compiles and returns correctly
4. Deliverable: Basic multiple values infrastructure

### P1 Complete (Stories 1, 2, 8)

1. Complete Phase 2: Foundational
2. Complete Phase 3: US1 (values)
3. Complete Phase 4: US2 (multiple-value-bind)
4. Complete Phase 5: US8 (floor/truncate/ceiling/round)
5. **STOP and VALIDATE**: `(multiple-value-bind (q r) (floor 7 3) ...)` works
6. Deliverable: Core multiple values functionality

### Full Implementation

1. P1 stories → P2 stories → P3 stories → Polish
2. Each story adds capability without breaking previous

---

## Task Summary

| Phase | Story | Task Count |
|-------|-------|------------|
| 2 | Foundational | 9 |
| 3 | US1 (values) | 9 |
| 4 | US2 (mvb) | 10 |
| 5 | US8 (arithmetic) | 10 |
| 6 | US3 (mvl) | 7 |
| 7 | US4 (nth-value) | 7 |
| 8 | US5 (values-list) | 8 |
| 9 | US6 (mvp1) | 8 |
| 10 | US7 (mvc) | 8 |
| 11 | Polish | 6 |
| **Total** | | **82** |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Tests MUST fail before implementing (TDD per Constitution VII)
- Verify Wasm output with `wasm-tools validate` after each story
- Commit after each task or logical group
