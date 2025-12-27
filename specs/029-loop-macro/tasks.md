# Tasks: LOOP Macro Implementation

**Input**: Design documents from `/specs/029-loop-macro/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: TDD mandatory per constitution (Principle VII). Tests MUST be written first and FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and LOOP macro infrastructure

- [X] T001 Define loop-context struct in src/clysm/lib/macros.lisp
- [X] T002 Define loop-iteration-clause struct hierarchy in src/clysm/lib/macros.lisp
- [X] T003 [P] Define loop-accumulation-clause struct in src/clysm/lib/macros.lisp
- [X] T004 [P] Define loop-termination-clause struct in src/clysm/lib/macros.lisp
- [X] T005 [P] Define loop-conditional-clause struct in src/clysm/lib/macros.lisp
- [X] T006 Export LOOP symbol in src/clysm/package.lisp
- [X] T007 Create tests/unit/loop-test.lisp with rove test scaffold

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: LOOP clause parser infrastructure that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T008 Implement parse-loop-clauses dispatcher in src/clysm/lib/macros.lisp
- [X] T009 Implement loop-keyword-p predicate for LOOP keyword recognition in src/clysm/lib/macros.lisp
- [X] T010 Implement make-loop-gensym for unique variable generation in src/clysm/lib/macros.lisp
- [X] T011 Implement expand-loop main entry point (skeleton) in src/clysm/lib/macros.lisp
- [X] T012 Register LOOP macro with (register-macro registry 'loop ...) in src/clysm/lib/macros.lisp
- [X] T013 Create tests/contract/loop-wasm-test.lisp with Wasm validation scaffold
- [X] T014 [P] Create tests/integration/loop-ansi-test.lisp with ANSI compliance scaffold

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Iteration with FOR/AS Clauses (Priority: P1) 🎯 MVP

**Goal**: Implement FOR/AS clauses for arithmetic, list, and vector iteration

**Independent Test**: `(loop for i from 1 to 3 collect i)` returns `(1 2 3)`

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T015 [P] [US1] Unit test for FOR arithmetic parsing (from/to/by) in tests/unit/loop-test.lisp
- [X] T016 [P] [US1] Unit test for FOR IN list parsing in tests/unit/loop-test.lisp
- [X] T017 [P] [US1] Unit test for FOR ON list parsing in tests/unit/loop-test.lisp
- [X] T018 [P] [US1] Unit test for FOR ACROSS vector parsing in tests/unit/loop-test.lisp
- [X] T019 [P] [US1] Unit test for FOR = THEN parsing in tests/unit/loop-test.lisp
- [X] T020 [P] [US1] Contract test: FOR arithmetic expansion validates as Wasm in tests/contract/loop-wasm-test.lisp
- [X] T021 [P] [US1] Integration test: FOR from 1 to 10 executes correctly in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 1

- [X] T022 [US1] Implement parse-for-arithmetic for FROM/TO/BELOW/ABOVE/DOWNTO/DOWNFROM/BY in src/clysm/lib/macros.lisp
- [X] T023 [US1] Implement parse-for-in for IN list iteration in src/clysm/lib/macros.lisp
- [X] T024 [US1] Implement parse-for-on for ON list iteration in src/clysm/lib/macros.lisp
- [X] T025 [US1] Implement parse-for-across for ACROSS vector iteration in src/clysm/lib/macros.lisp
- [X] T026 [US1] Implement parse-for-equals for = init THEN step in src/clysm/lib/macros.lisp
- [X] T027 [US1] Implement generate-iteration-init for initializing iteration variables in src/clysm/lib/macros.lisp
- [X] T028 [US1] Implement generate-iteration-step for stepping variables via psetq in src/clysm/lib/macros.lisp
- [X] T029 [US1] Implement generate-iteration-test for termination conditions in src/clysm/lib/macros.lisp
- [X] T030 [US1] Implement expand-loop-body with tagbody/go structure in src/clysm/lib/macros.lisp

**Checkpoint**: `(loop for i from 1 to 3 do (print i))` should work

---

## Phase 4: User Story 2 - Accumulation Clauses (Priority: P1) 🎯 MVP

**Goal**: Implement COLLECT, SUM, COUNT, MAXIMIZE, MINIMIZE, APPEND, NCONC

**Independent Test**: `(loop for i from 1 to 3 collect (* i i))` returns `(1 4 9)`

### Tests for User Story 2 ⚠️

- [X] T031 [P] [US2] Unit test for COLLECT parsing in tests/unit/loop-test.lisp
- [X] T032 [P] [US2] Unit test for SUM parsing in tests/unit/loop-test.lisp
- [X] T033 [P] [US2] Unit test for COUNT parsing in tests/unit/loop-test.lisp
- [X] T034 [P] [US2] Unit test for MAXIMIZE/MINIMIZE parsing in tests/unit/loop-test.lisp
- [X] T035 [P] [US2] Unit test for APPEND/NCONC parsing in tests/unit/loop-test.lisp
- [X] T036 [P] [US2] Unit test for INTO keyword parsing in tests/unit/loop-test.lisp
- [X] T037 [P] [US2] Unit test for conflicting accumulation error detection in tests/unit/loop-test.lisp
- [X] T038 [US2] Integration test: COLLECT returns correct list in tests/integration/loop-ansi-test.lisp
- [X] T039 [US2] Integration test: SUM returns correct total in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 2

- [X] T040 [US2] Implement parse-accumulation-clause for all accumulation types in src/clysm/lib/macros.lisp
- [X] T041 [US2] Implement validate-accumulations for conflict detection (FR-021) in src/clysm/lib/macros.lisp
- [X] T042 [US2] Implement generate-accumulator-init with proper initial values in src/clysm/lib/macros.lisp
- [X] T043 [US2] Implement generate-collect-update with tail-pointer technique in src/clysm/lib/macros.lisp
- [X] T044 [US2] Implement generate-sum-update in src/clysm/lib/macros.lisp
- [X] T045 [US2] Implement generate-count-update in src/clysm/lib/macros.lisp
- [X] T046 [US2] Implement generate-maximize-update in src/clysm/lib/macros.lisp
- [X] T047 [US2] Implement generate-minimize-update in src/clysm/lib/macros.lisp
- [X] T048 [US2] Implement generate-append-update in src/clysm/lib/macros.lisp
- [X] T049 [US2] Implement generate-nconc-update in src/clysm/lib/macros.lisp
- [X] T050 [US2] Integrate accumulation into expand-loop-body return value in src/clysm/lib/macros.lisp

**Checkpoint**: `(loop for i from 1 to 5 sum i)` returns `15`

---

## Phase 5: User Story 3 - Termination and Control Clauses (Priority: P2)

**Goal**: Implement WHILE, UNTIL, ALWAYS, NEVER, THEREIS, RETURN, LOOP-FINISH

**Independent Test**: `(loop for i from 1 until (> i 5) collect i)` returns `(1 2 3 4 5)`

### Tests for User Story 3 ⚠️

- [X] T051 [P] [US3] Unit test for WHILE parsing in tests/unit/loop-test.lisp
- [X] T052 [P] [US3] Unit test for UNTIL parsing in tests/unit/loop-test.lisp
- [X] T053 [P] [US3] Unit test for ALWAYS/NEVER/THEREIS parsing in tests/unit/loop-test.lisp
- [X] T054 [P] [US3] Unit test for RETURN parsing in tests/unit/loop-test.lisp
- [X] T055 [P] [US3] Unit test for LOOP-FINISH parsing in tests/unit/loop-test.lisp
- [X] T056 [US3] Integration test: WHILE terminates correctly in tests/integration/loop-ansi-test.lisp
- [X] T057 [US3] Integration test: ALWAYS returns T or NIL in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 3

- [X] T058 [US3] Implement parse-termination-clause for WHILE/UNTIL in src/clysm/lib/macros.lisp
- [X] T059 [US3] Implement parse-boolean-aggregation for ALWAYS/NEVER/THEREIS in src/clysm/lib/macros.lisp
- [X] T060 [US3] Implement parse-return for RETURN clause in src/clysm/lib/macros.lisp
- [X] T061 [US3] Implement generate-while-test in src/clysm/lib/macros.lisp
- [X] T062 [US3] Implement generate-until-test in src/clysm/lib/macros.lisp
- [X] T063 [US3] Implement generate-always with early NIL return in src/clysm/lib/macros.lisp
- [X] T064 [US3] Implement generate-never with early NIL return in src/clysm/lib/macros.lisp
- [X] T065 [US3] Implement generate-thereis with early value return in src/clysm/lib/macros.lisp
- [X] T066 [US3] Implement generate-return with block return in src/clysm/lib/macros.lisp
- [X] T067 [US3] Implement LOOP-FINISH as go to epilogue tag in src/clysm/lib/macros.lisp

**Checkpoint**: Boolean aggregation and early exit work correctly

---

## Phase 6: User Story 4 - Conditional Clauses (Priority: P2)

**Goal**: Implement IF, WHEN, UNLESS with ELSE and AND chaining

**Independent Test**: `(loop for i from 1 to 5 when (oddp i) collect i)` returns `(1 3 5)`

### Tests for User Story 4 ⚠️

- [X] T068 [P] [US4] Unit test for IF/WHEN parsing in tests/unit/loop-test.lisp
- [X] T069 [P] [US4] Unit test for UNLESS parsing in tests/unit/loop-test.lisp
- [X] T070 [P] [US4] Unit test for ELSE clause parsing in tests/unit/loop-test.lisp
- [X] T071 [P] [US4] Unit test for AND clause chaining in tests/unit/loop-test.lisp
- [X] T072 [US4] Integration test: WHEN filters correctly in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 4

- [X] T073 [US4] Implement parse-conditional-clause for IF/WHEN/UNLESS in src/clysm/lib/macros.lisp
- [X] T074 [US4] Implement parse-conditional-scope for clause grouping until END in src/clysm/lib/macros.lisp
- [X] T075 [US4] Implement parse-else-clause for ELSE branch in src/clysm/lib/macros.lisp
- [X] T076 [US4] Implement parse-and-chain for AND clause combination in src/clysm/lib/macros.lisp
- [X] T077 [US4] Implement generate-conditional expanding to if/when/unless in src/clysm/lib/macros.lisp

**Checkpoint**: Conditional accumulation works: `(loop for x in list when (evenp x) collect x)`

---

## Phase 7: User Story 5 - INITIALLY and FINALLY Clauses (Priority: P2)

**Goal**: Implement INITIALLY for prologue and FINALLY for epilogue

**Independent Test**: `(loop for i from 1 to 3 finally (return 'done))` returns `DONE`

### Tests for User Story 5 ⚠️

- [X] T078 [P] [US5] Unit test for INITIALLY parsing in tests/unit/loop-test.lisp
- [X] T079 [P] [US5] Unit test for FINALLY parsing in tests/unit/loop-test.lisp
- [X] T080 [US5] Integration test: INITIALLY executes before first iteration in tests/integration/loop-ansi-test.lisp
- [X] T081 [US5] Integration test: FINALLY executes after normal termination in tests/integration/loop-ansi-test.lisp
- [X] T082 [US5] Integration test: RETURN skips FINALLY in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 5

- [X] T083 [US5] Implement parse-initially-clause in src/clysm/lib/macros.lisp
- [X] T084 [US5] Implement parse-finally-clause in src/clysm/lib/macros.lisp
- [X] T085 [US5] Add INITIALLY forms after variable bindings in expand-loop-body in src/clysm/lib/macros.lisp
- [X] T086 [US5] Add FINALLY forms before result in expand-loop-body in src/clysm/lib/macros.lisp
- [X] T087 [US5] Ensure RETURN bypasses FINALLY via direct block return in src/clysm/lib/macros.lisp

**Checkpoint**: Prologue and epilogue execute at correct times

---

## Phase 8: User Story 6 - Variable Binding Clauses (Priority: P2)

**Goal**: Implement WITH clause for local variable binding

**Independent Test**: `(loop with x = 10 for i from 1 to 3 collect (+ x i))` returns `(11 12 13)`

### Tests for User Story 6 ⚠️

- [X] T088 [P] [US6] Unit test for WITH var = expr parsing in tests/unit/loop-test.lisp
- [X] T089 [P] [US6] Unit test for WITH var (nil default) parsing in tests/unit/loop-test.lisp
- [X] T090 [P] [US6] Unit test for WITH AND parallel binding parsing in tests/unit/loop-test.lisp
- [X] T091 [US6] Integration test: WITH bindings available in body in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 6

- [X] T092 [US6] Implement parse-with-clause in src/clysm/lib/macros.lisp
- [X] T093 [US6] Implement parse-with-and for parallel binding with AND in src/clysm/lib/macros.lisp
- [X] T094 [US6] Add WITH bindings to let wrapper in expand-loop-body in src/clysm/lib/macros.lisp

**Checkpoint**: WITH bindings work correctly with sequential and parallel modes

---

## Phase 9: User Story 7 - DO Clause for Side Effects (Priority: P3)

**Goal**: Implement DO/DOING clause for side effect forms

**Independent Test**: `(loop for i from 1 to 3 do (print i))` prints 1 2 3

### Tests for User Story 7 ⚠️

- [X] T095 [P] [US7] Unit test for DO clause parsing in tests/unit/loop-test.lisp
- [X] T096 [P] [US7] Unit test for DOING synonym parsing in tests/unit/loop-test.lisp
- [X] T097 [US7] Integration test: DO executes each iteration in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 7

- [X] T098 [US7] Implement parse-do-clause in src/clysm/lib/macros.lisp
- [X] T099 [US7] Add DO forms to body generation in expand-loop-body in src/clysm/lib/macros.lisp

**Checkpoint**: Side effect forms execute correctly each iteration

---

## Phase 10: User Story 8 - Named Loops and RETURN-FROM (Priority: P3)

**Goal**: Implement NAMED clause for block naming and nested loop exit

**Independent Test**: Nested loop with `(return-from outer value)` exits correctly

### Tests for User Story 8 ⚠️

- [X] T100 [P] [US8] Unit test for NAMED clause parsing in tests/unit/loop-test.lisp
- [X] T101 [US8] Integration test: NAMED establishes block in tests/integration/loop-ansi-test.lisp
- [X] T102 [US8] Integration test: RETURN-FROM exits named loop in tests/integration/loop-ansi-test.lisp

### Implementation for User Story 8

- [X] T103 [US8] Implement parse-named-clause in src/clysm/lib/macros.lisp
- [X] T104 [US8] Use named block in expand-loop-body when NAMED present in src/clysm/lib/macros.lisp
- [X] T105 [US8] Default block name is NIL when NAMED not present in src/clysm/lib/macros.lisp

**Checkpoint**: Named loops work with RETURN-FROM for nested loop exit

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: Improvements affecting multiple user stories

- [X] T106 [P] Add multiple FOR clause parallel stepping (psetq) in src/clysm/lib/macros.lisp
- [X] T107 [P] Add AND keyword support for parallel FOR stepping in src/clysm/lib/macros.lisp
- [X] T108 Add comprehensive error messages with source context in src/clysm/lib/macros.lisp
- [X] T109 [P] Add edge case tests for empty iteration in tests/unit/loop-test.lisp
- [X] T110 [P] Add edge case tests for simple LOOP (infinite) in tests/unit/loop-test.lisp
- [X] T111 Import ANSI LOOP tests from ansi-test/iteration/loop*.lsp to tests/integration/loop-ansi-test.lisp
- [X] T112 Run full test suite with nix flake check
- [X] T113 Verify at least 50 ANSI test cases pass (SC-005)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-10)**: All depend on Foundational phase completion
  - US1 + US2 are both P1 priority - core MVP
  - US3-US6 are P2 priority - important features
  - US7-US8 are P3 priority - advanced features
- **Polish (Phase 11)**: Depends on US1-US8 being complete

### User Story Dependencies

- **User Story 1 (P1)**: FOR/AS iteration - foundational, no story dependencies
- **User Story 2 (P1)**: Accumulation - uses US1 iteration, but independently testable
- **User Story 3 (P2)**: Termination - uses US1 iteration, independently testable
- **User Story 4 (P2)**: Conditionals - can wrap US2 accumulation, independently testable
- **User Story 5 (P2)**: INITIALLY/FINALLY - uses loop structure, independently testable
- **User Story 6 (P2)**: WITH bindings - uses loop structure, independently testable
- **User Story 7 (P3)**: DO clause - uses loop structure, independently testable
- **User Story 8 (P3)**: NAMED loops - wraps block naming, independently testable

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Parsing before code generation
- Code generation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- T003, T004, T005 can run in parallel (different struct definitions)
- T015-T021 (US1 tests) can all run in parallel
- T031-T039 (US2 tests) can all run in parallel
- T051-T057 (US3 tests) can all run in parallel
- All [P] marked tasks within each phase can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all US1 tests together:
Task: "Unit test for FOR arithmetic parsing in tests/unit/loop-test.lisp"
Task: "Unit test for FOR IN list parsing in tests/unit/loop-test.lisp"
Task: "Unit test for FOR ON list parsing in tests/unit/loop-test.lisp"
Task: "Unit test for FOR ACROSS vector parsing in tests/unit/loop-test.lisp"
Task: "Unit test for FOR = THEN parsing in tests/unit/loop-test.lisp"
Task: "Contract test: FOR arithmetic expansion validates as Wasm"
Task: "Integration test: FOR from 1 to 10 executes correctly"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (FOR/AS iteration)
4. Complete Phase 4: User Story 2 (Accumulation)
5. **STOP and VALIDATE**: Test `(loop for i from 1 to 10 collect (* i i))`
6. MVP delivers core LOOP functionality

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 (FOR/AS) → Test → Working iteration
3. Add US2 (Accumulation) → Test → MVP complete!
4. Add US3 (Termination) → Test → Better control flow
5. Add US4 (Conditionals) → Test → Filtering capability
6. Add US5-US8 → Test → Full LOOP implementation
7. Polish → 50+ ANSI tests passing

---

## Notes

- [P] tasks = different files or no dependencies
- [Story] label maps task to specific user story
- Each user story should be independently completable and testable
- TDD required: Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Hash-table iteration (BEING THE HASH-KEYS OF) deferred per research.md
