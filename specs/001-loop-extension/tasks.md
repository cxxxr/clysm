# Tasks: LOOP Macro Extension

**Input**: Design documents from `/specs/001-loop-extension/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: INCLUDED (Constitution VII requires TDD for all features)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: User story this task belongs to (US1, US2, US3, US4)
- Exact file paths included in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Test infrastructure and baseline verification

- [ ] T001 Create test directory structure at tests/unit/loop-extension/
- [ ] T002 Verify baseline LOOP functionality works (for/in, collect, etc.) by running existing tests
- [ ] T003 Document existing LOOP implementation state in src/clysm/lib/macros.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Data structure extensions required by ALL user stories

**CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 Extend loop-iter-hash struct with using-var field in src/clysm/lib/macros.lisp
- [ ] T005 Extend loop-iter-hash struct with using-type field in src/clysm/lib/macros.lisp
- [ ] T006 Extend loop-iter-hash struct with entries-var field in src/clysm/lib/macros.lisp
- [ ] T007 Extend loop-iter-hash struct with iter-var field in src/clysm/lib/macros.lisp
- [ ] T008 Add loop-iter-hash-p predicate if not already present in src/clysm/lib/macros.lisp

**Checkpoint**: Foundation ready - struct extensions complete

---

## Phase 3: User Story 1 - Hash-Table Iteration (Priority: P1) MVP

**Goal**: Support `being the hash-keys of`, `being the hash-values of`, and `using (hash-value v)` syntax

**Independent Test**: Compile and run LOOP expressions that iterate over hash-tables and verify correct key/value enumeration

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.2.1.7

### Tests for User Story 1 (TDD - Write First, Verify FAIL)

- [ ] T009 [P] [US1] Write test for hash-keys basic iteration in tests/unit/loop-extension/hash-iteration-test.lisp
- [ ] T010 [P] [US1] Write test for hash-values basic iteration in tests/unit/loop-extension/hash-iteration-test.lisp
- [ ] T011 [P] [US1] Write test for hash-keys with using (hash-value) in tests/unit/loop-extension/hash-iteration-test.lisp
- [ ] T012 [P] [US1] Write test for hash-values with using (hash-key) in tests/unit/loop-extension/hash-iteration-test.lisp
- [ ] T013 [P] [US1] Write test for empty hash-table iteration in tests/unit/loop-extension/hash-iteration-test.lisp

### Implementation for User Story 1

- [ ] T014 [US1] Update parse-for-hash to detect USING keyword in src/clysm/lib/macros.lisp
- [ ] T015 [US1] Update parse-for-hash to parse (hash-value var) form in src/clysm/lib/macros.lisp
- [ ] T016 [US1] Update parse-for-hash to parse (hash-key var) form in src/clysm/lib/macros.lisp
- [ ] T017 [US1] Add hash-table entry collection code in generate-iteration-bindings in src/clysm/lib/macros.lisp
- [ ] T018 [US1] Add primary variable (key or value) binding in generate-iteration-bindings in src/clysm/lib/macros.lisp
- [ ] T019 [US1] Add secondary variable (using) binding in generate-iteration-bindings in src/clysm/lib/macros.lisp
- [ ] T020 [US1] Add hash-table exhaustion test in generate-termination-tests in src/clysm/lib/macros.lisp
- [ ] T021 [US1] Add iterator advancement in generate-iteration-steps in src/clysm/lib/macros.lisp
- [ ] T022 [US1] Add primary variable update in generate-iteration-steps in src/clysm/lib/macros.lisp
- [ ] T023 [US1] Add secondary variable update in generate-iteration-steps in src/clysm/lib/macros.lisp
- [ ] T024 [US1] Verify all US1 tests pass (Red→Green complete)

**Checkpoint**: User Story 1 complete - hash-table iteration works independently

---

## Phase 4: User Story 2 - WITH Clause (Priority: P2)

**Goal**: Support `with var = initial-value` and parallel bindings with `and`

**Independent Test**: Run LOOP expressions with `with` clauses and verify bindings are correctly initialized

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.1.7

### Tests for User Story 2 (TDD - Write First, Verify FAIL)

- [ ] T025 [P] [US2] Write test for with basic binding in tests/unit/loop-extension/with-clause-test.lisp
- [ ] T026 [P] [US2] Write test for with no initializer (nil default) in tests/unit/loop-extension/with-clause-test.lisp
- [ ] T027 [P] [US2] Write test for with sequential bindings (let* semantics) in tests/unit/loop-extension/with-clause-test.lisp
- [ ] T028 [P] [US2] Write test for with parallel bindings using AND in tests/unit/loop-extension/with-clause-test.lisp

### Implementation for User Story 2

- [ ] T029 [US2] Verify with-bindings ordering in expand-loop (should precede iteration bindings) in src/clysm/lib/macros.lisp
- [ ] T030 [US2] Verify parse-with-clause handles AND keyword for parallel bindings in src/clysm/lib/macros.lisp
- [ ] T031 [US2] Fix with-bindings ordering if needed - WITH before FOR in all-bindings in src/clysm/lib/macros.lisp
- [ ] T032 [US2] Verify all US2 tests pass (Red→Green complete)

**Checkpoint**: User Story 2 complete - WITH clause works independently

---

## Phase 5: User Story 3 - FINALLY Clause (Priority: P2)

**Goal**: Support `finally` clause for post-loop cleanup and return value specification

**Independent Test**: Run LOOP expressions with `finally` clauses and verify cleanup code executes

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.8.1

### Tests for User Story 3 (TDD - Write First, Verify FAIL)

- [ ] T033 [P] [US3] Write test for finally with cleanup form in tests/unit/loop-extension/finally-clause-test.lisp
- [ ] T034 [P] [US3] Write test for finally with return in tests/unit/loop-extension/finally-clause-test.lisp
- [ ] T035 [P] [US3] Write test for finally with multiple forms in tests/unit/loop-extension/finally-clause-test.lisp
- [ ] T036 [P] [US3] Write test for finally without explicit return (nil default) in tests/unit/loop-extension/finally-clause-test.lisp

### Implementation for User Story 3

- [ ] T037 [US3] Verify finally-forms placement in expand-loop (after loop-end tag) in src/clysm/lib/macros.lisp
- [ ] T038 [US3] Verify finally-forms execute after loop completion in src/clysm/lib/macros.lisp
- [ ] T039 [US3] Fix finally-forms placement if needed in expand-loop in src/clysm/lib/macros.lisp
- [ ] T040 [US3] Verify all US3 tests pass (Red→Green complete)

**Checkpoint**: User Story 3 complete - FINALLY clause works independently

---

## Phase 6: User Story 4 - INTO Accumulator (Priority: P3)

**Goal**: Support `into variable` modifier for named accumulators accessible in body and finally

**Independent Test**: Run LOOP expressions with `into` clauses and verify correct accumulation

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.3

### Tests for User Story 4 (TDD - Write First, Verify FAIL)

- [ ] T041 [P] [US4] Write test for collect into named variable in tests/unit/loop-extension/into-clause-test.lisp
- [ ] T042 [P] [US4] Write test for sum into named variable in tests/unit/loop-extension/into-clause-test.lisp
- [ ] T043 [P] [US4] Write test for multiple into accumulators in tests/unit/loop-extension/into-clause-test.lisp
- [ ] T044 [P] [US4] Write test for into variable accessible in finally in tests/unit/loop-extension/into-clause-test.lisp
- [ ] T045 [P] [US4] Write test for into variable accessible in loop body in tests/unit/loop-extension/into-clause-test.lisp
- [ ] T046 [P] [US4] Write test for conflicting into clause error in tests/unit/loop-extension/into-clause-test.lisp

### Implementation for User Story 4

- [ ] T047 [US4] Verify into-var captured in loop-accumulation-clause struct in src/clysm/lib/macros.lisp
- [ ] T048 [US4] Verify generate-accumulator-bindings uses into-var when present in src/clysm/lib/macros.lisp
- [ ] T049 [US4] Verify into variable accessible in body (already in scope via let*) in src/clysm/lib/macros.lisp
- [ ] T050 [US4] Verify into variable accessible in finally clause in src/clysm/lib/macros.lisp
- [ ] T051 [US4] Add error detection for conflicting into clauses in parse-loop-clauses in src/clysm/lib/macros.lisp
- [ ] T052 [US4] Verify all US4 tests pass (Red→Green complete)

**Checkpoint**: User Story 4 complete - INTO accumulator works independently

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Validation, regression testing, and success criteria verification

- [ ] T053 Run full existing LOOP test suite to verify no regression (SC-003)
- [ ] T054 [P] Write contract test for Wasm validation of hash-table LOOP in tests/contract/loop/hash-iteration-wasm-test.lisp
- [ ] T055 [P] Write contract test for Wasm validation of WITH clause LOOP in tests/contract/loop/with-wasm-test.lisp
- [ ] T056 [P] Write contract test for Wasm validation of FINALLY clause LOOP in tests/contract/loop/finally-wasm-test.lisp
- [ ] T057 [P] Write contract test for Wasm validation of INTO clause LOOP in tests/contract/loop/into-wasm-test.lisp
- [ ] T058 Run wasm-tools validate on all generated Wasm (SC-004)
- [ ] T059 Test 40 compiler hash-table iteration patterns compile successfully (SC-002)
- [ ] T060 Run ANSI CL iteration test category and verify 50%+ pass rate (SC-001)
- [ ] T061 Compare runtime results with SBCL interpreter for verification (SC-005)
- [ ] T062 Run nix flake check to verify build passes (Constitution VIII)
- [ ] T063 Add HyperSpec links to code comments (Constitution IX)
- [ ] T064 Run quickstart.md validation scenarios

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (P1): Can start after Phase 2
  - US2 (P2): Can start after Phase 2 (parallel with US1)
  - US3 (P2): Can start after Phase 2 (parallel with US1, US2)
  - US4 (P3): Can start after Phase 2 (parallel with US1, US2, US3)
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (Hash-Table)**: Independent - no cross-story dependencies
- **US2 (WITH)**: Independent - no cross-story dependencies
- **US3 (FINALLY)**: Independent - no cross-story dependencies
- **US4 (INTO)**: Mostly independent; T044 tests INTO+FINALLY interaction but doesn't require US3 code changes

### Within Each User Story (TDD Order)

1. Write tests FIRST (T009-T013, T025-T028, T033-T036, T041-T046)
2. Verify tests FAIL (Red state)
3. Implement code (T014-T023, T029-T031, T037-T039, T047-T051)
4. Verify tests PASS (Green state)
5. Mark checkpoint complete

### Parallel Opportunities

**Phase 2 (Foundational)**:
- T004-T008 modify same file - execute sequentially

**Phase 3 (US1 Tests)**:
```bash
# Can run in parallel:
Task: T009 - hash-keys basic test
Task: T010 - hash-values basic test
Task: T011 - hash-keys with using test
Task: T012 - hash-values with using test
Task: T013 - empty hash-table test
```

**Phase 4 (US2 Tests)**:
```bash
# Can run in parallel:
Task: T025 - with basic binding test
Task: T026 - with no initializer test
Task: T027 - sequential bindings test
Task: T028 - parallel bindings test
```

**Phase 7 (Contract Tests)**:
```bash
# Can run in parallel:
Task: T054 - hash-table Wasm validation
Task: T055 - WITH Wasm validation
Task: T056 - FINALLY Wasm validation
Task: T057 - INTO Wasm validation
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (struct extensions)
3. Complete Phase 3: User Story 1 (Hash-Table Iteration)
4. **STOP and VALIDATE**:
   - All US1 tests pass
   - wasm-tools validate passes
   - Sample compiler patterns compile
5. Deploy/demo if ready (enables 40 compiler patterns)

### Incremental Delivery

1. **Setup + Foundational** → Infrastructure ready
2. **Add US1 (Hash-Table)** → Test independently → MVP delivers compiler unblocking
3. **Add US2 (WITH)** → Test independently → Cleaner loop code enabled
4. **Add US3 (FINALLY)** → Test independently → Cleanup patterns enabled
5. **Add US4 (INTO)** → Test independently → Multiple accumulators enabled
6. **Polish** → Full validation → Success criteria verified

### Single Developer Strategy

Execute phases sequentially in priority order:
1. Phase 1 → Phase 2 → Phase 3 (US1) → Phase 4 (US2) → Phase 5 (US3) → Phase 6 (US4) → Phase 7

---

## Notes

- [P] tasks = different files or independent changes, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- TDD required by Constitution VII: write tests first, verify they FAIL, then implement
- All Wasm output must pass wasm-tools validate (Constitution I, VIII)
- Add HyperSpec links per Constitution IX
- Total: 64 tasks across 7 phases
