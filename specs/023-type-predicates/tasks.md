# Tasks: ANSI CL Type Predicates and Numeric Predicates

**Input**: Design documents from `/specs/023-type-predicates/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: TDD required per Constitution VII. Tests written first, verified to fail, then implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Exact file paths included in descriptions

## Path Conventions

- **Source**: `src/clysm/compiler/codegen/func-section.lisp`
- **Tests**: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Test infrastructure and file preparation

- [x] T001 Create test package definition for type-predicates in tests/package.lisp
- [x] T002 [P] Create tests/unit/type-predicates-test.lisp with package header
- [x] T003 [P] Create tests/contract/predicates-wasm-test.lisp with package header
- [x] T004 [P] Create tests/integration/ansi-predicates-test.lisp with package header
- [x] T005 Add test files to clysm.asd test system definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Verify existing infrastructure supports predicate implementation

**⚠️ CRITICAL**: Verify type indices and helper functions exist before implementation

- [x] T006 Verify +type-bignum+, +type-ratio+, +type-float+, +type-complex+ in src/clysm/compiler/codegen/gc-types.lisp
- [x] T007 Verify +type-symbol+, +type-closure+ indices in src/clysm/compiler/codegen/gc-types.lisp
- [x] T008 Verify emit-is-fixnum, emit-is-bignum, emit-is-ratio, emit-is-float, emit-is-complex in src/clysm/compiler/codegen/numeric-runtime.lisp
- [x] T009 Verify T/NIL representation pattern in compile-consp (func-section.lisp:~1261)
- [x] T010 Run nix flake check to ensure clean baseline

**Checkpoint**: Foundation verified - user story implementation can now begin

---

## Phase 3: User Story 1 - Type Checking Predicates (Priority: P1) 🎯 MVP

**Goal**: Implement 8 type predicates (integerp, numberp, floatp, rationalp, complexp, symbolp, functionp, characterp)

**Independent Test**: Each predicate can be tested standalone with `(compile-and-run '(predicate value))`

### Tests for User Story 1

> **TDD**: Write tests FIRST, verify they FAIL, then implement

- [x] T011 [P] [US1] Write unit tests for integerp in tests/unit/type-predicates-test.lisp (fixnum→T, float→NIL, bignum→T)
- [x] T012 [P] [US1] Write unit tests for floatp in tests/unit/type-predicates-test.lisp (float→T, integer→NIL)
- [x] T013 [P] [US1] Write unit tests for rationalp in tests/unit/type-predicates-test.lisp (ratio→T, float→NIL)
- [x] T014 [P] [US1] Write unit tests for complexp in tests/unit/type-predicates-test.lisp (complex→T, real→NIL)
- [x] T015 [P] [US1] Write unit tests for numberp in tests/unit/type-predicates-test.lisp (all numeric types→T, symbol→NIL)
- [x] T016 [P] [US1] Write unit tests for symbolp in tests/unit/type-predicates-test.lisp (symbol→T, number→NIL, NIL→T)
- [x] T017 [P] [US1] Write unit tests for functionp in tests/unit/type-predicates-test.lisp (closure→T, symbol→NIL)
- [x] T018 [P] [US1] Write unit tests for characterp in tests/unit/type-predicates-test.lisp (char→T, integer→NIL)
- [x] T019 [US1] Run tests to verify they FAIL (predicates not yet implemented)

### Implementation for User Story 1

- [x] T020 [P] [US1] Implement compile-integerp in src/clysm/compiler/codegen/func-section.lisp (fixnum OR bignum check)
- [x] T021 [P] [US1] Implement compile-floatp in src/clysm/compiler/codegen/func-section.lisp (single type check)
- [x] T022 [P] [US1] Implement compile-rationalp in src/clysm/compiler/codegen/func-section.lisp (fixnum OR bignum OR ratio)
- [x] T023 [P] [US1] Implement compile-complexp in src/clysm/compiler/codegen/func-section.lisp (single type check)
- [x] T024 [US1] Implement compile-numberp in src/clysm/compiler/codegen/func-section.lisp (union of all numeric types)
- [x] T025 [P] [US1] Implement compile-symbolp in src/clysm/compiler/codegen/func-section.lisp (symbol check + NIL special case)
- [x] T026 [P] [US1] Implement compile-functionp in src/clysm/compiler/codegen/func-section.lisp (closure check)
- [x] T027 [P] [US1] Implement compile-characterp in src/clysm/compiler/codegen/func-section.lisp (i31 check - temporary)
- [x] T028 [US1] Register all 8 predicates in compile-primitive-call dispatcher in func-section.lisp
- [x] T029 [US1] Run unit tests to verify they PASS
- [x] T030 [US1] Validate Wasm output with wasm-tools validate for each predicate

**Checkpoint**: User Story 1 complete - 8 type predicates functional and tested

---

## Phase 4: User Story 2 - Numeric Predicates (Priority: P1)

**Goal**: Implement 5 numeric predicates (zerop, plusp, minusp, oddp, evenp)

**Independent Test**: Each predicate can be tested standalone with numeric arguments

### Tests for User Story 2

- [x] T031 [P] [US2] Write unit tests for zerop in tests/unit/type-predicates-test.lisp (0→T, 1→NIL, 0.0→T, -0.0→T)
- [x] T032 [P] [US2] Write unit tests for plusp in tests/unit/type-predicates-test.lisp (5→T, -3→NIL, 0→NIL)
- [x] T033 [P] [US2] Write unit tests for minusp in tests/unit/type-predicates-test.lisp (-5→T, 3→NIL, 0→NIL)
- [x] T034 [P] [US2] Write unit tests for oddp in tests/unit/type-predicates-test.lisp (7→T, 8→NIL)
- [x] T035 [P] [US2] Write unit tests for evenp in tests/unit/type-predicates-test.lisp (8→T, 7→NIL)
- [x] T036 [US2] Run tests to verify they FAIL (predicates not yet implemented)

### Implementation for User Story 2

- [x] T037 [US2] Implement compile-zerop in src/clysm/compiler/codegen/func-section.lisp (multi-type dispatch, == 0)
- [x] T038 [US2] Implement compile-plusp in src/clysm/compiler/codegen/func-section.lisp (multi-type dispatch, > 0)
- [x] T039 [US2] Implement compile-minusp in src/clysm/compiler/codegen/func-section.lisp (multi-type dispatch, < 0)
- [x] T040 [US2] Implement compile-oddp in src/clysm/compiler/codegen/func-section.lisp (integer-only, mod 2 != 0)
- [x] T041 [US2] Implement compile-evenp in src/clysm/compiler/codegen/func-section.lisp (integer-only, mod 2 == 0)
- [x] T042 [US2] Register all 5 predicates in compile-primitive-call dispatcher in func-section.lisp
- [x] T043 [US2] Run unit tests to verify they PASS
- [x] T044 [US2] Validate Wasm output with wasm-tools validate for each predicate

**Checkpoint**: User Story 2 complete - 5 numeric predicates functional and tested

---

## Phase 5: User Story 3 - Signum Function (Priority: P2)

**Goal**: Implement signum with type-preserving semantics

**Independent Test**: `(signum -42)` → `-1`, `(signum 2.5)` → `1.0`

### Tests for User Story 3

- [x] T045 [P] [US3] Write unit tests for signum integers in tests/unit/type-predicates-test.lisp (-42→-1, 0→0, 100→1)
- [x] T046 [P] [US3] Write unit tests for signum floats in tests/unit/type-predicates-test.lisp (-3.14→-1.0, 0.0→0.0, 2.5→1.0)
- [x] T047 [US3] Run tests to verify they FAIL (signum not yet implemented)

### Implementation for User Story 3

- [x] T048 [US3] Implement compile-signum in src/clysm/compiler/codegen/func-section.lisp (type-preserving dispatch)
- [x] T049 [US3] Register signum in compile-primitive-call dispatcher in func-section.lisp
- [x] T050 [US3] Run unit tests to verify they PASS
- [x] T051 [US3] Validate Wasm output with wasm-tools validate

**Checkpoint**: User Story 3 complete - signum functional and tested

---

## Phase 6: User Story 4 - ANSI Test Compatibility (Priority: P1)

**Goal**: Verify ANSI test pass rate improvement (numbers ≥10%, cons ≥5%)

**Independent Test**: Run ANSI test suite and compare pass rates

### Tests for User Story 4

- [x] T052 [US4] Write integration test to compile and run all 14 predicates via wasmtime in tests/integration/ansi-predicates-test.lisp
- [x] T053 [US4] Write contract tests to validate Wasm module structure in tests/contract/predicates-wasm-test.lisp

### Verification for User Story 4

- [x] T054 [US4] Run ANSI test suite numbers category and record pass rate
- [x] T055 [US4] Run ANSI test suite cons category and record pass rate
- [x] T056 [US4] Verify numbers pass rate ≥10% (SC-001)
- [x] T057 [US4] Verify cons pass rate ≥5% (SC-002)

**Checkpoint**: User Story 4 complete - ANSI test improvements verified

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and documentation

- [x] T058 Run nix flake check to verify all tests pass (SC-005)
- [x] T059 [P] Verify each predicate executes in <1ms (SC-006)
- [x] T060 [P] Update specs/023-type-predicates/validation.md with test results
- [x] T061 Review and commit all changes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - MVP, start first
- **User Story 2 (Phase 4)**: Depends on Foundational - can parallel with US1
- **User Story 3 (Phase 5)**: Depends on Foundational - can parallel with US1/US2
- **User Story 4 (Phase 6)**: Depends on US1, US2, US3 completion
- **Polish (Phase 7)**: Depends on all user stories

### User Story Dependencies

- **User Story 1 (Type Predicates)**: Independent, no cross-story dependencies
- **User Story 2 (Numeric Predicates)**: Independent, no cross-story dependencies
- **User Story 3 (Signum)**: Independent, no cross-story dependencies
- **User Story 4 (ANSI Tests)**: Depends on US1 + US2 + US3 for full validation

### Within Each User Story (TDD Flow)

1. Write tests first (all [P] tests can run in parallel)
2. Run tests → verify FAIL
3. Implement predicates (simple types [P], compound types sequential)
4. Register in dispatcher
5. Run tests → verify PASS
6. Validate Wasm output

### Parallel Opportunities

**Phase 1 (Setup)**: T002, T003, T004 can run in parallel

**Phase 3 (US1)**:
- All test tasks T011-T018 can run in parallel
- Simple predicates T020-T023, T025-T027 can run in parallel
- T024 (numberp) depends on simpler numeric predicates

**Phase 4 (US2)**:
- All test tasks T031-T035 can run in parallel
- Implementation is sequential (similar dispatch patterns)

**Phase 5 (US3)**:
- Test tasks T045-T046 can run in parallel

**Cross-Story Parallelism**:
- US1, US2, US3 can be implemented in parallel by different developers
- All three stories are independent until US4

---

## Parallel Example: User Story 1

```bash
# Launch all US1 tests in parallel:
Task: "Write unit tests for integerp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for floatp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for rationalp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for complexp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for numberp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for symbolp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for functionp in tests/unit/type-predicates-test.lisp"
Task: "Write unit tests for characterp in tests/unit/type-predicates-test.lisp"

# Launch simple type predicates in parallel:
Task: "Implement compile-integerp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-floatp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-rationalp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-complexp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-symbolp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-functionp in src/clysm/compiler/codegen/func-section.lisp"
Task: "Implement compile-characterp in src/clysm/compiler/codegen/func-section.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational verification
3. Complete Phase 3: User Story 1 (8 type predicates)
4. **STOP and VALIDATE**: All type predicates work in wasmtime
5. Can run some ANSI tests immediately

### Incremental Delivery

1. Setup + Foundational → Infrastructure ready
2. Add User Story 1 (type predicates) → Test → 8 new functions
3. Add User Story 2 (numeric predicates) → Test → 5 new functions
4. Add User Story 3 (signum) → Test → 1 new function
5. Run User Story 4 (ANSI tests) → Verify pass rate targets

### Parallel Team Strategy

With multiple developers:

1. Team verifies Foundational together
2. Once verified:
   - Developer A: User Story 1 (type predicates)
   - Developer B: User Story 2 (numeric predicates)
   - Developer C: User Story 3 (signum)
3. All converge for User Story 4 (ANSI validation)

---

## Notes

- [P] tasks = different files or independent sections, no dependencies
- [Story] label maps task to user story for traceability
- TDD required: tests MUST fail before implementation
- Each predicate follows same pattern: compile-XXX function + dispatcher registration
- Wasm validation required for each predicate
- Commit after each completed predicate or logical group
