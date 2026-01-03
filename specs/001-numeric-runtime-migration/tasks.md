# Tasks: Numeric Runtime Migration

**Input**: Design documents from `/specs/001-numeric-runtime-migration/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution VII (TDD Non-negotiable) and FR-013 (minimum 25 tests)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup

**Purpose**: Create the runtime library file structure

- [ ] T001 Create numeric-runtime.lisp file with package declaration and header comments in src/clysm/lib/numeric-runtime.lisp
- [ ] T002 Create numeric-runtime-test.lisp test file with rove test suite in tests/unit/numeric-runtime-test.lisp

---

## Phase 2: Foundational (Runtime Registration)

**Purpose**: Register all 5 functions in `*runtime-function-table*` - BLOCKS all implementation

**⚠️ CRITICAL**: Runtime dispatch must be registered before functions can be called

- [ ] T003 Add `register-numeric-runtime-functions` function in src/clysm/compiler/codegen/func-section.lisp
- [ ] T004 Register parse-integer with `:$parse-integer-rt` (variadic) in func-section.lisp
- [ ] T005 Register write-to-string with `:$write-to-string-rt` (variadic) in func-section.lisp
- [ ] T006 Register rationalize with `:$rationalize-rt` (arity 1) in func-section.lisp
- [ ] T007 Register signum with `:$signum-rt` (arity 1) in func-section.lisp
- [ ] T008 Register phase with `:$phase-rt` (arity 1) in func-section.lisp
- [ ] T009 Call `(register-numeric-runtime-functions)` at module load time in func-section.lisp

**Checkpoint**: All 5 functions now dispatch to runtime library (stubs)

---

## Phase 3: User Story 1 - Compiler Runtime Dispatch (Priority: P1) 🎯 MVP

**Goal**: Compiler emits runtime calls instead of inline Wasm for all 5 numeric functions

**Independent Test**: Compile code calling each function, verify Wasm contains `:call :$*-rt` instructions

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T010 [P] [US1] Test parse-integer basic parsing (positive integer) in tests/unit/numeric-runtime-test.lisp
- [ ] T011 [P] [US1] Test write-to-string basic output (integer to string) in tests/unit/numeric-runtime-test.lisp
- [ ] T012 [P] [US1] Test rationalize passthrough (integer unchanged) in tests/unit/numeric-runtime-test.lisp
- [ ] T013 [P] [US1] Test signum basic sign detection (positive/negative/zero) in tests/unit/numeric-runtime-test.lisp
- [ ] T014 [P] [US1] Test phase for real positive (returns 0) in tests/unit/numeric-runtime-test.lisp

### Implementation for User Story 1

- [ ] T015 [P] [US1] Implement parse-integer stub returning hardcoded value in src/clysm/lib/numeric-runtime.lisp
- [ ] T016 [P] [US1] Implement write-to-string stub returning "0" in src/clysm/lib/numeric-runtime.lisp
- [ ] T017 [P] [US1] Implement rationalize stub returning input in src/clysm/lib/numeric-runtime.lisp
- [ ] T018 [P] [US1] Implement signum stub returning 0 in src/clysm/lib/numeric-runtime.lisp
- [ ] T019 [P] [US1] Implement phase stub returning 0.0 in src/clysm/lib/numeric-runtime.lisp
- [ ] T020 [US1] Verify Stage 1 compilation succeeds with stubs via `sbcl --load build/stage1-complete.lisp`
- [ ] T021 [US1] Verify Wasm validation passes via `wasm-tools validate dist/clysm-stage1.wasm`

**Checkpoint**: All 5 functions compile and dispatch to runtime (stubs), Wasm validates

---

## Phase 4: User Story 2 - ANSI CL Keyword Arguments (Priority: P2)

**Goal**: parse-integer and write-to-string support all ANSI CL keyword arguments

**Independent Test**: Call functions with all keyword combinations, verify correct results

### Tests for User Story 2

- [ ] T022 [P] [US2] Test parse-integer with :radix 2 (binary) in tests/unit/numeric-runtime-test.lisp
- [ ] T023 [P] [US2] Test parse-integer with :radix 16 (hexadecimal) in tests/unit/numeric-runtime-test.lisp
- [ ] T024 [P] [US2] Test parse-integer with :start/:end bounds in tests/unit/numeric-runtime-test.lisp
- [ ] T025 [P] [US2] Test parse-integer with :junk-allowed t in tests/unit/numeric-runtime-test.lisp
- [ ] T026 [P] [US2] Test parse-integer with :junk-allowed nil signals error in tests/unit/numeric-runtime-test.lisp
- [ ] T027 [P] [US2] Test write-to-string with :base 2 (binary) in tests/unit/numeric-runtime-test.lisp
- [ ] T028 [P] [US2] Test write-to-string with :base 16 (hexadecimal) in tests/unit/numeric-runtime-test.lisp
- [ ] T029 [P] [US2] Test write-to-string with :base 36 in tests/unit/numeric-runtime-test.lisp

### Implementation for User Story 2

- [ ] T030 [US2] Implement digit-char-to-weight helper (char + radix → integer) in src/clysm/lib/numeric-runtime.lisp
- [ ] T031 [US2] Implement skip-whitespace helper for parse-integer in src/clysm/lib/numeric-runtime.lisp
- [ ] T032 [US2] Implement full parse-integer with :start :end :radix :junk-allowed in src/clysm/lib/numeric-runtime.lisp
- [ ] T033 [US2] Implement integer-to-digit helper (integer → char for radix) in src/clysm/lib/numeric-runtime.lisp
- [ ] T034 [US2] Implement full write-to-string with :base support in src/clysm/lib/numeric-runtime.lisp

**Checkpoint**: parse-integer and write-to-string fully ANSI compliant with keywords

---

## Phase 5: User Story 3 - Type-Correct Numeric Operations (Priority: P2)

**Goal**: signum, phase, rationalize handle all numeric types correctly

**Independent Test**: Pass values of each numeric type and verify type-congruent results

### Tests for User Story 3

- [ ] T035 [P] [US3] Test signum with integer input returns integer in tests/unit/numeric-runtime-test.lisp
- [ ] T036 [P] [US3] Test signum with float input returns float in tests/unit/numeric-runtime-test.lisp
- [ ] T037 [P] [US3] Test signum with ratio input returns integer in tests/unit/numeric-runtime-test.lisp
- [ ] T038 [P] [US3] Test signum with complex input returns unit complex in tests/unit/numeric-runtime-test.lisp
- [ ] T039 [P] [US3] Test phase with positive real returns 0.0 in tests/unit/numeric-runtime-test.lisp
- [ ] T040 [P] [US3] Test phase with negative real returns pi in tests/unit/numeric-runtime-test.lisp
- [ ] T041 [P] [US3] Test phase with complex in each quadrant in tests/unit/numeric-runtime-test.lisp
- [ ] T042 [P] [US3] Test rationalize with float 0.5 returns 1/2 in tests/unit/numeric-runtime-test.lisp
- [ ] T043 [P] [US3] Test rationalize with integer returns integer unchanged in tests/unit/numeric-runtime-test.lisp
- [ ] T044 [P] [US3] Test rationalize with ratio returns ratio unchanged in tests/unit/numeric-runtime-test.lisp

### Implementation for User Story 3

- [ ] T045 [US3] Implement signum-integer helper in src/clysm/lib/numeric-runtime.lisp
- [ ] T046 [US3] Implement signum-float helper in src/clysm/lib/numeric-runtime.lisp
- [ ] T047 [US3] Implement signum-complex helper (z/|z|) in src/clysm/lib/numeric-runtime.lisp
- [ ] T048 [US3] Implement full signum with type dispatch in src/clysm/lib/numeric-runtime.lisp
- [ ] T049 [US3] Implement phase for real numbers (0 or pi) in src/clysm/lib/numeric-runtime.lisp
- [ ] T050 [US3] Implement phase for complex numbers (atan2) in src/clysm/lib/numeric-runtime.lisp
- [ ] T051 [US3] Implement full phase with type dispatch in src/clysm/lib/numeric-runtime.lisp
- [ ] T052 [US3] Implement continued-fraction helper for rationalize in src/clysm/lib/numeric-runtime.lisp
- [ ] T053 [US3] Implement full rationalize with type dispatch in src/clysm/lib/numeric-runtime.lisp

**Checkpoint**: All functions handle full numeric tower correctly

---

## Phase 6: User Story 4 - Layer 1 Primitive Compliance (Priority: P3)

**Goal**: All runtime functions use only Layer 1 primitives for bootstrap compatibility

**Independent Test**: Code review + Stage 1 compilation succeeds

### Verification for User Story 4

- [ ] T054 [US4] Audit parse-integer for Layer 1 compliance in src/clysm/lib/numeric-runtime.lisp
- [ ] T055 [US4] Audit write-to-string for Layer 1 compliance in src/clysm/lib/numeric-runtime.lisp
- [ ] T056 [US4] Audit rationalize for Layer 1 compliance in src/clysm/lib/numeric-runtime.lisp
- [ ] T057 [US4] Audit signum for Layer 1 compliance in src/clysm/lib/numeric-runtime.lisp
- [ ] T058 [US4] Audit phase for Layer 1 compliance in src/clysm/lib/numeric-runtime.lisp
- [ ] T059 [US4] Regenerate Stage 1 and verify Wasm validation passes

**Checkpoint**: All functions verified Layer 1 compliant, Stage 1 builds

---

## Phase 7: User Story 5 - Comprehensive Unit Test Coverage (Priority: P3)

**Goal**: Each function has 5+ test cases including edge cases

**Independent Test**: Run `sbcl --eval "(asdf:test-system :clysm)"` and verify all pass

### Edge Case Tests for User Story 5

- [ ] T060 [P] [US5] Test parse-integer empty string signals error in tests/unit/numeric-runtime-test.lisp
- [ ] T061 [P] [US5] Test parse-integer whitespace only signals error in tests/unit/numeric-runtime-test.lisp
- [ ] T062 [P] [US5] Test parse-integer negative number parsing in tests/unit/numeric-runtime-test.lisp
- [ ] T063 [P] [US5] Test signum with zero (returns 0) in tests/unit/numeric-runtime-test.lisp
- [ ] T064 [P] [US5] Test signum with -0.0 (IEEE behavior) in tests/unit/numeric-runtime-test.lisp
- [ ] T065 [P] [US5] Test phase with zero (returns 0.0) in tests/unit/numeric-runtime-test.lisp
- [ ] T066 [P] [US5] Test phase with pure imaginary #C(0 1) in tests/unit/numeric-runtime-test.lisp
- [ ] T067 [P] [US5] Test write-to-string with 0 in tests/unit/numeric-runtime-test.lisp
- [ ] T068 [P] [US5] Test write-to-string with negative numbers in tests/unit/numeric-runtime-test.lisp
- [ ] T069 [P] [US5] Test rationalize with 0.0 returns 0 in tests/unit/numeric-runtime-test.lisp
- [ ] T070 [US5] Run full test suite and verify all 33+ tests pass

**Checkpoint**: All edge cases covered, test suite complete

---

## Phase 8: Polish & Validation

**Purpose**: Final verification and documentation

- [ ] T071 Add HyperSpec links to docstrings in src/clysm/lib/numeric-runtime.lisp
- [ ] T072 Update CLAUDE.md with feature completion in CLAUDE.md
- [ ] T073 Run `sbcl --load build/stage1-complete.lisp` final verification
- [ ] T074 Run `wasm-tools validate dist/clysm-stage1.wasm` final validation
- [ ] T075 Verify test count meets FR-013 minimum (25+ tests)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - MVP deliverable
- **User Story 2 (Phase 4)**: Depends on US1 stubs being in place
- **User Story 3 (Phase 5)**: Depends on US1 stubs being in place
- **User Story 4 (Phase 6)**: Depends on US2+US3 implementation complete
- **User Story 5 (Phase 7)**: Depends on all implementations complete
- **Polish (Phase 8)**: Depends on all user stories complete

### User Story Dependencies

| Story | Can Start After | Independent Test |
|-------|-----------------|------------------|
| US1 | Phase 2 (Foundational) | Stage 1 compiles, Wasm validates |
| US2 | US1 (stubs exist) | Keyword argument parsing works |
| US3 | US1 (stubs exist) | Type dispatch correct |
| US4 | US2+US3 complete | Code review + Stage 1 passes |
| US5 | All implementations | All 33+ tests pass |

### Parallel Opportunities

**Within Phase 1 (Setup)**:
```
T001 || T002 (different files)
```

**Within Phase 3 (US1 Tests)**:
```
T010 || T011 || T012 || T013 || T014 (all tests parallelizable)
```

**Within Phase 3 (US1 Implementation)**:
```
T015 || T016 || T017 || T018 || T019 (all stubs parallelizable)
```

**Within Phase 4 (US2 Tests)**:
```
T022 || T023 || T024 || T025 || T026 || T027 || T028 || T029
```

**Within Phase 5 (US3 Tests)**:
```
T035 || T036 || T037 || T038 || T039 || T040 || T041 || T042 || T043 || T044
```

**Within Phase 7 (US5 Edge Cases)**:
```
T060 || T061 || T062 || T063 || T064 || T065 || T066 || T067 || T068 || T069
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (register all 5 functions)
3. Complete Phase 3: User Story 1 (stubs)
4. **STOP and VALIDATE**: `wasm-tools validate dist/clysm-stage1.wasm`
5. Deploy/demo if ready

### Incremental Delivery

1. Setup + Foundational → Registration complete
2. User Story 1 → Stubs work, Wasm validates (MVP!)
3. User Story 2 → Keyword arguments work
4. User Story 3 → Full numeric tower support
5. User Story 4 → Layer 1 compliance verified
6. User Story 5 → All 33+ tests pass

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story should be independently completable
- Verify tests fail before implementing (TDD)
- Commit after each task or logical group
- Total: 75 tasks (33 tests + 42 implementation/verification)
