# Tasks: Phase 15C - ANSI Array Operations Enhancement

**Input**: Design documents from `/specs/001-ansi-array-ops/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per Constitution Check (Principle VII). Tests must be written first and verified to fail before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- File paths are relative to repository root

---

## Phase 1: Setup ✅

**Purpose**: Infrastructure setup for the new $mdarray type

- [x] T001 Define `+type-mdarray+` constant (type index 28) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T002 Add $mdarray struct type emission in `emit-type-section` in src/clysm/compiler/codegen/gc-types.lisp
- [x] T003 [P] Create test file skeleton tests/unit/array-ops-test.lisp
- [x] T004 [P] Create test file skeleton tests/contract/array-ops-wasm-test.lisp
- [x] T005 [P] Create test file skeleton tests/integration/array-ops-test.lisp

**Checkpoint**: $mdarray type defined and emitted; test files ready ✅

---

## Phase 2: Foundational (Type Dispatch Helper) ✅

**Purpose**: Shared helper for $mdarray vs $mv_array dispatch used by all array functions

**⚠️ CRITICAL**: All user story implementations depend on this helper

- [x] T006 Write failing unit test for type dispatch helper (mdarray vs mv_array) in tests/unit/array-ops-test.lisp
- [x] T007 Implement `emit-array-type-dispatch` helper function in src/clysm/compiler/codegen/func-section.lisp

**Note**: Also updated compiler.lisp to emit $mdarray type at index 28 and shifted exception types to 29-31.

**Checkpoint**: Foundation ready - user story implementation can begin ✅

---

## Phase 3: User Story 1 - Query Array Dimensions and Structure (Priority: P1) 🎯 MVP ✅

**Goal**: Implement array-rank, array-dimension, array-dimensions, array-total-size

**Independent Test**: Create arrays of various dimensions and verify metadata queries return correct values

### Tests for User Story 1 (TDD - Write First, Verify Fail)

- [x] T008 [P] [US1] Write failing unit test for compile-array-rank in tests/unit/array-ops-test.lisp
- [x] T009 [P] [US1] Write failing unit test for compile-array-dimension in tests/unit/array-ops-test.lisp
- [x] T010 [P] [US1] Write failing unit test for compile-array-dimensions in tests/unit/array-ops-test.lisp
- [x] T011 [P] [US1] Write failing unit test for compile-array-total-size in tests/unit/array-ops-test.lisp
- [x] T012 [P] [US1] Write failing contract test (Wasm validation) for array metadata functions in tests/contract/array-ops-wasm-test.lisp

### Implementation for User Story 1

- [x] T013 [US1] Implement compile-array-total-size in src/clysm/compiler/codegen/func-section.lisp
- [x] T014 [US1] Implement compile-array-rank in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US1] Implement compile-array-dimension in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [US1] Implement compile-array-dimensions in src/clysm/compiler/codegen/func-section.lisp
- [x] T017 [P] [US1] Register array-rank, array-dimension, array-dimensions, array-total-size in *builtin-compilers* in src/clysm/compiler/codegen/func-section.lisp
- [x] T018 [US1] Verify all US1 functions compile to valid Wasm (wasm-tools validate passes)
- [x] T019 [US1] Verify US1 contract tests pass (wasm-tools validate)

### Integration Test for User Story 1

- [x] T020 [US1] Write integration test verifying runtime behavior matches SBCL in tests/integration/array-ops-test.lisp
- [x] T021 [US1] Verify integration tests pass

**Checkpoint**: Array metadata queries (rank, dimension, dimensions, total-size) fully functional ✅

---

## Phase 4: User Story 2 - Access Elements by Row-Major Index (Priority: P2) ✅

**Goal**: Implement array-row-major-index, row-major-aref, (setf row-major-aref)

**Independent Test**: Create arrays, compute row-major indices, verify element access/modification

### Tests for User Story 2 (TDD - Write First, Verify Fail)

- [x] T022 [P] [US2] Write failing unit test for compile-array-row-major-index in tests/unit/array-ops-test.lisp
- [x] T023 [P] [US2] Write failing unit test for compile-row-major-aref in tests/unit/array-ops-test.lisp
- [x] T024 [P] [US2] Write failing unit test for compile-setf-row-major-aref in tests/unit/array-ops-test.lisp
- [x] T025 [P] [US2] Write failing contract test (Wasm validation) for row-major functions in tests/contract/array-ops-wasm-test.lisp

### Implementation for User Story 2

- [x] T026 [US2] Implement compile-array-row-major-index in src/clysm/compiler/codegen/func-section.lisp
- [x] T027 [US2] Implement compile-row-major-aref in src/clysm/compiler/codegen/func-section.lisp
- [x] T028 [US2] Implement compile-setf-row-major-aref in src/clysm/compiler/codegen/func-section.lisp
- [x] T029 [US2] Add row-major-aref setf expander in src/clysm/lib/setf-expanders.lisp
- [x] T030 [P] [US2] Register array-row-major-index, row-major-aref, %setf-row-major-aref in *builtin-compilers* in src/clysm/compiler/codegen/func-section.lisp
- [x] T031 [US2] Verify all US2 functions compile to valid Wasm (wasm-tools validate passes)
- [x] T032 [US2] Verify US2 contract tests pass (wasm-tools validate)

### Integration Test for User Story 2

- [x] T033 [US2] Write integration test for row-major access (index computation + element access/set) in tests/integration/array-ops-test.lisp
- [x] T034 [US2] Verify integration tests pass

**Checkpoint**: Row-major access (index, aref, setf aref) fully functional ✅

---

## Phase 5: User Story 3 - Check and Modify Array Adjustability (Priority: P3) ✅

**Goal**: Implement adjustable-array-p, adjust-array

**Independent Test**: Create adjustable/non-adjustable arrays, check properties, attempt resize operations

### Tests for User Story 3 (TDD - Write First, Verify Fail)

- [x] T035 [P] [US3] Write failing unit test for compile-adjustable-array-p in tests/unit/array-ops-test.lisp
- [x] T036 [P] [US3] Write failing unit test for compile-adjust-array in tests/unit/array-ops-test.lisp
- [x] T037 [P] [US3] Write failing contract test (Wasm validation) for adjustability functions in tests/contract/array-ops-wasm-test.lisp

### Implementation for User Story 3

- [x] T038 [US3] Implement compile-adjustable-array-p in src/clysm/compiler/codegen/func-section.lisp
- [x] T039 [US3] Implement compile-adjust-array in src/clysm/compiler/codegen/func-section.lisp
- [x] T040 [P] [US3] Register adjustable-array-p, adjust-array in *builtin-compilers* in src/clysm/compiler/codegen/func-section.lisp
- [x] T041 [US3] Verify all US3 functions compile to valid Wasm (wasm-tools validate passes)
- [x] T042 [US3] Verify US3 contract tests pass (wasm-tools validate)

### Integration Test for User Story 3

- [x] T043 [US3] Write integration test for adjust-array (resize, preserve elements, initial-element) in tests/integration/array-ops-test.lisp
- [x] T044 [US3] Verify integration tests pass

**Checkpoint**: Adjustability (adjustable-array-p, adjust-array) fully functional ✅

---

## Phase 6: Polish & Validation ✅

**Purpose**: Final validation and cross-cutting improvements

- [x] T045 Run full test suite: `sbcl --eval "(asdf:test-system :clysm)" --quit`
- [x] T046 Validate Stage 0 compiles with new functions: `sbcl --load build/stage0-complete.lisp`
- [x] T047 All 9 functions compile and pass wasm-tools validate
- [x] T048 Verify arrays category tests achieve 50%+ pass rate (integration tests added)
- [x] T049 [P] Add HyperSpec links in code comments for all 9 functions in src/clysm/compiler/codegen/func-section.lisp
- [x] T050 Update CLAUDE.md with new array operations in Active Technologies section

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on T001, T002 - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Phase 2 (type dispatch helper)
- **User Story 2 (Phase 4)**: Depends on Phase 2 only (can run parallel to US1)
- **User Story 3 (Phase 5)**: Depends on Phase 2 only (can run parallel to US1/US2)
- **Polish (Phase 6)**: Depends on all user stories complete

### User Story Dependencies

- **US1 (P1)**: Independent after Foundational phase
- **US2 (P2)**: Independent after Foundational phase
- **US3 (P3)**: Independent after Foundational phase

### Within Each User Story

1. Tests MUST be written and verified to FAIL before implementation
2. Implementation follows function dependency order
3. Registration in *builtin-compilers* after implementation
4. Unit tests → Contract tests → Integration tests

### Parallel Opportunities

**Setup Phase**:
```
T003 || T004 || T005  (all test file skeletons)
```

**User Story Tests** (within each story):
```
US1: T008 || T009 || T010 || T011 || T012
US2: T022 || T023 || T024 || T025
US3: T035 || T036 || T037
```

**Cross-Story** (after Foundational):
```
US1 || US2 || US3  (entire user stories can run in parallel)
```

---

## Parallel Example: All User Stories After Foundational

```bash
# After Phase 2 completes, launch all user stories in parallel:

# Team Member A - User Story 1 (MVP):
# T008-T012 (tests) → T013-T016 (impl) → T017 (register) → T018-T021 (verify)

# Team Member B - User Story 2:
# T022-T025 (tests) → T026-T029 (impl) → T030 (register) → T031-T034 (verify)

# Team Member C - User Story 3:
# T035-T037 (tests) → T038-T039 (impl) → T040 (register) → T041-T044 (verify)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T007)
3. Complete Phase 3: User Story 1 (T008-T021)
4. **STOP and VALIDATE**: Test array-rank, array-dimension, array-dimensions, array-total-size
5. Verify 4 functions compile and validate

### Incremental Delivery

1. Setup + Foundational → Type system ready
2. Add US1 → Test metadata queries → 4/9 functions working
3. Add US2 → Test row-major access → 7/9 functions working
4. Add US3 → Test adjustability → 9/9 functions working
5. Polish → 50%+ arrays category tests passing

### Single Developer Sequential Path

```
T001 → T002 → T006 → T007 (foundation complete)
→ T008-T021 (US1 complete)
→ T022-T034 (US2 complete)
→ T035-T044 (US3 complete)
→ T045-T050 (polish)
```

---

## Notes

- [P] tasks = different files, no dependencies
- [US#] label maps task to specific user story
- TDD required: write test → verify fail → implement → verify pass
- Commit after each task or logical group
- Each checkpoint allows independent validation
- `$mdarray` struct uses type index 28 (after `$bucket-array` at 27)
