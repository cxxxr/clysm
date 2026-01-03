# Tasks: Sequence and Array Runtime Migration

**Input**: Design documents from `/specs/001-sequence-array-runtime/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD) and SC-006 (100% test coverage)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

Files modified (from plan.md):
- `src/clysm/lib/sequence-runtime.lisp` - Extend with subseq-rt, adjust-array-rt
- `src/clysm/compiler/codegen/func-section.lisp` - Add registration, remove compile-* functions
- `tests/unit/sequence-array-runtime-test.lisp` - New test file

---

## Phase 1: Setup

**Purpose**: Create test infrastructure for TDD workflow

- [ ] T001 Create test file `tests/unit/sequence-array-runtime-test.lisp` with rove test suite structure
- [ ] T002 [P] Add HyperSpec reference comments for [subseq](resources/HyperSpec/Body/f_subseq.htm) and [adjust-array](resources/HyperSpec/Body/f_adjust.htm) to test file

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Ensure UTF-8 helpers and type predicates are accessible

**⚠️ CRITICAL**: US1 depends on UTF-8 helpers from string-runtime.lisp

- [ ] T003 Verify `utf8-continuation-byte-p` is exported and accessible from `src/clysm/lib/string-runtime.lisp`
- [ ] T004 [P] Verify `decode-utf8-char` is exported and accessible from `src/clysm/lib/string-runtime.lisp`
- [ ] T005 Add helper function `char-position-to-byte-position` in `src/clysm/lib/sequence-runtime.lisp` for UTF-8 index conversion

**Checkpoint**: UTF-8 helpers verified - User Story implementation can begin

---

## Phase 3: User Story 1 - Subseq on Strings with UTF-8 (Priority: P1) 🎯 MVP

**Goal**: Implement subseq-rt for UTF-8 encoded strings with character-position indices

**Independent Test**: Compile `(subseq "日本語" 1)` and verify result is "本語"

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T006 [P] [US1] Write test `test-string-subseq-ascii` for ASCII strings in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T007 [P] [US1] Write test `test-string-subseq-utf8-2byte` for 2-byte UTF-8 chars (é, ñ) in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T008 [P] [US1] Write test `test-string-subseq-utf8-3byte` for 3-byte UTF-8 chars (日本語) in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T009 [P] [US1] Write test `test-string-subseq-utf8-4byte` for 4-byte UTF-8 chars (emoji) in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T010 [P] [US1] Write test `test-string-subseq-empty` for empty result (start=end) in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T011 [P] [US1] Write test `test-string-subseq-bounds-error` for out-of-bounds error in `tests/unit/sequence-array-runtime-test.lisp`

### Implementation for User Story 1

- [ ] T012 [US1] Implement `string-subseq-rt` helper function for string subsequence with UTF-8 support in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T013 [US1] Add string-specific bounds validation in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T014 [US1] Run tests and verify all US1 tests pass

**Checkpoint**: String subseq with UTF-8 works independently - can demo with `(subseq "café" 0 4)`

---

## Phase 4: User Story 2 - Subseq on Lists and Vectors (Priority: P1)

**Goal**: Extend subseq-rt to support lists and vectors in addition to strings

**Independent Test**: Compile `(subseq '(a b c d) 1 3)` and verify result is `(b c)`

### Tests for User Story 2 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T015 [P] [US2] Write test `test-list-subseq-basic` for list subsequence in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T016 [P] [US2] Write test `test-list-subseq-empty` for empty list result in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T017 [P] [US2] Write test `test-list-subseq-whole` for entire list in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T018 [P] [US2] Write test `test-vector-subseq-basic` for vector subsequence in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T019 [P] [US2] Write test `test-vector-subseq-empty` for empty vector result in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T020 [P] [US2] Write test `test-subseq-type-dispatch` verifying correct type detection in `tests/unit/sequence-array-runtime-test.lisp`

### Implementation for User Story 2

- [ ] T021 [P] [US2] Implement `list-subseq-rt` helper function using nthcdr and copy in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T022 [P] [US2] Implement `vector-subseq-rt` helper function using aref and make-array in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T023 [US2] Implement main `subseq-rt` function with type dispatch (stringp/listp/vectorp) in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T024 [US2] Run tests and verify all US1 + US2 tests pass

**Checkpoint**: All sequence types work - subseq-rt is complete

---

## Phase 5: User Story 3 - Adjust-Array Basic Resizing (Priority: P2)

**Goal**: Implement adjust-array-rt for 1D arrays with :initial-element support

**Independent Test**: Compile `(adjust-array #(1 2 3) 5 :initial-element 0)` and verify result is `#(1 2 3 0 0)`

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T025 [P] [US3] Write test `test-adjust-array-grow` for growing array in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T026 [P] [US3] Write test `test-adjust-array-shrink` for shrinking array in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T027 [P] [US3] Write test `test-adjust-array-same-size` for same size in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T028 [P] [US3] Write test `test-adjust-array-initial-element` for :initial-element fill in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T029 [P] [US3] Write test `test-adjust-array-list-dims` for dimensions as list in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T030 [P] [US3] Write test `test-adjust-array-multidim-error` for multidimensional array error in `tests/unit/sequence-array-runtime-test.lisp`

### Implementation for User Story 3

- [ ] T031 [US3] Implement `adjust-array-rt` function with dimension extraction in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T032 [US3] Add element copy logic preserving existing elements in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T033 [US3] Add :initial-element fill logic for new slots in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T034 [US3] Add 1D array validation (error for multidimensional) in `src/clysm/lib/sequence-runtime.lisp`
- [ ] T035 [US3] Run tests and verify all US3 tests pass

**Checkpoint**: adjust-array-rt complete - 1D array resizing works

---

## Phase 6: User Story 4 - Runtime Dispatch Integration (Priority: P2)

**Goal**: Register functions in `*runtime-function-table*` and verify compiler dispatch

**Independent Test**: Verify `(gethash 'subseq clysm::*runtime-function-table*)` returns `(:$subseq-rt)`

### Tests for User Story 4 ⚠️

- [ ] T036 [P] [US4] Write test `test-subseq-registration` verifying runtime table entry in `tests/unit/sequence-array-runtime-test.lisp`
- [ ] T037 [P] [US4] Write test `test-adjust-array-registration` verifying runtime table entry in `tests/unit/sequence-array-runtime-test.lisp`

### Implementation for User Story 4

- [ ] T038 [US4] Add `register-sequence-array-runtime-functions` function in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T039 [US4] Call registration function at module load time in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T040 [US4] Run Stage 1 compilation with `sbcl --load build/stage1-complete.lisp`
- [ ] T041 [US4] Validate generated Wasm with `wasm-tools validate dist/clysm-stage1.wasm`
- [ ] T042 [US4] Run tests and verify all US4 tests pass

**Checkpoint**: Runtime dispatch works - compiler uses `*runtime-function-table*` for subseq and adjust-array

---

## Phase 7: User Story 5 - Dead Code Removal (Priority: P3)

**Goal**: Remove ~175 lines of compile-subseq and compile-adjust-array from func-section.lisp

**Independent Test**: `grep -c "compile-subseq\|compile-adjust-array" src/clysm/compiler/codegen/func-section.lisp` returns 0

### Implementation for User Story 5

- [ ] T043 [US5] Remove `compile-subseq` function (~115 lines) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T044 [US5] Remove `compile-adjust-array` function (~60 lines) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T045 [US5] Remove any helper functions used only by removed code in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T046 [US5] Remove case entries for subseq and adjust-array from compile-funcall in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T047 [US5] Verify line count reduction (~175 lines) in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T048 [US5] Run full test suite with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: Dead code removed - func-section.lisp is cleaner

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and documentation

- [ ] T049 Verify SC-001: Runtime library implementation < 300 lines
- [ ] T050 Verify SC-002: func-section.lisp reduced by ~175 lines
- [ ] T051 [P] Run Stage 1 compilation and Wasm validation (final verification)
- [ ] T052 [P] Verify SC-008: No regression in existing sequence-runtime.lisp tests
- [ ] T053 Run quickstart.md validation steps
- [ ] T054 Update CLAUDE.md Recent Changes section with feature summary

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational - MVP for strings
- **US2 (Phase 4)**: Depends on Foundational - can parallel with US1 (different helpers)
- **US3 (Phase 5)**: Depends on Foundational - independent of US1/US2
- **US4 (Phase 6)**: Depends on US1, US2, US3 completion (needs all functions implemented)
- **US5 (Phase 7)**: Depends on US4 completion (verify dispatch works before removing old code)
- **Polish (Phase 8)**: Depends on US5 completion

### User Story Dependencies

```
Phase 1 (Setup)
    │
    ▼
Phase 2 (Foundational)
    │
    ├──────────────┬──────────────┐
    ▼              ▼              ▼
Phase 3 (US1)  Phase 4 (US2)  Phase 5 (US3)
  Strings      Lists/Vectors    Arrays
    │              │              │
    └──────────────┴──────────────┘
                   │
                   ▼
            Phase 6 (US4)
            Registration
                   │
                   ▼
            Phase 7 (US5)
            Dead Code Removal
                   │
                   ▼
            Phase 8 (Polish)
```

### Parallel Opportunities

**Phase 3-4-5 Parallelization** (after Foundational):
- US1 (strings), US2 (lists/vectors), US3 (arrays) can run in parallel
- Different helper functions, no file conflicts
- T012-T014 || T021-T024 || T031-T035

**Within Each Story**:
- All tests marked [P] can run in parallel (same file, different test functions)
- T006-T011 (US1 tests) can all be written simultaneously
- T015-T020 (US2 tests) can all be written simultaneously
- T025-T030 (US3 tests) can all be written simultaneously

---

## Parallel Example: User Stories 1-3 After Foundational

```bash
# After Phase 2 completes, launch all three story implementations:

# Stream 1: US1 - String subseq
Task: "Write test test-string-subseq-ascii in tests/unit/sequence-array-runtime-test.lisp"
Task: "Implement string-subseq-rt in src/clysm/lib/sequence-runtime.lisp"

# Stream 2: US2 - List/Vector subseq
Task: "Write test test-list-subseq-basic in tests/unit/sequence-array-runtime-test.lisp"
Task: "Implement list-subseq-rt in src/clysm/lib/sequence-runtime.lisp"

# Stream 3: US3 - Adjust-array
Task: "Write test test-adjust-array-grow in tests/unit/sequence-array-runtime-test.lisp"
Task: "Implement adjust-array-rt in src/clysm/lib/sequence-runtime.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (String subseq with UTF-8)
4. **STOP and VALIDATE**: Test `(subseq "日本語" 1)` independently
5. Can demo UTF-8 string handling at this point

### Incremental Delivery

1. Setup + Foundational → Infrastructure ready
2. US1 (strings) → UTF-8 subseq works → Demo
3. US2 (lists/vectors) → Full subseq-rt complete → Demo
4. US3 (adjust-array) → adjust-array-rt complete → Demo
5. US4 (registration) → Compiler integration verified
6. US5 (cleanup) → Dead code removed, feature complete

---

## Notes

- [P] tasks = different files or independent test functions, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD workflow: Write tests first, verify they fail, then implement
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
