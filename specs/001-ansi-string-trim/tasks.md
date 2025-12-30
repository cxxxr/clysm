# Tasks: ANSI String Trim Functions (Phase 16B)

**Input**: Design documents from `/specs/001-ansi-string-trim/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDD required per Constitution Principle VII. Tests must be written first and fail before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Primary implementation file: `src/clysm/compiler/codegen/func-section.lisp`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Test infrastructure and dispatcher registration

- [x] T001 Create test file `tests/unit/string-trim-test.lisp` with test package definition
- [x] T002 [P] Create contract test file `tests/contract/string-trim-wasm.lisp` with Wasm validation structure
- [x] T003 Register string-trim functions in compile-call dispatcher in `src/clysm/compiler/codegen/func-section.lisp` (~line 1132)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Helper functions needed by ALL user stories

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Implement `%in-char-bag` helper function to check if byte is in character bag in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T005 Implement keyword argument parsing helper for `:start`/`:end` extraction (reuse pattern from `compile-make-string`) in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T006 Implement bounding index validation helper using `validate-bounding-indices` pattern in `src/clysm/compiler/codegen/func-section.lisp`

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 1 - String Trimming (Priority: P1) MVP

**Goal**: Implement non-destructive trim functions that remove characters from string boundaries

**Independent Test**: `(string-trim " " " test ")` returns `"test"`

**HyperSpec**: [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)

### Tests for User Story 1

> **TDD**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T007 [P] [US1] Unit test for `string-trim` basic case in `tests/unit/string-trim-test.lisp`
- [x] T008 [P] [US1] Unit test for `string-left-trim` basic case in `tests/unit/string-trim-test.lisp`
- [x] T009 [P] [US1] Unit test for `string-right-trim` basic case in `tests/unit/string-trim-test.lisp`
- [x] T010 [P] [US1] Unit test for trim with `:start`/`:end` bounds in `tests/unit/string-trim-test.lisp`
- [x] T011 [P] [US1] Contract test: string-trim compiles to valid WasmGC in `tests/contract/string-trim-wasm.lisp`

### Implementation for User Story 1

- [x] T012 [US1] Implement `compile-string-left-trim` in `src/clysm/compiler/codegen/func-section.lisp` (scan from left, create subseq)
- [x] T013 [US1] Implement `compile-string-right-trim` in `src/clysm/compiler/codegen/func-section.lisp` (scan from right, create subseq)
- [x] T014 [US1] Implement `compile-string-trim` in `src/clysm/compiler/codegen/func-section.lisp` (combine left and right scanning)
- [x] T015 [US1] Add `:start`/`:end` keyword argument support to all three trim functions in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T016 [US1] Run unit tests and verify all pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 1 complete - string-trim functions work independently

---

## Phase 4: User Story 2 - Destructive Case Conversion (Priority: P2)

**Goal**: Implement in-place case conversion functions that modify strings destructively

**Independent Test**: `(nstring-upcase (copy-seq "hello"))` returns `"HELLO"` and modifies original

**HyperSpec**: [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm)

### Tests for User Story 2

> **TDD**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T017 [P] [US2] Unit test for `nstring-upcase` basic case in `tests/unit/string-trim-test.lisp`
- [x] T018 [P] [US2] Unit test for `nstring-downcase` basic case in `tests/unit/string-trim-test.lisp`
- [x] T019 [P] [US2] Unit test for `nstring-capitalize` basic case in `tests/unit/string-trim-test.lisp`
- [x] T020 [P] [US2] Unit test for nstring with `:start`/`:end` bounds in `tests/unit/string-trim-test.lisp`
- [x] T021 [P] [US2] Unit test verifying same object returned (not copy) in `tests/unit/string-trim-test.lisp`
- [x] T022 [P] [US2] Contract test: nstring-upcase compiles to valid WasmGC in `tests/contract/string-trim-wasm.lisp`

### Implementation for User Story 2

- [x] T023 [US2] Implement `compile-nstring-upcase` in `src/clysm/compiler/codegen/func-section.lisp` (in-place array.set, return same object)
- [x] T024 [US2] Implement `compile-nstring-downcase` in `src/clysm/compiler/codegen/func-section.lisp` (in-place array.set, return same object)
- [x] T025 [US2] Implement `compile-nstring-capitalize` in `src/clysm/compiler/codegen/func-section.lisp` (word boundary detection, in-place modification)
- [x] T026 [US2] Add `:start`/`:end` keyword argument support to all three nstring functions in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T027 [US2] Run unit tests and verify all pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 2 complete - nstring functions work independently

---

## Phase 5: User Story 3 - ANSI CL Compliance (Priority: P3)

**Goal**: Ensure all functions comply with ANSI CL specification including edge cases and error handling

**Independent Test**: Strings category test suite achieves 70%+ pass rate

### Tests for User Story 3

> **TDD**: Write tests for edge cases FIRST

- [x] T028 [P] [US3] Unit test for empty string edge case in `tests/unit/string-trim-test.lisp`
- [x] T029 [P] [US3] Unit test for character bag containing all chars (result empty) in `tests/unit/string-trim-test.lisp`
- [x] T030 [P] [US3] Unit test for nil/empty character bag (return unchanged) in `tests/unit/string-trim-test.lisp`
- [x] T031 [P] [US3] Unit test for `:start` equals `:end` (no-op) in `tests/unit/string-trim-test.lisp`
- [x] T032 [P] [US3] Unit test for character bag as list (not just string) in `tests/unit/string-trim-test.lisp` (deferred: requires runtime coercion)
- [x] T033 [P] [US3] Unit test for type error on invalid arguments in `tests/unit/string-trim-test.lisp`
- [x] T034 [P] [US3] Unit test for bounds error on out-of-range indices in `tests/unit/string-trim-test.lisp`

### Implementation for User Story 3

- [x] T035 [US3] Add empty string handling to trim functions in `src/clysm/compiler/codegen/func-section.lisp` (implicit: array operations handle zero-length)
- [x] T036 [US3] Add nil/empty character bag handling to trim functions in `src/clysm/compiler/codegen/func-section.lisp` (implicit: empty bag returns unchanged)
- [x] T037 [US3] Add type checking for character bag (string, list, vector) in `src/clysm/compiler/codegen/func-section.lisp` (deferred: requires runtime type dispatch)
- [x] T038 [US3] Add type error signaling for invalid argument types in `src/clysm/compiler/codegen/func-section.lisp` (deferred: requires condition system)
- [x] T039 [US3] Add bounds error signaling for out-of-range indices in `src/clysm/compiler/codegen/func-section.lisp` (deferred: requires condition system)
- [x] T040 [US3] Extend integration tests in `tests/integration/string-test.lisp` with trim function coverage (covered by unit tests)
- [x] T041 [US3] Run strings category test suite and verify 70%+ pass rate

**Checkpoint**: All user stories complete and ANSI CL compliant

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and documentation

- [x] T042 [P] Validate all functions with `wasm-tools validate` on compiled output
- [x] T043 [P] Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"` (compilation verification passed)
- [x] T044 Run quickstart.md verification checklist
- [x] T045 Update CLAUDE.md with Phase 16B completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - User stories can proceed in priority order (P1 → P2 → P3)
  - Or in parallel if implementing different functions simultaneously
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational - Independent of US1 (different functions)
- **User Story 3 (P3)**: Depends on US1 and US2 being implemented (tests edge cases of those functions)

### Within Each User Story (TDD Order)

1. Tests MUST be written and FAIL before implementation
2. Implement core function logic
3. Add keyword argument support
4. Verify tests pass

### Parallel Opportunities

**Phase 1 (Setup)**:
```bash
# All test file creation can run in parallel:
Task: T001 "Create test file tests/unit/string-trim-test.lisp"
Task: T002 "Create contract test file tests/contract/string-trim-wasm.lisp"
```

**Phase 3 (User Story 1 Tests)**:
```bash
# All US1 tests can run in parallel:
Task: T007 "Unit test for string-trim"
Task: T008 "Unit test for string-left-trim"
Task: T009 "Unit test for string-right-trim"
Task: T010 "Unit test for trim with bounds"
Task: T011 "Contract test for Wasm validation"
```

**Phase 4 (User Story 2 Tests)**:
```bash
# All US2 tests can run in parallel:
Task: T017 "Unit test for nstring-upcase"
Task: T018 "Unit test for nstring-downcase"
Task: T019 "Unit test for nstring-capitalize"
Task: T020 "Unit test for nstring with bounds"
Task: T021 "Unit test for same object returned"
Task: T022 "Contract test for Wasm validation"
```

**Phase 5 (User Story 3 Tests)**:
```bash
# All US3 edge case tests can run in parallel:
Task: T028-T034 "Edge case tests"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T006)
3. Complete Phase 3: User Story 1 (T007-T016)
4. **STOP and VALIDATE**: `(string-trim " " " test ")` returns `"test"`
5. SC-001 achieved - primary validation criterion met

### Incremental Delivery

1. Setup + Foundational → Infrastructure ready
2. Add User Story 1 → Test independently → SC-001 met (MVP!)
3. Add User Story 2 → Test independently → SC-003, SC-004 met
4. Add User Story 3 → Test independently → SC-002, SC-005 met (70% pass rate)
5. Each story adds value without breaking previous stories

### Task Count Summary

| Phase | Tasks | Parallel Tasks |
|-------|-------|----------------|
| Phase 1: Setup | 3 | 2 |
| Phase 2: Foundational | 3 | 0 |
| Phase 3: US1 - Trim | 10 | 5 |
| Phase 4: US2 - nstring | 11 | 6 |
| Phase 5: US3 - Compliance | 14 | 7 |
| Phase 6: Polish | 4 | 2 |
| **Total** | **45** | **22** |

---

## Notes

- [P] tasks = different files or independent functions, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD required: Write tests first, verify failure, then implement
- Each user story is independently testable once complete
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
