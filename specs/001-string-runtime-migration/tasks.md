# Tasks: String Runtime Migration

**Input**: Design documents from `/specs/001-string-runtime-migration/`
**Prerequisites**: plan.md, spec.md, data-model.md, contracts/, research.md

**Tests**: REQUIRED per Constitution VII (TDD - Non-negotiable). Tests must be written before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

---

## Phase 1: Setup ✓

**Purpose**: Create runtime library file structure and test scaffolding

- [x] T001 Create string-runtime.lisp file with header and package declaration in src/clysm/lib/string-runtime.lisp
- [x] T002 [P] Create string-runtime-test.lisp test file scaffolding in tests/unit/string-runtime-test.lisp
- [x] T003 [P] Add string-runtime.lisp to clysm.asd system definition

---

## Phase 2: Foundational (Helper Functions) ✓

**Purpose**: Implement helper functions required by multiple runtime functions

**⚠️ CRITICAL**: Helper functions must be complete before main function implementation

### Tests for Helper Functions

- [x] T004 [P] Write tests for utf8-continuation-byte-p in tests/unit/string-runtime-test.lisp
- [x] T005 [P] Write tests for decode-utf8-char in tests/unit/string-runtime-test.lisp
- [x] T006 [P] Write tests for char-in-bag-p in tests/unit/string-runtime-test.lisp
- [x] T007 [P] Write tests for alpha-char-p-ascii in tests/unit/string-runtime-test.lisp

### Implementation for Helper Functions

- [x] T008 [P] Implement utf8-continuation-byte-p in src/clysm/lib/string-runtime.lisp
- [x] T009 [P] Implement decode-utf8-char with UTF-8 decoding logic in src/clysm/lib/string-runtime.lisp
- [x] T010 [P] Implement char-in-bag-p for character bag membership in src/clysm/lib/string-runtime.lisp
- [x] T011 [P] Implement alpha-char-p-ascii for ASCII alphabet check in src/clysm/lib/string-runtime.lisp

**Checkpoint**: All helper function tests pass. Main function implementation can begin. ✓

---

## Phase 3: User Story 1 - Runtime Library Implementation (Priority: P1) 🎯 MVP ✓

**Goal**: Implement all 5 string functions in pure Lisp using Layer 1 primitives only

**Independent Test**: Load string-runtime.lisp in SBCL and verify each function produces correct results for standard string operations

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T012 [P] [US1] Write tests for string-char-rt (basic indexing, UTF-8 chars, bounds error) in tests/unit/string-runtime-test.lisp
- [x] T013 [P] [US1] Write tests for string-trim-rt (no-op trim, whitespace trim, empty bag) in tests/unit/string-runtime-test.lisp
- [x] T014 [P] [US1] Write tests for string-left-trim-rt in tests/unit/string-runtime-test.lisp
- [x] T015 [P] [US1] Write tests for string-right-trim-rt in tests/unit/string-runtime-test.lisp
- [x] T016 [P] [US1] Write tests for string-capitalize-rt (words, punctuation, mixed case) in tests/unit/string-runtime-test.lisp
- [x] T017 [P] [US1] Write tests for nstring-capitalize-rt (destructive modification) in tests/unit/string-runtime-test.lisp
- [x] T018 [P] [US1] Write tests for string-compare-ci-rt (equal, not-equal, ordering) in tests/unit/string-runtime-test.lisp

### Implementation for User Story 1

- [x] T019 [US1] Implement string-char-rt with UTF-8 iteration and codepoint decode in src/clysm/lib/string-runtime.lisp
- [x] T020 [US1] Implement string-left-trim-rt with character bag matching in src/clysm/lib/string-runtime.lisp
- [x] T021 [US1] Implement string-right-trim-rt with character bag matching in src/clysm/lib/string-runtime.lisp
- [x] T022 [US1] Implement string-trim-rt using left/right-trim helpers in src/clysm/lib/string-runtime.lisp
- [x] T023 [US1] Implement string-capitalize-rt with word boundary detection in src/clysm/lib/string-runtime.lisp
- [x] T024 [US1] Implement nstring-capitalize-rt (destructive variant) in src/clysm/lib/string-runtime.lisp
- [x] T025 [US1] Implement string-compare-ci-rt core comparison logic in src/clysm/lib/string-runtime.lisp
- [x] T026 [US1] Implement string-equal-rt, string-not-equal-rt wrappers in src/clysm/lib/string-runtime.lisp
- [x] T027 [US1] Implement string-lessp-rt, string-greaterp-rt wrappers in src/clysm/lib/string-runtime.lisp
- [x] T028 [US1] Implement string-not-lessp-rt, string-not-greaterp-rt wrappers in src/clysm/lib/string-runtime.lisp
- [x] T029 [US1] Add HyperSpec reference comments to all function docstrings in src/clysm/lib/string-runtime.lisp

**Checkpoint**: All US1 tests pass. Runtime functions work correctly in isolation. ✓

---

## Phase 4: User Story 4 - ANSI Keyword Argument Support (Priority: P2) ✓

**Goal**: Add full :start/:end/:test keyword support to all applicable functions

**Independent Test**: Call each function with various keyword combinations and verify correct behavior

### Tests for User Story 4

- [x] T030 [P] [US4] Write tests for string-trim-rt with :start/:end keywords in tests/unit/string-runtime-test.lisp
- [x] T031 [P] [US4] Write tests for string-capitalize-rt with :start/:end keywords in tests/unit/string-runtime-test.lisp
- [x] T032 [P] [US4] Write tests for string-compare-ci-rt with :start1/:end1/:start2/:end2 in tests/unit/string-runtime-test.lisp
- [x] T033 [P] [US4] Write tests for bounds validation (start > end, beyond length) in tests/unit/string-runtime-test.lisp

### Implementation for User Story 4

- [x] T034 [US4] Add :start/:end parameter handling to string-trim-rt family in src/clysm/lib/string-runtime.lisp
- [x] T035 [US4] Add :start/:end parameter handling to string-capitalize-rt family in src/clysm/lib/string-runtime.lisp
- [x] T036 [US4] Add :start1/:end1/:start2/:end2 handling to string-compare-ci-rt in src/clysm/lib/string-runtime.lisp
- [x] T037 [US4] Implement bounds validation and error signaling for all functions in src/clysm/lib/string-runtime.lisp

**Checkpoint**: All US4 tests pass. Keyword arguments work per ANSI spec. ✓

> NOTE: Keyword argument support was implemented together with Phase 3 (US1) in a single pass.

---

## Phase 5: User Story 2 - Dispatch Table Registration (Priority: P2) ✓

**Goal**: Register all runtime functions in *runtime-function-table* for compiler dispatch

**Independent Test**: Check that `(gethash 'string-trim *runtime-function-table*)` returns the function entry with correct arity

### Tests for User Story 2

- [x] T038 [P] [US2] Write tests verifying char registration in *runtime-function-table* in tests/unit/string-runtime-test.lisp
- [x] T039 [P] [US2] Write tests verifying string-trim family registration in tests/unit/string-runtime-test.lisp
- [x] T040 [P] [US2] Write tests verifying string-capitalize family registration in tests/unit/string-runtime-test.lisp
- [x] T041 [P] [US2] Write tests verifying string comparison function registrations in tests/unit/string-runtime-test.lisp

### Implementation for User Story 2

- [x] T042 [US2] Register char and schar with :$string-char-rt (arity 2) in src/clysm/compiler/codegen/func-section.lisp
- [x] T043 [US2] Register string-trim, string-left-trim, string-right-trim (arity nil) in src/clysm/compiler/codegen/func-section.lisp
- [x] T044 [US2] Register string-capitalize, nstring-capitalize (arity nil) in src/clysm/compiler/codegen/func-section.lisp
- [x] T045 [US2] Register string-equal through string-not-greaterp (6 functions, arity nil) in src/clysm/compiler/codegen/func-section.lisp
- [x] T046 [US2] Run Stage 1 compilation to verify dispatch works in build/stage1-complete.lisp

**Checkpoint**: All US2 tests pass. Compiler dispatches to runtime functions. ✓

---

## Phase 6: User Story 3 - Inline Codegen Removal (Priority: P3) ⏸️ DEFERRED

**Goal**: Remove ~710 lines of compile-* functions from func-section.lisp

**Status**: DEFERRED - Runtime dispatch is working. Inline codegen is bypassed but not yet removed. Safe to remove in a future iteration with proper incremental testing.

**Independent Test**: Verify func-section.lisp no longer contains the target compile-* functions, all tests pass

### Tests for User Story 3

- [ ] T047 [P] [US3] Write integration test verifying Stage 1 compiles successfully in tests/integration/string-runtime/
- [ ] T048 [P] [US3] Write contract test verifying Wasm validates in tests/contract/string-runtime/

### Implementation for User Story 3

- [ ] T049 [US3] Remove compile-string-char (lines ~12954-13148) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T050 [US3] Remove compile-string-capitalize (lines ~14131-14250) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T051 [US3] Remove compile-string-trim and helpers (lines ~14257-14715) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T052 [US3] Remove compile-string-compare-ci and helpers (lines ~13434-13647) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T053 [US3] Remove compile-nstring-capitalize (lines ~14860-14979) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T054 [US3] Run full test suite to verify no regressions via sbcl --eval "(asdf:test-system :clysm)"
- [ ] T055 [US3] Run wasm-tools validate dist/clysm-stage1.wasm

**Checkpoint**: All tests pass. func-section.lisp reduced by ~710 lines.

---

## Phase 7: Polish & Verification ✓

**Purpose**: Final verification and documentation

- [ ] T056 [P] Verify line count reduction (wc -l src/clysm/compiler/codegen/func-section.lisp) - DEFERRED (depends on Phase 6)
- [x] T057 [P] Update CLAUDE.md with 001-string-runtime-migration entry
- [x] T058 Run quickstart.md validation checklist
- [x] T059 Final Stage 1 compilation and Wasm validation (21,263 bytes, validates successfully)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS main implementation
- **User Story 1 (Phase 3)**: Depends on Foundational helpers
- **User Story 4 (Phase 4)**: Depends on US1 (adds keywords to existing functions)
- **User Story 2 (Phase 5)**: Depends on US1 + US4 (functions must exist to register)
- **User Story 3 (Phase 6)**: Depends on US2 (dispatch must work before removing inline codegen)
- **Polish (Phase 7)**: Depends on all user stories complete

### User Story Dependencies

```
US1 (Runtime Library) ← Foundation
        ↓
US4 (Keyword Support) ← US1 (extends US1 functions)
        ↓
US2 (Dispatch Registration) ← US4 (registers complete functions)
        ↓
US3 (Codegen Removal) ← US2 (dispatch must work first)
```

### Within Each User Story

1. Tests written first (TDD per Constitution VII)
2. Tests must FAIL before implementation
3. Implementation makes tests pass
4. Checkpoint verification before next story

### Parallel Opportunities

**Phase 2 (Foundational)**:
- T004, T005, T006, T007 can run in parallel (all test tasks)
- T008, T009, T010, T011 can run in parallel (all implementation tasks)

**Phase 3 (US1)**:
- T012 through T018 can run in parallel (all test tasks)
- T019 through T028 are sequential (function dependencies)

**Phase 4 (US4)**:
- T030, T031, T032, T033 can run in parallel (all test tasks)

**Phase 5 (US2)**:
- T038, T039, T040, T041 can run in parallel (all test tasks)

**Phase 6 (US3)**:
- T047, T048 can run in parallel (test tasks)
- T049 through T053 are sequential (remove one at a time, verify)

---

## Parallel Example: Phase 2 Foundational

```bash
# Launch all helper function tests together:
Task: "Write tests for utf8-continuation-byte-p in tests/unit/string-runtime-test.lisp"
Task: "Write tests for decode-utf8-char in tests/unit/string-runtime-test.lisp"
Task: "Write tests for char-in-bag-p in tests/unit/string-runtime-test.lisp"
Task: "Write tests for alpha-char-p-ascii in tests/unit/string-runtime-test.lisp"

# Launch all helper implementations together:
Task: "Implement utf8-continuation-byte-p in src/clysm/lib/string-runtime.lisp"
Task: "Implement decode-utf8-char with UTF-8 decoding logic in src/clysm/lib/string-runtime.lisp"
Task: "Implement char-in-bag-p for character bag membership in src/clysm/lib/string-runtime.lisp"
Task: "Implement alpha-char-p-ascii for ASCII alphabet check in src/clysm/lib/string-runtime.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational helpers
3. Complete Phase 3: User Story 1 (basic runtime functions)
4. **STOP and VALIDATE**: All runtime functions work in SBCL
5. This is already useful for testing and debugging

### Full Delivery

1. Complete MVP (US1)
2. Add US4: Full keyword argument support
3. Add US2: Register functions for compiler dispatch
4. Add US3: Remove inline codegen (~710 lines reduction)
5. Polish: Final verification and documentation

### Sequential Execution (Solo Developer)

```
Setup → Foundational → US1 → US4 → US2 → US3 → Polish
```

All user stories are sequential due to dependencies (US4 extends US1, US2 needs US4, US3 needs US2).

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD REQUIRED: Tests must fail before implementation (Constitution VII)
- HyperSpec references REQUIRED in all function docstrings (Constitution IX)
- Use Layer 1 primitives only for self-hosting compatibility
- Commit after each checkpoint verification
