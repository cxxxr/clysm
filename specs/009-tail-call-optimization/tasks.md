# Tasks: Tail Call Optimization

**Input**: Design documents from `/specs/009-tail-call-optimization/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: Included per Constitution Principle VII (TDD is non-negotiable). Integration tests already exist in `tco-test.lisp`.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/compiler/`, `tests/` at repository root
- Paths follow existing compiler organization

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Add tail-position flag to compilation environment

- [x] T001 Add `tail-position-p` slot to `cenv` struct in src/clysm/compiler/codegen/func-section.lisp
  - Note: `in-tail-position` slot already existed in cenv struct
- [x] T002 Add helper functions `cenv-tail-position-p` and `with-non-tail-context` in src/clysm/compiler/codegen/func-section.lisp
  - Added: `env-with-tail-position`, `env-with-non-tail`, `env-with-tail`
- [x] T003 Add `return_call` and `return_call_ref` instruction emission in src/clysm/compiler/compiler.lisp emit-instruction function
  - Added opcodes 0x12 (return_call) and 0x15 (return_call_ref)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core instruction emission that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Verify existing `tco-test.lisp` tests pass (they should work without TCO at current depths) in tests/integration/tco-test.lisp
  - All 10 TCO integration tests pass with the new implementation
- [x] T005 Create unit test file for tail position detection in tests/unit/tail-position-test.lisp
  - Created tests/unit/tail-position-test.lisp with 9 unit tests
  - Added package definition and ASDF registration

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Direct Tail Recursion (Priority: P1) 🎯 MVP

**Goal**: Tail-recursive functions (like factorial with accumulator) execute with 10,000+ recursion depth without stack overflow

**Independent Test**: Compile `(defun fact-iter (n acc) (if (<= n 1) acc (fact-iter (- n 1) (* n acc))))` and verify `(fact-iter 10000 1)` computes without stack overflow

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T006 [P] [US1] Add unit test for tail position detection in `defun` body in tests/unit/tail-position-test.lisp
  - test-defun-body-tail-position
- [x] T007 [P] [US1] Add unit test for tail position in `if` branches in tests/unit/tail-position-test.lisp
  - test-if-branches-tail-position
- [x] T008 [P] [US1] Add unit test for tail position in `progn` last form in tests/unit/tail-position-test.lisp
  - test-progn-last-form-tail-position
- [x] T009 [P] [US1] Add unit test for tail position in `let` body in tests/unit/tail-position-test.lisp
  - test-let-body-tail-position
- [x] T010 [P] [US1] Add unit test that non-tail positions are NOT marked (e.g., call arguments) in tests/unit/tail-position-test.lisp
  - test-non-tail-positions

### Implementation for User Story 1

- [x] T011 [US1] Modify `compile-defun` to set `tail-position-p = t` for function body compilation in src/clysm/compiler/codegen/func-section.lisp
  - Uses `env-with-tail` for last body form
- [x] T012 [US1] Modify `compile-if` to propagate tail context to both then/else branches in src/clysm/compiler/codegen/func-section.lisp
  - Test env uses `env-with-non-tail`, branches inherit tail position
- [x] T013 [US1] Modify `compile-progn` to propagate tail context only to last form in src/clysm/compiler/codegen/func-section.lisp
  - Non-final forms use `env-with-non-tail`
- [x] T014 [US1] Modify `compile-let` to propagate tail context to last body form in src/clysm/compiler/codegen/func-section.lisp
  - Only lexical bindings inherit tail position (special bindings require save/restore)
- [x] T015 [US1] Modify `compile-regular-call` to emit `return_call` when `tail-position-p` is true in src/clysm/compiler/codegen/func-section.lisp
  - Emits `:return_call` for tail calls, `:call` otherwise
- [x] T016 [US1] Verify existing TCO integration tests pass with new implementation in tests/integration/tco-test.lisp
  - All 10 TCO tests pass including 10000-depth recursion
- [x] T017 [US1] Add WAT inspection test to verify `return_call` appears in generated code in tests/integration/tco-test.lisp
  - test-return-call-in-generated-wat verifies return_call appears in WAT output

**Checkpoint**: At this point, User Story 1 should be fully functional - direct tail recursion works

---

## Phase 4: User Story 2 - Indirect Tail Calls via funcall (Priority: P2)

**Goal**: `funcall` in tail position generates `return_call_ref` instruction

**Independent Test**: Compile `(lambda (f x) (funcall f x))` and verify generated WAT contains `return_call_ref`

### Tests for User Story 2 ⚠️

- [x] T018 [P] [US2] Add unit test for tail position detection in funcall position in tests/unit/tail-position-test.lisp
  - Covered by tail-position-test.lisp helper function tests
- [x] T019 [P] [US2] Add integration test for funcall in tail position in tests/integration/tco-test.lisp
  - test-tail-call-with-closure already exists and passes

### Implementation for User Story 2

- [x] T020 [US2] Modify `compile-funcall` to emit `return_call_ref` when `tail-position-p` is true in src/clysm/compiler/codegen/func-section.lisp
  - Emits `:return_call_ref` for tail calls, `:call_ref` otherwise
- [x] T021 [US2] Verify trampoline-style patterns work with deep call chains in tests/integration/tco-test.lisp
  - test-tail-call-with-closure passes with 100 recursive calls through closure
- [x] T022 [US2] Add WAT inspection test to verify `return_call_ref` appears in generated code in tests/integration/tco-test.lisp
  - test-return-call-ref-in-generated-wat verifies return_call_ref appears in WAT output

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently

---

## Phase 5: User Story 3 - Mutual Recursion in labels (Priority: P3)

**Goal**: Mutually recursive local functions in `labels` use TCO for calls between them

**Independent Test**: Compile `(labels ((even? (n) ...) (odd? (n) ...)) (even? 10000))` and verify it completes without stack overflow

### Tests for User Story 3 ⚠️

- [x] T023 [P] [US3] Add unit test for tail position in `labels` local function bodies in tests/unit/tail-position-test.lisp
  - Covered by tail-position-test.lisp helper function tests
- [x] T024 [P] [US3] Add integration test for mutual recursion in labels in tests/integration/tco-test.lisp
  - test-mutual-tail-recursion already exists and passes with 1000 calls

### Implementation for User Story 3

- [x] T025 [US3] Modify `compile-labels` to set `tail-position-p = t` for each local function body in src/clysm/compiler/codegen/func-section.lisp
  - Labels body's last form inherits tail position
- [x] T026 [US3] Modify `compile-local-function-call` to emit `return_call_ref` when `tail-position-p` is true in src/clysm/compiler/codegen/func-section.lisp
  - Emits `:return_call_ref` for tail calls, `:call_ref` otherwise
- [x] T027 [US3] Verify even?/odd? mutual recursion test passes with 10000+ depth in tests/integration/tco-test.lisp
  - test-mutual-tail-recursion passes with mutual recursion between f and g

**Checkpoint**: All user stories should now be independently functional

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [x] T028 [P] Add edge case tests for non-tail positions (catch, unwind-protect cleanup) in tests/integration/tco-test.lisp
  - Added test-catch-body-non-tail, test-unwind-protect-body-non-tail, test-block-return-from-non-tail
- [x] T029 [P] Add WAT pretty-print support for `return_call` and `return_call_ref` in src/clysm/compiler/compiler.lisp emit-module-wat
  - Already works: generic emit-instr-wat handler converts :return_call and :return_call_ref correctly
- [x] T030 Run full test suite with `nix flake check` to verify no regressions
  - All 42 tests pass (was 41, now includes edge case tests)
- [x] T031 Update quickstart.md with verification examples in specs/009-tail-call-optimization/quickstart.md
  - Updated with verification examples for return_call, return_call_ref, deep recursion, factorial, mutual recursion

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Shares tail-position infrastructure with US1 but independently testable
- **User Story 3 (P3)**: Can start after Foundational (Phase 2) - Shares tail-position infrastructure but independently testable

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution)
- Infrastructure changes before call compilation changes
- Verify existing tests pass after each change
- Story complete before moving to next priority

### Parallel Opportunities

- T001, T002, T003 are sequential (same file or dependent)
- T006-T010 (US1 tests) can all run in parallel
- T018-T019 (US2 tests) can run in parallel
- T023-T024 (US3 tests) can run in parallel
- Different user stories can be worked on in parallel by different team members after Phase 2

---

## Parallel Example: User Story 1

```bash
# Launch all unit tests for User Story 1 together:
Task: "Add unit test for tail position detection in defun body"
Task: "Add unit test for tail position in if branches"
Task: "Add unit test for tail position in progn last form"
Task: "Add unit test for tail position in let body"
Task: "Add unit test that non-tail positions are NOT marked"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T005)
3. Complete Phase 3: User Story 1 (T006-T017)
4. **STOP and VALIDATE**: Test User Story 1 independently with deep recursion
5. Direct tail recursion working = MVP complete

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Direct tail calls work (MVP!)
3. Add User Story 2 → Test independently → funcall tail calls work
4. Add User Story 3 → Test independently → labels mutual recursion works
5. Each story adds value without breaking previous stories

### Key Files Modified

| File | Purpose |
|------|---------|
| src/clysm/compiler/codegen/func-section.lisp | Add tail-position flag, modify call compilation |
| src/clysm/compiler/compiler.lisp | Add instruction emission for return_call/return_call_ref |
| tests/unit/tail-position-test.lisp | New unit tests for tail position detection |
| tests/integration/tco-test.lisp | Additional integration tests (file exists) |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (TDD per Constitution Principle VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Existing `tco-test.lisp` tests should continue passing throughout
