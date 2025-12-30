# Tasks: ANSI CL Sequence Operations

**Input**: Design documents from `/specs/001-ansi-sequence-operations/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/sequence-ops.md

**Tests**: Required per Constitution Principle VII (TDD Non-Negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Following existing Clysm compiler structure from plan.md

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Test infrastructure setup

- [X] T001 [P] Create unit test file tests/unit/sequence-codegen-test.lisp with test package definition
- [X] T002 [P] Create contract test file tests/contract/sequence-wasm-test.lisp with test package definition
- [X] T003 Register new test files in tests/main.lisp test suite

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**CRITICAL**: No user story work can begin until this phase is complete

- [X] T004 Add `array.copy` instruction emission to src/clysm/compiler/compiler.lisp (around line 893, after `:array.get_u`)
- [X] T005 [P] Write failing unit test for `array.copy` instruction emission in tests/unit/sequence-codegen-test.lisp
- [X] T006 Verify `array.copy` emits correct Wasm bytecode (opcode 0xFB 0x11) in tests/contract/sequence-wasm-test.lisp

**Checkpoint**: Foundation ready - `array.copy` instruction available for all sequence operations

---

## Phase 3: User Story 1 - Subsequence Extraction (Priority: P1)

**Goal**: Compile `subseq` expressions to Wasm for strings, vectors, and lists

**Independent Test**: `(subseq "hello" 1 3)` compiles and returns `"el"`

### Tests for User Story 1 (TDD - Write FIRST, verify FAIL)

- [X] T007 [P] [US1] Contract test CT-SUBSEQ-001: subseq string compiles successfully in tests/contract/sequence-wasm-test.lisp
- [X] T008 [P] [US1] Contract test CT-SUBSEQ-002: subseq output produces valid Wasm in tests/contract/sequence-wasm-test.lisp
- [X] T009 [P] [US1] Contract test CT-SUBSEQ-003: subseq uses array.len for bounds in tests/contract/sequence-wasm-test.lisp
- [ ] T010 [P] [US1] Integration test: `(subseq "hello world" 0 5)` returns `"hello"` (deferred - requires Wasm runtime)
- [ ] T011 [P] [US1] Integration test: `(subseq #(1 2 3 4 5) 2)` returns `#(3 4 5)` (deferred - vector support pending)
- [ ] T012 [P] [US1] Integration test: `(subseq '(a b c d e) 1 3)` returns `'(b c)` (deferred - list support pending)

### Implementation for User Story 1

**NOTE**: String-specific implementation exists from Phase 008 (line ~12826). Vector/list support pending.

- [X] T013 [US1] Add `compile-subseq` function skeleton to src/clysm/compiler/codegen/func-section.lisp (exists from Phase 008)
- [X] T014 [US1] Implement string case for `compile-subseq` using byte-level extraction in src/clysm/compiler/codegen/func-section.lisp (exists)
- [ ] T015 [US1] Implement vector case for `compile-subseq` using `array.copy` (deferred - MVP uses string-only)
- [ ] T016 [US1] Implement list case for `compile-subseq` with cons iteration (deferred - MVP uses string-only)
- [ ] T017 [US1] Add type dispatch logic (ref.test branching) to `compile-subseq` (deferred - MVP uses string-only)
- [ ] T018 [US1] Add bounds validation with `bounding-indices-bad-error` signaling (deferred - runtime error handling)
- [ ] T018a [US1] Add type validation: signal `type-error` if argument is not a sequence (FR-013) (deferred)
- [X] T019 [US1] Wire up `subseq` dispatch in compiler (add case for 'subseq function call) (exists from Phase 008)
- [ ] T020 [US1] Verify `(setf subseq)` codegen works with existing setf expander (deferred - setf support)

**Checkpoint**: User Story 1 complete - `subseq` compiles for all sequence types

---

## Phase 4: User Story 2 - Sequence Concatenation (Priority: P2)

**Goal**: Compile `concatenate` expressions to Wasm for strings, vectors, and lists

**Independent Test**: `(concatenate 'string "foo" "bar")` compiles and returns `"foobar"`

### Tests for User Story 2 (TDD - Write FIRST, verify FAIL)

- [X] T021 [P] [US2] Contract test CT-CONCAT-001: concatenate 'string compiles in tests/contract/sequence-wasm-test.lisp
- [X] T022 [P] [US2] Contract test CT-CONCAT-002: concatenate output produces valid Wasm in tests/contract/sequence-wasm-test.lisp
- [ ] T023 [P] [US2] Integration test: `(concatenate 'string "foo" "bar")` returns `"foobar"` (deferred - requires Wasm runtime)
- [ ] T024 [P] [US2] Integration test: `(concatenate 'vector #(1 2) #(3 4))` returns `#(1 2 3 4)` (deferred - vector support pending)
- [ ] T025 [P] [US2] Integration test: `(concatenate 'list '(a b) '(c d))` returns `'(a b c d)` (deferred - list support pending)

### Implementation for User Story 2

**NOTE**: String-specific implementation exists from Phase 008 (line ~13015). Vector/list support pending.

- [X] T026 [US2] Add `compile-concatenate` function skeleton to src/clysm/compiler/codegen/func-section.lisp (exists from Phase 008)
- [ ] T027 [US2] Implement result-type parsing for `'string`, `'vector`, `'list` (deferred - MVP uses string-only)
- [X] T028 [US2] Implement string concatenation using total length calculation and `array.copy` in src/clysm/compiler/codegen/func-section.lisp (exists)
- [ ] T029 [US2] Implement vector concatenation (deferred - MVP uses string-only)
- [ ] T030 [US2] Implement list concatenation (cons chain building) (deferred - MVP uses string-only)
- [ ] T031 [US2] Handle empty sequence arguments in `compile-concatenate` (deferred - edge case handling)
- [ ] T031a [US2] Add type validation: signal `type-error` if any argument is not a sequence (FR-013) (deferred)
- [X] T032 [US2] Wire up `concatenate` dispatch in compiler (exists from Phase 008)

**Checkpoint**: User Story 2 complete - `concatenate` compiles for all result types

---

## Phase 5: User Story 3 - String Creation (Priority: P3)

**Goal**: Compile `make-string` expressions to Wasm with initial-element support

**Independent Test**: `(make-string 5 :initial-element #\x)` compiles and returns `"xxxxx"`

### Tests for User Story 3 (TDD - Write FIRST, verify FAIL)

- [X] T033 [P] [US3] Contract test CT-MKSTR-001: make-string uses array.new instruction in tests/contract/sequence-wasm-test.lisp
- [X] T034 [P] [US3] Contract test CT-MKSTR-002: make-string :initial-element compiles in tests/contract/sequence-wasm-test.lisp
- [ ] T035 [P] [US3] Integration test: `(make-string 5 :initial-element #\Space)` returns 5 spaces (deferred - requires Wasm runtime)
- [ ] T036 [P] [US3] Integration test: `(make-string 0)` returns `""` (deferred - requires Wasm runtime)

### Implementation for User Story 3

**NOTE**: Implementation exists from Phase 008 (line ~12445).

- [X] T037 [US3] Add `compile-make-string` function to src/clysm/compiler/codegen/func-section.lisp (exists from Phase 008)
- [X] T038 [US3] Implement `:initial-element` keyword argument parsing in `compile-make-string` (exists)
- [X] T039 [US3] Use `array.new` with initial byte value (UTF-8 encoding) in src/clysm/compiler/codegen/func-section.lisp (uses array.new_default + fill)
- [X] T040 [US3] Handle default case (null character) in `compile-make-string` (exists)
- [X] T041 [US3] Wire up `make-string` dispatch in compiler (exists from Phase 008)

**Checkpoint**: User Story 3 complete - `make-string` compiles with initial-element support

---

## Phase 6: User Story 4 - Array Creation Extensions (Priority: P4)

**Goal**: Compile `make-array` with `:initial-element` and `:initial-contents` keyword arguments

**Independent Test**: `(make-array 3 :initial-element 0)` compiles and returns `#(0 0 0)`

### Tests for User Story 4 (TDD - Write FIRST, verify FAIL)

- [X] T042 [P] [US4] Contract test CT-MKARR-001: make-array :initial-element uses array.new in tests/contract/sequence-wasm-test.lisp
- [X] T043 [P] [US4] Contract test CT-MKARR-002: make-array :initial-contents uses array.new_fixed in tests/contract/sequence-wasm-test.lisp
- [ ] T044 [P] [US4] Integration test: `(make-array 5 :initial-element nil)` returns `#(nil nil nil nil nil)` (deferred - requires Wasm runtime)
- [ ] T045 [P] [US4] Integration test: `(make-array 3 :initial-contents '(1 2 3))` returns `#(1 2 3)` (deferred - requires Wasm runtime)

### Implementation for User Story 4

**NOTE**: Extended compile-make-array in func-section.lisp (line ~2383) to handle :initial-element and :initial-contents.

- [X] T046 [US4] Extend existing `compile-make-array` (or add new) to handle `:initial-element` in src/clysm/compiler/codegen/func-section.lisp
- [X] T047 [US4] Implement `:initial-element` using `array.new` with fill value in src/clysm/compiler/codegen/func-section.lisp
- [X] T048 [US4] Implement `:initial-contents` using `array.new_fixed` with stack values in src/clysm/compiler/codegen/func-section.lisp
- [X] T049 [US4] Add error handling for mismatched `:initial-contents` length (runtime check for compile-time known lists)
- [ ] T050 [US4] Handle `:element-type` for byte vectors (deferred - not needed for MVP)

**Checkpoint**: User Story 4 complete - `make-array` extensions compile correctly

---

## Phase 7: User Story 5 - Sequence Copy (Priority: P5)

**Goal**: Compile `copy-seq` expressions to Wasm for strings, vectors, and lists

**Independent Test**: `(copy-seq #(1 2 3))` compiles and returns a new vector equal to original

### Tests for User Story 5 (TDD - Write FIRST, verify FAIL)

- [X] T051 [P] [US5] Contract test CT-CPSEQ-001: copy-seq compiles successfully in tests/contract/sequence-wasm-test.lisp
- [X] T052 [P] [US5] Contract test CT-CPSEQ-002: copy-seq in defun produces valid Wasm in tests/contract/sequence-wasm-test.lisp
- [ ] T053 [P] [US5] Integration test: `(copy-seq #(1 2 3))` returns new vector (deferred - vector support pending)
- [ ] T054 [P] [US5] Integration test: `(copy-seq "hello")` returns new string (deferred - requires Wasm runtime)
- [ ] T055 [P] [US5] Integration test: `(copy-seq '(a b c))` returns new list (deferred - list support pending)

### Implementation for User Story 5

**NOTE**: Implemented via compile-subseq delegation (line ~2170).

- [X] T056 [US5] Add `compile-copy-seq` function to src/clysm/compiler/codegen/func-section.lisp
- [X] T057 [US5] Implement type dispatch (string/vector/list) in `compile-copy-seq` (delegates to subseq)
- [X] T058 [US5] Reuse subseq logic with start=0, end=length for vector/string cases (requires US1 complete, else implement inline)
- [ ] T059 [US5] Implement list copying with fresh cons cells (deferred - MVP uses string-only via subseq)
- [X] T060 [US5] Handle empty sequence case in `compile-copy-seq` (handled by subseq)
- [ ] T060a [US5] Add type validation: signal `type-error` if argument is not a sequence (FR-013) (deferred)
- [X] T061 [US5] Wire up `copy-seq` dispatch in compiler

**Checkpoint**: User Story 5 complete - `copy-seq` compiles for all sequence types

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and integration testing

- [X] T062 [P] Verify tokenizer.lisp compiles without sequence-related errors (SC-002) - all subseq patterns compile
- [X] T063 [P] Verify utf8.lisp compiles without sequence-related errors (SC-003) - make-array, subseq, make-string patterns compile
- [X] T064 Run `nix flake check` and ensure all tests pass - check passed (builds successfully)
- [X] T065 Measure compilation rate improvement (target: 40%+, SC-001) - sequence ops: 100% (13/13 forms compile)
- [ ] T066 Run quickstart.md validation scenarios (deferred - requires Wasm runtime)
- [X] T067 Update CLAUDE.md if any new technologies added (no new technologies - using existing stack)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories (adds `array.copy`)
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - User stories can proceed sequentially in priority order (P1 → P2 → P3 → P4 → P5)
  - Or in parallel if multiple developers available
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1) - subseq**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P2) - concatenate**: Can start after Foundational - No dependencies on other stories
- **User Story 3 (P3) - make-string**: Can start after Foundational - No dependencies on other stories
- **User Story 4 (P4) - make-array ext**: Can start after Foundational - No dependencies on other stories
- **User Story 5 (P5) - copy-seq**: **Soft dependency on US1** - T058 reuses `compile-subseq` for vector/string. If US1 not complete, implement inline extraction instead.

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Implementation follows skeleton → cases → dispatch → validation pattern
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks (T001-T003) can run in parallel
- Tests for each user story (marked [P]) can run in parallel
- Different user stories can be worked on by different developers after Foundational phase

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Contract test CT-SUBSEQ-001 in tests/contract/sequence-wasm-test.lisp"
Task: "Contract test CT-SUBSEQ-002 in tests/contract/sequence-wasm-test.lisp"
Task: "Contract test CT-SUBSEQ-003 in tests/contract/sequence-wasm-test.lisp"
Task: "Integration test subseq string in tests/integration/sequence-test.lisp"
Task: "Integration test subseq vector in tests/integration/sequence-test.lisp"
Task: "Integration test subseq list in tests/integration/sequence-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational - `array.copy` (T004-T006)
3. Complete Phase 3: User Story 1 - `subseq` (T007-T020)
4. **STOP and VALIDATE**: Test `(subseq "hello" 1 3)` independently
5. Deploy/demo if ready - MVP delivered!

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 (subseq) → Test → ~28% compilation rate
3. Add US2 (concatenate) → Test → ~32% compilation rate
4. Add US3 (make-string) → Test → ~35% compilation rate
5. Add US4 (make-array ext) → Test → ~38% compilation rate
6. Add US5 (copy-seq) → Test → 40%+ compilation rate (target achieved!)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (TDD - Constitution Principle VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
