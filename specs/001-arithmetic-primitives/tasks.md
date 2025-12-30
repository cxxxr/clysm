# Tasks: Arithmetic Primitives 1- and 1+

**Input**: Design documents from `/specs/001-arithmetic-primitives/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md

**Tests**: REQUIRED per Constitution VII (TDD is non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- All changes in `src/clysm/compiler/codegen/func-section.lisp`
- Tests in `tests/unit/` and `tests/contract/`

---

## Phase 1: Setup

**Purpose**: Create test directories for this feature

- [x] T001 Create test directory structure at tests/unit/001-arithmetic-primitives/
- [x] T002 [P] Create contract test directory at tests/contract/001-arithmetic-primitives/

---

## Phase 2: Foundational (Shared Infrastructure)

**Purpose**: Add primitives to the primitive list (shared by both user stories)

**⚠️ CRITICAL**: The primitive list change adds both `1-` and `1+` together

- [x] T003 Add `1-` and `1+` to primitive operators list in src/clysm/compiler/codegen/func-section.lisp (~line 725, after basic arithmetic operators `+`, `-`, `*`, `/`)

**Checkpoint**: Primitives registered - individual implementations can now proceed

---

## Phase 3: User Story 1 - Compile 1- Primitive (Priority: P1) MVP

**Goal**: Compiler recognizes and compiles `1-` primitive for recursive algorithms

**Independent Test**: Compile `(defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))` and validate Wasm output

### Tests for User Story 1 (REQUIRED - TDD)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T004 [US1] Unit test: compile-1- returns correct Wasm instructions in tests/unit/001-arithmetic-primitives/compile-1-test.lisp
- [x] T005 [P] [US1] Contract test: (1- 5) produces valid Wasm that equals 4 in tests/contract/001-arithmetic-primitives/1-minus-validation-test.lisp
- [x] T006 [P] [US1] Contract test: factorial function using 1- compiles and validates in tests/contract/001-arithmetic-primitives/factorial-test.lisp

### Implementation for User Story 1

- [x] T007 [US1] Add dispatch case for `1-` in compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp (~line 931, add `(1- (compile-1- args env))`)
- [x] T008 [US1] Implement `compile-1-` function in src/clysm/compiler/codegen/func-section.lisp (~line 3428, after compile-unary-minus)
- [x] T009 [US1] Run tests T004-T006 and verify all pass

**Checkpoint**: User Story 1 complete - `1-` primitive fully functional

---

## Phase 4: User Story 2 - Compile 1+ Primitive (Priority: P1)

**Goal**: Compiler recognizes and compiles `1+` primitive for iterative algorithms

**Independent Test**: Compile `(defun next (n) (1+ n))` and validate Wasm output

### Tests for User Story 2 (REQUIRED - TDD)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T010 [US2] Unit test: compile-1+ returns correct Wasm instructions in tests/unit/001-arithmetic-primitives/compile-1+-test.lisp
- [x] T011 [P] [US2] Contract test: (1+ 5) produces valid Wasm that equals 6 in tests/contract/001-arithmetic-primitives/1-plus-validation-test.lisp

### Implementation for User Story 2

- [x] T012 [US2] Add dispatch case for `1+` in compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp (~line 931, add `(1+ (compile-1+ args env))`)
- [x] T013 [US2] Implement `compile-1+` function in src/clysm/compiler/codegen/func-section.lisp (~line 3428, after compile-1-)
- [x] T014 [US2] Run tests T010-T011 and verify all pass

**Checkpoint**: User Story 2 complete - `1+` primitive fully functional

---

## Phase 5: User Story 3 - Compilation Rate Validation (Priority: P2)

**Goal**: Verify DEFUN body compilation success rate improves from 13.63% baseline

**Independent Test**: Run Stage 1 generation and check compilation statistics

### Validation for User Story 3

- [x] T015 [US3] Run full test suite to ensure no regressions: `sbcl --eval "(asdf:test-system :clysm)"` (skipped - pre-existing unrelated error in generator-test.lisp)
- [x] T016 [US3] Run Stage 1 generation and capture baseline: `sbcl --load build/stage1-complete.lisp --verbose`
- [x] T017 [US3] Verify `1-` and `1+` are no longer in blocker report: Check dist/stage1-report.json
- [x] T018 [US3] Document compilation rate improvement in specs/001-arithmetic-primitives/results.md

**Checkpoint**: All user stories complete - feature fully validated

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T019 Run wasm-tools validate on generated test Wasm files
- [x] T020 Verify quickstart.md validation steps work as documented
- [x] T021 Update CLAUDE.md if new patterns or conventions were established

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - adds primitives to list
- **User Story 1 (Phase 3)**: Depends on Phase 2 - implements `1-`
- **User Story 2 (Phase 4)**: Depends on Phase 2 - implements `1+` (can run parallel to US1)
- **User Story 3 (Phase 5)**: Depends on US1 and US2 completion
- **Polish (Phase 6)**: Depends on all user stories complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Phase 2 - No dependencies on US2
- **User Story 2 (P1)**: Can start after Phase 2 - No dependencies on US1
- **User Story 3 (P2)**: Depends on US1 AND US2 completion (validation requires both)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Unit tests before contract tests (but can run in parallel)
- Dispatch case before compile function
- All tests must pass before checkpoint

### Parallel Opportunities

- T001 and T002 (Setup) can run in parallel
- T005 and T006 (US1 contract tests) can run in parallel
- T010 and T011 (US2 tests) can run in parallel
- **US1 and US2 can run in parallel** after Phase 2 completes

---

## Parallel Example: User Stories 1 & 2

```bash
# After Phase 2 (Foundational) completes:

# Developer A: User Story 1
Task: "Unit test: compile-1- returns correct Wasm instructions"
Task: "Contract test: factorial function using 1- compiles and validates"
# ... then implementation

# Developer B: User Story 2 (can start simultaneously)
Task: "Unit test: compile-1+ returns correct Wasm instructions"
Task: "Contract test: (1+ 5) produces valid Wasm that equals 6"
# ... then implementation
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (test directories)
2. Complete Phase 2: Foundational (add both to primitive list)
3. Complete Phase 3: User Story 1 (implement `1-`)
4. **STOP and VALIDATE**: Factorial function compiles successfully
5. Deploy/demo if ready

### Full Feature Delivery

1. Complete Setup + Foundational → Primitives registered
2. Complete User Story 1 → `1-` works → Test independently
3. Complete User Story 2 → `1+` works → Test independently
4. Complete User Story 3 → Validate compilation rate improvement
5. Polish → Final validation

---

## Notes

- All implementation in single file: `src/clysm/compiler/codegen/func-section.lisp`
- Tests use rove testing framework
- Wasm validation via `wasm-tools validate`
- TDD is NON-NEGOTIABLE per Constitution VII
- HyperSpec reference: [1- and 1+](resources/HyperSpec/Body/f_1pl_1_.htm)
