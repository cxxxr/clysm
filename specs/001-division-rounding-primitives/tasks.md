# Tasks: Division/Rounding Function Primitives

**Input**: Design documents from `/specs/001-division-rounding-primitives/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required by Constitution VII (TDD) - tests are written before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Existing project structure per plan.md:
- **Compiler**: `src/clysm/compiler/codegen/func-section.lisp`
- **Analyzer**: `src/clysm/compiler/analyzer/free-vars.lisp`
- **Unit Tests**: `tests/unit/rounding-functions.lisp`
- **Contract Tests**: `tests/contract/rounding-wasm.lisp`

---

## Phase 1: Setup

**Purpose**: Verify existing infrastructure and prepare for implementation

- [x] T001 Verify existing `compile-truncate` pattern in `src/clysm/compiler/codegen/func-section.lisp:3485-3495`
- [x] T002 Verify AST parsing for floor/ceiling/round in `src/clysm/compiler/ast.lisp:817-819` and `parse-rounding-form`
- [x] T003 Verify multiple-values mechanism in `src/clysm/compiler/codegen/func-section.lisp` (`compile-values` function)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Add floor, ceiling, round, ffloor, fceiling, fround to `*primitive-operators*` in `src/clysm/compiler/analyzer/free-vars.lisp:11`
- [x] T005 Add dispatcher cases for floor, ceiling, round, ffloor, fceiling, fround in `compile-builtin` in `src/clysm/compiler/codegen/func-section.lisp:725` (near existing truncate case)
- [x] T006 Create helper function `compile-rounding-with-mv` for shared multiple-values logic in `src/clysm/compiler/codegen/func-section.lisp`

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Compile floor/ceiling/round Functions (Priority: P1) MVP

**Goal**: Compile floor, ceiling, round with two integer arguments, returning quotient and remainder

**Independent Test**: Compile `(floor 7 2)` and verify output returns 3 (quotient) and 1 (remainder)

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (Constitution VII)**

- [x] T007 [P] [US1] Create unit test file with floor/ceiling/round integer tests in `tests/unit/rounding-functions.lisp`
- [x] T008 [P] [US1] Create contract test for floor Wasm output validation in `tests/contract/rounding-wasm.lisp`

### Implementation for User Story 1

- [x] T009 [US1] Implement `compile-floor` for two-argument integer case in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T010 [US1] Implement `compile-ceiling` for two-argument integer case in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T011 [US1] Implement `compile-round` for two-argument integer case in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T012 [US1] Add negative integer test cases: `(floor -7 2)` → -4, 1 and `(ceiling -7 2)` → -3, -1
- [x] T013 [US1] Validate generated Wasm with `wasm-tools validate`

**Checkpoint**: User Story 1 complete - floor/ceiling/round work with integer arguments

---

## Phase 4: User Story 2 - Float-Result Variants (Priority: P2)

**Goal**: Compile ffloor, fceiling, fround returning float quotients

**Independent Test**: Compile `(ffloor 7.5 2.0)` and verify quotient is 3.0 (float) and remainder is 1.5

### Tests for User Story 2

- [x] T014 [P] [US2] Add ffloor/fceiling/fround tests to `tests/unit/rounding-functions.lisp`
- [x] T015 [P] [US2] Add contract test for ffloor Wasm output (must use $float struct) in `tests/contract/rounding-wasm.lisp`

### Implementation for User Story 2

- [x] T016 [US2] Implement `compile-ffloor` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T017 [US2] Implement `compile-fceiling` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T018 [US2] Implement `compile-fround` in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T019 [US2] Verify f-variants return $float struct for quotient (not i31ref)

**Checkpoint**: User Story 2 complete - ffloor/fceiling/fround work with float arguments

---

## Phase 5: User Story 3 - Single-Argument Rounding (Priority: P2)

**Goal**: Support single-argument form where divisor defaults to 1

**Independent Test**: Compile `(floor 3.7)` and verify returns 3 and approximately 0.7

### Tests for User Story 3

- [x] T020 [P] [US3] Add single-argument tests for all 6 functions in `tests/unit/rounding-functions.lisp`
- [x] T021 [P] [US3] Add contract test for single-argument form Wasm output in `tests/contract/rounding-wasm.lisp`

### Implementation for User Story 3

- [x] T022 [US3] Update `compile-floor` to handle single-argument form (synthesize divisor=1) in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T023 [US3] Update `compile-ceiling` to handle single-argument form in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T024 [US3] Update `compile-round` to handle single-argument form in `src/clysm/compiler/codegen/func-section.lisp`
- [x] T025 [US3] Update `compile-ffloor`, `compile-fceiling`, `compile-fround` for single-argument form in `src/clysm/compiler/codegen/func-section.lisp`

**Checkpoint**: User Story 3 complete - all 6 functions support single-argument form

---

## Phase 6: User Story 4 - Type-Preserving Results (Priority: P3)

**Goal**: floor/ceiling/round with integer arguments return integer quotient; with float arguments still return integer quotient (per ANSI CL)

**Independent Test**: Verify `(floor 10 3)` returns fixnum 3, `(floor 10.0 3.0)` returns integer 3, `(ffloor 10.0 3.0)` returns float 3.0

### Tests for User Story 4

- [x] T026 [P] [US4] Add type-verification tests in `tests/unit/rounding-functions.lisp`
- [x] T027 [P] [US4] Add contract test verifying i31ref vs $float output types in `tests/contract/rounding-wasm.lisp`

### Implementation for User Story 4

- [ ] T028 [US4] Add integer-path optimization to `compile-floor` (use i32 operations when both args are integers) in `src/clysm/compiler/codegen/func-section.lisp` **[DEFERRED: P3 optimization, requires compile-time type inference]**
- [ ] T029 [US4] Add integer-path optimization to `compile-ceiling` and `compile-round` in `src/clysm/compiler/codegen/func-section.lisp` **[DEFERRED: P3 optimization, requires compile-time type inference]**
- [x] T030 [US4] Ensure floor/ceiling/round with float args still return i31ref (integer quotient)

**Checkpoint**: User Story 4 complete - type preservation works correctly

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Validation, documentation, and integration verification

- [x] T031 Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
- [x] T032 Validate all generated Wasm: `wasm-tools validate dist/clysm-stage1.wasm`
- [x] T033 Run Stage 1 generation to measure compilation rate improvement: `sbcl --load build/stage1-complete.lisp`
- [x] T034 Compare Stage 1 report before/after to verify DEFUN failures reduced
- [x] T035 Update `CLAUDE.md` with new feature entry in "Recent Changes" section

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (P1) can start after Foundational
  - US2 (P2) can start after US1 or in parallel if staffed
  - US3 (P2) can start after US1 (modifies same functions)
  - US4 (P3) can start after US1-US3
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - Creates base implementations
- **User Story 2 (P2)**: Independent of US1 (new functions) - Can proceed in parallel
- **User Story 3 (P2)**: Depends on US1 & US2 (modifies existing functions)
- **User Story 4 (P3)**: Depends on US1 & US2 (optimizes existing functions)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (Constitution VII)
- Implementation in dependency order
- Wasm validation after each story

### Parallel Opportunities

- Setup tasks (T001-T003) can run in parallel
- Test tasks marked [P] within same story can run in parallel
- US1 and US2 can run in parallel (different functions)
- T031-T035 (Polish) are largely sequential

---

## Parallel Example: User Story 1

```bash
# Launch tests in parallel:
Task: "Create unit test file with floor/ceiling/round integer tests in tests/unit/rounding-functions.lisp"
Task: "Create contract test for floor Wasm output validation in tests/contract/rounding-wasm.lisp"

# Then implement sequentially (same file):
Task: "Implement compile-floor..."
Task: "Implement compile-ceiling..."
Task: "Implement compile-round..."
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T006)
3. Complete Phase 3: User Story 1 (T007-T013)
4. **STOP and VALIDATE**: Test floor/ceiling/round with integers
5. Run Stage 1 compilation to check if DEFUN failures reduced

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 → Test → Core rounding works (MVP!)
3. Add US2 → Test → f-variants work
4. Add US3 → Test → Single-argument form works
5. Add US4 → Test → Type preservation optimized

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Constitution VII requires TDD - write tests first
- All implementations go in `func-section.lisp` (same file, so US1 tasks not parallelizable)
- Wasm opcodes: f64.floor (0x9B), f64.ceil (0x9C), f64.nearest (0x9E)
- Multiple values: mv-count (global 2), mv-buffer (global 3)
