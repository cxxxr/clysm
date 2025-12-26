# Tasks: Numeric Accessors and Float Special Values

**Input**: Design documents from `/specs/019-numeric-accessors/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Existing tests in `tests/integration/ratio-test.lisp` and `tests/integration/float-test.lisp` drive this implementation (per Constitution VII. TDD).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

---

## Phase 1: Setup

**Purpose**: Verify existing infrastructure and test baseline

- [X] T001 Run existing tests to establish baseline failures in tests/integration/ratio-test.lisp
- [X] T002 [P] Run existing tests to establish baseline failures in tests/integration/float-test.lisp
- [X] T003 Verify $RATIO type definition exists with correct fields in src/clysm/compiler/codegen/gc-types.lisp
- [X] T004 [P] Verify $FLOAT type definition uses f64 in src/clysm/compiler/codegen/gc-types.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T005 Add emit-is-rational helper function in src/clysm/compiler/codegen/func-section.lisp (type check for fixnum/bignum/ratio)
- [X] T006 [P] Verify compile-call dispatch mechanism supports adding new function handlers in src/clysm/compiler/codegen/func-section.lisp
- [X] T007 [P] Verify ref.cast and struct.get instruction emission works for custom types in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Ratio Component Access (Priority: P1) 🎯 MVP

**Goal**: Implement `numerator` and `denominator` ANSI CL accessor functions for ratios and integers

**Independent Test**: `(numerator 1/3)` returns 1, `(denominator 1/3)` returns 3, `(numerator 5)` returns 5, `(denominator 5)` returns 1

### TDD: Verify Tests Fail First

- [X] T008 [US1] Run ratio-test.lisp numerator tests (lines 116-120) and confirm they fail in tests/integration/ratio-test.lisp
- [X] T009 [US1] Run ratio-test.lisp denominator tests (lines 121-124) and confirm they fail in tests/integration/ratio-test.lisp

### Implementation for User Story 1

- [X] T010 [US1] Implement compile-numerator function with type dispatch (fixnum→self, bignum→self, ratio→struct.get 0) in src/clysm/compiler/codegen/func-section.lisp
- [X] T011 [US1] Implement compile-denominator function with type dispatch (fixnum→1, bignum→1, ratio→struct.get 1) in src/clysm/compiler/codegen/func-section.lisp
- [X] T012 [US1] Register NUMERATOR handler in compile-call dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T013 [US1] Register DENOMINATOR handler in compile-call dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T014 [US1] Run ratio-test.lisp and verify numerator/denominator tests pass in tests/integration/ratio-test.lisp

**Checkpoint**: User Story 1 complete - `numerator` and `denominator` work for ratios and integers

---

## Phase 4: User Story 2 - Float Special Value Generation (Priority: P1)

**Goal**: Ensure IEEE 754 special values (+Infinity, -Infinity, NaN) are correctly generated from float arithmetic

**Independent Test**: `(/ 1.0 0.0)` returns +Infinity, `(/ -1.0 0.0)` returns -Infinity, `(- +inf +inf)` returns NaN

### TDD: Verify Tests Fail First

- [X] T015 [US2] Run float-test.lisp infinity generation tests (lines 77-83) and confirm current behavior in tests/integration/float-test.lisp
- [X] T016 [US2] Run float-test.lisp NaN generation tests (lines 84-91) and confirm current behavior in tests/integration/float-test.lisp

### Implementation for User Story 2

- [X] T017 [US2] Verify float division compiles to f64.div (not compile-time error) in src/clysm/compiler/codegen/func-section.lisp
- [X] T018 [US2] Ensure constant folding does NOT evaluate (/ 1.0 0.0) at compile time - defer to runtime in src/clysm/compiler/codegen/func-section.lisp
- [X] T019 [US2] Verify f64 special values are correctly boxed in $FLOAT struct in src/clysm/compiler/codegen/func-section.lisp
- [X] T020 [US2] Run float-test.lisp infinity/NaN generation tests and verify pass in tests/integration/float-test.lisp

**Checkpoint**: User Story 2 complete - IEEE 754 special values correctly generated

---

## Phase 5: User Story 3 - Float Special Value Comparison (Priority: P1)

**Goal**: Implement IEEE 754-compliant comparison semantics for NaN and Infinity values

**Independent Test**: `(= nan nan)` returns NIL, `(= +inf +inf)` returns T, `(> +inf 1.0)` returns T

### TDD: Verify Tests Fail First

- [X] T021 [US3] Run float-test.lisp NaN comparison tests (lines 92-95) and confirm they fail in tests/integration/float-test.lisp
- [X] T022 [US3] Run float-test.lisp infinity comparison tests (lines 96-100) and confirm current behavior in tests/integration/float-test.lisp

### Implementation for User Story 3

- [X] T023 [US3] Add emit-is-float helper function for type checking in src/clysm/compiler/codegen/func-section.lisp
- [X] T024 [US3] Extend compile-comparison-op to handle float operands using f64.eq/f64.lt/f64.gt/f64.le/f64.ge in src/clysm/compiler/codegen/func-section.lisp
- [X] T025 [US3] Add type dispatch to comparison operators: detect both operands are floats, extract f64 values, use f64 comparison in src/clysm/compiler/codegen/func-section.lisp
- [X] T026 [US3] Verify NaN comparison returns NIL (f64.eq with NaN returns 0 per IEEE 754) in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [US3] Verify infinity comparison returns correct results (+inf = +inf → T, +inf > finite → T) in src/clysm/compiler/codegen/func-section.lisp
- [X] T028 [US3] Run float-test.lisp comparison tests and verify all pass in tests/integration/float-test.lisp

**Checkpoint**: User Story 3 complete - NaN and Infinity comparisons work correctly

---

## Phase 6: User Story 4 - Double-Float Precision Preservation (Priority: P2)

**Goal**: Verify double-float literals (1.0d0) preserve full 64-bit IEEE 754 precision through compilation

**Independent Test**: `(/ 1.0d0 3.0d0)` result matches 0.3333333333333333d0 within 1.0d-15 tolerance

### TDD: Verify Tests

- [X] T029 [US4] Run float-test.lisp double-precision tests (lines 104-114) and confirm current behavior in tests/integration/float-test.lisp

### Verification for User Story 4

- [X] T030 [US4] Verify tokenizer parses 1.0d0 as double-float (uses 10.0d0 exponent) in src/clysm/reader/tokenizer.lisp
- [X] T031 [US4] Verify codegen emits f64.const with full precision (coerce to double-float) in src/clysm/compiler/codegen/func-section.lisp
- [X] T032 [US4] Add precision validation test: (/ 1.0d0 3.0d0) compared to 0.3333333333333333d0 in tests/integration/float-test.lisp
- [X] T033 [US4] Run float-test.lisp precision tests and verify pass in tests/integration/float-test.lisp

**Checkpoint**: User Story 4 complete - double-float precision preserved throughout compilation

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [X] T034 Run all numeric tower tests (010-numeric-tower) to verify no regressions in tests/
- [X] T035 [P] Verify generated Wasm validates with wasm-tools validate
- [X] T036 [P] Verify all tests execute successfully in wasmtime (NOTE: requires FFI shim from host-shim/)
- [X] T037 Run nix flake check to validate full test suite
- [X] T038 Update CLAUDE.md to reflect 019-numeric-accessors completion

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (Ratio Accessors) can proceed independently
  - US2 (Float Generation) can proceed independently
  - US3 (Float Comparison) can proceed independently (but builds on US2 context)
  - US4 (Double Precision) can proceed independently
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 3 (P1)**: Can start after Foundational - Logically builds on US2 (special values exist before comparing)
- **User Story 4 (P2)**: Can start after Foundational - No dependencies on other stories

### Within Each User Story

- TDD: Verify tests fail FIRST before implementing
- Implementation follows research.md patterns
- Verify tests pass AFTER implementation
- Story complete before moving to next priority

### Parallel Opportunities

- T001/T002: Baseline test runs can be parallel
- T003/T004: Type verification can be parallel
- T005/T006/T007: Foundational tasks can be parallel
- US1, US2, US4: Can be worked on in parallel (different functions/concerns)
- T034/T035/T036: Polish validation can be parallel

---

## Parallel Example: User Story 1

```bash
# Launch TDD verification in parallel:
Task: "Run ratio-test.lisp numerator tests (lines 116-120)"
Task: "Run ratio-test.lisp denominator tests (lines 121-124)"

# After TDD verification, implementation is sequential:
# T010 → T011 → T012 → T013 → T014
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (verify baseline)
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (numerator/denominator)
4. **STOP and VALIDATE**: Run ratio-test.lisp - all tests should pass
5. This provides core ANSI CL accessor functionality

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → `numerator`/`denominator` work (MVP!)
3. Add User Story 2 → Test independently → Special values generate correctly
4. Add User Story 3 → Test independently → Special value comparisons work
5. Add User Story 4 → Test independently → Double precision preserved
6. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (ratio accessors)
   - Developer B: User Story 2 + 3 (float special values - related)
   - Developer C: User Story 4 (precision verification)
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD: Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Wasm f64 instructions are IEEE 754 compliant - no special NaN handling code needed
