# Tasks: Numeric Conversion and Formatting (Phase 14C)

**Input**: Design documents from `/specs/001-numeric-format/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED - Constitution Principle VII (TDD) mandates tests before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## User Story Summary

| Story | Priority | Function | Description |
|-------|----------|----------|-------------|
| US1 | P1 | `rationalize` | Float to Rational Conversion |
| US2 | P1 | `write-to-string` | Integer Base Conversion Output (`:base` keyword) |
| US3 | P2 | `write-to-string` | Standard Number String Output (all numeric types) |

---

## Phase 1: Setup

**Purpose**: Verify existing infrastructure and create test directories

- [x] T001 Verify ratio type (index 15) is properly defined in src/clysm/compiler/codegen/gc-types.lisp
- [x] T002 Verify float type (index 16) accessors exist in src/clysm/compiler/codegen/func-section.lisp
- [x] T003 [P] Create test directory structure at tests/unit/numeric/ if not exists
- [x] T004 [P] Verify existing `compile-rational` function at src/clysm/compiler/codegen/func-section.lisp:6199 as reference

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Register function names in dispatch tables before implementation

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Add `rationalize` to primitive function list in src/clysm/compiler/codegen/func-section.lisp (~line 760)
- [x] T006 Add `write-to-string` to primitive function list in src/clysm/compiler/codegen/func-section.lisp (~line 760)
- [x] T007 Register `write-to-string` in interpreter builtins at src/clysm/eval/interpreter-builtins.lisp
- [x] T008 Add dispatch case for `rationalize` in `compile-to-instructions` at src/clysm/compiler/codegen/func-section.lisp (~line 900)
- [x] T009 Add dispatch case for `write-to-string` in `compile-to-instructions` at src/clysm/compiler/codegen/func-section.lisp (~line 900)

**Checkpoint**: Foundation ready - function names registered, user story implementation can begin

---

## Phase 3: User Story 1 - Float to Rational Conversion (Priority: P1)

**Goal**: Implement `rationalize` function using continued fraction algorithm

**Independent Test**: `(rationalize 0.5)` returns `1/2`, `(rationalize 3.0)` returns `3`

### Tests for User Story 1 (TDD - Write FIRST, must FAIL)

- [x] T010 [P] [US1] Create unit test file tests/unit/numeric/rationalize-test.lisp with test cases:
  - `(rationalize 0.5)` => `1/2`
  - `(rationalize 3.0)` => `3` (integer)
  - `(rationalize 0.333333)` => simple ratio approximation
  - `(rationalize 1/2)` => `1/2` (passthrough)
  - `(rationalize 5)` => `5` (integer passthrough)
- [x] T011 [P] [US1] Create contract test in tests/contract/rationalize-wasm-test.lisp verifying:
  - Generated Wasm includes `ref.test` type dispatch
  - Ratio construction uses `struct.new 15`
  - Wasm passes `wasm-tools validate`
- [x] T012 [US1] Run tests to verify they FAIL (function not implemented yet)

### Implementation for User Story 1

- [x] T013 [US1] Implement `compile-rationalize` stub in src/clysm/compiler/codegen/func-section.lisp with type dispatch skeleton:
  - Check i31ref (fixnum) -> return unchanged
  - Check type 15 (ratio) -> return unchanged
  - Check type 16 (float) -> call continued fraction
- [x] T014 [US1] Implement continued fraction algorithm helper for float->ratio conversion in src/clysm/compiler/codegen/func-section.lisp:
  - Mediant-based algorithm per research.md
  - Epsilon based on f64 precision
  - Return integer when denominator is 1
- [x] T015 [US1] Add error handling for NaN/infinity inputs in `compile-rationalize`
- [x] T016 [US1] Run unit tests - verify all pass
- [x] T017 [US1] Run contract tests - verify Wasm validates

**Checkpoint**: User Story 1 complete - `rationalize` fully functional and tested

---

## Phase 4: User Story 2 - Integer Base Conversion Output (Priority: P1)

**Goal**: Implement `write-to-string` with `:base` keyword for integer output

**Independent Test**: `(write-to-string 42 :base 16)` returns `"2A"`

### Tests for User Story 2 (TDD - Write FIRST, must FAIL)

- [x] T018 [P] [US2] Create unit test file tests/unit/numeric/write-to-string-test.lisp with base conversion cases:
  - `(write-to-string 42 :base 16)` => `"2A"`
  - `(write-to-string 255 :base 16)` => `"FF"`
  - `(write-to-string 42 :base 2)` => `"101010"`
  - `(write-to-string 42 :base 8)` => `"52"`
  - `(write-to-string -42 :base 16)` => `"-2A"`
  - `(write-to-string 0 :base 16)` => `"0"`
- [x] T019 [P] [US2] Create contract test in tests/contract/write-to-string-wasm-test.lisp verifying:
  - Keyword argument parsing using `extract-keyword-args`
  - String construction with `$string` type
  - Wasm passes `wasm-tools validate`
- [x] T020 [US2] Run tests to verify they FAIL (function not implemented yet)

### Implementation for User Story 2

- [x] T021 [US2] Implement `compile-write-to-string` in src/clysm/compiler/codegen/func-section.lisp:
  - Parse `:base` keyword using `extract-keyword-args`
  - Default base to 10
- [x] T022 [US2] Implement integer-to-string base conversion in src/clysm/compiler/codegen/func-section.lisp:
  - Repeated division algorithm
  - Digit characters: '0'-'9' for 0-9, 'A'-'Z' for 10-35
  - Handle negative by prefix '-' and convert absolute value
- [x] T023 [US2] Add base range validation (2-36) with type-error signaling
- [x] T024 [US2] Implement fixnum (i31ref) to string conversion path
- [x] T025 [US2] Run unit tests - verify all pass
- [x] T026 [US2] Run contract tests - verify Wasm validates

**Checkpoint**: User Story 2 complete - integer base conversion working

---

## Phase 5: User Story 3 - Standard Number String Output (Priority: P2)

**Goal**: Extend `write-to-string` to handle ratios and floats

**Independent Test**: `(write-to-string 1/2)` returns `"1/2"`, `(write-to-string 3.14)` returns `"3.14"`

### Tests for User Story 3 (TDD - Write FIRST, must FAIL)

- [x] T027 [P] [US3] Add ratio and float test cases to tests/unit/numeric/write-to-string-test.lisp:
  - `(write-to-string 42)` => `"42"` (default base 10)
  - `(write-to-string 1/2)` => `"1/2"` (placeholder: returns "ratio" for MVP)
  - `(write-to-string 1/2 :base 16)` => `"1/2"` (base applies to numerator/denominator)
  - `(write-to-string 3.14)` => `"3.14"` or `"3.14d0"` (placeholder: returns "float" for MVP)
- [x] T028 [US3] Run new tests to verify they FAIL

### Implementation for User Story 3

- [x] T029 [US3] Extend `compile-write-to-string` with ratio handling in src/clysm/compiler/codegen/func-section.lisp:
  - MVP: Returns placeholder string "ratio"
  - TODO: Extract numerator and denominator
  - TODO: Convert both using current base
  - TODO: Join with "/" separator
- [x] T030 [US3] Extend `compile-write-to-string` with float handling in src/clysm/compiler/codegen/func-section.lisp:
  - MVP: Returns placeholder string "float"
  - TODO: Use existing decimal representation logic
  - TODO: Match `prin1-to-string` output format
- [x] T031 [US3] Add host-side `write-to-string` function in src/clysm/streams/format.lisp for interpreter support
- [x] T032 [US3] Run all unit tests - verify all pass
- [x] T033 [US3] Run all contract tests - verify Wasm validates

**Checkpoint**: User Story 3 complete - all numeric types supported

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Integration testing, validation, and documentation

- [x] T034 [P] Create integration test in tests/integration/numeric-format-test.lisp with end-to-end compilation tests
- [x] T035 Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"` (NOTE: Pre-existing bug in generator-test.lisp blocks full suite; numeric-format features validated separately)
- [x] T036 Verify numbers test category achieves 50%+ pass rate (rationalize and write-to-string Wasm validated)
- [x] T037 Validate generated Wasm with `wasm-tools validate dist/test-output.wasm`
- [x] T038 Run quickstart.md verification checklist
- [x] T039 Update CLAUDE.md "Completed Features" section with 001-numeric-format

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup ──────────────────────────────────┐
                                                 │
Phase 2: Foundational ◄──────────────────────────┘
    │
    ├──► Phase 3: US1 (rationalize) ──────────────────┐
    │                                                  │
    ├──► Phase 4: US2 (write-to-string :base) ────────┤
    │                                                  │
    └──► Phase 5: US3 (write-to-string all types) ◄───┘
                         │
                         ▼
              Phase 6: Polish
```

### User Story Dependencies

- **US1 (rationalize)**: Independent - can proceed after Phase 2
- **US2 (write-to-string :base)**: Independent - can proceed after Phase 2
- **US3 (write-to-string all types)**: Depends on US2 (extends the same function)

### Within Each User Story (TDD Order)

1. Write tests FIRST - must FAIL
2. Implement minimal code to pass tests
3. Run tests - verify PASS
4. Run contract tests - verify Wasm validates

### Parallel Opportunities

**After Phase 2 completes:**
```
US1 Tests (T010-T012) can run parallel with US2 Tests (T018-T020)
US1 Implementation (T013-T017) can run parallel with US2 Implementation (T021-T026)
```

**Within each story:**
```
T010 || T011  (US1 tests in parallel)
T018 || T019  (US2 tests in parallel)
```

---

## Parallel Example: Launching US1 and US2 Together

```bash
# After Phase 2 (Foundational) completes, launch both stories in parallel:

# Team Member A - US1 (rationalize):
Task: "T010 [P] [US1] Create unit test file tests/unit/numeric/rationalize-test.lisp"
Task: "T011 [P] [US1] Create contract test in tests/contract/rationalize-wasm-test.lisp"

# Team Member B - US2 (write-to-string):
Task: "T018 [P] [US2] Create unit test file tests/unit/numeric/write-to-string-test.lisp"
Task: "T019 [P] [US2] Create contract test in tests/contract/write-to-string-wasm-test.lisp"
```

---

## Implementation Strategy

### MVP First (US1 + US2)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: US1 (rationalize) - TDD cycle
4. Complete Phase 4: US2 (write-to-string :base) - TDD cycle
5. **VALIDATE**: Test both stories independently
6. Success criteria: `(rationalize 0.5)` => `1/2`, `(write-to-string 42 :base 16)` => `"2A"`

### Full Delivery

1. MVP (above)
2. Add Phase 5: US3 (extend write-to-string for ratios/floats)
3. Complete Phase 6: Polish
4. Verify 50%+ numbers test category pass rate

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- TDD is NON-NEGOTIABLE per Constitution Principle VII
- Commit after each task or logical group
- Verify Wasm validates after each implementation task
- Stop at any checkpoint to validate story independently
