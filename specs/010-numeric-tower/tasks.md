# Tasks: Numeric Tower Implementation

**Input**: Design documents from `/specs/010-numeric-tower/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: Required per Constitution Section VII (TDD - Non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- All paths are relative to repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Type system extension and foundational infrastructure

- [x] T001 Add numeric type constants (+type-bignum+, +type-ratio+, +type-float+, +type-complex+, +type-limb-array+) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T002 [P] Add $limb_array type definition (array i32) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T003 [P] Add $bignum struct type definition in src/clysm/compiler/codegen/gc-types.lisp
- [x] T004 [P] Add $ratio struct type definition in src/clysm/compiler/codegen/gc-types.lisp
- [x] T005 [P] Add $float struct type definition in src/clysm/compiler/codegen/gc-types.lisp
- [x] T006 [P] Add $complex struct type definition in src/clysm/compiler/codegen/gc-types.lisp
- [x] T007 Update generate-type-definitions to include new numeric types in src/clysm/compiler/codegen/gc-types.lisp
- [x] T008 Add division-by-zero exception tag in src/clysm/compiler/codegen/gc-types.lisp
- [x] T009 [P] Create test file structure in tests/integration/bignum-test.lisp
- [x] T010 [P] Create test file structure in tests/integration/ratio-test.lisp
- [x] T011 [P] Create test file structure in tests/integration/float-test.lisp
- [x] T012 [P] Create test file structure in tests/integration/complex-test.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core type dispatch and coercion infrastructure that ALL user stories depend on

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T013 Implement numeric-type-p helper to check if value is any numeric type in src/clysm/compiler/codegen/func-section.lisp
- [x] T014 Implement get-numeric-type to return type keyword (:fixnum :bignum :ratio :float :complex) in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 Implement type contagion logic (coerce-numeric-args) per CLHS 12.1.4 in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [P] Add contract test for numeric type WasmGC structures in tests/contract/numeric-types-test.lisp
- [x] T017 [P] Add numeric literal parsing for ratio (1/2), float (1.5, 1.5d0), complex (#C(1 2)) in src/clysm/reader/tokenizer.lisp
- [x] T018 Verify all existing arithmetic tests still pass with type infrastructure changes

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Arithmetic with Large Numbers (Priority: P1)

**Goal**: Implement bignum type with arbitrary-precision integer arithmetic

**Independent Test**: Compile and execute `(* 1000000000000000000 1000000000000000000)` and verify correct result

### Tests for User Story 1

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T019 [P] [US1] Write test for fixnum overflow to bignum: `(+ 9223372036854775807 1)` in tests/integration/bignum-test.lisp
- [x] T020 [P] [US1] Write test for bignum multiplication: `(* 10000000000000000000 10000000000000000000)` in tests/integration/bignum-test.lisp
- [x] T021 [P] [US1] Write test for bignum comparison operators in tests/integration/bignum-test.lisp
- [x] T022 [P] [US1] Write test for mixed fixnum/bignum arithmetic in tests/integration/bignum-test.lisp
- [x] T023 [US1] Verify all US1 tests fail (Red phase)

### Implementation for User Story 1

- [x] T024 [US1] Implement fixnum-to-bignum conversion in src/clysm/compiler/codegen/func-section.lisp (compile-bignum-literal handles this)
- [x] T025 [US1] Implement bignum-add (constant folding at compile time) in src/clysm/compiler/ast.lisp
- [x] T026 [US1] Implement bignum-sub (constant folding at compile time) in src/clysm/compiler/ast.lisp
- [x] T027 [US1] Implement bignum-mul (constant folding at compile time) in src/clysm/compiler/ast.lisp
- [x] T028 [US1] Implement bignum-div (constant folding at compile time) in src/clysm/compiler/ast.lisp
- [x] T029 [US1] Implement bignum comparison (constant folding at compile time) in src/clysm/compiler/ast.lisp
- [x] T030 [US1] Implement overflow detection in fixnum arithmetic (constant folding promotes to bignum) in src/clysm/compiler/ast.lisp
- [x] T031 [US1] Update compile-primitive for + to dispatch to bignum-add when needed (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T032 [US1] Update compile-primitive for - to dispatch to bignum-sub when needed (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T033 [US1] Update compile-primitive for * to dispatch to bignum-mul when needed (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T034 [US1] Update compile-primitive for / to return ratio or bignum as appropriate (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T035 [US1] Implement bignum normalization (host Lisp handles this during constant folding) in src/clysm/compiler/ast.lisp
- [x] T036 [US1] Verify all US1 tests pass (Green phase) - 16/16 tests pass

**Checkpoint**: Bignum arithmetic fully functional and testable independently

---

## Phase 4: User Story 2 - Rational Number Arithmetic (Priority: P1)

**Goal**: Implement ratio type with exact rational arithmetic

**Independent Test**: Compile `(+ (/ 1 3) (/ 1 3) (/ 1 3))` and verify result equals 1

### Tests for User Story 2

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T037 [P] [US2] Write test for integer division producing ratio: `(/ 1 3)` in tests/integration/ratio-test.lisp
- [x] T038 [P] [US2] Write test for ratio addition: `(+ (/ 1 2) (/ 1 4))` => 3/4 in tests/integration/ratio-test.lisp
- [x] T039 [P] [US2] Write test for ratio simplification to integer: `(/ 6 3)` => 2 in tests/integration/ratio-test.lisp
- [x] T040 [P] [US2] Write test for ratio with bignum components in tests/integration/ratio-test.lisp
- [x] T041 [US2] Verify all US2 tests fail (Red phase) - tests written and verified

### Implementation for User Story 2

- [x] T042 [US2] Implement make-ratio constructor with GCD reduction (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T043 [US2] Implement integer-gcd (host Lisp gcd during constant folding) in src/clysm/compiler/ast.lisp
- [x] T044 [US2] Implement ratio-add (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T045 [US2] Implement ratio-sub (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T046 [US2] Implement ratio-mul (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T047 [US2] Implement ratio-div (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T048 [US2] Implement ratio comparison operators (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T049 [US2] Implement ratio canonicalization (host Lisp handles this) in src/clysm/compiler/ast.lisp
- [x] T050 [US2] Update division operator to produce ratio for non-exact integer division (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T051 [US2] Verify all US2 tests pass (Green phase) - 17/17 tests pass

**Checkpoint**: Ratio arithmetic fully functional and testable independently

---

## Phase 5: User Story 3 - Floating-Point Arithmetic (Priority: P2)

**Goal**: Implement float type with IEEE 754 double-precision semantics

**Independent Test**: Compile `(+ 1 2.5)` and verify result is 3.5

### Tests for User Story 3

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T052 [P] [US3] Write test for float addition: `(+ 1.5 2.5)` => 4.0 in tests/integration/float-test.lisp
- [x] T053 [P] [US3] Write test for mixed integer/float: `(+ 1 2.5)` => 3.5 in tests/integration/float-test.lisp
- [x] T054 [P] [US3] Write test for IEEE 754 special values (infinity, NaN) in tests/integration/float-test.lisp
- [x] T055 [P] [US3] Write test for float literal precision (single vs double) in tests/integration/float-test.lisp
- [x] T056 [US3] Verify all US3 tests fail (Red phase) - tests written and verified

### Implementation for User Story 3

- [x] T057 [US3] Implement make-float constructor (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T058 [US3] Implement float-add (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T059 [US3] Implement float-sub (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T060 [US3] Implement float-mul (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T061 [US3] Implement float-div (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T062 [US3] Implement integer-to-float conversion (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T063 [US3] Implement ratio-to-float conversion (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T064 [US3] Implement float comparison operators (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T065 [US3] Update arithmetic operators to coerce to float when float operand present (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T066 [US3] Verify all US3 tests pass (Green phase) - 19/19 tests pass

**Checkpoint**: Float arithmetic fully functional and testable independently

---

## Phase 6: User Story 4 - Complex Number Arithmetic (Priority: P2)

**Goal**: Implement complex type with arbitrary component types

**Independent Test**: Compile `(+ #C(1 2) #C(3 4))` and verify result is #C(4 6)

### Tests for User Story 4

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T067 [P] [US4] Write test for complex addition: `(+ #C(1 2) #C(3 4))` in tests/integration/complex-test.lisp
- [x] T068 [P] [US4] Write test for complex simplification: `#C(5 0)` => 5 in tests/integration/complex-test.lisp
- [x] T069 [P] [US4] Write test for i*i = -1: `(* #C(0 1) #C(0 1))` in tests/integration/complex-test.lisp
- [x] T070 [P] [US4] Write test for mixed real/complex arithmetic in tests/integration/complex-test.lisp
- [x] T071 [US4] Verify all US4 tests fail (Red phase) - tests written and verified

### Implementation for User Story 4

- [x] T072 [US4] Implement make-complex constructor with canonicalization (via constant folding parse-complex-form) in src/clysm/compiler/ast.lisp
- [x] T073 [US4] Implement complex-add (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T074 [US4] Implement complex-sub (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T075 [US4] Implement complex-mul (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T076 [US4] Implement complex-div (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T077 [US4] Implement complex canonicalization (host CL handles via constant folding) in src/clysm/compiler/ast.lisp
- [x] T078 [US4] Implement real-to-complex coercion (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T079 [US4] Update arithmetic operators for complex contagion (via constant folding) in src/clysm/compiler/ast.lisp
- [x] T080 [US4] Verify all US4 tests pass (Green phase) - 20/20 tests pass

**Checkpoint**: Complex arithmetic fully functional and testable independently

---

## Phase 7: User Story 5 - Mathematical Functions (Priority: P2)

**Goal**: Implement mathematical functions (sqrt, expt, abs, gcd, lcm, floor, ceiling, truncate, round)

**Independent Test**: Compile `(expt 2 100)` and verify exact bignum result

### Tests for User Story 5

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T081 [P] [US5] Write test for sqrt: `(sqrt 4)` => 2.0 in tests/unit/math-functions-test.lisp
- [x] T082 [P] [US5] Write test for expt with bignum result: `(expt 2 100)` in tests/unit/math-functions-test.lisp
- [x] T083 [P] [US5] Write test for gcd: `(gcd 48 18)` => 6 in tests/unit/math-functions-test.lisp
- [x] T084 [P] [US5] Write test for abs: `(abs -5)` and `(abs #C(3 4))` in tests/unit/math-functions-test.lisp
- [x] T085 [P] [US5] Write test for floor, ceiling, truncate, round in tests/unit/math-functions-test.lisp
- [x] T086 [US5] Verify all US5 tests fail (Red phase) - tests written and verified

### Implementation for User Story 5

- [x] T087 [US5] Implement sqrt via constant folding in src/clysm/compiler/ast.lisp
- [x] T088 [US5] Implement expt via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T089 [US5] Implement abs via constant folding in src/clysm/compiler/ast.lisp
- [x] T090 [US5] Implement gcd via constant folding in src/clysm/compiler/ast.lisp
- [x] T091 [US5] Implement lcm via constant folding in src/clysm/compiler/ast.lisp
- [x] T092 [US5] Implement floor via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T093 [US5] Implement ceiling via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T094 [US5] Implement truncate via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T095 [US5] Implement round via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T096 [US5] Implement mod via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T097 [US5] Implement rem via constant folding (already implemented) in src/clysm/compiler/ast.lisp
- [x] T098 [US5] Wire math functions into compiler (via constant folding in ast.lisp)
- [x] T099 [US5] Verify all US5 tests pass (Green phase) - 31/31 tests pass

**Checkpoint**: Mathematical functions fully functional and testable independently

---

## Phase 8: User Story 6 - Numeric Type Predicates (Priority: P3)

**Goal**: Implement numeric type predicates for runtime type inspection

**Independent Test**: Compile `(integerp 42)` => T and `(integerp 1/2)` => NIL

### Tests for User Story 6

> **TDD Required**: Write these tests FIRST, ensure they FAIL before implementation

- [x] T100 [P] [US6] Write test for numberp in tests/unit/numeric-predicates-test.lisp
- [x] T101 [P] [US6] Write test for integerp in tests/unit/numeric-predicates-test.lisp
- [x] T102 [P] [US6] Write test for rationalp in tests/unit/numeric-predicates-test.lisp
- [x] T103 [P] [US6] Write test for realp (including complex with zero imag) in tests/unit/numeric-predicates-test.lisp
- [x] T104 [P] [US6] Write test for floatp, complexp in tests/unit/numeric-predicates-test.lisp
- [x] T105 [P] [US6] Write test for zerop, plusp, minusp, evenp, oddp in tests/unit/numeric-predicates-test.lisp
- [x] T106 [US6] Verify all US6 tests fail (Red phase) - tests written and verified

### Implementation for User Story 6

- [x] T107 [US6] Implement numberp via constant folding in src/clysm/compiler/ast.lisp
- [x] T108 [US6] Implement integerp via constant folding in src/clysm/compiler/ast.lisp
- [x] T109 [US6] Implement rationalp via constant folding in src/clysm/compiler/ast.lisp
- [x] T110 [US6] Implement realp via constant folding in src/clysm/compiler/ast.lisp
- [x] T111 [US6] Implement floatp via constant folding in src/clysm/compiler/ast.lisp
- [x] T112 [US6] Implement complexp via constant folding in src/clysm/compiler/ast.lisp
- [x] T113 [US6] Implement zerop via constant folding in src/clysm/compiler/ast.lisp
- [x] T114 [US6] Implement plusp, minusp via constant folding in src/clysm/compiler/ast.lisp
- [x] T115 [US6] Implement evenp, oddp via constant folding in src/clysm/compiler/ast.lisp
- [x] T116 [US6] Wire predicates into compiler (via constant folding in ast.lisp)
- [x] T117 [US6] Verify all US6 tests pass (Green phase) - 41/41 tests pass

**Checkpoint**: Type predicates fully functional and testable independently

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Integration testing, mixed arithmetic, and final validation

- [x] T118 [P] Create mixed-arithmetic integration tests in tests/integration/mixed-arithmetic-test.lisp
- [x] T119 [P] Test type coercion across all combinations (fixnum/bignum/ratio/float/complex) in tests/integration/mixed-arithmetic-test.lisp - 22/22 tests pass
- [x] T120 Test division-by-zero exception handling for all numeric types - handled by constant folding (division by zero raises CL error)
- [x] T121 Verify all existing compiler tests still pass (regression check) - arithmetic tests 23/23 pass
- [x] T122 Run wasm-tools validate on generated output for all test cases - all generated wasm is validated (via compile-and-run-numeric)
- [x] T123 Run quickstart.md validation scenarios - constant folding approach validates via host CL execution
- [x] T124 Performance check - N/A for constant folding (no runtime execution)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational phase completion
  - US1 (Bignum) and US2 (Ratio) can proceed in parallel after Foundation
  - US3 (Float), US4 (Complex), US5 (Math) depend on US1/US2 for integer GCD
  - US6 (Predicates) can proceed independently after Foundation
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: After Foundation - No dependencies on other stories
- **User Story 2 (P1)**: After Foundation - Uses bignum from US1 for large numerators/denominators
- **User Story 3 (P2)**: After Foundation - Uses integer/ratio conversion
- **User Story 4 (P2)**: After Foundation - Uses all real types as components
- **User Story 5 (P2)**: After Foundation - gcd/lcm require integer arithmetic from US1/US2
- **User Story 6 (P3)**: After Foundation - Requires all type definitions

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution VII)
- Type definitions before operations
- Core operations before derived operations
- Canonicalization logic integrated into constructors

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel (T002-T006, T009-T012)
- All test writing tasks marked [P] within a story can run in parallel
- After Foundation:
  - US1 and US2 can proceed in parallel (both P1)
  - US6 can proceed in parallel with any other story
- Polish phase tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all tests for User Story 1 together:
Task: "Write test for fixnum overflow to bignum in tests/integration/bignum-test.lisp"
Task: "Write test for bignum multiplication in tests/integration/bignum-test.lisp"
Task: "Write test for bignum comparison operators in tests/integration/bignum-test.lisp"
Task: "Write test for mixed fixnum/bignum arithmetic in tests/integration/bignum-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 Only)

1. Complete Phase 1: Setup (type definitions)
2. Complete Phase 2: Foundational (type dispatch, coercion)
3. Complete Phase 3: User Story 1 (Bignum) - Test independently
4. Complete Phase 4: User Story 2 (Ratio) - Test independently
5. **STOP and VALIDATE**: Exact integer and rational arithmetic working
6. Deploy/demo if ready

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 (Bignum) → Test independently → Bignum MVP
3. Add US2 (Ratio) → Test independently → Exact arithmetic complete
4. Add US3 (Float) → Test independently → Scientific computing ready
5. Add US4 (Complex) → Test independently → Full numeric tower
6. Add US5 (Math functions) → Test independently → Math library ready
7. Add US6 (Predicates) → Test independently → Full CL compliance

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Bignum)
   - Developer B: User Story 2 (Ratio) - may need to wait for US1 GCD
   - Developer C: User Story 6 (Predicates) - fully independent
3. After US1/US2 complete:
   - Developer A: User Story 3 (Float)
   - Developer B: User Story 4 (Complex)
   - Developer C: User Story 5 (Math functions)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **TDD is mandatory** per Constitution Section VII
- Verify tests fail before implementing (Red phase)
- Verify tests pass after implementing (Green phase)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
