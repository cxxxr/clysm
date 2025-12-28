# Tasks: ANSI Numeric Functions Extension

**Input**: Design documents from `/specs/001-numeric-functions/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution VII (TDD mandate)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US6)
- Exact file paths included in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and FFI infrastructure for transcendental functions

- [x] T001 Create host-shim/math-shim.js with Math.* exports (sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, exp, log, pow, PI, E)
- [x] T002 [P] Add float tolerance helper approx= in tests/helpers.lisp with epsilon 1e-10
- [x] T003 Register math module FFI imports in src/clysm/ffi/import-gen.lisp
- [x] T004 [P] Add pi constant definition in src/clysm/lib/macros.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST complete before user story implementation

**WARNING**: No user story work can begin until this phase is complete

- [x] T005 Create test file tests/unit/basic-functions-test.lisp with package definition
- [x] T006 [P] Create test file tests/unit/trig-functions-test.lisp with package definition
- [x] T007 [P] Create test file tests/unit/bitwise-functions-test.lisp with package definition
- [x] T008 [P] Create test file tests/unit/math-functions-test.lisp with package definition (already existed)
- [x] T009 [P] Create test file tests/unit/hyperbolic-functions-test.lisp with package definition
- [x] T010 [P] Create test file tests/unit/complex-functions-test.lisp with package definition
- [x] T011 Update clysm.asd to include new test files in test module
- [x] T012 Verify existing numeric predicates tests pass with sbcl --eval "(asdf:test-system :clysm)"

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Arithmetic (Priority: P1)

**Goal**: Implement abs, signum, max, min, gcd, lcm for fundamental numeric operations

**Independent Test**: `(abs -5)` => 5, `(max 1 2 3)` => 3, `(gcd 12 18)` => 6

### Tests for User Story 1 (TDD: Write first, must FAIL)

- [x] T013 [P] [US1] Write test-abs-negative: (abs -5) => 5 in tests/unit/math-functions-test.lisp
- [x] T014 [P] [US1] Write test-abs-positive: (abs 5) => 5 in tests/unit/math-functions-test.lisp
- [x] T015 [P] [US1] Write test-abs-float: (abs -3.14) => 3.14 in tests/unit/math-functions-test.lisp
- [x] T016 [P] [US1] Write test-signum-negative: (signum -5) => -1 in tests/unit/basic-functions-test.lisp
- [x] T017 [P] [US1] Write test-signum-zero: (signum 0) => 0 in tests/unit/basic-functions-test.lisp
- [x] T018 [P] [US1] Write test-signum-positive: (signum 5) => 1 in tests/unit/basic-functions-test.lisp
- [x] T019 [P] [US1] Write test-max-multiple: (max 1 2 3) => 3 in tests/unit/basic-functions-test.lisp
- [x] T020 [P] [US1] Write test-min-multiple: (min 1 2 3) => 1 in tests/unit/basic-functions-test.lisp
- [x] T021 [P] [US1] Write test-gcd-basic: (gcd 48 18) => 6 in tests/unit/math-functions-test.lisp
- [x] T022 [P] [US1] Write test-gcd-zero: (gcd 0 5) => 5 in tests/unit/math-functions-test.lisp
- [x] T023 [P] [US1] Write test-lcm-basic: (lcm 4 6) => 12 in tests/unit/math-functions-test.lisp
- [x] T024 [P] [US1] Write test-lcm-multiple: (lcm 2 3 5) => 30 in tests/unit/math-functions-test.lisp

### Implementation for User Story 1

- [x] T025 [US1] Implement compile-abs in src/clysm/compiler/codegen/func-section.lisp using i31 conditional or f64.abs
- [x] T026 [US1] Add abs case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T027 [US1] Implement compile-signum in src/clysm/compiler/codegen/func-section.lisp using comparison chain (already existed)
- [x] T028 [US1] Add signum case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp (already existed)
- [x] T029 [US1] Implement compile-max in src/clysm/compiler/codegen/func-section.lisp with pairwise comparison
- [x] T030 [US1] Add max case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T031 [US1] Implement compile-min in src/clysm/compiler/codegen/func-section.lisp with pairwise comparison
- [x] T032 [US1] Add min case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T033 [US1] Implement compile-gcd in src/clysm/compiler/codegen/func-section.lisp using Euclidean algorithm
- [x] T034 [US1] Add gcd case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T035 [US1] Implement compile-lcm in src/clysm/compiler/codegen/func-section.lisp using |a*b|/gcd(a,b)
- [x] T036 [US1] Add lcm case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T037 [US1] Verify all US1 tests pass

**Checkpoint**: User Story 1 complete - basic arithmetic functions work independently

---

## Phase 4: User Story 3 - Bitwise Operations (Priority: P1)

**Goal**: Implement logcount and integer-length (ash, logand, logior, logxor, lognot already exist)

**Independent Test**: `(ash 1 10)` => 1024, `(logand #xFF00 #x0FF0)` => #x0F00, `(logcount 13)` => 3

### Tests for User Story 3 (TDD: Write first, must FAIL)

- [x] T038 [P] [US3] Write test-ash-left: (ash 1 10) => 1024 in tests/unit/bitwise-functions-test.lisp
- [x] T039 [P] [US3] Write test-ash-right: (ash 1024 -10) => 1 in tests/unit/bitwise-functions-test.lisp
- [x] T040 [P] [US3] Write test-logand: (logand #xFF00 #x0FF0) => #x0F00 in tests/unit/bitwise-functions-test.lisp
- [x] T041 [P] [US3] Write test-logior: (logior #xFF00 #x00FF) => #xFFFF in tests/unit/bitwise-functions-test.lisp
- [x] T042 [P] [US3] Write test-logxor: (logxor #xFF00 #x0FF0) => #xF0F0 in tests/unit/bitwise-functions-test.lisp
- [x] T043 [P] [US3] Write test-lognot: (lognot 0) => -1 in tests/unit/bitwise-functions-test.lisp
- [x] T044 [P] [US3] Write test-logcount-positive: (logcount 13) => 3 in tests/unit/bitwise-functions-test.lisp
- [x] T045 [P] [US3] Write test-logcount-negative: (logcount -1) => 0 in tests/unit/bitwise-functions-test.lisp
- [x] T046 [P] [US3] Write test-integer-length-zero: (integer-length 0) => 0 in tests/unit/bitwise-functions-test.lisp
- [x] T047 [P] [US3] Write test-integer-length-positive: (integer-length 1024) => 11 in tests/unit/bitwise-functions-test.lisp

### Implementation for User Story 3

- [x] T048 [US3] Verify existing ash, logand, logior, logxor, lognot tests pass
- [x] T049 [US3] Implement compile-logcount in src/clysm/compiler/codegen/func-section.lisp using i32.popcnt
- [x] T050 [US3] Add logcount case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T051 [US3] Implement compile-integer-length in src/clysm/compiler/codegen/func-section.lisp using (32 - i32.clz)
- [x] T052 [US3] Add integer-length case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T053 [US3] Verify all US3 tests pass

**Checkpoint**: User Story 3 complete - all bitwise operations work independently

---

## Phase 5: User Story 6 - Complex Number Support (Priority: P3, but needed for US2/US4)

**Goal**: Implement complex, realpart, imagpart, conjugate, phase (required for sqrt(-1) etc.)

**Independent Test**: `(realpart #C(3 4))` => 3, `(imagpart #C(3 4))` => 4

**Note**: Moved earlier because trigonometric and math functions need complex for edge cases

### Tests for User Story 6 (TDD: Write first, must FAIL)

- [x] T054 [P] [US6] Write test-complex-creation: (complex 3 4) in tests/unit/complex-functions-test.lisp
- [x] T055 [P] [US6] Write test-realpart: (realpart #C(3 4)) => 3 in tests/unit/complex-functions-test.lisp
- [x] T056 [P] [US6] Write test-imagpart: (imagpart #C(3 4)) => 4 in tests/unit/complex-functions-test.lisp
- [x] T057 [P] [US6] Write test-realpart-real: (realpart 5) => 5 in tests/unit/complex-functions-test.lisp
- [x] T058 [P] [US6] Write test-imagpart-real: (imagpart 5) => 0 in tests/unit/complex-functions-test.lisp
- [x] T059 [P] [US6] Write test-conjugate: (conjugate #C(3 4)) => #C(3 -4) in tests/unit/complex-functions-test.lisp
- [x] T060 [P] [US6] Write test-phase: (phase #C(0 1)) approx pi/2 in tests/unit/complex-functions-test.lisp

### Implementation for User Story 6

- [x] T061 [US6] Implement compile-complex in src/clysm/compiler/codegen/func-section.lisp using struct.new $complex
- [x] T062 [US6] Add complex case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T063 [US6] Implement compile-realpart in src/clysm/compiler/codegen/func-section.lisp with type dispatch
- [x] T064 [US6] Add realpart case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T065 [US6] Implement compile-imagpart in src/clysm/compiler/codegen/func-section.lisp with type dispatch
- [x] T066 [US6] Add imagpart case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T067 [US6] Implement compile-conjugate in src/clysm/compiler/codegen/func-section.lisp
- [x] T068 [US6] Add conjugate case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T069 [US6] Implement compile-phase in src/clysm/compiler/codegen/func-section.lisp using atan2 import
- [x] T070 [US6] Add phase case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T071 [US6] Verify all US6 tests pass

**Checkpoint**: User Story 6 complete - complex number infrastructure ready for trig/math functions

---

## Phase 6: User Story 2 - Trigonometric Functions (Priority: P1)

**Goal**: Implement sin, cos, tan, asin, acos, atan using FFI imports from host

**Independent Test**: `(sin (/ pi 2))` => ~1.0, `(cos 0)` => ~1.0

### Tests for User Story 2 (TDD: Write first, must FAIL)

- [x] T072 [P] [US2] Write test-sin-zero: (sin 0) approx 0.0 in tests/unit/trig-functions-test.lisp
- [x] T073 [P] [US2] Write test-sin-pi-half: (sin (/ pi 2)) approx 1.0 in tests/unit/trig-functions-test.lisp
- [x] T074 [P] [US2] Write test-cos-zero: (cos 0) approx 1.0 in tests/unit/trig-functions-test.lisp
- [x] T075 [P] [US2] Write test-cos-pi: (cos pi) approx -1.0 in tests/unit/trig-functions-test.lisp
- [x] T076 [P] [US2] Write test-tan-zero: (tan 0) approx 0.0 in tests/unit/trig-functions-test.lisp
- [x] T077 [P] [US2] Write test-tan-pi-quarter: (tan (/ pi 4)) approx 1.0 in tests/unit/trig-functions-test.lisp
- [x] T078 [P] [US2] Write test-asin-zero: (asin 0) approx 0.0 in tests/unit/trig-functions-test.lisp
- [x] T079 [P] [US2] Write test-asin-one: (asin 1) approx pi/2 in tests/unit/trig-functions-test.lisp
- [x] T080 [P] [US2] Write test-acos-one: (acos 1) approx 0.0 in tests/unit/trig-functions-test.lisp
- [x] T081 [P] [US2] Write test-acos-zero: (acos 0) approx pi/2 in tests/unit/trig-functions-test.lisp
- [x] T082 [P] [US2] Write test-atan-one: (atan 1) approx pi/4 in tests/unit/trig-functions-test.lisp
- [x] T083 [P] [US2] Write test-atan2: (atan 1 1) approx pi/4 in tests/unit/trig-functions-test.lisp

### Implementation for User Story 2

- [x] T084 [US2] Implement compile-sin in src/clysm/compiler/codegen/func-section.lisp calling math.sin import
- [x] T085 [US2] Add sin case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T086 [US2] Implement compile-cos in src/clysm/compiler/codegen/func-section.lisp calling math.cos import
- [x] T087 [US2] Add cos case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T088 [US2] Implement compile-tan in src/clysm/compiler/codegen/func-section.lisp calling math.tan import
- [x] T089 [US2] Add tan case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T090 [US2] Implement compile-asin in src/clysm/compiler/codegen/func-section.lisp with complex promotion for |x|>1
- [x] T091 [US2] Add asin case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T092 [US2] Implement compile-acos in src/clysm/compiler/codegen/func-section.lisp with complex promotion for |x|>1
- [x] T093 [US2] Add acos case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T094 [US2] Implement compile-atan in src/clysm/compiler/codegen/func-section.lisp supporting 1 and 2 arg forms
- [x] T095 [US2] Add atan case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T096 [US2] Verify all US2 tests pass

**Checkpoint**: User Story 2 complete - trigonometric functions work independently

---

## Phase 7: User Story 4 - Mathematical Functions (Priority: P2)

**Goal**: Implement exp, log, sqrt, expt for exponential and power calculations

**Independent Test**: `(exp 1)` => ~e, `(sqrt 4)` => 2.0, `(expt 2 10)` => 1024

### Tests for User Story 4 (TDD: Write first, must FAIL)

- [x] T097 [P] [US4] Write test-sqrt-perfect-square: (sqrt 4) => 2.0 in tests/unit/math-functions-test.lisp
- [x] T098 [P] [US4] Write test-sqrt-non-perfect-square: (sqrt 2) approx 1.414 in tests/unit/math-functions-test.lisp
- [x] T099 [P] [US4] Write test-sqrt-float: (sqrt 2.25) => 1.5 in tests/unit/math-functions-test.lisp
- [x] T100 [P] [US4] Write test-sqrt-negative: (sqrt -1) => complex in tests/unit/math-functions-test.lisp
- [x] T101 [P] [US4] Write test-expt-integer: (expt 2 10) => 1024 in tests/unit/math-functions-test.lisp
- [x] T102 [P] [US4] Write test-expt-bignum: (expt 2 100) => bignum in tests/unit/math-functions-test.lisp
- [x] T103 [P] [US4] Write test-expt-fractional: (expt 8 1/3) approx 2.0 in tests/unit/math-functions-test.lisp
- [x] T104 [P] [US4] Write test-expt-float: (expt 2.0 3) => 8.0 in tests/unit/math-functions-test.lisp
- [x] T105 [P] [US4] (Reserved - existing tests cover exp/log)

### Implementation for User Story 4

- [x] T106 [US4] Implement compile-exp in src/clysm/compiler/codegen/func-section.lisp calling math.exp import
- [x] T107 [US4] Add exp case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T108 [US4] Implement compile-log in src/clysm/compiler/codegen/func-section.lisp with optional base using math.log
- [x] T109 [US4] Add log case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T110 [US4] Implement compile-sqrt in src/clysm/compiler/codegen/func-section.lisp with f64.sqrt and complex promotion
- [x] T111 [US4] Add sqrt case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T112 [US4] Implement compile-expt in src/clysm/compiler/codegen/func-section.lisp using math.pow
- [x] T113 [US4] Add expt case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T114 [US4] Verify all US4 tests pass

**Checkpoint**: User Story 4 complete - mathematical functions work independently

---

## Phase 8: User Story 5 - Hyperbolic Functions (Priority: P2)

**Goal**: Implement sinh, cosh, tanh, asinh, acosh, atanh for scientific applications

**Independent Test**: `(sinh 0)` => 0.0, `(cosh 0)` => 1.0

### Tests for User Story 5 (TDD: Write first, must FAIL)

- [x] T115 [P] [US5] Write test-sinh-zero: (sinh 0) approx 0.0 in tests/unit/hyperbolic-functions-test.lisp
- [x] T116 [P] [US5] Write test-cosh-zero: (cosh 0) approx 1.0 in tests/unit/hyperbolic-functions-test.lisp
- [x] T117 [P] [US5] Write test-tanh-zero: (tanh 0) approx 0.0 in tests/unit/hyperbolic-functions-test.lisp
- [x] T118 [P] [US5] Write test-asinh-zero: (asinh 0) approx 0.0 in tests/unit/hyperbolic-functions-test.lisp
- [x] T119 [P] [US5] Write test-acosh-one: (acosh 1) approx 0.0 in tests/unit/hyperbolic-functions-test.lisp
- [x] T120 [P] [US5] Write test-atanh-zero: (atanh 0) approx 0.0 in tests/unit/hyperbolic-functions-test.lisp

### Implementation for User Story 5

- [x] T121 [US5] Implement compile-sinh in src/clysm/compiler/codegen/func-section.lisp calling math.sinh import
- [x] T122 [US5] Add sinh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T123 [US5] Implement compile-cosh in src/clysm/compiler/codegen/func-section.lisp calling math.cosh import
- [x] T124 [US5] Add cosh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T125 [US5] Implement compile-tanh in src/clysm/compiler/codegen/func-section.lisp calling math.tanh import
- [x] T126 [US5] Add tanh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T127 [US5] Implement compile-asinh in src/clysm/compiler/codegen/func-section.lisp calling math.asinh import
- [x] T128 [US5] Add asinh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T129 [US5] Implement compile-acosh in src/clysm/compiler/codegen/func-section.lisp calling math.acosh import
- [x] T130 [US5] Add acosh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T131 [US5] Implement compile-atanh in src/clysm/compiler/codegen/func-section.lisp calling math.atanh import
- [x] T132 [US5] Add atanh case to compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [x] T133 [US5] Verify all US5 tests pass

**Checkpoint**: User Story 5 complete - hyperbolic functions work independently

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Edge cases, validation, and integration testing

- [x] T134 [P] Add edge case test for log(0) signaling error in tests/unit/math-functions-test.lisp
- [x] T135 [P] Add edge case test for atan(0, 0) signaling error in tests/unit/trig-functions-test.lisp
- [x] T136 [P] Add edge case test for expt(0, -1) signaling error in tests/unit/math-functions-test.lisp
- [x] T137 Add type contagion tests for mixed-type operations in tests/integration/numeric-suite-test.lisp
- [x] T138 Create ANSI numbers category compliance test suite in tests/integration/numeric-suite-test.lisp
- [x] T139 Run full test suite and verify 50%+ numbers category pass rate (88.86% achieved: 2545 passed, 319 failed)
- [x] T140 Validate Wasm output with wasm-tools validate for all new functions
- [x] T141 Update host-shim/run-wasm.js to import math-shim.js
- [x] T142 Run quickstart.md verification expressions
- [x] T143 Code cleanup and documentation in src/clysm/compiler/codegen/func-section.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phases 3-8 (User Stories)**: All depend on Phase 2 completion
  - **US1 (Basic)**: No other story dependencies
  - **US3 (Bitwise)**: No other story dependencies
  - **US6 (Complex)**: No other story dependencies, but NEEDED by US2, US4
  - **US2 (Trig)**: Depends on US6 for complex edge cases
  - **US4 (Math)**: Depends on US6 for sqrt(-1) etc.
  - **US5 (Hyperbolic)**: No other story dependencies
- **Phase 9 (Polish)**: Depends on all user stories complete

### Recommended Execution Order

1. **Phase 1 + 2**: Setup and foundational (sequential)
2. **Phase 3 (US1)**: Basic arithmetic (MVP - can ship here!)
3. **Phase 4 (US3)**: Bitwise operations (parallel with Phase 3)
4. **Phase 5 (US6)**: Complex numbers (enables trig/math edge cases)
5. **Phase 6 (US2)**: Trigonometric (after US6)
6. **Phase 7 (US4)**: Mathematical (after US6)
7. **Phase 8 (US5)**: Hyperbolic (parallel with US2/US4)
8. **Phase 9**: Polish and integration

### Parallel Opportunities

#### Setup Phase (Phase 1)
```
Parallel: T002, T003, T004
```

#### Foundational Phase (Phase 2)
```
Parallel: T005, T006, T007, T008, T009, T010
```

#### User Story 1 Tests
```
Parallel: T013-T024 (all test tasks)
```

#### User Story 3 Tests
```
Parallel: T038-T047 (all test tasks)
```

#### Independent Story Development
```
After Phase 2:
  Developer A: US1 (Basic) + US3 (Bitwise)
  Developer B: US6 (Complex) → US2 (Trig) → US4 (Math)
  Developer C: US5 (Hyperbolic)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T004)
2. Complete Phase 2: Foundational (T005-T012)
3. Complete Phase 3: User Story 1 - Basic Arithmetic (T013-T037)
4. **STOP and VALIDATE**: Test independently with `(abs -5)`, `(max 1 2 3)`, `(gcd 12 18)`
5. Can demo basic arithmetic without any other stories

### 50%+ Numbers Category Target

Priority order to maximize test pass rate:
1. US1 (Basic): 6 functions, high test coverage
2. US3 (Bitwise): 7 functions (5 existing + 2 new)
3. US6 (Complex): 5 functions, enables edge cases
4. US2 (Trig): 6 functions, common tests
5. US4 (Math): 4 functions, critical for sqrt(-1)
6. US5 (Hyperbolic): 6 functions, lower priority tests

### Incremental Delivery

| Checkpoint | Stories Complete | Estimated Numbers Pass Rate |
|------------|------------------|----------------------------|
| MVP | US1 | ~20% |
| + Bitwise | US1 + US3 | ~30% |
| + Complex | US1 + US3 + US6 | ~35% |
| + Trig | + US2 | ~45% |
| + Math | + US4 | ~50%+ |
| + Hyperbolic | + US5 | ~55%+ |

---

## Summary

| Metric | Value |
|--------|-------|
| Total Tasks | 143 |
| Phase 1 (Setup) | 4 |
| Phase 2 (Foundational) | 8 |
| US1 (Basic Arithmetic) | 25 |
| US3 (Bitwise Operations) | 16 |
| US6 (Complex Numbers) | 18 |
| US2 (Trigonometric) | 25 |
| US4 (Mathematical) | 18 |
| US5 (Hyperbolic) | 19 |
| Phase 9 (Polish) | 10 |
| Parallel Opportunities | 80+ tasks marked [P] |
| MVP Scope | US1 only (37 tasks through T037) |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD required per Constitution VII - write tests first, ensure they fail
- Each user story independently completable and testable
- Commit after each implementation task or logical group
- Stop at any checkpoint to validate story independently
