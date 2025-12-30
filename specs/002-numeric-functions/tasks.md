# Tasks: Phase 14A - Basic Arithmetic Function Extension

**Input**: Design documents from `/specs/002-numeric-functions/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED by Constitution (Principle VII: TDD非交渉) - Write tests FIRST, verify FAIL, then implement.

**Organization**: Tasks grouped by user story for independent implementation. 6 user stories, 26 functions total.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1-US6)
- All paths are absolute from repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify existing infrastructure and prepare for new function implementation

- [X] T001 Verify FFI math imports exist in src/clysm/ffi/import-gen.lisp (sin, cos, tan, etc. in *math-function-specs*)
- [X] T002 [P] Verify host-shim/math-shim.js exports all required Math API bindings
- [X] T003 [P] Add atan2 to *math-function-specs* if missing in src/clysm/ffi/import-gen.lisp
- [X] T004 Define PI constant (3.141592653589793) in src/clysm/lib/constants.lisp or src/clysm/lib/macros.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story

**⚠️ CRITICAL**: All user stories depend on this phase

- [X] T005 Create helper function `compile-ffi-math-call` for FFI dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T006 [P] Create helper function `compile-to-f64` for float coercion in src/clysm/compiler/codegen/func-section.lisp
- [X] T007 [P] Create helper function `emit-wrap-f64` for float boxing in src/clysm/compiler/codegen/func-section.lisp
- [X] T008 [P] Create test helper `compile-and-run-numeric` in tests/unit/test-helpers.lisp
- [X] T009 [P] Create test helper `approx=` for floating-point comparison in tests/unit/test-helpers.lisp
- [X] T010 Extend `compile-call` dispatch table to route new function names in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 1 - Trigonometric Functions (Priority: P1) MVP

**Goal**: Implement sin, cos, tan, asin, acos, atan for graphics and scientific computing

**Independent Test**: `(sin (/ pi 2))` returns approximately 1.0

### Tests for User Story 1 (TDD - Write FIRST, verify FAIL)

- [X] T011 [P] [US1] Add test-sin-basic in tests/unit/trig-functions-test.lisp: (sin 0)=0, (sin (/ pi 2))≈1
- [X] T012 [P] [US1] Add test-cos-basic in tests/unit/trig-functions-test.lisp: (cos 0)=1, (cos pi)≈-1
- [X] T013 [P] [US1] Add test-tan-basic in tests/unit/trig-functions-test.lisp: (tan 0)=0, (tan (/ pi 4))≈1
- [X] T014 [P] [US1] Add test-asin-basic in tests/unit/trig-functions-test.lisp: (asin 0)=0, (asin 1)≈pi/2
- [X] T015 [P] [US1] Add test-acos-basic in tests/unit/trig-functions-test.lisp: (acos 1)=0, (acos 0)≈pi/2
- [X] T016 [P] [US1] Add test-atan-basic in tests/unit/trig-functions-test.lisp: (atan 0)=0, (atan 1)≈pi/4, (atan 1 1)≈pi/4

### Implementation for User Story 1

- [X] T017 [P] [US1] Implement compile-sin in src/clysm/compiler/codegen/func-section.lisp
- [X] T018 [P] [US1] Implement compile-cos in src/clysm/compiler/codegen/func-section.lisp
- [X] T019 [P] [US1] Implement compile-tan in src/clysm/compiler/codegen/func-section.lisp
- [X] T020 [P] [US1] Implement compile-asin in src/clysm/compiler/codegen/func-section.lisp
- [X] T021 [P] [US1] Implement compile-acos in src/clysm/compiler/codegen/func-section.lisp
- [X] T022 [US1] Implement compile-atan with 1/2-arg dispatch (uses atan2) in src/clysm/compiler/codegen/func-section.lisp
- [X] T023 [US1] Register sin, cos, tan, asin, acos, atan in compile-call dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T024 [US1] Verify all trig tests pass: sbcl --eval "(asdf:test-system :clysm/trig)"

**Checkpoint**: User Story 1 complete - trigonometric functions working

---

## Phase 4: User Story 2 - Bit Operation Functions (Priority: P1)

**Goal**: Implement ash, logand, logior, logxor, lognot, logcount for systems programming

**Independent Test**: `(ash 1 10)` returns 1024

### Tests for User Story 2 (TDD - Write FIRST, verify FAIL)

- [X] T025 [P] [US2] Add test-ash-basic in tests/unit/bit-functions-test.lisp: (ash 1 10)=1024, (ash 1024 -10)=1
- [X] T026 [P] [US2] Add test-ash-negative in tests/unit/bit-functions-test.lisp: (ash -1 10)=-1024
- [X] T027 [P] [US2] Add test-logand-basic in tests/unit/bit-functions-test.lisp: (logand)=-1, (logand #xFF #x0F)=15
- [X] T028 [P] [US2] Add test-logior-basic in tests/unit/bit-functions-test.lisp: (logior)=0, (logior #xF0 #x0F)=255
- [X] T029 [P] [US2] Add test-logxor-basic in tests/unit/bit-functions-test.lisp: (logxor)=0, (logxor #xFF #x0F)=240
- [X] T030 [P] [US2] Add test-lognot-basic in tests/unit/bit-functions-test.lisp: (lognot 0)=-1, (lognot -1)=0
- [X] T031 [P] [US2] Add test-logcount-basic in tests/unit/bit-functions-test.lisp: (logcount #xFF)=8, (logcount -1)=0

### Implementation for User Story 2

- [X] T032 [US2] Implement compile-ash with left/right shift dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T033 [P] [US2] Implement compile-logand (variadic, identity -1) in src/clysm/compiler/codegen/func-section.lisp
- [X] T034 [P] [US2] Implement compile-logior (variadic, identity 0) in src/clysm/compiler/codegen/func-section.lisp
- [X] T035 [P] [US2] Implement compile-logxor (variadic, identity 0) in src/clysm/compiler/codegen/func-section.lisp
- [X] T036 [US2] Verify existing lognot works or fix in src/clysm/compiler/codegen/func-section.lisp
- [X] T037 [US2] Implement compile-logcount using i32.popcnt in src/clysm/compiler/codegen/func-section.lisp
- [X] T038 [US2] Register ash, logand, logior, logxor, lognot, logcount in compile-call dispatch
- [X] T039 [US2] Verify all bit tests pass: sbcl --eval "(asdf:test-system :clysm/bit)"

**Checkpoint**: User Story 2 complete - bit operations working

---

## Phase 5: User Story 3 - Mathematical Functions (Priority: P2)

**Goal**: Implement exp, log, sqrt, expt, abs, signum for numerical computing

**Independent Test**: `(sqrt 4)` returns 2.0

### Tests for User Story 3 (TDD - Write FIRST, verify FAIL)

- [X] T040 [P] [US3] Add test-sqrt-basic in tests/unit/math-functions-test.lisp: (sqrt 4)=2.0, (sqrt 2)≈1.414
- [X] T041 [P] [US3] Add test-exp-basic in tests/unit/math-functions-test.lisp: (exp 0)=1, (exp 1)≈2.718
- [X] T042 [P] [US3] Add test-log-basic in tests/unit/math-functions-test.lisp: (log 1)=0, (log (exp 1))≈1
- [X] T043 [P] [US3] Add test-log-base in tests/unit/math-functions-test.lisp: (log 100 10)≈2, (log 8 2)≈3
- [X] T044 [P] [US3] Add test-expt-basic in tests/unit/math-functions-test.lisp: (expt 2 10)=1024, (expt 0 0)=1
- [X] T045 [P] [US3] Add test-abs-basic in tests/unit/math-functions-test.lisp: (abs -5)=5, (abs 5)=5
- [X] T046 [P] [US3] Add test-signum-basic in tests/unit/math-functions-test.lisp: (signum -42)=-1, (signum 42)=1

### Implementation for User Story 3

- [X] T047 [P] [US3] Implement compile-sqrt using f64.sqrt in src/clysm/compiler/codegen/func-section.lisp
- [X] T048 [P] [US3] Implement compile-exp via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T049 [US3] Implement compile-log with 1/2-arg dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T050 [P] [US3] Implement compile-expt via FFI (Math.pow) in src/clysm/compiler/codegen/func-section.lisp
- [X] T051 [US3] Implement compile-abs with type dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T052 [US3] Implement compile-signum with type dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T053 [US3] Register exp, log, sqrt, expt, abs, signum in compile-call dispatch
- [X] T054 [US3] Verify all math tests pass: sbcl --eval "(asdf:test-system :clysm/math)"

**Checkpoint**: User Story 3 complete - mathematical functions working

---

## Phase 6: User Story 4 - Hyperbolic Functions (Priority: P2)

**Goal**: Implement sinh, cosh, tanh, asinh, acosh, atanh for engineering calculations

**Independent Test**: `(cosh 0)` returns 1.0

### Tests for User Story 4 (TDD - Write FIRST, verify FAIL)

- [X] T055 [P] [US4] Add test-sinh-basic in tests/unit/hyperbolic-test.lisp: (sinh 0)=0, (sinh 1)≈1.175
- [X] T056 [P] [US4] Add test-cosh-basic in tests/unit/hyperbolic-test.lisp: (cosh 0)=1, (cosh 1)≈1.543
- [X] T057 [P] [US4] Add test-tanh-basic in tests/unit/hyperbolic-test.lisp: (tanh 0)=0
- [X] T058 [P] [US4] Add test-asinh-basic in tests/unit/hyperbolic-test.lisp: (asinh 0)=0
- [X] T059 [P] [US4] Add test-acosh-basic in tests/unit/hyperbolic-test.lisp: (acosh 1)=0
- [X] T060 [P] [US4] Add test-atanh-basic in tests/unit/hyperbolic-test.lisp: (atanh 0)=0

### Implementation for User Story 4

- [X] T061 [P] [US4] Implement compile-sinh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T062 [P] [US4] Implement compile-cosh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T063 [P] [US4] Implement compile-tanh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T064 [P] [US4] Implement compile-asinh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T065 [P] [US4] Implement compile-acosh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T066 [P] [US4] Implement compile-atanh via FFI in src/clysm/compiler/codegen/func-section.lisp
- [X] T067 [US4] Register sinh, cosh, tanh, asinh, acosh, atanh in compile-call dispatch
- [X] T068 [US4] Verify all hyperbolic tests pass

**Checkpoint**: User Story 4 complete - hyperbolic functions working

---

## Phase 7: User Story 5 - Numeric Type Conversion (Priority: P3)

**Goal**: Implement float, rational for type interoperability

**Independent Test**: `(float 5)` returns 5.0

### Tests for User Story 5 (TDD - Write FIRST, verify FAIL)

- [X] T069 [P] [US5] Add test-float-basic in tests/unit/conversion-test.lisp: (float 5)=5.0, (float 1/2)=0.5
- [X] T070 [P] [US5] Add test-rational-basic in tests/unit/conversion-test.lisp: (rational 0.5)=1/2

### Implementation for User Story 5

- [X] T071 [US5] Implement compile-float with type dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T072 [US5] Implement compile-rational with continued fraction in src/clysm/compiler/codegen/func-section.lisp
- [X] T073 [US5] Register float, rational in compile-call dispatch
- [X] T074 [US5] Verify all conversion tests pass

**Checkpoint**: User Story 5 complete - type conversion working

---

## Phase 8: User Story 6 - Parse Integer (Priority: P3)

**Goal**: Implement parse-integer with full ANSI CL compliance

**Independent Test**: `(parse-integer "123")` returns (values 123 3)

### Tests for User Story 6 (TDD - Write FIRST, verify FAIL)

- [X] T075 [P] [US6] Add test-parse-integer-basic in tests/unit/parse-integer-test.lisp: "123"→123
- [X] T076 [P] [US6] Add test-parse-integer-radix in tests/unit/parse-integer-test.lisp: "FF":radix16→255
- [X] T077 [P] [US6] Add test-parse-integer-sign in tests/unit/parse-integer-test.lisp: "-789"→-789, "+42"→42
- [X] T078 [P] [US6] Add test-parse-integer-whitespace in tests/unit/parse-integer-test.lisp: "  42  "→42
- [X] T079 [P] [US6] Add test-parse-integer-junk in tests/unit/parse-integer-test.lisp: "abc":junk-allowed→NIL
- [X] T080 [P] [US6] Add test-parse-integer-bounds in tests/unit/parse-integer-test.lisp: :start 1 :end 2

### Implementation for User Story 6

- [X] T081 [US6] Define parse-integer state machine constants in src/clysm/compiler/codegen/func-section.lisp
- [X] T082 [US6] Implement radix-digit-value helper in src/clysm/compiler/codegen/func-section.lisp
- [X] T083 [US6] Implement parse-integer core loop in src/clysm/compiler/codegen/func-section.lisp
- [X] T084 [US6] Implement keyword argument handling (:start :end :radix :junk-allowed) in src/clysm/compiler/codegen/func-section.lisp
- [X] T085 [US6] Implement compile-parse-integer in src/clysm/compiler/codegen/func-section.lisp
- [X] T086 [US6] Register parse-integer in compile-call dispatch
- [X] T087 [US6] Verify all parse-integer tests pass

**Checkpoint**: User Story 6 complete - parse-integer working

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Validation, documentation, and integration testing

- [X] T088 [P] Run wasm-tools validate on all generated test modules
- [X] T089 [P] Add edge case tests for NaN/Infinity inputs in tests/unit/edge-cases-test.lisp
- [X] T090 [P] Add negative number tests for bit operations in tests/unit/bit-edge-cases-test.lisp
- [X] T091 Create integration test suite in tests/integration/numeric-compliance-test.lisp
- [X] T092 Measure ANSI CL numeric test compliance rate (target: 50%+) - ACHIEVED: 100% (27/27)
- [X] T093 Update CLAUDE.md with new function documentation
- [X] T094 Run quickstart.md validation: verify (sin (/ pi 2))≈1, (ash 1 10)=1024, (sqrt 4)=2

---

## Dependencies & Execution Order

### Phase Dependencies

```
Setup (Phase 1) ─────────────────────┐
                                     ▼
Foundational (Phase 2) ──────────────┤
                                     │
    ┌────────────┬───────────────────┼───────────────────┬────────────┐
    ▼            ▼                   │                   ▼            ▼
US1: Trig    US2: Bit              │               US3: Math    US4: Hyper
 (P1)         (P1)                  │                (P2)         (P2)
    │            │                   │                   │            │
    └────────────┴───────────────────┼───────────────────┴────────────┘
                                     │
    ┌────────────┬───────────────────┘
    ▼            ▼
US5: Conv    US6: Parse
 (P3)         (P3)
    │            │
    └────────────┴─────────▶ Polish (Phase 9)
```

### User Story Dependencies

- **US1 (Trig)**: Foundational only - no cross-story dependencies
- **US2 (Bit)**: Foundational only - no cross-story dependencies
- **US3 (Math)**: Foundational only - independent of US1/US2
- **US4 (Hyper)**: Foundational only - independent of US1-US3
- **US5 (Conv)**: Foundational only - independent
- **US6 (Parse)**: Foundational + may use conversion functions

### Parallel Opportunities by Story

**US1 (Trig)**: T011-T016 tests parallel, T017-T021 impl parallel
**US2 (Bit)**: T025-T031 tests parallel, T033-T035 impl parallel
**US3 (Math)**: T040-T046 tests parallel, T047-T050 impl parallel
**US4 (Hyper)**: T055-T060 tests parallel, T061-T066 impl parallel
**US5 (Conv)**: T069-T070 tests parallel
**US6 (Parse)**: T075-T080 tests parallel

---

## Parallel Execution Examples

### User Story 1 - Tests (Launch together)
```bash
# All trig tests can be written simultaneously:
T011: test-sin-basic in tests/unit/trig-functions-test.lisp
T012: test-cos-basic in tests/unit/trig-functions-test.lisp
T013: test-tan-basic in tests/unit/trig-functions-test.lisp
T014: test-asin-basic in tests/unit/trig-functions-test.lisp
T015: test-acos-basic in tests/unit/trig-functions-test.lisp
T016: test-atan-basic in tests/unit/trig-functions-test.lisp
```

### User Story 2 - Implementation (Launch together)
```bash
# Variadic log functions can be implemented in parallel:
T033: compile-logand (identity -1)
T034: compile-logior (identity 0)
T035: compile-logxor (identity 0)
```

---

## Implementation Strategy

### MVP First (US1 + US2)

1. Complete Phase 1: Setup (T001-T004)
2. Complete Phase 2: Foundational (T005-T010)
3. Complete Phase 3: US1 Trig (T011-T024)
4. **VALIDATE**: `(sin (/ pi 2))` ≈ 1.0
5. Complete Phase 4: US2 Bit (T025-T039)
6. **VALIDATE**: `(ash 1 10)` = 1024

### Incremental Delivery

| Increment | Stories | Key Validation |
|-----------|---------|----------------|
| MVP | US1+US2 | sin, ash working |
| +Math | +US3 | sqrt, exp working |
| +Hyper | +US4 | sinh, cosh working |
| +Conv | +US5 | float, rational working |
| +Parse | +US6 | parse-integer working |
| Complete | All | 50%+ ANSI compliance |

---

## Summary

| Metric | Count |
|--------|-------|
| Total Tasks | 94 |
| Setup Tasks | 4 |
| Foundational Tasks | 6 |
| US1 Tasks (Trig) | 14 |
| US2 Tasks (Bit) | 15 |
| US3 Tasks (Math) | 15 |
| US4 Tasks (Hyper) | 14 |
| US5 Tasks (Conv) | 6 |
| US6 Tasks (Parse) | 13 |
| Polish Tasks | 7 |
| Parallel Opportunities | 68 tasks marked [P] |

**MVP Scope**: US1 (Trigonometric) + US2 (Bit Operations) = 29 tasks after Foundational

**Format Validation**: All 94 tasks follow checklist format with checkbox, ID, [P]/[Story] labels, and file paths.
