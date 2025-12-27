# Tasks: IEEE 754 Bit Extraction

**Input**: Design documents from `/specs/033-ieee754-bit-extraction/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/float-bits.md

**Tests**: TDD is mandated by constitution (Principle VII). Tests written first, verify failure before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Paths follow existing clysm3 structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create module skeleton and test file structure

- [ ] T001 Create float-bits module skeleton in src/clysm/lib/float-bits.lisp with package definition
- [ ] T002 [P] Create unit test file skeleton in tests/unit/float-bits-test.lisp
- [ ] T003 [P] Update src/clysm/package.lisp to export new float-bits symbols
- [ ] T004 Verify module loads without errors via ASDF

---

## Phase 2: Foundational (Detection Predicates)

**Purpose**: Portable detection functions shared by ALL user stories

**⚠️ CRITICAL**: All user stories depend on these detection predicates

### Tests for Foundation

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T005 [P] Test float-nan-p in tests/unit/float-bits-test.lisp
- [ ] T006 [P] Test float-infinity-p in tests/unit/float-bits-test.lisp
- [ ] T007 [P] Test float-negative-zero-p in tests/unit/float-bits-test.lisp
- [ ] T008 [P] Test float-subnormal-p in tests/unit/float-bits-test.lisp

### Implementation for Foundation

- [ ] T009 Implement float-nan-p in src/clysm/lib/float-bits.lisp using (/= x x) pattern
- [ ] T010 Implement float-infinity-p in src/clysm/lib/float-bits.lisp using (= x (* x 2)) pattern
- [ ] T011 [P] Implement float-positive-infinity-p in src/clysm/lib/float-bits.lisp
- [ ] T012 [P] Implement float-negative-infinity-p in src/clysm/lib/float-bits.lisp
- [ ] T013 Implement float-negative-zero-p in src/clysm/lib/float-bits.lisp using atan pattern
- [ ] T014 Implement float-subnormal-p in src/clysm/lib/float-bits.lisp checking exponent bounds

**Checkpoint**: All detection predicates pass tests. Foundation ready for user stories.

---

## Phase 3: User Story 1 - Compile Float Constants Portably (Priority: P1) 🎯 MVP

**Goal**: Portable single-float-bits and double-float-bits for normal float values

**Independent Test**: Compile `3.14d0` and verify Wasm contains `0x40091EB851EB851F`

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T015 [P] [US1] Test double-float-bits normal values in tests/unit/float-bits-test.lisp
- [ ] T016 [P] [US1] Test single-float-bits normal values in tests/unit/float-bits-test.lisp
- [ ] T017 [P] [US1] Test positive and negative zero encoding in tests/unit/float-bits-test.lisp
- [ ] T018 [US1] Contract test: double-float-bits returns correct bit patterns per contracts/float-bits.md

### Implementation for User Story 1

- [ ] T019 [US1] Implement double-float-bits for normal numbers in src/clysm/lib/float-bits.lisp using integer-decode-float
- [ ] T020 [US1] Implement single-float-bits for normal numbers in src/clysm/lib/float-bits.lisp using integer-decode-float
- [ ] T021 [US1] Handle zero values (+0.0, -0.0) in double-float-bits with sign bit preservation
- [ ] T022 [US1] Handle zero values (+0.0f0, -0.0f0) in single-float-bits with sign bit preservation
- [ ] T023 [US1] Update emit-f64 in src/clysm/compiler/compiler.lisp to use double-float-bits
- [ ] T024 [US1] Update emit-f32 in src/clysm/compiler/compiler.lisp to use single-float-bits
- [ ] T025 [US1] Remove sb-kernel:double-float-bits usage from src/clysm/compiler/compiler.lisp
- [ ] T026 [US1] Remove sb-kernel:single-float-bits usage from src/clysm/compiler/compiler.lisp

**Checkpoint**: Normal float constants compile correctly. Verify with `(compile-and-run '3.14d0)`.

---

## Phase 4: User Story 2 - Handle Special Float Values (Priority: P1)

**Goal**: Extend float-to-bits to handle infinity, NaN, and subnormal numbers

**Independent Test**: Compile `(/ 1.0d0 0.0d0)` and verify Wasm contains `0x7FF0000000000000`

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T027 [P] [US2] Test double-float-bits positive infinity in tests/unit/float-bits-test.lisp
- [ ] T028 [P] [US2] Test double-float-bits negative infinity in tests/unit/float-bits-test.lisp
- [ ] T029 [P] [US2] Test double-float-bits NaN canonicalization in tests/unit/float-bits-test.lisp
- [ ] T030 [P] [US2] Test double-float-bits subnormal values in tests/unit/float-bits-test.lisp
- [ ] T031 [P] [US2] Test single-float-bits special values (inf, NaN, subnormal) in tests/unit/float-bits-test.lisp

### Implementation for User Story 2

- [ ] T032 [US2] Extend double-float-bits to return canonical +Infinity pattern in src/clysm/lib/float-bits.lisp
- [ ] T033 [US2] Extend double-float-bits to return canonical -Infinity pattern in src/clysm/lib/float-bits.lisp
- [ ] T034 [US2] Extend double-float-bits to return canonical NaN pattern (0x7FF8000000000000) in src/clysm/lib/float-bits.lisp
- [ ] T035 [US2] Extend double-float-bits to handle subnormal numbers in src/clysm/lib/float-bits.lisp
- [ ] T036 [US2] Extend single-float-bits for +/-Infinity patterns in src/clysm/lib/float-bits.lisp
- [ ] T037 [US2] Extend single-float-bits for canonical NaN pattern (0x7FC00000) in src/clysm/lib/float-bits.lisp
- [ ] T038 [US2] Extend single-float-bits for subnormal numbers in src/clysm/lib/float-bits.lisp

**Checkpoint**: All IEEE 754 special values encode correctly. Verify edge cases from data-model.md.

---

## Phase 5: User Story 3 - Safe Compile-Time Arithmetic (Priority: P2)

**Goal**: Replace sb-int:with-float-traps-masked with portable with-safe-float-ops

**Independent Test**: Compile `(+ (/ 1.0 0.0) 1.0)` without error, producing infinity

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T039 [P] [US3] Test with-safe-float-ops handles division-by-zero in tests/unit/float-bits-test.lisp
- [ ] T040 [P] [US3] Test with-safe-float-ops handles invalid operation (0/0 = NaN) in tests/unit/float-bits-test.lisp
- [ ] T041 [US3] Test fold-arithmetic produces infinity for division by zero in tests/unit/ast-test.lisp

### Implementation for User Story 3

- [ ] T042 [US3] Implement with-safe-float-ops macro using handler-case in src/clysm/lib/float-bits.lisp
- [ ] T043 [US3] Handle division-by-zero condition returning appropriate infinity in with-safe-float-ops
- [ ] T044 [US3] Handle floating-point-invalid-operation condition returning NaN in with-safe-float-ops
- [ ] T045 [US3] Handle floating-point-overflow condition returning infinity in with-safe-float-ops
- [ ] T046 [US3] Update fold-arithmetic in src/clysm/compiler/ast.lisp to use with-safe-float-ops
- [ ] T047 [US3] Remove sb-int:with-float-traps-masked from src/clysm/compiler/ast.lisp

**Checkpoint**: Constant folding with special values works. Verify `(/ 1.0 0.0)` folds to infinity.

---

## Phase 6: User Story 4 - Portable Test Harness (Priority: P2)

**Goal**: Update test harness to use portable float handling

**Independent Test**: Run float tests successfully (conceptually, on non-SBCL implementation)

### Tests for User Story 4

- [ ] T048 [US4] Test compile-and-run-numeric handles infinity correctly in tests/unit/helpers-test.lisp

### Implementation for User Story 4

- [ ] T049 [US4] Update compile-and-run-numeric in tests/helpers.lisp to use with-safe-float-ops
- [ ] T050 [US4] Remove sb-int:with-float-traps-masked from tests/helpers.lisp
- [ ] T051 [US4] Verify existing float tests pass with new portable implementation

**Checkpoint**: Test harness is portable. All float tests pass.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final verification, cleanup, and documentation

- [ ] T052 Verify no SBCL-specific symbols remain: grep -r "sb-kernel:.*float-bits" src/
- [ ] T053 Verify no SBCL-specific symbols remain: grep -r "sb-int:with-float-traps-masked" src/ tests/
- [ ] T054 Run full test suite to verify no regressions
- [ ] T055 Validate Wasm output with wasm-tools validate for float-heavy test cases
- [ ] T056 [P] Compare Wasm binary output before/after change for bit-identical verification
- [ ] T057 Run quickstart.md validation steps
- [ ] T058 Update package.lisp exports if needed

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational (detection predicates)
- **User Story 2 (Phase 4)**: Depends on Foundational; extends US1 functions
- **User Story 3 (Phase 5)**: Depends on Foundational
- **User Story 4 (Phase 6)**: Depends on US3 (with-safe-float-ops)
- **Polish (Phase 7)**: Depends on all user stories complete

### User Story Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (detection predicates)
    ↓
    ├── Phase 3: US1 (normal floats) ─────┐
    │       ↓                              │
    │   Phase 4: US2 (special values)      │
    │                                      │
    └── Phase 5: US3 (safe arithmetic) ←───┘
            ↓
        Phase 6: US4 (test harness)
            ↓
        Phase 7: Polish
```

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Detection predicates before encoding functions
- Core implementation before compiler integration
- Story complete before moving to next priority

### Parallel Opportunities

- T002, T003 can run in parallel (Setup phase)
- T005, T006, T007, T008 can run in parallel (Foundation tests)
- T011, T012 can run in parallel (infinity variants)
- T015, T016, T017 can run in parallel (US1 tests)
- T027, T028, T029, T030, T031 can run in parallel (US2 tests)
- T039, T040 can run in parallel (US3 tests)

---

## Parallel Example: User Story 2

```bash
# Launch all tests for User Story 2 together:
Task: "Test double-float-bits positive infinity in tests/unit/float-bits-test.lisp"
Task: "Test double-float-bits negative infinity in tests/unit/float-bits-test.lisp"
Task: "Test double-float-bits NaN canonicalization in tests/unit/float-bits-test.lisp"
Task: "Test double-float-bits subnormal values in tests/unit/float-bits-test.lisp"
Task: "Test single-float-bits special values in tests/unit/float-bits-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Verify normal float compilation works
5. Continue to Phase 4 for special values

### Incremental Delivery

1. Phase 1 + Phase 2 → Foundation ready
2. Phase 3 (US1) → Normal floats work (MVP!)
3. Phase 4 (US2) → Special values work
4. Phase 5 (US3) → Constant folding safe
5. Phase 6 (US4) → Test harness portable
6. Phase 7 → Final verification

### Success Criteria Mapping

| Success Criterion | Verified By Tasks |
|-------------------|-------------------|
| SC-001: Existing tests pass | T051, T054 |
| SC-002: Bit-identical output | T056 |
| SC-003: No SBCL symbols remain | T052, T053 |
| SC-004: No float exceptions | T041, T046 |
| SC-005: Special values correct | T027-T038 |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
