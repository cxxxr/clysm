# Tasks: Phase 14B - Numeric Type Predicates Enhancement

**Input**: Design documents from `/specs/001-numeric-predicates/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, quickstart.md
**Branch**: `001-numeric-predicates`

**Scope Note**: User Stories 1 & 2 (sign/parity predicates) are **already implemented** in `func-section.lisp:4414-4603`. This task list covers only the remaining 10 functions from User Stories 3, 4, and 5.

**Tests**: TDD required per Constitution VII. Tests written first, must fail before implementation.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US3, US4, US5) - maps to spec.md

---

## Phase 1: Setup

**Purpose**: Verify development environment and existing dependencies

- [x] T001 Verify nix develop environment loads with SBCL 2.4+ and wasm-tools
- [x] T002 Verify existing bit operations (ash, logand, logior, logxor) compile correctly

**Checkpoint**: Development environment ready

---

## Phase 2: Foundational (Shared Test Infrastructure)

**Purpose**: Create test file structure that all user stories will use

**⚠️ CRITICAL**: No implementation can begin until test infrastructure exists

- [x] T003 Create test file tests/unit/numeric-predicates-test.lisp with rove test package setup
- [x] T004 [P] Create test file tests/contract/byte-ops-wasm-test.lisp for Wasm output validation
- [x] T005 [P] Create helper function `compile-and-validate` in test utils for Wasm validation

**Checkpoint**: Test infrastructure ready - TDD can now proceed

---

## Phase 3: User Story 3 - Bit Testing Functions (Priority: P2)

**Goal**: Implement [logbitp](resources/HyperSpec/Body/f_logbtp.htm) and [logtest](resources/HyperSpec/Body/f_logtes.htm) for testing individual bits

**Independent Test**: Compile `(logbitp 0 5)` and verify it returns T; compile `(logtest 4 3)` and verify it returns NIL

### Tests for User Story 3 (TDD - write first, must fail)

- [x] T006 [P] [US3] Write unit test for logbitp in tests/unit/numeric-predicates-test.lisp
- [x] T007 [P] [US3] Write unit test for logtest in tests/unit/numeric-predicates-test.lisp
- [x] T008 [US3] Run tests and verify they FAIL (functions not yet implemented)

### Implementation for User Story 3

- [x] T009 [US3] Add `logbitp` dispatch case to func-section.lisp ~line 920
- [x] T010 [US3] Implement `compile-logbitp` in src/clysm/compiler/codegen/func-section.lisp
- [x] T011 [US3] Add `logtest` dispatch case to func-section.lisp ~line 920
- [x] T012 [US3] Implement `compile-logtest` in src/clysm/compiler/codegen/func-section.lisp
- [x] T013 [US3] Run tests and verify they PASS
- [x] T014 [US3] Validate generated Wasm with `wasm-tools validate`

**Checkpoint**: Bit testing functions complete and independently testable

---

## Phase 4: User Story 4 - Byte Specifier Functions (Priority: P2)

**Goal**: Implement [byte](resources/HyperSpec/Body/f_by_by.htm), [byte-size](resources/HyperSpec/Body/f_by_by.htm), [byte-position](resources/HyperSpec/Body/f_by_by.htm) using fixnum encoding

**Independent Test**: Compile `(byte-size (byte 8 4))` and verify it returns 8; compile `(byte-position (byte 8 4))` and verify it returns 4

### Tests for User Story 4 (TDD - write first, must fail)

- [x] T015 [P] [US4] Write unit test for byte constructor in tests/unit/numeric-predicates-test.lisp
- [x] T016 [P] [US4] Write unit test for byte-size in tests/unit/numeric-predicates-test.lisp
- [x] T017 [P] [US4] Write unit test for byte-position in tests/unit/numeric-predicates-test.lisp
- [x] T018 [US4] Run tests and verify they FAIL (functions not yet implemented)

### Implementation for User Story 4

- [x] T019 [US4] Add `byte` dispatch case to func-section.lisp ~line 920
- [x] T020 [US4] Implement `compile-byte` using encoding `(size << 6) | position` in src/clysm/compiler/codegen/func-section.lisp
- [x] T021 [P] [US4] Add `byte-size` dispatch case to func-section.lisp ~line 920
- [x] T022 [P] [US4] Implement `compile-byte-size` using `spec >> 6` in src/clysm/compiler/codegen/func-section.lisp
- [x] T023 [P] [US4] Add `byte-position` dispatch case to func-section.lisp ~line 920
- [x] T024 [P] [US4] Implement `compile-byte-position` using `spec & 63` in src/clysm/compiler/codegen/func-section.lisp
- [x] T025 [US4] Run tests and verify they PASS
- [x] T026 [US4] Validate generated Wasm with `wasm-tools validate`

**Checkpoint**: Byte specifier functions complete and independently testable

---

## Phase 5: User Story 5 - Byte Extraction and Manipulation (Priority: P3)

**Goal**: Implement [ldb](resources/HyperSpec/Body/f_ldb.htm), [dpb](resources/HyperSpec/Body/f_dpb.htm), [mask-field](resources/HyperSpec/Body/f_mask_f.htm), [deposit-field](resources/HyperSpec/Body/f_deposi.htm)

**Depends on**: User Story 4 (byte specifier functions)

**Independent Test**: Compile `(ldb (byte 4 4) #xAB)` and verify it returns #xA; compile `(dpb #xC (byte 4 4) #xAB)` and verify it returns #xCB

### Tests for User Story 5 (TDD - write first, must fail)

- [x] T027 [P] [US5] Write unit test for ldb in tests/unit/numeric-predicates-test.lisp
- [x] T028 [P] [US5] Write unit test for dpb in tests/unit/numeric-predicates-test.lisp
- [x] T029 [P] [US5] Write unit test for mask-field in tests/unit/numeric-predicates-test.lisp
- [x] T030 [P] [US5] Write unit test for deposit-field in tests/unit/numeric-predicates-test.lisp
- [x] T031 [US5] Run tests and verify they FAIL (functions not yet implemented)

### Implementation for User Story 5

- [x] T032 [US5] Add `ldb` dispatch case to func-section.lisp ~line 920
- [x] T033 [US5] Implement `compile-ldb` in src/clysm/compiler/codegen/func-section.lisp (formula: `(integer >> position) & mask`)
- [x] T034 [US5] Add `mask-field` dispatch case to func-section.lisp ~line 920
- [x] T035 [US5] Implement `compile-mask-field` in src/clysm/compiler/codegen/func-section.lisp (formula: `integer & field-mask`)
- [x] T036 [US5] Add `dpb` dispatch case to func-section.lisp ~line 920
- [x] T037 [US5] Implement `compile-dpb` in src/clysm/compiler/codegen/func-section.lisp (formula: `(integer & ~mask) | ((newbyte << pos) & mask)`)
- [x] T038 [US5] Add `deposit-field` dispatch case to func-section.lisp ~line 920
- [x] T039 [US5] Implement `compile-deposit-field` in src/clysm/compiler/codegen/func-section.lisp (formula: `(integer & ~mask) | (newbyte & mask)`)
- [x] T040 [US5] Run tests and verify they PASS
- [x] T041 [US5] Validate generated Wasm with `wasm-tools validate`

**Checkpoint**: All byte operations complete and independently testable

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Registration, documentation, and final validation

- [x] T042 [P] Update feature-registry.lisp with entries for all 10 functions in src/clysm/validation/feature-registry.lisp
- [x] T043 [P] Add HyperSpec links as comments in each compile-* function
- [x] T044 [P] Create integration test in tests/integration/byte-ops-run-test.lisp
- [x] T045 Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"` (Note: Pre-existing error in generator-test.lisp; all 10 Phase 14B functions compile and validate)
- [x] T046 Verify quickstart.md checklist is complete

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup) → Phase 2 (Foundational) → Phase 3-5 (User Stories) → Phase 6 (Polish)
```

### User Story Dependencies

- **US3 (Bit Testing)**: No dependencies on other stories - can start after Phase 2
- **US4 (Byte Specifiers)**: No dependencies on other stories - can start after Phase 2
- **US5 (Byte Operations)**: Depends on US4 completion (needs byte, byte-size, byte-position)

```
Phase 2 (Foundational)
    │
    ├──→ US3 (Bit Testing) ──→ Phase 6
    │
    ├──→ US4 (Byte Specifiers) ──→ US5 (Byte Operations) ──→ Phase 6
```

### Within Each User Story (TDD)

1. Write tests FIRST (T006-T008, T015-T018, T027-T031)
2. Run tests → MUST FAIL
3. Implement functions (T009-T014, T019-T026, T032-T041)
4. Run tests → MUST PASS
5. Validate Wasm output

### Parallel Opportunities

**Phase 2 (can run in parallel)**:
```
T004 (contract test file) || T005 (test helper)
```

**US3 Tests (can run in parallel)**:
```
T006 (logbitp test) || T007 (logtest test)
```

**US4 Tests (can run in parallel)**:
```
T015 (byte test) || T016 (byte-size test) || T017 (byte-position test)
```

**US4 Implementation (can run in parallel after T020)**:
```
T021+T022 (byte-size) || T023+T024 (byte-position)
```

**US5 Tests (can run in parallel)**:
```
T027 (ldb) || T028 (dpb) || T029 (mask-field) || T030 (deposit-field)
```

**Phase 6 (can run in parallel)**:
```
T042 (registry) || T043 (comments) || T044 (integration test)
```

---

## Parallel Example: User Story 4

```bash
# Launch all tests in parallel (TDD step 1):
Task: "Write unit test for byte constructor in tests/unit/numeric-predicates-test.lisp"
Task: "Write unit test for byte-size in tests/unit/numeric-predicates-test.lisp"
Task: "Write unit test for byte-position in tests/unit/numeric-predicates-test.lisp"

# After byte implemented, launch accessors in parallel:
Task: "Implement compile-byte-size using spec >> 6"
Task: "Implement compile-byte-position using spec & 63"
```

---

## Implementation Strategy

### MVP First (User Story 3 + 4 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: US3 (Bit Testing) - `logbitp`, `logtest`
4. Complete Phase 4: US4 (Byte Specifiers) - `byte`, `byte-size`, `byte-position`
5. **STOP and VALIDATE**: 5 functions working independently
6. Can ship MVP with bit testing + byte specifiers

### Full Delivery

1. MVP + Phase 5: US5 (Byte Operations)
2. Phase 6: Polish
3. All 10 functions complete

### Suggested Order

```
T001 → T002 → T003 → T004 → T005 (Setup + Foundational)
                ↓
    ┌───────────┴───────────┐
    ↓                       ↓
US3 (T006-T014)      US4 (T015-T026)
    │                       │
    │                       ↓
    │               US5 (T027-T041)
    │                       │
    └───────────┬───────────┘
                ↓
        Phase 6 (T042-T046)
```

---

## Notes

- All 10 functions go into single file: `src/clysm/compiler/codegen/func-section.lisp`
- Follow existing patterns from `compile-evenp` (line 4576) and `compile-ash` (line 2837)
- Byte specifier encoding: `(size << 6) | position` - matches SBCL/CCL
- TDD is non-negotiable per Constitution VII
- Commit after each function implementation passes tests
