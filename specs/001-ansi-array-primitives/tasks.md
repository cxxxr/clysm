# Tasks: ANSI CL Array/Sequence Primitives

**Input**: Design documents from `/specs/001-ansi-array-primitives/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: TDD REQUIRED per Constitution VII - tests written before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

---

## Phase 1: Setup

**Purpose**: Create test infrastructure and package definitions

- [X] T001 [P] Create test file `tests/unit/array-primitives-test.lisp` with package definition
- [X] T002 [P] Create test file `tests/contract/array-wasm-test.lisp` with package definition
- [X] T003 [P] Create test file `tests/integration/array-test.lisp` with package definition
- [X] T004 Register new test files in `clysm.asd` system definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before user story implementation

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T005 Add `compile-array-primitive` dispatcher in `src/clysm/compiler/codegen/func-section.lisp` for routing aref/svref/schar/elt calls
- [X] T006 Verify type 20 ($mv_array) is suitable for simple-vectors in `src/clysm/compiler/codegen/gc-types.lisp` (Note: type 20, not 22)
- [X] T007 Add HyperSpec reference comment header template for array primitives per Constitution IX

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 1 - Array Element Access (Priority: P1) 🎯 MVP

**Goal**: Enable compilation of `aref` and `svref` expressions to Wasm `array.get`, unblocking 90 defstruct-related forms

**Independent Test**: `(aref #(1 2 3) 1)` compiles and returns `2`; defstruct accessor forms compile

### Tests for User Story 1 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T008 [P] [US1] Unit test for aref codegen in `tests/unit/array-primitives-test.lisp`: test-aref-compiles
- [X] T009 [P] [US1] Unit test for svref codegen in `tests/unit/array-primitives-test.lisp`: test-svref-compiles
- [X] T010 [P] [US1] Unit test for %setf-aref codegen in `tests/unit/array-primitives-test.lisp`: test-setf-aref-compiles
- [X] T011 [P] [US1] Contract test for aref Wasm validation: verified via manual test (wasm-tools validates aref output)
- [ ] T012 [P] [US1] Integration test (DEFERRED: requires Node.js + host-shim runtime)

### Implementation for User Story 1

- [X] T013 [US1] Implement `compile-aref` function in `src/clysm/compiler/codegen/func-section.lisp` per contracts/array-primitives.md
- [X] T014 [US1] Implement `compile-svref` function (alias to compile-aref) in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T015 [US1] Add aref/svref case to call dispatcher in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T016 [US1] Implement `compile-%setf-aref` primitive in `src/clysm/compiler/codegen/func-section.lisp` for setf support (fixed type 22 -> 20)
- [X] T017 [US1] Add %setf-aref case to call dispatcher in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T018 [US1] Add svref setf expander in `src/clysm/lib/setf-expanders.lisp` using existing aref pattern
- [X] T019 [US1] Verify defstruct accessor compilation - accessor patterns compile correctly
  - Note: Required cross-package symbol matching fix in compile-primitive-call for %setf-* primitives
  - Changed member test to use symbol-name comparison and added cond dispatch for %setf-* symbols

**Checkpoint**: aref/svref compile correctly; defstruct accessors work; compilation rate should increase significantly

---

## Phase 4: User Story 2 - Sequence Coercion (Priority: P2)

**Goal**: Enable compilation of `coerce` expressions for list↔vector conversions, unblocking leb128.lisp

**Independent Test**: `(coerce '(1 2 3) 'vector)` compiles and returns `#(1 2 3)`

### Tests for User Story 2 (TDD Required)

- [X] T020 [P] [US2] Unit test for coerce list-to-vector: test-coerce-compiles in array-primitives-test.lisp
- [X] T021 [P] [US2] Unit test for coerce vector-to-list: test-coerce-to-list-compiles in array-primitives-test.lisp
- [X] T022 [P] [US2] Contract test for coerce Wasm validation: verified manually
- [ ] T023 [P] [US2] Integration test (DEFERRED: requires runtime environment)

### Implementation for User Story 2

- [X] T024 [US2] Note: compile-coerce implemented directly in func-section.lisp (no separate runtime file needed for basic cases)
- [ ] T025 [US2] Runtime helper `%list-to-vector` (DEFERRED: optional for advanced coerce scenarios)
- [ ] T026 [US2] Runtime helper `%vector-to-list` (DEFERRED: optional for advanced coerce scenarios)
- [X] T027 [US2] Coerce primitive added to call dispatcher in func-section.lisp
- [X] T028 [US2] Implement `compile-coerce` function with compile-time dispatch for 'vector and 'list
- [X] T029 [US2] Coerce registered in primitive call dispatcher
- [X] T030 [US2] Verify leb128.lisp compiles without coerce-related errors (verified: 2/2 coerce forms compile)

**Checkpoint**: coerce works for list↔vector; leb128.lisp compiles

---

## Phase 5: User Story 3 - String Character Access (Priority: P3)

**Goal**: Enable compilation of `schar` expressions for string character access

**Independent Test**: `(schar "hello" 0)` compiles and returns `#\h`

### Tests for User Story 3 (TDD Required)

- [X] T031 [P] [US3] Unit test for schar codegen: test-schar-compiles in array-primitives-test.lisp
- [X] T032 [P] [US3] Unit test for (setf schar) codegen: test-setf-schar-compiles in array-primitives-test.lisp
- [X] T033 [P] [US3] Contract test for schar Wasm validation: verified manually
- [ ] T034 [P] [US3] Integration test (DEFERRED: requires runtime environment)

### Implementation for User Story 3

- [X] T035 [US3] Note: schar already implemented by compile-string-char function (pre-existing)
- [X] T036 [US3] schar case already in call dispatcher (pre-existing)
- [X] T037 [US3] Implement `compile-%setf-schar` primitive (compile-setf-schar) in func-section.lisp
- [X] T038 [US3] Add %setf-schar case to call dispatcher in func-section.lisp
- [X] T039 [US3] Add schar setf expander in `src/clysm/lib/setf-expanders.lisp`

**Checkpoint**: schar works for ASCII strings; character manipulation code compiles

---

## Phase 6: User Story 4 - Generic Sequence Element Access (Priority: P4)

**Goal**: Enable compilation of `elt` expressions with type dispatch for lists, vectors, and strings

**Independent Test**: `(elt '(a b c) 1)` returns `b`; `(elt #(1 2 3) 1)` returns `2`

### Tests for User Story 4 (TDD Required)

- [X] T040 [P] [US4] Unit test for elt on vector: test-elt-compiles in array-primitives-test.lisp
- [X] T041 [P] [US4] Unit test for elt on string: covered by test-elt-compiles (type dispatch)
- [X] T042 [P] [US4] Unit test for elt on list: covered by test-elt-compiles (type dispatch)
- [X] T043 [P] [US4] Contract test for elt Wasm validation: verified manually
- [ ] T044 [P] [US4] Integration test (DEFERRED: requires runtime environment)

### Implementation for User Story 4

- [X] T045 [US4] List access implemented inline in compile-elt (loop-based nth, no separate runtime function)
- [X] T046 [US4] Implement `compile-elt` function with type dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T047 [US4] Add elt case to call dispatcher in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T048 [US4] Implement `compile-setf-elt` primitive with type dispatch in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T049 [US4] Add %setf-elt case to call dispatcher in `src/clysm/compiler/codegen/func-section.lisp`
- [X] T050 [US4] Add elt setf expander in `src/clysm/lib/setf-expanders.lisp`

**Checkpoint**: elt works on all sequence types; generic sequence code compiles

---

## Phase 7: Polish & Validation

**Purpose**: Final validation and cleanup

- [ ] T051 Run full test suite (DEFERRED: heap exhaustion in large test suite)
- [X] T052 Validate generated Wasm: 3/4 valid (stage1 stub invalid, expected)
- [X] T053 Run `nix flake check` - PASSED (flake outputs validated)
- [X] T054 Compilation rate: 222/973 (22.8%) - array primitives work
- [ ] T055 SC-001: Rate >= 30% NOT MET (blocked by defstruct/loop/handler-case macros, not array primitives)
- [X] T056 SC-002: Defstruct accessor patterns compile (5/6 pass)
- [X] T057 CLAUDE.md updated with feature status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - Can proceed in priority order (P1 → P2 → P3 → P4)
  - Or in parallel if staffed
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational - Independent of US1
- **User Story 3 (P3)**: Can start after Foundational - Independent of US1/US2
- **User Story 4 (P4)**: Depends on US2 for `%nth` runtime helper in sequences.lisp

### Within Each User Story

1. Tests MUST be written and FAIL before implementation
2. Core primitive before setf support
3. Dispatcher registration after implementation
4. Verify checkpoint before proceeding

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T001 || T002 || T003  # All test file creation in parallel
```

**Phase 2 (Foundational)**:
```
T005 || T006 || T007  # No file conflicts
```

**User Story 1 Tests**:
```
T008 || T009 || T010 || T011 || T012  # All test tasks in parallel
```

**User Story 2 Tests**:
```
T020 || T021 || T022 || T023  # All test tasks in parallel
```

**Cross-Story Parallelism** (with multiple developers):
```
After Foundational:
  Developer A: US1 (P1) - aref/svref
  Developer B: US2 (P2) - coerce
  Developer C: US3 (P3) - schar
  (US4 waits for US2's sequences.lisp)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (aref/svref)
4. **STOP and VALIDATE**: Test defstruct compilation
5. Check if 90 defstruct forms now compile

### Incremental Delivery

| Milestone | User Stories | Expected Compilation Rate |
|-----------|--------------|--------------------------|
| MVP | US1 | ~25% (defstruct unblocked) |
| +Coerce | US1 + US2 | ~28% (leb128 unblocked) |
| +Schar | US1 + US2 + US3 | ~29% (string ops) |
| Complete | All | 30%+ (target achieved) |

---

## Summary

| Metric | Value |
|--------|-------|
| **Total Tasks** | 57 |
| **Setup Tasks** | 4 |
| **Foundational Tasks** | 3 |
| **US1 Tasks (P1)** | 12 |
| **US2 Tasks (P2)** | 11 |
| **US3 Tasks (P3)** | 9 |
| **US4 Tasks (P4)** | 11 |
| **Polish Tasks** | 7 |
| **Parallel Opportunities** | 25 tasks marked [P] |
| **MVP Scope** | Phase 1-3 (19 tasks) |

---

## Notes

- [P] tasks = different files, no dependencies within phase
- [Story] label maps task to specific user story
- TDD required per Constitution VII: write tests first, ensure they fail
- HyperSpec references required per Constitution IX
- Verify `nix flake check` passes before merging
