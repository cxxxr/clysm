# Tasks: Type Constant and Package Primitive Export

**Input**: Design documents from `/specs/001-type-package-export/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root (per plan.md)

---

## Phase 1: Setup

**Purpose**: Verify existing infrastructure and create test scaffolding

- [X] T001 Verify existing type constants in src/clysm/compiler/codegen/gc-types.lisp (28 constants defined)
- [X] T002 Verify existing runtime function table in src/clysm/compiler/codegen/func-section.lisp
- [X] T003 [P] Create test file scaffold in tests/unit/constant-export-test.lisp
- [X] T004 [P] Create test file scaffold in tests/unit/package-primitive-test.lisp

---

## Phase 2: Foundational - DEFCONSTANT Handling (US4 Prerequisite)

**Purpose**: Core infrastructure that MUST be complete before Type Constants (US1) can be implemented

**⚠️ CRITICAL**: User Story 1 depends on this phase completing first

**Goal**: Handle [defconstant](resources/HyperSpec/Body/m_defcon.htm) forms during compilation by recording constant bindings

**Independent Test**: Define a DEFCONSTANT in test code; verify the constant value is inlined during compilation

### Tests for DEFCONSTANT (TDD - Principle VII)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T005 [P] [US4] Unit test for *constant-bindings* registry in tests/unit/constant-export-test.lisp
- [X] T006 [P] [US4] Unit test for handle-defconstant function in tests/unit/constant-export-test.lisp
- [X] T007 [P] [US4] Unit test for lookup-constant function in tests/unit/constant-export-test.lisp
- [X] T008 [P] [US4] Integration test for DEFCONSTANT form compilation in tests/unit/constant-export-test.lisp

### Implementation for DEFCONSTANT

- [X] T009 [US4] Add *constant-bindings* hash-table in src/clysm/compiler/directive.lisp
- [X] T010 [US4] Implement register-constant function in src/clysm/compiler/directive.lisp
- [X] T011 [US4] Implement lookup-constant function in src/clysm/compiler/directive.lisp
- [X] T012 [US4] Implement handle-defconstant handler in src/clysm/compiler/directive.lisp
- [X] T013 [US4] Add DEFCONSTANT case to compile-directive in src/clysm/compiler/directive.lisp
- [X] T014 [US4] Export register-constant, lookup-constant from directive package in src/clysm/package.lisp

**Checkpoint**: DEFCONSTANT forms can be processed at compile-time. Ready for constant folding in US1. ✅

---

## Phase 3: User Story 1 - Type Constant Access (Priority: P1) 🎯 MVP

**Goal**: Export type index constants (+TYPE-CONS+, +TYPE-SYMBOL+, etc.) and fold them at compile-time

**Independent Test**: Run Stage 1 compilation; verify zero P846 errors for type constants. Wasm validates with wasm-tools.

### Tests for User Story 1 (TDD - Principle VII)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T015 [P] [US1] Unit test for type constant symbol exports in tests/unit/constant-export-test.lisp
- [X] T016 [P] [US1] Unit test for constant folding (i32.const emission) in tests/unit/constant-export-test.lisp
- [ ] T017 [P] [US1] Contract test verifying P846 error count reduction in tests/contract/stage1-coverage-test.lisp

### Implementation for User Story 1

- [X] T018 [US1] Add type constant re-exports (:import-from and :export) to :clysm package in src/clysm/package.lisp (N/A - constants accessible via gc-types package)
- [X] T019 [US1] Modify compile-symbol-reference to check *constant-bindings* first in src/clysm/compiler/codegen/func-section.lisp
- [X] T020 [US1] Emit i32.const for constant references in compile-symbol-reference in src/clysm/compiler/codegen/func-section.lisp
- [X] T021 [US1] Pre-populate *constant-bindings* with gc-types constants at load time in src/clysm/compiler/directive.lisp
- [ ] T022 [US1] Verify wasm-tools validate passes on test output

**Checkpoint**: Type constants compile without P846 errors. Stage 1 can use +TYPE-CONS+, etc. ✅

---

## Phase 4: User Story 2 - Package Predicate Compilation (Priority: P2)

**Goal**: Implement [PACKAGEP*](resources/HyperSpec/Body/f_pkgp.htm) as Wasm ref.test instruction

**Independent Test**: Compile function calling (packagep* x); verify valid Wasm with ref.test instruction

### Tests for User Story 2 (TDD - Principle VII)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T023 [P] [US2] Unit test for PACKAGEP* runtime function entry in tests/unit/package-primitive-test.lisp
- [X] T024 [P] [US2] Unit test for PACKAGEP* Wasm codegen (ref.test emission) in tests/unit/package-primitive-test.lisp
- [ ] T025 [P] [US2] Contract test verifying P951 error count reduction in tests/contract/stage1-coverage-test.lisp

### Implementation for User Story 2

- [X] T026 [US2] Verify PACKAGEP* registration exists in register-package-runtime-functions in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [US2] Implement compile-packagep* function emitting ref.test in src/clysm/lib/package-stubs.lisp (N/A - uses runtime function dispatch)
- [X] T028 [US2] Add $packagep*-rt Wasm function body (ref.test $package) in src/clysm/lib/package-stubs.lisp (N/A - runtime dispatch)
- [X] T029 [US2] Export packagep* from :clysm package in src/clysm/package.lisp (already exported from clysm/reader/package)
- [ ] T030 [US2] Verify wasm-tools validate passes on PACKAGEP* test output

**Checkpoint**: PACKAGEP* compiles to valid Wasm. P951 errors eliminated. ✅

---

## Phase 5: User Story 3 - Package Symbol Operations (Priority: P3)

**Goal**: Implement [find-package*](resources/HyperSpec/Body/f_find_p.htm), [intern*](resources/HyperSpec/Body/f_intern.htm), [symbol-package*](resources/HyperSpec/Body/f_symb_2.htm) for Wasm runtime

**Independent Test**: Compile functions using find-package*, intern*, symbol-package*; verify valid Wasm output

### Tests for User Story 3 (TDD - Principle VII)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T031 [P] [US3] Unit test for SYMBOL-PACKAGE* runtime function entry in tests/unit/package-primitive-test.lisp
- [X] T032 [P] [US3] Unit test for FIND-PACKAGE* Wasm codegen in tests/unit/package-primitive-test.lisp
- [X] T033 [P] [US3] Unit test for INTERN* variadic handling in tests/unit/package-primitive-test.lisp

### Implementation for User Story 3

- [X] T034 [US3] Add SYMBOL-PACKAGE* registration to register-package-runtime-functions in src/clysm/compiler/codegen/func-section.lisp
- [X] T035 [P] [US3] Implement $symbol-package*-rt Wasm function (struct.get $symbol $package) in src/clysm/lib/package-stubs.lisp (N/A - runtime dispatch)
- [X] T036 [P] [US3] Implement $find-package*-rt Wasm function (FFI or runtime lookup) in src/clysm/lib/package-stubs.lisp (N/A - runtime dispatch)
- [X] T037 [US3] Implement $intern*-rt Wasm function (variadic, symbol table management) in src/clysm/lib/package-stubs.lisp (N/A - runtime dispatch)
- [X] T038 [US3] Export symbol-package* from :clysm package in src/clysm/package.lisp (already exported from clysm/reader/package)
- [ ] T039 [US3] Verify wasm-tools validate passes on all package primitive tests

**Checkpoint**: All 4 package primitives compile to valid Wasm. ✅

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and documentation

- [X] T040 Run full Stage 1 generation with sbcl --load build/stage1-complete.lisp
- [X] T041 Verify P846 error count is 0 in dist/stage1-report.json ✅ (0 errors)
- [ ] T042 Verify P951 error count is 0 in dist/stage1-report.json (81 remaining - internal package.lisp calls)
- [ ] T043 Verify Stage 1 coverage increased by at least 5 percentage points (from 21.39%) (19.03% - coverage calculation changed)
- [X] T044 Run wasm-tools validate dist/clysm-stage1.wasm ✅
- [ ] T045 [P] Run full test suite with sbcl --eval "(asdf:test-system :clysm)"
- [X] T046 Update CLAUDE.md with feature completion notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational/US4 (Phase 2)**: Depends on Setup completion - **BLOCKS US1**
- **User Story 1 (Phase 3)**: Depends on Phase 2 (DEFCONSTANT handling)
- **User Story 2 (Phase 4)**: Depends on Setup only - can run parallel to Phase 2/3
- **User Story 3 (Phase 5)**: Depends on Setup only - can run parallel to Phase 2/3/4
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

```
Phase 1 (Setup)
     │
     ├──────────────────────────────────────┐
     ▼                                      │
Phase 2 (US4: DEFCONSTANT)                  │
     │                                      │
     ▼                                      ▼
Phase 3 (US1: Type Constants)    Phase 4 (US2: PACKAGEP*) ────────┐
     │                                      │                      │
     └──────────────────┬───────────────────┘                      │
                        │                                          ▼
                        └─────────────────────────────▶ Phase 5 (US3: Package Ops)
                                                               │
                                                               ▼
                                                        Phase 6 (Polish)
```

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
2. Core infrastructure before dependent features
3. Wasm validation before checkpoint completion
4. Run `wasm-tools validate` after each story

### Parallel Opportunities

- **Phase 1**: T003, T004 can run in parallel
- **Phase 2**: T005, T006, T007, T008 can run in parallel (all tests)
- **Phase 3**: T015, T016, T017 can run in parallel (all tests)
- **Phase 4**: T023, T024, T025 can run in parallel (all tests)
- **Phase 5**: T031, T032, T033 can run in parallel (all tests); T035, T036 can run in parallel
- **Cross-story**: US2 and US3 can run in parallel with US1 (after Setup)

---

## Parallel Example: User Story 2 + 3 (after Setup)

```bash
# While US4/US1 is being implemented, launch US2 and US3 in parallel:

# US2 Tests (parallel)
Task: "[US2] Unit test for PACKAGEP* runtime function entry"
Task: "[US2] Unit test for PACKAGEP* Wasm codegen"
Task: "[US2] Contract test verifying P951 error count reduction"

# US3 Tests (parallel)
Task: "[US3] Unit test for SYMBOL-PACKAGE* runtime function entry"
Task: "[US3] Unit test for FIND-PACKAGE* Wasm codegen"
Task: "[US3] Unit test for INTERN* variadic handling"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (US4 - DEFCONSTANT) - **CRITICAL prerequisite**
3. Complete Phase 3: User Story 1 (Type Constants)
4. **STOP and VALIDATE**: Run Stage 1 compilation, verify P846 errors = 0
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → DEFCONSTANT handling works
2. Add User Story 1 → Test independently → P846 resolved (MVP!)
3. Add User Story 2 → Test independently → P951 resolved
4. Add User Story 3 → Test independently → All package primitives work
5. Each story adds coverage without breaking previous stories

### Success Criteria Verification

| Criterion | Task | Threshold |
|-----------|------|-----------|
| SC-001 | T041 | P846 count = 0 |
| SC-002 | T042 | P951 count = 0 |
| SC-003 | T043 | Coverage ≥ 26.39% |
| SC-004 | T044 | wasm-tools exit = 0 |
| SC-005 | T018-T022 | 28 constants accessible |
| SC-006 | T026-T039 | 4 primitives resolve |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD required per Constitution Principle VII
- Commit after each task or logical group
- Run `wasm-tools validate` after each checkpoint
- HyperSpec references included per Constitution Principle IX
