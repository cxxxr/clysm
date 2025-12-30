# Tasks: Compile-Time Directive Processing

**Input**: Design documents from `/specs/001-compile-time-directives/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: REQUIRED per Constitution Principle VII (TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and directive module structure

- [x] T001 Create directive module file at src/clysm/compiler/directive.lisp with package definition
- [x] T002 Add directive.lisp to clysm.asd system definition (after compiler.lisp)
- [x] T003 [P] Create unit test file at tests/unit/directive-test.lisp with package definition
- [x] T004 [P] Create contract test file at tests/contract/directive-output-test.lisp with package definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core directive detection infrastructure that ALL user stories depend on

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Implement `directive-form-p` predicate in src/clysm/compiler/directive.lisp (detect in-package, defpackage, declaim, proclaim)
- [x] T006 Implement `compile-toplevel-form` function skeleton in src/clysm/compiler/directive.lisp (dispatch on directive-form-p)
- [x] T007 Integrate `compile-toplevel-form` into compile-to-module loop in src/clysm/compiler/compiler.lisp (lines 65-135)
- [x] T008 Add error handling wrapper with source location context in src/clysm/compiler/directive.lisp

**Checkpoint**: Foundation ready - directive detection integrated into compilation pipeline

---

## Phase 3: User Story 1 - IN-PACKAGE Directive Handling (Priority: P1)

**Goal**: Process [in-package](resources/HyperSpec/Body/m_in_pkg.htm) forms at compile-time, changing `*package*` without generating AST/Wasm

**Independent Test**: Compile a file with `(in-package :clysm)` followed by symbol definitions; verify no AST for in-package, symbols in correct package

### Tests for User Story 1 (TDD - write FIRST, ensure FAIL)

- [x] T009 [P] [US1] Unit test for directive-form-p with in-package forms in tests/unit/directive-test.lisp
- [x] T010 [P] [US1] Unit test for package context change via compile-toplevel-form in tests/unit/directive-test.lisp
- [x] T011 [P] [US1] Contract test verifying nil return (no AST) for in-package in tests/contract/directive-output-test.lisp
- [x] T012 [P] [US1] Unit test for error on non-existent package in tests/unit/directive-test.lisp

### Implementation for User Story 1

- [x] T013 [US1] Implement in-package evaluation in compile-toplevel-form in src/clysm/compiler/directive.lisp
- [x] T014 [US1] Add package-error handling for undefined packages in src/clysm/compiler/directive.lisp
- [x] T015 [US1] Verify all tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: in-package forms (49 of 61) now compile without error; story independently testable

---

## Phase 4: User Story 2 - DEFPACKAGE Directive Handling (Priority: P2)

**Goal**: Process [defpackage](resources/HyperSpec/Body/m_defpkg.htm) forms at compile-time, creating packages without generating AST/Wasm

**Independent Test**: Compile a file with `(defpackage :test-pkg (:use :cl))` followed by `(in-package :test-pkg)`; verify package created at compile-time

### Tests for User Story 2 (TDD - write FIRST, ensure FAIL)

- [x] T016 [P] [US2] Unit test for directive-form-p with defpackage forms in tests/unit/directive-test.lisp
- [x] T017 [P] [US2] Unit test for package creation via compile-toplevel-form in tests/unit/directive-test.lisp
- [x] T018 [P] [US2] Contract test verifying nil return (no AST) for defpackage in tests/contract/directive-output-test.lisp
- [x] T019 [P] [US2] Integration test for defpackage followed by in-package in tests/unit/directive-test.lisp

### Implementation for User Story 2

- [x] T020 [US2] Implement defpackage evaluation in compile-toplevel-form in src/clysm/compiler/directive.lisp
- [x] T021 [US2] Handle complex defpackage options (nicknames, shadow, import-from) in src/clysm/compiler/directive.lisp
- [x] T022 [US2] Verify all tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: defpackage forms (9 of 61) now compile without error; US1 + US2 both work

---

## Phase 5: User Story 3 - DECLAIM/PROCLAIM Directive Handling (Priority: P3)

**Goal**: Process [declaim](resources/HyperSpec/Body/m_declai.htm) and [proclaim](resources/HyperSpec/Body/f_procla.htm) forms at compile-time without generating AST/Wasm

**Independent Test**: Compile `(declaim (optimize (speed 3)))` and verify no AST generated, declaration recorded in host environment

### Tests for User Story 3 (TDD - write FIRST, ensure FAIL)

- [x] T023 [P] [US3] Unit test for directive-form-p with declaim/proclaim forms in tests/unit/directive-test.lisp
- [x] T024 [P] [US3] Unit test for declaim optimize in compile-toplevel-form in tests/unit/directive-test.lisp
- [x] T025 [P] [US3] Unit test for declaim special in compile-toplevel-form in tests/unit/directive-test.lisp
- [x] T026 [P] [US3] Contract test verifying nil return (no AST) for declaim/proclaim in tests/contract/directive-output-test.lisp

### Implementation for User Story 3

- [x] T027 [US3] Implement declaim evaluation in compile-toplevel-form in src/clysm/compiler/directive.lisp
- [x] T028 [US3] Implement proclaim evaluation in compile-toplevel-form in src/clysm/compiler/directive.lisp
- [x] T029 [US3] Verify all tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: All 61 directive forms now compile without error; all 3 user stories complete

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Validation, integration testing, and cleanup

- [x] T030 Run full codebase compilation with `sbcl --load build/stage0-complete.lisp` to verify 61 error reduction
- [x] T031 [P] Add docstrings to all exported functions in src/clysm/compiler/directive.lisp
- [x] T032 [P] Verify no Wasm bytecode generated for any directive form via manual inspection
- [x] T033 Run `nix flake check` to ensure all CI gates pass (Constitution Principle VIII)
- [x] T034 Update CLAUDE.md if new technologies were added

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phases 3-5)**: All depend on Foundational phase completion
  - US1 (in-package): Highest impact, implement first
  - US2 (defpackage): Enables package creation for US1
  - US3 (declaim/proclaim): Final 3 errors
- **Polish (Phase 6)**: Depends on all user stories complete

### User Story Dependencies

- **US1 (P1)**: Can start after Foundational - independent
- **US2 (P2)**: Can start after Foundational - integration test with US1 but independently testable
- **US3 (P3)**: Can start after Foundational - fully independent of US1/US2

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD)
2. Implement core functionality
3. Run tests to verify GREEN
4. Story complete before moving to next priority

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T003 (unit test file) || T004 (contract test file)
```

**Phase 3 (US1 Tests)**:
```
T009 || T010 || T011 || T012 (all test tasks)
```

**Phase 4 (US2 Tests)**:
```
T016 || T017 || T018 || T019 (all test tasks)
```

**Phase 5 (US3 Tests)**:
```
T023 || T024 || T025 || T026 (all test tasks)
```

**Phase 6 (Polish)**:
```
T031 (docstrings) || T032 (wasm verification)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (in-package)
4. **STOP and VALIDATE**: Run `sbcl --load build/stage0-complete.lisp`, expect 49 fewer errors
5. This is a viable MVP - 80% of directive errors resolved

### Incremental Delivery

1. Setup + Foundational → Directive detection pipeline ready
2. Add US1 (in-package) → 49 errors resolved → Test independently
3. Add US2 (defpackage) → 58 errors resolved → Test independently
4. Add US3 (declaim/proclaim) → 61 errors resolved → Full feature complete
5. Each story adds measurable value (error count reduction)

### Single Developer Strategy

Since directives share the same evaluation mechanism, sequential execution is optimal:
1. Setup + Foundational (establish infrastructure)
2. US1 → US2 → US3 (prioritized, each building on shared directive.lisp)
3. Polish (validation and cleanup)

---

## Notes

- [P] tasks = different files, no dependencies on incomplete tasks
- [Story] label maps task to specific user story for traceability
- TDD is REQUIRED per Constitution Principle VII
- Each user story reduces error count measurably
- Total tasks: 34
- Tasks per user story: US1=7, US2=7, US3=7, Setup=4, Foundational=4, Polish=5
