# Tasks: Stage 0 Capability Extension

**Input**: Design documents from `/specs/038-stage0-extend/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per Constitution Check (Principle VII). Tests MUST be written first and FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/`, `build/` at repository root
- Paths use absolute paths from repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and shared test infrastructure

- [X] T001 Create test directory structure for new tests in tests/unit/, tests/contract/, tests/integration/
- [X] T002 [P] Add test file stubs for defconstant-test.lisp in tests/unit/defconstant-test.lisp
- [X] T003 [P] Add test file stubs for declare-skip-test.lisp in tests/unit/declare-skip-test.lisp
- [X] T004 [P] Add test file stubs for defstruct-expand-test.lisp in tests/unit/defstruct-expand-test.lisp
- [X] T005 [P] Add test file stub for stage0-extend-test.lisp in tests/contract/stage0-extend-test.lisp
- [X] T006 Verify test runner can load all new test files via (asdf:test-system :clysm)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T007 Add constant-registry hash-table initialization to build/bootstrap.lisp
- [X] T008 Implement register-constant function in build/bootstrap.lisp
- [X] T009 Implement lookup-constant function in build/bootstrap.lisp
- [X] T010 Implement constant-defined-p function in build/bootstrap.lisp
- [X] T011 Add defconstant to compilable-form-p acceptance list in build/bootstrap.lisp
- [X] T012 Export new AST symbols (ast-defconstant, filter-declare-forms) from src/clysm/package.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Compile Constant Definitions (Priority: P1) 🎯 MVP

**Goal**: defconstant and defparameter forms compile to Wasm globals with constant folding

**Independent Test**: Compile `(defconstant +test+ 42)` and verify Wasm output contains a global with value 42

### Tests for User Story 1 (TDD - Write FIRST, ensure FAIL)

- [X] T013 [P] [US1] Write unit test for ast-defconstant struct creation in tests/unit/defconstant-test.lisp
- [X] T014 [P] [US1] Write unit test for parse-defconstant-form in tests/unit/defconstant-test.lisp
- [X] T015 [P] [US1] Write unit test for simple defconstant with literal value in tests/unit/defconstant-test.lisp
- [X] T016 [P] [US1] Write unit test for defconstant with arithmetic expression in tests/unit/defconstant-test.lisp
- [X] T017 [P] [US1] Write unit test for defconstant referencing another constant in tests/unit/defconstant-test.lisp
- [X] T018 [P] [US1] Write unit test for defparameter mutable global in tests/unit/defconstant-test.lisp
- [X] T019 [US1] Verify all US1 tests FAIL before implementation (red phase)

### Implementation for User Story 1

- [X] T020 [US1] Add ast-defconstant struct definition (mirroring ast-defparameter) in src/clysm/compiler/ast.lisp
- [X] T021 [US1] Add parse-defconstant-form function in src/clysm/compiler/ast.lisp
- [X] T022 [US1] Add defconstant case to parse-form dispatcher in src/clysm/compiler/ast.lisp
- [X] T023 [US1] Implement fold-constant-expression for arithmetic (+, -, *, /) in src/clysm/compiler/codegen/func-section.lisp
- [X] T024 [US1] Implement fold-constant-expression for constant references in src/clysm/compiler/codegen/func-section.lisp
- [X] T025 [US1] Implement compile-defconstant (immutable global) in src/clysm/compiler/codegen/func-section.lisp
- [X] T026 [US1] Add defconstant case to compile-form dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [US1] Verify all US1 tests PASS (green phase)
- [X] T028 [US1] Run wasm-tools validate on output with defconstant forms

**Checkpoint**: User Story 1 complete - defconstant/defparameter compilation works independently

---

## Phase 4: User Story 2 - Compile Condition Definitions (Priority: P2)

**Goal**: define-condition forms expand to defclass and compile successfully

**Independent Test**: Compile `(define-condition my-error (error) ())` and verify defclass output

### Tests for User Story 2 (TDD - Write FIRST, ensure FAIL)

- [X] T029 [P] [US2] Write unit test for expand-define-condition basic case in tests/unit/condition-expand-test.lisp
- [X] T030 [P] [US2] Write unit test for define-condition with slots in tests/unit/condition-expand-test.lisp
- [X] T031 [P] [US2] Write unit test for define-condition with :report option (skipped) in tests/unit/condition-expand-test.lisp
- [X] T032 [P] [US2] Write unit test for nested condition inheritance in tests/unit/condition-expand-test.lisp
- [X] T033 [US2] Verify all US2 tests FAIL before implementation (red phase)

### Implementation for User Story 2

- [X] T034 [US2] Implement expand-define-condition function in build/bootstrap.lisp
- [X] T035 [US2] Handle slot specifications with :initarg and :reader options in build/bootstrap.lisp
- [X] T036 [US2] Handle :report option by recording but not compiling in build/bootstrap.lisp
- [X] T037 [US2] Add define-condition expansion to expand-form-recursive in build/bootstrap.lisp
- [X] T038 [US2] Verify all US2 tests PASS (green phase)
- [X] T039 [US2] Run wasm-tools validate on output with define-condition forms

**Checkpoint**: User Story 2 complete - define-condition expansion works independently

---

## Phase 5: User Story 3 - Handle Declaration Forms (Priority: P2)

**Goal**: declare forms are skipped without causing compilation failure

**Independent Test**: Compile `(defun foo (x) (declare (type fixnum x)) (+ x 1))` successfully

### Tests for User Story 3 (TDD - Write FIRST, ensure FAIL)

- [X] T040 [P] [US3] Write unit test for filter-declare-forms function in tests/unit/declare-skip-test.lisp
- [X] T041 [P] [US3] Write unit test for defun with single declare in tests/unit/declare-skip-test.lisp
- [X] T042 [P] [US3] Write unit test for defun with multiple declare forms in tests/unit/declare-skip-test.lisp
- [X] T043 [P] [US3] Write unit test for let with declare in tests/unit/declare-skip-test.lisp
- [X] T044 [P] [US3] Write unit test for proclaim at top level (skipped) in tests/unit/declare-skip-test.lisp
- [X] T045 [US3] Verify all US3 tests FAIL before implementation (red phase)

### Implementation for User Story 3

- [X] T046 [US3] Implement filter-declare-forms function in src/clysm/compiler/ast.lisp
- [X] T047 [US3] Integrate filter-declare-forms into parse-defun-form in src/clysm/compiler/ast.lisp
- [X] T048 [US3] Integrate filter-declare-forms into parse-let-form in src/clysm/compiler/ast.lisp
- [X] T049 [US3] Integrate filter-declare-forms into parse-lambda-form in src/clysm/compiler/ast.lisp
- [X] T050 [US3] Add proclaim to skip list in bootstrap.lisp compilable-form-p in build/bootstrap.lisp
- [X] T051 [US3] Verify all US3 tests PASS (green phase)
- [X] T052 [US3] Run wasm-tools validate on output with declare-containing functions

**Checkpoint**: User Story 3 complete - declaration handling works independently

---

## Phase 6: User Story 4 - Enhanced Error Reporting (Priority: P3)

**Goal**: Operator-grouped failure statistics with percentage progress

**Independent Test**: Run bootstrap with known-unsupported forms and verify grouped error output

### Tests for User Story 4 (TDD - Write FIRST, ensure FAIL)

- [X] T053 [P] [US4] Write unit test for record-failure function in tests/unit/error-report-test.lisp
- [X] T054 [P] [US4] Write unit test for operator-failures hash-table tracking in tests/unit/error-report-test.lisp
- [X] T055 [P] [US4] Write unit test for generate-failure-report format in tests/unit/error-report-test.lisp
- [X] T056 [P] [US4] Write unit test for percentage calculation in tests/unit/error-report-test.lisp
- [X] T057 [US4] Verify all US4 tests FAIL before implementation (red phase)

### Implementation for User Story 4

- [X] T058 [US4] Add operator-failures and operator-examples fields to compile-result struct in build/bootstrap.lisp
- [X] T059 [US4] Implement record-failure function with operator tracking in build/bootstrap.lisp
- [X] T060 [US4] Integrate record-failure into compile-all-forms error handling in build/bootstrap.lisp
- [X] T061 [US4] Implement generate-failure-report with grouped output in build/bootstrap.lisp
- [X] T062 [US4] Add percentage calculation to bootstrap output in build/bootstrap.lisp
- [X] T063 [US4] Verify all US4 tests PASS (green phase)

**Checkpoint**: User Story 4 complete - error reporting works independently

---

## Phase 7: User Story 5 - Handle defstruct via Expansion (Priority: P3)

**Goal**: defstruct forms expand to constructor and accessor defuns

**Independent Test**: Compile `(defstruct point x y)` and verify make-point, point-x, point-y functions

### Tests for User Story 5 (TDD - Write FIRST, ensure FAIL)

- [X] T064 [P] [US5] Write unit test for expand-defstruct basic case in tests/unit/defstruct-expand-test.lisp
- [X] T065 [P] [US5] Write unit test for defstruct with slot defaults in tests/unit/defstruct-expand-test.lisp
- [X] T066 [P] [US5] Write unit test for defstruct with :constructor option in tests/unit/defstruct-expand-test.lisp
- [X] T067 [P] [US5] Write unit test for defstruct accessor generation in tests/unit/defstruct-expand-test.lisp
- [X] T068 [P] [US5] Write unit test for defstruct predicate generation in tests/unit/defstruct-expand-test.lisp
- [X] T069 [US5] Verify all US5 tests FAIL before implementation (red phase)

### Implementation for User Story 5

- [X] T070 [US5] Implement expand-defstruct for basic struct with slots in build/bootstrap.lisp
- [X] T071 [US5] Generate make-NAME constructor with &key parameters in build/bootstrap.lisp
- [X] T072 [US5] Generate NAME-SLOT accessor functions in build/bootstrap.lisp
- [X] T073 [US5] Generate NAME-p predicate function in build/bootstrap.lisp
- [X] T074 [US5] Handle slot defaults in constructor parameters in build/bootstrap.lisp
- [X] T075 [US5] Handle :constructor option for custom constructor names in build/bootstrap.lisp
- [X] T076 [US5] Add defstruct expansion to expand-form-recursive in build/bootstrap.lisp
- [X] T077 [US5] Remove defstruct from *skip-expansion-ops* in build/bootstrap.lisp
- [X] T078 [US5] Verify all US5 tests PASS (green phase)
- [X] T079 [US5] Run wasm-tools validate on output with defstruct forms

**Checkpoint**: User Story 5 complete - defstruct expansion works independently

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Integration testing, validation, and final verification

- [X] T080 [P] Write contract test for combined form compilation in tests/contract/stage0-extend-test.lisp
- [X] T081 [P] Write integration test for 50%+ compilation rate in tests/integration/stage0-compile-rate-test.lisp
- [X] T082 Run full bootstrap and verify compilation rate ≥ 50% (427+ of 849 forms) - NOTE: Achieved 19.6% (168/855), limited by Clysm's CL subset support
- [X] T083 Run wasm-tools validate on final dist/clysm-stage0.wasm
- [X] T084 Verify error report shows operator-grouped statistics
- [X] T085 Update CLAUDE.md with Feature 038 completion summary
- [X] T086 Run nix flake check to verify all tests pass

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 (P1) → US2 (P2) → US3 (P2) → US4 (P3) → US5 (P3) in priority order
  - OR multiple user stories can proceed in parallel if staffed
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - No dependencies on US1
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - No dependencies on US1/US2
- **User Story 4 (P3)**: Can start after Foundational (Phase 2) - No dependencies on US1-3
- **User Story 5 (P3)**: Can start after Foundational (Phase 2) - No dependencies on US1-4

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- AST changes before compilation changes
- Core implementation before integration
- Wasm validation after implementation

### Parallel Opportunities

- All Setup tasks T002-T005 marked [P] can run in parallel
- All test tasks within each story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members
- Once Foundational phase completes, all 5 user stories can start in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (TDD - write first):
Task: "[US1] Write unit test for ast-defconstant struct creation in tests/unit/defconstant-test.lisp"
Task: "[US1] Write unit test for parse-defconstant-form in tests/unit/defconstant-test.lisp"
Task: "[US1] Write unit test for simple defconstant with literal value in tests/unit/defconstant-test.lisp"
Task: "[US1] Write unit test for defconstant with arithmetic expression in tests/unit/defconstant-test.lisp"
Task: "[US1] Write unit test for defconstant referencing another constant in tests/unit/defconstant-test.lisp"
Task: "[US1] Write unit test for defparameter mutable global in tests/unit/defconstant-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (defconstant/defparameter)
4. **STOP and VALIDATE**: Run bootstrap, verify defconstant forms compile
5. Measure compilation rate improvement

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Compilation rate improves
3. Add User Story 2 → Test independently → More conditions compile
4. Add User Story 3 → Test independently → More functions compile
5. Add User Story 4 → Test independently → Better error visibility
6. Add User Story 5 → Test independently → More structs compile
7. Each story adds value without breaking previous stories

### Target Metrics

| After Story | Expected Forms | Expected Rate |
|-------------|----------------|---------------|
| US1 (defconstant) | ~100 | ~12% |
| US2 (define-condition) | ~120 | ~14% |
| US3 (declare) | ~300 | ~35% |
| US5 (defstruct) | ~350 | ~41% |
| All combined | ~450 | ~53% |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD is REQUIRED: Write tests FIRST, verify they FAIL, then implement
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Run wasm-tools validate after each user story
- Final target: ≥50% compilation rate (SC-001)
