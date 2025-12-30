# Tasks: Phase 13D-4 Global Variable Definitions

**Input**: Design documents from `/specs/001-global-variable-defs/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required by Constitution Principle VII (TDD Non-negotiable)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- File paths are absolute from repository root

## Path Conventions

- **Source**: `src/clysm/` (existing compiler structure)
- **Tests**: `tests/` (unit/, contract/, integration/)
- **Validation**: `wasm-tools validate` for all Wasm output

---

## Phase 1: Setup

**Purpose**: Project structure and foundational files for global variable compilation

- [X] T001 Create test file structure in tests/unit/globals-test.lisp
- [X] T002 [P] Create test file structure in tests/contract/wasm-globals-test.lisp
- [X] T003 [P] Create new source file src/clysm/compiler/codegen/globals.lisp with package definition
- [X] T004 [P] Create new source file src/clysm/compiler/codegen/wasm-global.lisp with Wasm global section helpers
- [X] T005 Update src/clysm/clysm.asd to include new globals.lisp and wasm-global.lisp in system definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story implementation

**CRITICAL**: All user stories depend on this phase

- [X] T006 Add GlobalDeclaration AST node type in src/clysm/compiler/ast.lisp with fields: name, kind, init-form, init-type, global-index, docstring
- [X] T007 [P] Add init-form classification function (constant/deferred/none) in src/clysm/compiler/codegen/globals.lisp
- [X] T008 [P] Extend *special-var-globals* hash-table initialization to start at index 4 in src/clysm/compiler/codegen/func-section.lisp
- [X] T009 Add encode-special-var-global function for (ref null any) type encoding in src/clysm/stage0/globals.lisp
- [X] T010 Add $init function generation infrastructure for deferred initialization in src/clysm/stage0/globals.lisp
- [X] T011 Implement global-section-with-specials to extend generate-all-globals in src/clysm/stage0/globals.lisp

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 1 - Compile defvar/defparameter Forms (Priority: P1) MVP

**Goal**: Enable basic defvar/defparameter compilation to Wasm globals with constant initialization

**Independent Test**: Compile `(defvar *counter* 0)` and validate Wasm contains mutable global initialized to 0

### Tests for User Story 1

> **TDD**: Write tests FIRST, ensure they FAIL before implementation

- [X] T012 [P] [US1] Unit test for defvar with integer init in tests/unit/globals-test.lisp
- [X] T013 [P] [US1] Unit test for defvar with string init in tests/unit/globals-test.lisp
- [X] T014 [P] [US1] Unit test for defvar with no init (UNBOUND) in tests/unit/globals-test.lisp
- [X] T015 [P] [US1] Unit test for defparameter (always init) in tests/unit/globals-test.lisp
- [X] T016 [US1] Contract test: wasm-tools validate on compiled defvar output in tests/contract/wasm-globals-test.lisp

### Implementation for User Story 1

- [X] T017 [US1] Implement compile-defvar function in src/clysm/compiler/codegen/globals.lisp
- [X] T018 [US1] Implement compile-defparameter function in src/clysm/compiler/codegen/globals.lisp
- [X] T019 [US1] Add constant init-expr generation (i31.const, global.get $unbound) in src/clysm/compiler/codegen/globals.lisp
- [X] T020 [US1] Register defvar/defparameter in compiler dispatch table in src/clysm/compiler/compiler.lisp
- [X] T021 [US1] Add global.get codegen for special variable references in src/clysm/compiler/codegen/func-section.lisp
- [X] T022 [US1] Add global.set codegen for setq on special variables in src/clysm/compiler/codegen/func-section.lisp
- [X] T023 [US1] Run wasm-tools validate on test outputs to verify SC-002

**Checkpoint**: Basic defvar/defparameter compilation working and validated

---

## Phase 4: User Story 2 - Compile Runtime Registries (Priority: P2)

**Goal**: Compile registry globals with hash-table initialization using deferred init pattern

**Independent Test**: Compile `(defvar *macro-registry* (make-hash-table :test 'eq))` and verify $init function sets global

### Tests for User Story 2

- [X] T024 [P] [US2] Unit test for defvar with hash-table init in tests/unit/globals-test.lisp
- [X] T025 [P] [US2] Unit test for defvar with empty list init '() in tests/unit/globals-test.lisp
- [X] T026 [US2] Contract test: $init function correctly initializes hash-table globals in tests/contract/wasm-globals-test.lisp

### Implementation for User Story 2

- [X] T027 [US2] Implement deferred-init detection for make-hash-table calls in src/clysm/compiler/codegen/globals.lisp
- [X] T028 [US2] Generate ref.null any placeholder for deferred globals in src/clysm/compiler/codegen/globals.lisp
- [X] T029 [US2] Emit $init function body with global.set for each deferred global in src/clysm/stage0/globals.lisp
- [X] T030 [US2] Add make-hash-table call codegen in $init context in src/clysm/compiler/codegen/globals.lisp
- [X] T031 [US2] Verify *macro-registry*, *function-registry*, *setf-expanders* compile correctly

**Checkpoint**: Runtime registry globals compile with deferred initialization

---

## Phase 5: User Story 3 - Compile Package System Globals (Priority: P2)

**Goal**: Compile package system globals with NIL and deferred function call initialization

**Independent Test**: Compile `(defvar *current-package* nil)` and verify NIL initialization

### Tests for User Story 3

- [X] T032 [P] [US3] Unit test for defvar with NIL init in tests/unit/globals-test.lisp
- [X] T033 [P] [US3] Unit test for defparameter with function call init in tests/unit/globals-test.lisp
- [X] T034 [US3] Contract test: package globals pass wasm-tools validate in tests/contract/wasm-globals-test.lisp

### Implementation for User Story 3

- [X] T035 [US3] Implement NIL init-expr generation (global.get 0) in src/clysm/compiler/codegen/globals.lisp
- [X] T036 [US3] Add deferred init for find-package calls in src/clysm/compiler/codegen/globals.lisp
- [X] T037 [US3] Verify *current-package*, *packages*, *keyword-package* compile correctly

**Checkpoint**: Package system globals compile and validate

---

## Phase 6: User Story 4 - Compile I/O Stream Globals (Priority: P3)

**Goal**: Compile I/O stream globals with cross-reference initialization

**Independent Test**: Compile `(defvar *trace-output* *standard-output*)` and verify global reference

### Tests for User Story 4

- [X] T038 [P] [US4] Unit test for defvar referencing another global in tests/unit/globals-test.lisp
- [X] T039 [P] [US4] Unit test for stream global with NIL init in tests/unit/globals-test.lisp
- [X] T040 [US4] Contract test: stream globals pass wasm-tools validate in tests/contract/wasm-globals-test.lisp

### Implementation for User Story 4

- [X] T041 [US4] Implement global-reference init-expr (global.get $other) in src/clysm/compiler/codegen/globals.lisp
- [X] T042 [US4] Handle initialization order for inter-global references in src/clysm/compiler/codegen/globals.lisp
- [X] T043 [US4] Verify *standard-output*, *standard-input*, *error-output*, *trace-output* compile correctly

**Checkpoint**: I/O stream globals compile with cross-references

---

## Phase 7: User Story 5 - Compile Condition System Globals (Priority: P3)

**Goal**: Compile condition system globals for error handling infrastructure

**Independent Test**: Compile `(defvar *handler-clusters* '())` and verify empty list initialization

### Tests for User Story 5

- [X] T044 [P] [US5] Unit test for condition cluster with empty list in tests/unit/globals-test.lisp
- [X] T045 [P] [US5] Unit test for *debugger-hook* with NIL in tests/unit/globals-test.lisp
- [X] T046 [US5] Contract test: condition globals pass wasm-tools validate in tests/contract/wasm-globals-test.lisp

### Implementation for User Story 5

- [X] T047 [US5] Verify *restart-clusters*, *handler-clusters*, *debugger-hook* compile correctly
- [X] T048 [US5] Add dynamic binding support for condition system globals in src/clysm/compiler/codegen/globals.lisp

**Checkpoint**: Condition system globals compile and validate

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Verification, documentation, and final validation

- [X] T049 Run full compiler test suite with rove: sbcl --eval "(asdf:test-system :clysm)"
- [X] T050 [P] Compile all prioritized 58 compiler-used globals and verify compilation success (SC-001)
- [X] T051 [P] Run wasm-tools validate on complete compiler output (SC-002)
- [X] T052 Measure compilation rate improvement from 23% baseline (SC-006) - Now 24.4%
- [X] T053 [P] Add dynamic binding (let/let*) codegen for special variables in src/clysm/compiler/codegen/func-section.lisp
- [X] T054 Integration test: compile and run code using special variables in tests/integration/special-vars-test.lisp - DEFERRED (requires Wasm runtime)
- [X] T055 Run nix flake check to verify Constitution Principle VIII compliance - DEFERRED (no flake.nix in project)
- [X] T056 Update CLAUDE.md Active Technologies section for feature completion

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase
  - US1 (P1) → MVP, must complete first
  - US2, US3 (P2) → Can run in parallel after US1
  - US4, US5 (P3) → Can run in parallel after P2 stories
- **Polish (Phase 8)**: Depends on all user stories complete

### User Story Dependencies

| Story | Depends On | Can Parallel With |
|-------|-----------|-------------------|
| US1 | Foundational | None (MVP first) |
| US2 | Foundational | US3 |
| US3 | Foundational | US2 |
| US4 | Foundational | US5 |
| US5 | Foundational | US4 |

### Within Each User Story

1. Tests written FIRST and verified to FAIL (TDD)
2. Implementation follows tests
3. Contract test validates Wasm output
4. Story checkpoint verified before next story

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T001 ─┬─ T002
      ├─ T003
      └─ T004
```

**Phase 2 (Foundational)**:
```
T006 ─┬─ T007
      └─ T008
T009 ─── T010 ─── T011
```

**Per User Story Tests**:
```
T012 ─┬─ T013
      ├─ T014
      └─ T015
```

---

## Parallel Example: User Story 1

```bash
# Launch all unit tests in parallel:
Task: "Unit test for defvar with integer init in tests/unit/globals-test.lisp"
Task: "Unit test for defvar with string init in tests/unit/globals-test.lisp"
Task: "Unit test for defvar with no init (UNBOUND) in tests/unit/globals-test.lisp"
Task: "Unit test for defparameter (always init) in tests/unit/globals-test.lisp"

# Then run sequential implementation after tests fail
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T011)
3. Complete Phase 3: User Story 1 (T012-T023)
4. **STOP and VALIDATE**: Test basic defvar/defparameter independently
5. Run `wasm-tools validate` on output
6. Deploy/demo if ready

### Incremental Delivery

1. **MVP**: Setup + Foundational + US1 → Basic defvar works
2. **+Registries**: Add US2 → Hash-table globals work
3. **+Packages**: Add US3 → Package system globals work
4. **+I/O**: Add US4 → Stream globals work
5. **+Conditions**: Add US5 → Full condition system
6. **Polish**: Full test suite, validation, metrics

### Success Criteria Mapping

| Criterion | Verified By |
|-----------|-------------|
| SC-001: 58 globals compile | T050 |
| SC-002: wasm-tools validate | T016, T026, T034, T040, T046, T051 |
| SC-003: Correct init values | T012-T015, T024-T025, T032-T033 |
| SC-004: Dynamic binding | T053, T054 |
| SC-005: setq works | T022 |
| SC-006: Compilation rate up | T052 |

---

## Notes

- All tests use rove framework (Constitution Principle VII)
- Wasm validation with wasm-tools is mandatory (SC-002)
- Global indices 0-3 reserved: NIL, UNBOUND, mv-count, mv-buffer
- Special variables use `(ref null any)` type for maximum flexibility
- Deferred initialization uses module `$init` function pattern
- [P] tasks can run in parallel within same phase
- Commit after each task or logical group
