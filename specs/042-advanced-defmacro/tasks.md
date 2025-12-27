# Tasks: Advanced Defmacro and Compile-Time Macro Expansion

**Input**: Design documents from `/specs/042-advanced-defmacro/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/macro-api.lisp

**Tests**: Included per Constitution Principle VII (TDD Non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Files modified: `src/clysm/compiler/transform/macro.lisp`, `src/clysm/compiler/codegen/gc-types.lisp`, `src/clysm/compiler/codegen/func-section.lisp`, `src/clysm/compiler/ast.lisp`
- Files created: `src/clysm/runtime/macro-runtime.lisp`, `tests/unit/macro/*.lisp`, `tests/contract/macro-wasm-test.lisp`, `tests/integration/macro-ansi-test.lisp`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test structure

- [ ] T001 Create test directory structure at tests/unit/macro/
- [ ] T002 [P] Create placeholder test file at tests/unit/macro/whole-test.lisp with package definition
- [ ] T003 [P] Create placeholder test file at tests/unit/macro/environment-test.lisp with package definition
- [ ] T004 [P] Create placeholder test file at tests/unit/macro/macro-function-test.lisp with package definition
- [ ] T005 [P] Create contract test file at tests/contract/macro-wasm-test.lisp with package definition
- [ ] T006 [P] Create integration test file at tests/integration/macro-ansi-test.lisp with package definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures that MUST be complete before ANY user story can be implemented

**CRITICAL**: No user story work can begin until this phase is complete

- [ ] T007 Define macro-lambda-list-info struct in src/clysm/compiler/transform/macro.lisp
- [ ] T008 Define macro-environment struct in src/clysm/compiler/transform/macro.lisp
- [ ] T009 Add $macro-environment WasmGC type (index 24) in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T010 Define macro-lambda-list-malformed condition in src/clysm/compiler/transform/macro.lisp
- [ ] T011 Define macro-expansion-depth-exceeded condition in src/clysm/compiler/transform/macro.lisp
- [ ] T012 Extend defmacro-result struct with lambda-info slot in src/clysm/compiler/transform/macro.lisp
- [ ] T013 Add unregister-macro function to macro-registry in src/clysm/compiler/transform/macro.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Define Macros with &whole Parameter (Priority: P1)

**Goal**: Support `&whole` as first lambda-list element, binding complete macro call form

**Independent Test**: Define macro with `&whole`, invoke it, verify form parameter receives `(my-macro arg1 arg2)`

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T014 [P] [US1] Unit test: &whole as first element extracts variable in tests/unit/macro/whole-test.lisp
- [ ] T015 [P] [US1] Unit test: &whole followed by required params works in tests/unit/macro/whole-test.lisp
- [ ] T016 [P] [US1] Unit test: &whole with destructuring params works in tests/unit/macro/whole-test.lisp
- [ ] T017 [P] [US1] Unit test: &whole not first signals error in tests/unit/macro/whole-test.lisp
- [ ] T018 [P] [US1] Unit test: &whole without following variable signals error in tests/unit/macro/whole-test.lisp

### Implementation for User Story 1

- [ ] T019 [US1] Implement extract-whole-param helper function in src/clysm/compiler/transform/macro.lisp
- [ ] T020 [US1] Implement parse-macro-lambda-list with &whole detection in src/clysm/compiler/transform/macro.lisp
- [ ] T021 [US1] Update parse-defmacro to call parse-macro-lambda-list in src/clysm/compiler/transform/macro.lisp
- [ ] T022 [US1] Update compile-defmacro to bind &whole variable to form in src/clysm/compiler/transform/macro.lisp
- [ ] T023 [US1] Verify all &whole tests pass, run wasm-tools validate on output

**Checkpoint**: User Story 1 fully functional - macros can use &whole parameter

---

## Phase 4: User Story 2 - Define Macros with &environment Parameter (Priority: P1)

**Goal**: Support `&environment` anywhere in lambda-list, binding lexical environment object

**Independent Test**: Define macro with `&environment`, verify env-macro-function can lookup local macros

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T024 [P] [US2] Unit test: &environment extracts env variable in tests/unit/macro/environment-test.lisp
- [ ] T025 [P] [US2] Unit test: &environment can appear anywhere in lambda-list in tests/unit/macro/environment-test.lisp
- [ ] T026 [P] [US2] Unit test: env-macro-function finds local macros in tests/unit/macro/environment-test.lisp
- [ ] T027 [P] [US2] Unit test: env-macro-function searches parent chain in tests/unit/macro/environment-test.lisp
- [ ] T028 [P] [US2] Unit test: make-macro-environment creates valid struct in tests/unit/macro/environment-test.lisp
- [ ] T029 [P] [US2] Unit test: extend-environment creates child with parent link in tests/unit/macro/environment-test.lisp

### Implementation for User Story 2

- [ ] T030 [US2] Implement extract-environment-param helper in src/clysm/compiler/transform/macro.lisp
- [ ] T031 [US2] Update parse-macro-lambda-list with &environment detection in src/clysm/compiler/transform/macro.lisp
- [ ] T032 [US2] Implement make-macro-environment function in src/clysm/compiler/transform/macro.lisp
- [ ] T033 [US2] Implement env-macro-function with parent chain search in src/clysm/compiler/transform/macro.lisp
- [ ] T034 [US2] Implement extend-environment function in src/clysm/compiler/transform/macro.lisp
- [ ] T035 [US2] Update compile-defmacro to bind &environment variable in src/clysm/compiler/transform/macro.lisp
- [ ] T036 [US2] Update macroexpand-1* to pass environment to expanders in src/clysm/compiler/transform/macro.lisp
- [ ] T037 [US2] Verify all &environment tests pass, run wasm-tools validate on output

**Checkpoint**: User Stories 1 AND 2 complete - macros can use &whole and &environment

---

## Phase 5: User Story 3 - Use macroexpand at Runtime (Priority: P2)

**Goal**: Runtime `macroexpand` and `macroexpand-1` in compiled Wasm code with two-value return

**Independent Test**: Call `(macroexpand-1 '(my-macro arg))` in compiled code, verify returns (expanded-form T)

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T038 [P] [US3] Unit test: macro-function returns expander or nil in tests/unit/macro/macro-function-test.lisp
- [ ] T039 [P] [US3] Unit test: macroexpand-1 returns (form nil) for non-macro in tests/unit/macro/macro-function-test.lisp
- [ ] T040 [P] [US3] Unit test: macroexpand-1 returns (expanded t) for macro in tests/unit/macro/macro-function-test.lisp
- [ ] T041 [P] [US3] Unit test: macroexpand loops until non-macro in tests/unit/macro/macro-function-test.lisp
- [ ] T042 [P] [US3] Unit test: macroexpand signals error after 1000 steps in tests/unit/macro/macro-function-test.lisp
- [ ] T043 [P] [US3] Contract test: runtime-macroexpand-1 Wasm IR validates in tests/contract/macro-wasm-test.lisp
- [ ] T044 [P] [US3] Contract test: runtime-macroexpand Wasm IR validates in tests/contract/macro-wasm-test.lisp

### Implementation for User Story 3

- [ ] T045 [US3] Implement macro-function ANSI CL API in src/clysm/compiler/transform/macro.lisp
- [ ] T046 [US3] Implement macroexpand-1 with two-value return in src/clysm/compiler/transform/macro.lisp
- [ ] T047 [US3] Implement macroexpand with depth counter and two-value return in src/clysm/compiler/transform/macro.lisp
- [ ] T048 [US3] Create src/clysm/runtime/macro-runtime.lisp with package definition
- [ ] T049 [US3] Implement runtime-macro-function Wasm IR generator in src/clysm/compiler/codegen/func-section.lisp
- [ ] T050 [US3] Implement runtime-macroexpand-1 Wasm IR generator in src/clysm/compiler/codegen/func-section.lisp
- [ ] T051 [US3] Implement runtime-macroexpand Wasm IR generator in src/clysm/compiler/codegen/func-section.lisp
- [ ] T052 [US3] Add compile-macroexpand AST node in src/clysm/compiler/ast.lisp
- [ ] T053 [US3] Implement compile-macro-function AST node in src/clysm/compiler/ast.lisp
- [ ] T054 [US3] Export macro registry as Wasm global for runtime access in src/clysm/compiler/codegen/func-section.lisp
- [ ] T055 [US3] Verify all runtime macroexpand tests pass

**Checkpoint**: User Story 3 complete - runtime macro expansion works in compiled Wasm

---

## Phase 6: User Story 4 - Self-Compile Clysm's Defmacro Definitions (Priority: P2)

**Goal**: Compile all 27 defmacro forms in Clysm source with Clysm compiler

**Independent Test**: Run compiler on Clysm source files, verify 100% defmacro compilation success

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T056 [P] [US4] Integration test: when* macro compiles and expands correctly in tests/integration/macro-ansi-test.lisp
- [ ] T057 [P] [US4] Integration test: cond* macro compiles and expands correctly in tests/integration/macro-ansi-test.lisp
- [ ] T058 [P] [US4] Integration test: handler-case macro compiles and expands correctly in tests/integration/macro-ansi-test.lisp
- [ ] T059 [P] [US4] Integration test: all 27 defmacro forms compile without error in tests/integration/macro-ansi-test.lisp
- [ ] T060 [P] [US4] Integration test: self-compiled macro produces same expansion as host in tests/integration/macro-ansi-test.lisp

### Implementation for User Story 4

- [ ] T061 [US4] Implement (setf macro-function) for runtime macro definition in src/clysm/compiler/transform/macro.lisp
- [ ] T062 [US4] Add compile-(setf-macro-function) AST node in src/clysm/compiler/ast.lisp
- [ ] T063 [US4] Implement Wasm IR for (setf macro-function) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T064 [US4] Create self-compilation test script at scripts/test-defmacro-self-compile.sh
- [ ] T065 [US4] Run self-compilation on lib/macros.lisp defmacros (7 forms)
- [ ] T066 [US4] Run self-compilation on conditions/*.lisp defmacros (7 forms)
- [ ] T067 [US4] Run self-compilation on remaining defmacros (13 forms)
- [ ] T068 [US4] Verify 100% defmacro compilation rate achieved

**Checkpoint**: User Story 4 complete - all Clysm defmacros self-compile successfully

---

## Phase 7: User Story 5 - Macro Expansion Error Reporting (Priority: P3)

**Goal**: Clear error messages with original form and context on expansion failure

**Independent Test**: Trigger macro error, verify message includes macro name and original form

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T069 [P] [US5] Unit test: expansion error includes original form in tests/unit/macro/whole-test.lisp
- [ ] T070 [P] [US5] Unit test: argument count mismatch reports expected vs actual in tests/unit/macro/whole-test.lisp
- [ ] T071 [P] [US5] Unit test: invalid argument type reports which argument in tests/unit/macro/whole-test.lisp

### Implementation for User Story 5

- [ ] T072 [US5] Enhance macro-expansion-depth-exceeded to include original form in src/clysm/compiler/transform/macro.lisp
- [ ] T073 [US5] Add expansion-context tracking to macroexpand-1 in src/clysm/compiler/transform/macro.lisp
- [ ] T074 [US5] Update compile-defmacro error handler to capture &whole form in src/clysm/compiler/transform/macro.lisp
- [ ] T075 [US5] Add argument count validation with clear error messages in src/clysm/compiler/transform/macro.lisp
- [ ] T076 [US5] Verify all error reporting tests pass

**Checkpoint**: User Story 5 complete - macro errors are informative and actionable

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T077 [P] Update CLAUDE.md with Feature 042 documentation
- [ ] T078 [P] Add docstrings to all new public functions in src/clysm/compiler/transform/macro.lisp
- [ ] T079 Run full test suite: `rove tests/unit/macro/*.lisp`
- [ ] T080 Run contract tests: `rove tests/contract/macro-wasm-test.lisp`
- [ ] T081 Run integration tests: `rove tests/integration/macro-ansi-test.lisp`
- [ ] T082 Run wasm-tools validate on all generated Wasm output
- [ ] T083 Run quickstart.md examples and verify all work correctly
- [ ] T084 Run `nix flake check` to verify green build

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - &whole support
- **User Story 2 (Phase 4)**: Depends on Foundational - &environment support
- **User Story 3 (Phase 5)**: Depends on US1+US2 - runtime macroexpand needs both
- **User Story 4 (Phase 6)**: Depends on US3 - self-compilation needs runtime expansion
- **User Story 5 (Phase 7)**: Depends on US1 - error reporting uses &whole
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

```
Foundational
     │
     ├──► US1 (&whole) ────┐
     │                      │
     └──► US2 (&environment)┼──► US3 (runtime) ──► US4 (self-compile)
                            │
                            └──► US5 (errors)
```

- **US1 + US2**: Can run in parallel after Foundational
- **US3**: Needs both US1 and US2 complete
- **US4**: Needs US3 complete
- **US5**: Can start after US1 (uses &whole for error context)

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Helper functions before main implementation
- Parse phase before compile phase
- Verify tests pass before moving to next story

### Parallel Opportunities

- T002-T006: All test file scaffolding in parallel
- T014-T018: All US1 tests in parallel
- T024-T029: All US2 tests in parallel
- T038-T044: All US3 tests in parallel
- T056-T060: All US4 tests in parallel
- T069-T071: All US5 tests in parallel
- T077-T078: Documentation updates in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Unit test: &whole as first element extracts variable in tests/unit/macro/whole-test.lisp"
Task: "Unit test: &whole followed by required params works in tests/unit/macro/whole-test.lisp"
Task: "Unit test: &whole with destructuring params works in tests/unit/macro/whole-test.lisp"
Task: "Unit test: &whole not first signals error in tests/unit/macro/whole-test.lisp"
Task: "Unit test: &whole without following variable signals error in tests/unit/macro/whole-test.lisp"
```

---

## Parallel Example: User Stories 1 + 2 Simultaneously

```bash
# After Foundational phase, US1 and US2 can proceed in parallel:

# Developer A working on US1:
Task: "[US1] Implement extract-whole-param helper function in src/clysm/compiler/transform/macro.lisp"

# Developer B working on US2 (different functions in same file - coordinate):
Task: "[US2] Implement extract-environment-param helper in src/clysm/compiler/transform/macro.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (&whole)
4. Complete Phase 4: User Story 2 (&environment)
5. **STOP and VALIDATE**: Test &whole and &environment independently
6. Can deploy/demo: Macros with advanced lambda-lists work

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 → Test &whole independently → Checkpoint
3. Add US2 → Test &environment independently → Checkpoint
4. Add US3 → Test runtime macroexpand → Checkpoint
5. Add US4 → Verify self-compilation → Major milestone!
6. Add US5 → Better error messages → Full feature complete

### Self-Hosting Milestone

After US4 completion:
- All 27 Clysm defmacros compile successfully
- Self-compilation rate improved
- Bootstrap verification passes

---

## Notes

- [P] tasks = different files or independent functions
- [Story] label maps task to specific user story for traceability
- Each user story independently completable and testable
- TDD required per Constitution Principle VII
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- wasm-tools validate required for all Wasm output
