# Tasks: func-section.lisp Refactoring

**Input**: Design documents from `/specs/001-func-section-refactor/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: Required per Constitution Principle VII (TDD非交渉)

**Organization**: Tasks grouped by user story, following plan.md phase dependencies.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Compiler codegen**: `src/clysm/compiler/codegen/`
- **Runtime lib**: `src/clysm/lib/`
- **Unit tests**: `tests/unit/`
- **Contract tests**: `tests/contract/`
- **Integration tests**: `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Record baseline metrics and prepare for refactoring

- [ ] T001 Record baseline line count for src/clysm/compiler/codegen/func-section.lisp (expect ~16,483)
- [ ] T002 Run `sbcl --eval "(asdf:test-system :clysm)"` and confirm all tests pass
- [ ] T003 Run `sbcl --load build/stage1-complete.lisp` and record compilation rate from dist/stage1-report.json
- [ ] T004 Run `wasm-tools validate dist/clysm-stage1.wasm` and confirm exit code 0

---

## Phase 2: Foundational (Instruction Collector Infrastructure)

**Purpose**: O(n) instruction collection pattern - BLOCKS User Story 1 dispatch table work

**⚠️ CRITICAL**: The dispatch table refactoring (US1) cannot begin until this phase is complete because all primitive compilers use instruction collection.

### Tests for Instruction Collector

- [ ] T005 [P] Write unit test for with-instruction-collector macro in tests/unit/instruction-collector-test.lisp
- [ ] T006 [P] Write unit test verifying emit/emit* produce correct instruction order in tests/unit/instruction-collector-test.lisp
- [ ] T007 Run tests and confirm they FAIL (TDD red phase)

### Implementation for Instruction Collector

- [ ] T008 Implement with-instruction-collector macro in src/clysm/compiler/codegen/func-section.lisp
- [ ] T009 Implement emit local macro inside with-instruction-collector in src/clysm/compiler/codegen/func-section.lisp
- [ ] T010 Implement emit* local macro inside with-instruction-collector in src/clysm/compiler/codegen/func-section.lisp
- [ ] T011 Run tests and confirm they PASS (TDD green phase)
- [ ] T012 Convert 5 append-based functions to use with-instruction-collector as proof of concept
- [ ] T013 Run full test suite `sbcl --eval "(asdf:test-system :clysm)"` to verify no regressions
- [ ] T014 Run `wasm-tools validate dist/clysm-stage1.wasm` to verify Wasm output unchanged

**Checkpoint**: Instruction collector ready - User Story 1 can now begin

---

## Phase 3: User Story 1 - Primitive Dispatch Table (Priority: P1) 🎯 MVP

**Goal**: Replace 200+ case branches with hash table dispatch for adding new primitives without modifying dispatch logic

**Independent Test**: Add test primitive via registration API, verify compilation succeeds without touching case statement

### Tests for User Story 1

- [ ] T015 [P] [US1] Write unit test for register-primitive-compiler in tests/unit/primitive-dispatch-test.lisp
- [ ] T016 [P] [US1] Write unit test for lookup-primitive-compiler in tests/unit/primitive-dispatch-test.lisp
- [ ] T017 [P] [US1] Write unit test for duplicate registration (override) behavior in tests/unit/primitive-dispatch-test.lisp
- [ ] T018 [P] [US1] Write unit test for by-name registration (%SETF-* ops) in tests/unit/primitive-dispatch-test.lisp
- [ ] T019 [P] [US1] Write contract test verifying Wasm output unchanged after dispatch migration in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T020 [US1] Run tests and confirm they FAIL (TDD red phase)

### Implementation for User Story 1

- [ ] T021 [US1] Create *primitive-compilers* hash table (:test eq) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T022 [US1] Create *primitive-compilers-by-name* hash table (:test equal) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T023 [US1] Implement register-primitive-compiler function in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T024 [US1] Implement lookup-primitive-compiler function in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T025 [US1] Implement unregister-primitive-compiler function in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T026 [US1] Implement list-registered-primitives debug function in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T027 [US1] Run unit tests and confirm they PASS (TDD green phase)
- [ ] T028 [US1] Add primitive-dispatch.lisp to clysm.asd system definition
- [ ] T029 [US1] Create hybrid compile-primitive-call that tries table then falls back to case in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T030 [P] [US1] Register arithmetic primitives (+, -, *, /) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T031 [P] [US1] Register type predicates (consp, symbolp, numberp, etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T032 [P] [US1] Register list primitives (car, cdr, cons, list, list*) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T033 [US1] Add primitive-registry.lisp to clysm.asd system definition
- [ ] T034 [US1] Update func-section.lisp compile-primitive-call to use new dispatch in src/clysm/compiler/codegen/func-section.lisp
- [ ] T035 [US1] Run contract test and confirm Wasm output unchanged
- [ ] T036 [US1] Run `sbcl --load build/stage1-complete.lisp` and verify compilation rate unchanged
- [ ] T037 [US1] Run `wasm-tools validate dist/clysm-stage1.wasm` and verify exit code 0
- [ ] T038 [US1] Migrate remaining primitives from case statement to registration (batch 1: 50 primitives)
- [ ] T039 [US1] Migrate remaining primitives from case statement to registration (batch 2: 50 primitives)
- [ ] T040 [US1] Migrate remaining primitives from case statement to registration (batch 3: remaining primitives)
- [ ] T041 [US1] Remove legacy case branches from func-section.lisp once all primitives migrated
- [ ] T042 [US1] Verify P457 (COMPILE-UNARY-MATH-FFI) error pattern eliminated by running Stage 1 compilation
- [ ] T043 [US1] Record new line count for func-section.lisp (target: -300 lines)

**Checkpoint**: User Story 1 complete - primitives can be added via registration without modifying dispatch logic

---

## Phase 4: User Story 5 - Equality Function Unification (Priority: P2)

**Goal**: Unify eq/eql/equal/equalp into shared type dispatch infrastructure

**Independent Test**: Add new type to type dispatch table, verify all 4 equality predicates handle it correctly

### Tests for User Story 5

- [ ] T044 [P] [US5] Write unit test for build-equality-type-dispatch with :eq level in tests/unit/type-dispatch-test.lisp
- [ ] T045 [P] [US5] Write unit test for build-equality-type-dispatch with :eql level in tests/unit/type-dispatch-test.lisp
- [ ] T046 [P] [US5] Write unit test for build-equality-type-dispatch with :equal level in tests/unit/type-dispatch-test.lisp
- [ ] T047 [P] [US5] Write unit test for build-equality-type-dispatch with :equalp level in tests/unit/type-dispatch-test.lisp
- [ ] T048 [P] [US5] Write contract test verifying Wasm output matches existing 024-equality-predicates tests in tests/contract/equality-wasm-test.lisp
- [ ] T049 [US5] Run tests and confirm they FAIL (TDD red phase)

### Implementation for User Story 5

- [ ] T050 [US5] Create type-dispatch.lisp module structure in src/clysm/compiler/codegen/type-dispatch.lisp
- [ ] T051 [US5] Implement build-equality-type-dispatch function in src/clysm/compiler/codegen/type-dispatch.lisp
- [ ] T052 [US5] Implement compile-type-dispatch code generator in src/clysm/compiler/codegen/type-dispatch.lisp
- [ ] T053 [US5] Implement compile-equality-predicate unified function in src/clysm/compiler/codegen/type-dispatch.lisp
- [ ] T054 [US5] Run unit tests and confirm they PASS (TDD green phase)
- [ ] T055 [US5] Add type-dispatch.lisp to clysm.asd system definition
- [ ] T056 [US5] Refactor compile-eq to use compile-equality-predicate with :level :eq in src/clysm/compiler/codegen/func-section.lisp
- [ ] T057 [US5] Refactor compile-eql to use compile-equality-predicate with :level :eql in src/clysm/compiler/codegen/func-section.lisp
- [ ] T058 [US5] Refactor compile-equal to use compile-equality-predicate with :level :equal in src/clysm/compiler/codegen/func-section.lisp
- [ ] T059 [US5] Refactor compile-equalp to use compile-equality-predicate with :level :equalp in src/clysm/compiler/codegen/func-section.lisp
- [ ] T060 [US5] Run 024-equality-predicates test suite and confirm all pass
- [ ] T061 [US5] Run contract test and confirm Wasm output unchanged
- [ ] T062 [US5] Remove duplicated type dispatch code from original equality functions in func-section.lisp
- [ ] T063 [US5] Record new line count for func-section.lisp (target: -400 lines cumulative)

**Checkpoint**: User Story 5 complete - equality predicates use unified type dispatch

---

## Phase 5: User Story 4 - cXXr Function Consolidation (Priority: P3)

**Goal**: Replace 12 individual compile-cXXr functions with macro-generated versions

**Independent Test**: Compile all cXXr operations (caar, cadr, etc.), verify output identical to previous implementation

### Tests for User Story 4

- [ ] T064 [P] [US4] Write unit test for compile-cxr-chain function in tests/unit/cxr-consolidation-test.lisp
- [ ] T065 [P] [US4] Write unit test for define-cxr-compiler macro expansion in tests/unit/cxr-consolidation-test.lisp
- [ ] T066 [P] [US4] Write contract test verifying all 12 cXXr Wasm outputs unchanged in tests/contract/cxr-wasm-test.lisp
- [ ] T067 [US4] Run tests and confirm they FAIL (TDD red phase)

### Implementation for User Story 4

- [ ] T068 [US4] Export compile-cxr-chain from clysm/compiler/codegen package in src/clysm/package.lisp
- [ ] T069 [US4] Implement define-cxr-compiler macro in src/clysm/compiler/codegen/func-section.lisp
- [ ] T070 [US4] Run unit tests and confirm they PASS (TDD green phase)
- [ ] T071 [US4] Replace compile-caar with (define-cxr-compiler caar (:car :car)) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T072 [US4] Replace compile-cadr with (define-cxr-compiler cadr (:cdr :car)) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T073 [US4] Replace compile-cdar with (define-cxr-compiler cdar (:car :cdr)) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T074 [US4] Replace compile-cddr with (define-cxr-compiler cddr (:cdr :cdr)) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T075 [US4] Replace remaining 8 cXXr functions (caaar through cddddr) with macro calls in src/clysm/compiler/codegen/func-section.lisp
- [ ] T076 [US4] Run contract test and confirm Wasm output unchanged
- [ ] T077 [US4] Verify P626 (COMPILE-CXR-CHAIN) error pattern eliminated by running Stage 1 compilation
- [ ] T078 [US4] Record new line count for func-section.lisp (target: -100 lines cumulative)

**Checkpoint**: User Story 4 complete - cXXr functions consolidated via macro

---

## Phase 6: User Story 2 - Runtime Function Migration (Priority: P2)

**Goal**: Migrate complex functions from inline Wasm to Lisp runtime library

**Independent Test**: Migrate string-trim family, verify tests pass and compilation rate unchanged

### Tests for User Story 2

- [ ] T079 [P] [US2] Write unit test for string-runtime.lisp string-trim implementation in tests/unit/string-runtime-test.lisp
- [ ] T080 [P] [US2] Write unit test for numeric-runtime.lisp parse-integer implementation in tests/unit/numeric-runtime-test.lisp
- [ ] T081 [P] [US2] Write contract test verifying runtime call Wasm generation in tests/contract/runtime-call-wasm-test.lisp
- [ ] T082 [US2] Run tests and confirm they FAIL (TDD red phase)

### Implementation for User Story 2 - String Functions

- [ ] T083 [US2] Create string-runtime.lisp module in src/clysm/lib/string-runtime.lisp
- [ ] T084 [US2] Implement string-trim in src/clysm/lib/string-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_stg_tr.htm)
- [ ] T085 [US2] Implement string-left-trim in src/clysm/lib/string-runtime.lisp
- [ ] T086 [US2] Implement string-right-trim in src/clysm/lib/string-runtime.lisp
- [ ] T087 [US2] Implement string-upcase in src/clysm/lib/string-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_stg_up.htm)
- [ ] T088 [US2] Implement string-downcase in src/clysm/lib/string-runtime.lisp
- [ ] T089 [US2] Implement string-capitalize in src/clysm/lib/string-runtime.lisp
- [ ] T090 [US2] Implement nstring-upcase, nstring-downcase, nstring-capitalize in src/clysm/lib/string-runtime.lisp
- [ ] T091 [US2] Register string functions in *runtime-function-table* in src/clysm/lib/string-runtime.lisp
- [ ] T092 [US2] Add string-runtime.lisp to clysm.asd system definition
- [ ] T093 [US2] Remove compile-string-trim (135 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T094 [US2] Remove compile-string-capitalize (126 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T095 [US2] Remove compile-string-compare-ci (125 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T096 [US2] Remove compile-nstring-capitalize (125 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T097 [US2] Remove remaining string compile-* functions from func-section.lisp
- [ ] T098 [US2] Run string-related tests and confirm all pass

### Implementation for User Story 2 - Numeric Functions

- [ ] T099 [US2] Create numeric-runtime.lisp module in src/clysm/lib/numeric-runtime.lisp
- [ ] T100 [US2] Implement parse-integer in src/clysm/lib/numeric-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_parse_.htm)
- [ ] T101 [US2] Implement write-to-string in src/clysm/lib/numeric-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_wr_to_.htm)
- [ ] T102 [US2] Implement rationalize in src/clysm/lib/numeric-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_ration.htm)
- [ ] T103 [US2] Implement signum in src/clysm/lib/numeric-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_signum.htm)
- [ ] T104 [US2] Implement phase in src/clysm/lib/numeric-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_phase.htm)
- [ ] T105 [US2] Register numeric functions in *runtime-function-table* in src/clysm/lib/numeric-runtime.lisp
- [ ] T106 [US2] Add numeric-runtime.lisp to clysm.asd system definition
- [ ] T107 [US2] Remove compile-parse-integer (242 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T108 [US2] Remove compile-write-to-string (214 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T109 [US2] Remove compile-rationalize (192 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T110 [US2] Remove compile-signum (152 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T111 [US2] Remove compile-phase (116 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T112 [US2] Run numeric-related tests and confirm all pass

### Implementation for User Story 2 - Sequence/Array Functions

- [ ] T113 [US2] Implement subseq in src/clysm/lib/sequence-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_subseq.htm)
- [ ] T114 [US2] Implement adjust-array in src/clysm/lib/sequence-runtime.lisp per [HyperSpec](resources/HyperSpec/Body/f_adjust.htm)
- [ ] T115 [US2] Register sequence functions in *runtime-function-table* in src/clysm/lib/sequence-runtime.lisp
- [ ] T116 [US2] Remove compile-subseq (189 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T117 [US2] Remove compile-adjust-array (143 lines) from src/clysm/compiler/codegen/func-section.lisp
- [ ] T118 [US2] Run sequence-related tests and confirm all pass
- [ ] T119 [US2] Verify P334 (REGISTER-RUNTIME-FUNCTION) error pattern eliminated by running Stage 1 compilation
- [ ] T120 [US2] Record new line count for func-section.lisp (target: -1,750 lines cumulative)

**Checkpoint**: User Story 2 complete - complex functions migrated to runtime library

---

## Phase 7: User Story 3 - Remaining Efficiency Conversion (Priority: P3)

**Goal**: Complete conversion of all append patterns to instruction collector

**Independent Test**: Measure compilation time before/after on representative module

### Implementation for User Story 3 (Completion)

- [ ] T121 [US3] Convert remaining append patterns in func-section.lisp (estimated 500 occurrences)
- [ ] T122 [US3] Run `sbcl --eval "(asdf:test-system :clysm)"` and confirm all tests pass
- [ ] T123 [US3] Measure compilation time on large module and compare to baseline
- [ ] T124 [US3] Run `sbcl --load build/stage1-complete.lisp` and verify compilation rate unchanged
- [ ] T125 [US3] Record new line count for func-section.lisp (target: additional -500 lines)

**Checkpoint**: User Story 3 complete - all instruction collection uses O(n) pattern

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final cleanup, verification, and documentation

- [ ] T126 [P] Remove any dead code paths from func-section.lisp
- [ ] T127 [P] Update CLAUDE.md with new module documentation
- [ ] T128 Run final line count and verify <8,000 lines achieved in func-section.lisp
- [ ] T129 Run `sbcl --eval "(asdf:test-system :clysm)"` and confirm all tests pass
- [ ] T130 Run `sbcl --load build/stage1-complete.lisp` and verify compilation rate ≥19.20%
- [ ] T131 Run `wasm-tools validate dist/clysm-stage1.wasm` and verify exit code 0
- [ ] T132 Verify all error patterns eliminated: P457, P626, P334
- [ ] T133 Run quickstart.md verification steps
- [ ] T134 [P] Create integration test validating full refactor in tests/integration/func-section-refactor-test.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phase 3 (US1)**: Depends on Phase 2 instruction collector
- **Phase 4 (US5)**: Depends on Phase 3 dispatch table
- **Phase 5 (US4)**: Can start after Phase 2 (parallel with US5)
- **Phase 6 (US2)**: Depends on Phase 4 equality unification
- **Phase 7 (US3)**: Can start after Phase 2 (parallel with other stories)
- **Phase 8 (Polish)**: Depends on all user stories complete

### User Story Dependencies

```
Setup (Phase 1)
    │
    ▼
Foundational: Instruction Collector (Phase 2) ─────────────────┐
    │                                                          │
    ▼                                                          │
US1: Dispatch Table (Phase 3) ──────┐                          │
    │                               │                          │
    ▼                               ▼                          ▼
US5: Equality Unification (P4)    US4: cXXr (P5)         US3: Efficiency (P7)
    │                               │                          │
    ▼                               │                          │
US2: Runtime Migration (Phase 6) ◀──┴──────────────────────────┘
    │
    ▼
Polish (Phase 8)
```

### Parallel Opportunities

**Within Phase 2 (Foundational)**:
- T005, T006 (unit tests) can run in parallel

**Within Phase 3 (US1)**:
- T015, T016, T017, T018, T019 (tests) can run in parallel
- T030, T031, T032 (primitive registration batches) can run in parallel

**Within Phase 4 (US5)**:
- T044, T045, T046, T047, T048 (tests) can run in parallel

**Within Phase 5 (US4)**:
- T064, T065, T066 (tests) can run in parallel

**Within Phase 6 (US2)**:
- T079, T080, T081 (tests) can run in parallel

**Cross-Phase Parallelism**:
- US4 (Phase 5) can run in parallel with US5 (Phase 4) after Phase 2
- US3 completion (Phase 7) can run in parallel with US2 (Phase 6)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write unit test for register-primitive-compiler in tests/unit/primitive-dispatch-test.lisp"
Task: "Write unit test for lookup-primitive-compiler in tests/unit/primitive-dispatch-test.lisp"
Task: "Write unit test for duplicate registration behavior in tests/unit/primitive-dispatch-test.lisp"
Task: "Write unit test for by-name registration in tests/unit/primitive-dispatch-test.lisp"
Task: "Write contract test verifying Wasm output in tests/contract/primitive-dispatch-wasm-test.lisp"

# Launch primitive registration batches together:
Task: "Register arithmetic primitives in src/clysm/compiler/codegen/primitive-registry.lisp"
Task: "Register type predicates in src/clysm/compiler/codegen/primitive-registry.lisp"
Task: "Register list primitives in src/clysm/compiler/codegen/primitive-registry.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (baseline metrics)
2. Complete Phase 2: Foundational (instruction collector)
3. Complete Phase 3: User Story 1 (dispatch table)
4. **STOP and VALIDATE**: New primitives can be added without modifying case statement
5. Demonstrate value: Add test primitive, verify compilation

### Incremental Delivery

1. Setup + Foundational → Instruction collector ready
2. Add US1 → Test dispatch table → **MVP!** (primitives via registration)
3. Add US5 → Test equality unification → Cleaner equality predicates
4. Add US4 → Test cXXr consolidation → Code deduplication
5. Add US2 → Test runtime migration → Major line reduction
6. Add US3 completion → Performance improvement
7. Polish → Final verification

### Target Line Reduction Per Phase

| Phase | Target | Cumulative | Remaining |
|-------|--------|------------|-----------|
| Baseline | - | 16,483 | 16,483 |
| Phase 3 (US1) | -300 | 16,183 | 16,183 |
| Phase 4 (US5) | -400 | 15,783 | 15,783 |
| Phase 5 (US4) | -100 | 15,683 | 15,683 |
| Phase 6 (US2) | -1,750 | 13,933 | 13,933 |
| Phase 7 (US3) | -500 | 13,433 | 13,433 |
| Phase 8 (Polish) | -5,500+ | <8,000 | <8,000 |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- TDD required per Constitution Principle VII: verify tests FAIL before implementing
- Commit after each task or logical group
- Run `wasm-tools validate` after any Wasm-affecting changes
- HyperSpec links provided for ANSI CL function implementations
