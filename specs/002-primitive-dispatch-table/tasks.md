# Tasks: Primitive Dispatch Table

**Input**: Design documents from `/specs/002-primitive-dispatch-table/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included (TDD mandated by Constitution Principle VII)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup

**Purpose**: Verify existing infrastructure and establish baselines

- [ ] T001 Verify existing dispatch infrastructure in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T002 [P] Generate baseline Wasm output for all primitives using current case statement
- [ ] T003 [P] Create primitive count verification script to track registration progress

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Modify compile-primitive-call to use dispatch-primitive

**⚠️ CRITICAL**: No primitive migration can begin until this phase is complete

- [ ] T004 Modify compile-primitive-call to call dispatch-primitive in src/clysm/compiler/codegen/func-section.lisp
- [ ] T005 Add NIL fallback to case statement for unregistered primitives in src/clysm/compiler/codegen/func-section.lisp
- [ ] T006 Verify compiler still works with empty registry (all primitives fall back to case)

**Checkpoint**: Dispatch integration ready - primitive registration can now begin

---

## Phase 3: User Story 1 - Primitive Compilation Lookup (Priority: P1) 🎯 MVP

**Goal**: Migrate all 240+ primitives to hash-table dispatch with byte-identical Wasm output

**Independent Test**: Compile test suite of all primitives, compare Wasm output byte-for-byte against baseline

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T007 [P] [US1] Create baseline comparison test framework in tests/unit/primitive-baseline-test.lisp
- [ ] T008 [P] [US1] Add byte-identical verification test for type predicates in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T009 [P] [US1] Add byte-identical verification test for list operations in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T010 [P] [US1] Add byte-identical verification test for arithmetic in tests/contract/primitive-dispatch-wasm-test.lisp

### Implementation for User Story 1 - High Priority Categories

#### Type Predicates (25 primitives)

- [ ] T011 [US1] Register type predicate primitives ([consp](resources/HyperSpec/Body/f_consp.htm), [symbolp](resources/HyperSpec/Body/f_symbol.htm), [numberp](resources/HyperSpec/Body/f_nump.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T012 [US1] Verify type predicates pass baseline comparison test
- [ ] T013 [US1] Remove type predicate case branches from src/clysm/compiler/codegen/func-section.lisp

#### List Operations (35 primitives)

- [ ] T014 [US1] Register list primitives ([car](resources/HyperSpec/Body/f_car_c.htm), [cdr](resources/HyperSpec/Body/f_car_c.htm), [cons](resources/HyperSpec/Body/f_cons.htm), [list](resources/HyperSpec/Body/f_list_.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T015 [US1] Verify list operations pass baseline comparison test
- [ ] T016 [US1] Remove list operation case branches from src/clysm/compiler/codegen/func-section.lisp

#### Equality (4 primitives)

- [ ] T017 [US1] Register equality primitives ([eq](resources/HyperSpec/Body/f_eq.htm), [eql](resources/HyperSpec/Body/f_eql.htm), [equal](resources/HyperSpec/Body/f_equal.htm), [equalp](resources/HyperSpec/Body/f_equalp.htm)) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T018 [US1] Verify equality primitives pass baseline comparison test
- [ ] T019 [US1] Remove equality case branches from src/clysm/compiler/codegen/func-section.lisp

#### Arithmetic (28 primitives)

- [ ] T020 [US1] Register arithmetic primitives ([+](resources/HyperSpec/Body/f_pl.htm), [-](resources/HyperSpec/Body/f__.htm), [*](resources/HyperSpec/Body/f_st.htm), [/](resources/HyperSpec/Body/f_sl.htm), [1+](resources/HyperSpec/Body/f_1pl_1_.htm), [1-](resources/HyperSpec/Body/f_1pl_1_.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T021 [US1] Verify arithmetic primitives pass baseline comparison test
- [ ] T022 [US1] Remove arithmetic case branches from src/clysm/compiler/codegen/func-section.lisp

#### Comparison (12 primitives)

- [ ] T023 [US1] Register comparison primitives ([=](resources/HyperSpec/Body/f_eq_sle.htm), [<](resources/HyperSpec/Body/f_eq_sle.htm), [>](resources/HyperSpec/Body/f_eq_sle.htm), [<=](resources/HyperSpec/Body/f_eq_sle.htm), [>=](resources/HyperSpec/Body/f_eq_sle.htm), [/=](resources/HyperSpec/Body/f_eq_sle.htm), [zerop](resources/HyperSpec/Body/f_zerop.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T024 [US1] Verify comparison primitives pass baseline comparison test
- [ ] T025 [US1] Remove comparison case branches from src/clysm/compiler/codegen/func-section.lisp

### Implementation for User Story 1 - Medium Priority Categories

#### Sequence (25 primitives)

- [ ] T026 [US1] Register sequence primitives ([length](resources/HyperSpec/Body/f_length.htm), [elt](resources/HyperSpec/Body/f_elt.htm), [subseq](resources/HyperSpec/Body/f_subseq.htm), [reverse](resources/HyperSpec/Body/f_revers.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T027 [US1] Remove sequence case branches from src/clysm/compiler/codegen/func-section.lisp

#### Array (20 primitives)

- [ ] T028 [US1] Register array primitives ([aref](resources/HyperSpec/Body/f_aref.htm), [array-rank](resources/HyperSpec/Body/f_ar_ran.htm), [array-dimension](resources/HyperSpec/Body/f_ar_dim.htm), [make-array](resources/HyperSpec/Body/f_mk_ar.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T029 [US1] Remove array case branches from src/clysm/compiler/codegen/func-section.lisp

#### String (22 primitives)

- [ ] T030 [US1] Register string primitives ([char](resources/HyperSpec/Body/f_char_.htm), [string=](resources/HyperSpec/Body/f_stgeq_.htm), [string-upcase](resources/HyperSpec/Body/f_stg_up.htm), [string-trim](resources/HyperSpec/Body/f_stg_tr.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T031 [US1] Remove string case branches from src/clysm/compiler/codegen/func-section.lisp

#### Character (15 primitives)

- [ ] T032 [US1] Register character primitives ([char-code](resources/HyperSpec/Body/f_char_c.htm), [code-char](resources/HyperSpec/Body/f_code_c.htm), [char-upcase](resources/HyperSpec/Body/f_char_u.htm), [alpha-char-p](resources/HyperSpec/Body/f_alpha_.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T033 [US1] Remove character case branches from src/clysm/compiler/codegen/func-section.lisp

#### Bit Operations (10 primitives)

- [ ] T034 [US1] Register bit operation primitives ([ash](resources/HyperSpec/Body/f_ash.htm), [logand](resources/HyperSpec/Body/f_logand.htm), [logior](resources/HyperSpec/Body/f_logior.htm), [logxor](resources/HyperSpec/Body/f_logxor.htm), [lognot](resources/HyperSpec/Body/f_lognot.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T035 [US1] Remove bit operation case branches from src/clysm/compiler/codegen/func-section.lisp

### Implementation for User Story 1 - Low Priority Categories

#### Trigonometric (12 primitives)

- [ ] T036 [US1] Register trigonometric primitives ([sin](resources/HyperSpec/Body/f_sin_c.htm), [cos](resources/HyperSpec/Body/f_sin_c.htm), [tan](resources/HyperSpec/Body/f_sin_c.htm), [asin](resources/HyperSpec/Body/f_asin_.htm), [acos](resources/HyperSpec/Body/f_asin_.htm), [atan](resources/HyperSpec/Body/f_asin_.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T037 [US1] Remove trigonometric case branches from src/clysm/compiler/codegen/func-section.lisp

#### Hyperbolic (6 primitives)

- [ ] T038 [US1] Register hyperbolic primitives ([sinh](resources/HyperSpec/Body/f_sinh_.htm), [cosh](resources/HyperSpec/Body/f_sinh_.htm), [tanh](resources/HyperSpec/Body/f_sinh_.htm), [asinh](resources/HyperSpec/Body/f_sinh_.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T039 [US1] Remove hyperbolic case branches from src/clysm/compiler/codegen/func-section.lisp

#### Symbol (8 primitives)

- [ ] T040 [US1] Register symbol primitives ([symbol-name](resources/HyperSpec/Body/f_symb_2.htm), [symbol-value](resources/HyperSpec/Body/f_symb_5.htm), [symbol-function](resources/HyperSpec/Body/f_symb_1.htm), [get](resources/HyperSpec/Body/f_get.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T041 [US1] Remove symbol case branches from src/clysm/compiler/codegen/func-section.lisp

#### Hash Table (6 primitives)

- [ ] T042 [US1] Register hash table primitives ([gethash](resources/HyperSpec/Body/f_gethas.htm), [remhash](resources/HyperSpec/Body/f_remhas.htm), [maphash](resources/HyperSpec/Body/f_maphas.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T043 [US1] Remove hash table case branches from src/clysm/compiler/codegen/func-section.lisp

#### Control (8 primitives)

- [ ] T044 [US1] Register control primitives ([apply](resources/HyperSpec/Body/f_apply.htm), [funcall](resources/HyperSpec/Body/f_funcal.htm), [values](resources/HyperSpec/Body/f_values.htm), [error](resources/HyperSpec/Body/f_error.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T045 [US1] Remove control case branches from src/clysm/compiler/codegen/func-section.lisp

#### Misc (14 primitives)

- [ ] T046 [US1] Register misc primitives ([identity](resources/HyperSpec/Body/f_identi.htm), [constantly](resources/HyperSpec/Body/f_cons_1.htm), [not](resources/HyperSpec/Body/f_not.htm), [null](resources/HyperSpec/Body/f_not.htm), etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T047 [US1] Remove misc case branches from src/clysm/compiler/codegen/func-section.lisp

### Cleanup and Verification

- [ ] T048 [US1] Verify compile-primitive-call is under 30 lines (SC-004) in src/clysm/compiler/codegen/func-section.lisp
- [ ] T049 [US1] Run full compiler test suite: `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T050 [US1] Generate Stage 1 and validate Wasm: `sbcl --load build/stage1-complete.lisp && wasm-tools validate dist/clysm-stage1.wasm`
- [ ] T051 [US1] Verify all 240+ primitives are registered with count verification script

**Checkpoint**: All primitives migrated to dispatch table, byte-identical Wasm output verified

---

## Phase 4: User Story 2 - Register New Primitive (Priority: P2)

**Goal**: Verify and document extensibility - add new primitives without modifying core dispatch

**Independent Test**: Register a new test primitive, compile code that uses it, verify Wasm output

### Tests for User Story 2 ⚠️

- [ ] T052 [P] [US2] Create test for registering new primitive in tests/unit/primitive-dispatch-test.lisp
- [ ] T053 [P] [US2] Create test for overwrite warning behavior in tests/unit/primitive-dispatch-test.lisp
- [ ] T054 [P] [US2] Create test for invalid registration error in tests/unit/primitive-dispatch-test.lisp

### Implementation for User Story 2

- [ ] T055 [US2] Create example test primitive registration in tests/unit/primitive-extensibility-test.lisp
- [ ] T056 [US2] Verify no core code changes needed (SC-003) by compiling with new primitive
- [ ] T057 [US2] Verify overwrite warning (FR-008) works correctly
- [ ] T058 [US2] Verify error on NIL name/function (FR-009)
- [ ] T059 [US2] Update quickstart.md with extensibility example in specs/002-primitive-dispatch-table/quickstart.md

**Checkpoint**: Extensibility verified - new primitives can be added without core changes

---

## Phase 5: User Story 3 - Query Registered Primitives (Priority: P3)

**Goal**: Implement and verify query API for introspection

**Independent Test**: Call query functions and verify accurate information about registered primitives

### Tests for User Story 3 ⚠️

- [ ] T060 [P] [US3] Create test for list-registered-primitives in tests/unit/primitive-dispatch-test.lisp
- [ ] T061 [P] [US3] Create test for primitive-registered-p in tests/unit/primitive-dispatch-test.lisp
- [ ] T062 [P] [US3] Create test for get-primitive-info in tests/unit/primitive-dispatch-test.lisp

### Implementation for User Story 3

- [ ] T063 [US3] Verify list-registered-primitives returns all 240+ primitives (FR-007)
- [ ] T064 [US3] Verify primitive-registered-p returns T/NIL correctly (FR-006)
- [ ] T065 [US3] Verify get-primitive-info returns correct metadata including arity
- [ ] T066 [US3] Add category filtering to list-registered-primitives if needed

**Checkpoint**: Query API verified - all introspection functions work correctly

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final verification and cleanup

- [ ] T067 [P] Run full test suite with Nix: `nix flake check`
- [ ] T068 [P] Validate Wasm structure: `wasm-tools print dist/clysm-stage1.wasm | head -100`
- [ ] T069 Code cleanup: remove any dead code from func-section.lisp
- [ ] T070 Update CLAUDE.md with 002-primitive-dispatch-table completion status
- [ ] T071 Run quickstart.md validation steps

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - US1 must complete before US2/US3 (need registered primitives to test)
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - Core functionality
- **User Story 2 (P2)**: Can start after US1 core migration complete - Tests extensibility
- **User Story 3 (P3)**: Can start after US1 core migration complete - Tests introspection

### Within User Story 1

- Tests (T007-T010) MUST be written and FAIL before implementation
- Categories can be migrated in parallel if working on different files
- Per-category cycle: Register → Verify → Remove case branches
- All categories must complete before cleanup (T048-T051)

### Parallel Opportunities

- T002, T003 can run in parallel (Setup)
- T007-T010 can run in parallel (US1 tests)
- T052-T054 can run in parallel (US2 tests)
- T060-T062 can run in parallel (US3 tests)
- T067, T068 can run in parallel (Polish)

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together:
Task: "Create baseline comparison test framework in tests/unit/primitive-baseline-test.lisp"
Task: "Add byte-identical verification test for type predicates in tests/contract/primitive-dispatch-wasm-test.lisp"
Task: "Add byte-identical verification test for list operations in tests/contract/primitive-dispatch-wasm-test.lisp"
Task: "Add byte-identical verification test for arithmetic in tests/contract/primitive-dispatch-wasm-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (all 16 categories)
4. **STOP and VALIDATE**: Test byte-identical output for all primitives
5. Merge as MVP if ready

### Incremental Delivery

1. Complete Setup + Foundational → Dispatch integration ready
2. Migrate high-priority categories (Type Predicates, List, Equality, Arithmetic, Comparison) → Test
3. Migrate medium-priority categories (Sequence, Array, String, Character, Bit) → Test
4. Migrate low-priority categories (Trig, Hyperbolic, Symbol, Hash, Control, Misc) → Test
5. Complete cleanup → MVP complete
6. Add User Story 2 (Extensibility) → Test independently
7. Add User Story 3 (Query API) → Test independently

### Category Batching

Each category follows the TDD cycle:
1. Register primitives in category
2. Run baseline comparison test
3. If pass: Remove case branches
4. If fail: Debug, fix, re-test

---

## Notes

- [P] tasks = different files, no dependencies
- [US1], [US2], [US3] labels map tasks to specific user stories
- TDD mandated by Constitution Principle VII
- HyperSpec links required by Constitution Principle IX
- Commit after each category migration
- Stop at any checkpoint to validate independently
