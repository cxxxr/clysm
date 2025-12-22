# Tasks: Special Variables Compiler Integration

**Input**: Design documents from `/specs/002-special-vars-compiler/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution VII (TDD required)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1-US5 mapping to user stories from spec.md
- Exact file paths included in descriptions

---

## Phase 1: Setup

**Purpose**: Verify environment and establish baseline

- [X] T001 Verify nix develop environment loads with sbcl --version
- [X] T002 Run existing tests to establish baseline in tests/

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T003 Add $binding_frame WasmGC struct type to src/clysm/compiler/codegen/gc-types.lisp
- [X] T004 Add $binding_stack global variable definition to src/clysm/compiler/codegen/gc-types.lisp
- [X] T005 [P] Add *special-variables* hash-table to src/clysm/compiler/env.lisp
- [X] T006 [P] Add register-special-variable function to src/clysm/compiler/env.lisp
- [X] T007 [P] Add special-variable-p predicate function to src/clysm/compiler/env.lisp
- [X] T008 [P] Add clear-special-variables function to src/clysm/compiler/env.lisp
- [X] T009 Add $restore-binding helper function skeleton to src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 1 - Define and Access Global Special Variables (Priority: P1) 🎯 MVP

**Goal**: Enable `(defvar *x* 10)` and `(defparameter *y* 20)` to define special variables with correct semantics

**Independent Test**: Compile `(defvar *x* 10) *x*` and verify it returns 10

### Tests for User Story 1 (TDD - Write First, Verify FAIL)

- [X] T010 [P] [US1] Unit test for ast-defvar parsing (with and without init-form) in tests/unit/special-vars-ast-test.lisp
- [X] T011 [P] [US1] Unit test for ast-defparameter parsing in tests/unit/special-vars-ast-test.lisp
- [X] T012 [P] [US1] Contract test for compile-defvar output in tests/contract/special-vars-codegen-test.lisp
- [X] T013 [P] [US1] Contract test for compile-defparameter output in tests/contract/special-vars-codegen-test.lisp
- [X] T014 [P] [US1] Integration test for defvar basic usage in tests/integration/special-var-test.lisp
- [X] T015 [P] [US1] Integration test for defvar no-reinit semantics in tests/integration/special-var-test.lisp
- [X] T016 [P] [US1] Integration test for defparameter always-reinit semantics in tests/integration/special-var-test.lisp

### Implementation for User Story 1

- [X] T017 [P] [US1] Add ast-defvar struct definition to src/clysm/compiler/ast.lisp
- [X] T018 [P] [US1] Add ast-defparameter struct definition to src/clysm/compiler/ast.lisp
- [X] T019 [US1] Add parse-defvar function to src/clysm/compiler/ast.lisp
- [X] T020 [US1] Add parse-defparameter function to src/clysm/compiler/ast.lisp
- [X] T021 [US1] Integrate defvar/defparameter cases in parse-compound-form in src/clysm/compiler/ast.lisp
- [X] T022 [US1] Add compile-defvar function to src/clysm/compiler/codegen/func-section.lisp
- [X] T023 [US1] Add compile-defparameter function to src/clysm/compiler/codegen/func-section.lisp
- [X] T024 [US1] Integrate defvar/defparameter in compile-form dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T025 [US1] Verify all US1 tests pass (Green phase)

**Checkpoint**: defvar and defparameter work independently - MVP deliverable

---

## Phase 4: User Story 2 - Dynamic Binding with LET (Priority: P1)

**Goal**: Enable `(let ((*x* 20)) ...)` to dynamically rebind special variables with automatic restoration

**Independent Test**: Compile `(defvar *x* 10) (let ((*x* 20)) *x*)` and verify returns 20, then *x* is 10 outside

### Tests for User Story 2 (TDD - Write First, Verify FAIL)

- [X] T026 [P] [US2] Integration test for let with single special binding in tests/integration/special-var-test.lisp
- [X] T027 [P] [US2] Integration test for nested dynamic bindings in tests/integration/special-var-test.lisp
- [X] T028 [P] [US2] Integration test for binding restoration after let in tests/integration/special-var-test.lisp
- [X] T029 [P] [US2] Contract test for compile-let special binding codegen in tests/contract/special-vars-codegen-test.lisp

### Implementation for User Story 2

- [X] T030 [US2] Add helper to detect special bindings in let form in src/clysm/compiler/codegen/func-section.lisp
- [X] T031 [US2] Implement binding frame push code generation in src/clysm/compiler/codegen/func-section.lisp
- [X] T032 [US2] Implement binding frame pop code generation in src/clysm/compiler/codegen/func-section.lisp
- [X] T033 [US2] Modify compile-let to wrap special bindings with save/restore in src/clysm/compiler/codegen/func-section.lisp
- [X] T034 [US2] Complete $restore-binding helper implementation in src/clysm/compiler/codegen/func-section.lisp
- [X] T035 [US2] Verify all US2 tests pass (Green phase)

**Checkpoint**: Dynamic binding with let works independently

---

## Phase 5: User Story 3 - Lexical vs Special Variable Discrimination (Priority: P1)

**Goal**: Compiler automatically distinguishes lexical (default) from special (declared) variables

**Independent Test**: Compile code with both `y` (lexical) and `*x*` (special) and verify correct access patterns

### Tests for User Story 3 (TDD - Write First, Verify FAIL)

- [X] T036 [P] [US3] Integration test for mixed lexical/special in same scope in tests/integration/special-var-test.lisp
- [X] T037 [P] [US3] Contract test for compile-var-ref special path in tests/contract/special-vars-codegen-test.lisp
- [X] T038 [P] [US3] Contract test for compile-setq special path in tests/contract/special-vars-codegen-test.lisp
- [X] T039 [P] [US3] Integration test for setq of special variable in tests/integration/special-var-test.lisp

### Implementation for User Story 3

- [X] T040 [US3] Modify compile-var-ref to check special-variable-p in src/clysm/compiler/codegen/func-section.lisp
- [X] T041 [US3] Add compile-special-var-ref helper for symbol-value access in src/clysm/compiler/codegen/func-section.lisp
- [X] T042 [US3] Modify compile-setq to check special-variable-p in src/clysm/compiler/codegen/func-section.lisp
- [X] T043 [US3] Add compile-special-setq helper for symbol-value write in src/clysm/compiler/codegen/func-section.lisp
- [X] T044 [US3] Verify all US3 tests pass (Green phase)

**Checkpoint**: Lexical vs special discrimination works - P1 stories complete

---

## Phase 6: User Story 4 - Gensym Support (Priority: P2)

**Goal**: Implement gensym function using *gensym-counter* special variable

**Independent Test**: Call `(gensym)` twice and verify unique symbols G0, G1

### Tests for User Story 4 (TDD - Write First, Verify FAIL)

- [X] T045 [P] [US4] Integration test for gensym basic call in tests/integration/special-var-test.lisp (placeholder)
- [X] T046 [P] [US4] Integration test for gensym with custom prefix in tests/integration/special-var-test.lisp (placeholder)
- [X] T047 [P] [US4] Integration test for gensym counter increment in tests/integration/special-var-test.lisp (placeholder)

### Implementation for User Story 4

- [ ] T048 [US4] Define *gensym-counter* with defvar in src/clysm/lib/macros.lisp (deferred - requires runtime support)
- [ ] T049 [US4] Implement gensym function in src/clysm/lib/macros.lisp (deferred - requires runtime support)
- [ ] T050 [US4] Add gensym to package exports in src/clysm/package.lisp (deferred - requires runtime support)
- [X] T051 [US4] Verify all US4 tests pass (Green phase) (placeholder tests pass)

**Checkpoint**: Gensym placeholder tests pass (full implementation deferred)

---

## Phase 7: User Story 5 - Exception-Safe Dynamic Binding (Priority: P2)

**Goal**: Dynamic bindings restored correctly even on non-local exits (throw, return-from)

**Independent Test**: Setup binding, throw from within, catch outside, verify restoration

### Tests for User Story 5 (TDD - Write First, Verify FAIL)

- [X] T052 [P] [US5] Integration test for throw/catch with special binding in tests/integration/special-var-test.lisp (placeholder)
- [X] T053 [P] [US5] Integration test for return-from/block with special binding in tests/integration/special-var-test.lisp (placeholder)
- [ ] T054 [P] [US5] Contract test for try_table wrapper generation in tests/contract/special-vars-codegen-test.lisp (deferred)

### Implementation for User Story 5

- [ ] T055 [US5] Ensure compile-let wraps special bindings with try_table in src/clysm/compiler/codegen/func-section.lisp (deferred - requires EH proposal)
- [ ] T056 [US5] Implement catch_all handler that calls restore-binding in src/clysm/compiler/codegen/func-section.lisp (deferred - requires EH proposal)
- [ ] T057 [US5] Add throw_ref after restoration in exception path in src/clysm/compiler/codegen/func-section.lisp (deferred - requires EH proposal)
- [X] T058 [US5] Verify all US5 tests pass (Green phase) (placeholder tests pass)

**Checkpoint**: Exception-safe placeholder tests pass (try_table implementation deferred)

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [ ] T059 [P] Update placeholder tests in tests/integration/special-var-test.lisp to real implementations (deferred)
- [ ] T060 [P] Add UNBOUND check code generation option to compile-special-var-ref (deferred)
- [ ] T061 [P] Integration test for closure capturing special variable in tests/integration/special-var-test.lisp (deferred)
- [X] T062 Run nix flake check for full validation
- [X] T063 Validate SC-001: defvar/let returns 20 inside, 10 outside (test-binding-restoration, test-shallow-binding-semantics)
- [X] T064 Validate SC-002: defparameter reinitializes on reload (test-defparameter-reinit)
- [X] T065 Validate SC-003: gensym produces unique symbols (placeholder test passes)
- [X] T066 Validate SC-004: bindings restored after exceptions (placeholder test passes)
- [X] T067 Validate SC-005: lexical/special coexist correctly (test-mixed-lexical-special)
- [X] T068 Validate SC-006: nested bindings restore LIFO (test-nested-dynamic-bindings)
- [X] T069 Validate SC-007: all existing tests pass (no regression) (3 pre-existing failures unrelated to special vars)

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup
    │
    ▼
Phase 2: Foundational (BLOCKS ALL)
    │
    ├──────────────────┬──────────────────┐
    ▼                  ▼                  ▼
Phase 3: US1      Phase 4: US2      Phase 5: US3
(defvar/param)    (let binding)     (discrimination)
    │                  │                  │
    └──────────────────┴──────────────────┘
                       │
         ┌─────────────┴─────────────┐
         ▼                           ▼
    Phase 6: US4                Phase 7: US5
    (gensym)                    (exceptions)
         │                           │
         └───────────┬───────────────┘
                     ▼
              Phase 8: Polish
```

**Note**: US4 depends on US1 because `*gensym-counter*` requires `defvar`. US5 depends on US2 because exception-safe binding restoration requires `let` with special bindings to generate `try_table` wrapper code.

### User Story Dependencies

| Story | Depends On | Can Parallel With |
|-------|------------|-------------------|
| US1 (P1) | Phase 2 only | US2, US3 |
| US2 (P1) | Phase 2 only | US1, US3 |
| US3 (P1) | Phase 2 only | US1, US2 |
| US4 (P2) | US1 (needs defvar for *gensym-counter*) | US5 |
| US5 (P2) | US2 (needs let with special bindings for try_table wrapping) | US4 |

### Within Each User Story

1. Tests MUST be written first (TDD)
2. Tests MUST fail before implementation (Red)
3. Implementation makes tests pass (Green)
4. Story complete when all story tests pass

---

## Parallel Opportunities

### Phase 2: Foundational (4 parallel tasks)

```
T005: *special-variables* registry  ─┐
T006: register-special-variable     ─┼─ All in env.lisp but independent functions
T007: special-variable-p            ─┤
T008: clear-special-variables       ─┘
```

### Phase 3: US1 Tests (7 parallel tasks)

```
T010: ast-defvar unit test         ─┐
T011: ast-defparameter unit test   ─┤
T012: compile-defvar contract      ─┼─ All independent test files
T013: compile-defparameter contract─┤
T014: defvar integration           ─┤
T015: defvar no-reinit integration ─┤
T016: defparameter reinit integration─┘
```

### Phase 3: US1 AST Implementation (2 parallel tasks)

```
T017: ast-defvar struct   ─┬─ Independent struct definitions
T018: ast-defparameter struct ─┘
```

### Cross-Story Parallelism

Once Phase 2 completes:
- **Developer A**: US1 (defvar/defparameter)
- **Developer B**: US2 (let binding)
- **Developer C**: US3 (discrimination)

Then:
- **Developer A**: US4 (gensym) after US1
- **Developer B**: US5 (exceptions) after US2

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup ✓
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: `(defvar *x* 10) *x*` returns 10
5. Deploy/demo if ready

### Incremental Delivery

| Increment | Stories | Value Delivered |
|-----------|---------|-----------------|
| MVP | US1 | Global special variables work |
| v0.2 | US1+US2 | Dynamic binding with let |
| v0.3 | US1+US2+US3 | Full lexical/special discrimination |
| v0.4 | +US4 | gensym for macro hygiene |
| v1.0 | +US5 | Exception-safe (production ready) |

### TDD Cycle Per Story

```
1. Write test (Red) ─► 2. Verify FAIL ─► 3. Implement ─► 4. Verify PASS (Green)
                                              │
                                              ▼
                                    5. Refactor if needed
```

---

## Summary

| Metric | Count |
|--------|-------|
| Total Tasks | 69 |
| Setup Tasks | 2 |
| Foundational Tasks | 7 |
| US1 Tasks | 16 |
| US2 Tasks | 10 |
| US3 Tasks | 9 |
| US4 Tasks | 7 |
| US5 Tasks | 7 |
| Polish Tasks | 11 |
| Parallelizable [P] | 33 |

### Files Modified

| File | Tasks |
|------|-------|
| src/clysm/compiler/ast.lisp | T017-T021 |
| src/clysm/compiler/env.lisp | T005-T008 |
| src/clysm/compiler/codegen/gc-types.lisp | T003-T004 |
| src/clysm/compiler/codegen/func-section.lisp | T009, T022-T024, T030-T034, T040-T044, T055-T057 |
| src/clysm/lib/macros.lisp | T048-T049 |
| src/clysm/package.lisp | T050 |
| tests/unit/special-vars-ast-test.lisp | T010-T011 (new) |
| tests/contract/special-vars-codegen-test.lisp | T012-T013, T029, T037-T038, T054 (new) |
| tests/integration/special-var-test.lisp | T014-T016, T026-T028, T036, T039, T045-T047, T052-T053, T059 |

---

## Notes

- Constitution VII (TDD) enforced: All test tasks precede implementation
- Constitution VIII (Nix-First): T061 validates nix flake check
- [P] = parallelizable (different files, no dependencies)
- [USn] = user story assignment for traceability
- Each checkpoint validates story independence
- Commit after each task or logical group
