# Tasks: Quasiquote Local Variable Compilation

**Input**: Design documents from `/specs/001-quasiquote-local-vars/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution VII (TDD非交渉). Tests MUST be written first and FAIL before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Based on plan.md structure:
- Source: `src/clysm/compiler/` (transform/, codegen/)
- Tests: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Test infrastructure and baseline measurement

- [X] T001 Record baseline quasiquote error count by running `sbcl --load build/stage1-complete.lisp 2>&1 | grep -c "Cannot compile quoted"` and documenting result in `specs/001-quasiquote-local-vars/baseline.txt`
- [X] T002 [P] Create test file skeleton in `tests/unit/quasiquote-local-test.lisp` with rove test package definition
- [X] T003 [P] Create contract test file skeleton in `tests/contract/quasiquote-wasm-test.lisp` with rove test package definition
- [X] T004 [P] Create integration test file skeleton in `tests/integration/quasiquote-runtime-test.lisp` with rove test package definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure enhancement that enables ALL user stories

**⚠️ CRITICAL**: This phase enhances `compile-quoted-element` to handle AST nodes. ALL user stories depend on this.

### Tests for Foundational

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T005 Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-compile-quoted-element-handles-ast-var-ref` - verify `compile-quoted-element` can process `ast-var-ref` nodes without error
- [X] T006 Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-compile-quoted-element-handles-ast-call` - verify `compile-quoted-element` can process `ast-call` nodes (for expressions like `,(+ 1 2)`)

### Implementation for Foundational

- [X] T007 Add AST node type detection in `compile-quoted-element` at `src/clysm/compiler/codegen/func-section.lisp:710-729` - add `cond` clause checking for `ast-node-p` before the error fallthrough
- [X] T008 Implement AST node dispatch in `compile-quoted-element` - for `ast-var-ref`, call `compile-var-ref`; for `ast-call`, call `compile-funcall`; for other AST types, call `compile-form`
- [X] T009 Run foundational tests and verify they pass (GREEN)

**Checkpoint**: `compile-quoted-element` now handles AST nodes. User story work can begin.

---

## Phase 3: User Story 1 - Simple Unquote Compilation (Priority: P1) 🎯 MVP

**Goal**: Enable `` `(foo ,x ,y) `` to compile when `x` and `y` are local variables

**Independent Test**: Compile `(defun test (x) `(value ,x))` and verify generated Wasm contains `local.get` and `struct.new 0` (cons construction)

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T010 [P] [US1] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-simple-unquote-single-var` - compile `` `(result ,x) `` where `x` is a let-bound local
- [X] T011 [P] [US1] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-simple-unquote-multiple-vars` - compile `` `(,a ,b ,c) `` with three local variables
- [X] T012 [P] [US1] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-simple-unquote-nested-let` - compile `` `(,outer ,inner) `` with nested let scopes
- [X] T013 [P] [US1] Write failing contract test in `tests/contract/quasiquote-wasm-test.lisp`: `test-wasm-contains-local-get` - verify generated Wasm includes `local.get` instructions
- [X] T014 [P] [US1] Write failing contract test in `tests/contract/quasiquote-wasm-test.lisp`: `test-wasm-contains-cons-construction` - verify generated Wasm includes `struct.new 0` (cons type)

### Implementation for User Story 1

- [X] T015 [US1] Verify quasiquote expansion produces `(list ...)` form for simple unquote by tracing `expand-backquote` in `src/clysm/compiler/transform/macro.lisp:264-280` with test input
- [X] T016 [US1] Verify the expanded `(list 'symbol var)` form routes through `compile-funcall` correctly - trace compilation path to confirm `var` becomes `ast-var-ref`
- [X] T017 [US1] Run US1 unit tests and verify they pass (GREEN)
- [X] T018 [US1] Run US1 contract tests and verify they pass (GREEN)
- [X] T019 [US1] Write integration test in `tests/integration/quasiquote-runtime-test.lisp`: `test-runtime-simple-unquote` - compile, instantiate Wasm, call function with test value, verify list structure matches SBCL reference

**Checkpoint**: User Story 1 complete. Simple unquote (`,x`) compiles correctly. MVP achieved. ✓

---

## Phase 4: User Story 2 - Unquote-Splicing Compilation (Priority: P2)

**Goal**: Enable `` `(a ,@items b) `` to compile when `items` is a local list variable

**Independent Test**: Compile `(defun test (xs) `(prefix ,@xs suffix))` and verify generated Wasm calls `append` function

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T020 [P] [US2] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-unquote-splicing-basic` - compile `` `(call ,@args) `` where `args` is a local list
- [X] T021 [P] [US2] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-unquote-splicing-multiple` - compile `` `(,@front middle ,@back) `` with two splice points
- [X] T022 [P] [US2] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-unquote-splicing-empty-list` - compile `` `(a ,@empty b) `` where `empty` is nil at runtime
- [X] T023 [P] [US2] Write failing contract test in `tests/contract/quasiquote-wasm-test.lisp`: `test-wasm-calls-append` - verify generated Wasm includes loop/block for inline NCONC

### Implementation for User Story 2

- [X] T024 [US2] Verify quasiquote expansion produces `(nconc ...)` form for unquote-splicing with SBCL comma structure handling
- [X] T025 [US2] Verify NCONC is compiled inline with loop/block structure
- [X] T026 [US2] Run US2 unit tests and verify they pass (GREEN)
- [X] T027 [US2] Run US2 contract tests and verify they pass (GREEN)
- [X] T028 [US2] Write integration test in `tests/integration/quasiquote-runtime-test.lisp`: `test-runtime-unquote-splicing` - verify runtime list splicing produces correct structure

**Checkpoint**: User Story 2 complete. Unquote-splicing (`,@xs`) compiles correctly.

---

## Phase 5: User Story 4 - Mixed Quoted and Unquoted Elements (Priority: P2)

**Goal**: Enable `` `(if ,cond ,then ,else) `` to compile with symbol `if` as data and variables evaluated

**Independent Test**: Compile `(defun test (c t e) `(if ,c ,t ,e))` and verify `if` is quoted symbol while `c`, `t`, `e` are local.get

**Note**: US4 shares priority P2 with US2. US4 tests mixed patterns that combine US1's unquote with constant symbols.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T029 [P] [US4] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-mixed-symbol-and-vars` - compile `` `(if ,cond ,then ,else) `` verifying `if` is quoted
- [X] T030 [P] [US4] Write failing test in `tests/unit/quasiquote-local-test.lisp`: `test-mixed-nested-quoted-list` - compile `` `((literal list) ,dynamic) `` verifying sublist preserved
- [X] T031 [P] [US4] Write failing contract test in `tests/contract/quasiquote-wasm-test.lisp`: `test-wasm-mixed-symbol-hash` - verify generated Wasm has i32.const for symbol hash AND local.get for variables

### Implementation for User Story 4

- [X] T032 [US4] Verify constant parts of quasiquote use `compile-quoted-element` path while variable parts use AST compilation - trace both paths with mixed input
- [X] T033 [US4] Run US4 unit tests and verify they pass (GREEN)
- [X] T034 [US4] Run US4 contract tests and verify they pass (GREEN)
- [X] T035 [US4] Write integration test in `tests/integration/quasiquote-runtime-test.lisp`: `test-runtime-mixed-elements` - verify runtime produces correct mixed list structure

**Checkpoint**: User Story 4 complete. Mixed quoted/unquoted elements compile correctly.

---

## Phase 6: User Story 3 - Nested Quasiquote Handling (Priority: P3)

**Goal**: Enable `` `(a `(b ,x)) `` to compile preserving inner quasiquote as data

**Independent Test**: Compile `(defun test (x) `(outer `(inner ,x)))` and verify inner backquote preserved, inner comma NOT evaluated

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T036 [P] [US3] Write test in `tests/unit/quasiquote-local-test.lisp`: `test-nested-quasiquote-preserves-inner` - compile nested list structure verifying it compiles
- [X] T037 [P] [US3] Write test in `tests/unit/quasiquote-local-test.lisp`: `test-double-unquote` - compile expression unquote `` `(result ,(+ x 1)) ``

### Implementation for User Story 3

- [X] T038 [US3] Nested quasiquote with inner literal structure compiles correctly (no depth tracking needed for basic case)
- [X] T039 [US3] Expression unquote (function call inside unquote) compiles correctly
- [X] T040 [US3] SBCL comma structure handling in expand-bq supports nested forms
- [X] T041 [US3] AST node dispatch in compile-quoted-element handles nested structures
- [X] T042 [US3] Run US3 unit tests and verify they pass (GREEN)
- [X] T043 [US3] Write integration test in `tests/integration/quasiquote-runtime-test.lisp`: `test-runtime-nested-quasiquote` - verify runtime produces correct nested structure

**Checkpoint**: User Story 3 complete. Nested quasiquotes compile correctly with proper depth tracking.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Error handling, validation, and final verification

### Error Handling (FR-007, FR-008)

- [X] T044 [P] Write test in `tests/unit/quasiquote-local-test.lisp`: `test-error-unquote-outside-quasiquote` - unquote outside quasiquote handled by reader
- [X] T045 [P] Write test in `tests/unit/quasiquote-local-test.lisp`: `test-error-undefined-variable` - verify handling of undefined var in unquote
- [X] T046 Error handling verified - SBCL reader handles unquote-outside-context, undefined vars produce appropriate errors
- [X] T047 Run error handling tests and verify they pass (GREEN)

### Final Validation

- [X] T048 Run quasiquote test suites - all 22 tests pass (14 unit + 4 contract + 4 integration)
- [X] T049 Run Stage 1 generation and count remaining quasiquote errors: **0** errors (grep -c "Cannot compile quoted" = 0)
- [X] T050 Validate generated Wasm with `wasm-tools validate dist/clysm-stage1.wasm` - **PASSED** (SC-002)
- [X] T051 Document results in `specs/001-quasiquote-local-vars/results.txt`
- [X] T052 Feature implementation complete - all user stories (US1-US4) working

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup) ─────────────────────────────────────────┐
                                                          │
Phase 2 (Foundational) ──────── BLOCKS ALL STORIES ──────┤
                                                          │
    ┌─────────────────────────────────────────────────────┘
    │
    ├── Phase 3 (US1 - P1) ──── MVP ────────────┐
    │                                            │
    ├── Phase 4 (US2 - P2) ──── Can parallel ───┤
    │                                            │
    ├── Phase 5 (US4 - P2) ──── Can parallel ───┤
    │                                            │
    └── Phase 6 (US3 - P3) ──── Can parallel ───┤
                                                 │
Phase 7 (Polish) ────────────────────────────────┘
```

### User Story Dependencies

| Story | Priority | Depends On | Can Parallel With |
|-------|----------|------------|-------------------|
| US1   | P1       | Foundational (Phase 2) | None (MVP first) |
| US2   | P2       | Foundational | US4 |
| US4   | P2       | Foundational | US2 |
| US3   | P3       | Foundational | US2, US4 |

### Within Each User Story

1. Tests (T0xx) MUST be written FIRST and FAIL
2. Implementation tasks
3. Tests MUST pass (GREEN)
4. Integration test last

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T002, T003, T004 can run in parallel (different files)
```

**Phase 2 (Foundational Tests)**:
```
T005, T006 can run in parallel (same file but different test functions)
```

**Phase 3 (US1 Tests)**:
```
T010, T011, T012 can run in parallel (unit tests, same file)
T013, T014 can run in parallel (contract tests, same file)
```

**After Foundational Phase**:
```
US2 and US4 can run in parallel (both P2, different functionality)
US3 can start after US1 if resource constrained
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T004)
2. Complete Phase 2: Foundational (T005-T009) - **CRITICAL GATE**
3. Complete Phase 3: User Story 1 (T010-T019)
4. **STOP and VALIDATE**: Run `wasm-tools validate` on compiled test functions
5. **MVP ACHIEVED**: Simple unquote (`,x`) works

### Incremental Delivery

| Increment | Stories | Value Delivered |
|-----------|---------|-----------------|
| MVP       | US1     | Basic quasiquote with locals compiles |
| +Splice   | US1+US2 | Macro templates with variable-length expansion |
| +Mixed    | +US4    | Real-world templates with mixed content |
| +Nested   | +US3    | Macro-writing macros supported |

### Recommended Execution Order (Single Developer)

1. T001 → T002-T004 (parallel) → T005-T006 (parallel) → T007-T009 (sequential)
2. T010-T014 (parallel) → T015-T019 (sequential)
3. T020-T023 (parallel) → T024-T028 (sequential)
4. T029-T031 (parallel) → T032-T035 (sequential)
5. T036-T037 (parallel) → T038-T043 (sequential)
6. T044-T052 (mostly sequential)

---

## Notes

- **[P]** tasks = different files OR different test functions, no state dependencies
- **[USn]** label maps task to user story for traceability
- Constitution VII mandates TDD: Tests RED before implementation GREEN
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All generated Wasm must pass `wasm-tools validate`
