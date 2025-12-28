# Tasks: Phase 13D - True Self-Hosting Achievement

**Input**: Design documents from `/specs/001-true-self-hosting/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per Constitution VII. Tests MUST be written first and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1-US6)
- Include exact file paths

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test infrastructure

- [x] T001 Create test directory structure at tests/unit/stage0/
- [x] T002 [P] Add stage0 test package definition in tests/unit/stage0/package.lisp
- [x] T003 [P] Create test runner configuration for stage0 tests in tests/unit/stage0/run-tests.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure required by ALL user stories

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Core Evaluator Infrastructure

- [x] T004 Create environment module in src/clysm/stage0/env.lisp with extend-env and lookup functions
- [x] T005 [P] Write unit test for env.lisp in tests/unit/stage0/env-test.lisp (test extend-env, lookup)
- [x] T006 Create base evaluator skeleton in src/clysm/stage0/eval.lisp with eval-form dispatch
- [x] T007 [P] Write unit test skeleton for eval.lisp in tests/unit/stage0/eval-test.lisp
- [x] T008 Create primitives module skeleton in src/clysm/stage0/primitives.lisp
- [x] T009 [P] Write unit test skeleton for primitives in tests/unit/stage0/primitives-test.lisp
- [x] T010 Update src/clysm/stage0/package.lisp to export new functions (eval-form, extend-env, lookup)

### Symbol Interning

- [x] T011 Implement intern-symbol function in src/clysm/stage0/globals.lisp (string-based equality)
- [x] T012 [P] Write unit test for symbol interning in tests/unit/stage0/globals-test.lisp

**Checkpoint**: Foundation ready - `nix flake check` passes, test infrastructure works

---

## Phase 3: User Story 1 - Compile Arithmetic Expression (Priority: P1) 🎯 MVP

**Goal**: `(+ 1 2)` compiles and executes correctly, returning 3

**Independent Test**: Run `wasmtime run dist/clysm-stage0.wasm --invoke compile_form "(+ 1 2)"` → returns valid Wasm

### Tests for User Story 1

> **TDD: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T013 [P] [US1] Test fixnum evaluation in tests/unit/stage0/eval-test.lisp - `(eval-form 42 nil)` → 42
- [x] T014 [P] [US1] Test addition primitive in tests/unit/stage0/primitives-test.lisp - `(+ 1 2)` → 3
- [x] T015 [P] [US1] Test subtraction primitive in tests/unit/stage0/primitives-test.lisp - `(- 5 3)` → 2
- [x] T016 [P] [US1] Test multiplication primitive in tests/unit/stage0/primitives-test.lisp - `(* 3 4)` → 12
- [x] T017 [P] [US1] Test division primitive in tests/unit/stage0/primitives-test.lisp - `(/ 10 2)` → 5
- [x] T018 [P] [US1] Test nested arithmetic in tests/unit/stage0/eval-test.lisp - `(+ 1 (* 2 3))` → 7
- [x] T019 [P] [US1] Contract test compile_form returns valid Wasm in tests/contract/stage0-exports-test.lisp

### Implementation for User Story 1

- [x] T020 [US1] Implement fixnum type check (i31ref detection) in src/clysm/stage0/eval.lisp
- [x] T021 [US1] Implement eval-form for fixnum literals in src/clysm/stage0/eval.lisp
- [x] T022 [P] [US1] Implement prim-add (+) in src/clysm/stage0/primitives.lisp
- [x] T023 [P] [US1] Implement prim-sub (-) in src/clysm/stage0/primitives.lisp
- [x] T024 [P] [US1] Implement prim-mul (*) in src/clysm/stage0/primitives.lisp
- [x] T025 [P] [US1] Implement prim-div (/) in src/clysm/stage0/primitives.lisp
- [x] T026 [US1] Implement arithmetic dispatch in eval-compound in src/clysm/stage0/eval.lisp
- [x] T027 [US1] Update codegen to emit i32.add/sub/mul/div_s for arithmetic in src/clysm/stage0/codegen.lisp
- [x] T028 [US1] Wire compile_form export to use eval-form in src/clysm/stage0/exports.lisp
- [x] T029 [US1] Regenerate Stage 0 and verify `wasm-tools validate dist/clysm-stage0.wasm`

**Checkpoint**: `(+ 1 2)` compiles, nested arithmetic works, all US1 tests pass

---

## Phase 4: User Story 2 - Compile Function Definition (Priority: P1)

**Goal**: `(defun f (x) x)` produces Wasm module with exported function "f"

**Independent Test**: Compile `(defun identity (x) x)` → Wasm exports callable "identity"

### Tests for User Story 2

- [x] T030 [P] [US2] Test defun parsing in tests/unit/stage0/eval-test.lisp - extracts name, params, body
- [x] T031 [P] [US2] Test simple defun evaluation in tests/unit/stage0/eval-test.lisp - `(defun f (x) x)`
- [x] T032 [P] [US2] Test defun with arithmetic body in tests/unit/stage0/eval-test.lisp - `(defun add (a b) (+ a b))`
- [x] T033 [P] [US2] Contract test defun produces exported function in tests/contract/stage0-exports-test.lisp

### Implementation for User Story 2

- [x] T034 [US2] Implement eval-defun in src/clysm/stage0/eval.lisp - extracts name, params, body
- [x] T035 [US2] Register defun in function table in src/clysm/stage0/eval.lisp
- [x] T036 [US2] Implement function call dispatch in eval-compound in src/clysm/stage0/eval.lisp
- [x] T037 [US2] Implement parameter binding for function calls in src/clysm/stage0/eval.lisp
- [x] T038 [US2] Update codegen to emit function section entries in src/clysm/stage0/codegen.lisp
- [x] T039 [US2] Update codegen to emit export section with function names in src/clysm/stage0/codegen.lisp
- [x] T040 [US2] Verify multiple defuns produce all exports in Stage 0 output

**Checkpoint**: `(defun f (x) x)` works, `(defun add (a b) (+ a b))` callable, all US2 tests pass

---

## Phase 5: User Story 3 - Achieve Non-Trivial Fixed-Point (Priority: P1)

**Goal**: Stage 1 >= 1KB, Stage 1 == Stage 2 (byte-identical)

**Independent Test**: `./scripts/verify-fixpoint.sh --json` → status "ACHIEVED", size >= 1024

### Tests for User Story 3

- [x] T041 [P] [US3] Integration test Stage 1 size >= 1KB in tests/integration/fixpoint-test.lisp
- [x] T042 [P] [US3] Integration test Stage 1 == Stage 2 in tests/integration/fixpoint-test.lisp
- [x] T043 [P] [US3] Contract test compile_all produces complete module in tests/contract/stage0-exports-test.lisp

### Implementation for User Story 3

- [x] T044 [US3] Define bootstrap compiler source forms in src/clysm/stage0/bootstrap-source.lisp
- [x] T045 [US3] Implement compile_all to process form list in src/clysm/stage0/exports.lisp
- [x] T046 [US3] Ensure compile_all emits complete Wasm module (all sections) in src/clysm/stage0/output.lisp
- [x] T047 [US3] Update build/stage1-gen.lisp to use compile_all with bootstrap source
- [x] T048 [US3] Update build/stage2-gen.lisp to use Stage 1 with same bootstrap source
- [x] T049 [US3] Verify Stage 1 >= 1024 bytes with `ls -la dist/clysm-stage1.wasm`
- [x] T050 [US3] Verify `cmp dist/clysm-stage1.wasm dist/clysm-stage2.wasm` returns identical
- [x] T051 [US3] Run `./scripts/verify-fixpoint.sh --json` and confirm status "ACHIEVED"

**Checkpoint**: Fixed-point achieved! Stage 1 >= 1KB, Stage 1 == Stage 2, MVP COMPLETE

---

## Phase 6: User Story 4 - Compile Control Structures (Priority: P2)

**Goal**: if, let, let* work correctly in compiled code

**Independent Test**: `(if (< 1 2) 10 20)` → 10, `(let ((x 5)) (+ x 3))` → 8

### Tests for User Story 4

- [x] T052 [P] [US4] Test if-true branch in tests/unit/stage0/eval-test.lisp - `(if t 1 2)` → 1
- [x] T053 [P] [US4] Test if-false branch in tests/unit/stage0/eval-test.lisp - `(if nil 1 2)` → 2
- [x] T054 [P] [US4] Test comparison-based if in tests/unit/stage0/eval-test.lisp - `(if (< 1 2) 10 20)` → 10
- [x] T055 [P] [US4] Test let binding in tests/unit/stage0/eval-test.lisp - `(let ((x 5)) x)` → 5
- [x] T056 [P] [US4] Test let with expression in tests/unit/stage0/eval-test.lisp - `(let ((x 5)) (+ x 3))` → 8
- [x] T057 [P] [US4] Test let* sequential binding in tests/unit/stage0/eval-test.lisp - `(let* ((x 1) (y (+ x 1))) y)` → 2

### Implementation for User Story 4

- [x] T058 [P] [US4] Implement prim-lt (<) in src/clysm/stage0/primitives.lisp
- [x] T059 [P] [US4] Implement prim-gt (>) in src/clysm/stage0/primitives.lisp
- [x] T060 [P] [US4] Implement prim-num-eq (=) in src/clysm/stage0/primitives.lisp
- [x] T061 [US4] Implement eval-if in src/clysm/stage0/eval.lisp with condition evaluation
- [x] T062 [US4] Implement eval-let in src/clysm/stage0/eval.lisp with environment extension
- [x] T063 [US4] Implement eval-let* in src/clysm/stage0/eval.lisp with sequential binding
- [x] T064 [US4] Update codegen for if (Wasm if/else block) in src/clysm/stage0/codegen.lisp
- [x] T065 [US4] Update codegen for local variables in src/clysm/stage0/codegen.lisp

**Checkpoint**: Control structures work, all US4 tests pass

---

## Phase 7: User Story 5 - Compile Lambda Expressions (Priority: P2)

**Goal**: Lambda expressions create callable closures

**Independent Test**: `((lambda (x) (+ x 1)) 5)` → 6

### Tests for User Story 5

- [x] T066 [P] [US5] Test lambda creation in tests/unit/stage0/eval-test.lisp - returns closure
- [x] T067 [P] [US5] Test lambda application in tests/unit/stage0/eval-test.lisp - `((lambda (x) x) 42)` → 42
- [x] T068 [P] [US5] Test lambda with arithmetic in tests/unit/stage0/eval-test.lisp - `((lambda (x) (+ x 1)) 5)` → 6
- [x] T069 [P] [US5] Test multi-arg lambda in tests/unit/stage0/eval-test.lisp - `((lambda (x y) (+ x y)) 2 3)` → 5

### Implementation for User Story 5

- [x] T070 [US5] Implement make-closure in src/clysm/stage0/eval.lisp - captures env
- [x] T071 [US5] Implement eval-lambda in src/clysm/stage0/eval.lisp - returns closure struct
- [x] T072 [US5] Implement apply-closure in src/clysm/stage0/eval.lisp - binds params, evaluates body
- [x] T073 [US5] Update codegen for closure type emission in src/clysm/stage0/codegen.lisp
- [x] T074 [US5] Update codegen for call_ref instruction in src/clysm/stage0/codegen.lisp

**Checkpoint**: Lambda works, closures capture environment, all US5 tests pass

---

## Phase 8: User Story 6 - Support Basic Types and Primitives (Priority: P2)

**Goal**: car, cdr, cons, eq work correctly; quote preserves literals

**Independent Test**: `(car (cons 1 2))` → 1, `(eq 'a 'a)` → T

### Tests for User Story 6

- [x] T075 [P] [US6] Test quote in tests/unit/stage0/eval-test.lisp - `'a` → symbol a
- [x] T076 [P] [US6] Test cons creation in tests/unit/stage0/primitives-test.lisp - `(cons 1 2)` → cons cell
- [x] T077 [P] [US6] Test car in tests/unit/stage0/primitives-test.lisp - `(car (cons 1 2))` → 1
- [x] T078 [P] [US6] Test cdr in tests/unit/stage0/primitives-test.lisp - `(cdr (cons 1 2))` → 2
- [x] T079 [P] [US6] Test eq same symbol in tests/unit/stage0/primitives-test.lisp - `(eq 'a 'a)` → T
- [x] T080 [P] [US6] Test eq different symbols in tests/unit/stage0/primitives-test.lisp - `(eq 'a 'b)` → NIL

### Implementation for User Story 6

- [x] T081 [US6] Implement eval-quote in src/clysm/stage0/eval.lisp - returns unevaluated form
- [x] T082 [US6] Implement symbol evaluation (lookup in env) in src/clysm/stage0/eval.lisp
- [x] T083 [P] [US6] Implement prim-cons in src/clysm/stage0/primitives.lisp
- [x] T084 [P] [US6] Implement prim-car in src/clysm/stage0/primitives.lisp
- [x] T085 [P] [US6] Implement prim-cdr in src/clysm/stage0/primitives.lisp
- [x] T086 [US6] Implement prim-eq in src/clysm/stage0/primitives.lisp (ref.eq for identity)
- [x] T087 [US6] Update codegen for cons creation (struct.new) in src/clysm/stage0/codegen.lisp
- [x] T088 [US6] Update codegen for car/cdr (struct.get) in src/clysm/stage0/codegen.lisp

**Checkpoint**: All primitives work, quote works, all US6 tests pass

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, cleanup, and documentation

- [x] T089 Run full test suite with `sbcl --eval "(asdf:test-system :clysm)"`
- [x] T090 Verify `nix flake check` passes
- [x] T091 [P] Update CLAUDE.md with any new patterns discovered
- [x] T092 [P] Add error handling for unsupported forms in src/clysm/stage0/eval.lisp
- [x] T093 [P] Add division by zero check in src/clysm/stage0/primitives.lisp
- [x] T094 Run quickstart.md validation steps
- [x] T095 Final fixed-point verification with `./scripts/verify-fixpoint.sh --json`

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup)
    ↓
Phase 2 (Foundational) ──→ BLOCKS all user stories
    ↓
┌───┴───┬───────┬───────┬───────┬───────┐
↓       ↓       ↓       ↓       ↓       ↓
US1     US2     US3     US4     US5     US6
(P1)    (P1)    (P1)    (P2)    (P2)    (P2)
 │       │       │
 └───────┴───────┴──→ MVP COMPLETE (after US3)
                              ↓
                        Phase 9 (Polish)
```

### User Story Dependencies

| Story | Depends On | Can Parallel With |
|-------|------------|-------------------|
| US1 (Arithmetic) | Foundational | - |
| US2 (Defun) | US1 (needs arithmetic for body) | - |
| US3 (Fixed-Point) | US1, US2 (compile_all needs defun) | - |
| US4 (Control) | Foundational | US5, US6 |
| US5 (Lambda) | US2 (function calling) | US4, US6 |
| US6 (Primitives) | Foundational | US4, US5 |

### Critical Path (MVP)

```
Setup → Foundational → US1 → US2 → US3 → Fixed-Point Achieved!
```

---

## Parallel Execution Examples

### Phase 2 Parallelization

```bash
# Run in parallel (different files):
Task: "Write unit test for env.lisp in tests/unit/stage0/env-test.lisp"
Task: "Write unit test skeleton for eval.lisp in tests/unit/stage0/eval-test.lisp"
Task: "Write unit test skeleton for primitives in tests/unit/stage0/primitives-test.lisp"
```

### US1 Tests Parallelization

```bash
# All US1 tests can run in parallel:
Task: "Test fixnum evaluation in tests/unit/stage0/eval-test.lisp"
Task: "Test addition primitive in tests/unit/stage0/primitives-test.lisp"
Task: "Test subtraction primitive in tests/unit/stage0/primitives-test.lisp"
Task: "Test multiplication primitive in tests/unit/stage0/primitives-test.lisp"
```

### US4/US5/US6 Parallelization (After US3)

```bash
# Once MVP complete, these can all run in parallel:
Task: "Implement eval-if in src/clysm/stage0/eval.lisp"  # US4
Task: "Implement make-closure in src/clysm/stage0/eval.lisp"  # US5
Task: "Implement prim-cons in src/clysm/stage0/primitives.lisp"  # US6
```

---

## Implementation Strategy

### MVP First (User Stories 1-3)

1. Complete Phase 1: Setup (3 tasks)
2. Complete Phase 2: Foundational (9 tasks)
3. Complete Phase 3: US1 - Arithmetic (17 tasks)
4. Complete Phase 4: US2 - Defun (11 tasks)
5. Complete Phase 5: US3 - Fixed-Point (11 tasks)
6. **STOP and VALIDATE**: Run `./scripts/verify-fixpoint.sh --json`
7. **MVP ACHIEVED** when Stage 1 >= 1KB and Stage 1 == Stage 2

### Incremental Delivery After MVP

1. Add US4 (Control) → Test independently
2. Add US5 (Lambda) → Test independently
3. Add US6 (Primitives) → Test independently
4. Polish phase → Full validation

---

## Summary

| Metric | Count |
|--------|-------|
| **Total Tasks** | 95 |
| **Setup Tasks** | 3 |
| **Foundational Tasks** | 9 |
| **US1 Tasks (Arithmetic)** | 17 |
| **US2 Tasks (Defun)** | 11 |
| **US3 Tasks (Fixed-Point)** | 11 |
| **US4 Tasks (Control)** | 14 |
| **US5 Tasks (Lambda)** | 9 |
| **US6 Tasks (Primitives)** | 14 |
| **Polish Tasks** | 7 |
| **Parallel Opportunities** | 38 tasks marked [P] |

**MVP Scope**: US1 + US2 + US3 = 39 tasks (after Foundational)

**Success Criteria**: `./scripts/verify-fixpoint.sh --json` shows Stage 1 >= 1KB, Stage 1 == Stage 2
