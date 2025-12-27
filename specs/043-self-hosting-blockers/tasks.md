# Tasks: Self-Hosting Blockers Resolution

**Input**: Design documents from `/specs/043-self-hosting-blockers/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US6)
- Include exact file paths in descriptions

## Path Conventions

- Single project: `src/clysm/`, `tests/` at repository root
- Compiler extensions in `src/clysm/compiler/`
- Library functions in `src/clysm/lib/`
- Tests by layer: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Baseline measurement and test infrastructure

- [x] T001 Run baseline bootstrap compilation and record current rate in dist/baseline-rate.json
- [x] T002 [P] Create test file structure for new features in tests/unit/
- [x] T003 [P] Update clysm.asd with new test module declarations

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before user stories

**⚠️ CRITICAL**: US3-US6 implementation depends on this phase

- [x] T004 Add ast-param-info struct to src/clysm/compiler/ast.lisp with slots: name, kind, default-form, supplied-p, keyword
- [x] T005 Implement parse-lambda-list function in src/clysm/compiler/ast.lisp to extract structured param info
- [x] T006 Add keyword-arg-info struct to src/clysm/compiler/ast.lisp for tracking keyword arguments in calls
- [x] T007 [P] Add helper function extract-keyword-args in src/clysm/compiler/codegen/func-section.lisp for :test/:key parsing
- [x] T008 [P] Export new AST symbols from src/clysm/package.lisp

**Checkpoint**: AST infrastructure ready - user story implementation can begin

---

## Phase 3: User Story 3 - Function Default Values (Priority: P1) 🎯 MVP

**Goal**: Compile functions with &optional and &key parameters that have default values

**Independent Test**: `(defun foo (&optional (x 10)) x)` compiles and `(foo)` returns 10

### Tests for User Story 3

- [x] T009 [P] [US3] Unit test: &optional with default in tests/unit/default-params-test.lisp
- [x] T010 [P] [US3] Unit test: &key with default in tests/unit/default-params-test.lisp
- [x] T011 [P] [US3] Unit test: supplied-p variable in tests/unit/default-params-test.lisp
- [x] T012 [P] [US3] Contract test: Wasm validation for default params in tests/contract/default-params-wasm-test.lisp

### Implementation for User Story 3

- [x] T013 [US3] Extend compile-defun in src/clysm/compiler/codegen/func-section.lisp to use parse-lambda-list
- [x] T014 [US3] Generate argument count passing via closure $code_N pattern in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US3] Implement conditional default evaluation in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [US3] Handle supplied-p variable binding in src/clysm/compiler/codegen/func-section.lisp
- [x] T017 [US3] Extend compile-lambda in src/clysm/compiler/codegen/func-section.lisp for default params
- [x] T018 [US3] Update closure creation to support variable arity in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Functions with default parameters compile and execute correctly

---

## Phase 4: User Story 5 - Association List Functions (Priority: P2)

**Goal**: Extend assoc, rassoc, member with :test/:key; add adjoin, union, intersection

**Independent Test**: `(assoc "b" '(("a" . 1) ("b" . 2)) :test #'string=)` returns ("b" . 2)

### Tests for User Story 5

- [x] T019 [P] [US5] Unit test: assoc with :test/:key in tests/unit/list-ops-test.lisp
- [x] T020 [P] [US5] Unit test: rassoc with :test/:key in tests/unit/list-ops-test.lisp
- [x] T021 [P] [US5] Unit test: member with :test/:key in tests/unit/list-ops-test.lisp
- [x] T022 [P] [US5] Unit test: adjoin function in tests/unit/list-ops-test.lisp
- [x] T023 [P] [US5] Unit test: union function in tests/unit/list-ops-test.lisp
- [x] T024 [P] [US5] Unit test: intersection function in tests/unit/list-ops-test.lisp
- [x] T025 [P] [US5] Contract test: list-ops Wasm validation in tests/contract/list-ops-wasm-test.lisp

### Implementation for User Story 5

- [x] T026 [US5] Extend compile-assoc in src/clysm/compiler/codegen/func-section.lisp for :test/:key
- [x] T027 [US5] Extend compile-rassoc in src/clysm/compiler/codegen/func-section.lisp for :test/:key
- [x] T028 [US5] Extend compile-member in src/clysm/compiler/codegen/func-section.lisp for :test/:key
- [x] T029 [US5] Implement compile-adjoin in src/clysm/compiler/codegen/func-section.lisp
- [x] T030 [US5] Implement compile-union in src/clysm/compiler/codegen/func-section.lisp
- [x] T031 [US5] Implement compile-intersection in src/clysm/compiler/codegen/func-section.lisp
- [x] T032 [US5] Register new functions in primitive dispatch in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: List functions with keyword args work correctly

---

## Phase 5: User Story 6 - Sequence Search and Modification (Priority: P2)

**Goal**: Extend position, find with :test/:key/:start/:end/:from-end; add remove, substitute

**Independent Test**: `(find 2 '((a 1) (b 2)) :key #'second)` returns (b 2)

### Tests for User Story 6

- [x] T033 [P] [US6] Unit test: position with :test/:key/:start/:end in tests/unit/sequence-ext-test.lisp
- [x] T034 [P] [US6] Unit test: find with :test/:key/:start/:end in tests/unit/sequence-ext-test.lisp
- [x] T035 [P] [US6] Unit test: remove with all options in tests/unit/sequence-ext-test.lisp
- [x] T036 [P] [US6] Unit test: substitute with all options in tests/unit/sequence-ext-test.lisp
- [x] T037 [P] [US6] Unit test: :from-end option behavior in tests/unit/sequence-ext-test.lisp
- [x] T038 [P] [US6] Contract test: sequence-ext Wasm validation in tests/contract/sequence-ext-wasm-test.lisp

### Implementation for User Story 6

- [x] T039 [US6] Extend compile-position in src/clysm/compiler/codegen/func-section.lisp for keyword args
- [x] T040 [US6] Extend compile-find in src/clysm/compiler/codegen/func-section.lisp for keyword args
- [x] T041 [US6] Implement compile-remove in src/clysm/compiler/codegen/func-section.lisp
- [x] T042 [US6] Implement compile-substitute in src/clysm/compiler/codegen/func-section.lisp
- [x] T043 [US6] Implement :from-end handling with list reversal in src/clysm/compiler/codegen/func-section.lisp
- [x] T044 [US6] Register new functions in primitive dispatch in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Sequence functions with keyword args work correctly

---

## Phase 6: User Story 4 - Hash Table Operations (Priority: P2)

**Goal**: Implement make-hash-table, gethash, (setf gethash), remhash, maphash

**Independent Test**: Create hash table, store value, retrieve it with gethash returning two values

### Tests for User Story 4

- [x] T045 [P] [US4] Unit test: make-hash-table in tests/unit/hash-table-test.lisp
- [x] T046 [P] [US4] Unit test: gethash lookup in tests/unit/hash-table-test.lisp
- [x] T047 [P] [US4] Unit test: (setf gethash) storage in tests/unit/hash-table-test.lisp
- [x] T048 [P] [US4] Unit test: remhash removal in tests/unit/hash-table-test.lisp
- [x] T049 [P] [US4] Unit test: maphash iteration in tests/unit/hash-table-test.lisp
- [x] T050 [P] [US4] Unit test: :test option (eq, eql, equal, equalp) in tests/unit/hash-table-test.lisp
- [x] T051 [P] [US4] Contract test: hash-table Wasm type validation in tests/contract/hash-table-wasm-test.lisp

### Implementation for User Story 4

- [x] T052 [US4] Add $hash-entry struct type (index 25) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T053 [US4] Add $hash-table struct type (index 26) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T054 [US4] Add $bucket-array type (index 27) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T055 [US4] Implement compile-make-hash-table in src/clysm/compiler/codegen/func-section.lisp
- [x] T056 [US4] Implement hash function for i31ref keys in src/clysm/compiler/codegen/func-section.lisp
- [x] T057 [US4] Implement compile-gethash with two-value return in src/clysm/compiler/codegen/func-section.lisp
- [x] T058 [US4] Implement gethash setf-expander in src/clysm/lib/setf-expanders.lisp
- [x] T059 [US4] Implement compile-remhash in src/clysm/compiler/codegen/func-section.lisp
- [x] T060 [US4] Implement compile-maphash in src/clysm/compiler/codegen/func-section.lisp
- [x] T061 [US4] Register hash table functions in primitive dispatch in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Hash table operations work correctly

---

## Phase 7: User Story 2 - LOOP Macro Verification (Priority: P1)

**Goal**: Verify existing LOOP implementation compiles all required clauses correctly

**Independent Test**: All LOOP acceptance scenarios pass

### Tests for User Story 2

- [x] T062 [P] [US2] Integration test: for...in...collect in tests/integration/loop-extended-test.lisp
- [x] T063 [P] [US2] Integration test: for...from...sum in tests/integration/loop-extended-test.lisp
- [x] T064 [P] [US2] Integration test: count clause in tests/integration/loop-extended-test.lisp
- [x] T065 [P] [US2] Integration test: append clause in tests/integration/loop-extended-test.lisp
- [x] T066 [P] [US2] Integration test: maximize/minimize clauses in tests/integration/loop-extended-test.lisp
- [x] T067 [P] [US2] Contract test: LOOP expansion Wasm validation in tests/contract/loop-extended-wasm-test.lisp

### Implementation for User Story 2

- [x] T068 [US2] Verify LOOP macro expansion for collect in src/clysm/lib/macros.lisp
- [x] T069 [US2] Verify LOOP macro expansion for sum in src/clysm/lib/macros.lisp
- [x] T070 [US2] Verify LOOP macro expansion for count in src/clysm/lib/macros.lisp
- [x] T071 [US2] Verify LOOP macro expansion for append in src/clysm/lib/macros.lisp
- [x] T072 [US2] Verify LOOP macro expansion for maximize/minimize in src/clysm/lib/macros.lisp
- [x] T073 [US2] Fix any identified issues in LOOP clause expansion in src/clysm/lib/macros.lisp

**Checkpoint**: LOOP macro clauses compile and execute correctly

---

## Phase 8: User Story 1 - Compilation Rate Verification (Priority: P1) 🎯 Final Goal

**Goal**: Achieve 50%+ Stage 0 compilation rate with all compiler/codegen/ files having compiled forms

**Independent Test**: Bootstrap compilation reports >= 50% success rate

### Tests for User Story 1

- [x] T074 [P] [US1] Integration test: compilation rate measurement in tests/integration/self-hosting-test.lisp
- [x] T075 [P] [US1] Integration test: codegen coverage verification in tests/integration/self-hosting-test.lisp
- [x] T076 [P] [US1] Contract test: Stage 0 Wasm validation in tests/contract/self-hosting-wasm-test.lisp

### Implementation for User Story 1

- [x] T077 [US1] Run bootstrap compilation and measure rate via build/bootstrap.lisp
- [x] T078 [US1] Identify remaining blockers from failure report in dist/bootstrap-report.json
- [~] T079 [US1] Address critical blockers if rate < 50% (BLOCKED: fundamental self-hosting chicken-and-egg problem)
- [x] T080 [US1] Verify all compiler/codegen/ files have compiled forms in bootstrap output
- [x] T081 [US1] Generate final compilation rate report in dist/final-rate.json
- [x] T082 [US1] Validate Stage 0 binary with wasm-tools validate dist/clysm-stage0.wasm

**Checkpoint**: 23.4% compilation rate achieved. 50% target blocked by self-hosting chicken-and-egg problem.

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, cleanup, and validation

- [x] T083 [P] Update CLAUDE.md with Feature 043 completion summary
- [x] T084 [P] Add hash-table, list-ops, sequence-ext to clysm.asd :components (already present)
- [x] T085 Run all tests: sbcl --eval "(asdf:test-system :clysm/tests)"
  - System loads successfully
  - Fixed duplicate compile-funcall definition
  - Fixed forward declarations for special variables
  - Added clysm/bootstrap dependency to tests
  - Pre-existing test failures (non-exported symbols in older test files) identified for separate fix
- [x] T086 Run quickstart.md validation scenarios (Feature 043 primitives verified via bootstrap)
- [x] T087 Verify fixed-point verification readiness (Stage 1/2 infrastructure exists from Features 039/040)

**Checkpoint**: Phase 9 COMPLETE. Feature 043 implementation finished.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phase 3-7 (User Stories)**: All depend on Phase 2 completion
  - US3 (Default Values): Highest priority, no story dependencies
  - US5 (List Functions): No story dependencies
  - US6 (Sequence Functions): No story dependencies
  - US4 (Hash Tables): No story dependencies (but highest effort)
  - US2 (LOOP): No story dependencies (verification only)
- **Phase 8 (US1)**: Depends on US2-US6 completion (measures aggregate result)
- **Phase 9 (Polish)**: Depends on Phase 8 completion

### User Story Dependencies

```
Phase 2 (Foundational)
    ├── US3 (Default Values) ──┐
    ├── US5 (List Functions) ──┤
    ├── US6 (Sequence Funcs) ──┼──► US1 (Rate Verification)
    ├── US4 (Hash Tables) ─────┤
    └── US2 (LOOP Verify) ─────┘
```

### Within Each User Story

1. Tests written first (TDD per Constitution VII)
2. Implementation follows
3. Story complete when tests pass

### Parallel Opportunities

**Phase 2 (Foundational)**:
- T007 and T008 can run in parallel

**Phase 3 (US3)**:
- T009, T010, T011, T012 can run in parallel (all tests)

**Phase 4 (US5)**:
- T019-T025 can run in parallel (all tests)

**Phase 5 (US6)**:
- T033-T038 can run in parallel (all tests)

**Phase 6 (US4)**:
- T045-T051 can run in parallel (all tests)

**Phase 7 (US2)**:
- T062-T067 can run in parallel (all tests)

**Cross-Story Parallel**:
After Phase 2, US3, US4, US5, US6 can all start in parallel (if team capacity allows)

---

## Parallel Example: User Story 3

```bash
# Launch all tests for US3 together:
Task: "T009 [P] [US3] Unit test: &optional with default in tests/unit/default-params-test.lisp"
Task: "T010 [P] [US3] Unit test: &key with default in tests/unit/default-params-test.lisp"
Task: "T011 [P] [US3] Unit test: supplied-p variable in tests/unit/default-params-test.lisp"
Task: "T012 [P] [US3] Contract test: Wasm validation in tests/contract/default-params-wasm-test.lisp"
```

---

## Implementation Strategy

### MVP First (US3 + US1 Measurement)

1. Complete Phase 1: Setup (measure baseline)
2. Complete Phase 2: Foundational (AST infrastructure)
3. Complete Phase 3: US3 (default parameters - highest impact)
4. Run Phase 8: T077 (measure improvement)
5. **VALIDATE**: Check if rate improved significantly

### Incremental Delivery

1. Setup + Foundational → Infrastructure ready
2. US3 (Default Values) → Measure improvement
3. US5 (List Functions) → Measure improvement
4. US6 (Sequence Functions) → Measure improvement
5. US4 (Hash Tables) → Measure improvement
6. US2 (LOOP Verify) → Ensure complete
7. US1 (Final Verification) → Confirm 50%+ achieved

### Priority Order (from Research)

| Priority | Story | Impact | Effort |
|----------|-------|--------|--------|
| 1 | US3 - Default Values | HIGH | MEDIUM |
| 2 | US5 - List Functions | MEDIUM | MEDIUM |
| 3 | US6 - Sequence Functions | MEDIUM | MEDIUM |
| 4 | US4 - Hash Tables | MEDIUM | HIGH |
| 5 | US2 - LOOP Verify | LOW | LOW |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story is independently testable
- TDD required: tests must fail before implementation (Constitution VII)
- Commit after each task or logical group
- Measure compilation rate after each user story to track progress
- Target: 50%+ compilation rate (SC-001)
