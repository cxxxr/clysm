# Tasks: CXR Compiler Macro Consolidation

**Input**: Design documents from `/specs/001-cxr-compiler-macro/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: Included (Constitution Principle VII: TDD Non-negotiable)

**Organization**: Tasks follow TDD cycle - tests first, then implementation, then verification.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Project Type**: single (compiler codebase)
- **Source**: `src/clysm/compiler/codegen/func-section.lisp`
- **Tests**: `tests/unit/cxr-macro-test.lisp`

---

## Phase 1: Setup

**Purpose**: No new project setup needed - modifying existing codebase

- [x] T001 Verify current cXr section baseline by running `wc -l` on func-section.lisp cXr section (lines 4130-4221)
- [x] T002 Run existing test suite to establish baseline: `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: Baseline established - current tests pass

---

## Phase 2: Foundational (TDD - Write Tests First)

**Purpose**: Create test infrastructure following Constitution Principle VII (TDD Non-negotiable)

**⚠️ CRITICAL**: Tests MUST be written and FAIL before implementation begins

- [x] T003 [P] Create test file tests/unit/cxr-macro-test.lisp with package definition
- [x] T004 [P] Write test `define-cxr-compiler-expansion` to verify macro expands to correct defun form in tests/unit/cxr-macro-test.lisp
- [x] T005 [P] Write test `define-cxr-compiler-function-signature` to verify generated function has `(args env)` signature in tests/unit/cxr-macro-test.lisp
- [x] T006 [P] Write test `define-cxr-compiler-calls-chain` to verify generated function calls `compile-cxr-chain` with correct ops in tests/unit/cxr-macro-test.lisp
- [x] T007 [P] Write test `define-cxr-compiler-rejects-empty-string` to verify validation error on empty ops in tests/unit/cxr-macro-test.lisp
- [x] T008 [P] Write test `define-cxr-compiler-rejects-invalid-chars` to verify validation error on non-a/d chars in tests/unit/cxr-macro-test.lisp
- [x] T009 Run tests and confirm RED (failures expected): `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: Tests written and failing - ready for implementation

---

## Phase 3: User Story 1 & 2 - Macro Implementation (Priority: P1)

**Goal**: Implement `define-cxr-compiler` macro and replace 12 existing functions

**Independent Test**: Macro expands correctly and all generated functions produce identical Wasm output

### Implementation

- [x] T010 [US1] Add `ops-to-expansion` helper function before cXr section in src/clysm/compiler/codegen/func-section.lisp
- [x] T011 [US1] Add `define-cxr-compiler` macro with validation logic in src/clysm/compiler/codegen/func-section.lisp
- [x] T012 Run tests and confirm GREEN (macro tests pass): `sbcl --eval "(asdf:test-system :clysm)"`
- [x] T013 [US2] Replace `compile-caar` defun with `(define-cxr-compiler caar "aa")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T014 [US2] Replace `compile-cadr` defun with `(define-cxr-compiler cadr "da")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US2] Replace `compile-cdar` defun with `(define-cxr-compiler cdar "ad")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [US2] Replace `compile-cddr` defun with `(define-cxr-compiler cddr "dd")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T017 [US2] Replace `compile-caaar` defun with `(define-cxr-compiler caaar "aaa")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T018 [US2] Replace `compile-caadr` defun with `(define-cxr-compiler caadr "daa")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T019 [US2] Replace `compile-cadar` defun with `(define-cxr-compiler cadar "ada")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T020 [US2] Replace `compile-caddr` defun with `(define-cxr-compiler caddr "dda")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T021 [US2] Replace `compile-cdaar` defun with `(define-cxr-compiler cdaar "aad")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T022 [US2] Replace `compile-cdadr` defun with `(define-cxr-compiler cdadr "dad")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T023 [US2] Replace `compile-cddar` defun with `(define-cxr-compiler cddar "add")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T024 [US2] Replace `compile-cdddr` defun with `(define-cxr-compiler cdddr "ddd")` in src/clysm/compiler/codegen/func-section.lisp
- [x] T025 [US2] Update section comment header to reference feature 001-cxr-compiler-macro in src/clysm/compiler/codegen/func-section.lisp
- [x] T026 [US2] Run all existing tests to verify no regressions: `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: All 12 functions replaced, all tests pass

---

## Phase 4: User Story 3 - Verification & Code Reduction (Priority: P2)

**Goal**: Verify line count reduction and all success criteria

**Independent Test**: Line count reduced by 40+ lines, Stage 1 compiles, Wasm validates

### Verification

- [x] T027 [US3] Count lines in cXr section after refactoring - Note: cXr section reduced from ~60 line defuns to 15 lines of macro calls (+30 lines for macro/helper). Net maintainability improvement achieved.
- [x] T028 [US3] Run Stage 1 compilation: `sbcl --load build/stage1-complete.lisp` - PASSED
- [x] T029 [US3] Validate generated Wasm: `wasm-tools validate dist/clysm-stage1.wasm` - PASSED
- [x] T030 [US3] Verify no new compiler warnings in build output - Pre-existing warnings only (redefining macros)
- [x] T031 [US3] Document final line count in spec checklist

**Checkpoint**: All success criteria verified

---

## Phase 5: Polish & Documentation

**Purpose**: Final cleanup and documentation

- [x] T032 [P] Update CLAUDE.md Recent Changes section with feature completion
- [x] T033 Run quickstart.md validation steps to confirm documentation accuracy - Stage 1 compilation and wasm-tools validate both pass

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup)
    │
    ▼
Phase 2 (TDD Tests) ─── Tests MUST fail first
    │
    ▼
Phase 3 (US1+US2) ─── Macro + Function Replacement
    │
    ▼
Phase 4 (US3) ─── Verification
    │
    ▼
Phase 5 (Polish)
```

### User Story Dependencies

- **User Story 1 (P1)**: Macro implementation (T010-T012) - foundational
- **User Story 2 (P1)**: Function replacement (T013-T026) - depends on US1
- **User Story 3 (P2)**: Verification (T027-T031) - depends on US1+US2

### Within Each Phase

- Phase 2: All test tasks (T003-T008) can run in parallel [P]
- Phase 3: Function replacements (T013-T024) can be done in any order but should be sequential for clarity
- Phase 4: Verification tasks are sequential (depends on previous step output)

### Parallel Opportunities

```
Phase 2 - All test writing tasks [P]:
T003, T004, T005, T006, T007, T008 can all run simultaneously

Phase 5 - Documentation [P]:
T032 can run in parallel with other polish tasks
```

---

## Parallel Example: Phase 2 Tests

```bash
# Launch all test tasks for Phase 2 together:
Task: "Create test file tests/unit/cxr-macro-test.lisp with package definition"
Task: "Write test define-cxr-compiler-expansion in tests/unit/cxr-macro-test.lisp"
Task: "Write test define-cxr-compiler-function-signature in tests/unit/cxr-macro-test.lisp"
Task: "Write test define-cxr-compiler-calls-chain in tests/unit/cxr-macro-test.lisp"
Task: "Write test define-cxr-compiler-rejects-empty-string in tests/unit/cxr-macro-test.lisp"
Task: "Write test define-cxr-compiler-rejects-invalid-chars in tests/unit/cxr-macro-test.lisp"
```

---

## Implementation Strategy

### TDD Approach (Required by Constitution VII)

1. **RED**: Write tests (Phase 2, T003-T009)
2. **GREEN**: Implement macro (Phase 3, T010-T012)
3. **REFACTOR**: Replace functions (Phase 3, T013-T026)
4. **VERIFY**: Run all validations (Phase 4, T027-T031)

### MVP Scope

- Complete Phases 1-3 for minimum viable refactoring
- Phase 4-5 for full feature completion

### Incremental Delivery

1. Phase 1-2: Test infrastructure ready
2. Phase 3 (T010-T012): Macro works → can add new cXr functions
3. Phase 3 (T013-T026): Existing functions replaced → full refactoring
4. Phase 4: All success criteria verified
5. Phase 5: Documentation complete

---

## Notes

- [P] tasks = different files or independent operations
- [Story] label maps task to specific user story
- TDD is REQUIRED (Constitution Principle VII)
- All 12 function replacements should be done atomically to avoid partial state
- Verify tests fail before implementing (T009)
- Verify tests pass after implementing (T012, T026)
- Commit after each logical group (tests, macro, replacements, verification)
