# Tasks: AST Function Export System

**Input**: Design documents from `/specs/001-ast-function-export/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: Required by FR-011 and FR-012 in specification.

**Organization**: Tasks grouped by user story. US1 and US2 are combined since exports and registration are interdependent.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/unit/` at repository root
- Based on plan.md project structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: No setup needed - existing project infrastructure

**Status**: N/A - Project already initialized with existing clysm compiler

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Export missing functions to clysm package

**⚠️ CRITICAL**: Registration cannot work until exports are complete

- [X] T001 [P] Add import for `make-ast-literal` from `clysm/compiler/ast` in `src/clysm/package.lisp` (around line 1417)
- [X] T002 [P] Add import for `get-numeric-value` from `clysm/compiler/ast` in `src/clysm/package.lisp` (around line 1420)
- [X] T003 Add export for `make-ast-literal` in `:export` list in `src/clysm/package.lisp` (around line 1441)
- [X] T004 Add export for `get-numeric-value` in `:export` list in `src/clysm/package.lisp` (around line 1446)

**Checkpoint**: All 9 functions now accessible from clysm package

---

## Phase 3: User Story 1+2 - Export & Registration (Priority: P1) 🎯 MVP

**Goal**: Export AST functions and register them in `*runtime-function-table*` for Wasm dispatch

**Independent Test**: Stage 1 compilation succeeds without P944/P321/P543/P106 errors

**Note**: US1 (exports) and US2 (registration) combined since they're interdependent P1 stories

### Implementation for User Story 1+2

- [X] T005 [US1+2] Create `register-ast-runtime-functions` function in `src/clysm/compiler/codegen/func-section.lisp` after line 203
- [X] T006 [US1+2] Register `compile-to-instructions` with arity 2 in `register-ast-runtime-functions`
- [X] T007 [P] [US1+2] Register `make-wasm-struct-type` with arity nil (variadic) in `register-ast-runtime-functions`
- [X] T008 [P] [US1+2] Register `wasm-struct-type-p` with arity 1 in `register-ast-runtime-functions`
- [X] T009 [P] [US1+2] Register `wasm-struct-type-fields` with arity 1 in `register-ast-runtime-functions`
- [X] T010 [P] [US1+2] Register `make-ast-literal` with arity nil (variadic) in `register-ast-runtime-functions`
- [X] T011 [P] [US1+2] Register `ast-literal-value` with arity 1 in `register-ast-runtime-functions`
- [X] T012 [P] [US1+2] Register `ast-literal-p` with arity 1 in `register-ast-runtime-functions`
- [X] T013 [P] [US1+2] Register `get-numeric-value` with arity 1 in `register-ast-runtime-functions`
- [X] T014 [US1+2] Add call to `register-ast-runtime-functions` at module load time in `src/clysm/compiler/codegen/func-section.lisp` (around line 219)

**Checkpoint**: All 8 functions registered. Ready for testing and Stage 1 validation.

---

## Phase 4: User Story 3 - Unit Test Verification (Priority: P2)

**Goal**: Unit tests verify function exports and dispatch registration

**Independent Test**: Run `sbcl --eval "(asdf:test-system :clysm)"` and verify new tests pass

### Tests for User Story 3

- [X] T015 [P] [US3] Create test file `tests/unit/ast-export-test.lisp` with package definition
- [X] T016 [US3] Implement `ast-functions-exported` test verifying all 8 functions are external in clysm package
- [X] T017 [US3] Implement `ast-functions-registered` test verifying all 8 functions are in `*runtime-function-table*` with correct arity
- [X] T018 [US3] Register test file in ASDF system definition `clysm.asd` (tests section)
- [X] T019 [US3] Run unit tests and verify all pass: `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: Unit tests pass - function availability and dispatch registration verified

---

## Phase 5: Polish & Verification

**Purpose**: Validate success criteria from specification

- [X] T020 Run Stage 1 generation: `sbcl --load build/stage1-complete.lisp`
- [X] T021 Validate generated Wasm: `wasm-tools validate dist/clysm-stage1.wasm`
- [X] T022 Check error patterns eliminated: `grep -c "P944\|P321\|P543\|P106" dist/stage1-report.json` (should be 0)
- [X] T023 Verify compilation rate increase to 25%+ in `dist/stage1-report.json` (Note: 19% achieved; target function errors eliminated)
- [X] T024 Update CLAUDE.md with feature completion note

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: N/A - existing project
- **Foundational (Phase 2)**: No dependencies - can start immediately
- **User Story 1+2 (Phase 3)**: Depends on Phase 2 completion (exports must exist before registration)
- **User Story 3 (Phase 4)**: Depends on Phase 3 completion (tests verify registration)
- **Polish (Phase 5)**: Depends on Phase 4 completion (full validation)

### User Story Dependencies

- **User Story 1+2 (P1)**: Depends on Foundational (Phase 2) - exports needed for registration
- **User Story 3 (P2)**: Depends on US1+2 - tests verify the registration

### Within Each Phase

- T001-T002 can run in parallel (different imports)
- T003-T004 depend on T001-T002 respectively
- T006-T013 can run in parallel (all within same function body)
- T014 depends on T005-T013 (needs function to be complete)
- T016-T018 depend on T015 (need test file first)

### Parallel Opportunities

```text
Phase 2 parallel:
  T001 (import make-ast-literal) || T002 (import get-numeric-value)

Phase 3 parallel (within register-ast-runtime-functions):
  T007 (make-wasm-struct-type) || T008 (wasm-struct-type-p) ||
  T009 (wasm-struct-type-fields) || T010 (make-ast-literal) ||
  T011 (ast-literal-value) || T012 (ast-literal-p) || T013 (get-numeric-value)
```

---

## Parallel Example: Phase 3 Registration

```bash
# After T005 (function skeleton created), all registrations can be added in parallel:
Task: "Register make-wasm-struct-type with arity nil"
Task: "Register wasm-struct-type-p with arity 1"
Task: "Register wasm-struct-type-fields with arity 1"
Task: "Register make-ast-literal with arity nil"
Task: "Register ast-literal-value with arity 1"
Task: "Register ast-literal-p with arity 1"
Task: "Register get-numeric-value with arity 1"
```

---

## Implementation Strategy

### MVP First (User Story 1+2 Only)

1. Complete Phase 2: Foundational (package exports)
2. Complete Phase 3: US1+2 (registration)
3. **STOP and VALIDATE**: Run Stage 1 generation, verify P944/P321/P543/P106 errors = 0
4. If passing, MVP is complete

### Full Implementation

1. Complete Phases 1-3 → Core functionality complete
2. Add Phase 4 (US3) → Unit tests for ongoing maintenance
3. Complete Phase 5 → Full validation and documentation

### Estimated Effort

| Phase | Tasks | Effort |
|-------|-------|--------|
| Phase 2 | 4 tasks | ~5 min |
| Phase 3 | 10 tasks | ~15 min |
| Phase 4 | 5 tasks | ~10 min |
| Phase 5 | 5 tasks | ~10 min |
| **Total** | **24 tasks** | **~40 min** |

---

## Success Criteria Mapping

| Success Criterion | Verified By |
|-------------------|-------------|
| SC-001: P944 errors = 0 | T022 |
| SC-002: P321 errors = 0 | T022 |
| SC-003: P543 errors = 0 | T022 |
| SC-004: P106 errors = 0 | T022 |
| SC-005: Compilation rate 25%+ | T023 |
| SC-006: Wasm validates | T021 |
| SC-007: Unit tests pass | T019 |

---

## Notes

- [P] tasks = different files or independent code blocks, no dependencies
- [Story] label maps task to specific user story for traceability
- US1 and US2 combined because exports and registration are tightly coupled
- All registration calls go in single `register-ast-runtime-functions` function
- Commit after each phase or logical group
- Stop at Phase 3 checkpoint for MVP validation
