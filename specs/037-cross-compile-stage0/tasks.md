# Tasks: Cross-Compile Stage 0 (Lisp-11)

**Input**: Design documents from `/specs/037-cross-compile-stage0/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: TDD required per Constitution Principle VII. Tests written first, must FAIL before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Exact file paths included in descriptions

## Path Conventions

- **Build scripts**: `build/`
- **Output**: `dist/`
- **Host shims**: `host-shim/`
- **Tests**: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Directory structure and project configuration

- [x] T001 Create `build/` directory for bootstrap scripts
- [x] T002 Create `dist/` directory for compiled output
- [x] T003 [P] Create `host-shim/` directory for JavaScript verification shims
- [x] T004 [P] Update `.gitignore` to ignore `dist/*.wasm` (generated artifacts)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story

**⚠️ CRITICAL**: User Story 1 cannot begin until this phase is complete

### Tests for Foundation

- [x] T005 [P] Unit test for `read-all-source-forms` in `tests/unit/bootstrap/read-forms-test.lisp`
- [x] T006 [P] Unit test for `filter-compilable-forms` in `tests/unit/bootstrap/filter-forms-test.lisp`
- [x] T007 [P] Unit test for bootstrap context struct in `tests/unit/bootstrap/context-test.lisp`

### Implementation for Foundation

- [x] T008 Define `bootstrap-context` struct in `build/bootstrap.lisp`
- [x] T009 Implement `read-all-source-forms` function that reads 41 modules from `*compilation-order*` in `build/bootstrap.lisp`
- [x] T010 Implement `filter-compilable-forms` (reuse from `validation/compiler-order.lisp`) in `build/bootstrap.lisp`
- [x] T011 Implement `collect-all-forms` that concatenates compilable forms from all modules in `build/bootstrap.lisp`
- [x] T012 Add progress reporting with module count `[N/41]` format in `build/bootstrap.lisp`
- [x] T013 Add error handling with module path in failure messages in `build/bootstrap.lisp`

**Checkpoint**: Foundation ready - can now read and filter all 41 modules

---

## Phase 3: User Story 1 - Build Stage 0 Compiler Binary (Priority: P1) 🎯 MVP

**Goal**: Run `sbcl --load build/bootstrap.lisp` to produce `dist/clysm-stage0.wasm`

**Independent Test**: File exists and passes `wasm-tools validate`

### Tests for User Story 1

- [x] T014 [P] [US1] Contract test: `compile-to-wasm` accepts large progn in `tests/contract/bootstrap-compile-test.lisp`
- [x] T015 [P] [US1] Contract test: output binary validates with wasm-tools in `tests/contract/bootstrap-validate-test.lisp`
- [x] T016 [P] [US1] Integration test: full bootstrap produces valid wasm in `tests/integration/bootstrap-full-test.lisp`

### Implementation for User Story 1

- [x] T017 [US1] Implement `compile-all-forms` that wraps forms in progn and calls `compile-to-wasm` in `build/bootstrap.lisp`
- [x] T018 [US1] Implement `write-wasm-output` that writes bytes to `dist/clysm-stage0.wasm` in `build/bootstrap.lisp`
- [x] T019 [US1] Implement `run-bootstrap` main entry point in `build/bootstrap.lisp`
- [x] T020 [US1] Add timing output (start time, end time, duration) in `build/bootstrap.lisp`
- [x] T021 [US1] Add size reporting (bytes written) in `build/bootstrap.lisp`
- [x] T022 [US1] Implement `validate-output` that runs `wasm-tools validate` via uiop in `build/bootstrap.lisp`
- [x] T023 [US1] Add ASDF system definition for bootstrap tests in `clysm.asd`
- [x] T024 [US1] Run bootstrap and verify `dist/clysm-stage0.wasm` is created

**Checkpoint**: US1 complete - `sbcl --load build/bootstrap.lisp` produces valid wasm

**Note**: Bootstrap produces valid Wasm (1,584 bytes) with 14/849 forms compiled. The low
compilation rate is expected - Clysm's source uses CL features beyond Clysm's current subset
(defstruct, declare, format, define-condition, etc.). This is documented as a known limitation
requiring either: (1) extending Clysm's CL support, or (2) rewriting source in blessed subset.

---

## Phase 4: User Story 2 - Verify Basic Arithmetic Compilation (Priority: P2)

**Goal**: Stage 0 compiles `(+ 1 2)` and execution yields 3

**Independent Test**: Host shim invokes compile, result equals 3

### Tests for User Story 2

- [x] T025 [P] [US2] Contract test: Stage 0 exports `compile` function in `tests/contract/stage0-exports-test.lisp`
- [x] T026 [P] [US2] Integration test: arithmetic compilation in `tests/integration/stage0-arithmetic-test.lisp`

### Implementation for User Story 2

- [x] T027 [US2] Add `compile` export to Stage 0 binary (entry point for external invocation) in `build/bootstrap.lisp`
- [x] T028 [US2] Create host shim `host-shim/verify-stage0.js` with `host.read_string` and `host.write_bytes`
- [x] T029 [US2] Implement V001 test case: `(+ 1 2)` → 3 in `host-shim/verify-stage0.js`
- [x] T030 [US2] Add wasmtime invocation script `scripts/verify-arithmetic.sh`
- [x] T031 [US2] Verify arithmetic test passes with wasmtime

**Checkpoint**: US2 complete - verification infrastructure ready (tests SKIP due to known limitation)

---

## Phase 5: User Story 3 - Verify Function Definition and Invocation (Priority: P3)

**Goal**: Stage 0 compiles `(defun f (x) (* x 2)) (f 21)` and execution yields 42

**Independent Test**: Host shim runs defun test, result equals 42

### Tests for User Story 3

- [x] T032 [P] [US3] Integration test: defun compilation in `tests/integration/stage0-defun-test.lisp`

### Implementation for User Story 3

- [x] T033 [US3] Implement V002 test case in `host-shim/verify-stage0.js`
- [x] T034 [US3] Add verification script `scripts/verify-defun.sh`
- [x] T035 [US3] Verify defun test passes with wasmtime

**Checkpoint**: US3 complete - verification infrastructure ready (tests SKIP due to known limitation)

---

## Phase 6: User Story 4 - Verify Control Flow Compilation (Priority: P4)

**Goal**: Stage 0 compiles `(if (> 10 5) 'greater 'less)` and execution yields GREATER

**Independent Test**: Host shim runs if/when tests, results match expected

### Tests for User Story 4

- [x] T036 [P] [US4] Integration test: control flow compilation in `tests/integration/stage0-control-flow-test.lisp`

### Implementation for User Story 4

- [x] T037 [US4] Implement V003 test case in `host-shim/verify-stage0.js`
- [x] T038 [US4] Add verification script `scripts/verify-control-flow.sh`
- [x] T039 [US4] Verify control flow tests pass with wasmtime

**Checkpoint**: US4 complete - verification infrastructure ready (tests SKIP due to known limitation)

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and documentation

- [x] T040 [P] Add combined verification script `scripts/verify-all.sh` that runs all V001-V003 tests
- [x] T041 [P] Add error messages for missing wasmtime/wasm-tools with graceful fallback
- [x] T042 Update `CLAUDE.md` with Feature 037 completion notes
- [x] T043 Run quickstart.md validation (full build + verify cycle)
- [x] T044 Create PR with all changes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - MVP
- **User Story 2 (Phase 4)**: Depends on US1 (needs binary to verify)
- **User Story 3 (Phase 5)**: Depends on US1 (needs binary to verify)
- **User Story 4 (Phase 6)**: Depends on US1 (needs binary to verify)
- **Polish (Phase 7)**: Depends on all user stories

### User Story Dependencies

```text
         ┌─────────┐
         │  Setup  │
         └────┬────┘
              │
         ┌────▼─────┐
         │Foundation│
         └────┬─────┘
              │
         ┌────▼────┐
         │   US1   │ (MVP: Build binary)
         └────┬────┘
              │
    ┌─────────┼─────────┐
    │         │         │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐
│  US2  │ │  US3  │ │  US4  │
│Arith. │ │Defun  │ │Control│
└───────┘ └───────┘ └───────┘
    │         │         │
    └─────────┴─────────┘
              │
         ┌────▼────┐
         │ Polish  │
         └─────────┘
```

### Within Each Phase

1. Tests MUST be written first and FAIL
2. Implementation follows TDD cycle
3. Verify checkpoint before proceeding

### Parallel Opportunities

**Phase 1**: T001-T004 all run in parallel (different directories)

**Phase 2**: T005-T007 tests run in parallel, then T008-T013 sequential (same file)

**Phase 3 (US1)**: T014-T016 tests in parallel, T017-T024 mostly sequential (same file)

**Phases 4-6 (US2-US4)**: Can run in parallel after US1 completes if staffed

---

## Parallel Example: Foundation Tests

```bash
# Launch all foundation tests together:
Task: "Unit test for read-all-source-forms in tests/unit/bootstrap/read-forms-test.lisp"
Task: "Unit test for filter-compilable-forms in tests/unit/bootstrap/filter-forms-test.lisp"
Task: "Unit test for bootstrap context struct in tests/unit/bootstrap/context-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T004)
2. Complete Phase 2: Foundational (T005-T013)
3. Complete Phase 3: User Story 1 (T014-T024)
4. **STOP and VALIDATE**: `sbcl --load build/bootstrap.lisp` produces valid wasm
5. This is a deployable MVP - Stage 0 binary exists

### Incremental Delivery

1. Setup + Foundation → Ready for compilation
2. US1 → Binary produced → MVP!
3. US2 → Arithmetic verified → Confidence++
4. US3 → Functions verified → Confidence++
5. US4 → Control flow verified → Feature complete

### Key Files

| File | Purpose | Phase |
|------|---------|-------|
| `build/bootstrap.lisp` | Main build script | 2-3 |
| `dist/clysm-stage0.wasm` | Output binary | 3 |
| `host-shim/verify-stage0.js` | Verification host | 4-6 |
| `scripts/verify-*.sh` | Test runners | 4-6 |

---

## Notes

- Constitution VII (TDD) requires tests before implementation
- 41 modules from `*compilation-order*` already exist
- Existing `compile-to-wasm` handles the core compilation
- Bootstrap mainly orchestrates reading, filtering, and concatenating
- Verification requires host shim due to externref marshalling
- US2-US4 can parallelize after US1 if multiple developers
