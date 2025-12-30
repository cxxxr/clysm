# Tasks: Stage 1 Runtime Environment

**Input**: Design documents from `/specs/001-stage1-runtime/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/cli-interface.md, quickstart.md

**Tests**: Included per constitution requirement (VII. TDD: Shell scripts must verify exit codes and output)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

Based on plan.md structure:
- Host shim files: `host-shim/`
- Shell scripts: `scripts/`
- Integration tests: `tests/integration/stage1-runtime/`
- Wasm binaries: `dist/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test infrastructure setup

- [ ] T001 Verify Node.js 20+ is available (`node --version`)
- [ ] T002 Verify dist/clysm-stage1.wasm exists (24.5KB)
- [ ] T003 Verify wasm-tools is available for validation
- [ ] T004 [P] Create tests/integration/stage1-runtime/ directory structure
- [ ] T005 [P] Create tests/integration/stage1-runtime/test-helpers.sh with common assertions

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core runner infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T006 Create host-shim/stage1-runner.js skeleton following run-wasm.js pattern
- [ ] T007 Implement argument parsing in host-shim/stage1-runner.js (--help, --verbose, wasm-path)
- [ ] T008 Implement getAllImports() merging io-shim.js and fs-shim.js in host-shim/stage1-runner.js
- [ ] T009 Implement loadWasmModule() function in host-shim/stage1-runner.js
- [ ] T010 Define exit code constants (0, 1, 2, 3, 77) in host-shim/stage1-runner.js per data-model.md

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1+2 - Execute Stage 1 + FFI (Priority: P1) 🎯 MVP

**Goal**: Load and run dist/clysm-stage1.wasm with FFI support, execute _start successfully

**Independent Test**: Run `node host-shim/stage1-runner.js` and verify exit code 0

### Tests for User Story 1+2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T011 [P] [US1] Create tests/integration/stage1-runtime/test-load-module.sh verifying Wasm loads
- [ ] T012 [P] [US2] Create tests/integration/stage1-runtime/test-ffi-imports.sh verifying FFI resolution

### Implementation for User Story 1+2

- [ ] T013 [US1] Implement Wasm compilation and export discovery in host-shim/stage1-runner.js
- [ ] T014 [US1] Implement WebAssembly.instantiate() with merged FFI imports in host-shim/stage1-runner.js
- [ ] T015 [US1] Implement _start invocation with trap handling in host-shim/stage1-runner.js
- [ ] T016 [US1] Implement error reporting with stack traces in host-shim/stage1-runner.js
- [ ] T017 [US1] Implement exit code mapping (0=success, 2=failure, 3=missing) in host-shim/stage1-runner.js
- [ ] T018 [P] [US1] Create scripts/run-stage1.sh wrapper per contracts/cli-interface.md
- [ ] T019 [P] [US1] Add --help option to scripts/run-stage1.sh
- [ ] T020 [US1] Add --wasm and --verbose options to scripts/run-stage1.sh
- [ ] T021 [US2] Verify clysm:fs FFI functions (open, close, read-all, write-all) work via integration test
- [ ] T022 [US1] Run tests/integration/stage1-runtime/test-load-module.sh - MUST PASS
- [ ] T023 [US2] Run tests/integration/stage1-runtime/test-ffi-imports.sh - MUST PASS

**Checkpoint**: Stage 1 loads and executes _start successfully, FFI imports resolved

---

## Phase 4: User Story 3 - Console Output (Priority: P2)

**Goal**: Compiler output and debug messages appear on stdout/stderr

**Independent Test**: Run with --verbose and verify clysm:io functions produce output

### Tests for User Story 3

- [ ] T024 [P] [US3] Create tests/integration/stage1-runtime/test-console-output.sh verifying stdout/stderr

### Implementation for User Story 3

- [ ] T025 [US3] Add verbose logging for FFI function calls in host-shim/stage1-runner.js
- [ ] T026 [US3] Verify write-char outputs to stdout (fd=1) via manual test
- [ ] T027 [US3] Verify write-string outputs to stdout (fd=1) via manual test
- [ ] T028 [US3] Verify write operations to stderr (fd=2) work correctly
- [ ] T029 [US3] Run tests/integration/stage1-runtime/test-console-output.sh - MUST PASS

**Checkpoint**: Console output from Stage 1 appears correctly on stdout/stderr

---

## Phase 5: User Story 4 - Compile Form Verification (Priority: P2)

**Goal**: Test compile_form export if available, handle missing export gracefully

**Independent Test**: Run with `--expr "(+ 1 2)"` and verify exit code (0 or 77)

### Tests for User Story 4

- [ ] T030 [P] [US4] Create tests/integration/stage1-runtime/test-compile-form.sh handling both exported and not-exported cases

### Implementation for User Story 4

- [ ] T031 [US4] Add --expr argument parsing in host-shim/stage1-runner.js per contracts/cli-interface.md
- [ ] T032 [US4] Add --output argument parsing for compiled wasm output in host-shim/stage1-runner.js
- [ ] T033 [US4] Implement compile_form export detection in host-shim/stage1-runner.js
- [ ] T034 [US4] Implement exit code 77 with clear message when compile_form not exported
- [ ] T035 [US4] Implement compile_form invocation with string input (if exported) in host-shim/stage1-runner.js
- [ ] T036 [US4] Implement output validation with wasm-tools if compile_form succeeds
- [ ] T037 [US4] Run tests/integration/stage1-runtime/test-compile-form.sh - MUST PASS

**Checkpoint**: compile_form verification works (success or graceful skip with exit 77)

---

## Phase 6: User Story 5 - Stage 2 Generation (Priority: P3)

**Goal**: Use Stage 1 to compile Clysm source into Stage 2, produce report

**Independent Test**: Run `./scripts/generate-stage2.sh` and check for output or report

### Tests for User Story 5

- [ ] T038 [P] [US5] Create tests/integration/stage1-runtime/test-generate-stage2.sh verifying script behavior

### Implementation for User Story 5

- [ ] T039 [US5] Create scripts/generate-stage2.sh skeleton per contracts/cli-interface.md
- [ ] T040 [US5] Add argument parsing (--stage1, --output, --report, --source-dir, --verbose, --json, --help)
- [ ] T041 [US5] Implement Stage 1 loading via stage1-runner.js invocation
- [ ] T042 [US5] Implement compile_all export detection and exit 77 if missing
- [ ] T043 [US5] Implement source module iteration from src/clysm/ directory
- [ ] T044 [US5] Implement progress reporting (human-readable and --json modes)
- [ ] T045 [US5] Implement dist/stage2-report.json generation per data-model.md schema
- [ ] T046 [US5] Implement dist/clysm-stage2.wasm output (or placeholder if compile_all unavailable)
- [ ] T047 [US5] Run tests/integration/stage1-runtime/test-generate-stage2.sh - MUST PASS

**Checkpoint**: Stage 2 generation produces output or actionable report

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Documentation and final validation

- [ ] T048 [P] Update host-shim/README.md with Stage 1 runner documentation
- [ ] T049 [P] Add stage1-runner.js usage examples to quickstart.md
- [ ] T050 Run full integration test suite: all tests/integration/stage1-runtime/*.sh
- [ ] T051 Verify scripts/run-stage1.sh executes successfully
- [ ] T052 Verify scripts/generate-stage2.sh produces expected output/report
- [ ] T053 Validate all exit codes match data-model.md specification

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **US1+US2 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **US3 (P2)**: Can start after US1+US2 complete (needs working runner)
- **US4 (P2)**: Can start after US1+US2 complete (needs working runner)
- **US5 (P3)**: Can start after US1+US2 complete (needs working runner, may need US4 for compile_form)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per constitution)
- Runner skeleton before specific features
- Core implementation before error handling
- Story complete before moving to next priority

### Parallel Opportunities

- T004, T005 can run in parallel (Phase 1)
- T011, T012 can run in parallel (US1+2 tests)
- T018, T019 can run in parallel (shell script creation)
- T024 can run in parallel with US4 test creation
- T030, T038 can run in parallel (US4, US5 tests)
- T048, T049 can run in parallel (documentation)

---

## Parallel Example: User Story 1+2

```bash
# Launch tests in parallel first (TDD):
Task: "Create tests/integration/stage1-runtime/test-load-module.sh"
Task: "Create tests/integration/stage1-runtime/test-ffi-imports.sh"

# After tests written, implementation proceeds sequentially:
# T013 → T014 → T015 → T016 → T017 (runner implementation)

# Shell scripts can be created in parallel:
Task: "Create scripts/run-stage1.sh"
Task: "Add --help option to scripts/run-stage1.sh"
```

---

## Implementation Strategy

### MVP First (User Story 1+2 Only)

1. Complete Phase 1: Setup (5 tasks)
2. Complete Phase 2: Foundational (5 tasks)
3. Complete Phase 3: User Story 1+2 (13 tasks)
4. **STOP and VALIDATE**: Run `./scripts/run-stage1.sh` - should exit 0 or show known limitation
5. This is a usable MVP!

### Incremental Delivery

1. Setup + Foundational → Foundation ready (10 tasks)
2. Add US1+US2 → Test independently → MVP ready (23 tasks total)
3. Add US3 → Test independently → Better observability (29 tasks total)
4. Add US4 → Test independently → compile_form verification (37 tasks total)
5. Add US5 → Test independently → Stage 2 generation (47 tasks total)
6. Polish → Documentation complete (53 tasks total)

### Task Summary

| Phase | Tasks | Cumulative |
|-------|-------|------------|
| Setup | 5 | 5 |
| Foundational | 5 | 10 |
| US1+US2 (P1) | 13 | 23 |
| US3 (P2) | 6 | 29 |
| US4 (P2) | 8 | 37 |
| US5 (P3) | 10 | 47 |
| Polish | 6 | 53 |

---

## Notes

- [P] tasks = different files, no dependencies, can run in parallel
- [Story] label (US1-US5) maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD: Write tests first, ensure they fail, then implement (constitution VII requirement)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Current Stage 1 likely exits with code 77 for compile_form (14.1% compilation rate)
