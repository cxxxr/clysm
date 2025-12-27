# Tasks: Development Workflow Establishment

**Input**: Design documents from `/specs/041-dev-workflow/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per constitution (VII. テスト駆動開発). Test tasks are included.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

Single project using existing `src/clysm/` hierarchy:
- New modules: `src/clysm/workflow/`, `src/clysm/cli/`
- Tests: `tests/unit/workflow/`, `tests/contract/`, `tests/integration/`
- Scripts: `scripts/`, `bin/`, `build/`
- Host shim: `host-shim/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create new module directories and package definitions

- [x] T001 Create workflow module directory structure at src/clysm/workflow/
- [x] T002 Create CLI module directory structure at src/clysm/cli/
- [x] T003 [P] Create test directory structure at tests/unit/workflow/
- [x] T004 [P] Define workflow package exports in src/clysm/workflow/package.lisp
- [x] T005 [P] Define CLI package exports in src/clysm/cli/package.lisp
- [x] T006 Update ASDF system definition to include workflow and cli modules in clysm.asd

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data types and platform abstraction layer that ALL user stories depend on

**CRITICAL**: No user story work can begin until this phase is complete

### Data Types

- [x] T007 [P] Define source-module struct in src/clysm/workflow/types.lisp
- [x] T008 [P] Define compilation-error struct in src/clysm/workflow/types.lisp
- [x] T009 [P] Define compilation-result struct in src/clysm/workflow/types.lisp
- [x] T010 [P] Define compilation-options struct in src/clysm/workflow/types.lisp
- [x] T011 [P] Define progress-info struct in src/clysm/workflow/types.lisp
- [x] T012 [P] Define compilation-session struct in src/clysm/workflow/types.lisp
- [x] T013 Unit test for all struct definitions in tests/unit/workflow/types-test.lisp

### Platform Abstraction Layer

- [x] T014 [P] Define FFI declarations for fs.glob, fs.mtime, fs.read, fs.exists in src/clysm/workflow/platform.lisp
- [x] T015 [P] Implement SBCL-side glob-expand using UIOP in src/clysm/workflow/platform.lisp
- [x] T016 [P] Implement file-mtime wrapper for both SBCL and Stage 1 in src/clysm/workflow/platform.lisp
- [x] T017 [P] Implement read-file-string wrapper for both SBCL and Stage 1 in src/clysm/workflow/platform.lisp
- [x] T018 Unit test for platform abstraction in tests/unit/workflow/platform-test.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - CLI Compilation (Priority: P1) MVP

**Goal**: Developer can run `./clysm compile src/**/*.lisp -o output.wasm` to produce a valid Wasm binary

**Independent Test**: Run CLI with source files and verify valid Wasm output is produced

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T019 [P] [US1] Contract test for argument parsing in tests/contract/workflow-args-test.lisp
- [x] T020 [P] [US1] Contract test for compilation output in tests/contract/workflow-compile-test.lisp
- [x] T021 [P] [US1] Integration test for basic CLI compilation in tests/integration/workflow-cli-test.lisp

### Implementation for User Story 1

- [x] T022 [P] [US1] Implement parse-args function in src/clysm/cli/args.lisp
- [x] T023 [P] [US1] Implement option validation (required -o flag, pattern validation) in src/clysm/cli/args.lisp
- [x] T024 [P] [US1] Implement help and version display in src/clysm/cli/args.lisp
- [x] T025 [US1] Implement read-source-modules function in src/clysm/workflow/compiler.lisp
- [x] T026 [US1] Implement compile-module function (wraps existing compile-to-wasm) in src/clysm/workflow/compiler.lisp
- [x] T027 [US1] Implement compile-project main function in src/clysm/workflow/compiler.lisp
- [x] T028 [US1] Implement progress-display function for console output in src/clysm/workflow/compiler.lisp
- [x] T029 [US1] Implement CLI main entry point in src/clysm/cli/main.lisp
- [x] T030 [US1] Create SBCL-based CLI entry script in build/workflow.lisp
- [x] T031 [US1] Create shell wrapper script in bin/clysm
- [x] T032 [US1] Implement Wasm binary output and validation in src/clysm/workflow/compiler.lisp
- [x] T033 [US1] Implement exit code handling (0=success, 2=failure, 3=invalid args, 4=no files) in src/clysm/cli/main.lisp

**Checkpoint**: User Story 1 (CLI Compilation) fully functional - `./clysm compile` works

---

## Phase 4: User Story 2 - Incremental Compilation (Priority: P2)

**Goal**: Only changed modules are recompiled, with dependency tracking

**Independent Test**: Modify a single file and verify only that file and its dependents are recompiled

### Tests for User Story 2

- [x] T034 [P] [US2] Unit test for dependency graph construction in tests/unit/workflow/deps-test.lisp
- [x] T035 [P] [US2] Unit test for cache load/save in tests/unit/workflow/cache-test.lisp
- [x] T036 [P] [US2] Integration test for incremental recompilation in tests/integration/workflow-incremental-test.lisp

### Implementation for User Story 2

- [x] T037 [P] [US2] Define dependency-graph struct in src/clysm/workflow/types.lisp
- [x] T038 [P] [US2] Define compilation-cache and cached-module structs in src/clysm/workflow/types.lisp
- [x] T039 [US2] Implement extract-package function in src/clysm/workflow/deps.lisp
- [x] T040 [US2] Implement build-dependency-graph function in src/clysm/workflow/deps.lisp
- [x] T041 [US2] Implement get-dependents function (transitive closure) in src/clysm/workflow/deps.lisp
- [x] T042 [US2] Implement detect-cycles function in src/clysm/workflow/deps.lisp
- [x] T043 [US2] Implement load-cache function (S-expression format) in src/clysm/workflow/cache.lisp
- [x] T044 [US2] Implement save-cache function (S-expression format) in src/clysm/workflow/cache.lisp
- [x] T045 [US2] Implement find-dirty-modules function in src/clysm/workflow/cache.lisp
- [x] T046 [US2] Implement cache pruning for deleted files in src/clysm/workflow/cache.lisp
- [x] T047 [US2] Integrate incremental compilation into compile-project in src/clysm/workflow/compiler.lisp
- [x] T048 [US2] Implement --force flag to bypass cache in src/clysm/workflow/compiler.lisp
- [x] T049 [US2] Implement --cache-dir flag for custom cache location in src/clysm/cli/args.lisp
- [x] T050 [US2] Create .clysm-cache/ directory layout with proper .gitignore

**Checkpoint**: User Story 2 (Incremental Compilation) fully functional

---

## Phase 5: User Story 3 - Error Recovery (Priority: P2)

**Goal**: Compilation continues when some files have errors, all errors are reported

**Independent Test**: Introduce errors in some files, verify others compile and all errors are reported

### Tests for User Story 3

- [x] T051 [P] [US3] Unit test for error collection in tests/unit/workflow/error-test.lisp
- [x] T052 [P] [US3] Integration test for partial compilation in tests/integration/workflow-error-recovery-test.lisp

### Implementation for User Story 3

- [x] T053 [US3] Implement collect-errors function in src/clysm/workflow/compiler.lisp
- [x] T054 [US3] Implement format-error function with file:line:column in src/clysm/workflow/compiler.lisp
- [x] T055 [US3] Implement skip-dependents logic when module fails in src/clysm/workflow/compiler.lisp
- [x] T056 [US3] Implement error summary display in src/clysm/workflow/compiler.lisp
- [x] T057 [US3] Implement exit code 1 (partial success) handling in src/clysm/cli/main.lisp
- [x] T058 [US3] Implement --continue flag (default true) in src/clysm/cli/args.lisp
- [x] T059 [US3] Implement stderr output for errors in src/clysm/workflow/compiler.lisp

**Checkpoint**: User Story 3 (Error Recovery) fully functional

---

## Phase 6: User Story 4 - REPL Compilation (Priority: P3)

**Goal**: `(compile-file path)` function works from REPL with ANSI CL-compatible interface

**Independent Test**: Load REPL and call compile-file on a source file

### Tests for User Story 4

- [x] T060 [P] [US4] Unit test for compile-file return values in tests/unit/workflow/compile-file-test.lisp
- [x] T061 [P] [US4] Unit test for condition/restart handling in tests/unit/workflow/compile-file-test.lisp
- [x] T062 [P] [US4] Integration test for REPL compilation in tests/integration/workflow-repl-test.lisp

### Implementation for User Story 4

- [x] T063 [US4] Define compile-error condition class in src/clysm/workflow/repl.lisp
- [x] T064 [US4] Define compile-warning condition class in src/clysm/workflow/repl.lisp
- [x] T065 [US4] Define file-error condition class in src/clysm/workflow/repl.lisp
- [x] T066 [US4] Implement compile-file function signature per contracts/compile-file.md in src/clysm/workflow/repl.lisp
- [x] T067 [US4] Implement output-file derivation (.lisp → .wasm) in src/clysm/workflow/repl.lisp
- [x] T068 [US4] Implement use-value, continue, abort restarts in src/clysm/workflow/repl.lisp
- [x] T069 [US4] Implement verbose output for REPL in src/clysm/workflow/repl.lisp
- [x] T070 [US4] Integrate with compilation cache for :force handling in src/clysm/workflow/repl.lisp
- [x] T071 [US4] Export compile-file from workflow package in src/clysm/workflow/package.lisp

**Checkpoint**: User Story 4 (REPL Compilation) fully functional

---

## Phase 7: User Story 5 - Self-Hosting Development (Priority: P1)

**Goal**: Stage 1 binary can compile Clysm source without SBCL

**Independent Test**: Use Stage 1 binary to compile Clysm source and verify output

**Dependency**: Requires US1-US4 complete, plus Stage 1 binary with sufficient capability

### Tests for User Story 5

- [x] T072 [P] [US5] Contract test for host shim FFI functions in tests/contract/workflow-host-test.lisp
- [x] T073 [P] [US5] Integration test for self-hosted compilation in tests/integration/workflow-selfhost-test.lisp

### Implementation for User Story 5

- [x] T074 [P] [US5] Implement fs.glob FFI function in host-shim/workflow-host.js
- [x] T075 [P] [US5] Implement fs.mtime FFI function in host-shim/workflow-host.js
- [x] T076 [P] [US5] Implement process.args FFI function in host-shim/workflow-host.js
- [x] T077 [P] [US5] Implement process.exit FFI function in host-shim/workflow-host.js
- [x] T078 [P] [US5] Implement console.log and console.error FFI functions in host-shim/workflow-host.js
- [x] T079 [US5] Create wasmtime invocation wrapper in scripts/clysm-compile.sh
- [x] T080 [US5] Update bin/clysm to fall back to Stage 1 when SBCL unavailable
- [x] T081 [US5] Add clysm_main export to Stage 1 binary entry point
- [x] T082 [US5] Verify self-hosted compilation produces valid output
- [x] T083 [US5] Document self-hosting workflow in specs/041-dev-workflow/quickstart.md

**Checkpoint**: User Story 5 (Self-Hosting) achieved - development without SBCL possible

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements and documentation

- [x] T084 [P] Add verbose mode documentation to CLI help in src/clysm/cli/args.lisp
- [x] T085 [P] Add progress bar/spinner for long compilations in src/clysm/workflow/compiler.lisp
- [x] T086 [P] Implement --json output format for CI integration in src/clysm/workflow/compiler.lisp
- [x] T087 Run quickstart.md validation end-to-end
- [x] T088 Performance benchmark: verify <5s incremental, <60s full compilation
- [x] T089 Update CLAUDE.md with Feature 041 completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational - MVP
- **US2 (Phase 4)**: Depends on Foundational, integrates with US1
- **US3 (Phase 5)**: Depends on Foundational, integrates with US1
- **US4 (Phase 6)**: Depends on Foundational, shares compilation core with US1
- **US5 (Phase 7)**: Depends on US1-US4 complete
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **US1 (P1)**: Foundation only - No dependencies on other stories
- **US2 (P2)**: Foundation + integrates with US1 compiler (cache for incremental)
- **US3 (P2)**: Foundation + integrates with US1 compiler (error handling)
- **US4 (P3)**: Foundation + shares compilation core with US1
- **US5 (P1)**: Requires all of US1-US4 complete for meaningful self-hosting

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD required by constitution)
- Types/structs before functions using them
- Core functions before integration
- Export declarations after implementation

### Parallel Opportunities

**Setup Phase**:
- T003, T004, T005 can run in parallel

**Foundational Phase**:
- T007-T012 (all type definitions) can run in parallel
- T014-T017 (all platform abstractions) can run in parallel

**User Story 1**:
- T019-T021 (tests) can run in parallel
- T022-T024 (argument parsing) can run in parallel

**User Story 2**:
- T034-T036 (tests) can run in parallel
- T037-T038 (type definitions) can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for US1 together:
Task: T019 "Contract test for argument parsing"
Task: T020 "Contract test for compilation output"
Task: T021 "Integration test for basic CLI compilation"

# Launch all argument parsing tasks together:
Task: T022 "Implement parse-args function"
Task: T023 "Implement option validation"
Task: T024 "Implement help and version display"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (CLI Compilation)
4. **STOP and VALIDATE**: Run `./clysm compile src/**/*.lisp -o output.wasm`
5. If valid Wasm produced → MVP achieved

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 → `./clysm compile` works → MVP!
3. Add US2 → Incremental compilation saves time
4. Add US3 → Better error reporting
5. Add US4 → REPL integration
6. Add US5 → Self-hosting achieved!

### Recommended Execution Order

For a single developer:
1. Phase 1 (Setup): T001-T006
2. Phase 2 (Foundation): T007-T018
3. Phase 3 (US1 MVP): T019-T033
4. **Validate MVP**
5. Phase 4 (US2): T034-T050
6. Phase 5 (US3): T051-T059
7. Phase 6 (US4): T060-T071
8. Phase 7 (US5): T072-T083
9. Phase 8 (Polish): T084-T089

---

## Notes

- Constitution requires TDD: Write tests first, ensure they fail, then implement
- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently testable after its phase completes
- Commit after each task or logical group
- Total tasks: 89
- US1 (MVP): 21 tasks (T001-T006 setup + T007-T018 foundation + T019-T033)
