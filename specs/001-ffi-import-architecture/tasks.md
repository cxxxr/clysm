# Tasks: FFI Import Architecture

**Input**: Design documents from `/specs/001-ffi-import-architecture/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution (VII. TDD Non-negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Single project: `src/clysm/`, `tests/` at repository root
- Host shim: `host-shim/`

---

## Phase 1: Setup

**Purpose**: Project initialization and ASDF system updates

- [x] T001 Add ffi-usage.lisp to clysm.asd under compiler/analyzer module
- [x] T002 Add ffi-analysis package definition in src/clysm/compiler/analyzer/ffi-usage.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures and utilities that MUST be complete before ANY user story

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T003 Define `ffi-analysis` struct in src/clysm/compiler/analyzer/ffi-usage.lisp (used-ffis, has-dynamic-call-p, static-funcalls, dynamic-sites)
- [x] T004 [P] Define `ffi-mode` type (:minimal, :full, :auto) in src/clysm/compiler/compiler.lisp
- [x] T005 [P] Define `dynamic-call-in-minimal-mode` condition in src/clysm/conditions/types.lisp
- [x] T006 Implement `get-ffi-function-names` helper to list all registered FFI names in src/clysm/compiler/analyzer/ffi-usage.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Pure Computation Runs Standalone (Priority: P1) 🎯 MVP

**Goal**: `(+ 1 2)` compiles to Wasm that executes in wasmtime without host imports

**Independent Test**: Compile `(+ 1 2)` and run with `wasmtime` directly. Returns 3 without "unknown import" errors.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T007 [P] [US1] Unit test: `analyze-ffi-usage` returns empty used-ffis for pure arithmetic in tests/unit/ffi-usage-test.lisp
- [x] T008 [P] [US1] Contract test: `(+ 1 2)` produces Wasm with no import section in tests/contract/import-section-test.lisp
- [x] T009 [P] [US1] Integration test: wasmtime executes `(+ 1 2)` standalone returning 3 in tests/integration/wasmtime-test.lisp

### Implementation for User Story 1

- [x] T010 [US1] Implement `analyze-ffi-usage` basic walker (atoms, progn, let, if) in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T011 [US1] Implement FFI detection: check if function name is in `*ffi-environment*` in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T012 [US1] Modify `compile-to-wasm` to call `analyze-ffi-usage` and store result in src/clysm/compiler/compiler.lisp
- [x] T013 [US1] Modify `emit-module` to accept `:used-ffis` parameter in src/clysm/compiler/compiler.lisp
- [x] T014 [US1] Add `emit-selected-ffi-imports` function in src/clysm/ffi/import-gen.lisp (filters by used-ffis list)
- [x] T015 [US1] Modify `emit-import-section-if-needed` to use `emit-selected-ffi-imports` with used-ffis in src/clysm/compiler/compiler.lisp
- [x] T016 [US1] Verify tests pass: pure arithmetic compiles with zero imports

**Checkpoint**: User Story 1 complete. `(+ 1 2)` runs standalone in wasmtime.

---

## Phase 4: User Story 2 - Static FFI Calls Import Only Used Functions (Priority: P2)

**Goal**: `(sin 1.0)` imports only `clysm:math/sin`, not all FFI functions

**Independent Test**: Compile `(sin 1.0)` and verify import section contains only math-related import.

### Tests for User Story 2

- [x] T017 [P] [US2] Unit test: `analyze-ffi-usage` detects `sin` call returns used-ffis containing SIN in tests/unit/ffi-usage-test.lisp
- [x] T018 [P] [US2] Unit test: `(funcall 'write-char #\A)` detected as static call in tests/unit/ffi-usage-test.lisp
- [x] T019 [P] [US2] Contract test: `(sin 1.0)` imports only `clysm:math/sin` in tests/contract/import-section-test.lisp
- [x] T020 [P] [US2] Contract test: `(write-char #\A)` imports only `clysm:io/write-char` in tests/contract/import-section-test.lisp

### Implementation for User Story 2

- [x] T021 [US2] Extend `analyze-ffi-usage` to detect direct FFI function calls in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T022 [US2] Implement `detect-static-funcall-p`: recognize `(funcall 'sym ...)` pattern in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T023 [US2] Extend walker to handle `funcall` with quoted symbol as static in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T024 [US2] Extend walker to handle `#'func` form as static in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T025 [US2] Accumulate FFI names in `used-ffis` slot of ffi-analysis in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T026 [US2] Verify tests pass: selective imports for static FFI calls

**Checkpoint**: User Story 2 complete. Static FFI calls produce minimal import sections.

---

## Phase 5: User Story 3 - Dynamic Calls Work Through Host Runtime (Priority: P3)

**Goal**: `(funcall (intern "FOO") x)` detected as dynamic, works via host runtime

**Independent Test**: Compile code with `(funcall (intern "IDENTITY") 42)` and run with host shim. Returns 42.

### Tests for User Story 3

- [x] T027 [P] [US3] Unit test: `(funcall (intern "FOO") x)` detected as dynamic call in tests/unit/ffi-usage-test.lisp
- [x] T028 [P] [US3] Unit test: `(apply fn args)` detected as dynamic call in tests/unit/ffi-usage-test.lisp
- [x] T029 [P] [US3] Contract test: dynamic call includes `$dynamic-call` import in tests/contract/ffi-import-wasm-test.lisp
- [x] T030 [P] [US3] Integration test: dynamic call resolves function at runtime in tests/integration/dynamic-call-test.lisp

### Implementation for User Story 3

- [x] T031 [US3] Implement `detect-dynamic-call-p`: return true for non-quoted funcall/apply in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T032 [US3] Extend walker to set `has-dynamic-call-p` when dynamic pattern detected in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T033 [US3] Record dynamic-sites for error messages in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T034 [US3] Add `$dynamic-call` type definition to Type section (if not exists) in src/clysm/compiler/compiler.lisp
- [x] T035 [US3] Implement `emit-dynamic-call-import` in src/clysm/ffi/import-gen.lisp
- [x] T036 [US3] Modify `emit-module` to add `$dynamic-call` import when `has-dynamic-call-p` in src/clysm/compiler/compiler.lisp
- [x] T037 [US3] Create host-shim/runtime.js with function registry and dynamicCall implementation
- [x] T038 [US3] Register all FFI functions in runtime.js registry
- [x] T039 [US3] Implement dynamic call error handling (throw with function name) in host-shim/runtime.js
- [x] T040 [US3] Verify tests pass: dynamic calls work through host runtime

**Checkpoint**: User Story 3 complete. Dynamic calls resolve at runtime.

---

## Phase 6: User Story 4 - Compilation Modes Control FFI Behavior (Priority: P4)

**Goal**: `:ffi-mode` parameter controls import section generation

**Independent Test**: Compile same code with different modes and verify import sections differ.

### Tests for User Story 4

- [x] T041 [P] [US4] Unit test: `:minimal` mode errors on dynamic call in tests/unit/ffi-usage-test.lisp
- [x] T042 [P] [US4] Unit test: `:full` mode always includes `$dynamic-call` in tests/unit/ffi-usage-test.lisp
- [x] T043 [P] [US4] Unit test: `:auto` mode includes `$dynamic-call` only when needed in tests/unit/ffi-usage-test.lisp
- [x] T044 [P] [US4] Contract test: mode affects import section content in tests/contract/import-section-test.lisp

### Implementation for User Story 4

- [x] T045 [US4] Add `:ffi-mode` keyword parameter to `compile-to-wasm` (default `:auto`) in src/clysm/compiler/compiler.lisp
- [x] T046 [US4] Implement mode validation in `compile-to-wasm` in src/clysm/compiler/compiler.lisp
- [x] T047 [US4] Implement `:minimal` mode: signal error when dynamic call detected in src/clysm/compiler/compiler.lisp
- [x] T048 [US4] Implement `:full` mode: always include `$dynamic-call` import in src/clysm/compiler/compiler.lisp
- [x] T049 [US4] Implement `:auto` mode: include `$dynamic-call` only when `has-dynamic-call-p` in src/clysm/compiler/compiler.lisp
- [x] T050 [US4] Verify tests pass: all three modes work correctly

**Checkpoint**: User Story 4 complete. All compilation modes implemented.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Edge cases, documentation, and code quality

- [x] T051 [P] Extend walker to handle `labels`/`flet` local function bodies in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T052 [P] Extend walker to handle `lambda` bodies in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T053 [P] Add regression tests for io-usage.lisp compatibility in tests/unit/io-usage-test.lisp
- [x] T054 [P] Update existing import-section-test.lisp tests for new behavior in tests/contract/import-section-test.lisp
- [x] T055 Performance test: verify <15% compilation overhead in tests/integration/stage1-timing-test.lisp
- [x] T056 Update quickstart.md with usage examples
- [x] T057 Run `wasm-tools validate` on all generated test outputs
- [x] T058 Run `nix flake check` to verify CI passes
- [x] T059 [P] Add HyperSpec links for [funcall](resources/HyperSpec/Body/f_funcal.htm) and [apply](resources/HyperSpec/Body/f_apply.htm) in spec.md FR-004/FR-005 and code comments in src/clysm/compiler/analyzer/ffi-usage.lisp
- [x] T060 [P] Integration test: verify module compiled with old behavior (all FFI imports) still executes correctly with updated host runtime in tests/integration/backward-compat-test.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1-US4 can proceed in priority order (P1 → P2 → P3 → P4)
  - Some parallelization possible between stories (different files)
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P2)**: Builds on US1's basic analyzer - Should complete US1 first
- **User Story 3 (P3)**: Builds on US2's static detection - Should complete US2 first
- **User Story 4 (P4)**: Uses all previous infrastructure - Should complete US3 first

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution)
- Analyzer changes before compiler integration
- Compiler changes before import-gen changes
- Host shim changes after compiler

### Parallel Opportunities

- All tests within a user story marked [P] can run in parallel
- Phase 2 foundational tasks T004, T005 can run in parallel
- Phase 7 polish tasks T051-T054 can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all tests for User Story 1 together:
Task: "Unit test: analyze-ffi-usage returns empty for arithmetic in tests/unit/ffi-usage-test.lisp"
Task: "Contract test: (+ 1 2) produces Wasm with no imports in tests/contract/import-section-test.lisp"
Task: "Integration test: wasmtime executes standalone in tests/integration/wasmtime-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: `(+ 1 2)` runs in wasmtime standalone
5. This alone solves the critical blocking issue

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Pure computation works → **MVP!**
3. Add User Story 2 → Static FFI works → Better modules
4. Add User Story 3 → Dynamic calls work → Full CL compatibility
5. Add User Story 4 → Compilation modes → Developer control
6. Each story adds value without breaking previous stories

### Sequential Implementation (Recommended)

This feature has linear dependencies - each story builds on previous:

1. US1: Basic analyzer + emit-module fix
2. US2: Extend analyzer for FFI detection
3. US3: Add dynamic detection + host support
4. US4: Add mode parameter

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Tests MUST fail before implementation (TDD per Constitution VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Constitution requires: TDD, Nix check pass, HyperSpec references
