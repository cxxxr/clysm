# Tasks: Complete FFI Foundation

**Input**: Design documents from `/specs/027-complete-ffi/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution VII (TDD mandatory)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story?] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2...)
- Paths follow single project structure from plan.md

---

## Phase 1: Setup

**Purpose**: Test infrastructure and project organization for FFI completion

- [X] T001 Create test directory structure: tests/unit/ffi/, tests/contract/, tests/integration/
- [X] T002 [P] Create host test shim directory: host-shim/
- [X] T003 [P] Create JavaScript test host in host-shim/ffi-test-host.js with mock functions (log, add, throw-error, random)
- [X] T004 Add rove test system definition for FFI tests in clysm.asd

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core AST infrastructure that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T005 Add ast-ffi-call struct to src/clysm/compiler/ast.lisp (declaration, arguments, source-location fields)
- [X] T006 [P] Add ast-call-host struct to src/clysm/compiler/ast.lisp (function-name, arguments, source-location fields)
- [X] T007 Export ast-ffi-call and ast-call-host from src/clysm/package.lisp
- [X] T008 Implement FFI function lookup in parse-expr: check *ffi-environment* for FFI-declared functions in src/clysm/compiler/ast.lisp
- [X] T009 Implement parse-call-host: recognize (ffi:call-host ...) forms in src/clysm/compiler/ast.lisp
- [X] T010 Add compile-ffi-call skeleton function in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Foundation ready - AST nodes and parsing complete, user story implementation can begin

---

## Phase 3: User Story 1 - Declare and Call Host Function (Priority: P1) 🎯 MVP

**Goal**: Compiler developer can use `ffi:define-foreign-function` to declare and call host functions

**Independent Test**: Compile `(console-log "Hello")` after defining `console-log`, verify Wasm contains import and call instruction

### Tests for User Story 1 ⚠️ TDD Required

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T011 [P] [US1] Unit test for define-foreign-function registration in tests/unit/ffi/define-foreign-function-test.lisp
- [X] T012 [P] [US1] Unit test for FFI call AST parsing in tests/unit/ffi/ffi-call-parse-test.lisp
- [X] T013 [P] [US1] Unit test for marshal-to-wasm instruction generation in tests/unit/ffi/marshal-test.lisp
- [X] T014 [P] [US1] Contract test: Wasm import section generation in tests/contract/ffi-import-wasm-test.lisp
- [X] T015 [P] [US1] Contract test: Wasm call instruction in function body in tests/contract/ffi-call-wasm-test.lisp
- [X] T016 [US1] Integration test: compile define-foreign-function + call, validate with wasm-tools in tests/integration/ffi-import-call-test.lisp

### Implementation for User Story 1

- [X] T017 [US1] Implement compile-ffi-call in src/clysm/compiler/codegen/func-section.lisp: marshal args, emit call, unmarshal result
- [X] T018 [US1] Add marshal-fixnum-to-wasm: emit i31.get_s for :fixnum params in src/clysm/ffi/marshalling.lisp
- [X] T019 [US1] Add marshal-float-to-wasm: emit struct.get $float 0 for :float params in src/clysm/ffi/marshalling.lisp
- [X] T020 [US1] Add marshal-string-to-wasm: emit extern.convert_any for :string params in src/clysm/ffi/marshalling.lisp
- [X] T021 [US1] Add unmarshal-from-wasm: emit ref.i31 (fixnum), struct.new $float (float), any.convert_extern (string) in src/clysm/ffi/marshalling.lisp
- [X] T022 [US1] Wire compile-ffi-call into compile-call dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T023 [US1] Verify import function index assignment via assign-import-indices in src/clysm/ffi/import-gen.lisp

**Checkpoint**: User Story 1 complete - FFI import calls work with marshalling

---

## Phase 4: User Story 2 - Export Lisp Function to Host (Priority: P2)

**Goal**: Compiler developer can export Lisp functions for host invocation using `ffi:export-function`

**Independent Test**: Export `my-add`, compile, verify Wasm exports wrapper function with correct signature

### Tests for User Story 2 ⚠️ TDD Required

- [X] T024 [P] [US2] Unit test for export-function registration in tests/unit/ffi/export-function-test.lisp
- [X] T025 [P] [US2] Unit test for export wrapper generation in tests/unit/ffi/export-wrapper-test.lisp
- [X] T026 [P] [US2] Contract test: Wasm export section contains wrapper in tests/contract/ffi-export-wasm-test.lisp
- [X] T027 [US2] Integration test: export function, validate wrapper with wasm-tools in tests/integration/ffi-export-test.lisp

### Implementation for User Story 2

- [X] T028 [US2] Implement generate-export-wrapper in src/clysm/ffi/export-gen.lisp: unmarshal params, call Lisp fn, marshal result
- [X] T029 [US2] Add unmarshal-from-host-types for :fixnum (i32→i31ref), :float (f64→struct), :string (externref→ref $string) in src/clysm/ffi/marshalling.lisp
- [X] T030 [US2] Add marshal-to-host-types for result conversion in src/clysm/ffi/marshalling.lisp
- [X] T031 [US2] Wire wrapper function generation into compile-to-module in src/clysm/compiler/compiler.lisp
- [X] T032 [US2] Ensure wrapper function indices are correctly assigned in src/clysm/ffi/export-gen.lisp

**Checkpoint**: User Story 2 complete - Export wrappers work with marshalling

---

## Phase 5: User Story 3 - Handle Host Call Errors (Priority: P2)

**Goal**: FFI calls handle host exceptions gracefully, translating them to Lisp conditions

**Independent Test**: Call host function that throws, verify ffi-host-error is signaled with function name and message

### Tests for User Story 3 ⚠️ TDD Required

- [X] T033 [P] [US3] Unit test for try_table/catch_all instruction generation in tests/unit/ffi/error-handling-test.lisp
- [X] T034 [P] [US3] Unit test for signal-ffi-host-error condition in tests/unit/ffi/ffi-condition-test.lisp
- [X] T035 [P] [US3] Contract test: Wasm try_table structure in FFI calls in tests/contract/ffi-error-wasm-test.lisp
- [X] T036 [US3] Integration test: host throws, Lisp handler catches in tests/integration/ffi-error-handling-test.lisp

### Implementation for User Story 3

- [X] T037 [US3] Replace placeholder generate-ffi-try-catch-wrapper with real try_table/catch_all in src/clysm/ffi/import-gen.lisp
- [X] T038 [US3] Implement generate-host-error-signal: create ffi-host-error condition with function name in src/clysm/ffi/import-gen.lisp
- [X] T039 [US3] Add function name string table for error messages in src/clysm/ffi/import-gen.lisp
- [X] T040 [US3] Integrate try_table wrapper into compile-ffi-call in src/clysm/compiler/codegen/func-section.lisp
- [X] T041 [US3] Verify condition handlers work with FFI errors in src/clysm/ffi/types.lisp

**Checkpoint**: User Story 3 complete - Host errors translate to Lisp conditions

---

## Phase 6: User Story 4 - Dynamic Host Function Invocation (Priority: P3)

**Goal**: Call host functions dynamically by name using `ffi:call-host`

**Independent Test**: Compile `(ffi:call-host "host.random")`, verify dynamic dispatch to host

### Tests for User Story 4 ⚠️ TDD Required

- [X] T042 [P] [US4] Unit test for call-host AST parsing in tests/unit/ffi/call-host-parse-test.lisp
- [X] T043 [P] [US4] Unit test for argument array creation in tests/unit/ffi/call-host-args-test.lisp
- [X] T044 [P] [US4] Contract test: Wasm imports ffi.call_host_dynamic in tests/contract/ffi-dynamic-wasm-test.lisp
- [X] T045 [US4] Integration test: dynamic call to host function in tests/integration/ffi-call-host-test.lisp

### Implementation for User Story 4

- [X] T046 [US4] Create src/clysm/runtime/ffi-dispatch.lisp for dynamic dispatch infrastructure
- [X] T047 [US4] Add $call_host_dynamic import declaration (externref name, externref args) → anyref in src/clysm/ffi/import-gen.lisp
- [X] T048 [US4] Implement compile-call-host in src/clysm/compiler/codegen/func-section.lisp: create args array, call dispatch
- [X] T049 [US4] Generate argument array from runtime types in src/clysm/runtime/ffi-dispatch.lisp
- [X] T050 [US4] Wire call-host AST node into codegen dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T051 [US4] Add ffi-host-error signaling for unknown function in src/clysm/runtime/ffi-dispatch.lisp
- [X] T052 [US4] Update host-shim/ffi-test-host.js with call_host_dynamic implementation

**Checkpoint**: User Story 4 complete - Dynamic host calls work

---

## Phase 7: User Story 5 - Callback Support from Host (Priority: P3)

**Goal**: Host code can call back into Lisp during FFI execution (re-entrant calls)

**Independent Test**: Lisp calls host, host calls exported Lisp function, all returns correctly

### Tests for User Story 5 ⚠️ TDD Required

- [X] T053 [P] [US5] Unit test for re-entrant call stack preservation in tests/unit/ffi/callback-test.lisp
- [X] T054 [P] [US5] Contract test: nested export calls work in tests/contract/ffi-callback-wasm-test.lisp
- [X] T055 [US5] Integration test: depth-3 callbacks (Lisp→Host→Lisp→Host) in tests/integration/ffi-callback-test.lisp

### Implementation for User Story 5

- [X] T056 [US5] Verify export wrappers are re-entrant safe in src/clysm/ffi/export-gen.lisp
- [X] T057 [US5] Add callback host function in host-shim/ffi-test-host.js that invokes exported Lisp function
- [X] T058 [US5] Test special variable binding preservation across callbacks in tests/integration/ffi-callback-test.lisp
- [X] T059 [US5] Verify condition handlers work in nested callback contexts in tests/integration/ffi-callback-test.lisp

**Checkpoint**: User Story 5 complete - Re-entrant callbacks work correctly

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Edge cases, documentation, and validation

- [X] T060 [P] Add ffi-type-error signaling for fixnum overflow (>31-bit) in src/clysm/ffi/marshalling.lisp
- [X] T061 [P] Add nil handling for :string (externref null) and :boolean (i32 0) in src/clysm/ffi/marshalling.lisp
- [X] T062 [P] Add ref.cast error handling for invalid externref types in src/clysm/ffi/marshalling.lisp
- [X] T063 Run all FFI tests with nix flake check
- [X] T064 Validate all generated Wasm modules with wasm-tools validate
- [X] T065 Run quickstart.md validation scenarios
- [X] T066 Update CLAUDE.md with Feature 027 completion notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase
  - US1 (P1): Can start after Foundational
  - US2 (P2): Can start after Foundational (parallel with US1)
  - US3 (P2): Can start after Foundational (parallel with US1, US2)
  - US4 (P3): Can start after Foundational (parallel with all above)
  - US5 (P3): Depends on US2 (needs export wrappers)
- **Polish (Phase 8)**: Depends on all user stories

### User Story Dependencies

| Story | Priority | Depends On | Notes |
|-------|----------|------------|-------|
| US1 | P1 | Foundational only | MVP - core import/call |
| US2 | P2 | Foundational only | Export wrappers |
| US3 | P2 | Foundational only | Error handling |
| US4 | P3 | Foundational only | Dynamic dispatch |
| US5 | P3 | US2 | Callbacks need export wrappers |

### Within Each User Story

1. Tests MUST be written and FAIL before implementation
2. Unit tests before contract tests
3. Contract tests before integration tests
4. Implementation follows test structure
5. Story complete when all tests pass

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T001, T002, T003, T004 - all parallel (different files)
```

**Phase 2 (Foundational)**:
```
T005, T006 - parallel (both in ast.lisp but different structs)
```

**User Story Tests**:
```
All tests marked [P] within a story can run in parallel
```

**Cross-Story Parallelism**:
```
After Foundational: US1, US2, US3, US4 can all start in parallel
Only US5 must wait for US2
```

---

## Parallel Example: User Story 1

```bash
# Launch all unit tests for US1 together:
Task: "T011 - Unit test for define-foreign-function registration"
Task: "T012 - Unit test for FFI call AST parsing"
Task: "T013 - Unit test for marshal-to-wasm instruction generation"

# Launch all contract tests for US1 together:
Task: "T014 - Contract test: Wasm import section generation"
Task: "T015 - Contract test: Wasm call instruction in function body"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: FFI import/call works
5. Deploy/demo MVP

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. User Story 1 → Test → MVP! (core FFI imports work)
3. User Story 2 → Test → Exports work
4. User Story 3 → Test → Error handling works
5. User Story 4 → Test → Dynamic calls work
6. User Story 5 → Test → Callbacks work
7. Polish → Production ready

### Recommended Order

Given P1/P2/P3 priorities and dependencies:
1. **MVP**: US1 (P1) - Core functionality
2. **Essential**: US2 + US3 (P2) - Bidirectional interop + error handling
3. **Advanced**: US4 + US5 (P3) - Dynamic dispatch + callbacks

---

## Summary

| Phase | Story | Priority | Tasks | Parallel Tasks |
|-------|-------|----------|-------|----------------|
| 1 | Setup | - | 4 | 3 |
| 2 | Foundational | - | 6 | 2 |
| 3 | US1 | P1 | 13 | 7 |
| 4 | US2 | P2 | 9 | 4 |
| 5 | US3 | P2 | 9 | 4 |
| 6 | US4 | P3 | 11 | 4 |
| 7 | US5 | P3 | 7 | 3 |
| 8 | Polish | - | 7 | 3 |
| **Total** | | | **66** | **30** |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Constitution VII requires TDD - tests first, implementation second
- Verify tests FAIL before implementing
- Commit after each task or logical group
- Run `wasm-tools validate` on all generated modules
