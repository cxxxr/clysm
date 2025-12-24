# Tasks: FFI Foundation

**Input**: Design documents from `/specs/012-ffi-foundation/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per Constitution Principle VII. All tests must be written and FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- FFI module: `src/clysm/ffi/`
- Test harnesses: `test-harness/`

---

## Phase 1: Setup (FFI Module Infrastructure)

**Purpose**: Create FFI module structure and package definitions

- [X] T001 Create FFI package definition in src/clysm/ffi/package.lisp with exports for DEFINE-FOREIGN-FUNCTION, EXPORT-FUNCTION, CALL-HOST, FFI-HOST-ERROR, FFI-TYPE-ERROR
- [X] T002 [P] Add clysm/ffi package to clysm.asd system definition
- [X] T003 [P] Create test-harness directory structure with test-harness/wasmtime/ and test-harness/node/

---

## Phase 2: Foundational (Core Types & Backend Extensions)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational Phase

- [X] T004 [P] Unit test for FFI types (ForeignFunctionDecl, ExportDecl, MarshalType) in tests/unit/ffi-types-test.lisp
- [X] T005 [P] Unit test for WasmImport structure in tests/unit/ffi-types-test.lisp
- [X] T006 [P] Contract test for Import section encoding in tests/contract/ffi-section-test.lisp

### Implementation for Foundational Phase

- [X] T007 Define MarshalType type specifier (:fixnum :float :string :boolean :anyref :void) in src/clysm/ffi/types.lisp
- [X] T008 [P] Define ForeignFunctionDecl structure with validation in src/clysm/ffi/types.lisp
- [X] T009 [P] Define ExportDecl structure with validation in src/clysm/ffi/types.lisp
- [X] T010 [P] Define FFIEnvironment structure for compile-time tracking in src/clysm/ffi/types.lisp
- [X] T011 Define WasmImport structure in src/clysm/ffi/types.lisp (implemented in FFI package)
- [X] T012 Implement make-import-section function in src/clysm/backend/sections.lisp (extend existing file)
- [X] T013 Implement encode-import function for Import section encoding in src/clysm/backend/sections.lisp
- [X] T014 Define FFI-HOST-ERROR and FFI-TYPE-ERROR conditions in src/clysm/ffi/types.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - ホスト関数の呼び出し (Priority: P1) 🎯 MVP

**Goal**: Enable calling host functions from Lisp code via ffi:define-foreign-function and ffi:call-host

**Independent Test**: Declare a host function and invoke it from Lisp, verify correct result returned

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T015 [P] [US1] Unit test for define-foreign-function macro expansion in tests/unit/ffi-codegen-test.lisp
- [X] T016 [P] [US1] Unit test for host name parsing (module.field separation) in tests/unit/ffi-types-test.lisp
- [X] T017 [P] [US1] Contract test for generated Wasm import section in tests/contract/ffi-section-test.lisp
- [X] T018 [P] [US1] Integration test for host function call in tests/integration/ffi-import-test.lisp

### Implementation for User Story 1

- [X] T019 [US1] Implement parse-host-name to split "module.field" format in src/clysm/ffi/types.lisp
- [X] T020 [US1] Implement register-foreign-function to add declarations to FFIEnvironment in src/clysm/ffi/types.lisp
- [X] T021 [US1] Implement ffi:define-foreign-function macro in src/clysm/ffi/macros.lisp
- [X] T022 [US1] Implement generate-import-call to emit Wasm call instruction for imported function in src/clysm/ffi/import-gen.lisp
- [X] T023 [US1] Implement emit-ffi-imports to generate Import section entries in src/clysm/ffi/import-gen.lisp
- [X] T024 [US1] Implement ffi:call-host for dynamic host function invocation in src/clysm/lib/ffi-runtime.lisp
- [X] T025 [US1] Integrate FFI imports into compiler's module generation in src/clysm/compiler/compiler.lisp (extend)
- [X] T026 [US1] Create JavaScript test harness with sample host functions in test-harness/node/ffi-host.js

**Checkpoint**: Host function calls from Lisp should now work - test with wasmtime or Node.js

---

## Phase 4: User Story 2 - Lisp関数のエクスポート (Priority: P1)

**Goal**: Enable exporting Lisp functions for host invocation via ffi:export-function

**Independent Test**: Export a Lisp function and call it from host (JavaScript/wasmtime), verify correct result

### Tests for User Story 2

- [X] T027 [P] [US2] Unit test for export-function macro expansion in tests/unit/ffi-codegen-test.lisp
- [X] T028 [P] [US2] Unit test for export wrapper generation in tests/unit/ffi-codegen-test.lisp
- [X] T029 [P] [US2] Contract test for generated Wasm export section in tests/contract/ffi-section-test.lisp
- [X] T030 [P] [US2] Integration test for exported function call from host in tests/integration/ffi-export-test.lisp

### Implementation for User Story 2

- [X] T031 [US2] Implement register-export to add declarations to FFIEnvironment in src/clysm/ffi/types.lisp
- [X] T032 [US2] Implement ffi:export-function macro in src/clysm/ffi/macros.lisp
- [X] T033 [US2] Implement generate-export-wrapper to create trampoline function for closure invocation in src/clysm/ffi/export-gen.lisp
- [X] T034 [US2] Implement emit-ffi-exports to generate Export section entries in src/clysm/ffi/export-gen.lisp
- [X] T035 [US2] Integrate FFI exports into compiler's module generation in src/clysm/compiler/compiler.lisp (extend)
- [X] T036 [US2] Add export wrapper to test-harness/node/ffi-host.js to call Lisp exported functions

**Checkpoint**: Lisp functions can now be exported and called from host

---

## Phase 5: User Story 3 - 型マーシャリング (Priority: P2)

**Goal**: Automatic type conversion between Lisp types and Wasm/host types

**Independent Test**: Each type (fixnum, float, string, boolean, anyref) can be passed to/from host correctly

### Tests for User Story 3

- [X] T037 [P] [US3] Unit test for fixnum marshalling (i31ref ↔ i32) in tests/unit/ffi-marshalling-test.lisp
- [X] T038 [P] [US3] Unit test for float marshalling ($float ↔ f64) in tests/unit/ffi-marshalling-test.lisp
- [X] T039 [P] [US3] Unit test for string marshalling (WasmGC array, no linear memory) in tests/unit/ffi-marshalling-test.lisp
- [X] T040 [P] [US3] Unit test for boolean marshalling (t/nil ↔ 1/0) in tests/unit/ffi-marshalling-test.lisp
- [X] T041 [P] [US3] Unit test for anyref passthrough in tests/unit/ffi-marshalling-test.lisp
- [X] T042 [P] [US3] Unit test for type error on unsupported types in tests/unit/ffi-marshalling-test.lisp

### Implementation for User Story 3

- [X] T043 [P] [US3] Implement marshal-fixnum-to-i32 code generation in src/clysm/ffi/marshalling.lisp
- [X] T044 [P] [US3] Implement marshal-i32-to-fixnum code generation in src/clysm/ffi/marshalling.lisp
- [X] T045 [P] [US3] Implement marshal-float-to-f64 code generation in src/clysm/ffi/marshalling.lisp
- [X] T046 [P] [US3] Implement marshal-f64-to-float code generation in src/clysm/ffi/marshalling.lisp
- [X] T047 [P] [US3] Implement marshal-string-to-externref code generation (WasmGC array) in src/clysm/ffi/marshalling.lisp
- [X] T048 [P] [US3] Implement marshal-externref-to-string code generation in src/clysm/ffi/marshalling.lisp
- [X] T049 [P] [US3] Implement marshal-boolean-to-i32 code generation in src/clysm/ffi/marshalling.lisp
- [X] T050 [P] [US3] Implement marshal-i32-to-boolean code generation in src/clysm/ffi/marshalling.lisp
- [X] T051 [US3] Implement type-check-and-signal-error for unsupported types in src/clysm/ffi/marshalling.lisp
- [X] T052 [US3] Integrate marshalling into import call and export wrapper generation in src/clysm/ffi/import-gen.lisp and src/clysm/ffi/export-gen.lisp

**Checkpoint**: All 5 basic types can be marshalled correctly between Lisp and host

---

## Phase 6: User Story 4 - Import/Export宣言の自動生成 (Priority: P2)

**Goal**: Automatic generation of Wasm import/export sections from FFI declarations

**Independent Test**: Compile code with FFI declarations and validate binary passes wasm-tools validate

### Tests for User Story 4

- [X] T053 [P] [US4] Contract test for multiple imports in tests/contract/ffi-section-test.lisp
- [X] T054 [P] [US4] Contract test for multiple exports in tests/contract/ffi-section-test.lisp
- [X] T055 [P] [US4] Contract test for wasm-tools validate on generated binary in tests/contract/ffi-section-test.lisp

### Implementation for User Story 4

- [X] T056 [US4] Implement collect-ffi-declarations to gather all FFI macros from compilation unit in src/clysm/ffi/import-gen.lisp
- [X] T057 [US4] Implement generate-type-for-ffi-signature to create Wasm function types in src/clysm/ffi/import-gen.lisp
- [X] T058 [US4] Integrate FFI type generation with Type section in src/clysm/compiler/codegen/type-section.lisp (extend)
- [X] T059 [US4] Implement end-to-end compilation with FFI in src/clysm/compiler/compiler.lisp (extend)
- [X] T060 [US4] Add wasm-tools validate call to test infrastructure in tests/helpers.lisp (extend)

**Checkpoint**: Generated Wasm binaries with FFI declarations pass validation

---

## Phase 7: User Story 5 - 複数ホスト環境での動作 (Priority: P3)

**Goal**: Verify FFI works in both wasmtime and JavaScript environments

**Independent Test**: Same Wasm binary works correctly in wasmtime and Node.js

### Tests for User Story 5

- [X] T061 [P] [US5] Integration test for wasmtime execution in tests/integration/ffi-multi-host-test.lisp
- [X] T062 [P] [US5] Integration test for Node.js execution in tests/integration/ffi-multi-host-test.lisp

### Implementation for User Story 5

- [X] T063 [US5] Create Rust test harness for wasmtime with WasmGC support in test-harness/wasmtime/ffi-host.rs
- [X] T064 [US5] Add Cargo.toml for wasmtime test harness in test-harness/wasmtime/Cargo.toml
- [X] T065 [US5] Enhance Node.js test harness with all marshal types in test-harness/node/ffi-host.js (extend)
- [X] T066 [US5] Implement test runner script that executes both environments in test-harness/run-ffi-tests.sh
- [X] T067 [US5] Document host environment requirements in specs/012-ffi-foundation/quickstart.md (update)

**Checkpoint**: FFI works identically in wasmtime and JavaScript environments

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [X] T068 [P] Add error handling for try_table/catch around FFI calls (Constitution IV compliance) in src/clysm/ffi/import-gen.lisp
- [X] T069 [P] Add host exception to ffi:ffi-host-error condition translation in src/clysm/ffi/import-gen.lisp
- [X] T070 [P] Performance optimization: inline marshalling for common types in src/clysm/ffi/marshalling.lisp
- [X] T071 [P] Add package exports for all public FFI symbols in src/clysm/ffi/package.lisp (finalize) - All symbols exported and verified
- [X] T072 Run full test suite and ensure 30+ tests pass (SC-006) - 1538 tests passing!
- [X] T073 Validate no linear memory usage in generated code (SC-007, Constitution I) - Confirmed: no memory section
- [X] T074 Run quickstart.md validation scenarios - All scenarios pass

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 priority but can proceed in parallel
  - US3 and US4 depend on US1/US2 for full functionality
  - US5 depends on US1-US4 for meaningful testing
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Can run in parallel with US1
- **User Story 3 (P2)**: Depends on US1 and US2 for import/export infrastructure
- **User Story 4 (P2)**: Depends on US1 and US2 for FFI declarations to generate
- **User Story 5 (P3)**: Depends on US1-US4 for complete FFI functionality

### Within Each User Story

- Tests MUST be written and FAIL before implementation (Constitution VII)
- Types before code generation
- Code generation before compiler integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, US1 and US2 can start in parallel
- All tests for a user story marked [P] can run in parallel
- Marshalling implementations (T043-T050) can all run in parallel

---

## Parallel Example: User Story 3 (Type Marshalling)

```bash
# Launch all tests together (TDD - must fail first):
Task: "Unit test for fixnum marshalling in tests/unit/ffi-marshalling-test.lisp"
Task: "Unit test for float marshalling in tests/unit/ffi-marshalling-test.lisp"
Task: "Unit test for string marshalling in tests/unit/ffi-marshalling-test.lisp"
Task: "Unit test for boolean marshalling in tests/unit/ffi-marshalling-test.lisp"
Task: "Unit test for anyref passthrough in tests/unit/ffi-marshalling-test.lisp"
Task: "Unit test for type error on unsupported types in tests/unit/ffi-marshalling-test.lisp"

# Then launch all marshalling implementations together:
Task: "Implement marshal-fixnum-to-i32 code generation in src/clysm/ffi/marshalling.lisp"
Task: "Implement marshal-i32-to-fixnum code generation in src/clysm/ffi/marshalling.lisp"
Task: "Implement marshal-float-to-f64 code generation in src/clysm/ffi/marshalling.lisp"
Task: "Implement marshal-f64-to-float code generation in src/clysm/ffi/marshalling.lisp"
# ... etc
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Host function calls)
4. **STOP and VALIDATE**: Test with simple host function (console.log)
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test host calls → Demo (MVP!)
3. Add User Story 2 → Test exports → Demo (Bidirectional FFI!)
4. Add User Story 3 → Test all types → Demo (Full type support!)
5. Add User Story 4 → Validate binaries → Demo (Production quality!)
6. Add User Story 5 → Multi-host tests → Demo (Portable!)

### Suggested MVP Scope

**MVP = Phase 1 + Phase 2 + Phase 3 (User Story 1)**

This delivers:
- Host function declaration (`ffi:define-foreign-function`)
- Dynamic host function calls (`ffi:call-host`)
- Basic Import section generation
- Enough to do console output and basic I/O

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (Constitution VII: TDD)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- No linear memory usage - all string/data via WasmGC types (Constitution I)
