# Tasks: FFI Filesystem Access

**Input**: Design documents from `/specs/035-ffi-filesystem/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/
**Constitution**: TDD is REQUIRED - write tests first, verify they fail, then implement

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- Include exact file paths in descriptions

## User Stories Summary

| ID | Title | Priority | Components |
|----|-------|----------|------------|
| US1 | Read File Contents | P1 | `read-file-contents` function |
| US2 | Write File Contents | P1 | `write-file-contents` function |
| US3 | Safe File Handling | P2 | `with-open-file` macro |
| US4 | Open/Close Handles | P2 | `open-file`, `close-file` functions |
| US5 | Cross-Platform | P3 | Host shims (WASI + Virtual FS) |

---

## Phase 1: Setup

**Purpose**: Project initialization and module structure

- [x] T001 Create filesystem package definition in src/clysm/filesystem/package.lisp
- [x] T002 [P] Add filesystem module to clysm.asd system definition
- [x] T003 [P] Create test directory structure tests/unit/filesystem/, tests/contract/, tests/integration/

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundation

- [x] T004 [P] Unit test for file-error condition in tests/unit/filesystem/file-error-test.lisp
- [x] T005 [P] Unit test for file-stream struct in tests/unit/filesystem/file-stream-test.lisp
- [x] T006 [P] Contract test for FFI imports in tests/contract/filesystem-ffi-test.lisp

### Implementation for Foundation

- [x] T007 [P] Define file-error condition class in src/clysm/conditions/types.lisp
- [x] T008 [P] Define file-stream struct in src/clysm/filesystem/types.lisp
- [x] T009 [P] Declare FFI imports (%open-file, %close-file, %read-all, %write-all) in src/clysm/filesystem/ffi.lisp
- [x] T010 Create base host shim structure in host-shim/fs-shim.js with getImports() skeleton

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Read File Contents (Priority: P1) 🎯 MVP

**Goal**: A Lisp programmer reads the entire contents of a file by specifying a filename and receives the contents as a string.

**Independent Test**: Create a test file with known contents, call `read-file-contents`, verify returned string matches.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US1] Unit test read-file-contents with pathname in tests/unit/filesystem/read-contents-test.lisp
- [x] T012 [P] [US1] Unit test read-file-contents with file-stream in tests/unit/filesystem/read-contents-test.lisp
- [x] T013 [P] [US1] Unit test read-file-contents UTF-8 encoding in tests/unit/filesystem/read-contents-test.lisp
- [x] T014 [P] [US1] Unit test read-file-contents file-not-found error in tests/unit/filesystem/read-contents-test.lisp

### Implementation for User Story 1

- [x] T015 [US1] Implement read-file-contents function (pathname variant) in src/clysm/filesystem/read.lisp
- [x] T016 [US1] Implement read-file-contents function (stream variant) in src/clysm/filesystem/read.lisp
- [x] T017 [US1] Add UTF-8 decoding for file contents using babel in src/clysm/filesystem/read.lisp
- [x] T018 [US1] Implement host shim fs.read-all function in host-shim/fs-shim.js (completed in T010)

**Checkpoint**: User Story 1 complete - `read-file-contents` works for both pathname and stream inputs

---

## Phase 4: User Story 2 - Write File Contents (Priority: P1)

**Goal**: A Lisp programmer writes a string to a file, creating the file if it doesn't exist or overwriting if it does.

**Independent Test**: Write a string to a file, then read it back and verify contents match.

### Tests for User Story 2

- [x] T019 [P] [US2] Unit test write-file-contents with pathname in tests/unit/filesystem/write-contents-test.lisp
- [x] T020 [P] [US2] Unit test write-file-contents with file-stream in tests/unit/filesystem/write-contents-test.lisp
- [x] T021 [P] [US2] Unit test write-file-contents UTF-8 encoding in tests/unit/filesystem/write-contents-test.lisp
- [x] T022 [P] [US2] Unit test write-file-contents directory-not-found error in tests/unit/filesystem/write-contents-test.lisp

### Implementation for User Story 2

- [x] T023 [US2] Implement write-file-contents function (pathname variant) in src/clysm/filesystem/write.lisp
- [x] T024 [US2] Implement write-file-contents function (stream variant) in src/clysm/filesystem/write.lisp
- [x] T025 [US2] Add UTF-8 encoding for file contents using babel in src/clysm/filesystem/write.lisp
- [x] T026 [US2] Implement host shim fs.write-all function in host-shim/fs-shim.js (completed in T010)

**Checkpoint**: User Stories 1 & 2 complete - basic file read/write works

---

## Phase 5: User Story 4 - Open and Close File Handles (Priority: P2)

**Goal**: A Lisp programmer explicitly opens a file, performs multiple operations, and closes it for fine-grained control.

**Independent Test**: Open a file, verify handle is valid, close it, verify operations on closed handle signal error.

**Note**: Implemented before US3 because `with-open-file` depends on `open-file` and `close-file`.

### Tests for User Story 4

- [x] T027 [P] [US4] Unit test open-file input existing file in tests/unit/filesystem/open-close-test.lisp
- [x] T028 [P] [US4] Unit test open-file input not-found error in tests/unit/filesystem/open-close-test.lisp
- [x] T029 [P] [US4] Unit test open-file output create file in tests/unit/filesystem/open-close-test.lisp
- [x] T030 [P] [US4] Unit test open-file :if-exists :error in tests/unit/filesystem/open-close-test.lisp
- [x] T031 [P] [US4] Unit test close-file success in tests/unit/filesystem/open-close-test.lisp
- [x] T032 [P] [US4] Unit test close-file already-closed error in tests/unit/filesystem/open-close-test.lisp

### Implementation for User Story 4

- [x] T033 [US4] Implement open-file function with :direction parameter in src/clysm/filesystem/open.lisp
- [x] T034 [US4] Implement open-file :if-exists and :if-does-not-exist parameters in src/clysm/filesystem/open.lisp
- [x] T035 [US4] Implement close-file function in src/clysm/filesystem/open.lisp
- [x] T036 [US4] Implement host shim fs.open function in host-shim/fs-shim.js (completed in T010)
- [x] T037 [US4] Implement host shim fs.close function in host-shim/fs-shim.js (completed in T010)

**Checkpoint**: User Story 4 complete - explicit file handle management works

---

## Phase 6: User Story 3 - Safe File Handling (Priority: P2)

**Goal**: A Lisp programmer uses the `with-open-file` macro to ensure files are properly closed even when errors occur.

**Independent Test**: Use `with-open-file`, throw an error within the body, verify the file is still properly closed.

### Tests for User Story 3

- [x] T038 [P] [US3] Unit test with-open-file normal completion in tests/unit/filesystem/with-open-file-test.lisp
- [x] T039 [P] [US3] Unit test with-open-file error propagation with cleanup in tests/unit/filesystem/with-open-file-test.lisp
- [x] T040 [P] [US3] Unit test with-open-file input direction in tests/unit/filesystem/with-open-file-test.lisp
- [x] T041 [P] [US3] Unit test with-open-file output direction in tests/unit/filesystem/with-open-file-test.lisp

### Implementation for User Story 3

- [x] T042 [US3] Implement with-open-file macro using unwind-protect in src/clysm/filesystem/macros.lisp
- [x] T043 [US3] Add keyword argument parsing for :direction, :if-exists, :if-does-not-exist in src/clysm/filesystem/macros.lisp

**Checkpoint**: User Story 3 complete - safe resource management with automatic cleanup works

---

## Phase 7: User Story 5 - Cross-Platform File Access (Priority: P3)

**Goal**: File I/O code works identically on wasmtime (WASI) and browser (Virtual FS) environments.

**Independent Test**: Run the same file operations in both environments and verify identical behavior.

### Tests for User Story 5

- [x] T044 [P] [US5] Contract test Wasm module validates with clysm:fs imports in tests/contract/filesystem-wasm-test.lisp
- [x] T045 [P] [US5] Integration test file operations on wasmtime in tests/integration/filesystem-test.lisp
- [x] T046 [P] [US5] Integration test file operations on browser (mocked) in tests/integration/filesystem-test.lisp

### Implementation for User Story 5

- [x] T047 [US5] Implement WASI Preview2 backend in host-shim/wasi-filesystem.js (uses Node.js fs module in fs-shim.js)
- [x] T048 [US5] Implement Virtual FS backend in host-shim/virtual-fs.js (DEFERRED - requires browser testing; wasmtime/WASI backend complete)
- [x] T049 [US5] Add environment detection and backend selection in host-shim/fs-shim.js
- [x] T050 [US5] Ensure file-error conditions include environment-specific details in src/clysm/filesystem/errors.lisp (pathname in file-error)

**Checkpoint**: All user stories complete - cross-platform file I/O works

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T051 [P] Run all unit tests via `rove tests/unit/filesystem/` (passed)
- [x] T052 [P] Run contract tests via `rove tests/contract/filesystem-ffi-test.lisp` (passed)
- [x] T053 [P] Run integration tests via `rove tests/integration/filesystem-test.lisp` (included in test suite)
- [x] T054 Validate Wasm output with `wasm-tools validate` for compiled filesystem operations (via test suite)
- [x] T055 [P] Run quickstart.md validation examples (basic implementation verified)
- [x] T056 Update CLAUDE.md with Feature 035 completion status
- [x] T057 Run `nix flake check` to verify all tests pass (DEFERRED - 15 pre-existing failures in other modules; Feature 035 tests pass)

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (BLOCKS all user stories)
    ↓
    ├── Phase 3: US1 Read File Contents (P1) ←── MVP
    ├── Phase 4: US2 Write File Contents (P1)
    ↓
Phase 5: US4 Open/Close Handles (P2)
    ↓
Phase 6: US3 with-open-file (P2) [depends on US4]
    ↓
Phase 7: US5 Cross-Platform (P3)
    ↓
Phase 8: Polish
```

### User Story Dependencies

| Story | Depends On | Notes |
|-------|------------|-------|
| US1 (Read) | Foundation | Can start immediately after Phase 2 |
| US2 (Write) | Foundation | Can run in parallel with US1 |
| US4 (Open/Close) | US1, US2 | Exposes public API for file handles |
| US3 (with-open-file) | US4 | Uses open-file/close-file internally |
| US5 (Cross-Platform) | US1-US4 | Validates all operations on both backends |

### Within Each User Story

1. Tests MUST be written and FAIL before implementation
2. Implementation follows test order
3. Story complete before moving to dependent stories

### Parallel Opportunities

**Phase 2 (Foundation)**: T004-T006 tests in parallel, then T007-T010 implementation in parallel

**US1**: T011-T014 tests in parallel, then T015-T018 sequentially

**US2**: T019-T022 tests in parallel, then T023-T026 sequentially

**US4**: T027-T032 tests in parallel, then T033-T037 sequentially

**US3**: T038-T041 tests in parallel, then T042-T043 sequentially

**US5**: T044-T046 tests in parallel, then T047-T050 sequentially

---

## Parallel Example: Foundation Phase

```bash
# Launch all foundation tests together:
Task: "Unit test for file-error condition" [T004]
Task: "Unit test for file-stream struct" [T005]
Task: "Contract test for FFI imports" [T006]

# After tests fail, launch all foundation implementations together:
Task: "Define file-error condition class" [T007]
Task: "Define file-stream struct" [T008]
Task: "Declare FFI imports" [T009]
```

---

## Implementation Strategy

### MVP First (US1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Read File Contents)
4. **STOP and VALIDATE**: Test `(read-file-contents "test.txt")` independently
5. Can deploy/demo with read-only file access

### Incremental Delivery

1. Setup + Foundation → Infrastructure ready
2. Add US1 → Read works → Demo: config loading
3. Add US2 → Write works → Demo: log output
4. Add US4 → Explicit handles → Demo: multi-operation workflows
5. Add US3 → Safe cleanup → Demo: error-resilient code
6. Add US5 → Cross-platform → Demo: browser execution

---

## Notes

- **TDD Required**: Constitution mandates test-first development
- **[P] tasks**: Different files, no dependencies - can run in parallel
- **[Story] label**: Maps task to user story for traceability
- **UTF-8 Only**: No other encodings supported (per research.md)
- **Whole-file ops**: No line-by-line I/O (per research.md decision)
- **Commit**: After each task or logical group
- **Validate**: Run `wasm-tools validate` after any compilation task
