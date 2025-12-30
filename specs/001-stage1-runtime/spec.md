# Feature Specification: Stage 1 Runtime Environment

**Feature Branch**: `001-stage1-runtime`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-8: Stage 1実行環境を構築する。Node.js host-shimでdist/clysm-stage1.wasmを実行し、compile_form関数が動作することを確認する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Execute Stage 1 Wasm Module (Priority: P1)

A compiler developer needs to run the Stage 1 compiler (dist/clysm-stage1.wasm, 24.5KB) in Node.js to verify that the bootstrap pipeline produces a functional compiler.

**Why this priority**: This is the foundational capability that enables all subsequent bootstrap verification. Without a working runtime environment, no other Stage 1 functionality can be tested.

**Independent Test**: Can be tested by running `node host-shim/stage1-runner.js dist/clysm-stage1.wasm` and verifying the module loads and executes `_start` without errors.

**Acceptance Scenarios**:

1. **Given** clysm-stage1.wasm exists in dist/, **When** the runner script is executed, **Then** the Wasm module loads successfully and executes _start
2. **Given** the module requires FFI imports (clysm:io, clysm:fs), **When** the module is instantiated, **Then** all required imports are provided by the host shim
3. **Given** the _start function executes, **When** any runtime errors occur, **Then** error messages are displayed with stack traces

---

### User Story 2 - FFI File System Operations (Priority: P1)

The Stage 1 compiler needs to read Lisp source files and write compiled Wasm output through host-provided file system functions.

**Why this priority**: File I/O is essential for the compiler to read source files and produce output. This is co-equal with P1 as the runtime cannot function without FFI.

**Independent Test**: Can be tested by verifying fs.read-all returns file contents and fs.write-all creates files with correct contents.

**Acceptance Scenarios**:

1. **Given** a file path, **When** fs.read-all is called, **Then** the file contents are returned as a string (or null if file doesn't exist)
2. **Given** a file path and content string, **When** fs.write-all is called, **Then** the file is created/overwritten with the content
3. **Given** fs.open is called with a path, **When** the file exists, **Then** a file descriptor (integer) is returned
4. **Given** a file descriptor, **When** fs.close is called, **Then** the file handle is released

---

### User Story 3 - Console Output for Debugging (Priority: P2)

Developers need to see compiler output and debug messages during Stage 1 execution.

**Why this priority**: While essential for debugging, the compiler can technically run without console output. This enables observability.

**Independent Test**: Can be tested by having the Wasm module call write-string and verifying output appears on stdout.

**Acceptance Scenarios**:

1. **Given** the Wasm module calls clysm:io write-char, **When** executed, **Then** the character appears on stdout
2. **Given** the Wasm module calls clysm:io write-string, **When** executed, **Then** the string appears on stdout
3. **Given** stderr file descriptor (2), **When** write operations target it, **Then** output goes to stderr

---

### User Story 4 - Compile Form Verification (Priority: P2)

A developer wants to verify the Stage 1 compiler can compile a simple Lisp form like `(+ 1 2)` and produce valid Wasm bytes.

**Why this priority**: This validates the core compilation capability but depends on P1 scenarios working first.

**Independent Test**: Can be tested by invoking compile_form with a simple expression and checking the result is valid Wasm (magic bytes: 0x00 0x61 0x73 0x6d).

**Acceptance Scenarios**:

1. **Given** a simple Lisp expression string "(+ 1 2)", **When** compile_form is called, **Then** Wasm bytes are returned (or an error is reported)
2. **Given** compile_form returns bytes, **When** validated with wasm-tools, **Then** the output is structurally valid Wasm
3. **Given** the compiler encounters an unsupported form, **When** compile_form is called, **Then** a clear error message explains the limitation

---

### User Story 5 - Stage 2 Generation Script (Priority: P3)

A developer wants to use Stage 1 to compile the Clysm source code and produce Stage 2, advancing toward fixpoint verification.

**Why this priority**: This is the ultimate goal but depends on all prior scenarios. It may not be achievable until compilation rate improves beyond current 14.1%.

**Independent Test**: Can be tested by running `./scripts/generate-stage2.sh` and checking for dist/clysm-stage2.wasm output.

**Acceptance Scenarios**:

1. **Given** Stage 1 is functional, **When** the Stage 2 generation script runs, **Then** it attempts to compile all Clysm source modules
2. **Given** compilation produces output, **When** the script completes, **Then** dist/clysm-stage2.wasm exists (or a report explains what failed)
3. **Given** Stage 2 is generated, **When** compared to Stage 1, **Then** a fixpoint check reports whether they match

---

### Edge Cases

- What happens when Stage 1 wasm has missing or changed exports?
- How does the system handle Wasm instantiation failures due to import mismatches?
- What happens when fs.read-all is called on a non-existent file?
- How are UTF-8 encoding errors in source files handled?
- What happens if compile_form is not exported by Stage 1?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST load and execute dist/clysm-stage1.wasm using Node.js WebAssembly API
- **FR-002**: System MUST provide FFI function `clysm:fs.read-all` that reads file contents and returns string
- **FR-003**: System MUST provide FFI function `clysm:fs.write-all` that writes string contents to file
- **FR-004**: System MUST provide FFI function `clysm:fs.open` that returns file descriptor for path
- **FR-005**: System MUST provide FFI function `clysm:fs.close` that releases file descriptor
- **FR-006**: System MUST provide FFI function `clysm:io.write-char` for character output to stdout/stderr
- **FR-007**: System MUST provide FFI function `clysm:io.write-string` for string output to stdout/stderr
- **FR-008**: System MUST provide FFI function `clysm:io.read-char` for character input from stdin
- **FR-009**: System MUST provide FFI function `clysm:io.read-line` for line input from stdin
- **FR-010**: System MUST invoke the `_start` export to initialize the Stage 1 runtime
- **FR-011**: System MUST provide a shell script `scripts/run-stage1.sh` to execute Stage 1
- **FR-012**: System MUST provide a shell script `scripts/generate-stage2.sh` for Stage 2 generation
- **FR-013**: System MUST report execution status via exit codes (0=success, non-zero=failure with specific meaning)
- **FR-014**: System MUST handle the case where `compile_form` is not exported and report this clearly

### Key Entities

- **Stage 1 Wasm Module**: The 24.5KB compiled Wasm binary at dist/clysm-stage1.wasm containing the Clysm compiler
- **Host Shim**: Node.js module providing FFI implementations for clysm:io and clysm:fs namespaces
- **File Descriptor Table**: Runtime mapping of integer FDs to open file handles
- **Compilation Report**: JSON output documenting compilation results, errors, and progress

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 wasm module loads and executes _start successfully (exit code 0 or documented limitation code)
- **SC-002**: FFI functions respond correctly to basic operations (file read returns content, write creates file)
- **SC-003**: Console output from Stage 1 appears on Node.js stdout
- **SC-004**: Scripts execute in under 10 seconds for basic operations
- **SC-005**: Clear error messages identify any missing exports or FFI mismatches
- **SC-006**: Stage 2 generation produces either valid output or an actionable report

## Dependencies & Assumptions

### Dependencies

- Node.js 20+ with WebAssembly GC support
- wasm-tools for validation
- Existing host-shim infrastructure (io-shim.js, fs-shim.js)
- dist/clysm-stage1.wasm (24.5KB) from Phase 13D-7

### Assumptions

- Stage 1 exports `_start` function (confirmed via wasm-tools print)
- Stage 1 imports match clysm:io and clysm:fs namespaces (confirmed)
- compile_form may not be exported in current Stage 1 (14.1% compilation rate suggests limited functionality)
- UTF-8 encoding is used for all text I/O (per FR-022 from existing codebase)

## Out of Scope

- Improving Stage 1 compilation rate (separate feature)
- Achieving actual fixpoint (requires higher compilation rate)
- Browser/Web runtime support (Node.js only)
- wasmtime direct execution (Node.js host shim only)
