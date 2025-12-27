# Feature Specification: Stage 0 Complete Compiler

**Feature Branch**: `045-stage0-complete-compiler`
**Created**: 2025-12-28
**Status**: Draft
**Input**: Phase 13完了: Stage 0完全コンパイラを実装する。目標はStage 0がwasmtime上でClysm自身をコンパイルし、Stage 1を生成できること。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Simple Expression via CLI (Priority: P1)

As a developer, I can compile a simple Lisp expression using Stage 0 on wasmtime to verify the compiler's basic functionality.

**Why this priority**: This is the fundamental capability that proves Stage 0 is a working compiler. Without this, no further self-hosting work is possible.

**Independent Test**: Run `wasmtime run clysm-stage0.wasm --compile '(+ 1 2)'` and verify it produces valid Wasm output containing the computed result.

**Acceptance Scenarios**:

1. **Given** Stage 0 binary is available in dist/clysm-stage0.wasm, **When** user runs `wasmtime run dist/clysm-stage0.wasm --compile '(+ 1 2)'`, **Then** the command exits successfully (exit 0) and outputs valid Wasm bytes or compiled result indicator.

2. **Given** Stage 0 binary is running, **When** user compiles `(defun add (a b) (+ a b))`, **Then** the output contains a valid function definition in Wasm format.

3. **Given** Stage 0 binary is running, **When** user compiles an invalid expression `(+ 1`, **Then** the compiler reports an error with a meaningful message and exits with non-zero status.

---

### User Story 2 - Compile Clysm Source to Stage 1 (Priority: P1)

As a developer, I can use Stage 0 running on wasmtime to compile all Clysm compiler source files, producing a Stage 1 binary.

**Why this priority**: This is the core self-hosting capability. Stage 0 must be able to compile its own source code.

**Independent Test**: Run Stage 0 with all 45 compiler source modules as input and verify it produces a valid Stage 1 Wasm binary that passes wasm-tools validation.

**Acceptance Scenarios**:

1. **Given** Stage 0 binary and all Clysm source files, **When** compile_all is invoked via host shim, **Then** Stage 1 binary is generated at dist/clysm-stage1.wasm.

2. **Given** Stage 0 compiling Clysm source, **When** a source file contains unsupported CL feature, **Then** compilation continues with graceful degradation and logs the unsupported form.

3. **Given** Stage 0 has compiled all modules, **When** Stage 1 binary is validated, **Then** `wasm-tools validate dist/clysm-stage1.wasm` exits successfully.

---

### User Story 3 - Achieve Fixed-Point (Stage 1 == Stage 2) (Priority: P1)

As a developer, I can verify that the Clysm compiler achieves fixed-point by demonstrating Stage 1 and Stage 2 are byte-identical.

**Why this priority**: Fixed-point verification proves complete self-hosting - the compiler can exactly reproduce itself.

**Independent Test**: Run `./scripts/verify-fixpoint.sh` and verify it exits with code 0, confirming Stage 1 == Stage 2.

**Acceptance Scenarios**:

1. **Given** Stage 0, Stage 1, and Stage 2 binaries exist, **When** `./scripts/verify-fixpoint.sh` is run, **Then** it compares Stage 1 and Stage 2 byte-by-byte and reports "ACHIEVED" with exit 0.

2. **Given** Stage 1 compiles Clysm source to Stage 2, **When** Stage 2 binary size is measured, **Then** it matches Stage 1 binary size exactly.

3. **Given** fixed-point is achieved, **When** verification history is checked, **Then** the result is logged to dist/verification-history.jsonl with timestamp and status.

---

### User Story 4 - Runtime Initialization (Priority: P2)

As a Wasm runtime, I need Stage 0 to properly initialize its runtime environment including type definitions, global variables, and FFI bindings on startup.

**Why this priority**: Runtime initialization is required before any compilation can occur, but is internal infrastructure rather than user-facing.

**Independent Test**: Load Stage 0 in wasmtime and verify it initializes without errors before accepting compilation requests.

**Acceptance Scenarios**:

1. **Given** Stage 0 binary is loaded in wasmtime, **When** the module instantiates, **Then** all WasmGC types (24+ type definitions) are available.

2. **Given** Stage 0 initializes, **When** globals are accessed, **Then** NIL, UNBOUND, mv-count, and mv-buffer globals are properly set.

3. **Given** Stage 0 initializes, **When** FFI imports are resolved, **Then** host functions (fs.open, fs.read-all, fs.write-all, fs.close) are callable.

---

### User Story 5 - Read Source Files via FFI (Priority: P2)

As Stage 0 compiler, I need to read Clysm source files from the host filesystem to compile them into Stage 1.

**Why this priority**: File reading is required for compile_all but is infrastructure supporting the core self-hosting capability.

**Independent Test**: Have Stage 0 read a simple .lisp file and verify the content is correctly received as a string.

**Acceptance Scenarios**:

1. **Given** Stage 0 is running with fs-shim.js host, **When** read-file-contents is called with valid path, **Then** file contents are returned as UTF-8 string.

2. **Given** Stage 0 attempts to read non-existent file, **When** fs.read-all fails, **Then** file-error condition is signaled.

3. **Given** Stage 0 reads multiple source files, **When** all 45 modules are read, **Then** each file's content is available for compilation.

---

### Edge Cases

- What happens when wasmtime memory is exhausted during compilation? The compiler should signal memory-error and exit gracefully.
- How does Stage 0 handle circular dependencies between modules? Modules are compiled in pre-defined dependency order from src/clysm/validation/compiler-order.lisp.
- What happens if fs-shim.js is not available? Stage 0 should fail on first FFI call with meaningful error indicating missing host shim.
- How does Stage 0 handle malformed S-expressions in source files? Parser should signal read-error with line/column information.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Stage 0 MUST export a `compile_form` function that accepts a Lisp S-expression string and returns compiled Wasm bytes.
- **FR-002**: Stage 0 MUST export a `compile_all` function that compiles all Clysm source modules and returns a complete Wasm binary.
- **FR-003**: Stage 0 MUST initialize WasmGC runtime on module instantiation, including all 24+ type definitions.
- **FR-004**: Stage 0 MUST initialize global variables (NIL at index 0, UNBOUND at index 1, mv-count at index 2, mv-buffer at index 3).
- **FR-005**: Stage 0 MUST import host FFI functions (fs.open, fs.read-all, fs.write-all, fs.close) for filesystem access.
- **FR-006**: Stage 0 MUST produce valid WasmGC binary output that passes wasm-tools validation.
- **FR-007**: Stage 0 MUST support CLI invocation via wasmtime with `--compile` argument for single-expression compilation.
- **FR-008**: Stage 0 MUST compile all blessed subset Common Lisp features as documented in docs/blessed-subset.lisp.
- **FR-009**: Stage 0 MUST read source files in dependency order from the 45 modules listed in compiler-order.lisp.
- **FR-010**: Fixed-point verification script MUST compare Stage 1 and Stage 2 byte-by-byte and report status.
- **FR-011**: Stage 0 MUST handle compilation errors gracefully, logging failures and continuing with remaining forms.
- **FR-012**: Stage 0 MUST emit proper Wasm export section with named exports for all compiled top-level functions.

### Key Entities

- **Stage 0 Binary**: The compiler Wasm binary produced by SBCL/interpreter that runs on wasmtime. Located at dist/clysm-stage0.wasm.
- **Stage 1 Binary**: The compiler Wasm binary produced by Stage 0 compiling Clysm source. Located at dist/clysm-stage1.wasm.
- **Stage 2 Binary**: The compiler Wasm binary produced by Stage 1 compiling Clysm source. Located at dist/clysm-stage2.wasm.
- **Host Shim**: JavaScript module (host-shim/stage1-host.js) providing FFI implementations for wasmtime.
- **Compiler Order**: The 45 source modules in dependency order (src/clysm/validation/compiler-order.lisp).
- **Blessed Subset**: The supported Common Lisp features that Stage 0 can compile (docs/blessed-subset.lisp).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Running `wasmtime run dist/clysm-stage0.wasm --compile '(+ 1 2)'` completes successfully (exit 0) and outputs compiled result.
- **SC-002**: Stage 0 successfully compiles all 45 Clysm source modules to produce valid Stage 1 binary.
- **SC-003**: `wasm-tools validate dist/clysm-stage1.wasm` exits with code 0.
- **SC-004**: `wasm-tools validate dist/clysm-stage2.wasm` exits with code 0.
- **SC-005**: `./scripts/verify-fixpoint.sh` exits with code 0, confirming Stage 1 == Stage 2 (byte-identical).
- **SC-006**: Fixed-point verification completes within 5 minutes on standard development hardware.
- **SC-007**: Stage 0 binary size is under 5MB (reasonable for a compiler).
- **SC-008**: Stage 1 and Stage 2 binaries are identical in size and content (diff returns no differences).

## Assumptions

- wasmtime is installed and available in PATH for Wasm execution.
- wasm-tools is installed and available for Wasm validation.
- Node.js is available for running host-shim JavaScript files.
- The blessed subset of Common Lisp features is sufficient for compiling the Clysm compiler source.
- SBCL 2.4+ is available as the initial host for generating Stage 0 (or interpreter-based bootstrap).
- The 45 source modules represent the complete compiler codebase to be self-compiled.

## Dependencies

- **Feature 037**: Cross-Compile Stage 0 - Provides bootstrap infrastructure.
- **Feature 039**: Stage 1 Compiler Generation - Provides Stage 1 generation infrastructure.
- **Feature 040**: Fixed-Point Verification - Provides verification scripts and infrastructure.
- **Feature 044**: Interpreter Bootstrap - Provides alternative Stage 0 generation path.
- **Feature 027**: Complete FFI Foundation - Provides FFI types and marshalling.
- **Feature 035**: FFI Filesystem Access - Provides file system operations via FFI.
