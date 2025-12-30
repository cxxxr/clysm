# Feature Specification: Stage 1 Compiler Generation (Phase 13D-7)

**Feature Branch**: `040-stage1-compiler-gen`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-7: Stage 1コンパイラ生成を実装する。目標はSBCL上でClysmコンパイラ全体（約45,000行）をWasmにコンパイルし、実際に動作するStage 1を生成すること。Phase 13D-1〜13D-6で実装したANSI CL機能を活用し、コンパイル率80%+を達成。出力されるdist/clysm-stage1.wasmは100KB以上かつwasm-tools validateをパスすること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Full Clysm Compiler to Wasm (Priority: P1)

A compiler developer wants to compile the entire Clysm compiler codebase (approximately 45,000 lines of Common Lisp) to WebAssembly using SBCL and the Clysm host compiler, producing a functional Stage 1 binary.

**Why this priority**: This is the core goal of the phase - without successful full-codebase compilation, no subsequent bootstrap steps are possible.

**Independent Test**: Can be fully tested by running `sbcl --load build/stage1-complete.lisp` and verifying the output file exists at `dist/clysm-stage1.wasm` with size > 100KB.

**Acceptance Scenarios**:

1. **Given** SBCL with Clysm loaded and Phase 13D-1~6 features available, **When** the Stage 1 build script is executed, **Then** the system compiles all compiler source files to Wasm bytecode.
2. **Given** all compiler source files in `src/clysm/`, **When** compilation completes, **Then** the output is written to `dist/clysm-stage1.wasm`.
3. **Given** the generated `dist/clysm-stage1.wasm`, **When** file size is measured, **Then** size is at least 100KB.

---

### User Story 2 - Achieve 80%+ Compilation Rate (Priority: P1)

A compiler developer expects at least 80% of all compiler forms to compile successfully, leveraging the ANSI CL features implemented in Phase 13D-1~6 (aref, svref, coerce, subseq, concatenate, handler-case, values, the, labels, LOOP extensions).

**Why this priority**: High compilation rate is essential for a functional Stage 1 - without it, critical compiler functionality will be missing.

**Independent Test**: Can be tested by examining compilation statistics output during build, verifying compiled-forms / total-forms >= 0.80.

**Acceptance Scenarios**:

1. **Given** the Clysm compiler source files, **When** Stage 1 compilation runs, **Then** at least 80% of top-level forms compile without errors.
2. **Given** forms using Phase 13D features (aref, svref, coerce, subseq, concatenate, handler-case, values, the, labels, LOOP macros), **When** compiled, **Then** they generate valid Wasm instructions.
3. **Given** compilation completes, **When** statistics are reported, **Then** the report shows compilation rate percentage and lists any failed forms.

---

### User Story 3 - Validate Generated Wasm (Priority: P1)

A compiler developer wants assurance that the generated Stage 1 binary is structurally valid WebAssembly that can be loaded by Wasm runtimes.

**Why this priority**: Invalid Wasm cannot be executed - validation is a prerequisite for any runtime testing.

**Independent Test**: Can be tested by running `wasm-tools validate dist/clysm-stage1.wasm` and verifying exit code 0.

**Acceptance Scenarios**:

1. **Given** the generated `dist/clysm-stage1.wasm`, **When** `wasm-tools validate` is run, **Then** validation passes with exit code 0.
2. **Given** valid Wasm output, **When** examined with `wasm-tools print`, **Then** it shows well-formed module structure with functions, types, and globals.

---

### User Story 4 - Track Compilation Progress (Priority: P2)

A compiler developer wants visibility into which files and forms are compiling successfully vs. failing, to identify remaining blockers.

**Why this priority**: Diagnostic information aids debugging but is not strictly required for Stage 1 generation.

**Independent Test**: Can be tested by examining build log output for per-file statistics.

**Acceptance Scenarios**:

1. **Given** Stage 1 compilation in progress, **When** each source file is processed, **Then** the system reports file name, form count, and success count.
2. **Given** a form fails to compile, **When** error occurs, **Then** the system logs the form head and error type without aborting compilation.
3. **Given** compilation completes, **When** summary is generated, **Then** it lists total forms, compiled forms, failed forms, and overall percentage.

---

### Edge Cases

- What happens when a source file has syntax errors? System logs error and continues with remaining files.
- How does system handle forms with unsupported special operators? Logs warning, skips form, continues compilation.
- What happens when output directory `dist/` doesn't exist? System creates it automatically.
- How does system handle circular dependencies between compiler modules? Uses topological sort based on ASDF system definition.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST compile all Clysm compiler source files from `src/clysm/` directory tree to WebAssembly.
- **FR-002**: System MUST leverage Phase 13D-1~6 features including aref, svref, coerce, subseq, concatenate, handler-case, values, the, labels, and LOOP extensions.
- **FR-003**: System MUST achieve compilation success rate of at least 80% of total forms.
- **FR-004**: System MUST generate output to `dist/clysm-stage1.wasm`.
- **FR-005**: Generated Wasm MUST pass `wasm-tools validate` without errors.
- **FR-006**: Generated Wasm file MUST be at least 100KB in size.
- **FR-007**: System MUST report compilation statistics including total forms, compiled forms, and percentage.
- **FR-008**: System MUST continue compilation after individual form failures rather than aborting.
- **FR-009**: System MUST log failed forms with identifying information for debugging.
- **FR-010**: Build script MUST be executable via `sbcl --load build/stage1-complete.lisp`.

### Key Entities

- **Stage 1 Module**: The compiled Wasm binary containing the Clysm compiler, output at `dist/clysm-stage1.wasm`.
- **Compiler Form**: A top-level Lisp expression in the compiler source (defun, defmacro, defclass, etc.).
- **Compilation Statistics**: Aggregate data tracking successful vs. failed compilation attempts.
- **Source File**: A `.lisp` file in `src/clysm/` containing compiler implementation.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 compilation completes without fatal errors when `sbcl --load build/stage1-complete.lisp` is executed.
- **SC-002**: Generated `dist/clysm-stage1.wasm` file size is at least 100KB.
- **SC-003**: At least 80% of compiler forms compile successfully (compiled-forms / total-forms >= 0.80).
- **SC-004**: `wasm-tools validate dist/clysm-stage1.wasm` exits with code 0.
- **SC-005**: Build produces compilation statistics showing form counts and success rate.
- **SC-006**: All Phase 13D-1~6 features (aref, svref, coerce, subseq, concatenate, handler-case, values, the, labels, LOOP extensions) are utilized in compiled output.

## Assumptions

- SBCL 2.4+ is installed and available in PATH.
- Phase 13D-1~6 implementations are complete and functional.
- `wasm-tools` is installed and available in PATH.
- The `dist/` directory may or may not exist prior to build.
- Compilation uses the existing ASDF system definition for dependency ordering.
- The 45,000 line count is approximate; actual source size may vary.
- "Functional Stage 1" means structurally valid Wasm that passes validation, not necessarily runtime-executable (runtime testing is a subsequent phase).

## Scope Boundaries

### In Scope

- Compiling all `src/clysm/**/*.lisp` files to Wasm.
- Generating single output module `dist/clysm-stage1.wasm`.
- Compilation statistics reporting.
- Error recovery and continued compilation after failures.
- Wasm validation via wasm-tools.

### Out of Scope

- Runtime execution of Stage 1 (deferred to Phase 13D-8).
- Node.js host-shim integration (deferred).
- Fixpoint verification Stage 1 vs Stage 2 (deferred).
- Performance optimization of generated Wasm.
- Source map generation for debugging.
