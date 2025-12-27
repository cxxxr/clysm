# Feature Specification: Stage 1 Compiler Generation

**Feature Branch**: `039-stage1-compiler-gen`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 13A: Stage 1コンパイラ生成を実装する。目標はWasm版Clysm（Stage 0）を使用してClysm自身をコンパイルし、Stage 1バイナリを生成すること。Stage 0実行インフラ（wasmtime + FFI）の整備、ソースファイル読み込みプロトコル、コンパイル進捗測定、差分分析ツールを含む。検証基準: Stage 0がwasmtime上で基本フォームをコンパイル可能、Stage 1バイナリ生成（部分的でも可）。ブロッカー特定と将来の機能拡張計画も含める。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Stage 0 Runtime Verification (Priority: P1)

As a compiler developer, I want to verify that the Stage 0 Wasm binary can execute basic Lisp forms on wasmtime, so that I can confirm the bootstrap compiler is functional before attempting self-compilation.

**Why this priority**: This is the foundational verification step. Without a working Stage 0 runtime, no further self-compilation work is possible. This validates the entire Phase 037-038 bootstrap output.

**Independent Test**: Can be fully tested by running Stage 0 on wasmtime with test expressions and verifying correct results. Delivers confidence that the bootstrap compiler works.

**Acceptance Scenarios**:

1. **Given** the Stage 0 binary (dist/clysm-stage0.wasm) exists, **When** I execute it with wasmtime using a simple arithmetic expression like `(+ 1 2)`, **Then** the runtime returns the correct result (3).
2. **Given** Stage 0 is running on wasmtime, **When** I compile a `defun` form followed by a function call, **Then** the function is defined and callable, returning the expected value.
3. **Given** Stage 0 is running on wasmtime, **When** I attempt to compile an unsupported form, **Then** a clear error message is returned identifying the unsupported construct.

---

### User Story 2 - Source File Reading Protocol (Priority: P1)

As a compiler developer, I want Stage 0 to read Lisp source files from the filesystem via FFI, so that it can process the compiler source code for self-compilation.

**Why this priority**: Self-compilation requires reading source files. Without this capability, Stage 0 cannot access the compiler source code it needs to compile.

**Independent Test**: Can be fully tested by having Stage 0 read a test Lisp file and return its contents. Delivers the file I/O foundation for self-compilation.

**Acceptance Scenarios**:

1. **Given** a Lisp source file exists at a known path, **When** Stage 0 invokes the file reading FFI function, **Then** the file contents are returned as a string.
2. **Given** Stage 0 has read file contents, **When** I parse the contents as Lisp forms, **Then** each top-level form is available for compilation.
3. **Given** a file path that does not exist, **When** Stage 0 attempts to read it, **Then** an appropriate error condition is signaled.

---

### User Story 3 - Compilation Progress Measurement (Priority: P2)

As a compiler developer, I want to measure and track compilation progress when Stage 0 compiles the Clysm source, so that I can identify how much of the compiler can be self-compiled and track improvement over time.

**Why this priority**: Progress measurement is essential for iterative improvement. It helps identify blockers and validates that each enhancement increases self-compilation capability.

**Independent Test**: Can be fully tested by running Stage 0 compilation on a subset of source files and viewing progress statistics. Delivers visibility into self-compilation coverage.

**Acceptance Scenarios**:

1. **Given** Stage 0 is compiling source modules, **When** each form is processed, **Then** success/failure statistics are recorded per module.
2. **Given** compilation completes, **When** I request a progress report, **Then** I see total forms attempted, forms compiled successfully, forms failed, and percentage coverage.
3. **Given** compilation failures occur, **When** I view the failure report, **Then** failures are grouped by operator type with example expressions for each.

---

### User Story 4 - Stage 1 Binary Generation (Priority: P2)

As a compiler developer, I want Stage 0 to generate a Stage 1 Wasm binary from the compiled Clysm source, so that I can verify self-compilation produces a valid compiler.

**Why this priority**: This is the ultimate goal of the bootstrapping process. Even partial Stage 1 generation validates the self-compilation path.

**Independent Test**: Can be fully tested by running Stage 0 self-compilation and validating the output binary with wasm-tools. Delivers proof of self-compilation capability.

**Acceptance Scenarios**:

1. **Given** Stage 0 has successfully compiled forms from the source, **When** I request binary generation, **Then** a Wasm binary is written to the output path.
2. **Given** a Stage 1 binary is generated, **When** I validate it with wasm-tools, **Then** the binary passes validation without errors.
3. **Given** a Stage 1 binary is generated, **When** I compare its size and exports to Stage 0, **Then** I can see what functionality was included.

---

### User Story 5 - Blocker Analysis and Reporting (Priority: P2)

As a compiler developer, I want to identify and document blockers preventing full self-compilation, so that I can plan future enhancements to increase compilation coverage.

**Why this priority**: Understanding blockers enables prioritized development. Clear blocker documentation guides future feature work.

**Independent Test**: Can be fully tested by running analysis on failed forms and generating a blocker report. Delivers actionable insights for improvement.

**Acceptance Scenarios**:

1. **Given** compilation has completed with failures, **When** I run blocker analysis, **Then** I get a categorized list of unsupported constructs.
2. **Given** blocker analysis is complete, **When** I view the report, **Then** each blocker category shows the number of affected forms and priority recommendation.
3. **Given** blocker analysis results, **When** I generate a future work plan, **Then** features are ordered by impact on compilation coverage.

---

### User Story 6 - Diff Analysis Between Stages (Priority: P3)

As a compiler developer, I want to compare Stage 0 and Stage 1 binaries to understand differences, so that I can verify correctness and identify discrepancies.

**Why this priority**: Diff analysis validates compilation correctness. Lower priority because it's only meaningful after Stage 1 generation succeeds.

**Independent Test**: Can be fully tested by comparing two Wasm binaries and viewing a diff report. Delivers verification that self-compilation produces correct output.

**Acceptance Scenarios**:

1. **Given** both Stage 0 and Stage 1 binaries exist, **When** I run diff analysis, **Then** I see a summary of size, export, and type section differences.
2. **Given** diff analysis is running, **When** I request detailed comparison, **Then** function-level differences are listed with signatures.
3. **Given** Stage 1 has fewer exports than Stage 0, **When** I view the diff, **Then** missing exports are highlighted for investigation.

---

### Edge Cases

- What happens when Stage 0 encounters a recursive macro that causes infinite expansion?
- How does the system handle source files with encoding issues (non-UTF-8)?
- What happens if memory runs out during compilation of large modules?
- How are circular dependencies between modules detected and handled?
- What happens when a source file contains only comments and whitespace?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST execute Stage 0 Wasm binary on wasmtime with proper FFI host bindings
- **FR-002**: System MUST provide FFI functions for reading source files from the filesystem
- **FR-003**: System MUST parse Lisp source files into individual top-level forms
- **FR-004**: System MUST compile each form and track success/failure status
- **FR-005**: System MUST aggregate compilation statistics per module and overall
- **FR-006**: System MUST generate progress reports showing forms compiled, forms failed, and coverage percentage
- **FR-007**: System MUST group failures by operator type with example expressions
- **FR-008**: System MUST produce a valid Wasm binary from successfully compiled forms
- **FR-009**: System MUST validate generated binaries pass wasm-tools validation
- **FR-010**: System MUST categorize blockers by unsupported construct type
- **FR-011**: System MUST estimate impact of each blocker on overall compilation coverage
- **FR-012**: System MUST provide diff analysis comparing Stage 0 and Stage 1 binaries
- **FR-013**: System MUST handle compilation errors gracefully without crashing the runtime
- **FR-014**: System MUST support incremental compilation (resume from checkpoint)

### Key Entities

- **Stage 0 Binary**: The Wasm binary produced by host Clysm, capable of compiling Lisp forms
- **Stage 1 Binary**: The Wasm binary produced by Stage 0 self-compilation
- **Source Module**: A Lisp source file in the Clysm compiler codebase
- **Compilation Unit**: A single top-level form extracted from a source module
- **Progress Report**: Aggregated statistics on compilation success/failure
- **Blocker Report**: Categorized list of unsupported constructs preventing compilation
- **Diff Report**: Comparison between two Wasm binaries showing differences

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 0 correctly evaluates basic Lisp expressions (arithmetic, function calls, conditionals) on wasmtime
- **SC-002**: Stage 0 can read and parse all 41 compiler source modules
- **SC-003**: At least 25% of compiler forms compile successfully in Stage 0 (improvement from 19.6% in Phase 038)
- **SC-004**: A valid Stage 1 Wasm binary is generated that passes wasm-tools validation
- **SC-005**: Blocker report identifies the top 5 unsupported constructs by frequency
- **SC-006**: Progress reports are generated within 5 seconds of compilation completion
- **SC-007**: All compilation failures are captured with context (file, line, form, error type)
- **SC-008**: Diff analysis correctly identifies export and type section differences between binaries

## Assumptions

- Stage 0 binary (dist/clysm-stage0.wasm) from Phase 037-038 is available and valid
- wasmtime runtime is available in the development environment
- Host FFI shim (host-shim/) provides required filesystem access functions
- Source modules are in UTF-8 encoding
- Compilation order follows the dependency order established in compiler-order.lisp
- Memory limits on wasmtime are sufficient for compiling the Clysm codebase

## Dependencies

- Feature 037: Cross-Compile Stage 0 (Lisp-11) - COMPLETE
- Feature 038: Stage 0 Capability Extension - COMPLETE
- Feature 035: FFI Filesystem Access - COMPLETE
- Feature 036: Compiler Subset Validation - COMPLETE (provides module ordering)
