# Feature Specification: Fixed-Point Verification (Phase 13B)

**Feature Branch**: `040-fixed-point-verification`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 13B: 固定点検証を実装する。目標はStage 1コンパイラ（039で生成）を使用してStage 2を生成し、Stage 1 == Stage 2（ビット単位一致）を確認すること。wasmtime上でStage 1を実行し、ClysmソースをコンパイルしてStage 2を生成。バイナリ比較ツールで差分を検出し、固定点達成を自動検証する。固定点達成はセルフホスティング完了を意味する。達成できない場合は差分分析レポートを生成し、次のイテレーションで修正可能にする。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run Stage 1 Compiler to Generate Stage 2 (Priority: P1)

A developer uses the Stage 1 Wasm binary (generated in feature 039) running on wasmtime to compile the Clysm compiler source code, producing a Stage 2 Wasm binary. This is the core capability that enables fixed-point verification.

**Why this priority**: Without the ability to run Stage 1 and produce Stage 2, no verification is possible. This is the foundational capability for self-hosting validation.

**Independent Test**: Can be tested by executing Stage 1 on wasmtime with the Clysm source files as input and verifying a valid Wasm binary is produced as output.

**Acceptance Scenarios**:

1. **Given** a valid Stage 1 Wasm binary exists at `dist/clysm-stage1.wasm`, **When** the developer runs the Stage 2 generation command, **Then** the Stage 1 compiler processes all Clysm source modules and produces `dist/clysm-stage2.wasm`.
2. **Given** Stage 1 is running on wasmtime, **When** a source module fails to compile, **Then** the system logs the specific module and error, continues with remaining modules, and reports partial completion.
3. **Given** the host FFI shim is available, **When** Stage 1 needs file I/O operations, **Then** the FFI calls are properly handled by the Node.js host shim.

---

### User Story 2 - Verify Bit-Identical Match (Fixed-Point) (Priority: P1)

A developer compares Stage 1 and Stage 2 binaries byte-by-byte to verify they are identical. Bit-identical match (fixed-point) proves the compiler is self-hosting: it can reproduce itself exactly.

**Why this priority**: This is the primary success criterion. A bit-identical match is the mathematical proof of self-hosting completion.

**Independent Test**: Can be tested by running a binary comparison tool on two Wasm files and verifying the output indicates identical or different status.

**Acceptance Scenarios**:

1. **Given** both Stage 1 and Stage 2 binaries exist, **When** the verification command is run, **Then** the system reports "FIXED-POINT ACHIEVED" if binaries are byte-identical.
2. **Given** both Stage 1 and Stage 2 binaries exist, **When** the verification command is run and binaries differ, **Then** the system reports "FIXED-POINT NOT ACHIEVED" with diff summary.
3. **Given** Stage 1 or Stage 2 binary is missing, **When** the verification command is run, **Then** the system reports which binary is missing and how to generate it.

---

### User Story 3 - Generate Diff Analysis Report (Priority: P2)

When Stage 1 and Stage 2 differ, a developer receives a detailed diff analysis report identifying the specific differences (byte offsets, section mismatches, export differences). This enables targeted fixes for the next iteration.

**Why this priority**: Essential for debugging when fixed-point is not achieved, but only relevant after comparison reveals differences.

**Independent Test**: Can be tested by intentionally creating two different Wasm files and verifying the report correctly identifies their differences.

**Acceptance Scenarios**:

1. **Given** Stage 1 and Stage 2 binaries differ, **When** the diff analysis is run, **Then** a report is generated showing: byte offset of first difference, total differing bytes, section-by-section comparison.
2. **Given** Stage 1 and Stage 2 have different export sections, **When** the diff analysis is run, **Then** the report lists which exports are added, removed, or modified.
3. **Given** Stage 1 and Stage 2 have different type sections, **When** the diff analysis is run, **Then** the report identifies type signature differences.

---

### User Story 4 - Automated CI Verification (Priority: P2)

A CI pipeline automatically runs the fixed-point verification after each build, providing immediate feedback on self-hosting status. The pipeline uses exit codes and machine-readable output for integration.

**Why this priority**: Automation prevents regression and provides continuous validation, but requires the core functionality to be working first.

**Independent Test**: Can be tested by running the verification script and checking exit codes (0 for fixed-point achieved, non-zero otherwise).

**Acceptance Scenarios**:

1. **Given** a CI environment with wasmtime installed, **When** the verification script runs and fixed-point is achieved, **Then** the script exits with code 0 and outputs JSON result.
2. **Given** a CI environment, **When** the verification script runs and fixed-point is not achieved, **Then** the script exits with code 1 and outputs JSON with diff summary.
3. **Given** a CI environment, **When** Stage 1 fails to generate Stage 2, **Then** the script exits with code 2 and outputs JSON with compilation errors.

---

### User Story 5 - Iterative Improvement Tracking (Priority: P3)

A developer tracks progress toward fixed-point achievement across multiple iterations. The system maintains a history of compilation rates and diff sizes, showing improvement over time.

**Why this priority**: Useful for long-term development tracking, but not required for initial fixed-point verification.

**Independent Test**: Can be tested by running verification multiple times with different Stage 1 versions and verifying history is recorded.

**Acceptance Scenarios**:

1. **Given** previous verification runs exist, **When** a new verification completes, **Then** the result is appended to the history log with timestamp.
2. **Given** a history log exists, **When** the developer requests progress summary, **Then** the system shows: iteration count, best diff size achieved, trend direction.

---

### Edge Cases

- What happens when wasmtime is not installed or not in PATH?
- How does the system handle corrupted or invalid Stage 1 binary?
- What happens when Stage 1 runs out of memory during compilation?
- How are non-deterministic elements (timestamps, random seeds) handled if they exist in the compilation process?
- What happens when the host FFI shim crashes during Stage 1 execution?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST execute Stage 1 Wasm binary on wasmtime runtime with Node.js host shim for FFI support.
- **FR-002**: System MUST compile all Clysm source modules (as defined in 039 module list) using Stage 1 to produce Stage 2 binary.
- **FR-003**: System MUST perform byte-by-byte comparison of Stage 1 and Stage 2 binaries.
- **FR-004**: System MUST report fixed-point status (ACHIEVED/NOT ACHIEVED) with clear messaging.
- **FR-005**: System MUST generate structured diff analysis report when binaries differ.
- **FR-006**: System MUST provide machine-readable output (JSON) for CI integration.
- **FR-007**: System MUST use appropriate exit codes: 0 (fixed-point achieved), 1 (not achieved), 2 (compilation error), 3 (missing dependencies).
- **FR-008**: System MUST validate Stage 1 binary using wasm-tools before execution.
- **FR-009**: System MUST log compilation progress (module count, success rate, timing).
- **FR-010**: System MUST handle partial compilation failure gracefully, continuing with remaining modules.

### Key Entities

- **Stage 1 Binary**: The Wasm compiler binary generated by SBCL host (from feature 039), capable of compiling Clysm source.
- **Stage 2 Binary**: The Wasm compiler binary generated by running Stage 1 on wasmtime; should be identical to Stage 1 at fixed-point.
- **Diff Report**: Analysis document showing byte-level, section-level, and semantic differences between two binaries.
- **Verification Result**: Status record containing: fixed-point status, timestamp, diff summary (if applicable), Stage 1/Stage 2 paths.
- **Compilation Log**: Per-module compilation record with success/failure status, timing, and error messages.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Verification process completes within 5 minutes for full Clysm source compilation.
- **SC-002**: Binary comparison accurately detects any single-byte difference between Stage 1 and Stage 2.
- **SC-003**: Diff analysis report identifies at least 90% of differences at the section level (type, code, data, export sections).
- **SC-004**: CI verification provides actionable feedback within 10 minutes of commit.
- **SC-005**: Fixed-point achievement (Stage 1 == Stage 2) demonstrates complete self-hosting capability.
- **SC-006**: Each iteration toward fixed-point shows measurable progress (reduced diff size or increased compilation rate).

## Assumptions

- Stage 1 binary from feature 039 exists at `dist/clysm-stage1.wasm`.
- wasmtime runtime is available in the development/CI environment.
- Node.js is available for running FFI host shims.
- wasm-tools is available for Wasm validation.
- The Clysm source module list and compilation order from feature 039 is reused.
- Non-deterministic compilation elements (if any) will be addressed as they are discovered during verification.
- The existing diff infrastructure from feature 039 (`src/clysm/stage1/diff.lisp`) will be extended.

## Dependencies

- **Feature 039**: Stage 1 Compiler Generation - provides Stage 1 binary and compilation infrastructure.
- **Feature 037**: Cross-Compile Stage 0 - provides module ordering and bootstrap infrastructure.
- **wasmtime**: WebAssembly runtime for executing Stage 1.
- **wasm-tools**: Binary validation tool.
- **Node.js**: Runtime for FFI host shims.

## Out of Scope

- Extending Clysm's CL subset support (that would be a separate feature).
- Performance optimization of Stage 1 execution.
- Multi-stage bootstrapping beyond Stage 2 (Stage 3, etc.).
- Formal verification or proof of correctness beyond binary comparison.
