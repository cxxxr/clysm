# Feature Specification: Bootstrap Fixpoint Achievement

**Feature Branch**: `001-bootstrap-fixpoint`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-9: 固定点達成を実装する。目標はStage 1にcompile_form関数をエクスポートし、Stage 1でClysm自身をコンパイルしてStage 2を生成すること。Stage 1 == Stage 2（バイト単位一致）で固定点達成。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Stage 1 Compiler Export (Priority: P1)

As a compiler developer, I want Stage 1 to export its compile_form function so that it can be invoked by external runtime environments to compile Lisp forms into WebAssembly.

**Why this priority**: Without an exported compile_form function, Stage 1 cannot be used as a compiler. This is the foundational capability required for all subsequent bootstrap stages.

**Independent Test**: Can be tested by loading Stage 1 in Node.js, calling the exported compile_form with a simple Lisp form, and verifying it returns valid Wasm bytecode.

**Acceptance Scenarios**:

1. **Given** Stage 1 Wasm module is loaded in Node.js runtime, **When** I query the module exports, **Then** compile_form is listed as an exported function
2. **Given** Stage 1 is loaded with host shims, **When** I call compile_form with a simple form like `(+ 1 2)`, **Then** it returns valid Wasm bytecode that can be validated by wasm-tools
3. **Given** Stage 1 compile_form is invoked, **When** the input form uses unsupported features, **Then** a clear error indicating the blocker is returned

---

### User Story 2 - Stage 2 Generation (Priority: P2)

As a compiler developer, I want to use Stage 1 to compile the Clysm compiler source code, producing Stage 2 as output, so that I can verify the compiler's self-hosting capability.

**Why this priority**: Stage 2 generation is the core demonstration of self-hosting. Once Stage 1 can export compile_form, this user story validates that the exported compiler actually works on real compiler code.

**Independent Test**: Can be tested by running a generation script that loads Stage 1 and feeds it all Clysm source files, producing a Stage 2 Wasm binary.

**Acceptance Scenarios**:

1. **Given** Stage 1 is loaded with all host shims, **When** I run the Stage 2 generation script, **Then** it processes all compiler source files and produces a Stage 2 Wasm binary
2. **Given** the Stage 2 generation script runs, **When** compilation completes, **Then** the output Stage 2 Wasm passes wasm-tools validation
3. **Given** Stage 2 generation encounters compilation failures, **When** the script completes, **Then** a detailed report identifies which forms failed and why

---

### User Story 3 - Fixpoint Verification (Priority: P3)

As a compiler developer, I want to verify that Stage 1 and Stage 2 are byte-identical so that I can confirm the compiler has achieved bootstrap fixpoint.

**Why this priority**: Fixpoint verification is the ultimate proof of correct self-hosting. It can only be tested after Stage 2 is successfully generated, hence lower priority.

**Independent Test**: Can be tested by comparing Stage 1 and Stage 2 binary files byte-by-byte after both are generated.

**Acceptance Scenarios**:

1. **Given** both Stage 1 and Stage 2 Wasm binaries exist, **When** I run the fixpoint verification script, **Then** it reports whether the files are byte-identical
2. **Given** Stage 1 and Stage 2 differ, **When** verification runs, **Then** it provides a diff summary showing where the binaries diverge (offset, size difference, section differences)
3. **Given** Stage 1 and Stage 2 are identical, **When** verification succeeds, **Then** it outputs a confirmation message and exits with code 0

---

### User Story 4 - Blocker Reporting (Priority: P1)

As a compiler developer, I want detailed reporting of compilation blockers so that I can systematically address issues preventing fixpoint achievement.

**Why this priority**: Given the current 14% compilation rate, most forms will fail initially. Clear blocker reporting is essential for iterative improvement and is needed concurrently with P1.

**Independent Test**: Can be tested by running compilation on known-failing forms and verifying the report accurately identifies the missing features.

**Acceptance Scenarios**:

1. **Given** Stage 1 compilation fails for a form, **When** blocker reporting runs, **Then** it categorizes the failure (missing feature, type error, unsupported construct)
2. **Given** multiple forms fail with the same root cause, **When** the blocker report generates, **Then** it aggregates failures by category with counts
3. **Given** compilation blockers are identified, **When** the report completes, **Then** it prioritizes blockers by frequency of occurrence

---

### Edge Cases

- What happens when Stage 1 runs out of memory during compilation of large source files?
- How does the system handle circular dependencies in compiler source modules?
- What happens when host shim functions are missing or fail?
- How does Stage 2 generation handle forms that compile successfully but produce invalid Wasm?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Stage 1 Wasm module MUST export a `compile_form` function accessible from the host runtime
- **FR-002**: Stage 1 MUST export all functions required for compilation (symbol table, type system, codegen utilities)
- **FR-003**: The Stage 2 generation process MUST be able to load and invoke Stage 1's compile_form from Node.js
- **FR-004**: The Stage 2 generation script MUST process all Clysm compiler source files in the correct dependency order
- **FR-005**: Generated Stage 2 Wasm MUST pass wasm-tools validation
- **FR-006**: The fixpoint verification script MUST perform byte-level comparison of Stage 1 and Stage 2
- **FR-007**: The blocker report MUST identify and categorize all forms that fail to compile
- **FR-008**: The blocker report MUST include the specific error or missing feature for each failed form
- **FR-009**: Stage 1 compilation MUST handle at minimum the same forms that the current SBCL-hosted compiler handles
- **FR-010**: The system MUST produce deterministic output (same input always yields identical Wasm bytes)

### Key Entities

- **Stage 1**: WebAssembly module generated by SBCL + Clysm host compiler, containing the Clysm compiler
- **Stage 2**: WebAssembly module generated by Stage 1 compiling itself
- **Compilation Form**: A single Lisp expression or definition to be compiled
- **Blocker**: A missing feature, unsupported construct, or error preventing successful compilation
- **Host Shim**: JavaScript functions providing runtime services (I/O, FFI) to Wasm modules

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 exports compile_form and all required helper functions (verifiable by examining module exports)
- **SC-002**: Stage 2 can be generated from Stage 1 without manual intervention (automated script completes successfully)
- **SC-003**: Stage 2 passes wasm-tools validation with exit code 0
- **SC-004**: Fixpoint verification completes and reports comparison result (identical or detailed diff)
- **SC-005**: If fixpoint not achieved, blocker report identifies top 5 most frequent blockers with specific remediation needed
- **SC-006**: Compilation rate for compiler source forms is tracked and reported (current baseline: 14.2%)
- **SC-007**: All scripts exit with appropriate codes (0 for success, non-zero for failure) enabling CI integration

## Assumptions

- Node.js 20+ with WasmGC support is available in the development environment
- Existing host shims (io-shim.js, fs-shim.js) from Phase 13D-8 can be reused or extended
- The order of source file compilation is well-defined and documented
- Deterministic compilation is achievable by controlling all sources of non-determinism (timestamps, random numbers, hash table ordering)
- The current 14% compilation rate represents known blockers that will be systematically addressed

## Dependencies

- Phase 13D-8 (Stage 1 Runtime) must be complete with working host shims
- Stage 1 Wasm module (dist/clysm-stage1.wasm) must be valid and loadable
- wasm-tools must be installed for validation
