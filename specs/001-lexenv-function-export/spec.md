# Feature Specification: Lexical Environment Function Export System

**Feature Branch**: `001-lexenv-function-export`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Implement a lexical environment function export system for the Clysm WebAssembly GC compiler. The system needs to export internal compiler functions (env-add-local, env-lookup, make-lexical-env, loop-keyword-eq, numeric-literal-p) to the public clysm package, enabling Stage 1 compilation to access these functions. The goal is to reduce compilation errors from 21.43% to 25%+ by eliminating the ENV-ADD-LOCAL undefined function pattern (currently 118 errors, 13.52% of failures). All exported functions must be registered in the runtime function table for proper dispatch. Success is verified by regenerating the Stage 1 report and confirming error reduction."

## User Scenarios & Testing

### User Story 1 - Export Internal Functions to Public Package (Priority: P1)

As a compiler developer working on Stage 1 bootstrap, I need the internal lexical environment functions to be accessible from the public clysm package so that the Stage 1 compiler can resolve references to these functions during compilation.

**Why this priority**: This is the core requirement. Without exporting these functions, the Stage 1 compilation cannot proceed past the undefined function errors, blocking all downstream work.

**Independent Test**: Can be fully tested by attempting to call each exported function from code that only imports the clysm package, verifying the functions are accessible and callable.

**Acceptance Scenarios**:

1. **Given** the function `env-add-local` is defined internally, **When** a Stage 1 form references `clysm:env-add-local`, **Then** the symbol resolves to the internal function definition.
2. **Given** the function `env-lookup` is defined internally, **When** a Stage 1 form references `clysm:env-lookup`, **Then** the symbol resolves to the internal function definition.
3. **Given** the function `make-lexical-env` is defined internally, **When** a Stage 1 form references `clysm:make-lexical-env`, **Then** the symbol resolves to the internal function definition.
4. **Given** the function `loop-keyword-eq` is defined internally, **When** a Stage 1 form references `clysm:loop-keyword-eq`, **Then** the symbol resolves to the internal function definition.
5. **Given** the function `numeric-literal-p` is defined internally, **When** a Stage 1 form references `clysm:numeric-literal-p`, **Then** the symbol resolves to the internal function definition.

---

### User Story 2 - Register Functions in Runtime Function Table (Priority: P2)

As a compiler developer, I need all exported functions to be registered in the runtime function table so that the compiler's dispatch mechanism can generate proper Wasm code for calls to these functions.

**Why this priority**: Registration in the runtime function table is required for the compiler to generate correct call sequences. Without registration, even exported functions would fail during code generation.

**Independent Test**: Can be tested by querying the runtime function table for each function name and verifying entries exist with correct dispatch metadata.

**Acceptance Scenarios**:

1. **Given** `env-add-local` is exported, **When** the runtime function table is queried for `env-add-local`, **Then** the table returns a valid entry with dispatch information.
2. **Given** all five functions are exported, **When** Stage 1 compilation encounters a call to any of these functions, **Then** the compiler generates valid Wasm call instructions.

---

### User Story 3 - Verify Compilation Error Reduction (Priority: P3)

As a compiler developer, I need to verify that exporting these functions reduces the undefined function errors, measured by regenerating the Stage 1 report and confirming error count reduction.

**Why this priority**: Verification confirms the feature achieves its goal. This can only be tested after P1 and P2 are complete.

**Independent Test**: Can be tested by running the Stage 1 generation process, capturing the report, and comparing error counts against baseline.

**Acceptance Scenarios**:

1. **Given** the baseline has 118 ENV-ADD-LOCAL undefined function errors, **When** Stage 1 is regenerated after exporting the functions, **Then** ENV-ADD-LOCAL undefined function errors are eliminated (count = 0).
2. **Given** the baseline Stage 1 coverage is 21.43%, **When** Stage 1 is regenerated after exporting the functions, **Then** the coverage percentage increases to at least 25%.
3. **Given** Stage 1 is regenerated, **When** the output Wasm is validated, **Then** wasm-tools validate passes with exit code 0.

---

### Edge Cases

- What happens when a function is exported but has dependencies on other unexported internal functions? The system should export required dependencies or ensure the function operates independently.
- How does the system handle functions with identical names in different internal packages? The export must specify which internal definition takes precedence.
- What happens if a function is already exported under a different name? The system should detect and warn about potential symbol conflicts.

## Requirements

### Functional Requirements

- **FR-001**: System MUST export the function `env-add-local` to the clysm package.
- **FR-002**: System MUST export the function `env-lookup` to the clysm package.
- **FR-003**: System MUST export the function `make-lexical-env` to the clysm package.
- **FR-004**: System MUST export the function `loop-keyword-eq` to the clysm package.
- **FR-005**: System MUST export the function `numeric-literal-p` to the clysm package.
- **FR-006**: System MUST register each exported function in the runtime function table (`*runtime-function-table*`).
- **FR-007**: System MUST preserve the original function signatures (parameter count and types) when exporting.
- **FR-008**: System MUST ensure exported functions are callable from Stage 1 compiled code.
- **FR-009**: System MUST not break existing functionality that depends on these internal functions.

### Key Entities

- **Lexical Environment Functions**: The five internal compiler functions (`env-add-local`, `env-lookup`, `make-lexical-env`, `loop-keyword-eq`, `numeric-literal-p`) that manage lexical scope during compilation.
- **Runtime Function Table**: A dispatch table (`*runtime-function-table*`) that maps function names to their Wasm call generation metadata.
- **clysm Package**: The public package that exposes the compiler's user-facing API and must now include these internal functions.
- **Stage 1 Compiler**: The self-hosted compiler generated by compiling Clysm with itself, which requires access to these functions.

## Success Criteria

### Measurable Outcomes

- **SC-001**: ENV-ADD-LOCAL undefined function error pattern is eliminated (118 errors reduced to 0 errors).
- **SC-002**: Stage 1 compilation coverage increases from 21.43% to at least 25%.
- **SC-003**: All five exported functions are accessible via `clysm:function-name` syntax.
- **SC-004**: All five exported functions appear in the runtime function table with valid dispatch entries.
- **SC-005**: Generated Stage 1 Wasm binary passes validation (exit code 0).
- **SC-006**: No regression in existing Stage 1 compilation (forms that previously compiled successfully continue to compile).

## Assumptions

- The internal functions currently reside in an internal compiler package (e.g., `clysm.compiler`).
- The runtime function table mechanism established in prior phases (`001-io-list-runtime`) is the correct pattern to follow.
- The functions have stable signatures that do not require modification for export.
- The 118 ENV-ADD-LOCAL errors are the primary blockers addressed by this feature; other undefined function errors may remain.
