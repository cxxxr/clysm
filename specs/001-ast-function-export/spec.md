# Feature Specification: AST Function Export System

**Feature Branch**: `001-ast-function-export`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler enhancement system for Clysm that exports internal AST manipulation functions to the main clysm package"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Clysm Compiler Code Using AST Functions (Priority: P1)

As a compiler developer bootstrapping Clysm, I need internal AST manipulation functions to be available in the main clysm package so that compiler source code that references these functions can compile to Wasm without "undefined function" errors.

**Why this priority**: This is the core enabler for self-hosting. Without these functions exported, the compiler cannot compile itself, blocking the entire bootstrap pipeline.

**Independent Test**: Can be fully tested by running Stage 1 generation and verifying that forms using these functions compile successfully instead of producing P944/P321/P543/P106 error patterns.

**Acceptance Scenarios**:

1. **Given** a Lisp form that calls `compile-to-instructions`, **When** Stage 1 compilation runs, **Then** the form compiles successfully with proper Wasm function dispatch
2. **Given** a Lisp form that calls `make-wasm-struct-type`, **When** Stage 1 compilation runs, **Then** the form compiles successfully
3. **Given** a Lisp form that calls `ast-literal-value` or `ast-literal-p`, **When** Stage 1 compilation runs, **Then** the form compiles successfully
4. **Given** a Lisp form that calls `get-numeric-value`, **When** Stage 1 compilation runs, **Then** the form compiles successfully

---

### User Story 2 - Runtime Function Dispatch for Exported Functions (Priority: P1)

As a compiler developer, I need the exported functions to be registered in the runtime function table with correct arity information so that Wasm dispatch generates valid call instructions.

**Why this priority**: Function availability alone is insufficient; the functions must be properly registered for Wasm code generation to work correctly.

**Independent Test**: Can be tested by verifying that calling each exported function from compiled Wasm code produces valid Wasm bytecode that passes validation.

**Acceptance Scenarios**:

1. **Given** `compile-to-instructions` is called with 2 arguments (form and environment), **When** Wasm is generated, **Then** the call uses the correct arity (2)
2. **Given** `make-wasm-struct-type` is called with its required arguments, **When** Wasm is generated, **Then** the call dispatches correctly through the runtime function table
3. **Given** predicate functions (`wasm-struct-type-p`, `ast-literal-p`) are called, **When** Wasm is generated, **Then** the calls use arity 1

---

### User Story 3 - Unit Test Verification (Priority: P2)

As a compiler developer, I need unit tests that verify the function exports and dispatch so that I can confidently make changes without breaking the export system.

**Why this priority**: Tests provide safety net for ongoing development, but the core functionality must work first.

**Independent Test**: Can be tested by running the unit test suite and verifying all new tests pass.

**Acceptance Scenarios**:

1. **Given** the test suite includes function availability tests, **When** tests run, **Then** all exported functions are accessible from the clysm package
2. **Given** the test suite includes dispatch tests, **When** tests run, **Then** all functions are registered with correct arity in `*runtime-function-table*`

---

### Edge Cases

- What happens when an exported function is called with wrong arity? The system should produce a compile-time error indicating arity mismatch.
- How does the system handle functions that already exist in clysm package? Existing exports should be preserved without duplication.
- What happens if a function's internal implementation changes signature? Runtime table entries must be updated to match.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST export `compile-to-instructions` function to the clysm package
- **FR-002**: System MUST register `compile-to-instructions` in `*runtime-function-table*` with arity 2 (form, environment)
- **FR-003**: System MUST export `make-wasm-struct-type` function to the clysm package
- **FR-004**: System MUST export `wasm-struct-type-p` predicate function to the clysm package
- **FR-005**: System MUST export `wasm-struct-type-fields` accessor function to the clysm package
- **FR-006**: System MUST export `make-ast-literal` constructor function to the clysm package
- **FR-007**: System MUST export `ast-literal-value` accessor function to the clysm package
- **FR-008**: System MUST export `ast-literal-p` predicate function to the clysm package
- **FR-009**: System MUST export `get-numeric-value` function to the clysm package
- **FR-010**: System MUST register all exported functions in `*runtime-function-table*` with correct arity information for Wasm dispatch
- **FR-011**: System MUST include unit tests verifying function availability from clysm package
- **FR-012**: System MUST include unit tests verifying correct arity registration in runtime function table

### Key Entities

- **Runtime Function Table Entry**: Registration of an exported function including name symbol, implementation function, and arity specification
- **AST Literal**: Compiler data structure representing literal values in the AST (numbers, strings, etc.)
- **Wasm Struct Type**: Compiler data structure defining GC types for WasmGC code generation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 error pattern P944 (undefined `compile-to-instructions`) reduced to zero occurrences
- **SC-002**: Stage 1 error pattern P321 (undefined `make-wasm-struct-type`) reduced to zero occurrences
- **SC-003**: Stage 1 error pattern P543 (undefined `ast-literal-*` functions) reduced to zero occurrences
- **SC-004**: Stage 1 error pattern P106 (undefined `get-numeric-value`) reduced to zero occurrences
- **SC-005**: Stage 1 compilation rate increases from current 19% to 25% or higher
- **SC-006**: Generated Wasm passes validation with exit code 0
- **SC-007**: All new unit tests pass (100% pass rate)

## Assumptions

- The internal implementations of these functions already exist in the compiler codebase
- The `*runtime-function-table*` mechanism from previous features (001-io-list-runtime) is functional
- Error patterns P944, P321, P543, P106 are currently blocking compilation of forms that use these functions
- Exporting these functions will not introduce circular dependencies in the package structure
