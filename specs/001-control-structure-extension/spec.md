# Feature Specification: Control Structure Extensions for Wasm Compilation

**Feature Branch**: `001-control-structure-extension`
**Created**: 2025-12-30
**Status**: Draft
**Input**: Phase 13D-6: Control structure extensions for Wasm compilation support in the self-hosting compiler

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Multiple Value Returns (Priority: P1)

As a Clysm compiler developer, I need the compiler to successfully compile code that uses the `values` special form so that functions can return multiple values according to Common Lisp semantics.

**Why this priority**: Multiple value returns are pervasive in Common Lisp code. With 18 compilation failures attributed to `values`, this is the highest-impact improvement for compilation rate.

**Independent Test**: Can be fully tested by compiling a function that uses `values` to return multiple values, then calling it with `multiple-value-bind` and verifying all values are received correctly.

**Acceptance Scenarios**:

1. **Given** source code with a `values` form returning multiple values, **When** compiled to Wasm, **Then** the generated Wasm correctly stores all values for retrieval by the caller
2. **Given** source code where `values` returns zero values, **When** compiled to Wasm, **Then** the generated Wasm correctly signals zero values to the caller
3. **Given** source code where `values` is used in a non-tail position, **When** compiled to Wasm, **Then** the generated Wasm correctly propagates values through the expression context

---

### User Story 2 - Compile Type Declarations (Priority: P1)

As a Clysm compiler developer, I need the compiler to handle `the` type declaration forms so that existing Lisp code with type hints can compile without errors.

**Why this priority**: Type declarations are common in optimized Lisp code. With 18 compilation failures attributed to `the`, this significantly impacts compilation rate. Combined with User Story 1, these two stories address 36 of the 45 failures.

**Independent Test**: Can be fully tested by compiling code containing `the` type declarations and verifying the Wasm compiles successfully and behaves correctly.

**Acceptance Scenarios**:

1. **Given** source code with `(the fixnum x)` type declaration, **When** compiled to Wasm, **Then** the generated Wasm compiles successfully and returns the value of `x`
2. **Given** source code with nested `the` declarations, **When** compiled to Wasm, **Then** the generated Wasm compiles successfully
3. **Given** source code with `the` declaring complex types like `(the (or null cons) x)`, **When** compiled to Wasm, **Then** the generated Wasm compiles successfully

---

### User Story 3 - Compile Mutually Recursive Local Functions (Priority: P2)

As a Clysm compiler developer, I need the compiler to handle `labels` and `flet` forms with mutually recursive local functions so that complex algorithmic code can be compiled to Wasm.

**Why this priority**: Mutual recursion is essential for expressing certain algorithms elegantly. The compiler uses `labels` with mutual recursion in 6 locations that currently fail to compile.

**Independent Test**: Can be fully tested by compiling code with `labels` defining two functions that call each other, then verifying the resulting Wasm correctly executes the mutual recursion.

**Acceptance Scenarios**:

1. **Given** source code with `labels` defining function A that calls function B, and B that calls A, **When** compiled to Wasm, **Then** the generated Wasm correctly resolves both function references
2. **Given** source code with `flet` where an inner function references an outer `labels` function, **When** compiled to Wasm, **Then** the generated Wasm correctly resolves the forward reference
3. **Given** deeply nested `labels`/`flet` forms with complex reference patterns, **When** compiled to Wasm, **Then** all function references resolve correctly

---

### User Story 4 - Compile Exception Handling Code (Priority: P2)

As a Clysm compiler developer, I need the compiler to successfully compile code that uses `handler-case` exception handling constructs so that I can write robust error-handling logic that compiles to Wasm.

**Why this priority**: Exception handling is fundamental to robust software. The compiler itself uses `handler-case` in 3 critical locations, making this essential for self-hosting progress.

**Independent Test**: Can be fully tested by compiling a Lisp form containing `handler-case` and verifying the resulting Wasm module executes correctly, catching and handling errors as specified.

**Acceptance Scenarios**:

1. **Given** source code with a `handler-case` form that catches a specific error type, **When** compiled to Wasm, **Then** the generated Wasm correctly catches errors of that type and executes the handler body
2. **Given** source code with `handler-case` containing multiple handler clauses, **When** compiled to Wasm, **Then** each handler clause correctly matches its specified error type
3. **Given** source code where `handler-case` has no matching handler for a thrown error, **When** the Wasm executes, **Then** the error propagates to outer handlers or terminates appropriately

---

### Edge Cases

- What happens when `handler-case` has no handler clauses (just a protected form)?
- How does the system handle `values` called with dynamic number of arguments via apply?
- What happens when `labels` functions reference free variables from enclosing scope?
- How does `the` interact with type-checked forms like `typecase`?
- What happens when `handler-case` is nested within another `handler-case`?
- How are multiple values handled when returned from within a `handler-case` handler?
- What happens when `values` is called from within a `labels` local function?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST accept `values` special form and generate correct Wasm output
- **FR-002**: Compiler MUST handle `values` with zero, one, or multiple arguments
- **FR-003**: Compiler MUST correctly set multiple value count and buffer for `values` forms
- **FR-004**: Compiler MUST accept `the` type declarations and compile the enclosed expression
- **FR-005**: Compiler MUST pass through `the` forms without requiring runtime type verification
- **FR-006**: Compiler MUST resolve function references in `labels` forms when local functions reference each other (forward references)
- **FR-007**: Compiler MUST resolve mutual recursion in `labels` where functions reference each other
- **FR-008**: Compiler MUST handle `flet`/`labels` nested within each other with correct scoping
- **FR-009**: Compiler MUST transform `handler-case` forms into valid Wasm exception handling constructs
- **FR-010**: Compiler MUST support `handler-case` with multiple handler clauses
- **FR-011**: Compiler MUST support `handler-case` with error variable binding in handlers
- **FR-012**: All generated Wasm MUST pass validation using standard Wasm validation tools

### Key Entities

- **values form**: Multiple value return construct that stores values in the MV buffer and sets MV count
- **the form**: Type declaration wrapper around an expression (compiled as pass-through)
- **labels/flet form**: Local function definition construct with potential mutual recursion requiring forward reference resolution
- **handler-case form**: Exception handling construct with protected form and typed handler clauses

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 18 compiler forms using `values` compile successfully
- **SC-002**: All 18 compiler forms using `the` type declarations compile successfully
- **SC-003**: All 6 compiler forms using `labels` mutual recursion compile successfully
- **SC-004**: All 3 compiler forms using `handler-case` compile successfully
- **SC-005**: Total compilation failures reduced by 45 (from current baseline)
- **SC-006**: Generated Wasm passes validation for all newly supported constructs
- **SC-007**: Existing test suite continues to pass (no regressions)
- **SC-008**: Self-hosting compilation rate increases measurably from current ~23% baseline

## Assumptions

- The existing multiple value infrastructure (mv-count, mv-buffer globals) is sufficient for `values` support
- Type declarations via `the` can be safely passed through without runtime verification
- Forward references in `labels` can be resolved through index pre-allocation or a two-pass approach
- The Wasm runtime environment supports exception handling proposal (try-table/catch) for `handler-case`
- The compiler's existing AST structure can accommodate these control flow extensions

## Dependencies

- Existing codegen infrastructure in compiler
- Existing AST processing infrastructure
- Multiple values support (completed feature 025)
- Wasm exception handling support in target runtime for handler-case

## Out of Scope

- Runtime type verification for `the` declarations (type checking is optional for this phase)
- Full CL condition system (restart-case, restart-bind, handler-bind, etc.)
- Dynamic extent declarations (declare dynamic-extent)
- Optimization based on type declarations
- ignore-errors and other convenience macros (can be built on handler-case)
