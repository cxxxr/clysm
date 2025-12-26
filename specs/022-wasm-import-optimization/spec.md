# Feature Specification: Wasm Import Optimization

**Feature Branch**: `022-wasm-import-optimization`
**Created**: 2025-12-25
**Status**: Draft
**Input**: User description: "Clysm Wasm モジュールのホスト環境互換性"

## Background

Clysm is a compiler that transforms Common Lisp code into WebAssembly. The generated Wasm modules are executed in runtimes such as wasmtime or Node.js.

Currently, all compiled Wasm modules declare dependencies on external I/O functions even when the source code does not use any I/O operations. This prevents simple expressions like `(+ 1 2)` from being executed in standard Wasm runtimes, causing "unknown import" errors at instantiation time.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Execute Simple Arithmetic (Priority: P1)

As a developer, I want to compile arithmetic-only Lisp expressions and run them directly in wasmtime without any additional configuration or shims.

**Why this priority**: This is the foundational capability that unblocks all other use cases. Without this, no tests can pass.

**Independent Test**: Compile `(+ 1 2)` with Clysm and execute the resulting Wasm with `wasmtime --invoke _start module.wasm`. Expect return value 3.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(+ 1 2)` without I/O, **When** compiled and executed with wasmtime, **Then** the runtime does not report unknown import errors and returns the correct value (3)
2. **Given** a Lisp expression `(* 7 6)` without I/O, **When** compiled and executed with wasmtime, **Then** the execution succeeds and returns 42
3. **Given** a Lisp expression `(- 100 58)` without I/O, **When** compiled and executed with wasmtime, **Then** the execution succeeds and returns 42

---

### User Story 2 - Run ANSI Compliance Tests (Priority: P1)

As a developer, I want to run the ANSI Common Lisp test suite and achieve measurable pass rates for basic categories.

**Why this priority**: ANSI compliance testing validates the compiler's correctness. Currently 0% pass rate blocks quality assurance.

**Independent Test**: Run `(run-ansi-tests :category "numbers")` and verify at least 10% of tests show PASS status.

**Acceptance Scenarios**:

1. **Given** the numbers test category, **When** tests are executed, **Then** at least 10% of executable tests pass
2. **Given** the cons test category, **When** tests are executed, **Then** at least 5% of executable tests pass
3. **Given** any test that does not use I/O, **When** executed, **Then** the test completes without import-related errors

---

### User Story 3 - Preserve I/O Functionality (Priority: P2)

As a developer, I want code that uses I/O operations to continue working in appropriate host environments with shims.

**Why this priority**: Existing I/O functionality must not break. Backward compatibility is essential.

**Independent Test**: Compile a Lisp expression using `print` and execute with the existing host-shim. Verify output appears correctly.

**Acceptance Scenarios**:

1. **Given** a Lisp expression using `(print "hello")`, **When** compiled and executed with host-shim, **Then** "hello" is written to standard output
2. **Given** a Lisp expression using `(read-line)`, **When** compiled and executed with host-shim, **Then** input is correctly read from standard input
3. **Given** existing code that relies on I/O imports, **When** recompiled, **Then** behavior remains unchanged

---

### Edge Cases

- What happens when a Lisp expression indirectly requires I/O through function calls?
  - **Resolution**: Only direct I/O usage triggers I/O imports. Indirect calls through undefined functions are not analyzed.
- How does the system handle mixed code with both I/O and non-I/O paths?
  - **Resolution**: If any path uses I/O, the module includes I/O imports. The shim is required for execution.
- What happens when wasmtime is invoked on a module that requires I/O imports?
  - **Resolution**: Execution fails with a clear error message indicating which imports are missing and suggesting use of the shim.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST NOT emit I/O import declarations for modules that do not use I/O operations
- **FR-002**: Compiler MUST emit I/O import declarations for modules that use I/O operations (print, read, format, etc.)
- **FR-003**: Compiled modules without I/O dependencies MUST execute successfully in wasmtime without additional configuration
- **FR-004**: Compiled modules with I/O dependencies MUST continue to work with the existing host-shim
- **FR-005**: The build system MUST NOT require changes to execute non-I/O modules
- **FR-006**: Error messages from wasmtime MUST clearly indicate when a shim is required for I/O operations

### Key Entities

- **Wasm Module**: The compiled output containing functions, types, and import/export declarations
- **Import Declaration**: A statement in the Wasm module declaring required external functions
- **I/O Import**: Specific imports for `clysm:io::write-char`, `clysm:io::write-string`, `clysm:io::read-char`, `clysm:io::read-line`
- **Host Shim**: A runtime environment (Node.js-based) that provides I/O function implementations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Arithmetic expressions (`(+ 1 2)`, `(* 3 4)`, etc.) execute successfully in wasmtime without shims
- **SC-002**: ANSI numbers test category achieves at least 10% pass rate (currently 0%)
- **SC-003**: ANSI cons test category achieves at least 5% pass rate (currently 0%)
- **SC-004**: Existing I/O-using code continues to work with the host-shim (no regression)
- **SC-005**: Module compilation time does not increase by more than 10%
- **SC-006**: Generated Wasm module size for non-I/O code is smaller or equal to current size

## Assumptions

- The compiler has sufficient information at compile time to determine whether I/O operations are used
- Standard library functions are statically known and their I/O requirements can be pre-classified
- The host-shim implementation is correct and only needs modules with appropriate imports

## Out of Scope

- Adding new I/O capabilities
- Performance optimization beyond what's needed for this feature
- Supporting new Wasm runtimes
- Modifying the host-shim implementation
