# Feature Specification: Primitive Dispatch Table

**Feature Branch**: `001-primitive-dispatch-table`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler enhancement system for Clysm that replaces the monolithic 200+ branch case statement in compile-primitive-call with a hash-table driven dispatch mechanism."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Register New Primitive Compiler (Priority: P1)

A compiler developer wants to add compilation support for a new primitive operation (e.g., a new math function or list operation) without modifying the core dispatch function.

**Why this priority**: This is the primary value proposition - enabling extensibility without touching the monolithic case statement. Every future primitive addition benefits from this capability.

**Independent Test**: Can be fully tested by registering a new primitive compiler and verifying that calls to the new primitive are correctly compiled to the expected output.

**Acceptance Scenarios**:

1. **Given** the dispatch system is initialized, **When** a developer calls the registration API with a symbol, compiler function, and optional flags, **Then** the primitive is registered and immediately available for compilation.
2. **Given** a primitive is registered, **When** the compiler encounters a call to that primitive, **Then** the registered compiler function is invoked with the appropriate arguments.
3. **Given** a primitive is already registered, **When** a developer attempts to re-register with the same symbol, **Then** the new registration replaces the previous one (allowing redefinition during development).

---

### User Story 2 - Compile Standard Primitives via Symbol Lookup (Priority: P1)

A compiler user compiles code that uses standard primitives like `+`, `-`, `cons`, `car`, `cdr`. The primitives should compile correctly using symbol-based lookup.

**Why this priority**: Symbol-based lookup is the most common case, covering all standard Lisp primitives. This must work correctly for backward compatibility.

**Independent Test**: Can be fully tested by compiling expressions using standard primitives and verifying the generated output matches expected behavior.

**Acceptance Scenarios**:

1. **Given** standard primitives are registered in the dispatch table, **When** the compiler processes `(+ 1 2)`, **Then** the addition primitive compiler is invoked and generates correct output.
2. **Given** the dispatch table contains `car` and `cdr` primitives, **When** the compiler processes `(car (cons 1 2))`, **Then** both primitives compile correctly with proper nesting.
3. **Given** a symbol has no registered compiler, **When** the compiler encounters a call to that symbol, **Then** the system falls through to the default compilation path (function call).

---

### User Story 3 - Compile Cross-Package Symbols via String Lookup (Priority: P2)

A compiler user compiles code that uses cross-package internal symbols like `%SETF-AREF` or `MAKE-INSTANCE*`. These symbols may not be directly accessible by symbol identity, so string-based lookup is required.

**Why this priority**: Cross-package symbols are essential for setf expanders and CLOS operations, but occur less frequently than standard primitives.

**Independent Test**: Can be fully tested by registering a primitive with a string key and verifying that calls from different packages correctly dispatch to it.

**Acceptance Scenarios**:

1. **Given** `%SETF-AREF` is registered with string-based lookup, **When** code from any package calls this operation, **Then** the correct compiler is dispatched regardless of which package the call originates from.
2. **Given** a symbol with the same name exists in multiple packages, **When** the compiler looks up by string, **Then** it matches based on the symbol name (not package identity).
3. **Given** both symbol and string lookups are available, **When** looking up a primitive, **Then** the system first attempts symbol lookup (faster) and falls back to string lookup if not found.

---

### User Story 4 - Maintain Backward Compatibility (Priority: P1)

All existing primitives (200+ operations) must continue to compile correctly after the dispatch mechanism change.

**Why this priority**: Breaking existing code is unacceptable. This is a refactoring improvement, not a behavioral change.

**Independent Test**: Can be fully tested by running the existing test suite and verifying all primitive-related tests pass without modification.

**Acceptance Scenarios**:

1. **Given** the new dispatch system is active, **When** the full compiler test suite runs, **Then** all tests that previously passed continue to pass.
2. **Given** code compiled with the old case-statement dispatch, **When** the same code is compiled with the new table dispatch, **Then** the generated output is functionally equivalent.

---

### Edge Cases

- What happens when a primitive name contains special characters (e.g., `1+`, `1-`, `%SETF-*`)? The system must handle these without escaping or encoding issues.
- How does the system handle a primitive registered with both symbol and string keys that conflict? The symbol lookup takes precedence to maintain performance.
- What happens when compiling a recursive primitive call (e.g., `(+ (+ 1 2) 3)`)? Each nested call should independently dispatch to the registered compiler.
- What happens when the dispatch table is empty? The system should gracefully fall through to default function call compilation.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a dispatch table data structure that maps operation identifiers to compiler functions.
- **FR-002**: System MUST support symbol-based lookup for standard primitive operations.
- **FR-003**: System MUST support string-based lookup for cross-package symbols when symbol lookup fails.
- **FR-004**: System MUST provide a registration API that accepts: operation identifier (symbol or string), compiler function, and optional flags.
- **FR-005**: System MUST allow primitive compilers to be registered at load time (declaratively) and at runtime (programmatically).
- **FR-006**: System MUST dispatch to the registered compiler function when the corresponding primitive is encountered during compilation.
- **FR-007**: System MUST fall through to the default function call path when no registered compiler is found.
- **FR-008**: System MUST preserve the exact behavior of all 200+ existing primitives after migration.
- **FR-009**: System MUST support re-registration of primitives (for development/debugging purposes).
- **FR-010**: System MUST provide a way to query which primitives are currently registered.

### Key Entities

- **Dispatch Table**: Central registry mapping operation identifiers to compiler functions. Supports both symbol and string keys.
- **Primitive Compiler**: A function that generates compiled output for a specific primitive operation. Receives the call form and compilation context.
- **Registration Entry**: Contains the operation identifier, compiler function reference, and optional flags (e.g., arity hints, optimization flags).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing primitive-related tests pass without modification after the dispatch mechanism change.
- **SC-002**: New primitive compilers can be added by registering a single function call, without modifying the dispatch function.
- **SC-003**: Lookup time for primitives remains constant regardless of the number of registered primitives (hash-table O(1) behavior).
- **SC-004**: The monolithic case statement (200+ branches) is completely replaced by table-driven dispatch.
- **SC-005**: Cross-package symbols (e.g., `%SETF-AREF`) dispatch correctly when called from any package.
- **SC-006**: Developer can register, query, and redefine primitive compilers during interactive development.

## Assumptions

- The existing 200+ primitives have compiler functions that can be extracted from the case statement branches without modification.
- Symbol identity comparison is faster than string comparison and should be preferred when possible.
- The compilation context passed to primitive compilers contains all necessary information for code generation.
- Re-registration is permitted for development flexibility; production deployments may choose to disallow it.
