# Feature Specification: func-section.lisp Refactoring

**Feature Branch**: `001-func-section-refactor`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Refactor func-section.lisp: dispatch table, runtime migration, code efficiency"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compiler Maintainer Adds New Primitive (Priority: P1)

A compiler developer needs to add a new primitive function to the Clysm compiler. Currently, this requires modifying a 363-line dispatch function with 200+ case branches.

**Why this priority**: This is the most frequent maintenance task and the current architecture makes it error-prone and time-consuming. Every new primitive risks breaking existing functionality.

**Independent Test**: Can be tested by adding a test primitive function and verifying it compiles correctly, without modifying existing dispatch logic.

**Acceptance Scenarios**:

1. **Given** a new primitive function to add, **When** the developer registers it via the primitive registry, **Then** the function becomes available for compilation without modifying existing dispatch code
2. **Given** a registered primitive, **When** the compiler processes a call to that primitive, **Then** it correctly dispatches to the registered compiler function
3. **Given** an unregistered primitive, **When** the compiler encounters it, **Then** it reports a clear error message identifying the unknown primitive

---

### User Story 2 - Runtime Function Migration (Priority: P2)

A compiler developer wants to move complex standard library functions (string operations, numeric conversions, sequence operations) from inline Wasm generation to Lisp runtime library implementations.

**Why this priority**: This is the primary method for reducing func-section.lisp complexity while improving code quality. Runtime implementations are easier to test, debug, and maintain.

**Independent Test**: Can be tested by migrating one function family (e.g., string-trim functions) to runtime and verifying all existing tests pass.

**Acceptance Scenarios**:

1. **Given** a function registered in the runtime function table, **When** the compiler encounters a call to that function, **Then** it generates a call to the runtime implementation instead of inline Wasm
2. **Given** a runtime function with keyword arguments, **When** called with various argument combinations, **Then** all ANSI CL semantics are preserved
3. **Given** a migrated function, **When** Stage 1 compilation runs, **Then** the compilation rate does not decrease

---

### User Story 3 - Code Generation Efficiency Improvement (Priority: P3)

A compiler developer notices that list construction patterns use O(n²) append operations, causing performance issues during compilation of large modules.

**Why this priority**: Performance optimization enables faster development cycles but is less critical than architectural improvements.

**Independent Test**: Can be tested by measuring compilation time before and after the optimization on a representative module.

**Acceptance Scenarios**:

1. **Given** instruction collection using the new pattern, **When** generating n instructions, **Then** the time complexity is O(n) not O(n²)
2. **Given** existing compilation code, **When** converted to the new pattern, **Then** all existing tests continue to pass
3. **Given** the new instruction collector macro, **When** used in compilation functions, **Then** the generated Wasm is identical to the previous approach

---

### User Story 4 - cXXr Function Consolidation (Priority: P3)

A compiler developer notices 12 nearly-identical compile-cXXr functions that differ only in the car/cdr operation sequence.

**Why this priority**: Code deduplication reduces maintenance burden but has lower immediate impact than architectural changes.

**Independent Test**: Can be tested by verifying all cXXr operations (caar, cadr, cdar, cddr, etc.) compile and execute correctly.

**Acceptance Scenarios**:

1. **Given** any cXXr function call, **When** compiled using the consolidated generator, **Then** the output is identical to the previous implementation
2. **Given** the macro-based cXXr definition, **When** new cXXr functions are needed, **Then** they can be added in one line each
3. **Given** the compile-cxr-chain function, **When** called during compilation, **Then** it is properly accessible (resolving error pattern P626)

---

### User Story 5 - Equality Function Unification (Priority: P2)

A compiler developer needs to add support for a new type in equality comparisons. Currently, this requires modifying 4 separate functions totaling 800+ lines.

**Why this priority**: Equality predicates are core compiler functionality. Unifying them reduces the risk of inconsistencies and simplifies type system extensions.

**Independent Test**: Can be tested by adding a new type to the unified equality framework and verifying eq/eql/equal/equalp all handle it correctly.

**Acceptance Scenarios**:

1. **Given** the unified equality predicate, **When** comparing values at different levels (eq, eql, equal, equalp), **Then** ANSI CL semantics are preserved for all types
2. **Given** a new type added to the type dispatch table, **When** equality predicates are invoked, **Then** the new type is handled correctly in all four predicates
3. **Given** the refactored equality code, **When** running the 024-equality-predicates test suite, **Then** all tests pass

---

### Edge Cases

- What happens when a primitive is registered twice with different implementations?
- How does the system handle primitives that require special argument processing (like variadic functions)?
- What happens when a runtime function calls another runtime function that hasn't been registered yet?
- How does the system behave when migrating a function that has Wasm-specific optimizations?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a primitive dispatch table mechanism using hash tables for O(1) lookup
- **FR-002**: System MUST support registration of primitive compilers by symbol and by name (for %SETF-* prefixed operations)
- **FR-003**: System MUST support runtime function table for dispatching to Lisp implementations instead of inline Wasm
- **FR-004**: System MUST provide efficient instruction collection replacing O(n²) append patterns with O(n) push+nreverse
- **FR-005**: System MUST provide macro-based generation for cXXr function compilers
- **FR-006**: System MUST provide unified type dispatch infrastructure for equality predicates
- **FR-007**: System MUST maintain backward compatibility with all existing compilation functionality
- **FR-008**: System MUST preserve ANSI Common Lisp semantics for all migrated functions
- **FR-009**: System MUST report clear errors when encountering unregistered primitives

### Key Entities

- **Primitive Dispatch Table**: Hash table mapping operation symbols to compiler functions
- **Runtime Function Table**: Registry of functions implemented in Lisp runtime rather than inline Wasm
- **Instruction Collector**: Macro-based mechanism for efficient Wasm instruction accumulation
- **Type Dispatch Infrastructure**: Shared framework for generating type-checking code across equality predicates

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: func-section.lisp reduces from 16,483 lines to under 8,000 lines (>50% reduction)
- **SC-002**: All existing tests pass after each refactoring phase
- **SC-003**: Stage 1 Wasm generation succeeds with validation passing
- **SC-004**: Compilation rate maintains or improves from current 19.20% baseline
- **SC-005**: Adding a new primitive requires modifying only the registration code, not dispatch logic
- **SC-006**: Error patterns P457 (COMPILE-UNARY-MATH-FFI), P626 (COMPILE-CXR-CHAIN), and P334 (REGISTER-RUNTIME-FUNCTION) are eliminated
- **SC-007**: Developer can migrate a function to runtime in under 5 minutes with clear documentation

## Assumptions

- The existing test suite adequately covers the functionality being refactored
- Runtime function implementations will have equivalent or better performance than inline Wasm for complex functions
- The hash table lookup overhead is negligible compared to the compilation work performed
- All functions being migrated to runtime have pure Lisp implementations that don't require Wasm-specific optimizations
- The Stage 1 compilation infrastructure remains stable during this refactoring

## Constraints

- Must maintain full backward compatibility with existing compiler behavior
- Must not regress the current Stage 1 compilation rate
- Must preserve exact ANSI CL semantics for all standard functions
- Each refactoring phase must be independently deployable and testable
