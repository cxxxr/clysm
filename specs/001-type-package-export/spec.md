# Feature Specification: Type Constant and Package Primitive Export

**Feature Branch**: `001-type-package-export`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler enhancement system for Clysm that exports type index constants (+TYPE-CONS+, +TYPE-SYMBOL+, +TYPE-STRING+, etc.) and package primitives (packagep*, find-package*, intern*, symbol-package*) to enable Stage 1 self-compilation."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Type Constant Access in Compiled Code (Priority: P1)

As a Clysm compiler developer, I need type index constants (+TYPE-CONS+, +TYPE-SYMBOL+, +TYPE-STRING+, etc.) to be accessible in Wasm-compiled code so that runtime type dispatch and ref.test/ref.cast operations work correctly during Stage 1 self-compilation.

**Why this priority**: Type constants are the foundation of Clysm's runtime type system. Without them, the compiler cannot emit type-checking instructions (ref.test, ref.cast) or construct GC heap objects. This resolves P846 errors which affect 25 compilation failures (2.86% of all errors).

**Independent Test**: Run Stage 1 compilation and verify that functions using +TYPE-CONS+, +TYPE-SYMBOL+, etc. compile without "Unbound variable" errors. Compiled Wasm validates with wasm-tools.

**Acceptance Scenarios**:

1. **Given** code referencing +TYPE-CONS+ constant, **When** compiled to Wasm, **Then** the constant value (2) is inlined as an i32.const instruction
2. **Given** a DEFCONSTANT form defining a type index, **When** encountered during compilation, **Then** the constant is registered and its value resolved at compile-time
3. **Given** all 28 type index constants from gc-types.lisp, **When** Stage 1 generation runs, **Then** zero P846 errors occur for type constants

---

### User Story 2 - Package Predicate Compilation (Priority: P2)

As a Clysm compiler developer, I need the PACKAGEP* type predicate to compile to Wasm so that package type checking works at runtime, enabling Stage 1 to validate package arguments and dispatch correctly.

**Why this priority**: PACKAGEP* is the most frequently failing undefined function (79 occurrences, 9.04% of errors). Many compiler functions depend on package type checking for correct operation. Resolving P951 significantly improves compilation coverage.

**Independent Test**: Compile functions that call PACKAGEP* and verify they produce valid Wasm with proper ref.test instructions for package type checking.

**Acceptance Scenarios**:

1. **Given** a function calling (packagep* x), **When** compiled to Wasm, **Then** the call emits ref.test instruction for package struct type
2. **Given** PACKAGEP* registered in runtime function table, **When** compile-time function lookup occurs, **Then** the runtime dispatch entry is found
3. **Given** Stage 1 generation, **When** completed, **Then** P951 error count drops from 79 to 0

---

### User Story 3 - Package Symbol Operations (Priority: P3)

As a Clysm compiler developer, I need find-package*, intern*, and symbol-package* functions to compile to Wasm so that symbol table operations work correctly in the self-hosted compiler.

**Why this priority**: These functions are required for reader operation and symbol resolution during compilation. While less frequently called than PACKAGEP*, they are essential for any symbol manipulation at runtime.

**Independent Test**: Compile functions using find-package*, intern*, and symbol-package* and verify valid Wasm output with appropriate FFI calls or runtime dispatches.

**Acceptance Scenarios**:

1. **Given** code calling (find-package* "CL"), **When** compiled, **Then** appropriate runtime function call is emitted
2. **Given** code calling (intern* "FOO" pkg), **When** compiled, **Then** Wasm validates and can be instantiated
3. **Given** code calling (symbol-package* sym), **When** compiled, **Then** struct.get instruction extracts package field from symbol

---

### User Story 4 - DEFCONSTANT Compile-Time Resolution (Priority: P2)

As a Clysm compiler developer, I need DEFCONSTANT forms to be properly handled during compilation so that constant values are resolved at compile-time rather than causing runtime lookups or unbound variable errors.

**Why this priority**: Without DEFCONSTANT handling, type index constants and other compiler constants fail to compile. This is a prerequisite for User Story 1 to succeed.

**Independent Test**: Define a DEFCONSTANT and reference it in subsequent code; verify the constant value is inlined during compilation.

**Acceptance Scenarios**:

1. **Given** a DEFCONSTANT form, **When** processed by compiler, **Then** constant name and value are recorded in compilation environment
2. **Given** subsequent reference to defined constant, **When** compiled, **Then** constant value is substituted directly (no variable lookup)
3. **Given** DEFCONSTANT with non-literal expression, **When** processed, **Then** expression is evaluated at compile-time and result stored

---

### Edge Cases

- What happens when a constant is referenced before it is defined? System reports compile-time error with clear message.
- How does system handle redefining a constant with different value? Follow ANSI CL semantics: warn but allow redefinition.
- What happens when PACKAGEP* is called on non-heap object (fixnum, nil)? Returns NIL without error.
- How does intern* handle invalid package designator? Signals appropriate condition.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST export all type index constants (+type-nil+ through +type-mdarray+, indices 0-28) from gc-types.lisp to clysm package
- **FR-002**: Compiler MUST export type index constants from stage0/types.lisp to clysm/stage0 package
- **FR-003**: Compiler MUST handle DEFCONSTANT forms by recording constant bindings in compilation environment
- **FR-004**: Compiler MUST substitute constant values at compile-time when referenced (constant folding)
- **FR-005**: Runtime function table MUST include entry for PACKAGEP* with arity 1
- **FR-006**: Runtime function table MUST include entry for FIND-PACKAGE* with arity 1
- **FR-007**: Runtime function table MUST include entry for INTERN* with variadic arity (1-2 arguments)
- **FR-008**: Runtime function table MUST include entry for SYMBOL-PACKAGE* with arity 1
- **FR-009**: Compiler MUST generate valid WasmGC ref.test instructions for PACKAGEP* calls
- **FR-010**: Compiler MUST emit correct i32.const instructions when type constants are referenced
- **FR-011**: System MUST pass wasm-tools validate on all generated Stage 1 Wasm

### Key Entities

- **Type Index Constant**: Named integer constant mapping type name to WasmGC type section index (e.g., +TYPE-CONS+ = 2)
- **Runtime Function Table**: Compile-time registry mapping function symbols to Wasm function names and arities
- **Package Primitive**: Low-level function for package/symbol operations (packagep*, find-package*, intern*, symbol-package*)
- **Compilation Environment**: State tracking constants, macros, and bindings during compilation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: P846 error pattern (unbound type constants) reduced from 25 occurrences to 0
- **SC-002**: P951 error pattern (undefined PACKAGEP*) reduced from 79 occurrences to 0
- **SC-003**: Stage 1 compilation coverage increases by at least 5 percentage points (from current 21.39%)
- **SC-004**: All generated Stage 1 Wasm passes wasm-tools validate with exit code 0
- **SC-005**: 100% of type index constants (28 total) accessible in compiled code
- **SC-006**: All 4 package primitives (packagep*, find-package*, intern*, symbol-package*) resolve during compilation

## Assumptions

- Type index constants have fixed values that do not change between compilation and runtime
- Package primitives require Wasm-level type checking using ref.test/ref.cast instructions
- Existing runtime function table infrastructure can be extended for new function registrations
- DEFCONSTANT expressions are evaluable at compile-time (no runtime dependencies)
