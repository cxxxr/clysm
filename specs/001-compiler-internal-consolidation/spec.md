# Feature Specification: Compiler Internal Function Consolidation

**Feature Branch**: `001-compiler-internal-consolidation`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Implement a compiler internal function consolidation system for the Clysm WebAssembly GC compiler..."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Environment Management Compilation (Priority: P1)

When the compiler encounters lexical variable bindings during Stage 1 compilation, it must have access to environment management functions (ENV-ADD-LOCAL, ENV-LOOKUP, ENV-ADD-CLOSURE-VAR) that can themselves be compiled to Wasm. Currently these functions trigger compilation errors because they are undefined in the Wasm-compilable context.

**Why this priority**: This is the most frequent error pattern blocking Stage 1 compilation. ENV-ADD-LOCAL appears in the top blockers list, and resolving it will have the highest impact on compilation rate improvement.

**Independent Test**: Can be tested by compiling a single DEFUN that uses let-binding with local variables and verifying the generated Wasm validates successfully.

**Acceptance Scenarios**:

1. **Given** a DEFUN form containing let-bindings, **When** Stage 1 compilation runs, **Then** the environment management calls compile without error
2. **Given** a nested lambda with closure variables, **When** Stage 1 compilation runs, **Then** ENV-ADD-CLOSURE-VAR calls compile to valid Wasm
3. **Given** a variable reference inside a function body, **When** Stage 1 compilation runs, **Then** ENV-LOOKUP calls resolve correctly

---

### User Story 2 - Instruction Compilation Dispatch (Priority: P1)

When the compiler needs to convert AST nodes to Wasm instructions, it must dispatch through COMPILE-TO-INSTRUCTIONS. This central function handles all special forms (if, let, lambda, progn) and delegates to specialized compilers. Currently, calls to COMPILE-TO-INSTRUCTIONS fail because the function is undefined in the Wasm context.

**Why this priority**: COMPILE-TO-INSTRUCTIONS is the second-most common error pattern. It is called from multiple locations throughout the compiler, making it critical infrastructure.

**Independent Test**: Can be tested by compiling a simple DEFUN with an if-expression and verifying the dispatch produces valid Wasm.

**Acceptance Scenarios**:

1. **Given** an AST node representing an if-expression, **When** COMPILE-TO-INSTRUCTIONS is called, **Then** it dispatches to the conditional compiler and returns valid instructions
2. **Given** an AST node for a let-binding, **When** COMPILE-TO-INSTRUCTIONS is called, **Then** it handles local variable setup correctly
3. **Given** a lambda expression AST, **When** COMPILE-TO-INSTRUCTIONS is called, **Then** it generates closure creation code

---

### User Story 3 - Package System FFI Integration (Priority: P2)

When the compiler encounters package operations (finding packages, interning symbols), it must have Wasm-compilable stubs (PACKAGEP*, FIND-PACKAGE*, INTERN*) that delegate to the host runtime via FFI. These stubs allow package operations to occur at runtime without requiring full package system implementation in Wasm.

**Why this priority**: Package operations are common in compiler code but can be handled via FFI rather than native implementation, making this a high-value, lower-effort improvement.

**Independent Test**: Can be tested by compiling a DEFUN that calls intern and verifying the FFI call is generated correctly.

**Acceptance Scenarios**:

1. **Given** a call to INTERN* in source code, **When** compiled to Wasm, **Then** it generates an FFI call to the host runtime
2. **Given** a call to FIND-PACKAGE*, **When** compiled, **Then** it produces valid Wasm that invokes the host package lookup
3. **Given** a PACKAGEP* type check, **When** compiled, **Then** it generates appropriate FFI-backed predicate code

---

### User Story 4 - Wasm Type Definition Generation (Priority: P2)

When the compiler needs to define new struct types for WasmGC (closures, instances, custom data structures), it must use MAKE-WASM-STRUCT-TYPE and related functions. These functions generate the type section entries and field definitions required by the WasmGC specification.

**Why this priority**: Type construction is needed for advanced features but existing type indices handle most cases. This enables future extensibility more than current blocking issues.

**Independent Test**: Can be tested by defining a new struct type and verifying the type section of the generated Wasm contains the correct definition.

**Acceptance Scenarios**:

1. **Given** a request to create a new struct type with specified fields, **When** MAKE-WASM-STRUCT-TYPE is called, **Then** it registers the type and returns a valid type index
2. **Given** field specifications including mutability and types, **When** the type is constructed, **Then** the Wasm type section reflects these properties correctly
3. **Given** a type with reference fields, **When** compiled, **Then** the generated type uses appropriate WasmGC reference types

---

### User Story 5 - Runtime Library Function Migration (Priority: P3)

The list manipulation functions (assoc, member, find, position) should be migrated from inline Wasm emission in func-section.lisp to runtime library calls in list-runtime.lisp. This consolidates implementation, reduces code duplication, and simplifies maintenance.

**Why this priority**: This is a refactoring task that improves code quality but does not directly increase compilation rate. It can be done after the critical infrastructure is in place.

**Independent Test**: Can be tested by calling assoc in compiled code and verifying it invokes the runtime library function rather than inline emission.

**Acceptance Scenarios**:

1. **Given** a call to assoc in source code, **When** compiled, **Then** it generates a call to the runtime library function
2. **Given** the old compile-assoc function, **When** the migration is complete, **Then** the function is removed from func-section.lisp
3. **Given** all four list functions migrated, **When** compiling code that uses them, **Then** behavior is identical to the previous inline implementation

---

### Edge Cases

- What happens when ENV-LOOKUP cannot find a variable? Must signal an appropriate compile-time error.
- How does COMPILE-TO-INSTRUCTIONS handle unknown AST node types? Must fall back gracefully or signal a clear error.
- What happens when FFI calls for package operations fail at runtime? Must propagate host errors appropriately.
- How does type construction handle duplicate type requests? Must return existing type index rather than creating duplicates.

## Requirements *(mandatory)*

### Functional Requirements

#### Environment Management

- **FR-001**: System MUST define ENV-ADD-LOCAL function that adds a local variable binding to the compilation environment
- **FR-002**: System MUST define ENV-LOOKUP function that retrieves variable binding information from the environment
- **FR-003**: System MUST define ENV-ADD-CLOSURE-VAR function that registers variables captured in closures
- **FR-004**: All environment functions MUST be compilable to valid WasmGC code

#### Instruction Compilation

- **FR-005**: System MUST implement COMPILE-TO-INSTRUCTIONS as the central AST-to-Wasm dispatch function
- **FR-006**: COMPILE-TO-INSTRUCTIONS MUST handle special forms: if, let, let*, lambda, progn, block, return-from
- **FR-007**: COMPILE-TO-INSTRUCTIONS MUST delegate to specialized compilers for each form type
- **FR-008**: The function MUST be compilable to WasmGC and callable from compiled code

#### Package System Stubs

- **FR-009**: System MUST provide PACKAGEP* predicate stub that checks if an object is a package via FFI
- **FR-010**: System MUST provide FIND-PACKAGE* stub that looks up packages by name via FFI
- **FR-011**: System MUST provide INTERN* stub that interns symbols in packages via FFI
- **FR-012**: All package stubs MUST use the existing FFI infrastructure (feature 027)

#### Type Construction

- **FR-013**: System MUST implement MAKE-WASM-STRUCT-TYPE for creating new WasmGC struct types
- **FR-014**: Type construction MUST support field definitions with type and mutability specifications
- **FR-015**: System MUST return valid type indices usable in struct.new and ref.cast instructions
- **FR-016**: Type definitions MUST integrate with the existing type index scheme (see CLAUDE.md WasmGC Type Indices)

#### Runtime Migration

- **FR-017**: System MUST migrate assoc, member, find, position to runtime library calls
- **FR-018**: System MUST remove compile-assoc, compile-member and related inline emission functions from func-section.lisp
- **FR-019**: Migrated functions MUST maintain identical behavior to previous inline implementations
- **FR-020**: Runtime calls MUST use the *runtime-function-table* dispatch mechanism

#### Regression Prevention

- **FR-021**: All forms currently compiling successfully (5632+) MUST continue to compile after changes
- **FR-022**: Generated Wasm MUST pass wasm-tools validate
- **FR-023**: Stage 1 compilation MUST complete without hanging or crashing

### Key Entities

- **Compilation Environment**: A nested structure tracking lexical variable bindings, their types, and their locations (local index or closure slot)
- **AST Node**: Abstract syntax tree representation of Lisp forms, used as input to COMPILE-TO-INSTRUCTIONS
- **Wasm Instruction Sequence**: Ordered list of WasmGC instructions generated by compilation
- **Type Index**: Integer reference to a WasmGC type definition in the module's type section
- **Runtime Function Entry**: Mapping from function name to its implementation in the runtime library

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 compilation rate increases from 22% to 30% or higher (measured by compiled forms / total forms)
- **SC-002**: ENV-ADD-LOCAL error pattern is eliminated from the top blockers list in dist/stage1-report.json
- **SC-003**: COMPILE-TO-INSTRUCTIONS error pattern is eliminated from the top blockers list
- **SC-004**: All 5632+ currently-compiling forms continue to compile successfully (zero regressions)
- **SC-005**: Generated dist/clysm-stage1.wasm passes wasm-tools validate with exit code 0
- **SC-006**: Stage 1 Wasm file size remains reasonable (within 2x of current 24.5KB baseline)
- **SC-007**: Runtime library contains implementations for assoc, member, find, position (verified by code review)
- **SC-008**: func-section.lisp no longer contains compile-assoc, compile-member inline emission functions

## Assumptions

- The existing FFI infrastructure (feature 027) is sufficient for package system stub implementation
- The *runtime-function-table* mechanism from 001-io-list-runtime is working and extensible
- Current compilation rate baseline is approximately 22% (will be verified during implementation)
- WasmGC type indices 0-28 are already allocated per CLAUDE.md; new types start at index 29 or higher
- Environment management functions are called internally during compilation; they do not need to be exported

## Dependencies

- Feature 027 (FFI Foundation): Required for package system stubs
- Feature 001-io-list-runtime: Provides runtime library infrastructure for function migration
- Existing Stage 1 generation: build/stage1-complete.lisp must continue to work
- wasm-tools: Required for validation testing

## Out of Scope

- Full package system implementation in Wasm (only stubs that delegate to host)
- New CLOS primitives (covered by separate M3 milestone)
- DEFSTRUCT compilation improvements (separate feature)
- Stage 2 generation (depends on Stage 1 improvements first)
- Performance optimization of generated code
