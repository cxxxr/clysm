# Feature Specification: Internal Function Export System

**Feature Branch**: `001-internal-function-export`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler internal function export system for the Clysm Common Lisp to WebAssembly compiler"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Export Internal Compiler Functions (Priority: P1)

A Clysm developer compiles the compiler source code to WebAssembly. Previously, 21.57% of forms compiled successfully, with 280+ failures caused by undefined internal functions like `LEXICAL-ENV-PARENT`, `COMPILE-TO-INSTRUCTIONS`, and `MAKE-WASM-STRUCT-TYPE`. After this feature, these internal functions are properly exported and accessible, eliminating these error patterns.

**Why this priority**: This directly addresses the largest blockers preventing self-hosting. The top 10 error patterns account for 34% of all failures. Resolving undefined function errors is the highest-impact improvement.

**Independent Test**: Can be fully tested by regenerating Stage 1 and verifying that specific error patterns (P114, P944, P321, P543, P457, P626) no longer appear in the report.

**Acceptance Scenarios**:

1. **Given** the compiler source with internal functions in subpackages, **When** Stage 1 is regenerated, **Then** no "Undefined function: LEXICAL-ENV-PARENT" errors occur (was 119 occurrences)
2. **Given** the compiler source, **When** Stage 1 is regenerated, **Then** no "Undefined function: COMPILE-TO-INSTRUCTIONS" errors occur (was 36 occurrences)
3. **Given** the compiler source, **When** Stage 1 is regenerated, **Then** no "Undefined function: MAKE-WASM-STRUCT-TYPE" errors occur (was 17 occurrences)
4. **Given** the compiler source, **When** Stage 1 is regenerated, **Then** no "Undefined function: AST-LITERAL-VALUE" errors occur (was 11 occurrences)

---

### User Story 2 - Add Missing Type Predicate Primitives (Priority: P2)

The compiler uses `PACKAGEP*` as a type predicate during compilation. Currently, 25 functions fail because `PACKAGEP*` is undefined. After this feature, `PACKAGEP*` and any other missing type predicates are implemented as primitives.

**Why this priority**: Type predicates are fundamental to runtime type dispatch. Without them, many CLOS and package-related functions cannot compile. This resolves the P951 error pattern.

**Independent Test**: Can be fully tested by compiling a function that uses `PACKAGEP*` and verifying it produces valid Wasm output.

**Acceptance Scenarios**:

1. **Given** a function using `PACKAGEP*`, **When** compiled to Wasm, **Then** the compilation succeeds with valid type predicate instructions
2. **Given** Stage 1 regeneration, **When** complete, **Then** no "Undefined function: PACKAGEP*" errors occur (was 25 occurrences)

---

### User Story 3 - Fix QUASIQUOTE Handling (Priority: P3)

Macro definitions using backquote syntax fail with "Undefined function: QUASIQUOTE" errors. After this feature, quasiquote forms are properly expanded during macro processing.

**Why this priority**: Quasiquote is essential for macro definitions. While fewer in count (16 occurrences), it blocks entire categories of macros from compiling. This resolves the P464 error pattern.

**Independent Test**: Can be fully tested by compiling a function that contains quasiquote forms and verifying proper expansion.

**Acceptance Scenarios**:

1. **Given** a function with backquote syntax, **When** compiled to Wasm, **Then** the quasiquote is expanded to list construction operations
2. **Given** Stage 1 regeneration, **When** complete, **Then** no "Undefined function: QUASIQUOTE" errors occur (was 16 occurrences)

---

### User Story 4 - Export Previously Consolidated Functions (Priority: P4)

The recent `001-internal-function-consolidation` feature moved I/O and list functions to runtime libraries but `COMPILE-UNARY-MATH-FFI` and `COMPILE-CXR-CHAIN` still cause undefined function errors (14 and 12 occurrences respectively). These functions need proper package exports.

**Why this priority**: These were identified in the consolidation feature but still cause errors. Completing the export ensures the consolidation work is fully effective.

**Independent Test**: Can be fully tested by verifying the P457 and P626 error patterns are eliminated.

**Acceptance Scenarios**:

1. **Given** Stage 1 regeneration, **When** complete, **Then** no "Undefined function: COMPILE-UNARY-MATH-FFI" errors occur (was 14 occurrences)
2. **Given** Stage 1 regeneration, **When** complete, **Then** no "Undefined function: COMPILE-CXR-CHAIN" errors occur (was 12 occurrences)

---

### Edge Cases

- What happens when an internal function is called before its defining module is loaded? (Module load order must be respected)
- How does the system handle circular dependencies between internal packages? (Package definitions should be split from implementations)
- What happens if an exported function shadows an existing symbol? (Compiler should warn on symbol conflicts)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST export `LEXICAL-ENV-PARENT` from `clysm/compiler/env` package to `clysm` package
- **FR-002**: System MUST export `COMPILE-TO-INSTRUCTIONS` from `clysm/compiler/compiler` package to `clysm` package
- **FR-003**: System MUST export `MAKE-WASM-STRUCT-TYPE` from `clysm/compiler/codegen/gc-types` package to `clysm` package
- **FR-004**: System MUST export `AST-LITERAL-VALUE` from `clysm/compiler/ast` package to `clysm` package
- **FR-005**: System MUST implement `PACKAGEP*` as a Wasm type predicate primitive returning boolean for package type check
- **FR-006**: System MUST expand `QUASIQUOTE` forms during macro processing to equivalent list construction calls
- **FR-007**: System MUST export `COMPILE-UNARY-MATH-FFI` from codegen to `clysm` package (if not already exported)
- **FR-008**: System MUST export `COMPILE-CXR-CHAIN` from codegen to `clysm` package (if not already exported)
- **FR-009**: System MUST produce valid Wasm that passes `wasm-tools validate` after all changes
- **FR-010**: System MUST generate an updated `dist/stage1-report.json` showing improved compilation metrics

### Key Entities

- **Internal Function**: A function defined in a subpackage (e.g., `clysm/compiler/env`) that is used by the main compiler but not publicly exported
- **Type Predicate Primitive**: A Wasm instruction sequence that tests runtime type of a value (e.g., `PACKAGEP*` → `ref.test $package`)
- **Package Export**: A symbol that is made accessible from another package via the export/import mechanism
- **Quasiquote Form**: A Lisp form using backquote syntax that expands to list construction at read time or compile time

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 compilation rate reaches 35% or higher (up from 21.57%)
- **SC-002**: Error pattern P114 (LEXICAL-ENV-PARENT) reduced from 119 to 0 occurrences
- **SC-003**: Error pattern P944 (COMPILE-TO-INSTRUCTIONS) reduced from 36 to 0 occurrences
- **SC-004**: Error pattern P951 (PACKAGEP*) reduced from 25 to 0 occurrences
- **SC-005**: Error pattern P321 (MAKE-WASM-STRUCT-TYPE) reduced from 17 to 0 occurrences
- **SC-006**: Error pattern P464 (QUASIQUOTE) reduced from 16 to 0 occurrences
- **SC-007**: Error pattern P543 (AST-LITERAL-VALUE) reduced from 11 to 0 occurrences
- **SC-008**: Error pattern P457 (COMPILE-UNARY-MATH-FFI) reduced from 14 to 0 occurrences
- **SC-009**: Error pattern P626 (COMPILE-CXR-CHAIN) reduced from 12 to 0 occurrences
- **SC-010**: Generated `dist/clysm-stage1.wasm` passes `wasm-tools validate` with exit code 0
- **SC-011**: Total DEFUN failures reduced by at least 50% (from 846 to 423 or fewer)

## Assumptions

- The internal functions exist in subpackages and have consistent naming conventions
- Exporting functions does not require API changes to the functions themselves
- The `PACKAGEP*` primitive follows the same pattern as existing type predicates (`CONSP*`, `SYMBOLP*`, etc.)
- Quasiquote expansion can be handled at compile time without runtime support
- Module load order is already correctly defined in the ASDF system
