# Feature Specification: Compiler Code Generation Cleanup

**Feature Branch**: `001-codegen-cleanup`
**Created**: 2026-01-04
**Status**: Draft
**Input**: User description: "Build a compiler code generation cleanup system for Clysm that systematically identifies and removes dead code from func-section.lisp"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Dead Code Detection (Priority: P1)

As a compiler maintainer, I need to identify functions in func-section.lisp that are no longer called after runtime library migration, so I can safely remove them and reduce code complexity.

**Why this priority**: Dead code detection is the foundation for all subsequent cleanup. Without accurate detection, removal would risk breaking the compiler.

**Independent Test**: Can be fully tested by running the detection tool and verifying it correctly identifies functions that have been migrated to runtime libraries (string-runtime.lisp, numeric-runtime.lisp, sequence-runtime.lisp, list-runtime.lisp, io-runtime.lisp).

**Acceptance Scenarios**:

1. **Given** func-section.lisp with 309 compile-* functions, **When** running dead code detection, **Then** the system identifies all functions whose functionality has been migrated to *runtime-function-table*
2. **Given** a compile-* function that is registered in *runtime-function-table*, **When** checking if it's dead code, **Then** the original inline implementation is marked as removable
3. **Given** a compile-* function still used in active code paths, **When** running detection, **Then** it is NOT marked as dead code

---

### User Story 2 - Transitive Dead Code Analysis (Priority: P2)

As a compiler maintainer, I need to identify helper functions that are only used by dead code, so I can remove them along with their callers in a single cleanup pass.

**Why this priority**: Helper functions represent hidden dead code that won't be caught by direct function analysis. Removing them is necessary to achieve significant line reduction.

**Independent Test**: Can be tested by analyzing call graphs and verifying helper functions are correctly classified as live or dead based on their callers.

**Acceptance Scenarios**:

1. **Given** a helper function only called by dead compile-* functions, **When** running transitive analysis, **Then** the helper is marked as removable
2. **Given** a helper function used by both dead and live code, **When** running transitive analysis, **Then** the helper is NOT marked as removable
3. **Given** a chain of helper functions (A calls B calls C), **When** only A is dead, **Then** B and C are analyzed for other live callers before marking

---

### User Story 3 - Quasiquote Pattern Migration (Priority: P3)

As a compiler maintainer, I need to migrate remaining quasiquote splice patterns (,@) to the with-instruction-collector macro, so the codebase has consistent instruction generation patterns and is easier to maintain.

**Why this priority**: Pattern migration improves code consistency but doesn't directly remove lines. It's valuable for maintainability but secondary to actual dead code removal.

**Independent Test**: Can be tested by converting a single quasiquote pattern to with-instruction-collector and verifying the generated Wasm output is identical.

**Acceptance Scenarios**:

1. **Given** a function using `,@` for instruction list building, **When** migrating to with-instruction-collector, **Then** the generated Wasm bytecode is identical
2. **Given** 111 quasiquote splice patterns in func-section.lisp, **When** migration is complete, **Then** all patterns are converted to with-instruction-collector macro usage
3. **Given** a migrated function, **When** running Stage 1 compilation, **Then** the output Wasm passes validation

---

### User Story 4 - Safe Batch Removal with Validation (Priority: P1)

As a compiler maintainer, I need to remove dead code in batches with automated validation after each batch, so I can catch regressions immediately and maintain compiler correctness.

**Why this priority**: Validation is critical for maintaining compiler integrity. Without it, dead code removal could silently break compilation.

**Independent Test**: Can be tested by removing a single dead function, running the test suite, and verifying Stage 1 Wasm validation passes.

**Acceptance Scenarios**:

1. **Given** a batch of identified dead functions, **When** removing them, **Then** all unit tests pass
2. **Given** dead code removal, **When** running Stage 1 compilation, **Then** wasm-tools validate returns exit code 0
3. **Given** a removal that breaks tests, **When** validation fails, **Then** the system reports which removal caused the failure
4. **Given** successful batch removal, **When** checking line count, **Then** func-section.lisp has fewer lines than before

---

### Edge Cases

- What happens when a function appears dead but is called via dynamic dispatch through *runtime-function-table*?
- How does the system handle macro-generated function calls that don't appear in static analysis?
- What if removing dead code exposes previously shadowed bugs in live code?
- How does the system handle circular dependencies between helper functions?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST identify all compile-* functions in func-section.lisp whose functionality has been migrated to runtime libraries
- **FR-002**: System MUST analyze *runtime-function-table* registrations to determine which functions are now handled by runtime dispatch
- **FR-003**: System MUST build a call graph of helper functions and their callers within func-section.lisp
- **FR-004**: System MUST identify helper functions that have no live callers after dead code removal
- **FR-005**: System MUST migrate remaining quasiquote splice patterns (,@) to with-instruction-collector macro
- **FR-006**: System MUST run the full test suite after each removal batch
- **FR-007**: System MUST run wasm-tools validate on Stage 1 output after each removal batch
- **FR-008**: System MUST report line count changes after each removal batch
- **FR-009**: System MUST provide rollback capability if validation fails
- **FR-010**: System MUST preserve all exported symbols and public API functions

### Key Entities

- **Dead Function**: A compile-* function whose functionality is now handled by *runtime-function-table* dispatch
- **Helper Function**: Internal function (not compile-*) used to support code generation
- **Quasiquote Pattern**: Code using `,@` for instruction list construction
- **Removal Batch**: A set of related dead functions removed together for validation
- **Runtime Function**: Function registered in *runtime-function-table* that replaces inline Wasm codegen

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: func-section.lisp reduced from 15,973 lines to under 8,000 lines (>50% reduction)
- **SC-002**: All existing unit tests pass after cleanup (100% test compatibility)
- **SC-003**: Stage 1 compilation produces valid Wasm (wasm-tools validate exit code 0)
- **SC-004**: All 111 quasiquote splice patterns migrated to with-instruction-collector macro
- **SC-005**: Zero regressions in compilation coverage rate (maintains current 22%+ compilation rate)
- **SC-006**: Removal process completes in discrete, validated batches with clear progress tracking

## Assumptions

- The 6 runtime libraries (string-runtime.lisp, numeric-runtime.lisp, sequence-runtime.lisp, list-runtime.lisp, io-runtime.lisp, ffi-runtime.lisp) contain complete implementations of migrated functions
- Functions registered in *runtime-function-table* are the authoritative list of migrated functionality
- The with-instruction-collector macro is semantically equivalent to quasiquote splice patterns for instruction building
- The test suite provides adequate coverage to detect regressions from dead code removal
- Stage 1 Wasm validation is a reliable indicator of compiler correctness

## Out of Scope

- Adding new functionality to the compiler
- Modifying runtime library implementations
- Changing the *runtime-function-table* dispatch mechanism
- Performance optimization of remaining code
- Refactoring live code beyond quasiquote pattern migration
