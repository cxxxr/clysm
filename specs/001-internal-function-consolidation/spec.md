# Feature Specification: Compiler Internal Function Consolidation

**Feature Branch**: `001-internal-function-consolidation`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler internal function consolidation system for the Clysm WebAssembly GC compiler. The system must export 7 categories of internal functions (ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, LOOP-KEYWORD-EQ) that are currently undefined during Stage 1 compilation, affecting 242+ forms. Additionally, clean up func-section.lisp by removing dead code for functions already migrated to runtime library (assoc, member, find, remove, princ, format, etc.), reducing it from 18,351 lines to under 12,000 lines. Success is measured by Stage 1 compilation rate increasing from 22.15% to 25%+ and zero errors for the targeted undefined function patterns."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Forms with Internal Function References (Priority: P1)

As a compiler developer working on the Clysm self-hosting bootstrap, I need all internal compiler functions to be properly exported so that forms referencing them can compile successfully during Stage 1 generation.

**Why this priority**: This is the core functionality that directly addresses the 242+ forms currently failing due to undefined internal functions. Without this, Stage 1 compilation cannot progress.

**Independent Test**: Can be fully tested by running Stage 1 generation and verifying that forms using ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, and LOOP-KEYWORD-EQ compile without "undefined function" errors.

**Acceptance Scenarios**:

1. **Given** the compiler source code with forms that call ENV-ADD-LOCAL, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
2. **Given** the compiler source code with forms that call COMPILE-TO-INSTRUCTIONS, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
3. **Given** the compiler source code with forms that call MAKE-WASM-STRUCT-TYPE, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
4. **Given** the compiler source code with forms that call COMPILE-UNARY-MATH-FFI, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
5. **Given** the compiler source code with forms that call AST-LITERAL-VALUE, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
6. **Given** the compiler source code with forms that call COMPILE-CXR-CHAIN, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.
7. **Given** the compiler source code with forms that call LOOP-KEYWORD-EQ, **When** Stage 1 compilation runs, **Then** those forms compile successfully without "undefined function" errors.

---

### User Story 2 - Reduce Codebase Size by Removing Dead Code (Priority: P2)

As a compiler maintainer, I need dead code in func-section.lisp removed so the codebase is more maintainable and reflects the actual runtime library architecture.

**Why this priority**: Codebase cleanup improves maintainability but is secondary to fixing compilation errors. The dead code does not break compilation; it only adds confusion and maintenance burden.

**Independent Test**: Can be fully tested by measuring func-section.lisp line count before and after the cleanup, verifying it decreases from 18,351 lines to under 12,000 lines while all existing tests continue to pass.

**Acceptance Scenarios**:

1. **Given** func-section.lisp contains inline implementations for assoc, **When** the cleanup is performed, **Then** the inline assoc code is removed because it has been migrated to list-runtime.lisp.
2. **Given** func-section.lisp contains inline implementations for member, **When** the cleanup is performed, **Then** the inline member code is removed because it has been migrated to list-runtime.lisp.
3. **Given** func-section.lisp contains inline implementations for find, **When** the cleanup is performed, **Then** the inline find code is removed because it has been migrated to list-runtime.lisp.
4. **Given** func-section.lisp contains inline implementations for remove, **When** the cleanup is performed, **Then** the inline remove code is removed because it has been migrated to sequence-runtime.lisp.
5. **Given** func-section.lisp contains inline implementations for princ/print/prin1, **When** the cleanup is performed, **Then** the inline I/O code is removed because it has been migrated to io-runtime.lisp.
6. **Given** func-section.lisp contains inline implementations for format, **When** the cleanup is performed, **Then** the inline format code is removed because it has been migrated to io-runtime.lisp.
7. **Given** the cleanup is complete, **When** all existing tests are run, **Then** they pass without regression.

---

### User Story 3 - Improve Stage 1 Compilation Rate (Priority: P1)

As a compiler developer working toward self-hosting, I need the Stage 1 compilation rate to increase from 22.15% to 25%+ so that bootstrap progress is measurable and demonstrated.

**Why this priority**: This is the overarching success metric that validates both the internal function exports and the codebase cleanup. It directly measures progress toward self-hosting.

**Independent Test**: Can be fully tested by running Stage 1 generation and checking the compilation rate reported in dist/stage1-report.json.

**Acceptance Scenarios**:

1. **Given** the current Stage 1 compilation rate is 22.15%, **When** internal function exports are added and dead code is removed, **Then** the compilation rate increases to 25% or higher.
2. **Given** Stage 1 generation completes, **When** dist/stage1-report.json is examined, **Then** zero errors are reported for ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, or LOOP-KEYWORD-EQ undefined function patterns.

---

### Edge Cases

- What happens when an internal function is exported but the runtime library migration is incomplete? The function should still be callable, and any incomplete migration should be documented.
- How does the system handle circular dependencies between internal functions? The export mechanism should handle functions that call each other.
- What happens if func-section.lisp dead code removal accidentally removes still-used code? The existing test suite should catch regressions; any test failure blocks the cleanup.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST export ENV-ADD-LOCAL function so it is available during Stage 1 compilation.
- **FR-002**: System MUST export COMPILE-TO-INSTRUCTIONS function so it is available during Stage 1 compilation.
- **FR-003**: System MUST export MAKE-WASM-STRUCT-TYPE function so it is available during Stage 1 compilation.
- **FR-004**: System MUST export COMPILE-UNARY-MATH-FFI function so it is available during Stage 1 compilation.
- **FR-005**: System MUST export AST-LITERAL-VALUE function so it is available during Stage 1 compilation.
- **FR-006**: System MUST export COMPILE-CXR-CHAIN function so it is available during Stage 1 compilation.
- **FR-007**: System MUST export LOOP-KEYWORD-EQ function so it is available during Stage 1 compilation.
- **FR-008**: System MUST remove dead code from func-section.lisp for functions migrated to list-runtime.lisp (assoc, member, find, position, rassoc).
- **FR-009**: System MUST remove dead code from func-section.lisp for functions migrated to sequence-runtime.lisp (remove, delete, count, substitute families).
- **FR-010**: System MUST remove dead code from func-section.lisp for functions migrated to io-runtime.lisp (princ, prin1, print, write, format, terpri).
- **FR-011**: System MUST maintain backward compatibility with existing compiler functionality after dead code removal.
- **FR-012**: System MUST produce a valid Wasm module after Stage 1 generation (passes wasm-tools validate).

### Key Entities

- **Internal Function Export**: A mechanism to make internal compiler functions available during Stage 1 self-compilation. Each export must be registered in the appropriate package and callable from compiled code.
- **Dead Code Block**: A section of func-section.lisp that implements functionality already migrated to runtime libraries. Identified by function name and code pattern matching.
- **Runtime Library Function**: A function implemented in Lisp (io-runtime.lisp, list-runtime.lisp, sequence-runtime.lisp) that replaces inline Wasm codegen in func-section.lisp.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 compilation rate increases from 22.15% to 25% or higher.
- **SC-002**: Zero "undefined function" errors occur for ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, and LOOP-KEYWORD-EQ patterns during Stage 1 generation.
- **SC-003**: func-section.lisp line count decreases from 18,351 lines to under 12,000 lines.
- **SC-004**: Generated Stage 1 Wasm module passes validation (wasm-tools validate returns exit code 0).
- **SC-005**: All existing tests continue to pass after changes (sbcl --eval "(asdf:test-system :clysm)" completes successfully).

## Assumptions

- The 7 internal function categories (ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, LOOP-KEYWORD-EQ) are already implemented but not exported from their respective packages.
- The functions identified for dead code removal (assoc, member, find, remove, princ, format, etc.) have already been successfully migrated to runtime libraries and are confirmed working.
- The existing test suite provides adequate coverage to detect regressions from dead code removal.
- The 242+ affected forms are the primary blockers for reaching the 25%+ compilation rate target.

## Scope Boundaries

### In Scope

- Exporting the 7 categories of internal compiler functions
- Removing dead code from func-section.lisp for migrated functions
- Verifying Stage 1 compilation rate improvement
- Ensuring all existing tests pass

### Out of Scope

- Implementing new internal functions (only exporting existing ones)
- Migrating additional functions to runtime libraries (only removing already-migrated dead code)
- Improving compilation rate beyond what the 7 function exports provide
- Changes to runtime library implementations
