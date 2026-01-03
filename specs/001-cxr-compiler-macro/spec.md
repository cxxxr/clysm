# Feature Specification: CXR Compiler Macro Consolidation

**Feature Branch**: `001-cxr-compiler-macro`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler code generation refactoring for Clysm that consolidates 12 individual compile-cXXr functions (compile-caar, compile-cadr, compile-cdar, compile-cddr, compile-caaar, compile-caadr, compile-cadar, compile-caddr, compile-cdaar, compile-cdadr, compile-cddar, compile-cdddr) into a single define-cxr-compiler macro. Each function currently calls compile-cxr-chain with a specific operation sequence like (:car :car) or (:cdr :car). The macro should generate these functions automatically, reducing code duplication by approximately 100 lines in func-section.lisp. All existing tests must pass, Stage 1 compilation must succeed, and wasm-tools validation must pass."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compiler Maintainer Adds New CXR Function (Priority: P1)

A compiler maintainer needs to add support for a new cXXXXr accessor (e.g., `caaaar`). Currently, this requires writing a new 5-line function that follows the exact pattern of the 12 existing functions. With the `define-cxr-compiler` macro, the maintainer adds a single macro invocation that automatically generates the correct function.

**Why this priority**: This is the primary value proposition - reducing boilerplate and making the codebase more maintainable. Every future cXr function addition benefits from this.

**Independent Test**: Can be tested by adding a new cXr compiler function using the macro and verifying it compiles correctly and produces valid Wasm.

**Acceptance Scenarios**:

1. **Given** the `define-cxr-compiler` macro exists, **When** a maintainer adds `(define-cxr-compiler caaaar "aaaa")`, **Then** the `compile-caaaar` function is automatically generated with correct signature `(args env)` and calls `compile-cxr-chain` with "aaaa".
2. **Given** a new cXr function is added via the macro, **When** the compiler processes a form like `(caaaar x)`, **Then** it produces the same Wasm output as if the function were manually defined.

---

### User Story 2 - Existing CXR Functions Work Identically (Priority: P1)

All 12 existing compile-cXXr functions (caar through cdddr) continue to work exactly as before. Stage 1 compilation succeeds, all existing tests pass, and generated Wasm validates correctly.

**Why this priority**: Correctness is equally critical - this is a refactoring that must not change observable behavior.

**Independent Test**: Run the full test suite and Stage 1 compilation before and after the refactoring; outputs must be identical.

**Acceptance Scenarios**:

1. **Given** the refactored codebase with `define-cxr-compiler` macro, **When** Stage 1 compilation is executed, **Then** it succeeds without errors and produces valid Wasm.
2. **Given** the refactored codebase, **When** `wasm-tools validate` is run on the output, **Then** it passes with exit code 0.
3. **Given** a Lisp form using any cXr accessor (e.g., `(caddr list)`), **When** compiled after refactoring, **Then** the generated Wasm instructions are semantically equivalent to the pre-refactoring output.

---

### User Story 3 - Code Reduction and Readability (Priority: P2)

A developer reviewing func-section.lisp sees a concise macro definition followed by 12 single-line macro invocations instead of 12 nearly-identical 5-line function definitions. The code is easier to understand and modify.

**Why this priority**: Developer experience improvement - important but secondary to correctness.

**Independent Test**: Count lines of code before and after; verify significant reduction in the cXr accessor section.

**Acceptance Scenarios**:

1. **Given** the refactored codebase, **When** counting lines in the cXr accessors section, **Then** the line count is reduced by at least 40 lines compared to the original 60 lines.
2. **Given** the macro-based implementation, **When** a developer reads the cXr accessor section, **Then** the mapping between function names and operation sequences is immediately clear from the macro invocations.

---

### Edge Cases

- What happens when the macro is invoked with an empty operation string? The macro should reject this at compile time or generate a function that signals an error.
- What happens when the macro is invoked with invalid characters (not 'a' or 'd') in the operation string? The macro should validate the string and signal an error at macroexpansion time.
- What happens if the same cXr name is defined twice? Standard Common Lisp redefinition behavior applies (the second definition overwrites the first).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `define-cxr-compiler` macro that generates compile-cXXr functions from a function name and operation string.
- **FR-002**: Generated functions MUST have the signature `(args env)` matching existing compile-* function conventions.
- **FR-003**: Generated functions MUST call `compile-cxr-chain` with the provided operation string.
- **FR-004**: Generated functions MUST include appropriate docstrings describing the cXr operation being compiled.
- **FR-005**: All 12 existing compile-cXXr functions (caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr) MUST be reimplemented using the macro.
- **FR-006**: The macro MUST validate that the operation string contains only 'a' and 'd' characters.
- **FR-007**: The `compile-cxr-chain` function MUST remain unchanged and continue to serve as the underlying implementation.

### Key Entities

- **define-cxr-compiler**: Macro that generates compiler functions for cXr accessors
- **compile-cxr-chain**: Existing function that implements the actual Wasm code generation for chained CAR/CDR operations
- **Operation String**: A string like "aa", "da", "dda" representing the sequence of CAR ('a') and CDR ('d') operations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing unit tests pass without modification
- **SC-002**: Stage 1 compilation completes successfully (`sbcl --load build/stage1-complete.lisp` exits with code 0)
- **SC-003**: Generated Wasm validates successfully (`wasm-tools validate dist/clysm-stage1.wasm` exits with code 0)
- **SC-004**: The cXr accessors section in func-section.lisp is reduced by at least 40 lines
- **SC-005**: The 12 compile-cXXr functions are replaced by 12 single-line macro invocations plus the macro definition
- **SC-006**: No new compiler warnings are introduced by the refactoring

## Assumptions

- The existing `compile-cxr-chain` function is correct and well-tested; this refactoring only addresses the repetitive function definitions.
- The operation string format (e.g., "aa" for caar, "da" for cadr) is the correct representation and will not change.
- Standard Common Lisp macro semantics apply; the macro expands at compile time.
- The refactoring does not change any public API or exported symbols beyond the addition of `define-cxr-compiler`.

## Out of Scope

- Optimization of the `compile-cxr-chain` implementation itself
- Adding support for cXXXXr accessors (4-level deep) - though the macro would make this trivial to add later
- Changing the operation string format or introducing a new representation
- Modifying how the generated functions are registered in the primitive dispatch table
