# Feature Specification: Equality Predicate Type-Dispatch Consolidation

**Feature Branch**: `001-equality-type-dispatch`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler code generation refactoring for Clysm that consolidates the four equality predicate functions (compile-eq at 48 lines, compile-eql at 144 lines, compile-equal at 274 lines, compile-equalp at 374 lines) into a unified type-dispatch infrastructure. The refactoring must introduce a common compile-type-dispatch function for runtime type checking, a compile-equality-predicate master function accepting an equality level parameter (:eq, :eql, :equal, :equalp), and reduce the total equality-related code from 800+ lines to under 400 lines while maintaining byte-compatible Wasm output."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Maintain Equality Predicates (Priority: P1)

As a compiler maintainer, I need to modify equality predicate behavior (e.g., add support for a new type) without duplicating code across four separate functions, so that bug fixes and enhancements propagate consistently to all equality levels.

**Why this priority**: Code duplication is the primary maintenance burden. Currently, adding support for a new type requires changes in up to four places with subtle semantic differences. This consolidation eliminates that duplication.

**Independent Test**: Can be fully tested by running `sbcl --eval "(asdf:test-system :clysm)"` and verifying all equality-predicates tests pass. Delivers immediate value by making the codebase more maintainable.

**Acceptance Scenarios**:

1. **Given** the consolidated equality implementation, **When** all four equality predicates (eq, eql, equal, equalp) are compiled, **Then** the generated Wasm bytecode is identical to the pre-refactoring output for the same inputs.
2. **Given** the consolidated equality implementation, **When** a new type comparison is added to the type-dispatch infrastructure, **Then** the change automatically applies to all equality levels that support that type.
3. **Given** the consolidated equality implementation, **When** the equality-related code is measured, **Then** the total line count is under 400 lines (reduced from 840+ lines).

---

### User Story 2 - Stage 1 Compilation Success (Priority: P2)

As a bootstrap engineer, I need the refactored equality predicates to compile successfully during Stage 1 generation, so that the self-hosting bootstrap pipeline continues to work.

**Why this priority**: The Clysm compiler must be able to compile itself. Any refactoring that breaks Stage 1 generation blocks the entire self-hosting effort.

**Independent Test**: Can be tested by running `sbcl --load build/stage1-complete.lisp` and verifying it completes without errors. The output Wasm must pass `wasm-tools validate`.

**Acceptance Scenarios**:

1. **Given** the refactored equality predicates, **When** Stage 1 compilation is executed, **Then** the build completes successfully and produces a valid Wasm file.
2. **Given** the Stage 1 output, **When** validated with wasm-tools, **Then** validation passes with exit code 0.

---

### User Story 3 - Reduced File Size (Priority: P3)

As a codebase maintainer, I need the func-section.lisp file to be reduced to under 15,700 lines, so that the file remains navigable and the codebase continues its trend toward consolidation.

**Why this priority**: Large files are harder to navigate and maintain. This refactoring contributes to the ongoing effort to reduce func-section.lisp size.

**Independent Test**: Can be tested by running `wc -l src/clysm/compiler/codegen/func-section.lisp` and verifying the result is under 15,700.

**Acceptance Scenarios**:

1. **Given** the refactored implementation, **When** func-section.lisp line count is measured, **Then** the result is under 15,700 lines (currently 16,097 lines).

---

### Edge Cases

- What happens when comparing values of different types (e.g., integer vs float for equalp)?
- How does the system handle recursive structures (circular lists) in equal/equalp?
- What happens when comparing unbound markers or special internal values?
- How does type-dispatch handle unknown/unsupported types?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a compile-type-dispatch function that generates Wasm instructions for runtime type checking against WasmGC type indices.
- **FR-002**: System MUST provide a compile-equality-predicate master function that accepts an equality level parameter (:eq, :eql, :equal, :equalp) and dispatches to appropriate comparison logic.
- **FR-003**: The refactored implementation MUST produce byte-identical Wasm output compared to the original implementation for all supported input patterns.
- **FR-004**: All four equality predicates (eq, eql, equal, equalp) MUST be implemented using the shared infrastructure with no code duplication for type-checking logic.
- **FR-005**: The type-dispatch infrastructure MUST support all existing WasmGC types: cons, symbol, string, closure, float, ratio, instance, standard-class, hash-table, slot-vector, mv_array, macro-environment, mdarray.
- **FR-006**: The equality level semantics MUST follow ANSI Common Lisp specifications:
  - [:eq](resources/HyperSpec/Body/f_eq.htm) compares object identity only
  - [:eql](resources/HyperSpec/Body/f_eql.htm) additionally compares numbers of the same type and characters
  - [:equal](resources/HyperSpec/Body/f_equal.htm) additionally compares conses recursively, strings, and bit-vectors
  - [:equalp](resources/HyperSpec/Body/f_equalp.htm) additionally ignores case in characters/strings and compares numbers across types
- **FR-007**: System MUST maintain compatibility with the primitive dispatch table infrastructure (register-primitive-compiler API).

### Key Entities

- **Equality Level**: One of :eq, :eql, :equal, :equalp representing the semantic comparison depth
- **Type Dispatch Table**: Mapping from WasmGC type indices to comparison instruction sequences
- **Comparison Result**: Wasm instructions that leave NIL or T on the stack

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing equality-predicates tests pass (unit tests, integration tests, contract tests)
- **SC-002**: Stage 1 compilation completes successfully
- **SC-003**: Generated Wasm passes wasm-tools validation with exit code 0
- **SC-004**: func-section.lisp reduced to under 15,700 lines (from current 16,097)
- **SC-005**: Total equality-related code reduced to under 400 lines (from current 840+ lines)
- **SC-006**: Wasm output for equality predicates is byte-identical with pre-refactoring output

## Assumptions

- The current equality predicate implementations are correct and their Wasm output represents the target behavior
- The primitive dispatch table infrastructure (from feature 002-primitive-dispatch-table) is available and stable
- WasmGC type indices are stable and match the documented values in CLAUDE.md
- The existing test suite provides adequate coverage to verify behavioral equivalence
