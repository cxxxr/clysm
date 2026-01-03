# Feature Specification: Instruction Collector Refactor

**Feature Branch**: `001-instruction-collector-refactor`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler code generation optimization for Clysm that migrates the two largest functions (compile-equalp at 374 lines and compile-primitive-call at 363 lines) from O(n²) append-based list construction to O(n) push+nreverse pattern using the existing with-instruction-collector macro. The migration must maintain byte-identical Wasm output verified by contract tests, preserve all existing test coverage, and reduce the remaining 158 append patterns in func-section.lisp. Success is measured by all tests passing, Stage 1 compilation rate >= 24%, and wasm-tools validation success."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile-Equalp Migration (Priority: P1)

As a compiler developer, I need `compile-equalp` (374 lines) migrated from O(n²) append-based list construction to O(n) push+nreverse pattern using the existing `with-instruction-collector` macro so that the largest function in func-section.lisp uses efficient instruction collection.

**Why this priority**: `compile-equalp` is the largest function at 374 lines and represents the greatest opportunity for demonstrating the migration approach works correctly for complex recursive type comparison logic.

**Independent Test**: Can be fully tested by running equality-related tests before and after migration, then comparing generated Wasm bytecode to verify byte-identical output.

**Acceptance Scenarios**:

1. **Given** `compile-equalp` using append-based instruction accumulation, **When** migrated to `with-instruction-collector`, **Then** all equality predicate tests (equalp, equal) pass
2. **Given** the migrated `compile-equalp` function, **When** generating Wasm for equality comparisons, **Then** output is byte-identical to pre-migration output
3. **Given** the migrated function, **When** measuring complexity, **Then** function uses push+nreverse pattern with O(n) time complexity

---

### User Story 2 - Compile-Primitive-Call Migration (Priority: P1)

As a compiler developer, I need `compile-primitive-call` (363 lines) migrated to use the instruction collector macro so that the second-largest function benefits from efficient instruction collection.

**Why this priority**: `compile-primitive-call` is the second-largest function and handles all primitive function dispatch, making it critical for compiler correctness.

**Independent Test**: Can be fully tested by running primitive function call tests and comparing Wasm output before and after migration.

**Acceptance Scenarios**:

1. **Given** `compile-primitive-call` using append-based patterns, **When** migrated to `with-instruction-collector`, **Then** all primitive function tests pass
2. **Given** the migrated function, **When** compiling primitive calls (arithmetic, comparison, type predicates), **Then** generated Wasm is byte-identical to pre-migration output
3. **Given** complex primitives with multiple code paths, **When** compiled after migration, **Then** all code paths produce correct instructions

---

### User Story 3 - Remaining Append Pattern Reduction (Priority: P2)

As a compiler developer, I need the remaining 158 append patterns in func-section.lisp reduced so that the codebase consistently uses efficient instruction collection throughout.

**Why this priority**: After migrating the two largest functions, addressing remaining patterns completes the optimization effort and ensures codebase consistency.

**Independent Test**: Can be tested by counting remaining append patterns before and after migration and verifying all tests still pass.

**Acceptance Scenarios**:

1. **Given** 158 remaining append patterns in func-section.lisp, **When** additional functions are migrated, **Then** append pattern count decreases significantly
2. **Given** incrementally migrated functions, **When** running the full test suite, **Then** all tests continue to pass
3. **Given** the migrated codebase, **When** reviewing instruction accumulation patterns, **Then** remaining patterns are documented exceptions with justification

---

### User Story 4 - Contract Test Verification (Priority: P1)

As a compiler developer, I need contract tests that verify byte-identical Wasm output so that I have confidence the migration preserves exact behavior.

**Why this priority**: Byte-identical output verification is the primary correctness guarantee for this refactoring effort.

**Independent Test**: Contract tests can run independently to compare pre-migration and post-migration Wasm bytecode.

**Acceptance Scenarios**:

1. **Given** baseline Wasm output captured before migration, **When** running contract tests after migration, **Then** all captured outputs match exactly
2. **Given** representative test cases for equality and primitive calls, **When** compared byte-by-byte, **Then** zero differences exist
3. **Given** contract test failures, **When** investigating, **Then** specific instruction differences are clearly reported

---

### Edge Cases

- What happens when `emit` is called outside of `with-instruction-collector`? The macro should signal a clear compile-time or runtime error.
- How does the system handle empty instruction lists in `emit*`? It should handle them gracefully as a no-op.
- What happens with deeply nested control flow in `compile-equalp` (type dispatch for cons, array, hash-table)? Each branch must collect instructions correctly.
- How are conditional branches handled where different paths emit different instruction counts? The collector must preserve order regardless of control flow.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST migrate `compile-equalp` (374 lines) to use `with-instruction-collector` macro
- **FR-002**: System MUST migrate `compile-primitive-call` (363 lines) to use `with-instruction-collector` macro
- **FR-003**: System MUST produce byte-identical Wasm output after migration, verified by contract tests
- **FR-004**: System MUST preserve all existing test coverage with 100% pass rate
- **FR-005**: System MUST maintain Stage 1 compilation rate at 24% or higher
- **FR-006**: System MUST pass wasm-tools validation on all generated output
- **FR-007**: System MUST reduce the 158 remaining append patterns in func-section.lisp
- **FR-008**: System MUST use O(n) push+nreverse pattern instead of O(n²) append pattern
- **FR-009**: System MUST support nested instruction collection contexts within migrated functions
- **FR-010**: System MUST document any append patterns that cannot be migrated with justification

### Key Entities

- **Instruction Collector**: The existing `with-instruction-collector` macro that establishes O(n) collection scope
- **compile-equalp**: 374-line function handling equality predicate compilation with type dispatch
- **compile-primitive-call**: 363-line function handling all primitive function call dispatch
- **Contract Test**: Test that captures and compares Wasm bytecode to verify identical output
- **Append Pattern**: Code using `(append accumulated new-instructions)` that needs migration

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `compile-equalp` (374 lines) fully migrated to instruction collector pattern
- **SC-002**: `compile-primitive-call` (363 lines) fully migrated to instruction collector pattern
- **SC-003**: 100% of existing Clysm tests pass after migration
- **SC-004**: Stage 1 compilation rate is 24% or higher
- **SC-005**: Generated Wasm passes wasm-tools validation
- **SC-006**: Contract tests verify byte-identical Wasm output for migrated functions
- **SC-007**: Remaining append patterns in func-section.lisp reduced from 158 baseline

## Assumptions

- The existing `with-instruction-collector` macro is already implemented and functional
- The current compilation rate baseline is at or near 24% (based on recent project improvements)
- The 158 append pattern count is accurate for the current state of func-section.lisp
- Contract tests can capture Wasm bytecode at function-level granularity
- The push+nreverse pattern provides semantically equivalent output to append
- Existing test coverage is sufficient to validate behavior preservation
