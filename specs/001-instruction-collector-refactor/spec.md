# Feature Specification: Instruction Collector Refactor

**Feature Branch**: `001-instruction-collector-refactor`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler enhancement system for Clysm that refactors the instruction collection pattern in func-section.lisp. The system should replace 525 O(n²) append-based patterns with O(n) push+nreverse patterns using a new with-instruction-collector macro. Include emit and emit* local macros for ergonomic instruction emission. Implement gradual migration starting with the largest functions (compile-equalp at 374 lines, compile-primitive-call at 363 lines). Verify all existing tests pass and maintain 19%+ compilation rate. Target reduction: 500 lines of code."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Macro Infrastructure Creation (Priority: P1)

As a compiler developer, I need a `with-instruction-collector` macro that provides O(n) instruction collection semantics so that I can migrate existing O(n²) append patterns without changing behavior.

**Why this priority**: The macro is the foundational infrastructure that all other migration work depends on. Without this, no refactoring can begin.

**Independent Test**: Can be fully tested by creating a simple function that uses `with-instruction-collector` and verifying the collected instructions match expected output in the correct order.

**Acceptance Scenarios**:

1. **Given** a body of code using `(emit ...)` forms, **When** `with-instruction-collector` wraps the body, **Then** all emitted instructions are collected in order and returned as a list
2. **Given** nested uses of `(emit* list)` forms, **When** evaluating the body, **Then** all instructions from the list are collected in the correct order
3. **Given** an empty body, **When** `with-instruction-collector` is invoked, **Then** it returns an empty list

---

### User Story 2 - Large Function Migration (Priority: P2)

As a compiler developer, I need the largest functions (`compile-equalp` and `compile-primitive-call`) migrated to use the instruction collector so that I can validate the approach works for complex cases and achieve significant code reduction.

**Why this priority**: Migrating the two largest functions (374 + 363 = 737 lines) validates the approach at scale and provides the most immediate code reduction benefit.

**Independent Test**: Can be fully tested by running the existing test suite after migration and verifying identical Wasm output for all compilation scenarios that use these functions.

**Acceptance Scenarios**:

1. **Given** `compile-equalp` using legacy append patterns, **When** migrated to instruction collector, **Then** all existing equality tests pass with identical behavior
2. **Given** `compile-primitive-call` using legacy append patterns, **When** migrated to instruction collector, **Then** all primitive call tests pass with identical behavior
3. **Given** the migrated functions, **When** measuring line count, **Then** combined reduction is at least 100 lines from these two functions alone

---

### User Story 3 - Full Pattern Migration (Priority: P3)

As a compiler developer, I need all 525 O(n²) append-based patterns replaced with O(n) push+nreverse patterns so that the codebase is consistent and maintainable.

**Why this priority**: Completing the full migration ensures consistency and maximizes code reduction, but can proceed incrementally after validating the approach with large functions.

**Independent Test**: Can be tested by grepping for remaining append patterns and verifying count decreases to zero, while maintaining passing tests throughout migration.

**Acceptance Scenarios**:

1. **Given** 525 identified append patterns, **When** all are migrated, **Then** zero O(n²) append patterns remain in func-section.lisp
2. **Given** the full migration, **When** running all Clysm tests, **Then** all tests pass
3. **Given** the full migration, **When** measuring total line count reduction, **Then** reduction is at least 500 lines

---

### User Story 4 - Compilation Rate Preservation (Priority: P1)

As a compiler developer, I need the refactoring to maintain or improve the current 19%+ Stage 1 compilation rate so that bootstrap progress is not regressed.

**Why this priority**: Maintaining compilation rate is a critical constraint that must be verified alongside all migration work.

**Independent Test**: Can be tested by running Stage 1 generation and verifying the compilation rate in the generated report.

**Acceptance Scenarios**:

1. **Given** the current 19%+ compilation rate baseline, **When** any migration is performed, **Then** the compilation rate remains at or above 19%
2. **Given** the fully migrated codebase, **When** running `sbcl --load build/stage1-complete.lisp`, **Then** the resulting Wasm passes validation

---

### Edge Cases

- What happens when `emit` is called outside of `with-instruction-collector`? The macro should detect this and signal a clear compile-time or runtime error.
- How does the system handle empty instruction lists in `emit*`? It should handle them gracefully as a no-op.
- What happens when nested `with-instruction-collector` forms are used? Each collector should maintain independent state without interference.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `with-instruction-collector` macro that establishes a dynamic scope for instruction collection
- **FR-002**: System MUST provide an `emit` local macro that appends a single instruction to the collector using O(n) push semantics
- **FR-003**: System MUST provide an `emit*` local macro that appends multiple instructions from a list to the collector using O(n) push semantics
- **FR-004**: System MUST return collected instructions in correct order (using nreverse on completion)
- **FR-005**: System MUST support nested `with-instruction-collector` forms with independent collection state
- **FR-006**: System MUST signal a clear error when `emit` or `emit*` is used outside a collector context
- **FR-007**: System MUST maintain backward compatibility - all existing tests must pass after migration
- **FR-008**: System MUST achieve at least 500 lines of code reduction in func-section.lisp
- **FR-009**: System MUST maintain 19%+ Stage 1 compilation rate after migration
- **FR-010**: System MUST not change the semantics of generated Wasm bytecode

### Key Entities

- **Instruction Collector**: A lexical scope that accumulates Wasm instructions using O(n) operations
- **Emit Form**: A local macro that adds a single instruction to the active collector
- **Emit* Form**: A local macro that adds multiple instructions from a list to the active collector
- **Migration Target**: A function in func-section.lisp that currently uses append-based instruction accumulation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Total line count of func-section.lisp reduced by at least 500 lines (from ~16,500 to ~16,000 or less)
- **SC-002**: Zero O(n²) append-based instruction accumulation patterns remain in func-section.lisp after migration
- **SC-003**: 100% of existing Clysm tests pass after migration
- **SC-004**: Stage 1 compilation rate remains at 19% or higher
- **SC-005**: Generated Wasm output is byte-for-byte identical for representative test cases before and after migration
- **SC-006**: `compile-equalp` and `compile-primitive-call` are fully migrated and reduced in line count

## Assumptions

- The current append-based pattern count (approximately 525-675) is accurate and stable for the duration of this work
- The `push + nreverse` pattern provides equivalent semantics to the current append-based accumulation
- Line count reduction correlates with reduced boilerplate from the macro abstraction
- Existing test coverage is sufficient to validate behavior preservation
- The current ~16,500 line count for func-section.lisp is the baseline for measuring reduction
