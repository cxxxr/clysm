# Feature Specification: Tagbody/Go Control Flow

**Feature Branch**: `004-tagbody-go`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "Implement tagbody/go control flow for Wasm compiler based on resources/tagbody-go-plan.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Simple Loop Iteration (Priority: P1)

A Common Lisp developer writes a simple loop using `tagbody` with a single tag and backward jump. This is the most common pattern used by `loop`, `do`, `dolist`, and `dotimes` macros internally. The compiler generates efficient Wasm `loop` instructions without dispatch overhead.

**Why this priority**: This is the most frequently used pattern in real-world Common Lisp code. Every iteration macro (`loop`, `do`, `dolist`, `dotimes`) compiles down to this pattern. Handling this efficiently is critical for performance.

**Independent Test**: Can be fully tested by compiling a simple counting loop that increments a variable N times, then verifying the final value.

**Acceptance Scenarios**:

1. **Given** a tagbody with a single tag at the start and one `go` statement that jumps backward to it, **When** the code is compiled, **Then** it generates Wasm `loop` instructions without `br_table` dispatch
2. **Given** a simple loop that counts from 0 to 10, **When** executed in Wasm runtime, **Then** the result is 10 without stack overflow
3. **Given** a loop that iterates 10,000 times, **When** executed in Wasm runtime, **Then** it completes successfully without stack overflow (demonstrating proper tail-call optimization)

---

### User Story 2 - Forward Jump (Skip Pattern) (Priority: P2)

A Common Lisp developer uses `go` to skip over code conditionally. This pattern is used for early exits, conditional sections, and error handling within tagbody.

**Why this priority**: Forward jumps are the second most common pattern after simple loops. They enable conditional execution paths without nested `if` statements.

**Independent Test**: Can be tested by compiling code that skips over an assignment and verifying the skipped code does not execute.

**Acceptance Scenarios**:

1. **Given** a tagbody with `(go SKIP)` followed by `(setq x 999)` and then tag `SKIP`, **When** executed, **Then** x is never set to 999
2. **Given** multiple consecutive tags with forward jumps, **When** executed, **Then** control transfers correctly to each target tag

---

### User Story 3 - Complex Multi-directional Jumps (Priority: P3)

A Common Lisp developer writes a state machine or complex control flow using multiple tags with both forward and backward jumps between them. The compiler handles this with a dispatch loop mechanism.

**Why this priority**: While less common than simple loops, complex control flow patterns are essential for implementing state machines, parsers, and other algorithmic code that cannot be expressed with simple conditionals.

**Independent Test**: Can be tested by compiling a tagbody with multiple tags where control flows between them in both directions, verifying the sequence of operations.

**Acceptance Scenarios**:

1. **Given** a tagbody with tags A and B where B jumps to A and A jumps to B conditionally, **When** executed with a termination condition, **Then** the loop completes with correct final state
2. **Given** three or more tags with arbitrary jump patterns, **When** compiled, **Then** the dispatch mechanism correctly routes to each tag
3. **Given** a tagbody where code falls through from one tag to another without explicit `go`, **When** executed, **Then** all code sections execute in sequence

---

### User Story 4 - Sequential Execution Without Go (Priority: P4)

A Common Lisp developer uses `tagbody` without any `go` statements (tags present for documentation or future use). The compiler optimizes this to simple sequential execution.

**Why this priority**: This is an edge case where `tagbody` is used without its control flow features. While rare, it must work correctly.

**Independent Test**: Can be tested by compiling a tagbody with tags but no `go` statements and verifying all expressions execute in order.

**Acceptance Scenarios**:

1. **Given** a tagbody with multiple tags but no `go` statements, **When** executed, **Then** all forms execute sequentially and the result is NIL
2. **Given** a tagbody with no tags and no `go` statements, **When** executed, **Then** all forms execute sequentially

---

### Edge Cases

- What happens when `go` targets a tag that does not exist? The compiler reports a compile-time error with a clear message indicating the undefined tag.
- What happens when `go` is called outside of any `tagbody`? The compiler reports a compile-time error indicating `go` must be inside a `tagbody`.
- How does nested `tagbody` handle inner `go` targeting outer tags? Outer tags are not visible from inner tagbody; the compiler reports a compile-time error for undefined tag.
- What happens when `go` appears after the target tag within the same segment? This is a backward jump and is handled correctly by the dispatch mechanism.
- What happens when the last expression before a tag produces a value? The value is discarded; tagbody always returns NIL.
- What happens when `go` is unreachable (dead code after another `go`)? The compiler may generate the code but it will never execute; no error is raised.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST detect and classify tagbody into one of three strategies: sequential (no go), simple-loop (single backward jump), or dispatch (complex jumps)
- **FR-002**: Compiler MUST generate Wasm `loop` instruction for simple-loop strategy without `br_table` overhead
- **FR-003**: Compiler MUST generate Wasm `block`/`loop`/`br_table` dispatch mechanism for complex jump patterns
- **FR-004**: Compiler MUST correctly calculate `br` depth for all jump targets in dispatch strategy
- **FR-005**: Compiler MUST report a compile-time error when `go` references an undefined tag
- **FR-006**: Compiler MUST report a compile-time error when `go` is used outside of any `tagbody`
- **FR-007**: Compiler MUST handle dead code after `go` statements (code that cannot be reached)
- **FR-008**: Compiler MUST ensure all execution paths return NIL as the tagbody result
- **FR-009**: Compiler MUST support fallthrough between segments (when no `go` interrupts execution)
- **FR-010**: Compiler MUST correctly handle nested tagbody forms with independent tag scopes

### Key Entities

- **tagbody-context**: Compilation context containing strategy type, tag-to-index mapping, pc local variable index (for dispatch), and loop depth information
- **segment**: A portion of tagbody code starting with an optional tag and containing zero or more forms until the next tag or end
- **strategy**: Classification of tagbody compilation approach (:sequential, :simple-loop, :dispatch)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing Common Lisp iteration patterns (loop counting, conditional loops) compile and execute correctly
- **SC-002**: Deep iteration (10,000+ iterations) completes without stack overflow
- **SC-003**: Forward jump patterns correctly skip over code sections
- **SC-004**: Complex multi-directional jump patterns produce correct results as verified by test cases
- **SC-005**: Simple loop patterns generate optimized Wasm without dispatch overhead (verifiable by inspecting generated WAT)
- **SC-006**: All error conditions (undefined tag, go outside tagbody) produce clear compile-time error messages
