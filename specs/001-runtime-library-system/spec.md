# Feature Specification: Runtime Library System

**Feature Branch**: `001-runtime-library-system`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a runtime library system for the Clysm compiler that separates primitive operations from standard library functions with two layers: primitives (Wasm-direct) and runtime library (Lisp-implemented)."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compiler Developer Adds New Standard Function (Priority: P1)

A compiler developer wants to add a new standard library function (e.g., `remove-if`) without modifying the core codegen infrastructure. They write the function in Lisp using existing primitives and runtime library functions, then the compiler automatically includes it in the generated Wasm module.

**Why this priority**: This is the core value proposition - enabling standard library growth without codegen complexity. Unblocks self-hosting by allowing the compiler itself to use runtime-implemented functions.

**Independent Test**: Can be fully tested by adding a single new runtime function and verifying it compiles to Wasm and executes correctly.

**Acceptance Scenarios**:

1. **Given** an existing primitives layer with `car`, `cdr`, `consp`, **When** a developer defines `(defun assoc (item alist) ...)` in the runtime library using only these primitives, **Then** the compiler produces valid Wasm that executes correctly.
2. **Given** a runtime library with `assoc` defined, **When** another runtime function `find` is added that calls `assoc`, **Then** both functions are included in the Wasm module with correct call resolution.
3. **Given** a runtime function attempts to use undefined primitive `%undefined-op`, **When** compilation is attempted, **Then** the compiler reports a clear error identifying the missing primitive.

---

### User Story 2 - Migrate I/O Functions from func-section.lisp (Priority: P2)

A maintainer wants to reduce the complexity of func-section.lisp (currently 18,229 lines) by migrating I/O functions like `princ`, `print`, `format`, and `write` from hardcoded Wasm generation to runtime library implementations.

**Why this priority**: Directly addresses the 40% complexity reduction goal. I/O functions are ideal candidates because they build on FFI primitives and don't require complex control flow.

**Independent Test**: Can be tested by compiling a program using `princ`, verifying the output matches expected behavior, and measuring func-section.lisp line reduction.

**Acceptance Scenarios**:

1. **Given** `princ` is currently implemented as 200+ lines of Wasm codegen, **When** it is migrated to runtime library using `%host-write-char` and `%host-write-string` primitives, **Then** the Wasm output produces identical behavior.
2. **Given** `format` is migrated to runtime library, **When** compiling `(format t "~A ~D" x 42)`, **Then** the output is correct and func-section.lisp codegen for format is removed.
3. **Given** all I/O functions are migrated, **When** line count of func-section.lisp is measured, **Then** it shows at least 40% reduction from 18,229 lines.

---

### User Story 3 - Bootstrap Self-Hosting Compiler (Priority: P3)

The Clysm compiler needs to compile its own source code to Wasm. Functions used by the compiler itself (list operations, sequence functions, string operations) must be available in the runtime library so they can be included in the Stage 1 Wasm output.

**Why this priority**: Essential for achieving true self-hosting (Phase 13D goal). Depends on P1 and P2 being complete first.

**Independent Test**: Can be tested by compiling a subset of compiler code that uses runtime library functions and verifying execution.

**Acceptance Scenarios**:

1. **Given** `member`, `assoc`, `find` are in runtime library, **When** compiler code using these functions is compiled to Wasm, **Then** the Stage 1 output includes these runtime functions.
2. **Given** runtime library is complete, **When** `sbcl --load build/stage1-complete.lisp` runs, **Then** Stage 1 compilation rate increases from current 14.2% toward self-hosting threshold.

---

### Edge Cases

- What happens when a runtime function has circular dependencies (A calls B, B calls A)?
- How does the system handle runtime functions that need tail-call optimization?
- What happens when a primitive is removed or renamed after runtime functions depend on it?
- How are multiple-value returns handled in runtime library functions?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a Layer 1 primitives registry that defines operations compiling directly to Wasm instructions.
- **FR-002**: System MUST support memory primitives: `cons`, `car`, `cdr`, `rplaca`, `rplacd`.
- **FR-003**: System MUST support type predicates: `consp`, `numberp`, `stringp`, `symbolp`, `null`.
- **FR-004**: System MUST support arithmetic primitives: `+`, `-`, `*`, `/`, `mod`, `<`, `>`, `=`, `<=`, `>=`.
- **FR-005**: System MUST support FFI host calls: `%host-write-char`, `%host-write-string`, `%host-read-char`, `%host-read-line`.
- **FR-006**: System MUST provide a Layer 2 runtime library where functions are defined in Lisp source.
- **FR-007**: Runtime library functions MUST be compilable using only Layer 1 primitives or other runtime library functions.
- **FR-008**: System MUST detect and reject runtime functions that call undefined primitives or functions.
- **FR-009**: System MUST support I/O functions in runtime: `princ`, `print`, `prin1`, `terpri`, `write`, `format`.
- **FR-010**: System MUST support list operations in runtime: `assoc`, `member`, `find`, `position`, `nth`, `nthcdr`.
- **FR-011**: System MUST support sequence operations in runtime: `remove`, `remove-if`, `count`, `count-if`, `substitute`.
- **FR-012**: System MUST support string operations in runtime: `princ-to-string`, `prin1-to-string`, `write-to-string`.
- **FR-013**: Runtime library source MUST be organized in separate files by category (io.lisp, list-ops.lisp, sequences.lisp, strings.lisp).
- **FR-014**: System MUST maintain backward compatibility with existing compiled Wasm calling conventions.
- **FR-015**: System MUST support multiple-value returns from runtime functions using existing mv-count/mv-buffer mechanism.

### Key Entities

- **Primitive**: A core operation that compiles directly to Wasm instructions. Has a name, signature, and Wasm instruction sequence.
- **Runtime Function**: A Lisp function compiled using the same compiler, stored as source in the runtime library. Has a name, parameter list, and body that uses only primitives or other runtime functions.
- **Primitive Registry**: A mapping from primitive names to their Wasm implementations. Used during compilation to resolve calls.
- **Runtime Module**: A collection of runtime functions organized by category. Compiled into the Wasm module alongside user code.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: func-section.lisp reduces by at least 40% (from 18,229 lines to under 11,000 lines) after I/O migration.
- **SC-002**: New standard library functions can be added by writing Lisp source without modifying codegen files.
- **SC-003**: All migrated functions pass their existing test suites with identical behavior.
- **SC-004**: Stage 1 compilation rate increases by at least 10 percentage points after runtime library enables more self-hosting forms.
- **SC-005**: Runtime library functions execute correctly when called from both host-compiled and Wasm-compiled code.
- **SC-006**: Build time for Stage 1 Wasm does not increase by more than 20% due to runtime library compilation.
