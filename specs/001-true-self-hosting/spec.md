# Feature Specification: Phase 13D - True Self-Hosting Achievement

**Feature Branch**: `001-true-self-hosting`
**Created**: 2025-12-28
**Status**: Draft
**Input**: Phase 13D: 真のセルフホスティング達成を実装する。目標はStage 0に実際のコンパイルロジックを持たせ、非自明な固定点を達成すること。Wasm上で動作する最小Lispインタプリタを実装し、(+ 1 2)や(defun f (x) x)をコンパイル可能にする。Stage 1が1KB以上で、Stage 1 == Stage 2となることを検証基準とする。基本型（fixnum, symbol, cons）、プリミティブ（car, cdr, cons, +, -, *, /, <, >, =, eq）、制御構造（if, let, let*, defun, lambda, quote）をサポートする。

## Background

The current Stage 0 compiler (275 bytes) contains only stub functions that produce empty Wasm modules. While the bootstrap infrastructure is complete (fixed-point verification scripts, stage generation pipeline), Stage 0 lacks actual compilation logic. The current "fixed-point" is trivial: Stage 1 == Stage 2 == empty 17-byte modules.

**Goal**: Implement a minimal but real Lisp interpreter/compiler in Wasm that can compile simple forms, producing a non-trivial fixed-point where Stage 1 and Stage 2 are meaningful, substantial binaries.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Arithmetic Expression (Priority: P1)

As a developer, I can use Stage 0 to compile a basic arithmetic expression like `(+ 1 2)`, and the result is a valid Wasm module containing the computed logic.

**Why this priority**: This is the most fundamental proof that Stage 0 contains real compilation logic, not just stubs. Without arithmetic compilation, nothing else can work.

**Independent Test**: Run `wasmtime run dist/clysm-stage0.wasm --invoke compile_form "(+ 1 2)"` and verify the output is a valid Wasm binary that, when executed, returns 3.

**Acceptance Scenarios**:

1. **Given** Stage 0 binary with interpreter implementation, **When** compile_form is invoked with "(+ 1 2)", **Then** a valid Wasm binary is returned (not empty, passes validation).

2. **Given** Stage 0 compiles "(* 3 4)", **When** the resulting Wasm is executed, **Then** the result is 12.

3. **Given** Stage 0 compiles nested arithmetic "(+ 1 (* 2 3))", **When** the resulting Wasm is executed, **Then** the result is 7.

---

### User Story 2 - Compile Function Definition (Priority: P1)

As a developer, I can use Stage 0 to compile a function definition like `(defun f (x) x)`, producing a Wasm module with an exported function.

**Why this priority**: Function definition is essential for compiling any real Lisp code. The compiler itself is composed of functions.

**Independent Test**: Compile `(defun identity (x) x)` and verify the resulting Wasm exports a callable "identity" function.

**Acceptance Scenarios**:

1. **Given** Stage 0 compiles "(defun f (x) x)", **When** the Wasm is examined, **Then** it contains an exported function "f".

2. **Given** Stage 0 compiles "(defun add (a b) (+ a b))", **When** the Wasm is executed with arguments 2 and 3, **Then** the result is 5.

3. **Given** Stage 0 compiles multiple defuns, **When** Wasm is validated, **Then** all functions are present in exports.

---

### User Story 3 - Achieve Non-Trivial Fixed-Point (Priority: P1)

As a developer, I can verify that Stage 0 produces a Stage 1 of at least 1KB, and that Stage 1 == Stage 2, demonstrating meaningful self-hosting.

**Why this priority**: The non-trivial fixed-point is the ultimate proof of self-hosting. A 17-byte empty module is not self-hosting.

**Independent Test**: Run `./scripts/verify-fixpoint.sh --json` and verify Stage 1 size is >= 1024 bytes and Stage 1 == Stage 2.

**Acceptance Scenarios**:

1. **Given** Stage 0 compiles its own source, **When** Stage 1 is generated, **Then** Stage 1 size is at least 1KB.

2. **Given** Stage 1 compiles the same source, **When** Stage 2 is generated, **Then** Stage 1 and Stage 2 are byte-identical.

3. **Given** fixed-point is achieved, **When** verification reports status, **Then** status is "ACHIEVED" with non-trivial size metrics.

---

### User Story 4 - Compile Control Structures (Priority: P2)

As a developer, I can use Stage 0 to compile control structures (if, let, let*) to produce conditional and binding logic in Wasm.

**Why this priority**: Control flow is required for any non-trivial program but builds on arithmetic and function capabilities.

**Independent Test**: Compile `(if (< 1 2) 10 20)` and verify execution returns 10.

**Acceptance Scenarios**:

1. **Given** Stage 0 compiles "(if (< 1 2) 10 20)", **When** executed, **Then** result is 10.

2. **Given** Stage 0 compiles "(let ((x 5)) (+ x 3))", **When** executed, **Then** result is 8.

3. **Given** Stage 0 compiles "(let* ((x 1) (y (+ x 1))) y)", **When** executed, **Then** result is 2.

---

### User Story 5 - Compile Lambda Expressions (Priority: P2)

As a developer, I can use Stage 0 to compile lambda expressions for creating anonymous functions.

**Why this priority**: Lambda is essential for higher-order programming but depends on function compilation working first.

**Independent Test**: Compile and execute `((lambda (x) (+ x 1)) 5)` to get 6.

**Acceptance Scenarios**:

1. **Given** Stage 0 compiles "(lambda (x) x)", **When** result is called with 42, **Then** returns 42.

2. **Given** Stage 0 compiles "((lambda (x y) (+ x y)) 2 3)", **When** executed, **Then** result is 5.

---

### User Story 6 - Support Basic Types and Primitives (Priority: P2)

As a developer, I can work with fixnum, symbol, and cons types, and use primitives (car, cdr, cons, eq) in compiled code.

**Why this priority**: List processing primitives are fundamental to Lisp but require the type system to be established first.

**Independent Test**: Compile `(car (cons 1 2))` and verify execution returns 1.

**Acceptance Scenarios**:

1. **Given** Stage 0 compiles "(cons 1 2)", **When** car is applied, **Then** result is 1.

2. **Given** Stage 0 compiles "(cdr (cons 1 2))", **When** executed, **Then** result is 2.

3. **Given** Stage 0 compiles "(eq 'a 'a)", **When** executed, **Then** result is T (true).

---

### Edge Cases

- What happens when Stage 0 encounters an unsupported form? The compiler should signal an error and skip the form, logging what was unsupported.
- How does the interpreter handle stack overflow during deep recursion? WasmGC runtime limits apply; recursive forms should use worklist patterns.
- What happens with division by zero in arithmetic? The compiler should check and signal a divide-by-zero condition.
- How are symbols interned without a full package system? A minimal symbol table with string comparison suffices for bootstrap.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Stage 0 MUST implement a Lisp interpreter/evaluator in Wasm that can process S-expressions.
- **FR-002**: Stage 0 MUST support fixnum type (31-bit integers using WasmGC i31ref).
- **FR-003**: Stage 0 MUST support symbol type (struct with name field).
- **FR-004**: Stage 0 MUST support cons type (struct with car and cdr fields).
- **FR-005**: Stage 0 MUST implement primitives: car, cdr, cons.
- **FR-006**: Stage 0 MUST implement arithmetic primitives: +, -, *, /.
- **FR-007**: Stage 0 MUST implement comparison primitives: <, >, =.
- **FR-008**: Stage 0 MUST implement equality primitive: eq.
- **FR-009**: Stage 0 MUST support control structure: if (conditional branching).
- **FR-010**: Stage 0 MUST support control structures: let, let* (lexical binding).
- **FR-011**: Stage 0 MUST support special forms: defun, lambda (function definition).
- **FR-012**: Stage 0 MUST support special form: quote (literal data).
- **FR-013**: compile_form export MUST accept an S-expression and return compiled Wasm bytes.
- **FR-014**: compile_all export MUST compile all forms and produce a complete Wasm module.
- **FR-015**: Generated Stage 1 MUST be at least 1KB in size (non-trivial compilation).
- **FR-016**: Stage 1 and Stage 2 MUST be byte-identical (fixed-point property).
- **FR-017**: All generated Wasm MUST pass wasm-tools validate.

### Key Entities

- **Interpreter Core**: The evaluation engine running in Wasm that processes Lisp forms.
- **Type System**: WasmGC type definitions for fixnum (i31ref), symbol, cons, and closure.
- **Primitive Table**: Mapping from primitive operation symbols to Wasm implementations.
- **Environment**: Data structure for tracking lexical variable bindings during evaluation.
- **Code Generator**: Component that transforms evaluated forms into Wasm binary output.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `(+ 1 2)` compiles and executes correctly, returning 3.
- **SC-002**: `(defun f (x) x)` compiles and produces a callable exported function.
- **SC-003**: Stage 1 binary size is at least 1024 bytes (1KB).
- **SC-004**: Stage 1 and Stage 2 are byte-identical (fixed-point achieved).
- **SC-005**: All generated Wasm passes `wasm-tools validate` without errors.
- **SC-006**: The complete set of primitives (car, cdr, cons, +, -, *, /, <, >, =, eq) is functional.
- **SC-007**: Control structures (if, let, let*, defun, lambda, quote) compile correctly.
- **SC-008**: Nested expressions like `(+ 1 (* 2 3))` produce correct results (7).

## Assumptions

- The current 275-byte Stage 0 infrastructure (type definitions, export stubs) can be extended rather than rewritten.
- WasmGC i31ref is sufficient for fixnum representation in the bootstrap phase.
- A minimal symbol table without full package system is acceptable for initial bootstrap.
- The interpreter approach (rather than direct compilation) is appropriate for achieving initial self-hosting.
- wasmtime and wasm-tools are available for execution and validation.

## Dependencies

- **Feature 040**: Fixed-Point Verification - Provides verification scripts for comparing Stage 1 and Stage 2.
- **Feature 045**: Stage 0 Complete Compiler - Provides the infrastructure this feature extends.
- **CLAUDE.md WasmGC Types**: Type indices 0-24 for cons, symbol, string, closure, etc.

## Out of Scope

- Full ANSI Common Lisp compliance (addressed in Phase 14+).
- Complex macros beyond the bootstrap minimum.
- Multi-value returns (simplify to single-value for bootstrap).
- Full condition system (minimal error handling only).
- Package system (single namespace for bootstrap).
