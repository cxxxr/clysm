# Feature Specification: Quasiquote Local Variable Compilation

**Feature Branch**: `001-quasiquote-local-vars`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler enhancement for Clysm (Common Lisp to WebAssembly compiler) that enables proper compilation of quasiquote expressions containing local variable references. When a backquote expression like `(,x ,y) contains unquoted variables, the compiler must correctly resolve those variables from the lexical environment and emit appropriate Wasm local.get instructions. The feature should handle both simple unquote (,var) and unquote-splicing (,@list) forms within quoted code templates. This is critical for enabling macro-generated code patterns used throughout the compiler source, which currently fail with 'Cannot compile quoted element' errors."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Simple Unquote Compilation (Priority: P1)

A compiler developer writes Lisp code containing quasiquote expressions with simple unquoted local variables (e.g., `` `(foo ,x ,y) `` where `x` and `y` are local variables). The compiler correctly resolves these variables from the lexical environment and emits Wasm instructions that construct the list at runtime with the variable values.

**Why this priority**: This is the fundamental case that blocks most macro-generated code patterns. Simple unquote is the most common quasiquote usage pattern and must work before more complex patterns can be addressed.

**Independent Test**: Can be fully tested by compiling a function containing `` `(result ,var) `` where `var` is a local binding, and verifying the generated Wasm contains appropriate local.get instructions and list construction.

**Acceptance Scenarios**:

1. **Given** a function with a local variable `x` bound via `let`, **When** the body contains `` `(value ,x) ``, **Then** the compiler emits Wasm that retrieves `x` via local.get and constructs a cons cell with symbol `value` and the runtime value of `x`
2. **Given** a function with multiple local variables `a`, `b`, `c`, **When** the body contains `` `(,a ,b ,c) ``, **Then** the compiler emits Wasm that retrieves all three variables and constructs a proper list
3. **Given** a function with nested let bindings, **When** an inner scope uses `` `(,outer ,inner) `` referencing both outer and inner variables, **Then** the compiler correctly resolves both from the lexical environment

---

### User Story 2 - Unquote-Splicing Compilation (Priority: P2)

A compiler developer writes Lisp code containing quasiquote expressions with unquote-splicing (e.g., `` `(a ,@items b) `` where `items` is a list). The compiler correctly handles splicing the list contents into the surrounding structure.

**Why this priority**: Unquote-splicing is essential for macro patterns that generate variable-length code sequences, common in compiler code generation.

**Independent Test**: Can be fully tested by compiling a function containing `` `(prefix ,@list-var suffix) `` and verifying the generated Wasm correctly appends/splices list contents.

**Acceptance Scenarios**:

1. **Given** a function with a local variable `args` bound to a list, **When** the body contains `` `(call ,@args) ``, **Then** the compiler emits Wasm that splices the list contents after the symbol `call`
2. **Given** a quasiquote with multiple splice points `` `(,@front middle ,@back) ``, **When** compiled, **Then** the runtime correctly constructs a list with spliced contents in order
3. **Given** an empty list variable, **When** spliced with `` `(a ,@empty-list b) ``, **Then** the result is the list `(a b)` without nil elements

---

### User Story 3 - Nested Quasiquote Handling (Priority: P3)

A compiler developer writes Lisp code with nested quasiquote expressions where inner backquotes preserve structure while outer unquotes evaluate. The compiler correctly handles quasiquote nesting depth.

**Why this priority**: Nested quasiquotes are common in macro-writing macros and advanced metaprogramming patterns used in compiler infrastructure.

**Independent Test**: Can be fully tested by compiling a function with `` `(outer `(inner ,x)) `` and verifying correct nesting behavior.

**Acceptance Scenarios**:

1. **Given** a doubly-nested quasiquote `` `(a `(b ,x)) `` where `x` is local, **When** compiled, **Then** the inner backquote and comma are preserved as data, not evaluated
2. **Given** `` `(a ,,x) `` with nesting level indicating double-unquote, **When** compiled, **Then** the compiler evaluates `x` and the result is unquoted at the appropriate level

---

### User Story 4 - Mixed Quoted and Unquoted Elements (Priority: P2)

A compiler developer writes quasiquote expressions containing a mix of literal symbols, literal lists, and unquoted expressions. The compiler correctly distinguishes quoted data from expressions to evaluate.

**Why this priority**: Real-world macro templates typically combine static structure with dynamic insertions.

**Independent Test**: Can be fully tested by compiling `` `(defun ,name (,@args) ,body) `` and verifying correct handling of each element type.

**Acceptance Scenarios**:

1. **Given** `` `(if ,condition ,then-form ,else-form) ``, **When** compiled, **Then** the symbol `if` is quoted data while `condition`, `then-form`, and `else-form` are evaluated from locals
2. **Given** `` `((literal list) ,dynamic) ``, **When** compiled, **Then** `(literal list)` is preserved as a quoted sublist while `dynamic` is evaluated

---

### Edge Cases

- What happens when an unquoted variable is unbound in the lexical environment?
  - The compiler MUST emit a compile-time error indicating the undefined variable
- How does the system handle unquoting of non-variable expressions like `` `(,(+ 1 2)) ``?
  - The expression is compiled and evaluated at runtime, with the result spliced in
- What happens with unquote at the top level (outside any quasiquote)?
  - The compiler MUST emit a compile-time error for unquote outside backquote context
- How are special forms inside unquoted positions handled (e.g., `` `(,(let ...)) ``)?
  - The unquoted expression is compiled normally using existing special form handling

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST recognize quasiquote expressions (backquote `` ` ``) and process them as template literals
- **FR-002**: Compiler MUST recognize unquote (`,`) and evaluate the following expression in the current lexical environment
- **FR-003**: Compiler MUST recognize unquote-splicing (`,@`) and splice the resulting list into the surrounding structure
- **FR-004**: Compiler MUST emit Wasm `local.get` instructions for unquoted local variable references
- **FR-005**: Compiler MUST emit Wasm code to construct cons cells and lists at runtime for quasiquote results
- **FR-006**: Compiler MUST correctly track quasiquote nesting depth to handle nested backquote/unquote combinations
- **FR-007**: Compiler MUST emit compile-time error for unquote outside of quasiquote context
- **FR-008**: Compiler MUST emit compile-time error for undefined variables in unquoted positions
- **FR-009**: Compiler MUST handle arbitrary expressions (not just variables) in unquoted positions
- **FR-010**: Compiler MUST preserve proper list structure including dotted pairs in quasiquote templates

### Key Entities

- **Quasiquote AST Node**: Represents a backquote expression with its template body; tracks nesting depth for nested quasiquotes
- **Unquote AST Node**: Represents a comma expression within quasiquote; contains the expression to evaluate
- **Unquote-Splicing AST Node**: Represents a comma-at expression; contains the list expression to splice
- **Lexical Environment**: Existing compiler structure tracking local variable bindings; queried to resolve unquoted variable references

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All quasiquote expressions with local variable references that previously failed with "Cannot compile quoted element" now compile successfully
- **SC-002**: Generated Wasm passes validation (`wasm-tools validate`) for all quasiquote compilation scenarios
- **SC-003**: Runtime execution of compiled quasiquote expressions produces correct list structures matching equivalent SBCL output
- **SC-004**: Stage 1 compilation rate improves by eliminating quasiquote-related compilation failures (baseline: track current quasiquote failure count before implementation)
- **SC-005**: No regression in compilation of non-quasiquote code paths

## Assumptions

- The existing lexical environment infrastructure correctly tracks local variable bindings and their Wasm local indices
- Runtime list construction primitives (cons, list) are already available and functional
- The reader/parser correctly converts backquote syntax to internal quasiquote representations
- The P464 (QUASIQUOTE) error pattern mentioned in CLAUDE.md has been partially addressed but quasiquote with local variables remains unsupported
