# Feature Specification: Tail Call Optimization

**Feature Branch**: `009-tail-call-optimization`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 2: 末尾呼び出し最適化（Tail Call Optimization）を実装する。目標はWebAssembly GCのreturn_call/return_call_ref命令を活用し、末尾再帰でスタックオーバーフローを防ぐこと。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Direct Tail Recursion (Priority: P1)

A developer writes a tail-recursive function like factorial with an accumulator pattern. When compiled to WebAssembly and executed, the function should handle arbitrarily large recursion depths (e.g., `(fact 10000)`) without stack overflow.

**Why this priority**: Direct tail recursion is the most common TCO use case and provides immediate value for writing idiomatic Lisp code. Without TCO, even simple recursive functions like factorial fail at relatively small inputs.

**Independent Test**: Can be fully tested by compiling `(defun fact-iter (n acc) (if (<= n 1) acc (fact-iter (- n 1) (* n acc))))` and verifying it computes `(fact-iter 10000 1)` without stack overflow.

**Acceptance Scenarios**:

1. **Given** a tail-recursive factorial function, **When** compiled and executed with input 100, **Then** the function returns the correct result without stack overflow
2. **Given** the same function, **When** the generated WAT is inspected, **Then** it contains `return_call` instruction instead of `call` for the recursive call
3. **Given** a tail-recursive fibonacci function with accumulator, **When** executed with large input (e.g., 1000), **Then** it completes without stack overflow

---

### User Story 2 - Indirect Tail Calls via funcall (Priority: P2)

A developer uses `funcall` in tail position to call a function stored in a variable or passed as a parameter. The compiler should generate `return_call_ref` instruction to optimize this case.

**Why this priority**: Higher-order functional programming frequently uses `funcall` in tail position (e.g., trampolines, continuation-passing style). This enables advanced functional patterns without stack growth.

**Independent Test**: Can be fully tested by compiling `(lambda (f x) (funcall f x))` and verifying the generated WAT contains `return_call_ref`.

**Acceptance Scenarios**:

1. **Given** a lambda `(lambda (f x) (funcall f x))`, **When** compiled, **Then** the generated WAT contains `return_call_ref` instead of `call_ref`
2. **Given** a trampoline-style function that calls closures in tail position, **When** executed with deep call chains, **Then** it completes without stack overflow

---

### User Story 3 - Mutual Recursion in labels (Priority: P3)

A developer writes mutually recursive local functions using `labels` (e.g., even?/odd? predicates). The compiler should apply TCO to calls between these local functions when they occur in tail position.

**Why this priority**: Mutual recursion is common in language processing (parsers, interpreters) and mathematical functions. Supporting TCO here enables writing clean mutually recursive algorithms.

**Independent Test**: Can be fully tested by compiling `(labels ((even? (n) (if (= n 0) t (odd? (- n 1)))) (odd? (n) (if (= n 0) nil (even? (- n 1))))) (even? 10000))` and verifying it completes without stack overflow.

**Acceptance Scenarios**:

1. **Given** mutually recursive even?/odd? functions in labels, **When** executed with input 10000, **Then** returns correct result without stack overflow
2. **Given** the same code, **When** the generated WAT is inspected, **Then** tail calls between local functions use `return_call_ref`

---

### Edge Cases

- What happens when a call is **not** in tail position (e.g., `(+ 1 (fact (- n 1)))`)?
  - The call should use regular `call` instruction, not `return_call`
- How does the system handle tail calls inside `unwind-protect`?
  - Calls within cleanup forms are not tail calls; the main form's tail call must execute cleanup first
- What happens with tail calls inside `catch` forms?
  - Tail calls inside `catch` are not true tail calls due to exception handling semantics
- How does the system handle multiple return values?
  - Initial implementation focuses on single-value returns; multi-value TCO is a future enhancement

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST identify tail positions in function bodies, including:
  - The last expression of `progn`
  - Both branches of `if` when `if` is in tail position
  - The body (last form) of `let`/`let*` when in tail position
  - The return value expression of `block`/`return-from` when in tail position

- **FR-002**: Compiler MUST mark AST nodes that are in tail position with a tail-position flag during analysis

- **FR-003**: Compiler MUST generate `return_call` instruction (instead of `call`) for direct function calls in tail position

- **FR-004**: Compiler MUST generate `return_call_ref` instruction (instead of `call_ref`) for `funcall` calls in tail position

- **FR-005**: Compiler MUST generate `return_call_ref` instruction for local function calls (from `flet`/`labels`) when in tail position

- **FR-006**: Compiler MUST NOT apply TCO to calls that are not in tail position, preserving correct semantics

- **FR-007**: Compiler MUST NOT apply TCO to calls within `catch` or `unwind-protect` cleanup forms, as these require stack preservation for exception handling

- **FR-008**: Generated WebAssembly MUST be valid and executable by wasmtime with tail-call extension enabled

### Key Entities

- **Tail Position**: A syntactic position where a function call's result is immediately returned without further computation. The compiler marks these during AST traversal.

- **Tail Call Flag**: A boolean flag on compilation environment or AST node indicating whether the current expression is in tail position. Propagated during compilation.

- **return_call/return_call_ref**: WebAssembly instructions that perform a tail call by replacing the current call frame instead of creating a new one.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Tail-recursive functions (e.g., factorial with accumulator) can execute with recursion depths of at least 10,000 without stack overflow
- **SC-002**: Generated WebAssembly code contains `return_call` or `return_call_ref` instructions for calls identified as being in tail position
- **SC-003**: Non-tail calls continue to work correctly, producing correct results
- **SC-004**: All existing compiler tests continue to pass (no regressions)
- **SC-005**: WebAssembly validation passes when loaded by wasmtime (with tail-call extension)
