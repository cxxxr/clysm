# Feature Specification: LOOP Macro Extension

**Feature Branch**: `001-loop-extension`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-5: LOOP拡張を実装する。目標はコンパイラで使用される全LOOPパターンのサポート。主要機能: (1) hash-tableイテレーション - being the hash-keys, being the hash-values, using (hash-value v)構文、(2) with句 - ローカル変数初期化、(3) finally句 - 終了時処理とreturn、(4) intoアキュムレータ - 中間結果への蓄積。検証基準: ANSIテストiterationカテゴリ50%+パス、コンパイラ内の40箇所のhash-tableイテレーションがコンパイル可能。"

## Overview

This feature extends the LOOP macro implementation to support additional ANSI Common Lisp constructs required for self-hosting the Clysm compiler. The compiler source code extensively uses LOOP patterns for hash-table iteration, local variable binding, cleanup actions, and result accumulation. Without these extensions, approximately 40 hash-table iteration patterns in the compiler cannot be compiled to WebAssembly.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Hash-Table Iteration (Priority: P1)

A compiler developer needs to iterate over hash-table contents during compilation, such as when processing symbol tables, type registries, or cached definitions. The LOOP macro must support iterating over hash-table keys, values, or both simultaneously.

**Why this priority**: Hash-table iteration is the most frequently used LOOP pattern in the compiler (40 occurrences). Without this, a significant portion of the compiler cannot be compiled to Wasm.

**Independent Test**: Can be fully tested by compiling and running LOOP expressions that iterate over hash-tables and verifying correct key/value enumeration.

**Acceptance Scenarios**:

1. **Given** a hash-table with entries, **When** using `(loop for k being the hash-keys of ht ...)`, **Then** iterates over all keys in the table
2. **Given** a hash-table with entries, **When** using `(loop for v being the hash-values of ht ...)`, **Then** iterates over all values in the table
3. **Given** a hash-table with entries, **When** using `(loop for k being the hash-keys of ht using (hash-value v) ...)`, **Then** iterates with both key and value accessible in each iteration
4. **Given** an empty hash-table, **When** using any hash-table iteration form, **Then** the loop body executes zero times and returns nil (or the specified default)

---

### User Story 2 - Local Variable Initialization with WITH (Priority: P2)

A developer needs to establish local bindings within a LOOP for intermediate calculations, counters, or state tracking that doesn't change during iteration.

**Why this priority**: The `with` clause enables cleaner code by avoiding external `let` bindings. It's commonly used alongside iteration clauses in the compiler.

**Independent Test**: Can be fully tested by running LOOP expressions with `with` clauses and verifying variable bindings are correctly initialized and accessible.

**Acceptance Scenarios**:

1. **Given** a LOOP with `with var = initial-value`, **When** the loop executes, **Then** the variable is bound to the initial value before iteration begins
2. **Given** a LOOP with `with var1 = val1 and var2 = val2`, **When** the loop executes, **Then** both variables are bound in parallel (like `let`)
3. **Given** a LOOP with `with var = expr` where expr references a previous `with` variable, **When** the loop executes, **Then** the bindings are established sequentially (like `let*`)
4. **Given** a LOOP with `with var` (no initializer), **When** the loop executes, **Then** the variable is bound to nil

---

### User Story 3 - Cleanup Actions with FINALLY (Priority: P2)

A developer needs to execute cleanup code or return a specific value after a loop completes, regardless of how it terminates (normal completion or early exit via `return`).

**Why this priority**: The `finally` clause is essential for resource cleanup and computing final results from accumulated state. Several compiler routines rely on finally for result construction.

**Independent Test**: Can be fully tested by running LOOP expressions with `finally` clauses and verifying the cleanup code executes and return values are correct.

**Acceptance Scenarios**:

1. **Given** a LOOP with `finally (return result)`, **When** the loop completes normally, **Then** the finally clause executes and returns the specified value
2. **Given** a LOOP with `finally (cleanup-form)`, **When** the loop completes, **Then** the cleanup form is executed
3. **Given** a LOOP with multiple forms in finally, **When** the loop completes, **Then** all forms execute in order
4. **Given** a LOOP with `finally` but no explicit `return`, **When** the loop completes, **Then** the loop returns nil after executing finally forms

---

### User Story 4 - Accumulation into Named Variables with INTO (Priority: P3)

A developer needs to accumulate results into a named variable rather than using the implicit accumulator, enabling multiple accumulations in a single loop or access to intermediate results.

**Why this priority**: The `into` modifier allows building multiple result lists or accessing accumulated values within the loop body. Used in compiler passes that build multiple output structures simultaneously.

**Independent Test**: Can be fully tested by running LOOP expressions with `into` clauses and verifying correct accumulation into named variables.

**Acceptance Scenarios**:

1. **Given** a LOOP with `collect item into result-list`, **When** the loop completes, **Then** result-list contains all collected items
2. **Given** a LOOP with multiple `into` clauses (e.g., `collect x into list1` and `sum y into total`), **When** the loop completes, **Then** each named accumulator contains its respective accumulated value
3. **Given** a LOOP with `into` and `finally (return ...)`, **When** the loop completes, **Then** the named accumulator variables are accessible in the finally clause
4. **Given** a LOOP with `into result` where result is also used in the loop body, **When** executing, **Then** the current accumulated value is accessible during iteration

---

### Edge Cases

- What happens when iterating over a hash-table that is modified during iteration?
  - **Assumption**: Behavior is undefined per ANSI CL; no special handling required
- How does the system handle `with` variable names that shadow outer bindings?
  - The inner binding takes precedence within the loop scope
- What happens when `finally` contains a `return-from` to an outer block?
  - The `return-from` takes precedence and exits the enclosing block
- How does `into` interact with `count`, `sum`, `maximize`, and `minimize`?
  - All accumulation clauses support `into` with the same semantics as `collect`
- What happens when the same variable name is used in multiple `into` clauses with different accumulation types?
  - This is an error condition; compilation should fail with a clear diagnostic

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support `being the hash-keys of` syntax for iterating over hash-table keys
- **FR-002**: System MUST support `being the hash-values of` syntax for iterating over hash-table values
- **FR-003**: System MUST support `using (hash-value var)` syntax for simultaneous key-value access
- **FR-004**: System MUST support `using (hash-key var)` syntax when iterating by values
- **FR-005**: System MUST support `with var = initial-value` for local variable initialization
- **FR-006**: System MUST support `with var1 = val1 and var2 = val2` for parallel bindings
- **FR-007**: System MUST support sequential `with` bindings where later bindings can reference earlier ones
- **FR-008**: System MUST support `finally` clause for post-loop cleanup and result computation
- **FR-009**: System MUST support multiple forms within a `finally` clause
- **FR-010**: System MUST support `return` within `finally` to specify the loop's return value
- **FR-011**: System MUST support `into variable` modifier for all accumulation clauses (collect, append, nconc, count, sum, maximize, minimize)
- **FR-012**: System MUST make `into` variables accessible within the loop body and `finally` clause
- **FR-013**: System MUST detect and report errors when the same variable is used in conflicting `into` clauses

### Key Entities

- **LOOP Clause**: A syntactic component of the LOOP macro (iteration, accumulation, termination, or variable binding)
- **Hash-Table Iterator**: State tracking for hash-table enumeration (current position, remaining entries)
- **Accumulator Variable**: Named or implicit variable holding accumulated results during loop execution
- **Finally Block**: Code sequence executed after loop termination, before returning

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: ANSI CL iteration test category achieves 50% or higher pass rate
- **SC-002**: All 40 hash-table iteration patterns in the compiler source compile successfully
- **SC-003**: Existing LOOP functionality (for/in, collect, append, do, when, until, while) continues to work without regression
- **SC-004**: Compilation of LOOP forms with new clauses produces valid Wasm that passes validation
- **SC-005**: Runtime execution of compiled LOOP forms produces results identical to SBCL interpreter

## Assumptions

- Hash-table iteration order is unspecified (matching ANSI CL behavior)
- The `with` clause follows `let*` semantics when bindings appear sequentially (without `and`)
- The `finally` clause executes exactly once, after all iterations complete
- Variables introduced by `into` are initialized to appropriate default values (nil for collect/append/nconc, 0 for count/sum, appropriate extrema for maximize/minimize)
- Loop variable scope follows ANSI CL: iteration variables and `with` variables are local to the loop
