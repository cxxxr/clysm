# Feature Specification: Macro System (Lisp-4)

**Feature Branch**: `016-macro-system`
**Created**: 2025-12-24
**Status**: Draft
**Input**: Phase 5 - Implement macro system with defmacro compile-time macro definition and backquote syntax support

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Define and Use Custom Macros (Priority: P1)

A Lisp developer defines a custom macro using `defmacro` and uses it in their code. The compiler detects the macro call, executes the macro function at compile time (on the host SBCL), and recursively processes the expanded result for compilation.

**Why this priority**: This is the core capability of the macro system. Without defmacro working, no other macro functionality is possible.

**Independent Test**: Can be tested by defining a simple macro like `(defmacro my-when (test &body body) \`(if ,test (progn ,@body)))` and verifying `(my-when t 1 2 3)` compiles and returns 3.

**Acceptance Scenarios**:

1. **Given** a defmacro definition for `my-when`, **When** the developer writes `(my-when t 1 2 3)`, **Then** the compiler expands it to `(if t (progn 1 2 3))` and the compiled code returns 3
2. **Given** a macro that generates another macro call, **When** compiling code using that macro, **Then** the compiler recursively expands until no more macro calls remain
3. **Given** a defmacro with destructuring lambda list, **When** the developer uses pattern matching in macro parameters, **Then** the macro correctly destructures the arguments

---

### User Story 2 - Use Backquote Syntax for Template Code (Priority: P1)

A Lisp developer uses backquote syntax (`, ,, ,@) to construct code templates in macro definitions. The reader or compiler transforms backquote expressions into proper list construction forms.

**Why this priority**: Backquote is essential for practical macro writing. Without it, macros become extremely verbose and error-prone.

**Independent Test**: Can be tested by evaluating expressions like `` `(a ,b ,@c) `` and verifying the result matches `(list 'a b (append c nil))` semantics.

**Acceptance Scenarios**:

1. **Given** a backquote expression `` `(a b c) ``, **When** compiled, **Then** it produces the list `(a b c)`
2. **Given** a backquote with unquote `` `(a ,x b) `` where x=1, **When** evaluated, **Then** it produces `(a 1 b)`
3. **Given** a backquote with unquote-splicing `` `(a ,@xs b) `` where xs=(1 2), **When** evaluated, **Then** it produces `(a 1 2 b)`
4. **Given** nested backquotes, **When** compiled, **Then** each level of quoting/unquoting is correctly handled

---

### User Story 3 - Use Standard Control Flow Macros (Priority: P2)

A Lisp developer uses standard control flow macros (`when`, `unless`, `cond`, `case`) that come built-in with the compiler. These provide familiar Lisp idioms for conditional execution.

**Why this priority**: Standard macros demonstrate the macro system works and provide essential developer conveniences, but depend on defmacro and backquote working first.

**Independent Test**: Can be tested by writing `(when t 1 2 3)` and verifying it returns 3, or `(unless nil 42)` returning 42.

**Acceptance Scenarios**:

1. **Given** the expression `(when t 1 2 3)`, **When** compiled and executed, **Then** it returns 3
2. **Given** the expression `(when nil 1 2 3)`, **When** compiled and executed, **Then** it returns nil
3. **Given** the expression `(unless nil 1 2 3)`, **When** compiled and executed, **Then** it returns 3
4. **Given** a `cond` with multiple clauses, **When** the second clause is true, **Then** it returns the result of that clause's body
5. **Given** a `case` expression, **When** a key matches, **Then** it returns the corresponding value

---

### User Story 4 - Use Iteration Macros (Priority: P2)

A Lisp developer uses iteration macros (`dolist`, `dotimes`, `do`) for common looping patterns. These compile into efficient loop constructs.

**Why this priority**: Iteration macros are frequently used but can be implemented after control flow macros are working.

**Independent Test**: Can be tested by `(let ((sum 0)) (dotimes (i 5) (incf sum i)) sum)` returning 10.

**Acceptance Scenarios**:

1. **Given** `(dotimes (i 5 i))`, **When** executed, **Then** it returns 5 (the result form)
2. **Given** `(dolist (x '(1 2 3)) (print x))`, **When** executed, **Then** it iterates over each element
3. **Given** a `do` loop with step forms, **When** the end condition becomes true, **Then** it returns the result forms

---

### User Story 5 - Inspect Macro Expansions (Priority: P3)

A Lisp developer uses `macroexpand-1` to see how a macro call expands to understand or debug macro behavior.

**Why this priority**: Debugging aid that helps developers but is not critical for macro functionality.

**Independent Test**: Can be tested by `(macroexpand-1 '(when t 1))` returning `(if t (progn 1))`.

**Acceptance Scenarios**:

1. **Given** a macro call form, **When** `macroexpand-1` is called, **Then** it returns the single-step expansion
2. **Given** a non-macro form, **When** `macroexpand-1` is called, **Then** it returns the form unchanged
3. **Given** `macroexpand` (full expansion), **When** called on nested macros, **Then** it recursively expands until no macros remain

---

### Edge Cases

- What happens when a macro is used before it is defined?
  - The compiler reports an "undefined macro" error at compile time
- How does the system handle infinite macro expansion (macro that expands to itself)?
  - The compiler detects expansion depth exceeding a reasonable limit and signals an error
- What happens when backquote nesting depth exceeds expected levels?
  - Nested backquotes are correctly tracked and processed regardless of depth
- How are macro lambda list errors handled (wrong number of arguments)?
  - The compiler reports a clear error indicating expected vs. actual arguments

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support `defmacro` for defining compile-time macros with name, lambda-list, and body
- **FR-002**: System MUST detect macro calls during form traversal by checking if a symbol names a macro
- **FR-003**: System MUST execute macro functions on the host SBCL at compile time (cross-compilation)
- **FR-004**: System MUST recursively process macro expansion results until no macro calls remain
- **FR-005**: System MUST transform backquote syntax in the reader or compiler:
  - `` `form `` transforms to `(quasiquote form)`
  - `,expr` transforms to `(unquote expr)`
  - `,@expr` transforms to `(unquote-splicing expr)`
- **FR-006**: System MUST maintain a compile-time environment for macro function registration and lookup
- **FR-007**: System MUST provide `macroexpand-1` to show single-step macro expansion
- **FR-008**: System MUST provide `macroexpand` for full recursive expansion
- **FR-009**: System MUST implement standard control macros: `when`, `unless`, `cond`, `case`
- **FR-010**: System MUST implement sequence macros: `prog1`, `prog2`
- **FR-011**: System MUST implement iteration macros: `dolist`, `dotimes`, `do`
- **FR-012**: System MUST support destructuring in macro lambda lists
- **FR-013**: System MUST report clear errors for undefined macros at compile time
- **FR-014**: System MUST detect and report macro expansion depth exceeding limits

### Key Entities

- **Macro Definition**: Represents a named macro with its lambda-list and body; stored in compile-time environment
- **Compile-Time Environment**: Registry mapping macro names to their expansion functions; supports hierarchical lookup
- **Quasiquote Form**: Intermediate representation of backquote expressions before conversion to list construction
- **Expansion Result**: The output of applying a macro function to its arguments

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All standard macros (`when`, `unless`, `cond`, `case`, `dolist`, `dotimes`, `do`, `prog1`, `prog2`) compile and execute correctly
- **SC-002**: `(when t 1 2 3)` evaluates to 3
- **SC-003**: Nested backquotes at any depth produce correct results
- **SC-004**: `macroexpand-1` correctly shows expansion results for all defined macros
- **SC-005**: User-defined macros with `defmacro` work identically to built-in macros
- **SC-006**: Macro errors (undefined, wrong arguments, infinite expansion) produce clear, actionable error messages
- **SC-007**: All macro-using code compiles within reasonable time (no performance regression from macro expansion)

## Assumptions

- The host SBCL environment is available during compilation for executing macro functions
- Existing compiler infrastructure for function calls and special forms is in place
- The reader already supports basic s-expression parsing; backquote syntax may be added at reader or compiler level
- Sequence and iteration operations (`progn`, `if`, `let`, etc.) are already implemented
