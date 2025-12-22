# Feature Specification: Special Variables Compiler Integration

**Feature Branch**: `002-special-vars-compiler`
**Created**: 2025-12-22
**Status**: Draft
**Input**: User description: "Clysm WasmGC Common Lispコンパイラにスペシャル変数（動的スコープ変数）のコンパイラ統合を実装する"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Define and Access Global Special Variables (Priority: P1)

A compiler user wants to define global special variables using `defvar` and `defparameter`, then access their values throughout their program. This is the foundation of Common Lisp's dynamic scoping model and is required for macros like `gensym` that depend on `*gensym-counter*`.

**Why this priority**: This is the core functionality that enables all other special variable features. Without the ability to define and access special variables, the macro system cannot function properly.

**Independent Test**: Can be fully tested by compiling `(defvar *x* 10) *x*` and verifying it returns 10. This alone provides value by enabling global configuration variables.

**Acceptance Scenarios**:

1. **Given** a fresh compilation environment, **When** `(defvar *my-var* 42)` is compiled and executed, **Then** the symbol `*my-var*` has value 42 accessible via symbol-value
2. **Given** `*my-var*` is already defined with value 42, **When** `(defvar *my-var* 100)` is compiled and executed, **Then** the value remains 42 (defvar does not reinitialize)
3. **Given** a fresh compilation environment, **When** `(defparameter *config* "initial")` is compiled and executed, **Then** the symbol has value "initial"
4. **Given** `*config*` is already defined with value "initial", **When** `(defparameter *config* "updated")` is compiled and executed, **Then** the value becomes "updated" (defparameter always reinitializes)

---

### User Story 2 - Dynamic Binding with LET (Priority: P1)

A compiler user wants to temporarily rebind special variables within a lexical scope using `let`, with automatic restoration when exiting the scope. This enables patterns like temporarily overriding output streams or configuration values.

**Why this priority**: Dynamic binding is essential for Common Lisp's dynamic scoping model. Many standard patterns (like `with-output-to-string`, `with-standard-io-syntax`) rely on this behavior.

**Independent Test**: Can be tested by compiling `(defvar *x* 10) (let ((*x* 20)) *x*)` and verifying it returns 20, then verifying `*x*` is 10 outside the let.

**Acceptance Scenarios**:

1. **Given** `(defvar *x* 10)` is defined, **When** `(let ((*x* 20)) *x*)` is compiled and executed, **Then** it returns 20
2. **Given** `(defvar *x* 10)` is defined, **When** `(let ((*x* 20)) *x*)` completes, **Then** accessing `*x*` outside the let returns 10
3. **Given** `(defvar *x* 10)` is defined, **When** nested lets `(let ((*x* 20)) (let ((*x* 30)) *x*))` is compiled, **Then** it returns 30 and each scope properly restores
4. **Given** a special variable binding, **When** an error occurs inside the let body, **Then** the original value is restored (integration with unwind-protect)

---

### User Story 3 - Lexical vs Special Variable Discrimination (Priority: P1)

A compiler user wants the compiler to automatically distinguish between lexical variables (default) and special variables (declared via defvar/defparameter), generating appropriate code for each.

**Why this priority**: Correct discrimination is fundamental to Common Lisp semantics. Mixing up lexical and dynamic scope would cause subtle, hard-to-debug program errors.

**Independent Test**: Can be tested by compiling code that has both lexical `y` and special `*x*` and verifying they behave correctly in nested contexts.

**Acceptance Scenarios**:

1. **Given** `(defvar *x* 1)` and `(let ((y 2)) ...)`, **When** referencing `*x*` inside let, **Then** it uses symbol-value access
2. **Given** `(defvar *x* 1)` and `(let ((y 2)) ...)`, **When** referencing `y` inside let, **Then** it uses local variable access
3. **Given** both `*x*` (special) and `y` (lexical) exist, **When** `(setq *x* 5)` is compiled, **Then** it modifies the symbol's value field
4. **Given** both `*x*` (special) and `y` (lexical) exist, **When** `(setq y 5)` is compiled, **Then** it modifies the local variable

---

### User Story 4 - Gensym Support (Priority: P2)

A compiler user wants `gensym` to generate unique symbols using an internal counter (`*gensym-counter*`). This is critical for macro hygiene and is a common use case for special variables.

**Why this priority**: Gensym is essential for writing hygienic macros. While not strictly required for basic compilation, it's the primary motivation for implementing special variables.

**Independent Test**: Can be tested by calling `(gensym)` twice and verifying each returns a unique symbol with incrementing suffix.

**Acceptance Scenarios**:

1. **Given** `*gensym-counter*` is initialized to 0, **When** `(gensym)` is called, **Then** it returns a symbol like `G0` and increments counter
2. **Given** `*gensym-counter*` is at value N, **When** `(gensym)` is called, **Then** it returns `GN` and counter becomes N+1
3. **Given** a custom prefix, **When** `(gensym "MY-")` is called, **Then** it returns a symbol like `MY-N`

---

### User Story 5 - Exception-Safe Dynamic Binding (Priority: P2)

A compiler user wants dynamic bindings to be properly restored even when non-local exits (throw, return-from, errors) occur within the binding scope.

**Why this priority**: Safety and correctness. Without proper cleanup, special variables could leak incorrect values after exceptions.

**Independent Test**: Can be tested by setting up a dynamic binding, throwing from within, catching outside, and verifying the original value is restored.

**Acceptance Scenarios**:

1. **Given** `(defvar *x* 10)`, **When** `(catch 'exit (let ((*x* 20)) (throw 'exit *x*)))` executes, **Then** `*x*` is 10 after the catch
2. **Given** `(defvar *x* 10)`, **When** `(block nil (let ((*x* 20)) (return-from nil *x*)))` executes, **Then** `*x*` is 10 after the block

---

### Edge Cases

- What happens when accessing an unbound special variable? The system signals an unbound-variable error at runtime.
- What happens when defvar has no initial value form? The variable is declared special but remains unbound until explicitly set.
- What happens with nested dynamic bindings of the same variable? Each binding creates a new stack frame; restoration proceeds in LIFO order.
- What happens when a closure captures a special variable reference? The closure accesses the dynamic binding active at call time, not definition time.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST parse `defvar` forms and create appropriate AST nodes representing global special variable declarations
- **FR-002**: System MUST parse `defparameter` forms and create appropriate AST nodes representing global special variable declarations with mandatory initialization
- **FR-003**: System MUST maintain a registry of declared special variables during compilation
- **FR-004**: System MUST generate code that stores initial values in symbol's value slot for defvar (only if unbound) and defparameter (always)
- **FR-005**: System MUST distinguish special variables from lexical variables during variable reference compilation
- **FR-006**: System MUST generate symbol-value access code for special variable references instead of local variable access
- **FR-007**: System MUST detect when a `let` binding shadows a special variable and generate dynamic binding code
- **FR-008**: System MUST generate binding frame creation on entry to dynamic let bindings
- **FR-009**: System MUST generate binding frame restoration code on normal exit from dynamic let bindings
- **FR-010**: System MUST integrate dynamic binding restoration with unwind-protect for exception safety
- **FR-011**: System MUST compile `setq` of special variables to symbol-value modification

### Key Entities *(include if feature involves data)*

- **Special Variable Registry**: A compilation-time data structure mapping symbol names to their special variable status. Persists across compilation units.
- **Binding Frame**: A runtime structure that stores (symbol, old-value) pairs for dynamic bindings. Implemented as linked list in WasmGC.
- **Symbol Structure**: Existing $symbol WasmGC struct with name, value, function, and plist fields. The value field is used for special variable storage.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Programs using `(defvar *x* 10) (let ((*x* 20)) *x*)` return 20 inside let and 10 outside let
- **SC-002**: Programs using `(defparameter *y* 5)` update the value on re-evaluation (unlike defvar)
- **SC-003**: `gensym` produces unique symbols with incrementing numeric suffixes
- **SC-004**: Dynamic bindings are correctly restored after exceptions (throw/catch, return-from/block)
- **SC-005**: Lexical and special variables coexist correctly in the same scope without interference
- **SC-006**: Nested dynamic bindings restore in correct LIFO order
- **SC-007**: All existing compiler tests continue to pass (no regression)

## Assumptions

- The existing $symbol WasmGC structure's value field is suitable for special variable storage
- The existing unwind-protect implementation can be extended for dynamic binding cleanup
- The binding frame linked list approach provides acceptable performance for typical Common Lisp programs
- The naming convention `*name*` (earmuffs) is a convention only; any symbol can be declared special via defvar/defparameter
