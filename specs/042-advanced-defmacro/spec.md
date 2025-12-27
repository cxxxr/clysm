# Feature Specification: Advanced Defmacro and Compile-Time Macro Expansion

**Feature Branch**: `042-advanced-defmacro`
**Created**: 2025-12-28
**Status**: Draft
**Input**: Phase 9C: 高度なdefmacroとコンパイル時マクロ展開を実装する。目標はWasm内でのマクロ定義とマクロ展開の完全サポート。&whole（マクロ呼び出しフォーム全体へのアクセス）、&environment（レキシカル環境へのアクセス）をサポートし、macroexpand/macroexpand-1をWasm内で実行可能にする。これによりClysm自身の36箇所のdefmacro使用がセルフコンパイル可能となり、セルフホスティング達成に向けたコンパイル率向上に貢献する。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Define Macros with &whole Parameter (Priority: P1)

As a Clysm developer, I can define macros that access the entire macro call form using the `&whole` parameter, enabling advanced macro transformations that require knowledge of the complete source form.

**Why this priority**: The `&whole` parameter is essential for ANSI Common Lisp compliance and is used by many standard macros. Without it, Clysm cannot support macros that need to examine or manipulate the original form (e.g., for error reporting with source location or advanced transformation patterns).

**Independent Test**: Can be fully tested by defining a macro with `&whole`, invoking it, and verifying the macro receives the complete call form including the macro name.

**Acceptance Scenarios**:

1. **Given** a macro definition with `&whole form` as the first lambda-list element, **When** the macro is invoked with `(my-macro arg1 arg2)`, **Then** the `form` parameter receives the complete list `(my-macro arg1 arg2)`.
2. **Given** a macro using `&whole` for source-location-aware error messages, **When** the macro detects an error in its arguments, **Then** the error message includes the original macro call form.
3. **Given** a macro that uses both `&whole` and destructuring parameters, **When** invoked, **Then** both the whole form and individual parameters are correctly bound.

---

### User Story 2 - Define Macros with &environment Parameter (Priority: P1)

As a Clysm developer, I can define macros that access the lexical environment using the `&environment` parameter, enabling macros to query or utilize compile-time environment information.

**Why this priority**: The `&environment` parameter is critical for macros that need to check whether symbols are locally bound (macros, symbol-macros, or variables) at macro-expansion time. This is required for correct implementation of standard macros like `setf` expanders that need environment introspection.

**Independent Test**: Can be fully tested by defining a macro with `&environment`, and verifying it can query information about locally defined macros in the expansion environment.

**Acceptance Scenarios**:

1. **Given** a macro definition with `&environment env` in its lambda-list, **When** the macro is invoked, **Then** the `env` parameter receives an environment object representing the lexical environment at the point of macro call.
2. **Given** a macro using `&environment` to check if a symbol has a local macro definition, **When** invoked in a context with `macrolet`-defined local macros, **Then** the macro can correctly detect those local macro bindings.
3. **Given** a macro that passes its environment to `macroexpand-1`, **When** expanding a form that contains a locally-defined macro, **Then** the local macro definition is used for expansion.

---

### User Story 3 - Use macroexpand at Runtime (Priority: P2)

As a Clysm developer, I can use `macroexpand` and `macroexpand-1` functions within compiled code to expand macro forms at runtime, enabling dynamic code generation and meta-programming patterns.

**Why this priority**: Runtime macro expansion enables advanced meta-programming patterns such as code generation, DSL implementation, and debugging tools. While less common than compile-time expansion, it is essential for self-hosting since the compiler itself uses these functions.

**Independent Test**: Can be fully tested by calling `macroexpand-1` on a list containing a macro call and verifying the returned form is the expanded result.

**Acceptance Scenarios**:

1. **Given** a globally defined macro `my-macro`, **When** `(macroexpand-1 '(my-macro arg))` is evaluated, **Then** it returns the expanded form and T as the second value.
2. **Given** a form that is not a macro call, **When** `(macroexpand-1 form)` is evaluated, **Then** it returns the original form unchanged and NIL as the second value.
3. **Given** a macro that expands to another macro call, **When** `(macroexpand '(outer-macro arg))` is called, **Then** it repeatedly expands until the result is no longer a macro call.

---

### User Story 4 - Self-Compile Clysm's Defmacro Definitions (Priority: P2)

As a Clysm maintainer, I can compile Clysm's own 36 `defmacro` definitions using the Clysm compiler, advancing toward self-hosting capability.

**Why this priority**: Self-hosting is a major milestone demonstrating compiler maturity. The 36 `defmacro` forms in Clysm's source are currently blocking self-compilation. Successfully compiling them proves the macro system is complete enough to bootstrap the compiler.

**Independent Test**: Can be tested by running the compiler on its own source files containing `defmacro` and verifying they compile without errors and the resulting macros function correctly.

**Acceptance Scenarios**:

1. **Given** Clysm source files containing `defmacro` definitions, **When** compiled by the Clysm compiler, **Then** all 36 defmacro forms compile successfully without errors.
2. **Given** a self-compiled macro from Clysm's source, **When** invoked during subsequent compilation, **Then** it produces the same expansion as when compiled by the host compiler.
3. **Given** macros using `&whole` or `&environment` in Clysm's source, **When** self-compiled, **Then** they retain full functionality including access to whole forms and environment introspection.

---

### User Story 5 - Macro Expansion Error Reporting (Priority: P3)

As a Clysm developer, I receive clear error messages when macro expansion fails, including the original macro call form and the location of the error within the macro definition.

**Why this priority**: Good error messages dramatically improve developer productivity when debugging macro-related issues. While not strictly required for functionality, it significantly enhances usability.

**Independent Test**: Can be tested by intentionally triggering a macro expansion error and verifying the error message includes the macro name, original form, and helpful context.

**Acceptance Scenarios**:

1. **Given** a macro that signals an error during expansion, **When** the macro is invoked, **Then** the error message includes the complete macro call form (via `&whole` if used).
2. **Given** an incorrect number of arguments to a macro, **When** the macro is invoked, **Then** the error clearly states the expected vs. provided argument count.
3. **Given** a macro expansion that fails due to invalid argument types, **When** expansion fails, **Then** the error indicates which argument was invalid.

---

### Edge Cases

- What happens when `&whole` is used in a position other than before the first parameter? (Should signal a compile-time error)
- How does the system handle `&environment` in macros defined at runtime via `eval`? (Should work but environment may be minimal)
- What happens when `macroexpand` is called on a form where the macro is not defined? (Returns form unchanged with NIL second value)
- How are circular macro expansions detected? (Stack overflow or explicit limit with error)
- What happens when a macro expander function signals an error? (Propagates as compile-time error with context)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support `&whole` as the first element in a macro lambda-list, binding the complete macro call form.
- **FR-002**: System MUST signal a compile-time error if `&whole` appears anywhere except before all other parameters.
- **FR-003**: System MUST support `&environment` in macro lambda-lists, binding a lexical environment object.
- **FR-004**: System MUST allow `&environment` to appear in any position in the lambda-list (per ANSI CL).
- **FR-005**: System MUST provide `macroexpand-1` function that expands a form once if it is a macro call.
- **FR-006**: System MUST provide `macroexpand` function that repeatedly expands until the result is not a macro call.
- **FR-007**: Both `macroexpand` and `macroexpand-1` MUST accept an optional environment argument.
- **FR-008**: Both `macroexpand` and `macroexpand-1` MUST return two values: the expanded form and a boolean indicating whether expansion occurred.
- **FR-009**: System MUST support `macro-function` to retrieve the expander function for a macro symbol.
- **FR-010**: System MUST support `(setf macro-function)` to programmatically define or redefine macros.
- **FR-011**: The environment object MUST support querying local macro definitions (from `macrolet`).
- **FR-012**: System MUST detect and report circular macro expansions with a clear error message.
- **FR-013**: Errors during macro expansion MUST include the original macro call form for debugging.
- **FR-014**: All 36 `defmacro` forms in Clysm's source code MUST compile successfully with this implementation.

### Key Entities

- **Macro Function**: A function that transforms a macro call form into an expanded form at compile time. Stored in the global macro registry indexed by symbol.
- **Lexical Environment**: An opaque object representing compile-time bindings including local macro definitions, symbol-macro definitions, and variable declarations.
- **Macro Lambda-List**: A destructuring parameter specification that can include `&whole`, `&environment`, `&optional`, `&rest`, `&key`, `&body`, and `&aux`.
- **Macro Registry**: A global mapping from macro names to their expander functions, supporting lookup and modification at both compile-time and runtime.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All macros using `&whole` can access the complete macro call form, verified by test suite covering common patterns.
- **SC-002**: All macros using `&environment` can query local macro bindings, verified by test suite with `macrolet` scenarios.
- **SC-003**: `macroexpand` and `macroexpand-1` execute correctly in compiled code, verified by runtime invocation tests.
- **SC-004**: 100% of Clysm's 36 `defmacro` definitions compile successfully without errors.
- **SC-005**: Self-compiled macros produce identical expansions compared to host-compiled versions for the same input.
- **SC-006**: Macro expansion errors include source form information in 100% of error cases.
- **SC-007**: Circular macro expansion is detected within 1000 expansion steps and reports a clear error.

## Assumptions

- The existing Feature 016 macro system provides the foundation for macro registration and basic `defmacro` without `&whole`/`&environment`.
- Environment introspection is limited to macro definitions (not variable bindings) as that is sufficient for ANSI CL compliance.
- The 36 `defmacro` count is based on current Clysm source and may vary slightly with ongoing development.
- Runtime `macroexpand` operates on the global macro registry; local macros require explicit environment passing.

## Dependencies

- **Feature 016 (Macro System)**: Provides base `defmacro` implementation and macro registry.
- **Feature 017 (Eval/JIT)**: Required for runtime macro expansion in compiled code.
- **Feature 025 (Multiple Values)**: Required for `macroexpand` returning two values.
