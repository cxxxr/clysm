# Feature Specification: ANSI Common Lisp Condition System

**Feature Branch**: `014-condition-system`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 8F: 条件システム（Condition System）を実装する。ANSI Common Lisp準拠の条件・リスタートシステムを構築し、構造化されたエラーハンドリングを可能にする。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Error Signaling and Handling (Priority: P1)

A developer writes code that may encounter errors and wants to handle them gracefully using the Common Lisp condition system. They use `handler-case` to catch specific error types and provide recovery logic.

**Why this priority**: Error handling is the most fundamental use case of the condition system. Without this, programs cannot gracefully recover from exceptional situations.

**Independent Test**: Can be fully tested by signaling an error with `error` function and catching it with `handler-case`, verifying that control transfers to the handler and returns the expected value.

**Acceptance Scenarios**:

1. **Given** a `handler-case` form with a handler for `type-error`, **When** a `type-error` is signaled inside the protected form, **Then** the type-error handler executes and returns its value
2. **Given** nested `handler-case` forms, **When** an error is signaled, **Then** the innermost matching handler is invoked
3. **Given** a `handler-case` form, **When** no error is signaled, **Then** the protected form's value is returned normally

---

### User Story 2 - Restart-Based Recovery (Priority: P1)

A developer wants to provide multiple recovery options when an error occurs. They use `restart-case` to establish restarts and `invoke-restart` to invoke them from error handlers.

**Why this priority**: Restarts are what distinguish Common Lisp's condition system from simple exception handling. They allow separation of error detection from error recovery.

**Independent Test**: Can be fully tested by establishing a restart with `restart-case`, signaling an error, and invoking the restart from a handler to verify recovery works correctly.

**Acceptance Scenarios**:

1. **Given** a `restart-case` form with an `use-value` restart, **When** an error is signaled and the restart is invoked with a value, **Then** the restart returns that value as the result
2. **Given** multiple restarts established, **When** `compute-restarts` is called, **Then** all visible restarts are returned
3. **Given** a named restart, **When** `find-restart` is called with that name, **Then** the restart object is returned

---

### User Story 3 - Warning and Non-Fatal Conditions (Priority: P2)

A developer wants to signal conditions that are not necessarily fatal. They use `warn` to signal warnings and optionally muffle them.

**Why this priority**: Warnings provide a way to communicate non-fatal issues without stopping program execution, which is important for robust programs.

**Independent Test**: Can be fully tested by calling `warn`, observing the warning is signaled, and using `muffle-warning` restart to suppress it.

**Acceptance Scenarios**:

1. **Given** a `warn` call, **When** no handler intervenes, **Then** the warning message is output and execution continues
2. **Given** a handler that invokes `muffle-warning`, **When** a warning is signaled, **Then** the warning is suppressed and execution continues silently
3. **Given** a `simple-warning` with format arguments, **When** signaled, **Then** the formatted message is available

---

### User Story 4 - Continuable Errors with cerror (Priority: P2)

A developer wants to signal an error that can be continued from. They use `cerror` to provide a continue format string and allow the `continue` restart to proceed.

**Why this priority**: Continuable errors allow interactive debugging and automated recovery for errors that can reasonably be continued from.

**Independent Test**: Can be fully tested by calling `cerror`, invoking the `continue` restart, and verifying execution proceeds normally.

**Acceptance Scenarios**:

1. **Given** a `cerror` call with a continue format string, **When** the `continue` restart is invoked, **Then** execution continues after the `cerror` form
2. **Given** a `cerror` call, **When** no handler handles it and no restart is invoked, **Then** the error is raised to the debugger

---

### User Story 5 - Handler-Bind for Non-Transferring Handlers (Priority: P2)

A developer wants to establish handlers that can inspect conditions without necessarily handling them (declining to transfer control). They use `handler-bind` instead of `handler-case`.

**Why this priority**: Handler-bind enables advanced patterns like logging conditions before they propagate, or conditionally handling based on condition properties.

**Independent Test**: Can be fully tested by establishing a handler-bind that logs and declines, verifying the condition propagates to outer handlers.

**Acceptance Scenarios**:

1. **Given** a `handler-bind` handler that returns normally, **When** a condition is signaled, **Then** the condition propagates to outer handlers
2. **Given** a `handler-bind` handler that invokes a restart, **When** a condition is signaled, **Then** control transfers via the restart
3. **Given** multiple `handler-bind` handlers for the same condition type, **When** a condition is signaled, **Then** handlers are tried in order from innermost to outermost

---

### User Story 6 - Standard Restarts (Priority: P3)

A developer uses standard restarts (`abort`, `continue`, `use-value`, `store-value`, `muffle-warning`) in their code for common recovery patterns.

**Why this priority**: Standard restarts provide conventional recovery options that tools and users expect, but most core functionality works without them.

**Independent Test**: Can be fully tested by establishing each standard restart and verifying it can be found and invoked with expected behavior.

**Acceptance Scenarios**:

1. **Given** an `abort` restart established, **When** invoked, **Then** it performs a non-local exit
2. **Given** a `use-value` restart, **When** invoked with a value, **Then** that value is used in place of the erroneous one
3. **Given** a `store-value` restart, **When** invoked with a value, **Then** that value is stored for future use

---

### User Story 7 - With-Simple-Restart Convenience Macro (Priority: P3)

A developer wants a simple way to establish a restart that just returns nil and a secondary value indicating whether it was used.

**Why this priority**: `with-simple-restart` is a convenience macro that simplifies common restart patterns but is not essential for core functionality.

**Independent Test**: Can be fully tested by wrapping code in `with-simple-restart`, invoking the restart, and verifying the return values.

**Acceptance Scenarios**:

1. **Given** a `with-simple-restart` form, **When** the code completes normally, **Then** the values are returned with nil as secondary value
2. **Given** a `with-simple-restart` form, **When** the restart is invoked, **Then** nil is returned with t as secondary value

---

### Edge Cases

- What happens when a handler signals another condition? The new condition should propagate through remaining handlers.
- How does the system handle recursive error signaling? Handlers are unbound while a handler is executing to prevent infinite recursion.
- What happens when `invoke-restart` is called with a restart not currently visible? An error of type `control-error` is signaled.
- How does `unwind-protect` interact with non-local exits via restarts? Cleanup forms are executed during stack unwinding.
- What happens when a condition is signaled with no applicable handlers? For `error`, it enters the debugger; for `signal`, it returns nil; for `warn`, it outputs to `*error-output*`.

## Requirements *(mandatory)*

### Functional Requirements

#### Condition Class Hierarchy

- **FR-001**: System MUST provide a `condition` base class that all conditions inherit from
- **FR-002**: System MUST provide `serious-condition` as a subclass of `condition` for conditions requiring intervention
- **FR-003**: System MUST provide `error` as a subclass of `serious-condition` for error conditions
- **FR-004**: System MUST provide `warning` as a subclass of `condition` for warning conditions
- **FR-005**: System MUST provide `simple-condition` mixin with `format-control` and `format-arguments` slots
- **FR-006**: System MUST provide `simple-error` combining `simple-condition` and `error`
- **FR-007**: System MUST provide `simple-warning` combining `simple-condition` and `warning`
- **FR-008**: System MUST provide `type-error` with `datum` and `expected-type` slots
- **FR-009**: System MUST provide `undefined-function` with `name` slot

#### Handler Establishment

- **FR-010**: System MUST provide `handler-case` macro that establishes handlers and transfers control on match
- **FR-011**: System MUST provide `handler-bind` macro that establishes handlers without automatic control transfer
- **FR-012**: Handler selection MUST use type-based dispatch, checking if condition is of handler's specified type
- **FR-013**: System MUST search handlers from innermost to outermost scope

#### Restart System

- **FR-014**: System MUST provide `restart-case` macro for establishing restarts around a form
- **FR-015**: System MUST provide `restart-bind` macro for establishing restarts with explicit functions
- **FR-016**: System MUST provide `invoke-restart` function to invoke a restart by name or object
- **FR-017**: System MUST provide `invoke-restart-interactively` for restarts requiring interactive input
- **FR-018**: System MUST provide `find-restart` function to locate a restart by name
- **FR-019**: System MUST provide `compute-restarts` function to return all visible restarts

#### Signaling Functions

- **FR-020**: System MUST provide `signal` function for signaling conditions that may be handled
- **FR-021**: System MUST provide `warn` function for signaling warnings with default behavior of printing to `*error-output*`
- **FR-022**: System MUST provide `error` function for signaling unrecoverable errors
- **FR-023**: System MUST provide `cerror` function for signaling continuable errors with a continue restart

#### Standard Restarts

- **FR-024**: System MUST provide `abort` restart for aborting to a convenient restart point
- **FR-025**: System MUST provide `continue` restart for continuing from a continuable error
- **FR-026**: System MUST provide `muffle-warning` restart for suppressing warnings
- **FR-027**: System MUST provide `use-value` restart for substituting a value
- **FR-028**: System MUST provide `store-value` restart for storing a value for future use

#### Convenience Macros

- **FR-029**: System MUST provide `with-simple-restart` macro for establishing a simple named restart

#### Integration with Existing Infrastructure

- **FR-030**: Handler and restart stacks MUST use stack-based binding similar to special variable shallow binding
- **FR-031**: Non-local exits via restarts MUST execute `unwind-protect` cleanup forms
- **FR-032**: System MUST integrate with existing `catch`/`throw` mechanism for control transfer
- **FR-033**: System MUST integrate with WasmGC `try_table` exception mechanism for stack unwinding

### Key Entities

- **Condition**: An object representing an exceptional situation; has a type used for handler matching
- **Handler**: A binding that associates a condition type with a function to call when that type is signaled
- **Restart**: A named point to which control can transfer, with an optional interactive function for gathering arguments
- **Handler Cluster**: A set of handlers established by a single `handler-case` or `handler-bind` form
- **Restart Cluster**: A set of restarts established by a single `restart-case` or `restart-bind` form

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can catch and handle specific condition types using `handler-case` with 100% reliability
- **SC-002**: Developers can establish and invoke restarts using `restart-case` and `invoke-restart` with correct control transfer
- **SC-003**: The `signal`, `warn`, and `error` functions behave according to ANSI Common Lisp specification
- **SC-004**: `unwind-protect` cleanup forms execute correctly when control transfers through restarts
- **SC-005**: Nested handlers and restarts resolve in correct order (innermost first for handlers)
- **SC-006**: All standard condition types (`error`, `warning`, `type-error`, etc.) are properly defined and inheritable
- **SC-007**: All standard restarts (`abort`, `continue`, `muffle-warning`, `use-value`, `store-value`) function correctly

## Assumptions

- The existing `catch`/`throw`/`unwind-protect` infrastructure is stable and can be extended for condition system use
- WasmGC's `try_table` exception mechanism provides sufficient capability for stack unwinding
- The shallow binding implementation for special variables provides a pattern that can be adapted for handler/restart stacks
- The CLOS implementation is sufficient to support condition class hierarchy with multiple inheritance (`simple-error`, `simple-warning`)

## Constraints

- Must leverage existing `catch`/`throw`/`unwind-protect` foundation rather than implementing separate unwinding mechanism
- Must integrate with WasmGC's `try_table` exception mechanism for efficient stack unwinding
- Handler and restart stacks must use stack-based management similar to special variable shallow binding
- Must maintain compatibility with ANSI Common Lisp semantics for the condition system
