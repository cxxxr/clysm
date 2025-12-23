# Feature Specification: Catch/Throw Dynamic Exception Handling

**Feature Branch**: `005-catch-throw`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "Common Lispのcatch/throw機構を実装する。throwは評価時のタグに基づいて対応するcatchまで制御を移し、値を返す。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Catch/Throw (Priority: P1) MVP

As a Lisp developer, I want to use catch/throw to implement non-local exits with values, so that I can handle exceptional conditions and early returns from deeply nested code.

**Why this priority**: This is the fundamental catch/throw mechanism - without it, no other exception handling patterns work. It enables early exit from computation with a return value.

**Independent Test**: Can be fully tested by compiling `(catch 'tag (throw 'tag 42))` and verifying it returns 42. Delivers the core value of non-local control transfer.

**Acceptance Scenarios**:

1. **Given** a catch form with tag 'foo, **When** throw with matching tag 'foo and value 42 is executed, **Then** the catch form returns 42
2. **Given** a catch form with tag 'bar, **When** body executes without throw, **Then** the catch form returns the last value of the body
3. **Given** a throw with value, **When** the throw is executed, **Then** any code after the throw in the same scope is not executed

---

### User Story 2 - Cross-Function Throw (Priority: P1)

As a Lisp developer, I want throw to work across function boundaries, so that I can exit from deeply nested function calls back to an enclosing catch.

**Why this priority**: Real-world exception handling almost always involves throws from within called functions. This is essential for practical error handling.

**Independent Test**: Define a helper function that throws, wrap its call in catch, and verify the throw propagates correctly.

**Acceptance Scenarios**:

1. **Given** a catch form calling a function that executes throw with matching tag, **When** the function runs, **Then** control returns to the catch with the thrown value
2. **Given** multiple levels of function calls with throw at the deepest level, **When** throw executes, **Then** all intermediate frames are exited and control returns to the matching catch

---

### User Story 3 - Nested Catch with Tag Selection (Priority: P2)

As a Lisp developer, I want throw to find the correct catch when multiple catches are nested, so that I can implement layered exception handling with different tags for different purposes.

**Why this priority**: Enables structured exception handling patterns where different catches handle different categories of exceptional conditions.

**Independent Test**: Nest multiple catches with different tags and verify throw finds the correct one.

**Acceptance Scenarios**:

1. **Given** `(catch 'outer (catch 'inner (throw 'outer 42)))`, **When** executed, **Then** the outer catch receives value 42 (inner catch is skipped)
2. **Given** nested catches where inner and outer have different tags, **When** throw targets the outer tag, **Then** inner catch body continues executing until throw is reached
3. **Given** `(catch 'outer (catch 'inner (throw 'outer 42)) 99)`, **When** executed, **Then** result is 42, not 99 (code after inner catch is not executed)

---

### User Story 4 - Unwind-Protect Integration (Priority: P3)

As a Lisp developer, I want cleanup forms in unwind-protect to execute even when throw transfers control, so that I can ensure resource cleanup happens regardless of how a block exits.

**Why this priority**: Critical for resource management (file handles, locks, connections), but builds on the basic throw mechanism.

**Independent Test**: Wrap throw in unwind-protect, verify cleanup executes before control reaches catch.

**Acceptance Scenarios**:

1. **Given** `(catch 'foo (unwind-protect (throw 'foo 1) (cleanup-action)))`, **When** throw executes, **Then** cleanup-action runs before catch receives the value
2. **Given** multiple nested unwind-protects between throw and catch, **When** throw executes, **Then** all cleanup forms execute in reverse order (innermost first)

---

### Edge Cases

- What happens when throw has no matching catch? System signals a runtime error for unhandled throw
- What happens when tag expressions evaluate to the same symbol at runtime? They match (eq comparison)
- What happens when throw value is NIL? NIL is returned to the catch (throw always takes a value)
- What happens when catch body is empty? Returns NIL
- What happens when a catch tag is not a symbol? Behavior is undefined; typically signals error at compile time

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST evaluate the catch tag expression at runtime before executing the body
- **FR-002**: System MUST evaluate the throw tag expression at runtime and compare with catch tags using eq
- **FR-003**: System MUST transfer control to the nearest enclosing catch with matching tag when throw executes
- **FR-004**: System MUST return the throw's value expression as the result of the matching catch form
- **FR-005**: System MUST return the last value of the body when catch completes without throw
- **FR-006**: System MUST propagate throw across function call boundaries
- **FR-007**: System MUST execute unwind-protect cleanup forms during throw unwinding
- **FR-008**: System MUST signal a runtime error for throw with no matching catch
- **FR-009**: System MUST skip catch forms whose tags do not match (eq) the throw tag
- **FR-010**: System MUST NOT execute code after throw within the same block

### Key Entities

- **Catch Frame**: Represents an active catch point with its evaluated tag and continuation
- **Throw Value**: The pair of (tag, value) that initiates non-local transfer
- **Catch Tag**: A symbol used to match throws with catches via eq comparison

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing catch/throw tests in tests/integration/control-flow-test.lisp pass (test-catch-throw, test-nested-catch, test-throw-from-function, test-catch-wrong-tag)
- **SC-002**: 10,000 nested throw operations complete without stack overflow
- **SC-003**: Throw across 100 function call levels completes successfully
- **SC-004**: Unwind-protect cleanup forms execute 100% of the time during throw unwinding
- **SC-005**: Unmatched throw signals identifiable runtime error within 1ms

## Scope Boundaries

### In Scope

- Basic catch/throw with symbol tags
- Cross-function throw propagation
- Nested catch with tag-based dispatch
- Integration with unwind-protect cleanup

### Out of Scope

- Condition system (signal, handler-bind, restart-case) - separate feature
- Multiple-value throw (throw returning multiple values) - Phase 8
- Non-symbol catch tags - undefined behavior
- Compile-time detection of definitely-unmatched throws - optimization

## Assumptions

- Tags are evaluated at runtime; static analysis of tag matching is not required
- The eq function for symbol comparison is already available
- Unwind-protect mechanism exists but may need integration work
- Wasm target supports some form of exception handling or can simulate it

## Dependencies

- Block/return-from: Already implemented, provides local non-local exit pattern
- Tagbody/go: Already implemented, provides local control transfer pattern
- Unwind-protect: Partially implemented, needs integration with throw unwinding
