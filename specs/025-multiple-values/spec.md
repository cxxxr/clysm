# Feature Specification: ANSI Common Lisp Multiple Values Support

**Feature Branch**: `025-multiple-values`
**Created**: 2025-12-26
**Status**: Draft
**Input**: User description: "ANSI Common Lispの多値(Multiple Values)サポートを実装する"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Return Multiple Values from Functions (Priority: P1)

Lisp programmers need to return multiple values from a single function call, a fundamental Common Lisp feature. This enables functions like `floor` and `truncate` to return both the quotient and remainder in a single call, and allows custom functions to return related results together.

**Why this priority**: Multiple values are a core Common Lisp feature that many other features depend on. Functions like `floor`, `truncate`, `round`, and `ceiling` are required to return both quotient and remainder. Without this, the compiler cannot achieve ANSI compliance for these fundamental operations.

**Independent Test**: Can be tested by compiling `(values 1 2 3)` and verifying that the primary value (1) is returned correctly, and secondary values (2, 3) are accessible through the value buffer mechanism.

**Acceptance Scenarios**:

1. **Given** a compiled function containing `(values)`, **When** the function is called, **Then** it returns NIL and reports 0 values
2. **Given** a compiled function containing `(values 42)`, **When** the function is called, **Then** it returns 42 and reports 1 value
3. **Given** a compiled function containing `(values 1 2 3)`, **When** the function is called, **Then** it returns 1 as primary value, stores 2 and 3 in the value buffer, and reports 3 values
4. **Given** a function returning multiple values, **When** called in a single-value context, **Then** only the primary value is used and secondary values are discarded

---

### User Story 2 - Bind Multiple Values to Variables (Priority: P1)

Lisp programmers need to capture all returned values from a function and bind them to individual variables for further processing. This is essential for using functions that return multiple values.

**Why this priority**: Without the ability to receive multiple values, the `values` form would be useless. `multiple-value-bind` is the primary mechanism for accessing secondary values.

**Independent Test**: Can be tested by compiling `(multiple-value-bind (q r) (floor 7 3) (list q r))` and verifying it produces `(2 1)`.

**Acceptance Scenarios**:

1. **Given** `(multiple-value-bind (a b) (values 1 2) (+ a b))`, **When** compiled and executed, **Then** it returns 3
2. **Given** `(multiple-value-bind (a b c) (values 1 2) c)`, **When** compiled and executed, **Then** c is bound to NIL (fewer values than variables)
3. **Given** `(multiple-value-bind (a) (values 1 2 3) a)`, **When** compiled and executed, **Then** a is bound to 1 (extra values ignored)
4. **Given** `(multiple-value-bind () (values 1 2) 'done)`, **When** compiled and executed, **Then** it returns DONE (all values ignored)

---

### User Story 3 - Collect Multiple Values as a List (Priority: P2)

Lisp programmers need to collect all returned values into a list for dynamic processing, especially when the number of values is not known at compile time.

**Why this priority**: `multiple-value-list` provides flexibility for handling unknown numbers of values and is commonly used in REPL implementations and debugging.

**Independent Test**: Can be tested by compiling `(multiple-value-list (values 1 2 3))` and verifying it returns `(1 2 3)`.

**Acceptance Scenarios**:

1. **Given** `(multiple-value-list (values))`, **When** compiled and executed, **Then** it returns NIL
2. **Given** `(multiple-value-list (values 1 2 3))`, **When** compiled and executed, **Then** it returns (1 2 3)
3. **Given** `(multiple-value-list (floor 7 3))`, **When** compiled and executed, **Then** it returns (2 1)

---

### User Story 4 - Access Specific Value by Index (Priority: P2)

Lisp programmers need to access a specific value from a multiple-value-returning form by its index, without binding all values.

**Why this priority**: `nth-value` provides efficient access to specific values without the overhead of binding unused values.

**Independent Test**: Can be tested by compiling `(nth-value 1 (values 'a 'b 'c))` and verifying it returns `B`.

**Acceptance Scenarios**:

1. **Given** `(nth-value 0 (values 'a 'b 'c))`, **When** compiled and executed, **Then** it returns A
2. **Given** `(nth-value 2 (values 'a 'b 'c))`, **When** compiled and executed, **Then** it returns C
3. **Given** `(nth-value 5 (values 'a 'b))`, **When** compiled and executed, **Then** it returns NIL (index out of range)

---

### User Story 5 - Spread List Elements as Multiple Values (Priority: P2)

Lisp programmers need to convert a list into multiple values, the inverse of `multiple-value-list`.

**Why this priority**: `values-list` enables programmatic generation of multiple values from data structures.

**Independent Test**: Can be tested by compiling `(multiple-value-list (values-list '(1 2 3)))` and verifying it returns `(1 2 3)`.

**Acceptance Scenarios**:

1. **Given** `(values-list nil)`, **When** compiled and executed, **Then** it returns no values (equivalent to `(values)`)
2. **Given** `(values-list '(1 2 3))`, **When** compiled and executed, **Then** it returns 1 as primary value with 2 and 3 as secondary values
3. **Given** a non-list argument to `values-list`, **When** compiled and executed, **Then** it signals a type-error condition

---

### User Story 6 - Preserve Multiple Values Through Evaluation (Priority: P2)

Lisp programmers need to preserve and return the multiple values of a specific form while executing other forms for side effects.

**Why this priority**: `multiple-value-prog1` is necessary for saving intermediate multiple-value results while performing cleanup or other operations.

**Independent Test**: Can be tested by compiling `(multiple-value-prog1 (values 1 2) (print 'done))` and verifying it returns values 1 and 2.

**Acceptance Scenarios**:

1. **Given** `(multiple-value-prog1 (values 1 2 3) (print 'side-effect))`, **When** compiled and executed, **Then** it prints SIDE-EFFECT and returns 1, 2, 3 as multiple values
2. **Given** `(multiple-value-prog1 42 (setq x 10))`, **When** compiled and executed, **Then** x is set to 10 and 42 is returned

---

### User Story 7 - Pass Multiple Values to Functions (Priority: P3)

Lisp programmers need to pass all returned values from one function as arguments to another function.

**Why this priority**: `multiple-value-call` enables function composition with multiple values, but is less commonly used than other forms.

**Independent Test**: Can be tested by compiling `(multiple-value-call #'+ (values 1 2) (values 3 4))` and verifying it returns 10.

**Acceptance Scenarios**:

1. **Given** `(multiple-value-call #'list (values 1 2) (values 3))`, **When** compiled and executed, **Then** it returns (1 2 3)
2. **Given** `(multiple-value-call #'+ (floor 7 3) (floor 5 2))`, **When** compiled and executed, **Then** it returns 6 (2+1+2+1)

---

### User Story 8 - Update Existing Functions for Multiple Values (Priority: P1)

Mathematical functions `floor`, `truncate`, `ceiling`, `round`, and `mod`/`rem` must return multiple values per ANSI Common Lisp specification.

**Why this priority**: These are standard library functions that ANSI CL requires to return multiple values. Without updating them, the compiler fails ANSI conformance tests.

**Independent Test**: Can be tested by compiling `(multiple-value-list (floor 7 3))` and verifying it returns `(2 1)`.

**Acceptance Scenarios**:

1. **Given** `(floor 7 3)`, **When** compiled and executed, **Then** it returns 2 as primary value and 1 as secondary value (remainder)
2. **Given** `(truncate -7 3)`, **When** compiled and executed, **Then** it returns -2 as primary value and -1 as secondary value
3. **Given** `(ceiling 7 3)`, **When** compiled and executed, **Then** it returns 3 as primary value and -2 as secondary value
4. **Given** `(round 7 3)`, **When** compiled and executed, **Then** it returns 2 as primary value and 1 as secondary value

---

### Edge Cases

- What happens when `values` is called with no arguments? Returns NIL as primary value with 0 values count
- What happens when more variables are bound than values returned? Extra variables are bound to NIL
- What happens when a non-list is passed to `values-list`? Signals a type-error condition
- What happens when `nth-value` index exceeds available values? Returns NIL
- What happens when multiple-value forms are nested? Inner form's values are captured, outer form sees only primary value unless explicitly receiving multiple values
- What happens when multiple-value forms appear in tail position? Values are preserved and returned from enclosing function
- What happens when `values` appears in non-tail position? Only primary value is used in subsequent computation; secondary values are discarded

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST support `values` special form that returns zero or more values
- **FR-002**: Compiler MUST support `multiple-value-bind` macro that binds variables to multiple values from a form
- **FR-003**: Compiler MUST support `multiple-value-list` macro that collects all values into a list
- **FR-004**: Compiler MUST support `nth-value` macro that retrieves a value by index
- **FR-005**: Compiler MUST support `values-list` function that spreads list elements as multiple values
- **FR-006**: Compiler MUST support `multiple-value-prog1` macro that preserves values through subsequent forms
- **FR-007**: Compiler MUST support `multiple-value-call` macro that passes multiple values as function arguments
- **FR-008**: Compiler MUST update `floor` to return quotient and remainder as two values
- **FR-009**: Compiler MUST update `truncate` to return quotient and remainder as two values
- **FR-010**: Compiler MUST update `ceiling` to return quotient and remainder as two values
- **FR-011**: Compiler MUST update `round` to return quotient and remainder as two values
- **FR-012**: Compiler MUST maintain a global value count that tracks number of returned values
- **FR-013**: Compiler MUST maintain a global value buffer that stores secondary values (values beyond the primary)
- **FR-014**: In single-value context, only the primary value MUST be used; secondary values MUST be discarded
- **FR-015**: When fewer values are returned than variables bound, extra variables MUST be bound to NIL
- **FR-016**: The `values-list` function MUST signal type-error when given a non-list argument

### Key Entities

- **Value Buffer**: Storage for secondary values (values 2 through N), implemented as a global accessible structure
- **Value Count**: A counter tracking how many values were returned by the most recent multiple-value form
- **Primary Value**: The first value returned, which follows normal return semantics (stack-based)
- **Secondary Values**: Values beyond the primary, stored in the value buffer

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 8 multiple-value forms compile successfully to valid Wasm modules
- **SC-002**: `(values 1 2 3)` returns 1 as primary value with 2, 3 accessible as secondary values
- **SC-003**: `(multiple-value-bind (a b) (values 1 2) (+ a b))` evaluates to 3
- **SC-004**: `(multiple-value-list (values 1 2 3))` evaluates to (1 2 3)
- **SC-005**: `(floor 7 3)` returns 2 as primary value and 1 as secondary value
- **SC-006**: ANSI test suite multiple-value tests pass at 95% or higher rate
- **SC-007**: Existing single-value code continues to work unchanged (backward compatibility)
- **SC-008**: No performance regression for functions that don't use multiple values

## Assumptions

- The maximum number of multiple values is bounded by available buffer size (implementation may choose a reasonable limit such as 20 values)
- Multiple values are thread-local or the system is single-threaded (global buffer assumption)
- The primary value always remains on the evaluation stack per normal Wasm calling conventions
- Macro expansion for `multiple-value-bind`, `multiple-value-list`, `nth-value`, `multiple-value-prog1`, and `multiple-value-call` happens at compile time
