# Feature Specification: Destructuring-Bind Macro

**Feature Branch**: `031-destructuring-bind-macro`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 9B: destructuring-bind マクロを実装する。目標はANSI Common Lisp準拠の分解束縛。&optional, &rest, &key, &body, &whole, &allow-other-keysをサポートし、ネスト構造の分解も可能にする。コンパイラ内の9箇所で使用されるためセルフホスティングに必須。パラメータ不足時はプログラムエラー、余剰引数は&restなしの場合エラー。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Destructuring with Required Parameters (Priority: P1)

As a developer using the clysm3 compiler, I want to use `destructuring-bind` to extract values from a list into variables so that I can work with structured data more easily.

**Why this priority**: Basic destructuring is the core functionality. Without required parameter support, no other features can work. This is the minimum viable implementation.

**Independent Test**: Can be fully tested by compiling and running code like `(destructuring-bind (a b c) '(1 2 3) (list a b c))` and verifying the result is `(1 2 3)`.

**Acceptance Scenarios**:

1. **Given** a list `(1 2 3)` and pattern `(a b c)`, **When** `destructuring-bind` is applied, **Then** variables `a`, `b`, `c` are bound to `1`, `2`, `3` respectively.
2. **Given** a list `((1 2) (3 4))` and pattern `((a b) (c d))`, **When** `destructuring-bind` is applied with nested patterns, **Then** variables `a`, `b`, `c`, `d` are bound to `1`, `2`, `3`, `4` respectively.
3. **Given** a list with fewer elements than required parameters, **When** `destructuring-bind` is applied, **Then** a `program-error` condition is signaled.

---

### User Story 2 - Optional and Rest Parameters (Priority: P2)

As a developer, I want to use `&optional` and `&rest` lambda-list keywords in destructuring patterns so that I can handle variable-length lists with default values.

**Why this priority**: Optional and rest parameters are commonly used in macro lambda lists and function argument processing. They extend the basic functionality to handle flexible data structures.

**Independent Test**: Can be tested by compiling code like `(destructuring-bind (a &optional b &rest c) '(1) (list a b c))` and verifying `a=1`, `b=NIL`, `c=NIL`.

**Acceptance Scenarios**:

1. **Given** a list `(1 2 3 4)` and pattern `(a &optional b &rest c)`, **When** `destructuring-bind` is applied, **Then** `a=1`, `b=2`, `c=(3 4)`.
2. **Given** a list `(1)` and pattern `(a &optional (b 10))`, **When** `destructuring-bind` is applied, **Then** `a=1`, `b=10` (default value used).
3. **Given** a list `(1 2)` and pattern `(a &optional (b nil b-supplied-p))`, **When** `destructuring-bind` is applied, **Then** `b-supplied-p=T` since `b` was provided.
4. **Given** a list with more elements than required/optional parameters and no `&rest`, **When** `destructuring-bind` is applied, **Then** an error condition is signaled.

---

### User Story 3 - Keyword Parameters (Priority: P3)

As a developer, I want to use `&key` and `&allow-other-keys` in destructuring patterns so that I can extract named arguments from property-list-style data.

**Why this priority**: Keyword arguments are essential for macro lambda-list processing and self-hosting. Many compiler internal macros use keyword arguments.

**Independent Test**: Can be tested by compiling code like `(destructuring-bind (&key x y) '(:x 1 :y 2) (list x y))` and verifying the result is `(1 2)`.

**Acceptance Scenarios**:

1. **Given** a list `(:x 1 :y 2)` and pattern `(&key x y)`, **When** `destructuring-bind` is applied, **Then** `x=1`, `y=2`.
2. **Given** a list `(:y 2)` and pattern `(&key (x 10) y)`, **When** `destructuring-bind` is applied, **Then** `x=10` (default), `y=2`.
3. **Given** a list `(:x 1 :z 3)` and pattern `(&key x)`, **When** `destructuring-bind` is applied without `&allow-other-keys`, **Then** an error is signaled for unrecognized key `:z`.
4. **Given** a list `(:x 1 :z 3)` and pattern `(&key x &allow-other-keys)`, **When** `destructuring-bind` is applied, **Then** `x=1` and `:z` is ignored without error.

---

### User Story 4 - Whole and Body Parameters (Priority: P4)

As a developer, I want to use `&whole` and `&body` lambda-list keywords so that I can capture the entire argument list while also destructuring it, and handle macro body forms correctly.

**Why this priority**: `&whole` and `&body` are critical for macro definitions in self-hosting. `&body` is syntactically equivalent to `&rest` but provides better IDE/editor indentation hints.

**Independent Test**: Can be tested by compiling code like `(destructuring-bind (&whole w a b) '(1 2) (list w a b))` and verifying result is `((1 2) 1 2)`.

**Acceptance Scenarios**:

1. **Given** a list `(1 2 3)` and pattern `(&whole w a b c)`, **When** `destructuring-bind` is applied, **Then** `w=(1 2 3)`, `a=1`, `b=2`, `c=3`.
2. **Given** a pattern with `&body`, **When** `destructuring-bind` is applied, **Then** it behaves identically to `&rest` for binding purposes.
3. **Given** a nested pattern `(&whole w (a b) c)` and list `((1 2) 3)`, **When** `destructuring-bind` is applied, **Then** `w=((1 2) 3)`, `a=1`, `b=2`, `c=3`.

---

### User Story 5 - Compiler Self-Hosting Support (Priority: P5)

As the clysm3 compiler, I need `destructuring-bind` implemented so that I can compile my own source code that uses this macro in 9 locations.

**Why this priority**: Self-hosting is a milestone goal. The compiler must be able to compile itself, and 9 internal locations depend on `destructuring-bind`.

**Independent Test**: Can be tested by identifying the 9 compiler locations that use `destructuring-bind` and verifying each compiles and runs correctly.

**Acceptance Scenarios**:

1. **Given** the clysm3 compiler source code, **When** all 9 locations using `destructuring-bind` are identified and compiled, **Then** each location successfully compiles without errors.
2. **Given** complex nested patterns used in the compiler, **When** `destructuring-bind` processes them, **Then** all variables are correctly bound.

---

### Edge Cases

- What happens when the list is NIL and required parameters exist? A `program-error` is signaled.
- How does the system handle dotted lists like `(a b . c)`? The `&rest`/`&body` variable captures the dotted tail as a non-list.
- What happens when `&key` and `&rest` are combined? The `&rest` variable captures the entire keyword-value plist, and `&key` extracts specific keys from it.
- How are duplicate keys handled with `&key`? The first occurrence is used (ANSI CL behavior).
- What happens when a keyword is provided but with value NIL? The variable is bound to NIL, and `supplied-p` is T if specified.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Macro MUST expand `destructuring-bind` to code that binds variables according to a destructuring lambda-list pattern.
- **FR-002**: Macro MUST support required parameters in flat and nested patterns.
- **FR-003**: Macro MUST support `&optional` parameters with default values and supplied-p variables.
- **FR-004**: Macro MUST support `&rest` to capture remaining list elements.
- **FR-005**: Macro MUST support `&key` parameters with default values, supplied-p variables, and alternate keyword names.
- **FR-006**: Macro MUST support `&body` as a synonym for `&rest`.
- **FR-007**: Macro MUST support `&whole` to bind the entire list before destructuring.
- **FR-008**: Macro MUST support `&allow-other-keys` to permit unrecognized keywords.
- **FR-009**: Macro MUST signal `program-error` when required parameters cannot be satisfied due to insufficient list elements.
- **FR-010**: Macro MUST signal an error when excess elements exist and no `&rest`/`&body` is specified.
- **FR-011**: Macro MUST signal an error when unrecognized keywords are present and `&allow-other-keys` is not specified.
- **FR-012**: Macro MUST support nested destructuring patterns at any depth.
- **FR-013**: Macro MUST handle dotted-list patterns where the final `cdr` is bound to a variable.
- **FR-014**: Macro MUST evaluate default value forms only when the corresponding parameter is not supplied.

### Key Entities

- **Destructuring Lambda-List**: A pattern that specifies how to decompose a list. Contains required parameters, `&optional`, `&rest`/`&body`, `&key`, `&whole`, and `&allow-other-keys` sections.
- **Supplied-P Variable**: An optional variable that indicates whether a corresponding `&optional` or `&key` parameter was explicitly provided.
- **Default Value Form**: An expression evaluated to provide a default value when a parameter is not supplied.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 14 functional requirements are implemented and pass corresponding tests.
- **SC-002**: Nested destructuring works to at least 5 levels of nesting depth.
- **SC-003**: All 9 compiler locations using `destructuring-bind` compile and execute correctly.
- **SC-004**: Error conditions are signaled correctly for malformed inputs (insufficient elements, excess elements, unknown keys).
- **SC-005**: Macro expansion time is negligible (macro expansion completes without noticeable delay for typical patterns).

## Assumptions

- The existing macro system (Feature 016) is functional and can be used to define `destructuring-bind`.
- The condition system (Feature 014) is available for signaling `program-error` and other errors.
- Lambda-list parsing utilities may be shared with or adapted from existing function lambda-list parsing code.
- `&environment` parameter support is deferred to a future phase if not already implemented.
