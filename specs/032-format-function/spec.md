# Feature Specification: FORMAT Function Foundation

**Feature Branch**: `032-format-function`
**Created**: 2025-12-27
**Status**: Draft
**Input**: Phase 9D: FORMAT基盤を実装する。目標はANSI Common LispのFORMAT関数のサブセット実装。コンパイラが93箇所でformatを使用しており、セルフホスティングに必須。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Text Formatting with Object Insertion (Priority: P1)

A Lisp developer uses FORMAT to construct strings with embedded values for debugging, error messages, and output generation. This is the foundational use case that enables all compiler diagnostics.

**Why this priority**: The compiler relies on FORMAT for 93 call sites. Basic object printing (~A, ~S) and numeric formatting (~D) are essential for error messages, debugging output, and code generation feedback.

**Independent Test**: Can be fully tested by calling `(format nil "Value: ~A" 42)` and verifying the returned string is `"Value: 42"`.

**Acceptance Scenarios**:

1. **Given** a format string with ~A directive, **When** `(format nil "Result: ~A" object)` is called, **Then** the object's aesthetic (human-readable) representation is inserted into the string.
2. **Given** a format string with ~S directive, **When** `(format nil "Expr: ~S" object)` is called, **Then** the object's standard (re-readable) representation is inserted into the string.
3. **Given** a format string with ~D directive, **When** `(format nil "Count: ~D" 42)` is called, **Then** the integer is printed in decimal format.
4. **Given** more arguments than directives, **When** FORMAT is called, **Then** excess arguments are silently ignored per ANSI CL spec.

---

### User Story 2 - String Return vs Stream Output (Priority: P1)

A Lisp developer chooses between building strings in memory or writing directly to output streams depending on the use case.

**Why this priority**: Destination flexibility is core to FORMAT's utility. The compiler needs string construction (nil destination) for error messages, and stream output (t or stream) for interactive debugging.

**Independent Test**: Can be tested by `(format nil "test")` returning `"test"`, `(format t "test")` writing to *standard-output*, and `(format stream "test")` writing to a custom stream.

**Acceptance Scenarios**:

1. **Given** nil as destination, **When** FORMAT is called, **Then** a string containing the formatted output is returned.
2. **Given** t as destination, **When** FORMAT is called, **Then** output is written to *standard-output* and nil is returned.
3. **Given** a stream object as destination, **When** FORMAT is called, **Then** output is written to that stream and nil is returned.

---

### User Story 3 - Newline and Whitespace Control (Priority: P2)

A Lisp developer uses FORMAT directives to control line breaks and ensure proper output formatting for multi-line messages.

**Why this priority**: Multi-line error messages and structured output require newline control. This is used extensively in compiler diagnostics.

**Independent Test**: Can be tested by `(format nil "Line1~%Line2")` returning a two-line string.

**Acceptance Scenarios**:

1. **Given** a format string with ~% directive, **When** FORMAT is called, **Then** a newline character is inserted at that position.
2. **Given** a format string with ~& directive, **When** FORMAT is called with output not at column 0, **Then** a newline is inserted (fresh-line behavior).
3. **Given** a format string with ~& directive, **When** FORMAT is called with output already at column 0, **Then** no newline is inserted.
4. **Given** a format string with ~~ directive, **When** FORMAT is called, **Then** a literal tilde character is inserted.

---

### User Story 4 - List Iteration (Priority: P2)

A Lisp developer formats lists of items using iteration directives to produce structured output without manual loops.

**Why this priority**: The compiler generates code listings and reports that iterate over collections. Iteration directives eliminate verbose explicit loops.

**Independent Test**: Can be tested by `(format nil "Items: ~{~A~^, ~}" '(a b c))` returning `"Items: a, b, c"`.

**Acceptance Scenarios**:

1. **Given** a format string with ~{...~} and a list argument, **When** FORMAT is called, **Then** the enclosed format string is applied to each element of the list.
2. **Given** a format string with ~^, **When** processing the last element of iteration, **Then** the directive exits the iteration early (separator handling).
3. **Given** an empty list to ~{...~}, **When** FORMAT is called, **Then** no output is produced for that directive.

---

### User Story 5 - Conditional Formatting (Priority: P3)

A Lisp developer uses conditional directives to select format branches based on argument values.

**Why this priority**: Conditional formatting enables context-aware messages (singular/plural, present/absent values) without external conditionals.

**Independent Test**: Can be tested by `(format nil "~[zero~;one~;two~:;many~]" 1)` returning `"one"`.

**Acceptance Scenarios**:

1. **Given** a format string with ~[...~;...~], **When** FORMAT is called with a numeric argument, **Then** the clause matching that index is selected.
2. **Given** a format string with ~:; (default clause), **When** the numeric argument exceeds available clauses, **Then** the default clause is used.
3. **Given** a format string with ~:[false~;true~], **When** FORMAT is called with nil, **Then** the first clause is selected; otherwise the second clause is selected.

---

### User Story 6 - Recursive Processing (Priority: P3)

A Lisp developer uses FORMAT to process nested format strings or sub-arguments for composable formatting.

**Why this priority**: Recursive processing enables building complex format strings from components, useful for extensible compiler messages.

**Independent Test**: Can be tested by `(format nil "~?" "Value: ~A" '(42))` returning `"Value: 42"`.

**Acceptance Scenarios**:

1. **Given** a format string with ~?, **When** FORMAT is called, **Then** the next argument is treated as a format string and the following argument as its argument list.
2. **Given** a nested format string that consumes multiple arguments, **When** processed via ~?, **Then** all required arguments are consumed from the provided list.

---

### Edge Cases

- What happens when fewer arguments are provided than directives consume? (Error: too few arguments for FORMAT)
- What happens with malformed format strings (unclosed ~{, unmatched ~])? (Error: malformed format string)
- How does ~D handle non-integer arguments? (Error: ~D requires an integer)
- What happens with nil as an argument to ~A? (Prints "NIL" as aesthetic representation)
- What happens with ~& as the first directive? (No newline if stream is at column 0)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement the FORMAT function accepting destination, control-string, and variadic arguments.
- **FR-002**: System MUST support nil as destination, returning a string containing formatted output.
- **FR-003**: System MUST support t as destination, writing to *standard-output* and returning nil.
- **FR-004**: System MUST support stream objects as destination, writing to the stream and returning nil.
- **FR-005**: System MUST implement ~A directive to print objects aesthetically (human-readable, no escapes).
- **FR-006**: System MUST implement ~S directive to print objects in standard form (re-readable, with escapes).
- **FR-007**: System MUST implement ~D directive to print integers in decimal notation.
- **FR-008**: System MUST implement ~% directive to emit a newline character.
- **FR-009**: System MUST implement ~& directive to emit a newline only if not at column 0 (fresh-line).
- **FR-010**: System MUST implement ~~ directive to emit a literal tilde character.
- **FR-011**: System MUST implement ~{format~} directive for iterating over list arguments.
- **FR-012**: System MUST implement ~^ directive to exit iteration when no more elements remain.
- **FR-013**: System MUST implement ~[clause~;clause~;...~] directive for indexed conditional selection.
- **FR-014**: System MUST implement ~:[false~;true~] directive for boolean conditional selection.
- **FR-015**: System MUST implement ~:; for default clause in conditional directives.
- **FR-016**: System MUST implement ~? directive for recursive format string processing.
- **FR-017**: System MUST signal a format-error condition when the control string is malformed.
- **FR-018**: System MUST signal a format-error condition when insufficient arguments are provided.
- **FR-019**: System MUST silently ignore excess arguments (per ANSI CL specification).
- **FR-020**: System MUST support case-insensitive directive characters (~a and ~A are equivalent).

### Key Entities

- **Control String**: The format template containing literal text and directives. Parsed at runtime (or compile-time for constant strings).
- **Directive**: A tilde (~) followed by optional parameters and a directive character. Controls how arguments are consumed and formatted.
- **Argument List**: The variadic arguments to FORMAT, consumed sequentially by directives.
- **Destination**: nil (string), t (*standard-output*), or a stream object. Determines output target.
- **Format-Error**: A condition signaled for malformed format strings or argument mismatches.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 93 FORMAT call sites in the compiler produce correct output when the compiler is self-hosted.
- **SC-002**: FORMAT with nil destination returns correctly formatted strings for all supported directive combinations.
- **SC-003**: FORMAT with t destination writes correct output to *standard-output*.
- **SC-004**: All ANSI CL test suite cases for the implemented FORMAT subset pass.
- **SC-005**: Malformed format strings and argument mismatches signal appropriate error conditions.
- **SC-006**: FORMAT handles nested directives (~{...~} containing other directives) correctly up to 3 levels deep.

## Assumptions

- The PRIN1, PRINC, and WRITE-STRING functions from the existing stream infrastructure are available for object printing.
- The *standard-output* special variable is already implemented.
- Integer printing is already supported by existing numeric infrastructure.
- Stream column tracking (for ~& fresh-line) will use a simple heuristic: column 0 after newline, otherwise non-zero.
- This implementation covers the subset needed for self-hosting; advanced directives (~F, ~E, ~$, ~R, etc.) are out of scope.
