# Feature Specification: Numeric Conversion and Formatting (Phase 14C)

**Feature Branch**: `001-numeric-format`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 14C: 数値変換・フォーマット完成を実装する。目標はrationalize（float→ratio変換、連分数近似）とwrite-to-string（:base対応による基数変換出力）の実装。検証基準: (rationalize 0.5) => 1/2, (write-to-string 42 :base 16) => 2A, numbersカテゴリ50%+パス。これによりPhase 14を完了させ、ANSI準拠率35%達成に向けた基盤を整える。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Float to Rational Conversion (Priority: P1)

As a Common Lisp developer using Clysm, I need to convert floating-point numbers to exact rational representations using `rationalize`, so I can perform exact arithmetic operations and avoid floating-point precision issues.

**Why this priority**: `rationalize` is a core ANSI CL numeric function required for numeric type coercion. It enables developers to work with exact rationals when precision matters, and is essential for numeric interoperability.

**Independent Test**: Can be fully tested by calling `(rationalize <float>)` with various floating-point values and verifying exact rational outputs.

**Acceptance Scenarios**:

1. **Given** a simple decimal float like `0.5`, **When** `(rationalize 0.5)` is called, **Then** the result is the exact ratio `1/2`
2. **Given** an integer-valued float like `3.0`, **When** `(rationalize 3.0)` is called, **Then** the result is the integer `3`
3. **Given** a repeating decimal approximation like `0.333333`, **When** `(rationalize 0.333333)` is called, **Then** the result is a simple ratio like `1/3` or close approximation
4. **Given** an irrational approximation like `0.14159265358979`, **When** `(rationalize 0.14159265358979)` is called, **Then** the result is a rational approximation with reasonably small denominator

---

### User Story 2 - Integer Base Conversion Output (Priority: P1)

As a Common Lisp developer using Clysm, I need to convert integers to string representations in various bases using `write-to-string` with the `:base` keyword, so I can format numbers for hexadecimal, binary, octal, or custom base display.

**Why this priority**: Base conversion output is fundamental for debugging, data formatting, and interoperability. Hexadecimal representation is especially critical for system programming and Wasm development.

**Independent Test**: Can be fully tested by calling `(write-to-string <integer> :base <radix>)` and verifying the string output matches expected base representation.

**Acceptance Scenarios**:

1. **Given** integer `42` and base `16`, **When** `(write-to-string 42 :base 16)` is called, **Then** the result is `"2A"`
2. **Given** integer `255` and base `16`, **When** `(write-to-string 255 :base 16)` is called, **Then** the result is `"FF"`
3. **Given** integer `42` and base `2`, **When** `(write-to-string 42 :base 2)` is called, **Then** the result is `"101010"`
4. **Given** integer `42` and base `8`, **When** `(write-to-string 42 :base 8)` is called, **Then** the result is `"52"`
5. **Given** negative integer `-42` and base `16`, **When** `(write-to-string -42 :base 16)` is called, **Then** the result is `"-2A"`

---

### User Story 3 - Standard Number String Output (Priority: P2)

As a Common Lisp developer using Clysm, I need `write-to-string` to handle various numeric types (integers, ratios, floats) in the default base 10 format, so I can convert numbers to strings for general-purpose output.

**Why this priority**: Extends base conversion to cover all numeric types, ensuring comprehensive string output support for the number tower.

**Independent Test**: Can be fully tested by calling `(write-to-string <number>)` with integers, ratios, and floats, verifying readable string output.

**Acceptance Scenarios**:

1. **Given** integer `42`, **When** `(write-to-string 42)` is called, **Then** the result is `"42"`
2. **Given** ratio `1/2`, **When** `(write-to-string 1/2)` is called, **Then** the result is `"1/2"`
3. **Given** float `3.14`, **When** `(write-to-string 3.14)` is called, **Then** the result is a string representation like `"3.14"` or `"3.14d0"`

---

### Edge Cases

- What happens when `rationalize` receives an already-exact number (integer or ratio)? The input is returned unchanged.
- How does `rationalize` handle special float values (infinity, NaN)? Signals an arithmetic error condition.
- What happens when `write-to-string` receives base outside valid range (2-36)? Signals a type error.
- How does `write-to-string` handle base 36 with digits beyond 'Z'? Uses digits 0-9 and letters A-Z for bases up to 36.
- What happens when `write-to-string` receives a ratio with `:base` other than 10? Both numerator and denominator use the specified base.
- How does `rationalize` handle very small floats near zero? Returns small rationals or zero as appropriate.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `rationalize` function that converts floating-point numbers to rational approximations using continued fraction algorithm
- **FR-002**: System MUST return integers unchanged when passed to `rationalize`
- **FR-003**: System MUST return ratios unchanged when passed to `rationalize`
- **FR-004**: `rationalize` MUST produce rationals with reasonably small denominators (continued fraction approximation, not exact float-to-ratio conversion like `rational`)
- **FR-005**: System MUST implement `write-to-string` function with `:base` keyword parameter support
- **FR-006**: `write-to-string` MUST support bases 2 through 36 for integer output
- **FR-007**: `write-to-string` MUST use uppercase letters A-Z for digit values 10-35
- **FR-008**: `write-to-string` MUST handle negative integers by prefixing with minus sign
- **FR-009**: `write-to-string` MUST default to base 10 when `:base` is not specified
- **FR-010**: `write-to-string` MUST handle ratios by converting both numerator and denominator using specified base
- **FR-011**: `write-to-string` MUST handle floating-point numbers with standard decimal representation
- **FR-012**: System MUST signal appropriate error conditions for invalid inputs (NaN, infinity for rationalize; invalid base range for write-to-string)

### Key Entities

- **Ratio**: Exact rational number represented as numerator/denominator pair, created by `rationalize` output
- **Numeric Tower**: Integer to Ratio to Float hierarchy that both functions must respect for type coercion
- **Base/Radix**: Integer 2-36 specifying the numeric base for string conversion

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `(rationalize 0.5)` produces exactly `1/2`
- **SC-002**: `(write-to-string 42 :base 16)` produces exactly `"2A"`
- **SC-003**: Numbers test category achieves 50% or greater pass rate
- **SC-004**: All acceptance scenarios pass without errors
- **SC-005**: Phase 14 completion contributes to ANSI compliance rate progress toward 35%
- **SC-006**: Both `rationalize` and `write-to-string` compile to valid WasmGC code

## Assumptions

- Continued fraction algorithm for `rationalize` follows standard mathematical approach (Stern-Brocot tree or similar)
- Float representation uses IEEE 754 double precision (already supported per CLAUDE.md feature 019)
- Ratio type already exists in the Clysm type system (type index 5 per CLAUDE.md)
- `write-to-string` implements a subset of ANSI CL behavior focused on numeric types; full printer semantics are out of scope
- Test pass rate calculation uses existing Clysm test infrastructure
