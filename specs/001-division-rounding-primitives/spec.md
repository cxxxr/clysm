# Feature Specification: Division/Rounding Function Primitives

**Feature Branch**: `001-division-rounding-primitives`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-1e: 除算・丸め関数プリミティブを実装する。floor, ceiling, round関数をWasmGCプリミティブとして追加。これらはANSI CL標準関数で、2値（商と剰余）を返す。truncate/rem/modは実装済みだが、floor/ceiling/roundが未実装でDEFUNコンパイル失敗の主要原因となっている。WasmのF64命令（f64.floor, f64.ceil, f64.nearest）を使用し、整数引数の場合はfixnum、浮動小数点引数の場合はfloatで結果を返す。ffloor/fceiling/froundも合わせて実装。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile floor/ceiling/round Functions (Priority: P1)

When compiling Common Lisp code that uses floor, ceiling, or round functions, the Clysm compiler produces valid WasmGC bytecode that correctly calculates the quotient and remainder.

**Why this priority**: These functions are ANSI CL standard functions used extensively throughout the compiler codebase. Their absence causes DEFUN compilation failures, which is the top blocker for self-hosting (18,933 failures). Implementing these primitives directly unblocks a significant portion of compilation.

**Independent Test**: Can be fully tested by compiling expressions like `(floor 7 2)`, `(ceiling 7 2)`, `(round 7 2)` and verifying the output values (quotient and remainder) match ANSI CL semantics.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(floor 7 2)`, **When** compiled and executed, **Then** returns 2 values: 3 (quotient) and 1 (remainder)
2. **Given** a Lisp expression `(ceiling 7 2)`, **When** compiled and executed, **Then** returns 2 values: 4 (quotient) and -1 (remainder)
3. **Given** a Lisp expression `(round 7 2)`, **When** compiled and executed, **Then** returns 2 values: 4 (quotient) and -1 (remainder)
4. **Given** a Lisp expression `(floor -7 2)`, **When** compiled and executed, **Then** returns 2 values: -4 (quotient) and 1 (remainder)
5. **Given** a Lisp expression `(ceiling -7 2)`, **When** compiled and executed, **Then** returns 2 values: -3 (quotient) and -1 (remainder)

---

### User Story 2 - Float-Result Variants (ffloor, fceiling, fround) (Priority: P2)

When compiling Common Lisp code that uses ffloor, fceiling, or fround functions, the compiler produces WasmGC bytecode that returns floating-point quotients.

**Why this priority**: These are the float-result variants of the rounding functions. They are less commonly used than the integer versions but are required for full ANSI CL compliance and are used in numeric algorithms.

**Independent Test**: Can be tested by compiling expressions like `(ffloor 7.5 2.0)` and verifying the quotient is returned as a float.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(ffloor 7.5 2.0)`, **When** compiled and executed, **Then** returns a float quotient 3.0 and remainder 1.5
2. **Given** a Lisp expression `(fceiling 7.5 2.0)`, **When** compiled and executed, **Then** returns a float quotient 4.0 and remainder -0.5
3. **Given** a Lisp expression `(fround 7.5 2.0)`, **When** compiled and executed, **Then** returns a float quotient 4.0 and remainder -0.5

---

### User Story 3 - Single-Argument Rounding (Priority: P2)

When calling floor, ceiling, or round with a single argument, the compiler produces code that rounds the number and returns it with the fractional part as remainder.

**Why this priority**: Single-argument forms are commonly used for simple rounding operations (e.g., `(floor 3.7)` → 3). Required for ANSI CL compliance.

**Independent Test**: Can be tested by compiling `(floor 3.7)` and verifying it returns 3 and 0.7.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(floor 3.7)`, **When** compiled and executed, **Then** returns 2 values: 3 and approximately 0.7
2. **Given** a Lisp expression `(ceiling 3.2)`, **When** compiled and executed, **Then** returns 2 values: 4 and approximately -0.8
3. **Given** a Lisp expression `(round 3.5)`, **When** compiled and executed, **Then** returns 2 values: 4 and -0.5

---

### User Story 4 - Type-Preserving Results (Priority: P3)

When floor/ceiling/round are called with integer arguments, the quotient is returned as an integer (fixnum). The f-variants (ffloor, fceiling, fround) always return float quotients.

**Why this priority**: Type preservation is important for performance and correctness in numeric algorithms, but basic functionality takes precedence.

**Independent Test**: Can be tested by verifying `(floor 10 3)` returns an integer 3, while `(ffloor 10.0 3.0)` returns a float 3.0.

**Acceptance Scenarios**:

1. **Given** `(floor 10 3)` with integer arguments, **When** compiled and executed, **Then** the quotient 3 is a fixnum
2. **Given** `(floor 10.0 3.0)` with float arguments, **When** compiled and executed, **Then** the quotient 3 is still an integer (floor/ceiling/round always return integers per ANSI CL)
3. **Given** `(ffloor 10.0 3.0)` with float arguments, **When** compiled and executed, **Then** the quotient 3.0 is a float

---

### Edge Cases

- What happens when divisor is zero? System signals a division-by-zero error
- What happens when dividing negative numbers? Follows ANSI CL semantics (floor rounds toward negative infinity, ceiling toward positive infinity)
- What happens with very large numbers? Uses i32/f64 representation limits; overflow behavior follows Wasm semantics
- What happens with special floats (NaN, infinity)? Follows IEEE 754 semantics as implemented by Wasm f64 operations
- What happens with ratio arguments? Converted to float for computation using existing ratio-to-float conversion

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `floor` function that returns quotient rounded toward negative infinity and remainder
- **FR-002**: System MUST implement `ceiling` function that returns quotient rounded toward positive infinity and remainder
- **FR-003**: System MUST implement `round` function that returns quotient rounded to nearest integer (banker's rounding for ties) and remainder
- **FR-004**: System MUST implement `ffloor`, `fceiling`, `fround` variants that return float quotients
- **FR-005**: All functions MUST return 2 values: quotient and remainder, using the existing multiple-values mechanism
- **FR-006**: Remainder MUST satisfy the invariant: `dividend = quotient * divisor + remainder`
- **FR-007**: System MUST handle both single-argument form (divisor defaults to 1) and two-argument form
- **FR-008**: System MUST use Wasm F64 instructions (`f64.floor`, `f64.ceil`, `f64.nearest`) for floating-point operations
- **FR-009**: System MUST integrate with existing AST parsing that already recognizes floor/ceiling/round operators
- **FR-010**: System MUST preserve constant folding behavior for compile-time known values

### Key Entities

- **Rounding Function**: A function that divides two numbers and returns quotient and remainder based on rounding mode (floor/ceiling/round)
- **Multiple Values**: The mechanism for returning 2 values (quotient and remainder) from a single function call
- **Type Contagion**: Rules for determining result types based on argument types (integer vs float)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 9 rounding functions (floor, ceiling, round, ffloor, fceiling, fround, and single-argument variants) compile and execute correctly
- **SC-002**: Generated Wasm passes validation with exit code 0
- **SC-003**: Test suite covers all rounding modes with positive, negative, and zero arguments
- **SC-004**: DEFUN compilation failures related to missing floor/ceiling/round are eliminated
- **SC-005**: Stage 1 compilation rate improves (measured by reduction in forms failing due to these functions)

## Assumptions

- Existing `truncate` implementation pattern can be extended for floor/ceiling/round
- Wasm F64 instructions provide correct IEEE 754 semantics for floor/ceil/nearest
- Existing multiple-values mechanism (mv-count, mv-buffer) is sufficient for returning 2 values
- Ratio arguments will be converted to float using existing conversion mechanisms
