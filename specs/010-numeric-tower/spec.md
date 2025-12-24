# Feature Specification: Numeric Tower Implementation

**Feature Branch**: `010-numeric-tower`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 8 標準ライブラリ: 数値塔（Numeric Tower）を実装する。目標はCommon Lisp完全準拠の数値型システム。Bignum（任意精度整数）、Ratio（有理数）、Float（浮動小数点）、Complex（複素数）の4型を追加し、型強制と混合演算をサポート。数学関数（sqrt, expt, abs, gcd, lcm等）と数値述語を含む。wasmtimeで実行可能なWasmGCコードを生成すること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Arithmetic with Large Numbers (Priority: P1)

A Lisp programmer writes code that performs arithmetic operations on numbers larger than 64-bit integers. The compiler generates WasmGC code that correctly computes results using arbitrary-precision integers (bignums).

**Why this priority**: Bignum support is the foundation of the numeric tower. Without arbitrary-precision integers, many Common Lisp programs cannot be compiled correctly. This is the most fundamental numeric type beyond fixnum.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(* 1000000000000000000 1000000000000000000)` and verifying the correct bignum result is produced.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(+ 9223372036854775807 1)`, **When** compiled and executed, **Then** the result is the correct bignum 9223372036854775808
2. **Given** a Lisp expression `(* 10000000000000000000 10000000000000000000)`, **When** compiled and executed, **Then** the result is the correct 100000000000000000000000000000000000000
3. **Given** a bignum value, **When** compared with another bignum using `=`, `<`, `>`, **Then** the comparison returns the correct boolean result
4. **Given** mixed fixnum and bignum operands, **When** arithmetic is performed, **Then** type coercion happens automatically and produces correct results

---

### User Story 2 - Rational Number Arithmetic (Priority: P1)

A Lisp programmer writes code using rational numbers (ratios). Division of integers produces exact rational results, and arithmetic on rationals maintains exactness without floating-point errors.

**Why this priority**: Ratios are essential for exact arithmetic in Common Lisp. Integer division must produce ratios, not truncated integers. This is a core part of the numeric tower that many Lisp programs depend on.

**Independent Test**: Can be fully tested by compiling `(/ 1 3)` and verifying it produces the ratio 1/3, then `(+ (/ 1 3) (/ 1 3) (/ 1 3))` equals 1.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(/ 1 3)`, **When** compiled and executed, **Then** the result is the ratio 1/3 (not 0 or 0.333...)
2. **Given** a Lisp expression `(+ (/ 1 2) (/ 1 4))`, **When** compiled and executed, **Then** the result is the ratio 3/4
3. **Given** a ratio that simplifies to an integer like `(/ 6 3)`, **When** compiled and executed, **Then** the result is the integer 2
4. **Given** ratios with very large numerators and denominators, **When** arithmetic is performed, **Then** results remain exact using bignum components

---

### User Story 3 - Floating-Point Arithmetic (Priority: P2)

A Lisp programmer writes code using floating-point numbers for scientific computations. The compiler supports both single-float and double-float types with standard IEEE 754 semantics.

**Why this priority**: Floating-point numbers are essential for scientific computing, graphics, and many practical applications. While not as fundamental as exact numeric types, they are widely used.

**Independent Test**: Can be fully tested by compiling expressions like `(+ 1.5 2.5)` and `(sqrt 2.0)` and verifying results match IEEE 754 semantics.

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(+ 1.5 2.5)`, **When** compiled and executed, **Then** the result is 4.0
2. **Given** a Lisp expression with mixed integer and float like `(+ 1 2.5)`, **When** compiled and executed, **Then** integer is coerced to float and result is 3.5
3. **Given** special float values (infinity, NaN), **When** used in arithmetic, **Then** IEEE 754 semantics are followed
4. **Given** single-float and double-float literals, **When** compiled, **Then** the appropriate precision is preserved

---

### User Story 4 - Complex Number Arithmetic (Priority: P2)

A Lisp programmer writes code using complex numbers for mathematical computations. Complex numbers with any numeric component type (integer, ratio, float) are supported.

**Why this priority**: Complex numbers complete the numeric tower and are essential for mathematical applications, signal processing, and scientific computing.

**Independent Test**: Can be fully tested by compiling `(+ #C(1 2) #C(3 4))` and verifying the result is #C(4 6).

**Acceptance Scenarios**:

1. **Given** a Lisp expression `(+ #C(1 2) #C(3 4))`, **When** compiled and executed, **Then** the result is #C(4 6)
2. **Given** a complex number with zero imaginary part like `#C(5 0)`, **When** created, **Then** it is automatically simplified to the integer 5
3. **Given** a Lisp expression `(* #C(0 1) #C(0 1))`, **When** compiled and executed, **Then** the result is -1
4. **Given** mixed real and complex operands, **When** arithmetic is performed, **Then** real numbers are coerced to complex and results are correct

---

### User Story 5 - Mathematical Functions (Priority: P2)

A Lisp programmer uses standard mathematical functions (sqrt, expt, abs, gcd, lcm, floor, ceiling, truncate, round, mod, rem, sin, cos, etc.). These functions work correctly across all numeric types with proper type coercion.

**Why this priority**: Mathematical functions are essential for practical programming. They should work uniformly across the numeric tower following Common Lisp semantics.

**Independent Test**: Can be fully tested by compiling expressions like `(sqrt 4)`, `(expt 2 10)`, `(gcd 12 18)` and verifying correct results.

**Acceptance Scenarios**:

1. **Given** `(sqrt 4)`, **When** compiled and executed, **Then** the result is 2.0 (float per Common Lisp spec)
2. **Given** `(sqrt 2)`, **When** compiled and executed, **Then** the result is approximately 1.4142... (float)
3. **Given** `(expt 2 100)`, **When** compiled and executed, **Then** the result is the exact bignum 1267650600228229401496703205376
4. **Given** `(gcd 48 18)`, **When** compiled and executed, **Then** the result is 6
5. **Given** `(abs -5)` and `(abs #C(3 4))`, **When** compiled and executed, **Then** results are 5 and 5.0 respectively

---

### User Story 6 - Numeric Type Predicates (Priority: P3)

A Lisp programmer uses numeric type predicates to inspect and dispatch on numeric types. All standard predicates (numberp, integerp, rationalp, realp, complexp, floatp, etc.) work correctly.

**Why this priority**: Type predicates are essential for type-based dispatch and runtime type checking, but are less frequently used than the core arithmetic operations.

**Independent Test**: Can be fully tested by compiling `(integerp 42)`, `(rationalp 1/2)`, `(complexp #C(1 2))` and verifying correct boolean results.

**Acceptance Scenarios**:

1. **Given** `(numberp 42)`, **When** compiled and executed, **Then** the result is T
2. **Given** `(integerp 1/2)`, **When** compiled and executed, **Then** the result is NIL
3. **Given** `(rationalp 1/2)`, **When** compiled and executed, **Then** the result is T
4. **Given** `(realp #C(1 0))`, **When** compiled and executed, **Then** the result is T (complex with zero imaginary is real)
5. **Given** `(complexp 5)`, **When** compiled and executed, **Then** the result is NIL

---

### Edge Cases

- What happens when dividing by zero? (Should signal appropriate error condition)
- How does the system handle overflow from fixnum to bignum? (Automatic promotion)
- What happens with ratios involving very large bignums? (Should compute correctly, may be slow)
- How are negative zero and positive zero handled in floats? (IEEE 754 semantics)
- What happens when complex results have zero imaginary parts? (Automatic simplification to real)
- How does type coercion work across the numeric tower? (Follow Common Lisp contagion rules)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement bignum type supporting arbitrary-precision integers
- **FR-002**: System MUST implement ratio type with exact rational arithmetic
- **FR-003**: System MUST implement float type supporting at least double-precision IEEE 754
- **FR-004**: System MUST implement complex type with any numeric component type
- **FR-005**: System MUST automatically coerce numeric types following Common Lisp contagion rules
- **FR-006**: System MUST implement arithmetic operations (+, -, *, /) for all numeric types
- **FR-007**: System MUST implement comparison operations (=, /=, <, >, <=, >=) for real numbers
- **FR-008**: System MUST implement integer division operations (floor, ceiling, truncate, round)
- **FR-009**: System MUST implement mathematical functions (sqrt, expt, abs, gcd, lcm)
- **FR-010**: System MUST implement numeric type predicates (numberp, integerp, rationalp, realp, complexp, floatp)
- **FR-011**: System MUST simplify ratios to lowest terms automatically
- **FR-012**: System MUST simplify complex numbers with zero imaginary part to real numbers
- **FR-013**: System MUST generate valid WasmGC code executable by wasmtime
- **FR-014**: System MUST automatically promote fixnum to bignum on overflow
- **FR-015**: System MUST support numeric literals in standard Common Lisp syntax (1/2, 1.5, 1.5d0, #C(1 2))

### Key Entities

- **Fixnum**: Machine-word-sized integer (existing type, to be integrated into tower)
- **Bignum**: Arbitrary-precision integer, represented as array of digits
- **Ratio**: Exact rational number with integer numerator and denominator
- **Float**: IEEE 754 floating-point number (single or double precision)
- **Complex**: Complex number with real and imaginary parts (any numeric type)
- **Numeric Tower Hierarchy**: number > complex > real > rational > integer > fixnum/bignum

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing compiler tests continue to pass after numeric tower implementation
- **SC-002**: Bignum arithmetic produces correct results for numbers up to at least 1000 digits
- **SC-003**: Ratio arithmetic maintains exactness (no floating-point conversion unless explicitly requested)
- **SC-004**: Mixed-type arithmetic follows Common Lisp type contagion rules correctly
- **SC-005**: Mathematical functions produce results matching Common Lisp reference implementation (SBCL) to within floating-point precision
- **SC-006**: Numeric predicates return correct results for all numeric types
- **SC-007**: Generated Wasm code executes successfully in wasmtime without runtime errors
- **SC-008**: Type coercion is transparent to the programmer (no explicit conversion needed for standard operations)

## Assumptions

- Wasmtime supports WasmGC features needed for numeric type representation
- IEEE 754 double-precision is sufficient for the float type (single-float may be represented internally as double)
- Bignum performance is acceptable for practical use cases (not optimized for cryptographic applications)
- Standard Common Lisp numeric literal syntax is already parsed by the compiler frontend
- The existing fixnum implementation will be integrated into the numeric tower rather than replaced
