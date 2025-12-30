# Feature Specification: Phase 14A - Basic Arithmetic Function Extension

**Feature Branch**: `002-numeric-functions`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 14A: 基本算術関数拡張を実装する。目標はANSI CL数値関連テスト準拠率50%+。実装対象: (1) 三角関数群, (2) 双曲線関数群, (3) ビット演算関数, (4) 数学関数, (5) 数値変換, (6) parse-integer完全実装"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Trigonometric Calculations (Priority: P1)

As a Clysm user, I want to perform trigonometric calculations (sin, cos, tan and their inverses) so that I can implement mathematical algorithms and graphics computations in my compiled Wasm programs.

**Why this priority**: Trigonometric functions are fundamental to many mathematical computations and are commonly used in graphics, physics simulations, and scientific computing. They form the foundation for more complex mathematical operations.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(sin (/ pi 2))` and verifying the output is approximately 1.0.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(sin (/ pi 2))`, **When** compiled to Wasm and executed, **Then** the result is approximately 1.0 (within floating-point tolerance)
2. **Given** a Clysm program with `(cos 0)`, **When** compiled to Wasm and executed, **Then** the result is 1.0
3. **Given** a Clysm program with `(tan (/ pi 4))`, **When** compiled to Wasm and executed, **Then** the result is approximately 1.0
4. **Given** a Clysm program with `(asin 1)`, **When** compiled to Wasm and executed, **Then** the result is approximately pi/2
5. **Given** a Clysm program with `(atan 1 1)`, **When** compiled to Wasm and executed, **Then** the result is approximately pi/4 (two-argument form)

---

### User Story 2 - Bit Operation Functions (Priority: P1)

As a Clysm user, I want to perform bitwise operations (shift, and, or, xor, not, count) so that I can implement low-level algorithms and data manipulation in my compiled Wasm programs.

**Why this priority**: Bit operations are essential for systems programming, cryptographic algorithms, hash functions, and efficient data manipulation. They directly map to Wasm i32/i64 instructions for optimal performance.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(ash 1 10)` and verifying the output is 1024.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(ash 1 10)`, **When** compiled to Wasm and executed, **Then** the result is 1024
2. **Given** a Clysm program with `(ash 1024 -10)`, **When** compiled to Wasm and executed, **Then** the result is 1 (right shift)
3. **Given** a Clysm program with `(logand #xFF #x0F)`, **When** compiled to Wasm and executed, **Then** the result is #x0F (15)
4. **Given** a Clysm program with `(logior #xF0 #x0F)`, **When** compiled to Wasm and executed, **Then** the result is #xFF (255)
5. **Given** a Clysm program with `(logxor #xFF #x0F)`, **When** compiled to Wasm and executed, **Then** the result is #xF0 (240)
6. **Given** a Clysm program with `(lognot 0)`, **When** compiled to Wasm and executed, **Then** the result is -1
7. **Given** a Clysm program with `(logcount #xFF)`, **When** compiled to Wasm and executed, **Then** the result is 8

---

### User Story 3 - Mathematical Functions (Priority: P2)

As a Clysm user, I want to use standard mathematical functions (exp, log, sqrt, expt, abs, signum) so that I can implement scientific and numerical algorithms in my compiled Wasm programs.

**Why this priority**: These functions are core mathematical operations required for numerical computing, but depend on the trigonometric foundation being in place for comprehensive math support.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(sqrt 4)` and verifying the output is 2.0.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(sqrt 4)`, **When** compiled to Wasm and executed, **Then** the result is 2.0
2. **Given** a Clysm program with `(sqrt 2)`, **When** compiled to Wasm and executed, **Then** the result is approximately 1.414
3. **Given** a Clysm program with `(exp 1)`, **When** compiled to Wasm and executed, **Then** the result is approximately e (2.718...)
4. **Given** a Clysm program with `(log (exp 1))`, **When** compiled to Wasm and executed, **Then** the result is 1.0
5. **Given** a Clysm program with `(expt 2 10)`, **When** compiled to Wasm and executed, **Then** the result is 1024
6. **Given** a Clysm program with `(abs -5)`, **When** compiled to Wasm and executed, **Then** the result is 5
7. **Given** a Clysm program with `(signum -42)`, **When** compiled to Wasm and executed, **Then** the result is -1

---

### User Story 4 - Hyperbolic Functions (Priority: P2)

As a Clysm user, I want to perform hyperbolic calculations (sinh, cosh, tanh and their inverses) so that I can implement engineering and physics calculations in my compiled Wasm programs.

**Why this priority**: Hyperbolic functions are used in specialized mathematical domains (catenary curves, signal processing, relativistic physics) and complement the trigonometric functions.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(cosh 0)` and verifying the output is 1.0.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(sinh 0)`, **When** compiled to Wasm and executed, **Then** the result is 0.0
2. **Given** a Clysm program with `(cosh 0)`, **When** compiled to Wasm and executed, **Then** the result is 1.0
3. **Given** a Clysm program with `(tanh 0)`, **When** compiled to Wasm and executed, **Then** the result is 0.0
4. **Given** a Clysm program with `(asinh 0)`, **When** compiled to Wasm and executed, **Then** the result is 0.0
5. **Given** a Clysm program with `(acosh 1)`, **When** compiled to Wasm and executed, **Then** the result is 0.0

---

### User Story 5 - Numeric Type Conversion (Priority: P3)

As a Clysm user, I want to convert between numeric types (integer to float, float to rational) so that I can manage numeric precision and type requirements in my programs.

**Why this priority**: Type conversion is important for interoperability between different numeric types but is less frequently needed than core arithmetic operations.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(float 5)` and verifying the output is 5.0.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(float 5)`, **When** compiled to Wasm and executed, **Then** the result is 5.0
2. **Given** a Clysm program with `(float 1/2)`, **When** compiled to Wasm and executed, **Then** the result is 0.5
3. **Given** a Clysm program with `(rational 0.5)`, **When** compiled to Wasm and executed, **Then** the result is 1/2

---

### User Story 6 - Parse Integer from Strings (Priority: P3)

As a Clysm user, I want to parse integers from strings with full ANSI CL compliance (radix support, sign handling, boundary detection) so that I can handle user input and data parsing in my programs.

**Why this priority**: String-to-integer parsing is essential for user input handling but is a more specialized operation that depends on the core numeric infrastructure.

**Independent Test**: Can be fully tested by compiling and executing expressions like `(parse-integer "123")` and verifying the output is 123.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(parse-integer "123")`, **When** compiled to Wasm and executed, **Then** the primary return value is 123 and secondary value is 3 (end position)
2. **Given** a Clysm program with `(parse-integer "FF" :radix 16)`, **When** compiled to Wasm and executed, **Then** the result is 255
3. **Given** a Clysm program with `(parse-integer "  42  " :junk-allowed nil)`, **When** compiled to Wasm and executed, **Then** the result is 42 (whitespace trimmed)
4. **Given** a Clysm program with `(parse-integer "-789")`, **When** compiled to Wasm and executed, **Then** the result is -789
5. **Given** a Clysm program with `(parse-integer "abc" :junk-allowed t)`, **When** compiled to Wasm and executed, **Then** the result is NIL (no valid integer)

---

### Edge Cases

- What happens when trigonometric functions receive infinity or NaN inputs? System should return appropriate IEEE 754 special values (NaN, infinity)
- What happens when sqrt receives a negative number? System should signal a domain error or return NaN depending on implementation choice
- What happens when ash shift count exceeds integer width? System should handle according to ANSI CL semantics (large shifts produce 0 or sign-extended values)
- What happens when logcount receives a negative integer? System should count bits in two's complement representation
- What happens when parse-integer receives an empty string? System should signal an error or return NIL based on :junk-allowed
- What happens when expt receives 0^0? System should return 1 per ANSI CL convention
- What happens when log receives 0? System should signal error or return negative infinity

## Requirements *(mandatory)*

### Functional Requirements

#### Trigonometric Functions
- **FR-001**: System MUST implement `sin` function accepting a number and returning its sine as a float
- **FR-002**: System MUST implement `cos` function accepting a number and returning its cosine as a float
- **FR-003**: System MUST implement `tan` function accepting a number and returning its tangent as a float
- **FR-004**: System MUST implement `asin` function accepting a number in [-1, 1] and returning its arc sine
- **FR-005**: System MUST implement `acos` function accepting a number in [-1, 1] and returning its arc cosine
- **FR-006**: System MUST implement `atan` function accepting one or two arguments (y, x) and returning arc tangent

#### Hyperbolic Functions
- **FR-007**: System MUST implement `sinh` function accepting a number and returning its hyperbolic sine
- **FR-008**: System MUST implement `cosh` function accepting a number and returning its hyperbolic cosine
- **FR-009**: System MUST implement `tanh` function accepting a number and returning its hyperbolic tangent
- **FR-010**: System MUST implement `asinh` function accepting a number and returning its inverse hyperbolic sine
- **FR-011**: System MUST implement `acosh` function accepting a number >= 1 and returning its inverse hyperbolic cosine
- **FR-012**: System MUST implement `atanh` function accepting a number in (-1, 1) and returning its inverse hyperbolic tangent

#### Bit Operation Functions
- **FR-013**: System MUST implement `ash` (arithmetic shift) accepting an integer and shift count, supporting both left (positive) and right (negative) shifts
- **FR-014**: System MUST implement `logand` accepting zero or more integers and returning their bitwise AND
- **FR-015**: System MUST implement `logior` accepting zero or more integers and returning their bitwise OR
- **FR-016**: System MUST implement `logxor` accepting zero or more integers and returning their bitwise XOR
- **FR-017**: System MUST implement `lognot` accepting an integer and returning its bitwise complement
- **FR-018**: System MUST implement `logcount` accepting an integer and returning the count of 1 bits (for positive) or 0 bits (for negative)

#### Mathematical Functions
- **FR-019**: System MUST implement `exp` function accepting a number and returning e raised to that power
- **FR-020**: System MUST implement `log` function accepting one or two arguments (number, optional base) and returning logarithm
- **FR-021**: System MUST implement `sqrt` function accepting a non-negative number and returning its square root
- **FR-022**: System MUST implement `expt` function accepting base and exponent, returning base raised to exponent
- **FR-023**: System MUST implement `abs` function accepting a number and returning its absolute value
- **FR-024**: System MUST implement `signum` function accepting a number and returning -1, 0, or 1 based on sign

#### Numeric Type Conversion
- **FR-025**: System MUST implement `float` function converting any real number to a floating-point representation
- **FR-026**: System MUST implement `rational` function converting a real number to a rational representation

#### String Parsing
- **FR-027**: System MUST implement `parse-integer` accepting a string and keyword arguments (:start, :end, :radix, :junk-allowed)
- **FR-028**: `parse-integer` MUST return two values: the parsed integer (or NIL) and the position where parsing ended
- **FR-029**: `parse-integer` MUST support radix values from 2 to 36
- **FR-030**: `parse-integer` MUST handle leading/trailing whitespace per ANSI CL specification

### Key Entities

- **Float**: IEEE 754 double-precision floating-point number (represented internally)
- **Integer**: Arbitrary-precision integer (small values optimized, large values boxed)
- **Rational**: Ratio of two integers (numerator/denominator pair)
- **Constant PI**: Mathematical constant π (3.14159...) available for trigonometric calculations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: ANSI CL numeric-related test compliance rate reaches 50% or higher (current baseline to be established)
- **SC-002**: All 6 trigonometric functions (sin, cos, tan, asin, acos, atan) compile and execute correctly for standard inputs
- **SC-003**: All 6 hyperbolic functions (sinh, cosh, tanh, asinh, acosh, atanh) compile and execute correctly for standard inputs
- **SC-004**: All 6 bit operation functions (ash, logand, logior, logxor, lognot, logcount) compile and execute correctly
- **SC-005**: All 6 mathematical functions (exp, log, sqrt, expt, abs, signum) compile and execute correctly for standard inputs
- **SC-006**: Both numeric conversion functions (float, rational) compile and execute correctly
- **SC-007**: parse-integer fully implements ANSI CL specification including all keyword arguments
- **SC-008**: Reference verification expressions return expected values:
  - `(sin (/ pi 2))` ≈ 1.0 (within 1e-10 tolerance)
  - `(ash 1 10)` = 1024 (exact)
  - `(sqrt 4)` = 2.0 (exact)
- **SC-009**: Floating-point results maintain double precision accuracy (15-17 significant digits)
- **SC-010**: Bit operations correctly handle both positive and negative integers up to 64-bit range

## Assumptions

- The compiler already supports basic arithmetic operations (+, -, *, /)
- Wasm f64 instructions (f64.sqrt, f64.abs, f64.neg, etc.) are available and correctly implemented
- Wasm i32/i64 bit instructions (i32.and, i32.or, i32.xor, i32.shl, i32.shr_s, i32.clz, i32.popcnt) are available
- FFI to JavaScript Math API is available for complex transcendental functions not directly supported by Wasm
- The constant PI is defined or can be computed with sufficient precision
- Multiple-value returns are functional (required for parse-integer)

## Constraints

- Functions must compile to valid bytecode that passes validation
- Trigonometric and hyperbolic functions may use FFI calls where native support is unavailable
- Bit operations must produce results consistent with ANSI Common Lisp semantics for arbitrary-precision integers
- parse-integer must handle the full ANSI CL specification including all edge cases
