# Feature Specification: ANSI Numeric Functions Extension

**Feature Branch**: `001-numeric-functions`
**Created**: 2025-12-28
**Status**: Draft
**Input**: Phase 14A - Extend numeric/arithmetic functions to achieve ANSI Common Lisp compliance with 50%+ numbers category test pass rate

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Arithmetic and Comparison Functions (Priority: P1)

As a Common Lisp developer, I need the compiler to support fundamental numeric operations (abs, signum, max, min, gcd, lcm) so that I can write standard Common Lisp code for basic mathematical computations.

**Why this priority**: These are the most frequently used numeric functions in any Common Lisp program. Without them, even simple calculations become impossible.

**Independent Test**: Can be fully tested by evaluating expressions like `(abs -5)`, `(max 1 2 3)`, `(gcd 12 18)` and verifying correct results.

**Acceptance Scenarios**:

1. **Given** a negative number, **When** `abs` is called, **Then** return its positive absolute value
2. **Given** multiple numeric arguments, **When** `max` is called, **Then** return the largest value
3. **Given** multiple numeric arguments, **When** `min` is called, **Then** return the smallest value
4. **Given** two integers, **When** `gcd` is called, **Then** return their greatest common divisor
5. **Given** two integers, **When** `lcm` is called, **Then** return their least common multiple
6. **Given** any real number, **When** `signum` is called, **Then** return -1, 0, or 1 based on sign

---

### User Story 2 - Trigonometric Functions (Priority: P1)

As a Common Lisp developer, I need trigonometric functions (sin, cos, tan, asin, acos, atan) so that I can perform geometric and scientific calculations using standard ANSI Common Lisp.

**Why this priority**: Trigonometric functions are essential for scientific computing, graphics, and many numerical algorithms. They form a core part of the ANSI standard.

**Independent Test**: Can be fully tested by evaluating `(sin (/ pi 2))` and verifying it returns approximately 1.0, and similar tests for other trig functions.

**Acceptance Scenarios**:

1. **Given** pi/2 radians, **When** `sin` is called, **Then** return approximately 1.0
2. **Given** 0 radians, **When** `cos` is called, **Then** return approximately 1.0
3. **Given** pi/4 radians, **When** `tan` is called, **Then** return approximately 1.0
4. **Given** 1.0, **When** `asin` is called, **Then** return approximately pi/2
5. **Given** 1.0, **When** `acos` is called, **Then** return approximately 0
6. **Given** two arguments (y, x), **When** `atan` is called, **Then** return the angle in the correct quadrant

---

### User Story 3 - Bitwise Operations (Priority: P1)

As a Common Lisp developer, I need bitwise operations (ash, logand, logior, logxor, lognot, logcount, integer-length) so that I can perform low-level bit manipulation on integers.

**Why this priority**: Bitwise operations are essential for systems programming, encoding/decoding, and performance-critical code. They are heavily tested in compliance suites.

**Independent Test**: Can be fully tested by evaluating `(ash 1 10)` and verifying it returns 1024, and `(logand #xFF00 #x0FF0)` returning #x0F00.

**Acceptance Scenarios**:

1. **Given** integer 1 and shift count 10, **When** `ash` is called, **Then** return 1024
2. **Given** #xFF00 and #x0FF0, **When** `logand` is called, **Then** return #x0F00
3. **Given** #xFF00 and #x00FF, **When** `logior` is called, **Then** return #xFFFF
4. **Given** two integers, **When** `logxor` is called, **Then** return their exclusive-or
5. **Given** an integer, **When** `lognot` is called, **Then** return its bitwise complement
6. **Given** an integer, **When** `logcount` is called, **Then** return count of 1-bits (positive) or 0-bits (negative)
7. **Given** an integer, **When** `integer-length` is called, **Then** return minimum bits needed to represent it

---

### User Story 4 - Mathematical Functions (Priority: P2)

As a Common Lisp developer, I need mathematical functions (exp, log, sqrt, expt) so that I can perform exponential, logarithmic, and power calculations.

**Why this priority**: These functions build on P1 priorities and are required for scientific computing but less commonly used than basic arithmetic.

**Independent Test**: Can be fully tested by evaluating `(exp 1)` returning approximately e, `(sqrt 4)` returning 2, `(expt 2 10)` returning 1024.

**Acceptance Scenarios**:

1. **Given** 1, **When** `exp` is called, **Then** return approximately e (2.718281828...)
2. **Given** e, **When** `log` is called with default base, **Then** return approximately 1.0
3. **Given** 100 and base 10, **When** `log` is called, **Then** return approximately 2.0
4. **Given** 4, **When** `sqrt` is called, **Then** return 2.0
5. **Given** 2 and 10, **When** `expt` is called, **Then** return 1024
6. **Given** negative number, **When** `sqrt` is called, **Then** return a complex number

---

### User Story 5 - Hyperbolic Functions (Priority: P2)

As a Common Lisp developer, I need hyperbolic functions (sinh, cosh, tanh, asinh, acosh, atanh) so that I can perform hyperbolic calculations for scientific applications.

**Why this priority**: Hyperbolic functions complement trigonometric functions but are used less frequently. They complete the mathematical function set.

**Independent Test**: Can be fully tested by evaluating `(sinh 0)` returning 0, `(cosh 0)` returning 1.

**Acceptance Scenarios**:

1. **Given** 0, **When** `sinh` is called, **Then** return 0
2. **Given** 0, **When** `cosh` is called, **Then** return 1.0
3. **Given** 0, **When** `tanh` is called, **Then** return 0
4. **Given** 0, **When** `asinh` is called, **Then** return 0
5. **Given** 1, **When** `acosh` is called, **Then** return 0
6. **Given** 0, **When** `atanh` is called, **Then** return 0

---

### User Story 6 - Complex Number Support (Priority: P3)

As a Common Lisp developer, I need complex number operations (complex, realpart, imagpart, conjugate, phase) so that I can work with complex numbers as defined by ANSI Common Lisp.

**Why this priority**: Complex numbers are required for complete ANSI compliance and enable advanced mathematical operations, but are less commonly used than real-number operations.

**Independent Test**: Can be fully tested by evaluating `(realpart #C(3 4))` returning 3, `(imagpart #C(3 4))` returning 4.

**Acceptance Scenarios**:

1. **Given** real 3 and imaginary 4, **When** `complex` is called, **Then** return #C(3 4)
2. **Given** #C(3 4), **When** `realpart` is called, **Then** return 3
3. **Given** #C(3 4), **When** `imagpart` is called, **Then** return 4
4. **Given** #C(3 4), **When** `conjugate` is called, **Then** return #C(3 -4)
5. **Given** #C(0 1), **When** `phase` is called, **Then** return approximately pi/2

---

### Edge Cases

- What happens when `ash` receives a negative shift count? (Right shift should occur)
- What happens when `log` receives 0? (Should signal an error or return negative infinity per implementation)
- What happens when `asin`/`acos` receives values outside [-1, 1]? (Should return complex numbers)
- What happens when `sqrt` receives a negative number? (Should return complex number)
- What happens when `gcd`/`lcm` receive zero? (gcd(n, 0) = n, lcm(n, 0) = 0)
- What happens when numeric functions receive mixed types (integer/float/ratio)? (Type contagion rules apply)
- What happens when `atan` is called with two zero arguments? (Should signal error or return 0 per implementation)
- What happens when `expt` raises 0 to negative power? (Should signal division-by-zero error)

## Requirements *(mandatory)*

### Functional Requirements

**Basic Functions**
- **FR-001**: System MUST implement `abs` returning the absolute value of any real number
- **FR-002**: System MUST implement `signum` returning -1, 0, or 1 based on the sign of a real number
- **FR-003**: System MUST implement `max` accepting one or more real arguments and returning the largest
- **FR-004**: System MUST implement `min` accepting one or more real arguments and returning the smallest
- **FR-005**: System MUST implement `gcd` computing the greatest common divisor of zero or more integers
- **FR-006**: System MUST implement `lcm` computing the least common multiple of zero or more integers

**Trigonometric Functions**
- **FR-007**: System MUST implement `sin` computing the sine of a number in radians
- **FR-008**: System MUST implement `cos` computing the cosine of a number in radians
- **FR-009**: System MUST implement `tan` computing the tangent of a number in radians
- **FR-010**: System MUST implement `asin` computing the arc sine, returning radians
- **FR-011**: System MUST implement `acos` computing the arc cosine, returning radians
- **FR-012**: System MUST implement `atan` computing the arc tangent, supporting both one and two argument forms

**Hyperbolic Functions**
- **FR-013**: System MUST implement `sinh` computing the hyperbolic sine
- **FR-014**: System MUST implement `cosh` computing the hyperbolic cosine
- **FR-015**: System MUST implement `tanh` computing the hyperbolic tangent
- **FR-016**: System MUST implement `asinh` computing the inverse hyperbolic sine
- **FR-017**: System MUST implement `acosh` computing the inverse hyperbolic cosine
- **FR-018**: System MUST implement `atanh` computing the inverse hyperbolic tangent

**Bitwise Operations**
- **FR-019**: System MUST implement `ash` performing arithmetic shift on integers
- **FR-020**: System MUST implement `logand` computing bitwise AND of zero or more integers
- **FR-021**: System MUST implement `logior` computing bitwise inclusive OR of zero or more integers
- **FR-022**: System MUST implement `logxor` computing bitwise exclusive OR of zero or more integers
- **FR-023**: System MUST implement `lognot` computing bitwise complement of an integer
- **FR-024**: System MUST implement `logcount` counting 1-bits (positive) or 0-bits (negative) in an integer
- **FR-025**: System MUST implement `integer-length` returning minimum bits needed to represent an integer

**Mathematical Functions**
- **FR-026**: System MUST implement `exp` computing e raised to a given power
- **FR-027**: System MUST implement `log` computing natural logarithm, with optional base parameter
- **FR-028**: System MUST implement `sqrt` computing the principal square root
- **FR-029**: System MUST implement `expt` computing a base raised to an exponent power

**Complex Numbers**
- **FR-030**: System MUST implement `complex` creating a complex number from real and imaginary parts
- **FR-031**: System MUST implement `realpart` extracting the real component of a number
- **FR-032**: System MUST implement `imagpart` extracting the imaginary component of a number
- **FR-033**: System MUST implement `conjugate` computing the complex conjugate
- **FR-034**: System MUST implement `phase` computing the angle of a complex number in radians

**Type Handling**
- **FR-035**: System MUST apply ANSI Common Lisp type contagion rules for mixed numeric types
- **FR-036**: System MUST return complex numbers when real-valued functions produce imaginary results

### Key Entities

- **Integer**: Arbitrary-precision whole numbers supporting bitwise operations
- **Float**: IEEE 754 double-precision floating-point numbers (using WasmGC f64)
- **Ratio**: Exact rational numbers (numerator/denominator pairs)
- **Complex**: Numbers with real and imaginary components

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Numbers category test pass rate reaches 50% or higher (up from current baseline)
- **SC-002**: All 6 trigonometric functions pass standard compliance tests
- **SC-003**: All 7 bitwise operations pass standard compliance tests
- **SC-004**: Verification expressions produce correct results:
  - `(sin (/ pi 2))` evaluates to approximately 1.0
  - `(ash 1 10)` evaluates to exactly 1024
  - `(logand #xFF00 #x0FF0)` evaluates to exactly #x0F00
- **SC-005**: All functions handle edge cases without crashing (appropriate errors for invalid inputs)
- **SC-006**: Mixed-type operations produce correct results following ANSI type contagion rules

## Assumptions

- WasmGC f64 instructions will be used for floating-point operations where available
- The existing numeric tower implementation (Feature 010/019) provides the foundation for type handling
- The pi constant is available in the runtime environment
- Error signaling follows ANSI Common Lisp condition system conventions
- Bitwise operations operate on the integer representation (no arbitrary precision beyond 64 bits initially)

## Dependencies

- Feature 010: Numeric tower foundation
- Feature 019: Numeric accessors and IEEE 754 floats
- WasmGC f64 instruction support in the target runtime
