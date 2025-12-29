# Feature Specification: ANSI Common Lisp Numeric Functions

**Feature Branch**: `001-ansi-numeric-functions`
**Created**: 2025-12-29
**Status**: Draft
**Input**: User description: "Phase 14A: ANSI Common Lisp準拠の数値関数を完全実装する。目標はnumbersカテゴリテスト準拠率50%+。実装対象: (1) 三角関数 sin/cos/tan/asin/acos/atan、(2) 双曲線関数 sinh/cosh/tanh/asinh/acosh/atanh、(3) ビット演算 ash/logand/logior/logxor/lognot/logcount、(4) 複素数 complex/realpart/imagpart/conjugate/phase、(5) 数学関数 exp/log/sqrt/expt/abs/signum/max/min/gcd/lcm。FFI経由でWasm importを活用し、ANSI仕様に準拠した振る舞いを実現すること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Trigonometric Function Computation (Priority: P1)

A compiler user evaluates trigonometric expressions in compiled Lisp code. The functions must accept real numbers and return floating-point results consistent with ANSI Common Lisp behavior.

**Why this priority**: Trigonometric functions are fundamental for scientific computation and have well-defined FFI mappings to standard math libraries.

**Independent Test**: Can be fully tested by evaluating expressions like `(sin 0.5)`, `(cos pi)`, `(atan 1 1)` and validating results against IEEE 754 double-precision values.

**Acceptance Scenarios**:

1. **Given** a real number argument, **When** `sin`, `cos`, or `tan` is called, **Then** the result matches the IEEE 754 double-precision value for that operation.
2. **Given** a value within the valid domain, **When** `asin`, `acos`, or `atan` is called, **Then** the result is the corresponding arc value in radians.
3. **Given** two numeric arguments, **When** `atan` is called with two arguments, **Then** the result is the arc tangent of y/x using the signs to determine the quadrant.

---

### User Story 2 - Bitwise Integer Operations (Priority: P1)

A compiler user performs bitwise operations on integers. The functions must handle arbitrary-precision integers and return correct results per ANSI specification.

**Why this priority**: Bit operations are essential for low-level programming, cryptography, and system utilities. They operate purely on integers without FFI dependencies.

**Independent Test**: Can be fully tested by evaluating expressions like `(logand #b1100 #b1010)`, `(ash 1 10)`, `(logcount 255)` and verifying against expected bit patterns.

**Acceptance Scenarios**:

1. **Given** two integer arguments, **When** `logand`, `logior`, or `logxor` is called, **Then** the result is the bitwise AND, OR, or XOR respectively.
2. **Given** an integer and a shift count, **When** `ash` is called, **Then** the result is the arithmetic shift (left for positive count, right for negative).
3. **Given** an integer, **When** `lognot` is called, **Then** the result is the bitwise complement (two's complement).
4. **Given** an integer, **When** `logcount` is called, **Then** the result is the count of 1-bits for positive integers or 0-bits for negative integers.

---

### User Story 3 - Mathematical Functions (Priority: P1)

A compiler user evaluates mathematical expressions including exponentials, logarithms, roots, and basic operations. The functions must conform to ANSI behavior with proper handling of edge cases.

**Why this priority**: Core math functions are required for most numerical applications and directly map to Wasm/JavaScript Math functions via FFI.

**Independent Test**: Can be fully tested by evaluating expressions like `(exp 1)`, `(log 10)`, `(sqrt 2)`, `(expt 2 10)`, `(gcd 48 18)`, `(lcm 4 6)` and verifying results.

**Acceptance Scenarios**:

1. **Given** a numeric argument, **When** `exp`, `log`, or `sqrt` is called, **Then** the result is the exponential, natural logarithm, or square root respectively.
2. **Given** two numeric arguments, **When** `expt` is called, **Then** the result is the first argument raised to the power of the second.
3. **Given** a numeric argument, **When** `abs` or `signum` is called, **Then** the result is the absolute value or sign indicator (-1, 0, 1) respectively.
4. **Given** multiple numeric arguments, **When** `max` or `min` is called, **Then** the result is the largest or smallest argument respectively.
5. **Given** two or more non-negative integers, **When** `gcd` or `lcm` is called, **Then** the result is the greatest common divisor or least common multiple.

---

### User Story 4 - Hyperbolic Function Computation (Priority: P2)

A compiler user evaluates hyperbolic function expressions. The functions must accept real numbers and return correct floating-point results.

**Why this priority**: Hyperbolic functions are less commonly used than trigonometric but are important for certain scientific and engineering applications.

**Independent Test**: Can be fully tested by evaluating expressions like `(sinh 0)`, `(cosh 0)`, `(tanh 0.5)` and validating against IEEE 754 values.

**Acceptance Scenarios**:

1. **Given** a real number argument, **When** `sinh`, `cosh`, or `tanh` is called, **Then** the result is the hyperbolic sine, cosine, or tangent respectively.
2. **Given** a value within the valid domain, **When** `asinh`, `acosh`, or `atanh` is called, **Then** the result is the inverse hyperbolic function value.

---

### User Story 5 - Complex Number Operations (Priority: P2)

A compiler user creates and manipulates complex numbers. The functions must support complex number construction and component extraction per ANSI specification.

**Why this priority**: Complex number support is required for ANSI compliance but is less commonly used in typical Lisp applications.

**Independent Test**: Can be fully tested by evaluating expressions like `(complex 3 4)`, `(realpart #C(3 4))`, `(abs #C(3 4))`, `(phase #C(0 1))` and verifying results.

**Acceptance Scenarios**:

1. **Given** two real numbers, **When** `complex` is called, **Then** the result is a complex number with those real and imaginary parts.
2. **Given** a complex number, **When** `realpart` or `imagpart` is called, **Then** the result is the real or imaginary component respectively.
3. **Given** a complex number, **When** `conjugate` is called, **Then** the result is the complex conjugate (negated imaginary part).
4. **Given** a complex number, **When** `phase` is called, **Then** the result is the angle in radians (equivalent to `(atan (imagpart z) (realpart z))`).
5. **Given** a complex number, **When** `abs` is called, **Then** the result is the magnitude (distance from origin).

---

### Edge Cases

- What happens when trigonometric inverse functions receive out-of-domain values (e.g., `(asin 2)`)?
  - Per ANSI CL, this should return a complex number or signal an error depending on implementation choice.
  - **Assumption**: Return a complex result for mathematical consistency.
- How does `log` handle zero or negative arguments?
  - Per ANSI CL, `(log 0)` signals an error; negative arguments return complex results.
- How does `sqrt` handle negative arguments?
  - Per ANSI CL, `(sqrt -1)` returns `#C(0 1)` (pure imaginary).
- How does `ash` handle very large shift counts?
  - Per ANSI CL, large left shifts produce correspondingly large integers; large right shifts produce 0 or -1.
- How does `expt` handle special cases like `(expt 0 0)`?
  - Per ANSI CL, `(expt 0 0)` returns 1.
- How do `gcd` and `lcm` handle zero arguments?
  - Per ANSI CL, `(gcd)` returns 0, `(lcm)` returns 1, `(gcd n 0)` returns `|n|`, `(lcm n 0)` returns 0.

## Requirements *(mandatory)*

### Functional Requirements

#### Trigonometric Functions

- **FR-001**: System MUST implement `sin` accepting a real number and returning the sine as a float.
- **FR-002**: System MUST implement `cos` accepting a real number and returning the cosine as a float.
- **FR-003**: System MUST implement `tan` accepting a real number and returning the tangent as a float.
- **FR-004**: System MUST implement `asin` accepting a number in [-1, 1] and returning the arc sine in radians.
- **FR-005**: System MUST implement `acos` accepting a number in [-1, 1] and returning the arc cosine in radians.
- **FR-006**: System MUST implement `atan` accepting one or two arguments and returning the arc tangent in radians.

#### Hyperbolic Functions

- **FR-007**: System MUST implement `sinh` accepting a real number and returning the hyperbolic sine.
- **FR-008**: System MUST implement `cosh` accepting a real number and returning the hyperbolic cosine.
- **FR-009**: System MUST implement `tanh` accepting a real number and returning the hyperbolic tangent.
- **FR-010**: System MUST implement `asinh` accepting a real number and returning the inverse hyperbolic sine.
- **FR-011**: System MUST implement `acosh` accepting a number >= 1 and returning the inverse hyperbolic cosine.
- **FR-012**: System MUST implement `atanh` accepting a number in (-1, 1) and returning the inverse hyperbolic tangent.

#### Bitwise Operations

- **FR-013**: System MUST implement `ash` accepting an integer and shift count, returning the arithmetic-shifted result.
- **FR-014**: System MUST implement `logand` accepting zero or more integers, returning the bitwise AND (identity -1 for no args).
- **FR-015**: System MUST implement `logior` accepting zero or more integers, returning the bitwise inclusive OR (identity 0 for no args).
- **FR-016**: System MUST implement `logxor` accepting zero or more integers, returning the bitwise exclusive OR (identity 0 for no args).
- **FR-017**: System MUST implement `lognot` accepting an integer, returning the bitwise complement.
- **FR-018**: System MUST implement `logcount` accepting an integer, returning the count of 1-bits (positive) or 0-bits (negative).

#### Complex Number Operations

- **FR-019**: System MUST implement `complex` accepting two real numbers and returning a complex number.
- **FR-020**: System MUST implement `realpart` accepting a number and returning the real component.
- **FR-021**: System MUST implement `imagpart` accepting a number and returning the imaginary component (0 for reals).
- **FR-022**: System MUST implement `conjugate` accepting a number and returning the complex conjugate.
- **FR-023**: System MUST implement `phase` accepting a number and returning the angle in radians.

#### Mathematical Functions

- **FR-024**: System MUST implement `exp` accepting a number and returning e raised to that power.
- **FR-025**: System MUST implement `log` accepting one or two arguments (number and optional base), returning the logarithm.
- **FR-026**: System MUST implement `sqrt` accepting a number and returning the principal square root.
- **FR-027**: System MUST implement `expt` accepting a base and exponent, returning the power.
- **FR-028**: System MUST implement `abs` accepting a number and returning the absolute value (magnitude for complex).
- **FR-029**: System MUST implement `signum` accepting a number and returning the sign indicator.
- **FR-030**: System MUST implement `max` accepting one or more real numbers, returning the largest.
- **FR-031**: System MUST implement `min` accepting one or more real numbers, returning the smallest.
- **FR-032**: System MUST implement `gcd` accepting zero or more non-negative integers, returning the GCD.
- **FR-033**: System MUST implement `lcm` accepting zero or more non-negative integers, returning the LCM.

#### FFI Integration

- **FR-034**: System MUST utilize Wasm imports (via host FFI) for standard math operations (sin, cos, exp, log, sqrt, etc.).
- **FR-035**: System MUST handle type coercion appropriately (integers to floats for transcendental functions).

### Key Entities

- **Float**: IEEE 754 double-precision floating-point number (WasmGC type index 4).
- **Integer**: Arbitrary-precision integer (fixnum or bignum).
- **Complex**: Complex number with real and imaginary parts (requires new WasmGC type or representation).
- **Ratio**: Rational number (numerator/denominator pair, type index 5).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Numbers category test compliance rate reaches 50% or higher (baseline to be established from current compliance).
- **SC-002**: All 35 functional requirements pass their corresponding acceptance tests.
- **SC-003**: All trigonometric functions return results matching host math library to within IEEE 754 double-precision tolerance.
- **SC-004**: All bitwise operations produce correct results for integers up to at least 64-bit range.
- **SC-005**: Complex number operations correctly handle both complex and real number inputs.
- **SC-006**: Edge cases (infinity, NaN, domain errors) are handled consistently with ANSI specification.

## Assumptions

- **A-001**: Host environment (Node.js) provides standard JavaScript Math object functions via FFI.
- **A-002**: IEEE 754 double-precision is the standard floating-point representation.
- **A-003**: Current WasmGC float type (index 4) is sufficient for trigonometric and transcendental function results.
- **A-004**: Complex numbers will be represented using an existing or new WasmGC struct type.
- **A-005**: For out-of-domain inputs to inverse trigonometric/hyperbolic functions, the implementation will return complex results rather than signaling errors (mathematical continuation approach).
- **A-006**: Bitwise operations will handle integers using two's complement representation.

## Dependencies

- **D-001**: Existing FFI foundation (Feature 027) for Wasm imports.
- **D-002**: Existing float type support (Feature 019) for IEEE 754 handling.
- **D-003**: Host shim providing JavaScript Math function bindings.
