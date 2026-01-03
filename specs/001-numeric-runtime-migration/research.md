# Research: Numeric Runtime Migration

**Feature**: 001-numeric-runtime-migration
**Date**: 2026-01-04

## Overview

This document consolidates research findings for implementing 5 ANSI CL numeric functions as a runtime library.

## Function Specifications

### 1. [parse-integer](resources/HyperSpec/Body/f_parse_.htm)

**Signature**: `(parse-integer string &key start end radix junk-allowed)`

**ANSI CL Behavior**:
- Parses an integer from STRING in the given RADIX (default 10)
- Returns two values: the integer and the index where parsing stopped
- With `:junk-allowed nil` (default), signals `parse-error` if no valid integer found
- With `:junk-allowed t`, returns NIL if no valid integer found
- Skips leading whitespace, accepts optional sign (+/-)
- Valid digits are 0-9 and A-Z (case-insensitive) based on radix

**Decision**: Implement with Layer 1 primitives (char, char-code, digit-char-p logic)
**Rationale**: Standard parsing algorithm; can be implemented without Layer 2 dependencies
**Alternatives Rejected**:
- Inline Wasm: Would require complex control flow in Wasm
- FFI to host: Breaks self-hosting requirement

**Layer 1 Primitives Required**:
- `char`, `char-code`, `length` (string access)
- `+`, `-`, `*` (arithmetic)
- `=`, `<`, `<=`, `>=` (comparison)
- `values` (multiple value return)

### 2. [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm)

**Signature**: `(write-to-string object &key base ...)`

**ANSI CL Behavior**:
- Converts OBJECT to its printed representation as a string
- For integers with `:base`, outputs in specified radix (2-36)
- Default `:base` is `*print-base*` (10)
- Uppercase letters A-Z for digits 10-35

**Decision**: Implement integer and ratio cases; delegate complex cases to existing print infrastructure
**Rationale**: Integer-to-string with radix is the most common use case for numeric work
**Alternatives Rejected**:
- Full `write-to-string` with all keywords: Too complex for initial implementation

**Layer 1 Primitives Required**:
- `floor`, `mod`, `abs` (integer decomposition)
- `make-string`, `(setf char)` (string construction)
- `zerop`, `minusp` (sign handling)

### 3. [rationalize](resources/HyperSpec/Body/f_ration.htm)

**Signature**: `(rationalize number)`

**ANSI CL Behavior**:
- Converts a real number to a rational approximation
- For floats, uses continued fraction algorithm to find "simplest" ratio
- For integers/ratios, returns the input unchanged
- Unlike `rational`, may return an approximation (not exact bit-for-bit conversion)

**Decision**: Implement continued fraction algorithm with configurable tolerance
**Rationale**: Standard algorithm, well-documented, uses only basic arithmetic
**Alternatives Rejected**:
- Exact float-to-ratio (like `rational`): Different semantics, more complex

**Algorithm (Continued Fraction)**:
```
Given float x:
1. If x is integer, return x
2. a0 = floor(x), remainder = x - a0
3. Iterate: a[n+1] = floor(1/remainder), remainder = 1/remainder - a[n+1]
4. Build convergent p[n]/q[n] until |p[n]/q[n] - x| < tolerance
5. Return p[n]/q[n] as ratio
```

**Layer 1 Primitives Required**:
- `floor`, `truncate` (integer part extraction)
- `/`, `-` (arithmetic)
- `abs`, `<` (convergence check)
- `integerp`, `rationalp`, `floatp` (type predicates)

### 4. [signum](resources/HyperSpec/Body/f_signum.htm)

**Signature**: `(signum number)`

**ANSI CL Behavior**:
- Returns the sign of NUMBER in the same numeric type
- For integers: -1, 0, or 1
- For floats: -1.0, 0.0, or 1.0 (preserving float subtype)
- For complex: NUMBER / |NUMBER| (unit complex on the unit circle)
- For zero: returns zero in the same type

**Decision**: Type-dispatched implementation with separate paths for int/float/complex
**Rationale**: Simple branching based on type predicates
**Alternatives Rejected**: None - straightforward implementation

**Type Congruence Rules**:
| Input Type | Zero Result | Positive Result | Negative Result |
|------------|-------------|-----------------|-----------------|
| integer    | 0           | 1               | -1              |
| float      | 0.0         | 1.0             | -1.0            |
| ratio      | N/A (never zero) | 1          | -1              |
| complex    | N/A         | z/|z|           | z/|z|           |

**Layer 1 Primitives Required**:
- `zerop`, `minusp`, `plusp` (sign predicates)
- `integerp`, `floatp`, `rationalp`, `complexp` (type predicates)
- `realpart`, `imagpart`, `abs`, `/` (complex case)
- `sqrt`, `+`, `*` (magnitude calculation)

### 5. [phase](resources/HyperSpec/Body/f_phase.htm)

**Signature**: `(phase number)`

**ANSI CL Behavior**:
- Returns the angle (in radians) of NUMBER in the complex plane
- For real positive: 0
- For real negative: pi
- For zero: 0
- For complex: atan(imagpart/realpart) with proper quadrant handling

**Decision**: Use `atan` with two-argument form for proper quadrant
**Rationale**: Standard trigonometric function already available
**Alternatives Rejected**: None - standard approach

**Implementation Notes**:
- `(phase x)` = `(atan (imagpart x) (realpart x))` for complex
- For real numbers: `(if (minusp x) pi 0.0)`
- Uses `atan` two-argument form (atan2 semantics) for correct quadrant

**Layer 1 Primitives Required**:
- `realpart`, `imagpart` (complex component access)
- `atan` (trigonometric function - requires FFI or Wasm f64 ops)
- `minusp`, `zerop`, `complexp`, `realp` (type/sign predicates)

## Layer 1 Primitive Inventory

All required primitives and their availability:

| Primitive | Category | Status | Notes |
|-----------|----------|--------|-------|
| `+`, `-`, `*`, `/` | Arithmetic | Available | Basic Wasm ops |
| `floor`, `ceiling`, `truncate`, `mod` | Division | Available | Feature 001-division-rounding-primitives |
| `abs` | Arithmetic | Available | `(if (minusp x) (- x) x)` |
| `sqrt` | Math | Available | Wasm f64.sqrt |
| `atan` | Trig | Available | Feature 002-numeric-functions |
| `zerop`, `minusp`, `plusp` | Predicates | Available | Type check + comparison |
| `integerp`, `floatp`, `rationalp`, `complexp`, `numberp` | Type Predicates | Available | ref.test |
| `realpart`, `imagpart` | Complex | Available | Feature 019 |
| `char`, `char-code`, `length` | String | Available | Basic accessors |
| `make-string`, `(setf char)` | String | Available | String construction |
| `values` | Multiple Values | Available | Feature 025 |

## Edge Case Decisions

| Function | Edge Case | Behavior | Per ANSI |
|----------|-----------|----------|----------|
| parse-integer | Empty string | Signal `parse-error` | Yes |
| parse-integer | Whitespace only | Signal `parse-error` | Yes |
| parse-integer | `:junk-allowed t`, no digits | Return (values nil 0) | Yes |
| signum | 0 | 0 | Yes |
| signum | -0.0 | -0.0 (IEEE) | Yes (impl-defined) |
| signum | #C(0 0) | #C(0 0) | Yes |
| rationalize | Infinity | Signal error | Yes |
| rationalize | NaN | Signal error | Yes |
| rationalize | 0.0 | 0 (integer) | Yes |
| phase | 0 | 0.0 | Yes |
| phase | positive real | 0.0 | Yes |
| phase | negative real | pi | Yes |
| phase | #C(0 1) | pi/2 | Yes |
| write-to-string | 0 | "0" | Yes |
| write-to-string | negative | "-N" with sign | Yes |
| write-to-string | radix > 10 | Uppercase A-Z | Yes |

## Runtime Registration Design

Following the pattern from `001-string-runtime-migration`:

```lisp
(defun register-numeric-runtime-functions ()
  "Register numeric functions to use runtime library dispatch.
   Feature: 001-numeric-runtime-migration"
  ;; parse-integer: variadic for keyword args
  (register-runtime-function 'parse-integer :$parse-integer-rt nil)
  ;; write-to-string: variadic for keyword args
  (register-runtime-function 'write-to-string :$write-to-string-rt nil)
  ;; rationalize: 1 argument
  (register-runtime-function 'rationalize :$rationalize-rt 1)
  ;; signum: 1 argument
  (register-runtime-function 'signum :$signum-rt 1)
  ;; phase: 1 argument
  (register-runtime-function 'phase :$phase-rt 1))
```

## Test Strategy

| Function | Test Cases | Focus Areas |
|----------|------------|-------------|
| parse-integer | 8+ | Positive/negative, radix 2-36, keywords, errors |
| write-to-string | 6+ | Zero, negative, various radices, large numbers |
| rationalize | 5+ | Integers, exact floats (0.5), irrational approximations |
| signum | 8+ | All types (int/float/ratio/complex), zero, negative |
| phase | 6+ | Real positive/negative, complex quadrants, zero |

**Total**: 33+ test cases (exceeds FR-013 minimum of 25)

## Conclusion

All 5 functions can be implemented using available Layer 1 primitives. The implementation follows the established runtime migration pattern with function registration in `*runtime-function-table*`.
