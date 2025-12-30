# Data Model: Phase 14A - Basic Arithmetic Function Extension

**Date**: 2025-12-30
**Branch**: `002-numeric-functions`

## Overview

This document defines the data entities, type representations, and state transitions for numeric function implementation.

## Numeric Type Hierarchy

The clysm3 numeric tower follows ANSI CL with WasmGC representations:

```
                    number
                      │
          ┌───────────┴───────────┐
          │                       │
        real                   complex
          │                       │
    ┌─────┴─────┐            $complex
    │           │
  rational    float
    │           │
 ┌──┴──┐     $float
 │     │
integer ratio
 │       │
 │    $ratio
 │
 ┌──┴──┐
 │     │
fixnum bignum
 │       │
i31ref  $bignum
```

## Entity Definitions

### E1: Float ($float)

IEEE 754 double-precision floating-point number.

**WasmGC Representation**:
```wat
(type $float (struct
  (field $value f64)))
```

**Attributes**:
| Field | Type | Description |
|-------|------|-------------|
| value | f64 | IEEE 754 double (64-bit) |

**Invariants**:
- Value may be NaN, +Infinity, or -Infinity
- Precision: ~15-17 significant decimal digits

**Usage in Feature**:
- All trigonometric function results
- All hyperbolic function results
- exp, log, sqrt, expt results (for float inputs)
- Float coercion target

---

### E2: Integer (Fixnum / Bignum)

**Fixnum (i31ref)**:
Tagged small integers stored directly in reference.

```wat
;; No struct - uses i31ref directly
;; Range: -2^30 to 2^30 - 1 (-1,073,741,824 to 1,073,741,823)
```

**Bignum ($bignum)**:
Arbitrary-precision integers for values exceeding fixnum range.

```wat
(type $bignum (struct
  (field $sign i32)           ;; 0 = positive, 1 = negative
  (field $limbs (ref $i64-array))))

(type $i64-array (array (mut i64)))
```

**Attributes**:
| Field | Type | Description |
|-------|------|-------------|
| sign | i32 | Sign indicator (0=positive, 1=negative) |
| limbs | array[i64] | Little-endian limb array |

**Invariants**:
- Fixnum: Value fits in 30-bit signed range
- Bignum: Value exceeds fixnum range OR is result of bignum operation
- Bignums are normalized (no leading zero limbs except for zero itself)

**Usage in Feature**:
- Bit operation inputs and outputs
- parse-integer results
- Integer inputs to mathematical functions

---

### E3: Ratio ($ratio)

Rational number as numerator/denominator pair.

**WasmGC Representation**:
```wat
(type $ratio (struct
  (field $numerator anyref)    ;; integer (fixnum or bignum)
  (field $denominator anyref))) ;; positive integer
```

**Attributes**:
| Field | Type | Description |
|-------|------|-------------|
| numerator | integer | Numerator (may be negative) |
| denominator | integer | Denominator (always positive, non-zero) |

**Invariants**:
- GCD(|numerator|, denominator) = 1 (reduced form)
- Denominator > 0
- If numerator = 0, ratio is represented as fixnum 0

**Usage in Feature**:
- rational function output
- Input to abs, signum when type is ratio

---

### E4: Parse-Integer State Machine

Internal state for string parsing.

**States**:
```
enum ParseState {
  START,        // Initial state, expecting whitespace or sign or digit
  SIGN_SEEN,    // Sign character seen, expecting digit
  DIGITS,       // Accumulating digits
  TRAILING_WS,  // After valid number, allowing trailing whitespace
  DONE,         // Parsing complete
  ERROR         // Invalid input detected
}
```

**Context Structure** (compile-time only):
```lisp
(defstruct parse-context
  (state :start)              ; Current state
  (sign 1)                    ; Sign multiplier (+1 or -1)
  (accumulator 0)             ; Accumulated integer value
  (radix 10)                  ; Number base
  (start 0)                   ; Start index
  (end nil)                   ; End index (nil = string length)
  (position 0)                ; Current position
  (junk-allowed nil))         ; Allow trailing non-digits
```

---

### E5: Mathematical Constants

Predefined numeric constants.

| Constant | Type | Value | HyperSpec |
|----------|------|-------|-----------|
| pi | float | 3.141592653589793 | [pi](resources/HyperSpec/Body/v_pi.htm) |
| (exp 1) | float | 2.718281828459045 | Computed |

**Note**: PI should be defined as a constant in `lib/macros.lisp` for use in trigonometric calculations.

---

## Type Coercion Rules

### Numeric Promotion

When combining different numeric types:

```
fixnum + fixnum → fixnum (may overflow to bignum)
fixnum + bignum → bignum
fixnum + ratio → ratio
fixnum + float → float
ratio + ratio → ratio
ratio + float → float
float + float → float
```

### Coercion Functions

| Function | Input | Output |
|----------|-------|--------|
| float | integer | $float |
| float | ratio | $float |
| float | float | same $float |
| rational | float | $ratio or integer |
| rational | integer | same integer |
| rational | ratio | same ratio |

---

## State Transitions

### Parse-Integer State Machine

```
                  ┌──────────────────────────────────────────────┐
                  │                                              │
                  ▼                                              │
┌───────┐    ┌─────────┐    ┌───────────┐    ┌──────────┐    ┌──────┐
│ START │───▶│SIGN_SEEN│───▶│  DIGITS   │───▶│TRAILING_WS│───▶│ DONE │
└───────┘    └─────────┘    └───────────┘    └──────────┘    └──────┘
    │                             │                │              ▲
    │ digit                       │ end            │ end          │
    └─────────────────────────────┼────────────────┴──────────────┘
                                  │
                                  │ junk + !junk-allowed
                                  ▼
                              ┌───────┐
                              │ ERROR │
                              └───────┘
```

**Transition Table**:

| State | Input | Next State | Action |
|-------|-------|------------|--------|
| START | whitespace | START | Skip |
| START | +/- | SIGN_SEEN | Record sign |
| START | digit | DIGITS | Start accumulating |
| START | other | ERROR | Invalid start |
| SIGN_SEEN | digit | DIGITS | Start accumulating |
| SIGN_SEEN | other | ERROR | Sign without digits |
| DIGITS | digit | DIGITS | Accumulate: acc = acc * radix + digit |
| DIGITS | whitespace | TRAILING_WS | Stop accumulating |
| DIGITS | end | DONE | Return (acc * sign, position) |
| DIGITS | other | ERROR/DONE | Error if !junk-allowed, else DONE |
| TRAILING_WS | whitespace | TRAILING_WS | Skip |
| TRAILING_WS | end | DONE | Return (acc * sign, position) |
| TRAILING_WS | other | ERROR | Junk after number |

---

## Validation Rules

### Bit Operation Inputs

| Function | Input Validation |
|----------|------------------|
| ash | integer required; shift count is integer |
| logand | 0+ integers |
| logior | 0+ integers |
| logxor | 0+ integers |
| lognot | exactly 1 integer |
| logcount | exactly 1 integer |

### Mathematical Function Inputs

| Function | Input Validation | Domain |
|----------|------------------|--------|
| sin, cos, tan | 1 real | All real numbers |
| asin, acos | 1 real | [-1, 1] |
| atan | 1-2 reals | All real numbers |
| sinh, cosh, tanh | 1 real | All real numbers |
| asinh | 1 real | All real numbers |
| acosh | 1 real | [1, +∞) |
| atanh | 1 real | (-1, 1) |
| exp | 1 real | All real numbers |
| log | 1-2 positive reals | (0, +∞) |
| sqrt | 1 non-negative real | [0, +∞) |
| expt | 2 numbers | Various (see HyperSpec) |
| abs | 1 number | All numbers |
| signum | 1 number | All numbers |

### Parse-Integer Inputs

| Parameter | Validation |
|-----------|------------|
| string | Must be a string |
| :start | Non-negative integer ≤ string length |
| :end | Non-negative integer ≥ :start, ≤ string length |
| :radix | Integer in [2, 36] |
| :junk-allowed | Boolean |

---

## Relationships

```
┌──────────────────────────────────────────────────────────────────┐
│                      Numeric Functions                           │
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │ Trigonometric│    │   Bit Ops    │    │    Math      │       │
│  │  Functions   │    │  Functions   │    │  Functions   │       │
│  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘       │
│         │                   │                   │                │
│         ▼                   ▼                   ▼                │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                   Type Dispatch                          │   │
│  └──────────────────────────────────────────────────────────┘   │
│         │                   │                   │                │
│         ▼                   ▼                   ▼                │
│  ┌──────────┐        ┌──────────┐        ┌──────────┐           │
│  │  $float  │        │  fixnum  │        │  $ratio  │           │
│  │          │        │  $bignum │        │          │           │
│  └──────────┘        └──────────┘        └──────────┘           │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```
