# Data Model: IEEE 754 Bit Extraction

**Feature**: 033-ieee754-bit-extraction
**Date**: 2025-12-27

## Overview

This feature deals with compile-time data transformations, not runtime data storage. The "data model" describes the IEEE 754 encoding structure and the functions that transform floats to bit patterns.

## IEEE 754 Float Structure

### Single Precision (32-bit, f32)

```
┌─────┬──────────┬───────────────────────┐
│ S   │ Exponent │ Mantissa              │
│ 1b  │ 8 bits   │ 23 bits               │
└─────┴──────────┴───────────────────────┘
Bit: 31   30..23        22..0
```

| Field | Bits | Range | Notes |
|-------|------|-------|-------|
| Sign (S) | 1 | 0-1 | 0 = positive, 1 = negative |
| Exponent | 8 | 0-255 | Bias = 127; 0 = subnormal/zero, 255 = infinity/NaN |
| Mantissa | 23 | 0-8388607 | Implicit leading 1 for normals |

### Double Precision (64-bit, f64)

```
┌─────┬──────────────┬─────────────────────────────────────────────────┐
│ S   │ Exponent     │ Mantissa                                        │
│ 1b  │ 11 bits      │ 52 bits                                         │
└─────┴──────────────┴─────────────────────────────────────────────────┘
Bit: 63   62..52             51..0
```

| Field | Bits | Range | Notes |
|-------|------|-------|-------|
| Sign (S) | 1 | 0-1 | 0 = positive, 1 = negative |
| Exponent | 11 | 0-2047 | Bias = 1023; 0 = subnormal/zero, 2047 = infinity/NaN |
| Mantissa | 52 | 0-4503599627370495 | Implicit leading 1 for normals |

## Special Value Encodings

### Canonical Bit Patterns

| Value | Type | Hex Pattern | Components |
|-------|------|-------------|------------|
| +0.0 | f32 | `0x00000000` | S=0, E=0, M=0 |
| -0.0 | f32 | `0x80000000` | S=1, E=0, M=0 |
| +∞ | f32 | `0x7F800000` | S=0, E=255, M=0 |
| -∞ | f32 | `0xFF800000` | S=1, E=255, M=0 |
| NaN (canonical) | f32 | `0x7FC00000` | S=0, E=255, M≠0 (quiet) |
| +0.0 | f64 | `0x0000000000000000` | S=0, E=0, M=0 |
| -0.0 | f64 | `0x8000000000000000` | S=1, E=0, M=0 |
| +∞ | f64 | `0x7FF0000000000000` | S=0, E=2047, M=0 |
| -∞ | f64 | `0xFFF0000000000000` | S=1, E=2047, M=0 |
| NaN (canonical) | f64 | `0x7FF8000000000000` | S=0, E=2047, M≠0 (quiet) |

### Subnormal Numbers

Subnormal (denormalized) numbers have:
- Exponent = 0
- Implicit leading bit = 0 (not 1)
- Value = (-1)^S × 0.M × 2^(1-bias)

Smallest positive subnormal:
- f32: `0x00000001` = 2^-149 ≈ 1.4e-45
- f64: `0x0000000000000001` = 2^-1074 ≈ 4.9e-324

## Function Signatures

### Core Functions

```lisp
;; Main API - used by compiler
(single-float-bits value) → (unsigned-byte 32)
(double-float-bits value) → (unsigned-byte 64)

;; Detection predicates
(float-infinity-p x) → boolean
(float-positive-infinity-p x) → boolean
(float-negative-infinity-p x) → boolean
(float-nan-p x) → boolean
(float-negative-zero-p x) → boolean
(float-subnormal-p x) → boolean

;; Safe arithmetic (replaces with-float-traps-masked)
(safe-float-divide dividend divisor) → float
(safe-float-multiply a b) → float
(safe-float-add a b) → float
```

### State Transitions

No runtime state. All functions are pure transformations:

```
Input: Common Lisp float value
  ↓
Detection: Classify as normal/subnormal/infinity/NaN/zero
  ↓
Encoding: Map to IEEE 754 bit pattern
  ↓
Output: Unsigned integer (32 or 64 bits)
```

## Validation Rules

1. **Bit Count**: f32 must produce exactly 32 bits, f64 exactly 64 bits
2. **Special Value Detection**: Must correctly identify all 5 special categories
3. **Sign Preservation**: Must distinguish +0.0 from -0.0
4. **NaN Canonicalization**: All NaN inputs produce Wasm canonical NaN output
5. **Endianness**: Output bytes in little-endian order (per Wasm spec)

## Dependencies

| Entity | Depends On | Notes |
|--------|------------|-------|
| `emit-f32` | `single-float-bits` | Compiler uses for f32 constants |
| `emit-f64` | `double-float-bits` | Compiler uses for f64 constants |
| `fold-arithmetic` | `safe-float-*` | AST uses for constant folding |
| `compile-and-run-numeric` | `safe-float-divide` | Test harness uses for verification |
