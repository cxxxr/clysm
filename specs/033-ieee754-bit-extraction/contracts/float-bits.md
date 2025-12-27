# API Contract: Float Bit Extraction

**Feature**: 033-ieee754-bit-extraction
**Date**: 2025-12-27

## Module: `clysm/lib/float-bits`

Portable IEEE 754 float-to-bits conversion utilities.

---

## Core Functions

### `single-float-bits`

Convert single-float to IEEE 754 32-bit representation.

**Signature**:
```lisp
(single-float-bits value) → (unsigned-byte 32)
```

**Parameters**:
- `value` - A `single-float` value

**Returns**:
- 32-bit unsigned integer representing the IEEE 754 bit pattern

**Examples**:
```lisp
(single-float-bits 2.5f0)    ; => #x40200000
(single-float-bits -1.0f0)   ; => #xBF800000
(single-float-bits 0.0f0)    ; => #x00000000
(single-float-bits -0.0f0)   ; => #x80000000
```

**Special Values**:
| Input | Output |
|-------|--------|
| `+Inf` | `#x7F800000` |
| `-Inf` | `#xFF800000` |
| `NaN` | `#x7FC00000` (canonical) |

---

### `double-float-bits`

Convert double-float to IEEE 754 64-bit representation.

**Signature**:
```lisp
(double-float-bits value) → (unsigned-byte 64)
```

**Parameters**:
- `value` - A `double-float` value

**Returns**:
- 64-bit unsigned integer representing the IEEE 754 bit pattern

**Examples**:
```lisp
(double-float-bits 3.14d0)   ; => #x40091EB851EB851F
(double-float-bits -1.0d0)   ; => #xBFF0000000000000
(double-float-bits 0.0d0)    ; => #x0000000000000000
(double-float-bits -0.0d0)   ; => #x8000000000000000
```

**Special Values**:
| Input | Output |
|-------|--------|
| `+Inf` | `#x7FF0000000000000` |
| `-Inf` | `#xFFF0000000000000` |
| `NaN` | `#x7FF8000000000000` (canonical) |

---

## Detection Predicates

### `float-infinity-p`

**Signature**:
```lisp
(float-infinity-p x) → boolean
```

**Behavior**: Returns `T` if `x` is positive or negative infinity.

---

### `float-nan-p`

**Signature**:
```lisp
(float-nan-p x) → boolean
```

**Behavior**: Returns `T` if `x` is NaN (Not a Number).

---

### `float-negative-zero-p`

**Signature**:
```lisp
(float-negative-zero-p x) → boolean
```

**Behavior**: Returns `T` if `x` is negative zero (-0.0).

---

### `float-subnormal-p`

**Signature**:
```lisp
(float-subnormal-p x) → boolean
```

**Behavior**: Returns `T` if `x` is a subnormal (denormalized) number.

---

## Safe Arithmetic Functions

### `with-safe-float-ops`

**Signature**:
```lisp
(with-safe-float-ops &body body) → result
```

**Behavior**: Execute `body` with floating-point traps handled. Division by zero returns infinity, invalid operations return NaN.

**Example**:
```lisp
(with-safe-float-ops
  (/ 1.0d0 0.0d0))  ; => +Inf (no error signaled)
```

---

## Error Conditions

These functions do **not** signal errors. All inputs produce valid IEEE 754 bit patterns:

| Input Type | Result |
|------------|--------|
| Normal float | Correct bit pattern |
| Subnormal | Correct subnormal encoding |
| +/-Infinity | Canonical infinity pattern |
| NaN | Canonical NaN pattern |
| +/-Zero | Correct zero with sign |

---

## Compatibility

| CL Implementation | Status | Notes |
|-------------------|--------|-------|
| SBCL 2.4+ | ✅ Tested | Primary development platform |
| CCL 1.12+ | ✅ Targeted | Verified via `integer-decode-float` |
| ECL 24.5+ | ✅ Targeted | May need trap handling adjustments |
| CLISP | ⚠️ Limited | NaN handling may differ |
