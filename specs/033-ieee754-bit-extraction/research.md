# Research: IEEE 754 Bit Extraction

**Feature**: 033-ieee754-bit-extraction
**Date**: 2025-12-27

## Research Questions

1. How to portably extract IEEE 754 bit patterns from floats in Common Lisp?
2. How to handle special values (infinity, NaN, subnormal, -0.0)?
3. How to replace `sb-int:with-float-traps-masked` portably?

---

## Q1: Portable IEEE 754 Bit Extraction

### Decision: Use `integer-decode-float` with manual bit assembly

### Rationale

ANSI Common Lisp provides `integer-decode-float` which returns the significand, exponent, and sign of a float in a portable manner. By understanding IEEE 754 encoding, we can reconstruct the exact bit pattern.

### Implementation Strategy

**For normal numbers:**
```lisp
(defun float-to-bits-64 (value)
  "Convert double-float to 64-bit IEEE 754 representation."
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float value)
    ;; significand: integer (53 bits for double)
    ;; exponent: integer (biased by 52 for double)
    ;; sign: -1 or 1
    (let* ((sign-bit (if (= sign -1) 1 0))
           ;; IEEE 754 double: bias = 1023, mantissa = 52 bits
           (ieee-exp (+ exponent 52 1023))
           ;; Remove implicit leading 1 (except subnormals)
           (mantissa (logand significand #xFFFFFFFFFFFFF)))
      (logior (ash sign-bit 63)
              (ash ieee-exp 52)
              mantissa))))
```

**For special values:**
- `float-infinity-p`: Check if `(= value (* value 2))` and value ≠ 0
- `float-nan-p`: Check if `(/= value value)` (NaN is never equal to itself)
- Zero: Check exponent from `integer-decode-float` returns minimum

### Alternatives Considered

| Alternative | Rejected Because |
|-------------|------------------|
| FFI to C library | Adds external dependency, breaks pure CL portability |
| CFFI with platform-specific code | Complex, maintenance burden |
| Write bytes via streams | Indirect, potential endianness issues |
| Use ieee-floats library | External dependency; simpler to implement ~50 lines |

---

## Q2: Special Value Handling

### Decision: Explicit detection and canonical encoding

### Rationale

IEEE 754 special values have well-defined bit patterns that must be produced regardless of how the host CL represents them internally.

### Canonical Bit Patterns

| Value | f32 (32-bit) | f64 (64-bit) |
|-------|--------------|--------------|
| +Infinity | `0x7F800000` | `0x7FF0000000000000` |
| -Infinity | `0xFF800000` | `0xFFF0000000000000` |
| +0.0 | `0x00000000` | `0x0000000000000000` |
| -0.0 | `0x80000000` | `0x8000000000000000` |
| Canonical NaN | `0x7FC00000` | `0x7FF8000000000000` |

### Detection Functions

```lisp
(defun float-infinity-p (x)
  "Return T if X is positive or negative infinity."
  (and (floatp x)
       (not (= x x 0.0))  ; Not zero
       (= x (* x 2))))     ; Infinity property

(defun float-positive-infinity-p (x)
  (and (float-infinity-p x) (plusp x)))

(defun float-negative-infinity-p (x)
  (and (float-infinity-p x) (minusp x)))

(defun float-nan-p (x)
  "Return T if X is NaN (Not a Number)."
  (and (floatp x) (/= x x)))

(defun float-negative-zero-p (x)
  "Return T if X is negative zero."
  (and (zerop x)
       (minusp (atan x -1.0))))  ; atan(-0, -1) = -π
```

### Subnormal Numbers

Subnormals (denormals) have exponent = 0 and implicit leading bit = 0:
- Detected when `integer-decode-float` returns exponent = -1074 (double) or -149 (single)
- Mantissa is stored directly without implicit leading 1

---

## Q3: Portable Trap Masking

### Decision: Use `handler-case` with float-specific conditions

### Rationale

SBCL's `with-float-traps-masked` is non-portable. ANSI CL defines `floating-point-overflow`, `floating-point-underflow`, `floating-point-invalid-operation`, and `division-by-zero` as conditions that can be handled.

### Implementation

```lisp
(defmacro with-float-traps-handled (&body body)
  "Execute BODY, converting float traps to special values.
   Returns infinity for overflow/division-by-zero, NaN for invalid operations."
  `(handler-case
       (progn ,@body)
     (division-by-zero ()
       ;; Return positive infinity (caller determines sign)
       sb-ext:double-float-positive-infinity)
     (floating-point-overflow ()
       sb-ext:double-float-positive-infinity)
     (floating-point-invalid-operation ()
       ;; Return canonical NaN
       (/ 0.0d0 0.0d0))))
```

### Cross-Implementation Compatibility

| CL Implementation | Infinity Constant | NaN Generation |
|-------------------|-------------------|----------------|
| SBCL | `sb-ext:double-float-positive-infinity` | `(/ 0.0d0 0.0d0)` |
| CCL | `ccl:double-float-positive-infinity` | `(/ 0.0d0 0.0d0)` |
| ECL | Computed: `(/ 1.0d0 0.0d0)` | `(/ 0.0d0 0.0d0)` |
| CLISP | Computed: `(/ 1.0d0 0.0d0)` | Not directly supported |

**Portable approach**: Precompute infinity/NaN at load time:
```lisp
(defvar *positive-infinity-f64*
  (handler-case (/ 1.0d0 0.0d0)
    (division-by-zero ()
      ;; Fallback: construct from bits if handler catches
      most-positive-double-float)))
```

---

## Summary

| Question | Decision | Impact |
|----------|----------|--------|
| Bit extraction | `integer-decode-float` + manual assembly | ~60 lines new code |
| Special values | Explicit detection + canonical encoding | Guarantees bit-identical output |
| Trap masking | `handler-case` + portable conditions | Works on SBCL, CCL, ECL |

## References

- IEEE 754-2019 Standard for Floating-Point Arithmetic
- CLHS: `integer-decode-float`, `decode-float`, `float-radix`
- CLHS: Condition types for floating-point exceptions
