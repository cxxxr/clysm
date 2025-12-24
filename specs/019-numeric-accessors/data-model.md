# Data Model: Numeric Accessors and Float Special Values

**Feature**: 019-numeric-accessors
**Date**: 2025-12-24

## Entities

This feature extends existing entities from 010-numeric-tower.

### Ratio (existing - type index 15)

WasmGC struct representing exact rational numbers.

| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| numerator | anyref | No | Numerator (fixnum or bignum), carries sign |
| denominator | anyref | No | Denominator (positive fixnum or bignum, never zero) |

**Invariants**:
- GCD(|numerator|, denominator) = 1 (always reduced to lowest terms)
- denominator > 0 (sign always on numerator)
- If ratio simplifies to integer (denominator = 1), return integer instead

**WasmGC Definition** (from gc-types.lisp):
```wat
(type $ratio (struct
  (field $numerator anyref)
  (field $denominator anyref)))
```

### Float (existing - type index 16)

WasmGC struct wrapping IEEE 754 double-precision value.

| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| value | f64 | No | IEEE 754 binary64 value |

**Special Values**:
- +Infinity: `0x7FF0000000000000` bit pattern
- -Infinity: `0xFFF0000000000000` bit pattern
- NaN: Exponent = 0x7FF with non-zero mantissa

**WasmGC Definition** (from gc-types.lisp):
```wat
(type $float (struct
  (field $value f64)))
```

### Integer Types (existing)

| Type | Index | Description |
|------|-------|-------------|
| Fixnum | N/A | i31ref (unboxed, immediate) |
| Bignum | 14 | Arbitrary-precision integer |

## Relationships

```
Numeric Tower Hierarchy (for accessors):

  rational ─────────┬─ integer ──┬─ fixnum (i31ref)
                    │            └─ bignum ($bignum)
                    │
                    └─ ratio ($ratio)

  numerator/denominator applicable to: rational (integer or ratio)

  Float special values: +Inf, -Inf, NaN (not applicable to numerator/denominator)
```

## State Transitions

No state transitions - these are accessor functions (read-only).

## Validation Rules

### numerator Function

| Input Type | Validation | Result |
|------------|------------|--------|
| fixnum (i31ref) | Always valid | Return the fixnum |
| bignum | Always valid | Return the bignum |
| ratio | Always valid | Return numerator field |
| float | Type error | Signal TYPE-ERROR |
| complex | Type error | Signal TYPE-ERROR |
| other | Type error | Signal TYPE-ERROR |

### denominator Function

| Input Type | Validation | Result |
|------------|------------|--------|
| fixnum (i31ref) | Always valid | Return 1 (fixnum) |
| bignum | Always valid | Return 1 (fixnum) |
| ratio | Always valid | Return denominator field |
| float | Type error | Signal TYPE-ERROR |
| complex | Type error | Signal TYPE-ERROR |
| other | Type error | Signal TYPE-ERROR |

### Float Comparison Validation

| Operand Types | Validation | Behavior |
|--------------|------------|----------|
| float, float (both finite) | Always valid | Normal IEEE 754 comparison |
| float with NaN | Always valid | All comparisons return NIL |
| +Infinity, +Infinity | Always valid | Equal (T for =) |
| +Infinity, finite | Always valid | +Infinity is greater |
| -Infinity, finite | Always valid | -Infinity is less |
| -0.0, +0.0 | Always valid | Equal (T for =) |

## Data Volume Assumptions

- Ratio fields reference existing numeric objects (no duplication)
- Float struct contains single f64 (8 bytes + GC overhead)
- No storage requirements - pure computation functions
