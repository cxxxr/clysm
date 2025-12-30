# Data Model: Numeric Conversion and Formatting (Phase 14C)

**Date**: 2025-12-30
**Branch**: `001-numeric-format`

## Entities

This feature operates on existing Clysm numeric types. No new entities are introduced.

### Existing Entities Used

#### Ratio (Type Index 15)

**Definition**: `src/clysm/compiler/codegen/gc-types.lisp:350-365`

```wat
(type $ratio (struct
  (field $numerator anyref)    ;; fixnum or bignum, carries sign
  (field $denominator anyref)  ;; positive fixnum or bignum, never zero
))
```

| Field | Type | Description |
|-------|------|-------------|
| numerator | anyref (i31 or $bignum) | Signed integer, carries sign of ratio |
| denominator | anyref (i31 or $bignum) | Positive integer, always > 0 |

**Invariants**:
- GCD(|numerator|, denominator) = 1 (always reduced)
- denominator > 0

**Creation**: `struct.new 15` with numerator, denominator values

#### Float (Type Index 16)

**Definition**: `src/clysm/compiler/codegen/gc-types.lisp`

```wat
(type $float (struct
  (field $value f64)
))
```

| Field | Type | Description |
|-------|------|-------------|
| value | f64 | IEEE 754 double-precision floating point |

#### String (Type Index 2)

**Definition**: `src/clysm/compiler/codegen/gc-types.lisp`

```wat
(type $string (struct
  (field $bytes (ref $byte_array))
))
```

| Field | Type | Description |
|-------|------|-------------|
| bytes | ref $byte_array | UTF-8 encoded byte sequence |

**Used by**: `write-to-string` output

### Type Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `+type-ratio+` | 15 | Ratio type index |
| `+type-float+` | 16 | Float type index |
| `+type-string+` | 2 | String type index |
| `+type-bignum+` | 14 | Bignum type index |

---

## Data Flow

### `rationalize` Data Flow

```
Input: anyref (float, fixnum, ratio)
  │
  ├─► fixnum (i31ref) ──► return unchanged
  ├─► ratio (type 15) ──► return unchanged
  └─► float (type 16) ──► continued fraction algorithm
                              │
                              ├─► Integer result? ──► return fixnum
                              └─► Fractional? ──► struct.new $ratio
```

### `write-to-string` Data Flow

```
Input: anyref (number) + :base keyword
  │
  ├─► fixnum (i31ref) ──► base conversion ──► struct.new $string
  ├─► bignum (type 14) ──► bignum base conversion ──► struct.new $string
  ├─► ratio (type 15) ──► convert num + "/" + convert den ──► struct.new $string
  └─► float (type 16) ──► decimal representation ──► struct.new $string
```

---

## Validation Rules

### `rationalize` Input Validation

| Condition | Action |
|-----------|--------|
| Input is NaN | Signal `arithmetic-error` |
| Input is infinity | Signal `arithmetic-error` |
| Input is valid number | Proceed with conversion |

### `write-to-string` Input Validation

| Condition | Action |
|-----------|--------|
| `:base` < 2 | Signal `type-error` |
| `:base` > 36 | Signal `type-error` |
| `:base` not integer | Signal `type-error` |
| Input not a number | Signal `type-error` |

---

## State Transitions

N/A - Both functions are pure transformations with no mutable state.
