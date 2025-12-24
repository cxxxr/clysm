# Data Model: Numeric Tower

**Feature**: 010-numeric-tower | **Date**: 2025-12-24

## Entity Overview

```
┌─────────────────────────────────────────────────────────────┐
│                         NUMBER                               │
│                        (abstract)                            │
├──────────────────────────┬──────────────────────────────────┤
│         COMPLEX          │              REAL                │
│     $complex (type 17)   │           (abstract)             │
│   ┌────────┬────────┐    ├─────────────┬────────────────────┤
│   │ real   │ imag   │    │    FLOAT    │     RATIONAL       │
│   │anyref  │anyref  │    │   $float    │    (abstract)      │
│   └────────┴────────┘    │  (type 16)  ├──────────┬─────────┤
│                          │  ┌───────┐  │  RATIO   │ INTEGER │
│                          │  │ f64   │  │  $ratio  │(abstract│
│                          │  └───────┘  │ (type 15)│         │
│                          │             │┌────┬───┐├────┬────┤
│                          │             ││num │den││BIG │FIX │
│                          │             │└────┴───┘│NUM │NUM │
│                          │             │          │$big│i31 │
│                          │             │          │(14)│ref │
└──────────────────────────┴─────────────┴──────────┴────┴────┘
```

## Entity Definitions

### E1: Fixnum (existing)

**Description**: Machine-word-sized signed integer, represented as i31ref

**WasmGC Representation**: `i31ref` (unboxed tagged integer)

**Range**: -2^30 to 2^30 - 1 (−1,073,741,824 to 1,073,741,823)

**Attributes**:
| Field | Type | Description |
|-------|------|-------------|
| value | i31ref | 31-bit signed integer (tag bit used by GC) |

**Operations**: Unbox with `i31.get_s`, box with `ref.i31`

---

### E2: Bignum (NEW)

**Description**: Arbitrary-precision signed integer

**WasmGC Type Index**: 14 (`$bignum`)

**Representation**:
```wat
(type $limb_array (array (mut i32)))

(type $bignum (struct
  (field $sign i32)                    ;; 0 = non-negative, 1 = negative
  (field $limbs (ref $limb_array))))   ;; Array of 32-bit limbs, little-endian
```

**Attributes**:
| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| sign | i32 | no | Sign flag: 0 for positive/zero, 1 for negative |
| limbs | ref $limb_array | no | Array of 32-bit unsigned limbs (little-endian) |

**Invariants**:
- Zero is represented as sign=0, limbs=[0]
- No leading zero limbs (except for zero itself)
- Limbs store unsigned 32-bit values (0 to 2^32-1)

**Lifecycle**:
- Created when: Fixnum operation overflows, ratio division results in integer > fixnum range, literal parsed
- Simplified to fixnum when: Result fits in i31 range after operation

---

### E3: Ratio (NEW)

**Description**: Exact rational number as numerator/denominator pair

**WasmGC Type Index**: 15 (`$ratio`)

**Representation**:
```wat
(type $ratio (struct
  (field $numerator anyref)      ;; fixnum (i31ref) or bignum
  (field $denominator anyref)))  ;; fixnum (i31ref) or bignum, always positive
```

**Attributes**:
| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| numerator | anyref | no | Integer (fixnum or bignum), carries sign |
| denominator | anyref | no | Positive integer (fixnum or bignum), never zero |

**Invariants**:
- Denominator is always positive and non-zero
- GCD(|numerator|, denominator) = 1 (always reduced)
- If denominator = 1, result is integer (never ratio)
- Sign is carried by numerator only

**Lifecycle**:
- Created when: Integer division doesn't divide evenly
- Simplified to integer when: Denominator = 1 after reduction

---

### E4: Float (NEW)

**Description**: IEEE 754 double-precision floating-point number

**WasmGC Type Index**: 16 (`$float`)

**Representation**:
```wat
(type $float (struct
  (field $value f64)))
```

**Attributes**:
| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| value | f64 | no | IEEE 754 binary64 value |

**Special Values**:
| Value | Representation |
|-------|----------------|
| +∞ | `+inf.0` |
| -∞ | `-inf.0` |
| NaN | `nan` (quiet NaN) |
| -0.0 | Negative zero (distinct from +0.0) |

**Notes**:
- Single-float and double-float both use f64 internally
- Boxing required for anyref compatibility

---

### E5: Complex (NEW)

**Description**: Complex number with real and imaginary parts

**WasmGC Type Index**: 17 (`$complex`)

**Representation**:
```wat
(type $complex (struct
  (field $real anyref)   ;; fixnum, bignum, ratio, or float
  (field $imag anyref))) ;; fixnum, bignum, ratio, or float
```

**Attributes**:
| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| real | anyref | no | Real part (any real number type) |
| imag | anyref | no | Imaginary part (any real number type) |

**Invariants**:
- If imaginary part is exact zero AND real part is rational, simplified to real part
- Complex with float parts: `#C(5.0 0.0)` stays complex (float zero ≠ exact zero)
- Component types follow float contagion (if one is float, both become float)

**Lifecycle**:
- Created when: Arithmetic produces imaginary component, or literal parsed
- Simplified to real when: Imaginary part is exact rational zero

---

## Type Relationships

### Inheritance Hierarchy (for type predicates)

```
numberp
├── complexp    → $complex
└── realp
    ├── floatp      → $float
    └── rationalp
        ├── ratiop      → $ratio
        └── integerp
            ├── bignump     → $bignum
            └── fixnump     → i31ref
```

### Type Coercion Flow

```
                    ┌─────────────────┐
                    │    COMPLEX      │ ← Most general
                    └────────┬────────┘
                             │ complex contagion
                    ┌────────┴────────┐
                    │      FLOAT      │
                    └────────┬────────┘
                             │ float contagion
                    ┌────────┴────────┐
                    │      RATIO      │
                    └────────┬────────┘
                             │ integer division
                    ┌────────┴────────┐
                    │     BIGNUM      │
                    └────────┬────────┘
                             │ overflow promotion
                    ┌────────┴────────┐
                    │     FIXNUM      │ ← Most specific
                    └─────────────────┘
```

## Type Index Summary

| Index | Type Name | WasmGC Kind | Description |
|-------|-----------|-------------|-------------|
| 0-13 | (existing) | various | nil, unbound, cons, symbol, string, closure, etc. |
| 14 | $bignum | struct | Arbitrary-precision integer |
| 15 | $ratio | struct | Exact rational number |
| 16 | $float | struct | Boxed f64 floating-point |
| 17 | $complex | struct | Complex number |
| 18 | $limb_array | array | i32 array for bignum limbs |

## Validation Rules

### V1: Ratio Canonicalization
```
WHEN ratio created
THEN gcd(abs(numerator), denominator) = 1
AND denominator > 0
AND IF denominator = 1 THEN return integer instead
```

### V2: Complex Canonicalization
```
WHEN complex created with rational components
AND imaginary = 0 (exact)
THEN return real part instead of complex
```

### V3: Fixnum Overflow
```
WHEN fixnum arithmetic result r
AND (r < -2^30 OR r >= 2^30)
THEN promote operands to bignum and retry
```

### V4: Bignum Normalization
```
WHEN bignum created or modified
THEN no leading zero limbs (except for value 0)
AND IF fits in fixnum range THEN demote to fixnum
```

## State Transitions

```
┌──────────────────────────────────────────────────────────────────┐
│                     NUMERIC VALUE STATES                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Fixnum ──overflow──► Bignum ──to-ratio──► Ratio                 │
│    │                    │                    │                    │
│    │                    └────to-float───►    │                    │
│    │                                         │                    │
│    └──────to-float───────► Float ◄──to-float─┘                   │
│                              │                                    │
│                              │                                    │
│  Any Real ───add-imaginary───┼──► Complex                        │
│                              │        │                           │
│                              │        │ (imag=0, rational)        │
│                              ◄────────┘                           │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```
