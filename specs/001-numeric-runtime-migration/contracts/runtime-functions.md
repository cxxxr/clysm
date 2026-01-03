# Runtime Function Contracts: Numeric Runtime Migration

**Feature**: 001-numeric-runtime-migration
**Date**: 2026-01-04

## Function Contracts

### parse-integer-rt

**Purpose**: Parse an integer from a string with optional radix and bounds.

**Input**:
- `string`: String to parse
- `:start`: Start index (default 0)
- `:end`: End index (default string length)
- `:radix`: Base 2-36 (default 10)
- `:junk-allowed`: Boolean (default NIL)

**Output**: Two values
- Primary: Parsed integer or NIL (if junk-allowed and no valid integer)
- Secondary: Index where parsing stopped

**Errors**:
- `parse-error` if no valid integer and `:junk-allowed nil`
- `type-error` if radix not in 2-36

**Examples**:
```lisp
(parse-integer "123")           → 123, 3
(parse-integer "  -42" :start 2) → -42, 5
(parse-integer "FF" :radix 16)  → 255, 2
(parse-integer "abc" :junk-allowed t) → NIL, 0
```

---

### write-to-string-rt

**Purpose**: Convert a number to its string representation.

**Input**:
- `object`: Number to convert
- `:base`: Output radix 2-36 (default 10)

**Output**: String representation

**Errors**:
- `type-error` if base not in 2-36

**Examples**:
```lisp
(write-to-string 255)            → "255"
(write-to-string 255 :base 16)   → "FF"
(write-to-string -42)            → "-42"
(write-to-string 1/2)            → "1/2"
```

---

### rationalize-rt

**Purpose**: Convert a real number to a rational approximation.

**Input**:
- `number`: Real number (integer, ratio, or float)

**Output**: Rational number (integer or ratio)

**Errors**:
- `arithmetic-error` for infinity or NaN

**Invariants**:
- For integers: returns input unchanged
- For ratios: returns input unchanged
- For floats: returns "simplest" approximation

**Examples**:
```lisp
(rationalize 5)     → 5
(rationalize 1/3)   → 1/3
(rationalize 0.5)   → 1/2
(rationalize 0.333) → 1/3 (approximately)
```

---

### signum-rt

**Purpose**: Return the sign of a number in the same numeric type.

**Input**:
- `number`: Any numeric type

**Output**: Sign indicator in same type as input

**Type Congruence Table**:
| Input Type | Zero | Positive | Negative |
|------------|------|----------|----------|
| integer    | 0    | 1        | -1       |
| float      | 0.0  | 1.0      | -1.0     |
| ratio      | N/A  | 1        | -1       |
| complex    | 0    | z/|z|    | z/|z|    |

**Examples**:
```lisp
(signum 42)        → 1
(signum -3.14)     → -1.0
(signum 0)         → 0
(signum #C(3 4))   → #C(0.6 0.8)
```

---

### phase-rt

**Purpose**: Return the angle (phase) of a number in the complex plane.

**Input**:
- `number`: Any numeric type

**Output**: Float representing angle in radians

**Range**: `(-π, π]` (standard atan2 range)

**Special Cases**:
| Input | Output |
|-------|--------|
| Positive real | 0.0 |
| Negative real | π |
| Zero | 0.0 |
| Pure imaginary (+i) | π/2 |
| Pure imaginary (-i) | -π/2 |

**Examples**:
```lisp
(phase 1)          → 0.0
(phase -1)         → 3.141592653589793
(phase #C(0 1))    → 1.5707963267948966
(phase #C(1 1))    → 0.7853981633974483
```

## Registration Contract

All functions must be registered in `*runtime-function-table*` before compilation:

```lisp
(register-runtime-function 'parse-integer :$parse-integer-rt nil)
(register-runtime-function 'write-to-string :$write-to-string-rt nil)
(register-runtime-function 'rationalize :$rationalize-rt 1)
(register-runtime-function 'signum :$signum-rt 1)
(register-runtime-function 'phase :$phase-rt 1)
```
