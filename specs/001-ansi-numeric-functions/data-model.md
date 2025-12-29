# Data Model: ANSI Common Lisp Numeric Functions

**Branch**: `001-ansi-numeric-functions` | **Date**: 2025-12-29

## Type Definitions

### Existing Numeric Types (Reference)

| Type Index | Name | WasmGC Definition | Purpose |
|------------|------|-------------------|---------|
| 4 | `$float` | `(struct (field $value f64))` | IEEE 754 double |
| 5 | `$ratio` | `(struct (field $num i32) (field $den i32))` | Rational number |
| N/A | fixnum | `i31ref` | 31-bit signed integer |

### New Type: Complex Number

**Type Index**: 9 (next available after existing types)

```wat
(type $complex (struct
  (field $real (ref $float))    ;; Real part as boxed float
  (field $imag (ref $float))))  ;; Imaginary part as boxed float
```

**Global Index for Pure Imaginary Unit**:
```wat
(global $i (ref $complex)
  (struct.new $complex
    (struct.new $float (f64.const 0.0))
    (struct.new $float (f64.const 1.0))))
```

---

## Type Hierarchy

```
numberp
├── realp
│   ├── rationalp
│   │   ├── integerp (i31ref fixnum)
│   │   └── ratiop ($ratio)
│   └── floatp ($float)
└── complexp ($complex)
```

---

## Function Signatures

### Trigonometric Functions

| Function | Signature | Input Domain | Output Type |
|----------|-----------|--------------|-------------|
| `sin` | `(number) -> float` | All reals | $float |
| `cos` | `(number) -> float` | All reals | $float |
| `tan` | `(number) -> float` | All reals | $float |
| `asin` | `(number) -> number` | \|x\| <= 1: real; else complex | $float or $complex |
| `acos` | `(number) -> number` | \|x\| <= 1: real; else complex | $float or $complex |
| `atan` | `(number &optional number) -> float` | All reals | $float |

### Hyperbolic Functions

| Function | Signature | Input Domain | Output Type |
|----------|-----------|--------------|-------------|
| `sinh` | `(number) -> float` | All reals | $float |
| `cosh` | `(number) -> float` | All reals | $float |
| `tanh` | `(number) -> float` | All reals | $float |
| `asinh` | `(number) -> float` | All reals | $float |
| `acosh` | `(number) -> number` | x >= 1: real; else complex | $float or $complex |
| `atanh` | `(number) -> number` | \|x\| < 1: real; else complex | $float or $complex |

### Bitwise Operations

| Function | Signature | Input Domain | Output Type |
|----------|-----------|--------------|-------------|
| `ash` | `(integer integer) -> integer` | Any integers | i31ref or bignum |
| `logand` | `(&rest integer) -> integer` | Zero or more integers | i31ref or bignum |
| `logior` | `(&rest integer) -> integer` | Zero or more integers | i31ref or bignum |
| `logxor` | `(&rest integer) -> integer` | Zero or more integers | i31ref or bignum |
| `lognot` | `(integer) -> integer` | Any integer | i31ref or bignum |
| `logcount` | `(integer) -> integer` | Any integer | i31ref (always fits) |

### Complex Number Operations

| Function | Signature | Input Domain | Output Type |
|----------|-----------|--------------|-------------|
| `complex` | `(real &optional real) -> complex` | Two reals | $complex |
| `realpart` | `(number) -> real` | Any number | $float or i31ref |
| `imagpart` | `(number) -> real` | Any number | $float or i31ref (0 for reals) |
| `conjugate` | `(number) -> number` | Any number | Same type as input |
| `phase` | `(number) -> float` | Any number | $float |

### Mathematical Functions

| Function | Signature | Input Domain | Output Type |
|----------|-----------|--------------|-------------|
| `exp` | `(number) -> number` | Any number | $float or $complex |
| `log` | `(number &optional number) -> number` | Positive reals; complex for negative | $float or $complex |
| `sqrt` | `(number) -> number` | Non-negative reals; complex for negative | $float or $complex |
| `expt` | `(number number) -> number` | Any numbers | Depends on args |
| `abs` | `(number) -> number` | Any number | $float (magnitude for complex) |
| `signum` | `(number) -> number` | Any number | Same type or $complex |
| `max` | `(&rest real) -> real` | One or more reals | Same type as largest |
| `min` | `(&rest real) -> real` | One or more reals | Same type as smallest |
| `gcd` | `(&rest integer) -> integer` | Zero or more integers | i31ref (always non-negative) |
| `lcm` | `(&rest integer) -> integer` | Zero or more integers | i31ref or bignum |

---

## Type Coercion Rules

### Numeric Widening (for operations)

```
i31ref (fixnum) -> $float (for transcendental functions)
i31ref (fixnum) -> $ratio (when divided by non-factor)
$ratio -> $float (for transcendental functions)
real -> $complex (when result requires complex)
```

### Coercion in Codegen

```lisp
;; Pattern: Extract f64 from any real number
(local.tee $temp)
(ref.test i31)
(if (result f64)
  (local.get $temp) (ref.cast i31) i31.get_s f64.convert_i32_s
else
  (local.get $temp) (ref.cast (ref $float)) (struct.get $float 0)
end)
```

---

## Identity Values

| Function | Identity | Rationale |
|----------|----------|-----------|
| `logand` | -1 | All bits set; AND with any value returns that value |
| `logior` | 0 | No bits set; OR with any value returns that value |
| `logxor` | 0 | XOR with 0 is identity |
| `gcd` | 0 | gcd(0, n) = n |
| `lcm` | 1 | lcm(1, n) = n |
| `max` | N/A | Requires at least 1 argument |
| `min` | N/A | Requires at least 1 argument |

---

## State Transitions

Not applicable - numeric functions are pure (no state mutation).

---

## Validation Rules

### Argument Count

| Function | Min Args | Max Args |
|----------|----------|----------|
| `sin`, `cos`, `tan` | 1 | 1 |
| `atan`, `log` | 1 | 2 |
| `logand`, `logior`, `logxor`, `gcd`, `lcm` | 0 | unlimited |
| `ash`, `complex`, `expt` | 2 | 2 |
| `max`, `min` | 1 | unlimited |

### Type Constraints

| Function | Constraint |
|----------|------------|
| Bitwise ops | Arguments must be integers |
| `max`, `min` | Arguments must be real numbers |
| `gcd`, `lcm` | Arguments must be non-negative integers |
