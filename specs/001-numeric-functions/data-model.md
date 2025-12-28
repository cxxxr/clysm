# Data Model: ANSI Numeric Functions

**Date**: 2025-12-28
**Branch**: `001-numeric-functions`

## Numeric Type Hierarchy

### Existing Types (from gc-types.lisp)

```
                    number
                      │
          ┌───────────┼───────────┐
          │           │           │
        real       complex     (future)
          │           │
    ┌─────┴─────┐     │
    │           │     │
 rational    float    │
    │           │     │
 ┌──┴──┐    $float   $complex
 │     │    (16)      (17)
fixnum ratio
(i31)  (15)
  │
bignum
 (14)
```

### Type Indices (WasmGC)

| Index | Type | Wasm Representation | Operations Supported |
|-------|------|---------------------|---------------------|
| - | fixnum | `i31ref` | All numeric, bitwise |
| 14 | $bignum | `struct { $limb-array, sign }` | All numeric (limited bitwise) |
| 15 | $ratio | `struct { numerator, denominator }` | Rational arithmetic |
| 16 | $float | `struct { f64 }` | Float arithmetic, transcendental |
| 17 | $complex | `struct { real, imag }` | Complex arithmetic |

## Function Categories

### Category 1: Basic Functions

| Function | Input Types | Output Type | WasmGC Implementation |
|----------|-------------|-------------|----------------------|
| `abs` | real | same type | i31: conditional negate; f64: f64.abs |
| `signum` | real | -1, 0, or 1 | i31: comparison chain |
| `max` | real+ | real | pairwise comparison |
| `min` | real+ | real | pairwise comparison |
| `gcd` | integer* | integer | Euclidean algorithm |
| `lcm` | integer* | integer | Via gcd: lcm(a,b) = \|a*b\|/gcd(a,b) |

### Category 2: Trigonometric Functions

| Function | Input Type | Output Type | Implementation |
|----------|------------|-------------|----------------|
| `sin` | number | float/complex | FFI import: math.sin |
| `cos` | number | float/complex | FFI import: math.cos |
| `tan` | number | float/complex | FFI import: math.tan |
| `asin` | number | float/complex | FFI import: math.asin |
| `acos` | number | float/complex | FFI import: math.acos |
| `atan` | number [number] | float | FFI import: math.atan / math.atan2 |

### Category 3: Hyperbolic Functions

| Function | Input Type | Output Type | Implementation |
|----------|------------|-------------|----------------|
| `sinh` | number | float/complex | FFI import: math.sinh |
| `cosh` | number | float/complex | FFI import: math.cosh |
| `tanh` | number | float/complex | FFI import: math.tanh |
| `asinh` | number | float | FFI import: math.asinh |
| `acosh` | number | float/complex | FFI import: math.acosh |
| `atanh` | number | float/complex | FFI import: math.atanh |

### Category 4: Bitwise Operations

| Function | Input Type | Output Type | WasmGC Implementation |
|----------|------------|-------------|----------------------|
| `ash` | integer, integer | integer | i64.shl / i64.shr_s (existing) |
| `logand` | integer* | integer | i64.and (existing) |
| `logior` | integer* | integer | i64.or (existing) |
| `logxor` | integer* | integer | i64.xor (existing) |
| `lognot` | integer | integer | i64.xor -1 (existing) |
| `logcount` | integer | integer | i64.popcnt (NEW) |
| `integer-length` | integer | integer | 64 - i64.clz (NEW) |

### Category 5: Mathematical Functions

| Function | Input Type | Output Type | Implementation |
|----------|------------|-------------|----------------|
| `exp` | number | float/complex | FFI import: math.exp |
| `log` | number [base] | float/complex | FFI import: math.log |
| `sqrt` | number | float/complex | f64.sqrt (native) or complex |
| `expt` | number, number | number | FFI import or native |

### Category 6: Complex Number Functions

| Function | Input Type | Output Type | WasmGC Implementation |
|----------|------------|-------------|----------------------|
| `complex` | real, real | complex | struct.new $complex |
| `realpart` | number | real | struct.get $complex 0 |
| `imagpart` | number | real | struct.get $complex 1 |
| `conjugate` | number | number | negate imagpart |
| `phase` | number | float | atan2(imag, real) |

## Type Coercion Rules

Per ANSI CL type contagion:

```
integer op integer → integer
integer op ratio → ratio
integer op float → float
integer op complex → complex
ratio op ratio → ratio
ratio op float → float
ratio op complex → complex
float op float → float
float op complex → complex
complex op complex → complex
```

## Validation Rules

### Input Validation

| Function | Validation | Error Condition |
|----------|------------|-----------------|
| `sqrt(x)` where x < 0 | Return complex | None (auto-promote) |
| `log(0)` | Signal error | division-by-zero |
| `asin(x)` where \|x\| > 1 | Return complex | None (auto-promote) |
| `acos(x)` where \|x\| > 1 | Return complex | None (auto-promote) |
| `atan(0, 0)` | Signal error | arithmetic-error |
| `expt(0, n)` where n < 0 | Signal error | division-by-zero |
| Bitwise on bignum > 64-bit | Signal error | arithmetic-error |

### Output Constraints

| Type | Range | Precision |
|------|-------|-----------|
| fixnum | -2^30 to 2^30-1 | Exact |
| float | ±1.7976931348623157e+308 | ~15-17 significant digits |
| complex | components are float | Same as float |

## Constants

| Name | Value | Type |
|------|-------|------|
| `pi` | 3.141592653589793 | float |
| `most-positive-fixnum` | 2^30 - 1 | integer |
| `most-negative-fixnum` | -2^30 | integer |
