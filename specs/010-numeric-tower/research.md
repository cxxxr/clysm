# Research: Numeric Tower Implementation

**Feature**: 010-numeric-tower | **Date**: 2025-12-24

## 1. WasmGC Numeric Type Representation

### Decision: Use WasmGC structs for Bignum, Ratio, Complex; f64 for Float

**Rationale**:
- WasmGC provides native `f64` (double-precision IEEE 754) which can be used directly
- Bignum requires an array of digits - use WasmGC `(array i32)` for limbs
- Ratio and Complex are compound types requiring struct representation
- All types must be distinguishable at runtime for type predicates

**Alternatives Considered**:
1. Linear memory for bignum: Rejected per Constitution (Section I - no linear memory)
2. JavaScript BigInt interop: Rejected (adds host dependency, reduces portability)
3. Single unified struct with tag: Rejected (wastes memory, slower type checks)

### Type Index Allocation

Current type indices 0-13 are used. New numeric types will be allocated:
- Type 14: `$bignum` - arbitrary-precision integer
- Type 15: `$ratio` - rational number (numerator/denominator)
- Type 16: `$float` - boxed f64 for uniformity with other numeric types
- Type 17: `$complex` - complex number (real/imaginary parts)

## 2. Bignum Representation

### Decision: Array of i32 limbs with sign flag

**Structure**:
```wat
(type $bignum (struct
  (field $sign i32)      ;; 0 = positive, 1 = negative
  (field $limbs (ref $limb_array))))

(type $limb_array (array (mut i32)))
```

**Rationale**:
- i32 limbs are efficient for WebAssembly (native word size in 32-bit mode)
- Sign-magnitude representation simplifies arithmetic
- GC-managed array avoids linear memory allocation
- Limbs stored in little-endian order (LSB first) for easier carry propagation

**Alternatives Considered**:
1. Two's complement: More complex for arbitrary precision
2. i64 limbs: Larger but fewer multiplications; i32 chosen for simplicity
3. BCD encoding: Very slow, only useful for decimal output

### Bignum Arithmetic Algorithms

- **Addition**: School algorithm with carry propagation - O(n)
- **Subtraction**: School algorithm with borrow - O(n)
- **Multiplication**: Karatsuba for large numbers, school algorithm for small - O(n^1.58) / O(n^2)
- **Division**: Knuth Algorithm D - O(n*m)
- **GCD**: Binary GCD (Stein's algorithm) - O(n^2)

## 3. Ratio Representation

### Decision: Struct with bignum numerator and denominator

**Structure**:
```wat
(type $ratio (struct
  (field $numerator anyref)   ;; fixnum or bignum
  (field $denominator anyref))) ;; fixnum or bignum (always positive, never zero)
```

**Rationale**:
- Numerator/denominator can be either fixnum (i31ref) or bignum
- Denominator is always positive (sign stored in numerator)
- Must maintain canonical form: GCD(num, den) = 1

**Canonicalization Rules** (per CLHS 12.1.4.3):
- If denominator divides numerator evenly, return integer
- Reduce to lowest terms using GCD
- Denominator always positive

## 4. Float Representation

### Decision: Boxed f64 in struct for type uniformity

**Structure**:
```wat
(type $float (struct
  (field $value f64)))
```

**Rationale**:
- Common Lisp distinguishes single-float and double-float, but we use f64 for both
- Boxing needed for anyref compatibility in mixed collections
- Direct f64 operations available via struct.get

**IEEE 754 Special Values**:
- Positive/negative infinity: `(/ 1.0 0.0)` and `(/ -1.0 0.0)`
- NaN: `(- +inf.0 +inf.0)` (quiet NaN)
- Negative zero: `-0.0` preserved per IEEE 754

## 5. Complex Representation

### Decision: Struct with real and imaginary parts as anyref

**Structure**:
```wat
(type $complex (struct
  (field $real anyref)   ;; any real number (fixnum, bignum, ratio, float)
  (field $imag anyref))) ;; any real number
```

**Rationale**:
- Parts can be any real type, maintaining Common Lisp semantics
- Complex with float parts uses float contagion
- Complex with integer parts stays exact

**Canonicalization Rules** (per CLHS 12.1.5.3):
- If imaginary part is exact zero and real part is rational, return real part
- Note: `#C(5.0 0.0)` stays complex (float zero is not exact zero)

## 6. Type Contagion Rules

### Decision: Follow Common Lisp standard contagion

**Numeric Tower Hierarchy**:
```
number
├── complex
└── real
    ├── float
    └── rational
        ├── ratio
        └── integer
            ├── bignum
            └── fixnum
```

**Contagion Rules** (from [CLHS 12.1.4](https://lisp-docs.github.io/cl-language-reference/chap-12/bc-b-number-concepts)):

1. **Float and Rational**: Rational is converted to float of same precision
2. **Float Precision**: Result is largest float format among arguments
3. **Complex Contagion**: Non-complex becomes complex with zero imaginary
4. **Integer Overflow**: Fixnum automatically promotes to bignum

**Implementation Strategy**:
```lisp
(defun coerce-numeric-args (left right)
  "Return (values coerced-left coerced-right common-type)"
  (cond
    ((and (complexp left) (complexp right)) ...)
    ((complexp left) (values left (complex right 0) :complex))
    ((complexp right) (values (complex left 0) right :complex))
    ((and (floatp left) (floatp right)) ...)
    ((floatp left) (values left (float right) :float))
    ((floatp right) (values (float left) right :float))
    ;; Both rational - compute with exact arithmetic
    (t ...)))
```

## 7. Mathematical Functions

### Decision: Implement core functions with appropriate type dispatch

**Required Functions** (per spec FR-009):

| Function | Input Types | Result Type | Notes |
|----------|-------------|-------------|-------|
| `sqrt` | real | float (or complex for negative) | Always returns float for positive reals |
| `expt` | number, number | number | Integer base ^ integer power = integer |
| `abs` | number | number | Same type, or float for complex |
| `gcd` | integer* | integer | Binary GCD algorithm |
| `lcm` | integer* | integer | Via GCD: `lcm(a,b) = abs(a*b)/gcd(a,b)` |
| `floor` | real | integer, real | Two values returned |
| `ceiling` | real | integer, real | Two values returned |
| `truncate` | real | integer, real | Two values returned |
| `round` | real | integer, real | Two values returned |
| `mod` | real, real | real | `(- x (* (floor x y) y))` |
| `rem` | real, real | real | `(- x (* (truncate x y) y))` |

**Trigonometric Functions** (deferred - not in core spec):
- sin, cos, tan, asin, acos, atan
- sinh, cosh, tanh, asinh, acosh, atanh
- These may require external library or Wasm SIMD

## 8. Type Predicates

### Decision: Runtime type checking via ref.test and ref.cast

**Predicates** (per spec FR-010):

| Predicate | Returns T for |
|-----------|---------------|
| `numberp` | Any numeric type |
| `integerp` | fixnum or bignum |
| `rationalp` | fixnum, bignum, or ratio |
| `realp` | fixnum, bignum, ratio, or float |
| `floatp` | float |
| `complexp` | complex |
| `zerop` | 0, 0.0, #C(0 0) |
| `plusp` | Positive real |
| `minusp` | Negative real |
| `evenp` | Even integer |
| `oddp` | Odd integer |

**Implementation**:
```wat
;; Check if value is a number
(func $numberp (param $val anyref) (result i32)
  (if (result i32) (ref.test i31 (local.get $val))
    (then (i32.const 1))  ;; fixnum is a number
    (else
      (if (result i32) (ref.test (ref $bignum) (local.get $val))
        (then (i32.const 1))
        (else
          (if (result i32) (ref.test (ref $ratio) (local.get $val))
            (then (i32.const 1))
            (else
              ...)))))))
```

## 9. Division by Zero Handling

### Decision: Signal DIVISION-BY-ZERO condition using Wasm exceptions

**Rationale**:
- Common Lisp requires signaling a condition, not returning special values
- Wasm exception handling (try_table) available per Constitution Section IV
- Float division: IEEE 754 says return infinity, but CL may signal

**Implementation**:
```wat
(tag $division-by-zero)

(func $check-divisor (param $d anyref)
  (if (call $zerop (local.get $d))
    (then (throw $division-by-zero))))
```

## 10. Fixnum-Bignum Promotion

### Decision: Automatic promotion on overflow detection

**Strategy**:
- Perform i32 arithmetic on fixnums
- Check for overflow using Wasm's `i32.add` + comparison
- If overflow, convert both operands to bignum and retry

**Overflow Detection**:
```wat
;; Add with overflow check
(func $fixnum-add (param $a i31ref) (param $b i31ref) (result anyref)
  (local $ia i32)
  (local $ib i32)
  (local $result i32)

  (local.set $ia (i31.get_s (local.get $a)))
  (local.set $ib (i31.get_s (local.get $b)))
  (local.set $result (i32.add (local.get $ia) (local.get $ib)))

  ;; Check i31 range: -2^30 to 2^30-1
  (if (i32.or
        (i32.lt_s (local.get $result) (i32.const -1073741824))
        (i32.gt_s (local.get $result) (i32.const 1073741823)))
    (then
      ;; Overflow: promote to bignum
      (return (call $bignum-add
                (call $fixnum-to-bignum (local.get $a))
                (call $fixnum-to-bignum (local.get $b)))))
    (else
      (return (ref.i31 (local.get $result))))))
```

## Sources

- [WebAssembly GC Specification](https://webassembly.github.io/gc/core/syntax/types.html)
- [CLHS 12.1 Number Concepts](https://lisp-docs.github.io/cl-language-reference/chap-12/bc-b-number-concepts)
- [CLTL2 Precision, Contagion, and Coercion](https://www.lix.polytechnique.fr/~liberti/public/computing/prog/lisp/cltl/clm/node122.html)
- [WebAssembly Wide-Arithmetic Proposal](https://github.com/WebAssembly/wide-arithmetic/blob/main/proposals/wide-arithmetic/Overview.md)
- [Wasm 3.0 Announcement](https://webassembly.org/news/2025-09-17-wasm-3.0/)
