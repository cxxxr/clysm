# Data Model: Type Predicates

**Feature**: 023-type-predicates
**Date**: 2025-12-26

## Type Hierarchy

The Clysm type system uses WasmGC reference types with the following hierarchy:

```
anyref (universal root)
├── i31ref (fixnum, character - 31-bit tagged)
│   ├── Fixnum: signed 31-bit integer
│   └── Character: Unicode code point (temporary: shared encoding)
│
└── structref (heap-allocated objects)
    ├── $nil (type 0) - NIL singleton
    ├── $unbound (type 1) - Unbound marker
    ├── $cons (type 2) - Cons cells
    ├── $symbol (type 3) - Symbols
    ├── $string (type 4) - UTF-8 strings
    ├── $closure (type 5) - Functions/closures
    ├── $instance (type 6) - CLOS instances
    ├── $standard-class (type 7) - Class objects
    ├── $func-0..N (types 8-12) - Function signatures
    ├── $binding-frame (type 13) - Dynamic bindings
    ├── $bignum (type 14) - Arbitrary-precision integers
    ├── $ratio (type 15) - Exact rational numbers
    ├── $float (type 16) - IEEE 754 doubles
    ├── $complex (type 17) - Complex numbers
    ├── $limb-array (type 18) - Bignum internals
    └── $stream (type 19) - I/O streams
```

## Predicate Classification

### Type Predicates (Work on ANY value)

These predicates accept any Lisp value and return T or NIL:

| Predicate | Returns T for | Type Check |
|-----------|---------------|------------|
| `integerp` | Fixnum, Bignum | `ref.test :i31` OR `ref.test $bignum` |
| `numberp` | Any number | integerp OR ratiop OR floatp OR complexp |
| `floatp` | Float | `ref.test $float` |
| `rationalp` | Integer, Ratio | integerp OR `ref.test $ratio` |
| `complexp` | Complex | `ref.test $complex` |
| `symbolp` | Symbol | `ref.test $symbol` OR `ref.is_null` (NIL) |
| `functionp` | Closure | `ref.test $closure` |
| `characterp` | Character | `ref.test :i31` (temporary) |

### Numeric Predicates (Require numeric argument)

These predicates require a numeric argument:

| Predicate | Domain | Returns T when |
|-----------|--------|----------------|
| `zerop` | Number | value = 0 |
| `plusp` | Real | value > 0 |
| `minusp` | Real | value < 0 |
| `evenp` | Integer | value mod 2 = 0 |
| `oddp` | Integer | value mod 2 ≠ 0 |

### Numeric Function

| Function | Domain | Returns |
|----------|--------|---------|
| `signum` | Number | -1/0/1 (preserves type) |

## Type Structures (from gc-types.lisp)

### $bignum (type 14)

```wat
(type $bignum (struct
  (field $sign i32)           ; 0 = non-negative, 1 = negative
  (field $limbs (ref $limb-array))))
```

### $ratio (type 15)

```wat
(type $ratio (struct
  (field $numerator anyref)   ; Integer (fixnum or bignum)
  (field $denominator anyref))) ; Positive integer
```

Invariants:
- GCD(|numerator|, denominator) = 1
- denominator > 0

### $float (type 16)

```wat
(type $float (struct
  (field $value f64)))        ; IEEE 754 double-precision
```

Note: Both single-float and double-float use f64 internally.

### $complex (type 17)

```wat
(type $complex (struct
  (field $real anyref)        ; Real part (integer, ratio, or float)
  (field $imag anyref)))      ; Imaginary part
```

### $symbol (type 3)

```wat
(type $symbol (struct
  (field $name anyref)        ; String
  (field $value anyref)       ; Symbol value
  (field $function anyref)    ; Function binding
  (field $plist anyref)))     ; Property list
```

### $closure (type 5)

```wat
(type $closure (struct
  (field $code_0 (ref null $func-0))  ; 0-arg entry
  (field $code_1 (ref null $func-1))  ; 1-arg entry
  (field $code_2 (ref null $func-2))  ; 2-arg entry
  (field $code_n (ref null $func-n))  ; varargs entry
  (field $env anyref)))               ; Captured environment
```

## Return Value Encoding

All predicates return Lisp booleans:

| Value | Wasm Representation | Instructions |
|-------|---------------------|--------------|
| T | i31ref(1) | `(i32.const 1) ref.i31` |
| NIL | null | `ref.null :none` |

## Validation Rules

### Argument Validation

| Predicate | Argument Requirement | On Invalid |
|-----------|---------------------|------------|
| Type predicates | Any value | N/A (always valid) |
| zerop | Number | NIL (or error) |
| plusp, minusp | Real number | NIL (or error) |
| evenp, oddp | Integer | NIL (or error) |
| signum | Number | NIL (or error) |

### State Transitions

N/A - All predicates are pure functions with no state changes.

## Relationships

```
numberp ──────────────────┐
    ├── integerp ─────────┤
    │       ├── fixnum    │
    │       └── bignum    │
    ├── rationalp ────────┤
    │       ├── integerp  │
    │       └── ratio     │
    ├── floatp ───────────┤
    │       └── float     │
    └── complexp ─────────┘
            └── complex

symbolp
    └── symbol (includes NIL)

functionp
    └── closure
```

## Implementation Notes

### NIL Special Cases

NIL is both:
- A symbol (symbolp → T)
- The empty list (listp → T, already implemented)

Represented as `ref.null :none`, so:
- `symbolp` must check `ref.is_null` in addition to `ref.test $symbol`
- Type predicates on NIL: all return NIL except symbolp

### Character Encoding

Characters currently share i31 encoding with fixnums. Proper character/fixnum distinction requires type tagging in i31 range:

```
Current:  i31 = fixnum OR character (indistinguishable)
Future:   i31 low bits = type tag
          00 = fixnum
          01 = character
          etc.
```

For this feature: `characterp` returns T for all i31ref values (temporary implementation).
