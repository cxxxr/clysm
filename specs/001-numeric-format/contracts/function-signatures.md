# Function Signatures: Numeric Conversion and Formatting

**Date**: 2025-12-30
**Branch**: `001-numeric-format`

## API Contracts

### `rationalize`

**ANSI CL Reference**: [rationalize](../../resources/HyperSpec/Body/f_ration.htm)

#### Signature

```lisp
(rationalize number) → rational
```

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| number | real | Yes | Number to convert to rational |

#### Return Value

| Type | Description |
|------|-------------|
| rational | Integer or ratio approximating the input |

#### Type Dispatch

| Input Type | Output Type | Behavior |
|------------|-------------|----------|
| integer | integer | Return unchanged |
| ratio | ratio | Return unchanged |
| float | integer or ratio | Continued fraction approximation |

#### Error Conditions

| Condition | Error Type |
|-----------|------------|
| NaN input | `arithmetic-error` |
| Infinity input | `arithmetic-error` |
| Non-real input | `type-error` |

#### Examples

```lisp
(rationalize 0.5)       → 1/2
(rationalize 3.0)       → 3
(rationalize 0.333333)  → 1/3  ; or close approximation
(rationalize 1/2)       → 1/2
(rationalize 5)         → 5
```

---

### `write-to-string`

**ANSI CL Reference**: [write-to-string](../../resources/HyperSpec/Body/f_wr_to_.htm)

#### Signature

```lisp
(write-to-string object &key base) → string
```

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| object | T | Yes | - | Object to convert to string |
| :base | (integer 2 36) | No | 10 | Radix for integer output |

#### Return Value

| Type | Description |
|------|-------------|
| string | Readable representation of the object |

#### Type Dispatch (Numeric Objects)

| Input Type | Output Format |
|------------|---------------|
| fixnum | Base-converted digits |
| bignum | Base-converted digits |
| ratio | "numerator/denominator" (both base-converted) |
| float | Decimal representation |

#### Digit Characters

| Digit Value | Character |
|-------------|-----------|
| 0-9 | '0'-'9' |
| 10-35 | 'A'-'Z' |

#### Error Conditions

| Condition | Error Type |
|-----------|------------|
| `:base` < 2 | `type-error` |
| `:base` > 36 | `type-error` |
| `:base` not integer | `type-error` |

#### Examples

```lisp
(write-to-string 42)            → "42"
(write-to-string 42 :base 16)   → "2A"
(write-to-string 42 :base 2)    → "101010"
(write-to-string 42 :base 8)    → "52"
(write-to-string -42 :base 16)  → "-2A"
(write-to-string 255 :base 16)  → "FF"
(write-to-string 1/2)           → "1/2"
(write-to-string 1/2 :base 16)  → "1/2"
(write-to-string 3.14)          → "3.14" ; or "3.14d0"
```

---

## WasmGC Compilation Contract

### Expected Wasm Instructions

#### Type Check Pattern

```wat
;; Check if value is fixnum (i31ref)
(ref.test (ref i31) (local.get $val))

;; Check if value is ratio (type 15)
(ref.test (ref 15) (local.get $val))

;; Check if value is float (type 16)
(ref.test (ref 16) (local.get $val))
```

#### Struct Access Pattern

```wat
;; Get ratio numerator
(struct.get 15 0 (ref.cast (ref 15) (local.get $ratio)))

;; Get ratio denominator
(struct.get 15 1 (ref.cast (ref 15) (local.get $ratio)))

;; Get float value
(struct.get 16 0 (ref.cast (ref 16) (local.get $float)))
```

#### Ratio Construction Pattern

```wat
;; Create new ratio
(struct.new 15 (local.get $num) (local.get $den))
```

---

## Interpreter Registration Contract

Both functions must be registered in `interpreter-builtins.lisp`:

```lisp
;; rationalize (already registered)
(env-bind env 'rationalize #'rationalize)

;; write-to-string (to be added)
(env-bind env 'write-to-string #'write-to-string)
```
