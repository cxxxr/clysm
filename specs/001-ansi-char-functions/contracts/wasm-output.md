# Contract: WasmGC Character Function Output

**Date**: 2025-12-31
**Feature**: 001-ansi-char-functions

## Overview

All character functions compile to inline WasmGC instructions that pass `wasm-tools validate`.

## Input/Output Contracts

### Predicate Functions

Functions that return T or NIL based on character properties.

#### graphic-char-p

```
Input:  character (i31ref)
Output: T (i31ref with value 1) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
```

**Validation Contract**:
- Input is cast to i31 via `ref.cast`
- Returns `(:i32.const 1) :ref.i31` for T
- Returns `(:ref.null :none)` for NIL

#### standard-char-p

```
Input:  character (i31ref)
Output: T (i31ref with value 1) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
```

#### both-case-p

```
Input:  character (i31ref)
Output: T (i31ref with value 1) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
```

### Conversion Functions

#### char-name

```
Input:  character (i31ref)
Output: string ($string) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
```

**Validation Contract**:
- Returns $string struct for named characters
- Returns `(:ref.null :none)` for unnamed characters

#### name-char

```
Input:  string (anyref - $string expected)
Output: character (i31ref) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
```

**Validation Contract**:
- Performs case-insensitive string comparison
- Returns i31ref character for valid names
- Returns `(:ref.null :none)` for invalid names

#### digit-char

```
Input:  weight (i31ref), optional radix (i31ref, default 10)
Output: character (i31ref) or NIL (ref.null none)

Wasm signature: (func (param anyref) (result anyref))
             or (func (param anyref anyref) (result anyref))
```

**Validation Contract**:
- Returns digit character for valid weight < radix
- Returns `(:ref.null :none)` for weight >= radix or weight < 0
- Radix outside 2-36 signals type error

#### char-int

```
Input:  character (i31ref)
Output: integer (i31ref)

Wasm signature: (func (param anyref) (result anyref))
```

**Validation Contract**:
- Returns the same i31ref value (identity for this implementation)
- Equivalent to char-code

## Wasm Instruction Patterns

### Common Patterns

All functions follow these patterns from existing character functions:

```wat
;; Extract i32 from character
(:ref.cast :i31)
:i31.get_s
(:local.set $char)

;; Range check (e.g., 32-126 for graphic)
(:local.get $char)
(:i32.const 32)
:i32.ge_s
(:local.get $char)
(:i32.const 126)
:i32.le_s
:i32.and

;; Return T
(:i32.const 1)
:ref.i31

;; Return NIL
(:ref.null :none)

;; Return character from i32
(:local.get $code)
:ref.i31
```

### String Creation (char-name)

```wat
;; Create string struct for named character
(:struct.new $string ...)
```

## Validation Requirements

1. **wasm-tools validate**: All compiled functions must pass validation
2. **Type consistency**: Return type is always `anyref`
3. **No linear memory**: No `memory.grow`, `load`, or `store` instructions
4. **GC compliance**: Only use GC types (i31ref, struct, array)

## Error Conditions

| Function | Error Condition | Behavior |
|----------|-----------------|----------|
| digit-char | radix < 2 or radix > 36 | Signal type error |
| All predicates | Non-character input | Undefined (may trap on ref.cast) |
