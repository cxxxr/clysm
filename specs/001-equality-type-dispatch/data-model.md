# Data Model: Equality Predicate Type-Dispatch

**Date**: 2026-01-03

## Overview

This document defines the data structures and relationships for the unified equality predicate type-dispatch infrastructure.

## Entities

### Equality Level

Represents the semantic depth of comparison.

| Level | Keyword | Description |
|-------|---------|-------------|
| 0 | `:eq` | Object identity only |
| 1 | `:eql` | Identity + same-type numeric/char equality |
| 2 | `:equal` | Structural similarity (recursive cons, string=) |
| 3 | `:equalp` | Case-insensitive structural (numeric=, char-equal) |

**Validation Rules**:
- Must be one of the four defined keywords
- Higher levels subsume lower level semantics

### Type Dispatch Entry

Defines how to compare values of a specific WasmGC type.

| Field | Type | Description |
|-------|------|-------------|
| type-index | integer | WasmGC type index (0-28) |
| type-name | symbol | Human-readable name |
| comparator | function | `(level local-x local-y env) → instructions` |
| supports-recursion | boolean | Whether equal/equalp descend into this type |

**Key Types**:

| Index | Name | Recursion | Notes |
|-------|------|-----------|-------|
| - | null | No | ref.is_null check |
| - | i31ref | No | ref.eq or case-insensitive |
| 0 | $cons | Yes | Car/cdr comparison |
| 1 | $symbol | No | ref.eq only |
| 2 | $string | No | Byte-by-byte or case-insensitive |
| 4 | $float | No | f64.eq or numeric= |
| 5 | $ratio | No | Component or numeric= |

### Comparison Result

The Wasm instructions produced by comparison generators.

| Outcome | Wasm Representation |
|---------|---------------------|
| True | `(:i32.const 1) :ref.i31` |
| False | `(:ref.null :none)` |
| Continue (worklist) | `(:br $loop)` |
| Not-equal exit | `(:br $done)` after setting result to NIL |

### Worklist Entry (equal/equalp only)

For recursive cons comparison.

| Field | Type | Description |
|-------|------|-------------|
| pair | cons | `(cons x y)` - values to compare |
| rest | cons | Remaining pairs on worklist |

**Representation**: Cons cells used as pairs, worklist as cons list.

## Relationships

```
Equality Level
    │
    ▼
compile-equality-predicate
    │
    ├─── emit-null-comparison
    │
    └─── compile-type-dispatch
            │
            ├─── emit-i31-comparison (level)
            │
            ├─── emit-float-comparison (level)
            │
            ├─── emit-ratio-comparison (level)
            │
            ├─── emit-string-comparison (level)
            │
            ├─── emit-cons-comparison (level)
            │       │
            │       └─── [worklist push for equal/equalp]
            │
            └─── emit-default-comparison
```

## State Transitions

### Worklist Processing (equal/equalp)

```
┌─────────────────────────────────────────────────┐
│ Initial: worklist = [(cons arg1 arg2)]          │
│          result = T                              │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│ Loop: Pop pair from worklist                    │
│       Compare x and y based on type             │
└─────────────────────────────────────────────────┘
          │              │              │
          ▼              ▼              ▼
   ┌───────────┐  ┌───────────┐  ┌───────────┐
   │ Same type │  │ Different │  │ Both cons │
   │ Match     │  │ types     │  │           │
   └───────────┘  └───────────┘  └───────────┘
         │              │              │
         ▼              ▼              ▼
   [Continue]    [result=NIL]   [Push car/cdr]
   [next pair]   [Exit done]    [pairs to WL]
                                [Continue]
                                      │
                                      ▼
┌─────────────────────────────────────────────────┐
│ Worklist empty: Return result (T or NIL)        │
└─────────────────────────────────────────────────┘
```

## Wasm Local Variables

### Common to All Levels

| Local | Type | Purpose |
|-------|------|---------|
| local-x | anyref | First argument value |
| local-y | anyref | Second argument value |

### Additional for equal/equalp

| Local | Type | Purpose |
|-------|------|---------|
| worklist-local | anyref | Worklist of pairs to compare |
| pair-local | anyref | Current pair being compared |
| result-local | anyref | Result accumulator (T or NIL) |
| len1-local | i32 | String length 1 |
| len2-local | i32 | String length 2 |
| idx-local | i32 | String comparison index |

### Additional for equalp

| Local | Type | Purpose |
|-------|------|---------|
| byte1-local | i32 | String byte 1 |
| byte2-local | i32 | String byte 2 |
| f1-local | f64 | Float value 1 (for numeric=) |
| f2-local | f64 | Float value 2 (for numeric=) |
