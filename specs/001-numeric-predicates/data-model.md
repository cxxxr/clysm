# Data Model: Phase 14B - Numeric Type Predicates Enhancement

**Feature**: 001-numeric-predicates
**Date**: 2025-12-30

## Entities

### Byte Specifier

A byte specifier is an opaque value representing a contiguous field of bits within an integer.

**Representation**: Fixnum (i31ref) with encoded size and position.

**Encoding Schema**:
```
┌─────────────────────────────────────────────┐
│  i31ref (31 bits signed)                    │
├─────────────────────────────────────────────┤
│ Bits 0-5:  position (0-63)                  │
│ Bits 6-11: size (0-63)                      │
│ Bits 12-30: unused (always 0 for valid)     │
└─────────────────────────────────────────────┘
```

**Operations**:

| Function | Input | Output | Formula |
|----------|-------|--------|---------|
| `byte` | size, position | byte-spec | `(size << 6) \| position` |
| `byte-size` | byte-spec | size | `byte-spec >> 6` |
| `byte-position` | byte-spec | position | `byte-spec & 0x3F` |

**Invariants**:
- `0 <= size <= 63`
- `0 <= position <= 63`
- `size + position <= 31` for meaningful fixnum operations (soft limit)

**Examples**:
```lisp
(byte 8 0)    ; => 512  (low byte)
(byte 8 8)    ; => 520  (second byte)
(byte 4 4)    ; => 260  (nibble at position 4)
(byte 1 31)   ; => 95   (single bit at position 31)
(byte 0 0)    ; => 0    (empty byte)
```

### Integer Types Affected

| Type | WasmGC Repr | Range | Notes |
|------|-------------|-------|-------|
| Fixnum | i31ref | -2^30 to 2^30-1 | Primary target |
| Bignum | struct | Arbitrary | Future extension |

### Boolean Results

All predicates return:
- **T**: Represented as `(i32.const 1) ref.i31`
- **NIL**: Represented as `(ref.null :none)`

## State Transitions

N/A - All functions are pure and stateless.

## Validation Rules

| Rule | Check | Error |
|------|-------|-------|
| `logbitp` index | Must be non-negative integer | `TYPE-ERROR` |
| `byte` size | Must be non-negative integer | `TYPE-ERROR` |
| `byte` position | Must be non-negative integer | `TYPE-ERROR` |
| `ldb`/`dpb` integer | Must be integer | `TYPE-ERROR` |

## Relationships

```
┌─────────────┐     creates      ┌───────────────┐
│    byte     │ ───────────────> │ byte-specifier│
└─────────────┘                  └───────────────┘
                                        │
                        ┌───────────────┼───────────────┐
                        │               │               │
                        v               v               v
                 ┌───────────┐   ┌───────────┐   ┌─────────────┐
                 │ byte-size │   │byte-posit.│   │ ldb/dpb/... │
                 └───────────┘   └───────────┘   └─────────────┘
```

## Wasm Type Integration

| Entity | WasmGC Type | Type Index |
|--------|-------------|------------|
| Byte specifier | i31ref | N/A (primitive) |
| Fixnum | i31ref | N/A (primitive) |
| T | i31ref | N/A (value 1) |
| NIL | null (anyref) | N/A |

No new WasmGC types are required for this feature.
