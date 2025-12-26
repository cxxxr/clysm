# Data Model: Multiple Values Support

**Feature**: 025-multiple-values
**Date**: 2025-12-26

## Entities

### 1. Multiple Values Count (`$mv-count`)

**Description**: Tracks the number of values returned by the most recent values-producing form.

| Attribute | Type | Description |
|-----------|------|-------------|
| value | i32 | Count of values (0 to 20) |
| mutability | mut | Mutable (updated on each values call) |
| default | 1 | Single value by default |

**Invariants**:
- 0 ≤ value ≤ 20 (ANSI `multiple-values-limit`)
- Reset to 1 before every function call (single-value context)
- Set by: `values`, `values-list`, `floor`, `truncate`, `ceiling`, `round`
- Read by: `multiple-value-bind`, `multiple-value-list`, `nth-value`, `multiple-value-prog1`, `multiple-value-call`

### 2. Multiple Values Buffer (`$mv-buffer`)

**Description**: Array storing secondary values (values 2 through N).

| Attribute | Type | Description |
|-----------|------|-------------|
| elements | anyref[20] | Storage for secondary values |
| mutability | mut | Mutable array reference |
| indexing | 0-based | Buffer[0] = 2nd value, Buffer[1] = 3rd value, etc. |

**Invariants**:
- Only indices 0 through (mv-count - 2) contain valid values
- Indices beyond (mv-count - 2) may contain stale data (not cleared)
- NIL used for missing values when bound variables exceed available values

### 3. Primary Value

**Description**: The first value returned, passed on the Wasm stack.

| Attribute | Type | Description |
|-----------|------|-------------|
| location | stack | Top of Wasm operand stack |
| type | anyref | Any Lisp value |

**Invariants**:
- Always present (even `(values)` returns NIL as primary)
- Follows normal Wasm return semantics
- Unaffected by multiple-value machinery in single-value context

## Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                     values-producing form                   │
│                   (values v1 v2 v3 ...)                     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
        ┌─────────────────────┴─────────────────────┐
        │                                           │
        ▼                                           ▼
┌───────────────┐                         ┌─────────────────┐
│ Primary Value │                         │ Secondary Values│
│    (stack)    │                         │   (mv-buffer)   │
│     = v1      │                         │ [0]=v2, [1]=v3  │
└───────────────┘                         └─────────────────┘
        │                                           │
        │                                           │
        ▼                                           ▼
┌─────────────────────────────────────────────────────────────┐
│                    values-receiving form                    │
│         (multiple-value-bind (a b c) form body)             │
│                                                             │
│   a ← primary value (v1)                                    │
│   b ← mv-buffer[0] if mv-count > 1 else NIL                │
│   c ← mv-buffer[1] if mv-count > 2 else NIL                │
└─────────────────────────────────────────────────────────────┘
```

## State Transitions

### mv-count State Machine

```
                    ┌──────────────────┐
                    │   Initial: 1     │
                    │ (single value)   │
                    └────────┬─────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  (values)       │ │ (values a)      │ │ (values a b ..) │
│  count ← 0      │ │  count ← 1      │ │  count ← N      │
└─────────────────┘ └─────────────────┘ └─────────────────┘
         │                   │                   │
         └───────────────────┼───────────────────┘
                             │
                             ▼
                    ┌──────────────────┐
                    │ Next single-value│
                    │ context resets   │
                    │ count to 1       │
                    └──────────────────┘
```

## Validation Rules

| Rule ID | Entity | Constraint | Error Condition |
|---------|--------|------------|-----------------|
| MV-001 | mv-count | 0 ≤ count ≤ 20 | Values form with >20 args |
| MV-002 | mv-buffer | Valid anyref at each index | N/A (always valid anyref or null) |
| MV-003 | values-list | Argument must be list | type-error if not cons or nil |
| MV-004 | nth-value | Index must be non-negative | undefined (ANSI doesn't specify) |

## Global Index Allocation

| Index | Name | Type | Mutability | Init Expression |
|-------|------|------|------------|-----------------|
| 0 | $nil | anyref | const | `(struct.new 0)` |
| 1 | $unbound | anyref | const | `(struct.new 1)` |
| 2 | $mv-count | i32 | mut | `(i32.const 1)` |
| 3 | $mv-buffer | (ref $mv-array) | mut | `(array.new_default $mv-array (i32.const 20))` |
| 4+ | special vars | anyref | mut | varies |
