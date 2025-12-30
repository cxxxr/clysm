# Data Model: ANSI CL Sequence Operations

**Branch**: `001-ansi-sequence-operations` | **Date**: 2025-12-29

## Entities

### Sequence (Abstract)

The abstract supertype for all sequence types in Common Lisp. All sequence operations work polymorphically across these types.

**Subtypes**:
- String
- Vector
- List

**Validation Rules**:
- All bounding indices must be non-negative integers
- Start index must be ≤ end index
- End index must be ≤ sequence length

### String

**WasmGC Type**: `+type-string+` (index 4)
**Structure**: `(array (mut i8))` - mutable array of bytes (UTF-8 encoded)

| Field | Type | Description |
|-------|------|-------------|
| bytes | i8[] | UTF-8 encoded character data |

**Validation Rules**:
- Length ≥ 0
- Valid UTF-8 byte sequences (enforced at Lisp level)

### Vector (Simple-Vector)

**WasmGC Type**: `+type-anyref-array+` (index 21, aliased from +type-slot-vector+)
**Structure**: `(array (mut anyref))` - mutable array of any Lisp values

| Field | Type | Description |
|-------|------|-------------|
| elements | anyref[] | Arbitrary Lisp values |

**Validation Rules**:
- Length ≥ 0
- Elements can be any Lisp type (boxed in anyref)

### List (Cons Chain)

**WasmGC Type**: `+type-cons+` (index 2)
**Structure**: `(struct (field car anyref) (field cdr anyref))`

| Field | Type | Description |
|-------|------|-------------|
| car | anyref | First element |
| cdr | anyref | Rest of list (cons or NIL) |

**Validation Rules**:
- Proper list terminates with NIL
- No circular list support in scope

### Bounding Indices

Parameters for subsequence operations.

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| start | fixnum | (required) | Starting index (inclusive) |
| end | fixnum | (length seq) | Ending index (exclusive) |

**Invariants**:
- 0 ≤ start ≤ end ≤ (length sequence)
- Violation signals `bounding-indices-bad-error`

### Result Type Specifier

First argument to [concatenate](../../resources/HyperSpec/Body/f_concat.htm).

| Specifier | Output Type |
|-----------|-------------|
| `'string` | String (UTF-8 byte array) |
| `'vector` | Simple-vector (anyref array) |
| `'(vector element-type)` | Specialized vector |
| `'list` | List (cons chain) |

## State Transitions

Not applicable - sequence operations are functional (create new sequences, do not mutate existing except for `(setf subseq)`).

### (setf subseq) Mutation

```text
Before: sequence[start:end] = old-values
Action: (setf (subseq sequence start end) new-sequence)
After:  sequence[start:end] = new-values (min length elements copied)
```

## Relationships

```text
                    ┌─────────────┐
                    │  Sequence   │ (abstract)
                    └─────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
    ┌──────────┐   ┌───────────┐   ┌──────────┐
    │  String  │   │  Vector   │   │   List   │
    │ (type 4) │   │ (type 21) │   │ (type 2) │
    └──────────┘   └───────────┘   └──────────┘
         │               │               │
         └───────────────┴───────────────┘
                         │
                         ▼
                  ┌─────────────┐
                  │  Operations │
                  │  - subseq   │
                  │  - concat   │
                  │  - copy-seq │
                  └─────────────┘
```
