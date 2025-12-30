# Data Model: Phase 15C - ANSI Array Operations

**Date**: 2025-12-31
**Status**: Draft

## Entity Definitions

### 1. Multidimensional Array (`$mdarray`)

A wrapper struct for arrays with explicit dimension metadata.

**WasmGC Type Definition**:
```wat
(type $mdarray (struct
  (field $dimensions (ref $cons))     ;; Immutable list of dimension sizes
  (field $storage (ref $mv_array))    ;; 1D element storage
  (field $adjustable i32)             ;; 0=fixed, 1=adjustable
))
```

**Type Index**: 28 (defined in `gc-types.lisp` as `+type-mdarray+`)

**Fields**:

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| `$dimensions` | `(ref $cons)` | Immutable | Proper list of fixnum dimension sizes |
| `$storage` | `(ref $mv_array)` | Mutable (elements) | Underlying 1D array of anyref |
| `$adjustable` | `i32` | Immutable | Flag for resize capability |

### 2. Simple Vector (`$mv_array`) - Existing

Raw 1D array without dimension metadata. Used for backward compatibility.

**WasmGC Type Definition** (already exists as type 20):
```wat
(type $mv_array (array (mut anyref)))
```

**Implied Metadata**:
- Rank: 1
- Dimensions: `(array.len)`
- Total size: `array.len`
- Adjustable: NIL (always fixed)

## Entity Relationships

```
┌─────────────────────────────────────────────────────────┐
│                      anyref                             │
│                        │                                │
│           ┌────────────┴────────────┐                   │
│           ▼                         ▼                   │
│    ┌─────────────┐          ┌─────────────┐             │
│    │  $mdarray   │          │  $mv_array  │             │
│    │  (type 28)  │          │  (type 20)  │             │
│    └──────┬──────┘          └─────────────┘             │
│           │                        ▲                    │
│           │ $storage               │                    │
│           └────────────────────────┘                    │
└─────────────────────────────────────────────────────────┘
```

**Relationships**:
- `$mdarray` **contains** one `$mv_array` for element storage
- `$mdarray.$dimensions` is a cons list of fixnum sizes
- Both `$mdarray` and raw `$mv_array` are valid array representations

## Type Dispatch Logic

### Identifying Array Types

```
Input: anyref value
Output: (values storage rank dimensions total-size adjustable-p)

if (ref.test $mdarray value)
  ;; Multidimensional array
  storage    = struct.get $mdarray $storage value
  dimensions = struct.get $mdarray $dimensions value
  rank       = (length dimensions)
  total-size = array.len storage
  adjustable = (= 1 (struct.get $mdarray $adjustable value))
else if (ref.test $mv_array value)
  ;; Simple vector
  storage    = value
  dimensions = (list (array.len value))
  rank       = 1
  total-size = array.len value
  adjustable = NIL
else
  ;; Not an array - signal type-error
  (error 'type-error :datum value :expected-type 'array)
end
```

## Row-Major Index Mapping

### Subscript to Linear Index

For array with dimensions `(d₀ d₁ ... d_{n-1})` and subscripts `(s₀ s₁ ... s_{n-1})`:

```
row-major-index = s₀ × (d₁ × d₂ × ... × d_{n-1})
                + s₁ × (d₂ × d₃ × ... × d_{n-1})
                + ...
                + s_{n-2} × d_{n-1}
                + s_{n-1}
```

**Example**: 3×4×5 array, subscripts (1, 2, 3)
```
index = 1 × (4 × 5) + 2 × 5 + 3
      = 20 + 10 + 3
      = 33
```

### Linear Index to Subscripts (inverse)

```
For each dimension i from 0 to rank-1:
  stride = product(dimensions[i+1:])
  subscript[i] = index / stride
  index = index mod stride
```

## Validation Rules

### Array Construction

1. All dimension values MUST be non-negative fixnums
2. Product of dimensions MUST equal storage array length
3. `$adjustable` MUST be 0 or 1

### Subscript Validation

1. Number of subscripts MUST equal rank
2. Each subscript MUST be a non-negative fixnum
3. Each subscript MUST be less than corresponding dimension

### adjust-array Rules

1. Input array MUST have `$adjustable = 1`
2. New dimensions MUST all be non-negative fixnums
3. Existing elements are preserved in row-major order
4. New positions filled with `:initial-element` (default NIL)

## State Transitions

### Array Lifecycle

```
┌─────────────┐    make-array     ┌─────────────┐
│   (none)    │ ───────────────▶ │  allocated  │
└─────────────┘                   └──────┬──────┘
                                         │
                    ┌────────────────────┴────────────────────┐
                    │                                         │
                    ▼ (adjustable-p = NIL)                    ▼ (adjustable-p = T)
             ┌─────────────┐                           ┌─────────────┐
             │    fixed    │                           │ adjustable  │
             └─────────────┘                           └──────┬──────┘
                                                              │
                                                   adjust-array
                                                              │
                                                              ▼
                                                       ┌─────────────┐
                                                       │  resized    │
                                                       │ (new array) │
                                                       └─────────────┘
```

**Notes**:
- Fixed arrays cannot transition to adjustable
- `adjust-array` creates a new array object (per WasmGC immutability)
- GC reclaims unreferenced old array
