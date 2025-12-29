# Data Model: ANSI CL Array/Sequence Primitives

**Feature**: 001-ansi-array-primitives
**Date**: 2025-12-29

## Entity Definitions

### Simple-Vector

**WasmGC Representation**: Type 22 (`$mv_array`)

```wat
;; Type 22: (array (mut anyref))
;; Existing type, shared with multiple-values buffer
(type $simple_vector (array (mut anyref)))
```

| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| elements | `anyref[]` | Yes | Array of any Lisp values |

**Lisp Mapping**:
- `#(1 2 3)` → WasmGC array of type 22 containing i31ref values
- Element access: O(1) via `array.get 22`
- Length: O(1) via `array.len`

**Validation Rules**:
- Index must be non-negative integer
- Index must be < array length
- Elements can be any Lisp value (anyref)

### String

**WasmGC Representation**: Type 4 (`$string`)

```wat
;; Type 4: (array (mut i8))
;; Existing type for UTF-8 encoded strings
(type $string (array (mut i8)))
```

| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| bytes | `i8[]` | Yes | UTF-8 encoded bytes |

**Lisp Mapping**:
- `"hello"` → WasmGC array of type 4 containing ASCII bytes
- Character access: `array.get_u 4` returns unsigned byte, wrap in i31ref
- Length: `array.len` returns byte count (= char count for ASCII)

**Validation Rules**:
- Index must be non-negative integer
- Index must be < string length
- **Phase 13D-1 Assumption**: ASCII-only (1 byte = 1 character)

### Character

**WasmGC Representation**: i31ref (inline)

```wat
;; Characters are represented as i31ref containing Unicode code point
;; For ASCII: byte value == code point
```

| Encoding | Range | Representation |
|----------|-------|----------------|
| ASCII | 0-127 | i31ref with code point value |
| Extended | 128+ | i31ref with code point value (future) |

**Lisp Mapping**:
- `#\a` → i31ref containing 97
- `(char-code #\a)` → 97

### List (for elt)

**WasmGC Representation**: Type 2 (`$cons`)

```wat
;; Type 2: cons cell
(type $cons (struct (field (mut anyref)) (field (mut anyref))))
```

| Field | Index | Type | Description |
|-------|-------|------|-------------|
| car | 0 | anyref | First element |
| cdr | 1 | anyref | Rest of list |

**For `elt` on lists**:
- Traverse via `struct.get 2 1` (cdr) repeatedly
- Return `struct.get 2 0` (car) at target index

## Type Relationships

```
sequence
├── vector (abstract)
│   └── simple-vector → Type 22 ($mv_array)
├── string → Type 4 ($string)
└── list → Type 2 ($cons) chains
```

## State Transitions

### Array Element Modification

```
State 1: vector[i] = old-value
  ↓ (setf (aref vector i) new-value)
State 2: vector[i] = new-value
```

**WasmGC Operations**:
```wat
;; Transition from State 1 to State 2
local.get $vector
local.get $i
local.get $new-value
array.set 22
```

## Index Bounds Checking

All array access operations include runtime bounds checking:

```wat
;; Pattern for bounds-checked access
local.get $array
array.len                    ;; Get length
local.get $index
i32.le_u                     ;; index >= length?
(if (then
  ;; Signal bounds error
  ...throw...
))
;; Safe access
local.get $array
local.get $index
array.get 22
```

## Coercion Paths

| From | To | Method |
|------|----|--------|
| list | vector | Iterate list, collect into new array |
| vector | list | Iterate array, cons elements in reverse, nreverse |
| string | list | Iterate bytes, wrap each in i31ref as character |
| list (chars) | string | Iterate list, extract code points, fill array |

## Error Conditions

| Condition | When | Signal |
|-----------|------|--------|
| `type-error` | Index not an integer | Index must be of type INTEGER |
| `type-error` | Array not a vector | First arg must be a VECTOR |
| `type-error` | Invalid coerce target | Cannot coerce to type X |
| bounds error | Index out of range | Index N out of bounds for sequence of length M |
