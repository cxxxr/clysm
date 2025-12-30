# Data Model: ANSI Sequence Generic Functions

**Feature**: 001-ansi-sequence-functions (Phase 15B)
**Date**: 2025-12-31

## Overview

This feature implements ANSI CL sequence generic functions. The data model describes the types and entities involved in sequence operations, following WasmGC type representations per Constitution Principle I.

## Core Entities

### Sequence (Abstract)

The abstract supertype encompassing all ordered collections.

```
Sequence
├── List (proper list of cons cells)
├── Vector (WasmGC array)
└── String (character vector, WasmGC array of i8)
```

**WasmGC Type Mapping**:
| Lisp Type | WasmGC Type Index | Representation |
|-----------|-------------------|----------------|
| List | $cons (0) | Linked cons cells, NIL-terminated |
| Vector | (array anyref) | GC-managed array of anyref |
| String | $string (2) | UTF-8 bytes in GC array |

### Bounding Index Designator

Integer specifying position within a sequence.

**Attributes**:
- Value: Non-negative integer
- Range: `[0, sequence-length]`
- Default for `:start`: 0
- Default for `:end`: NIL (meaning sequence length)

**Validation Rules**:
- `0 <= start <= end <= length`
- If violated: signal `type-error` or implementation-specific error

### Test Function

Two-argument predicate for element comparison.

**Type**: `(function (T T) generalized-boolean)`

**Standard Test Functions**:
| Function | Behavior |
|----------|----------|
| `eql` | Default; identity for numbers, `eq` otherwise |
| `eq` | Pointer equality |
| `equal` | Structural equality |
| `equalp` | Case-insensitive, type-coercing equality |
| `string=` | String comparison |
| `char=` | Character comparison |

### Key Function

One-argument function for element transformation before comparison.

**Type**: `(function (T) U)`

**Standard Key Functions**:
| Function | Behavior |
|----------|----------|
| `identity` | Default; returns element unchanged |
| `car` | First element of cons |
| `cdr` | Rest of cons |
| Custom | User-defined transformation |

### Predicate

One-argument function returning generalized boolean.

**Type**: `(function (T) generalized-boolean)`

**Usage**: `-if` and `-if-not` function variants

## Function Signatures

### Group 1: Counting Functions

```lisp
;; [count](resources/HyperSpec/Body/f_countc.htm)
(count item sequence &key from-end start end key test)
  => count (non-negative integer)

(count-if predicate sequence &key from-end start end key)
  => count (non-negative integer)

(count-if-not predicate sequence &key from-end start end key)
  => count (non-negative integer)
```

### Group 2: Search Functions

```lisp
;; [find](resources/HyperSpec/Body/f_find_.htm)
(find item sequence &key from-end start end key test)
  => element or NIL

(find-if predicate sequence &key from-end start end key)
  => element or NIL

(find-if-not predicate sequence &key from-end start end key)
  => element or NIL

;; [position](resources/HyperSpec/Body/f_pos_p.htm)
(position item sequence &key from-end start end key test)
  => index (non-negative integer) or NIL

(position-if predicate sequence &key from-end start end key)
  => index or NIL

(position-if-not predicate sequence &key from-end start end key)
  => index or NIL
```

### Group 3: Comparison Functions

```lisp
;; [mismatch](resources/HyperSpec/Body/f_mismat.htm)
(mismatch sequence1 sequence2 &key from-end test key start1 end1 start2 end2)
  => index (non-negative integer) or NIL

;; [search](resources/HyperSpec/Body/f_search.htm)
(search sequence1 sequence2 &key from-end test key start1 end1 start2 end2)
  => index or NIL
```

### Group 4: Substitution Functions

```lisp
;; [substitute](resources/HyperSpec/Body/f_substc.htm)
(substitute newitem olditem sequence &key from-end test start end count key)
  => new-sequence

(substitute-if newitem predicate sequence &key from-end start end count key)
  => new-sequence

(substitute-if-not newitem predicate sequence &key from-end start end count key)
  => new-sequence

;; Destructive variants
(nsubstitute newitem olditem sequence &key from-end test start end count key)
  => sequence (modified)

(nsubstitute-if newitem predicate sequence &key from-end start end count key)
  => sequence (modified)

(nsubstitute-if-not newitem predicate sequence &key from-end start end count key)
  => sequence (modified)
```

### Group 5: Duplicate Removal Functions

```lisp
;; [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
(remove-duplicates sequence &key from-end test start end key)
  => new-sequence

;; Destructive variant
(delete-duplicates sequence &key from-end test start end key)
  => sequence (modified)
```

### Group 6: Modification Functions

```lisp
;; [fill](resources/HyperSpec/Body/f_fill.htm)
(fill sequence item &key start end)
  => sequence (modified)

;; [replace](resources/HyperSpec/Body/f_replac.htm)
(replace sequence1 sequence2 &key start1 end1 start2 end2)
  => sequence1 (modified)
```

## State Transitions

### Sequence Mutation (Destructive Operations)

```
┌─────────────────┐     nsubstitute/delete-duplicates/fill/replace     ┌─────────────────┐
│   Original      │ ──────────────────────────────────────────────────> │   Modified      │
│   Sequence      │                                                     │   Sequence      │
└─────────────────┘                                                     └─────────────────┘
```

**Invariants**:
- Sequence identity preserved (same object)
- Sequence length may change (delete-duplicates)
- Element positions may change (delete-duplicates)

### Sequence Copying (Non-Destructive Operations)

```
┌─────────────────┐     substitute/remove-duplicates     ┌─────────────────┐
│   Original      │ ────────────────────────────────────> │   Original      │
│   Sequence      │                                       │   (unchanged)   │
└─────────────────┘                                       └─────────────────┘
        │
        │ creates
        ▼
┌─────────────────┐
│   New           │
│   Sequence      │
└─────────────────┘
```

## Validation Rules

### Bounding Index Validation

```
validate(start, end, length):
  real_end = end ?? length
  assert 0 <= start
  assert start <= real_end
  assert real_end <= length
  return (start, real_end)
```

### :test/:test-not Mutual Exclusion

```
validate_test_args(test, test-not):
  if test AND test-not:
    signal error "Cannot supply both :test and :test-not"
```

## Relationships

```
                    ┌──────────────┐
                    │   Sequence   │
                    │   (input)    │
                    └──────┬───────┘
                           │
           ┌───────────────┼───────────────┐
           │               │               │
           ▼               ▼               ▼
    ┌──────────┐    ┌──────────┐    ┌──────────┐
    │  :test   │    │   :key   │    │ bounds   │
    │ function │    │ function │    │ (start/  │
    │          │    │          │    │  end)    │
    └────┬─────┘    └────┬─────┘    └────┬─────┘
         │               │               │
         └───────────────┼───────────────┘
                         │
                         ▼
                ┌────────────────┐
                │ Sequence Op    │
                │ (count/find/   │
                │  position/...) │
                └────────┬───────┘
                         │
                         ▼
                ┌────────────────┐
                │    Result      │
                │ (value/index/  │
                │  new-sequence) │
                └────────────────┘
```
