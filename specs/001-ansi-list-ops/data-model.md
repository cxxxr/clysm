# Data Model: Phase 15A - ANSI List Operations Extension

**Date**: 2025-12-30
**Branch**: `001-ansi-list-ops`

## Overview

This feature implements list operations on existing data structures. No new data types are introduced. All operations work with the existing WasmGC type system defined in the Clysm compiler.

## Existing Entities (Used by This Feature)

### Cons Cell ($cons)

**WasmGC Type Index**: 2

```wat
(type $cons (struct
  (field $car (mut anyref))   ;; Field 0: CAR
  (field $cdr (mut anyref))   ;; Field 1: CDR
))
```

**Operations**:
- `struct.new 2`: Create new cons cell
- `struct.get 2 0`: Access CAR
- `struct.get 2 1`: Access CDR
- `struct.set 2 0`: Modify CAR (rplaca)
- `struct.set 2 1`: Modify CDR (rplacd)
- `ref.test (ref $cons)`: Type check

**Relationships**:
- CDR of cons may be another cons (proper list), NIL (list terminator), or any value (dotted pair)
- CAR of cons may be any Lisp value

### NIL (List Terminator)

**WasmGC Representation**: Singleton struct (type $symbol, global index 0)

**Identity Check**: `ref.eq` against global NIL reference

**Role in List Operations**:
- Terminates proper lists
- Returned when search fails (member, assoc, etc.)
- Returned for empty results (butlast of empty list, etc.)

### Closure ($closure)

**WasmGC Type Index**: 3

Used for `:test` and `:key` keyword arguments.

```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))
))
```

**Usage in List Operations**:
- `:test` functions are 2-argument closures (compare two values)
- `:key` functions are 1-argument closures (extract comparison value)

## Logical Data Structures

### Proper List

A chain of cons cells where each CDR is either another cons or NIL.

```
(a b c) = cons[a, cons[b, cons[c, NIL]]]
```

**Validation**: Functions assume proper lists. Circular or improper lists may cause infinite loops (per ANSI CL).

### Association List (Alist)

A list of cons cells where:
- Each element is a cons cell (key . value)
- CAR is the key
- CDR is the value

```
((a . 1) (b . 2)) = cons[cons[a,1], cons[cons[b,2], NIL]]
```

**Handling Non-Cons Elements**: Per ANSI CL, non-cons elements in an alist are silently skipped.

### Set (as List)

A list used as an unordered collection:
- No duplicate elements (by `:test` equality)
- Order not significant
- Implemented as a proper list

## State Transitions

### Destructive Operations

| Operation | Before State | After State |
|-----------|--------------|-------------|
| `nbutlast` | `(a b c d)` with n=1 | CDR of 3rd cons → NIL, returns `(a b c)` |
| `rplaca` (used internally) | cons[x, y] | cons[new-x, y] |
| `rplacd` (used internally) | cons[x, y] | cons[x, new-y] |

### Non-Destructive Operations

All other operations create new cons cells and do not modify input lists.

## Validation Rules

| Entity | Rule | Enforcement |
|--------|------|-------------|
| List argument | Must be proper list or NIL | Runtime: may loop on improper lists |
| N argument (nth, nthcdr, butlast, last) | Non-negative integer | Runtime: undefined for negative |
| :test function | 2-argument function returning generalized boolean | Compile-time: type signature |
| :key function | 1-argument function | Compile-time: type signature |
| Alist | List of conses (non-cons skipped) | Runtime: ref.test check |

## Memory Allocation Patterns

| Operation | Allocation | Notes |
|-----------|------------|-------|
| `nth`, `nthcdr`, `last` | None | Traversal only |
| `member`, `assoc`, `rassoc` | None | Traversal only |
| `butlast` | O(n) cons cells | Fresh list copy |
| `nbutlast` | None | In-place modification |
| `intersection`, `union`, `set-difference` | O(result size) | New result list |
| `acons` | 2 cons cells | New pair + new head |
| `pairlis` | 2n cons cells | n pairs + n spine |
| `copy-alist` | 2n cons cells | Copy spine + entries |
| `adjoin` | 0 or 1 cons cell | Only if not member |
