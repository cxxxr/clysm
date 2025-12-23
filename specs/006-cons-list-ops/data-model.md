# Data Model: Cons Cell and List Operations

**Feature**: 006-cons-list-ops
**Date**: 2025-12-23
**Phase**: 1 (Design)

## Entity Overview

This feature uses the existing `$cons` WasmGC struct type. No new types are introduced.

## Entity: Cons Cell

### WasmGC Representation

```wat
;; Type index: 2 (constant +type-cons+)
(type $cons (struct
  (field car (mut anyref))   ;; field index 0
  (field cdr (mut anyref)))) ;; field index 1
```

### Fields

| Field | Index | Type | Mutable | Description |
|-------|-------|------|---------|-------------|
| car | 0 | anyref | Yes | First element of the pair |
| cdr | 1 | anyref | Yes | Second element of the pair |

### Invariants

1. **Non-null reference**: A cons cell is always a valid heap-allocated struct (never Wasm null)
2. **Any value**: Both car and cdr can hold any Lisp object (fixnum, cons, symbol, nil, etc.)
3. **Identity**: Each cons cell has unique identity; `(eq (cons 1 2) (cons 1 2))` is false

### Lifecycle

1. **Creation**: `struct.new 2` with car and cdr values on stack
2. **Access**: `struct.get 2 0` (car) or `struct.get 2 1` (cdr)
3. **Modification**: `struct.set 2 0` (rplaca) or `struct.set 2 1` (rplacd)
4. **Garbage Collection**: Managed by WasmGC; unreachable cons cells are automatically collected

## Entity: Proper List

### Definition

A proper list is a chain of cons cells where:
- Each cons cell's cdr points to another cons cell or NIL
- The final cdr is NIL (the empty list)

### Representation

```
(list 1 2 3) =

  +---+---+     +---+---+     +---+---+
  | 1 | ●-+---->| 2 | ●-+---->| 3 | ●-+----> NIL
  +---+---+     +---+---+     +---+---+
   car cdr       car cdr       car cdr
```

### Invariants

1. **Termination**: Proper lists always terminate with NIL
2. **Traversal**: `cdr` on each cons leads to next element or NIL
3. **Length**: Finite (no cycles in proper lists)

## Entity: NIL (Empty List)

### WasmGC Representation

Per constitution principle II, NIL is a singleton struct (NOT Wasm null):

```wat
;; Type index: 0 (constant +type-nil+)
(type $nil (struct
  (field tag i32)))  ;; distinguishing tag

;; Global singleton
(global $nil (ref 0) (struct.new 0 (i32.const 0)))
```

### Semantics

| Operation | Result | Notes |
|-----------|--------|-------|
| `(car nil)` | NIL | Returns NIL, not error |
| `(cdr nil)` | NIL | Returns NIL, not error |
| `(consp nil)` | NIL | NIL is not a cons |
| `(null nil)` | T | NIL is the null value |
| `(atom nil)` | T | NIL is an atom (not cons) |
| `(listp nil)` | T | NIL is the empty list |

### Identity

```lisp
(eq nil nil) => T  ;; singleton identity
(eq nil '()) => T  ;; '() is NIL
```

## Entity: T (True Symbol)

### Purpose

Predicate functions return T for true, NIL for false.

### WasmGC Representation

T is a symbol struct with special identity:

```wat
(global $t (ref 3) ...)  ;; Type 3 = symbol
```

### Usage in Predicates

```wat
;; Pattern for predicate return:
i32.const 1        ;; or result of ref.test/ref.eq
if (result anyref)
  global.get $t    ;; true case
else
  global.get $nil  ;; false case
end
```

## Type Relationships

```
                    anyref
                       |
         +-------------+-------------+
         |             |             |
     i31ref       (ref $cons)    (ref $nil)
    (fixnum)      (Type 2)       (Type 0)
         |             |             |
       +-+             |             +-- NIL singleton
       |          cons cells
    integers
```

## Operations Summary

### Constructors

| Operation | Input | Output | Wasm Instructions |
|-----------|-------|--------|-------------------|
| `cons` | x:anyref, y:anyref | (ref 2) | `struct.new 2` |
| `list` | args:anyref* | (ref 2) or nil | repeated `struct.new 2` |

### Accessors

| Operation | Input | Output | Wasm Instructions |
|-----------|-------|--------|-------------------|
| `car` | x:anyref | anyref | NIL check + `struct.get 2 0` |
| `cdr` | x:anyref | anyref | NIL check + `struct.get 2 1` |
| `first`-`tenth` | x:anyref | anyref | Composition of car/cdr |
| `rest` | x:anyref | anyref | Alias for cdr |
| `nth` | n:i32, x:anyref | anyref | Loop with cdr + car |
| `nthcdr` | n:i32, x:anyref | anyref | Loop with cdr |

### Predicates

| Operation | Input | Output | Wasm Instructions |
|-----------|-------|--------|-------------------|
| `consp` | x:anyref | T/NIL | `ref.test (ref 2)` |
| `null` | x:anyref | T/NIL | `ref.eq` with NIL |
| `atom` | x:anyref | T/NIL | NOT consp |
| `listp` | x:anyref | T/NIL | consp OR null |

### Mutators

| Operation | Input | Output | Wasm Instructions |
|-----------|-------|--------|-------------------|
| `rplaca` | cons:(ref 2), val:anyref | cons:(ref 2) | `struct.set 2 0` |
| `rplacd` | cons:(ref 2), val:anyref | cons:(ref 2) | `struct.set 2 1` |

## Memory Layout

All cons cells are allocated on the WasmGC heap. No linear memory is used.

### Allocation Pattern

```wat
;; Each cons allocation:
<car value>     ;; anyref
<cdr value>     ;; anyref
struct.new 2    ;; creates cons, pushes ref
```

### Reference Tracking

WasmGC automatically tracks references. When a cons cell becomes unreachable:
- GC identifies it as garbage
- Memory is reclaimed automatically
- No manual free/deallocation needed

## Error Conditions

| Condition | Trigger | Response |
|-----------|---------|----------|
| car/cdr on non-list | `(car 42)` | Type error (trap or signal) |
| rplaca on non-cons | `(rplaca nil x)` | Type error |
| rplacd on non-cons | `(rplacd nil x)` | Type error |
| nth negative index | `(nth -1 list)` | Undefined (may return NIL) |

## Compatibility Notes

### Common Lisp Compliance

- Cons cells are mutable (rplaca/rplacd)
- car/cdr of NIL returns NIL (not error)
- Lists are proper (terminate with NIL)
- Dotted pairs supported: `(cons 1 2)` creates `(1 . 2)`

### Differences from Standard CL

- No setf expansion for `(setf (car x) v)` (out of scope)
- No circular list detection
- No print representation (out of scope)
