# Contract: Cons Cell Operations

**Feature**: 006-cons-list-ops
**Date**: 2025-12-23

## Overview

This contract defines the interfaces for cons cell and list operations in the Clysm compiler.

## Compilation Functions

### compile-cons

**Signature**: `(compile-cons args env) -> instruction-list`

**Input**:
- `args`: List of two forms (car-form cdr-form)
- `env`: Compilation environment

**Output**: List of Wasm instructions that:
1. Evaluate car-form, leaving anyref on stack
2. Evaluate cdr-form, leaving anyref on stack
3. Create cons cell via `struct.new 2`

**Postcondition**: Stack has single `(ref 2)` (cons reference)

**Example**:
```lisp
(compile-cons '(1 2) env)
;; => ((:i32.const 1) (:ref.i31)
;;     (:i32.const 2) (:ref.i31)
;;     (:struct.new 2))
```

---

### compile-car

**Signature**: `(compile-car args env) -> instruction-list`

**Input**:
- `args`: List of one form (cons-or-nil-form)
- `env`: Compilation environment

**Output**: List of Wasm instructions that:
1. Evaluate form, leaving anyref on stack
2. If NIL, return NIL
3. If cons, return car field via `struct.get 2 0`
4. Otherwise, signal type error

**Postcondition**: Stack has single anyref (car value or NIL)

**NIL Behavior**: `(car nil)` returns NIL (not error)

---

### compile-cdr

**Signature**: `(compile-cdr args env) -> instruction-list`

**Input**: Same as compile-car

**Output**: List of Wasm instructions that:
1. Evaluate form, leaving anyref on stack
2. If NIL, return NIL
3. If cons, return cdr field via `struct.get 2 1`
4. Otherwise, signal type error

**Postcondition**: Stack has single anyref (cdr value or NIL)

**NIL Behavior**: `(cdr nil)` returns NIL (not error)

---

### compile-list

**Signature**: `(compile-list args env) -> instruction-list`

**Input**:
- `args`: List of zero or more forms
- `env`: Compilation environment

**Output**: List of Wasm instructions that:
1. If no args, push NIL
2. Otherwise, build cons chain right-to-left

**Postcondition**: Stack has single anyref (proper list or NIL)

**Special Cases**:
- `(list)` returns NIL
- `(list x)` returns `(cons x nil)`

---

### compile-consp

**Signature**: `(compile-consp args env) -> instruction-list`

**Input**:
- `args`: List of one form
- `env`: Compilation environment

**Output**: List of Wasm instructions that:
1. Evaluate form
2. Test if reference is cons type via `ref.test (ref 2)`
3. Return T if cons, NIL otherwise

**Postcondition**: Stack has T or NIL

**Semantics**:
- `(consp (cons 1 2))` => T
- `(consp nil)` => NIL
- `(consp 42)` => NIL

---

### compile-null

**Signature**: `(compile-null args env) -> instruction-list`

**Output**: List of Wasm instructions that:
1. Evaluate form
2. Compare with NIL singleton via `ref.eq`
3. Return T if equal, NIL otherwise

**Postcondition**: Stack has T or NIL

**Semantics**:
- `(null nil)` => T
- `(null '())` => T
- `(null (cons 1 2))` => NIL

---

### compile-atom

**Signature**: `(compile-atom args env) -> instruction-list`

**Output**: Instructions returning T if NOT a cons cell

**Semantics**:
- `(atom nil)` => T
- `(atom 42)` => T
- `(atom 'x)` => T
- `(atom (cons 1 2))` => NIL

---

### compile-listp

**Signature**: `(compile-listp args env) -> instruction-list`

**Output**: Instructions returning T if cons OR NIL

**Semantics**:
- `(listp nil)` => T
- `(listp (cons 1 2))` => T
- `(listp '(1 2 3))` => T
- `(listp 42)` => NIL

---

### compile-rplaca

**Signature**: `(compile-rplaca args env) -> instruction-list`

**Input**:
- `args`: List of two forms (cons-form new-car-form)
- `env`: Compilation environment

**Output**: List of Wasm instructions that:
1. Evaluate cons-form, cast to `(ref 2)`
2. Evaluate new-car-form
3. Set car field via `struct.set 2 0`
4. Return the cons (not the new value)

**Postcondition**: Stack has `(ref 2)` (the modified cons)

**Error**: Signals type error if cons-form is not a cons cell

---

### compile-rplacd

**Signature**: `(compile-rplacd args env) -> instruction-list`

**Input**: Same as compile-rplaca

**Output**: Same as compile-rplaca but sets cdr field (index 1)

---

### compile-nth

**Signature**: `(compile-nth args env) -> instruction-list`

**Input**:
- `args`: List of two forms (index-form list-form)
- `env`: Compilation environment

**Output**: Instructions that traverse list `index` times via cdr, then return car

**Semantics**: 0-indexed
- `(nth 0 '(a b c))` => A
- `(nth 2 '(a b c))` => C
- `(nth 5 '(a b c))` => NIL

---

### compile-nthcdr

**Signature**: `(compile-nthcdr args env) -> instruction-list`

**Output**: Instructions that return list after `n` cdr operations

**Semantics**:
- `(nthcdr 0 '(a b c))` => (A B C)
- `(nthcdr 2 '(a b c))` => (C)
- `(nthcdr 3 '(a b c))` => NIL

---

## Integration Contract

### Primitives Registration

All operations must be added to the primitives dispatch in `compile-primitive-call`:

```lisp
(case op
  ;; Cons operations
  (cons    (compile-cons args env))
  (car     (compile-car args env))
  (cdr     (compile-cdr args env))
  (list    (compile-list args env))
  ;; Predicates
  (consp   (compile-consp args env))
  (null    (compile-null args env))
  (atom    (compile-atom args env))
  (listp   (compile-listp args env))
  ;; Mutators
  (rplaca  (compile-rplaca args env))
  (rplacd  (compile-rplacd args env))
  ;; Accessors
  (first   (compile-car args env))
  (rest    (compile-cdr args env))
  (nth     (compile-nth args env))
  (nthcdr  (compile-nthcdr args env))
  ;; ... existing operations ...
  )
```

### Local Variable Requirements

Operations that need temporary storage must allocate locals via the env mechanism:

- `compile-car`: needs 1 temp local for NIL check
- `compile-cdr`: needs 1 temp local for NIL check
- `compile-list`: needs 1 temp local for accumulator
- `compile-nth`: needs 2 temp locals (counter, list-ptr)
- `compile-nthcdr`: needs 2 temp locals

### Global Dependencies

The following globals must exist in generated modules:

| Global | Type | Purpose |
|--------|------|---------|
| `$nil` | `(ref 0)` | NIL singleton for comparisons and returns |
| `$t` | `(ref 3)` | T symbol for predicate true returns |

## Test Contracts

### Unit Test Contract

Each compile-* function must have unit tests verifying:
1. Correct Wasm instructions are generated
2. Stack effects are correct
3. Local variable allocation works

### Integration Test Contract

Each operation must have integration tests verifying:
1. Compile → Execute → Correct result
2. NIL handling is correct
3. Error conditions are handled
