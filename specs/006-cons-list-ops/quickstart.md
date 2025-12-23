# Quickstart: Cons Cell and List Operations

**Feature**: 006-cons-list-ops
**Date**: 2025-12-23

## Overview

This feature adds cons cell and list operations to the Clysm compiler. After implementation, you can:

```lisp
;; Create cons cells
(cons 1 2)          ;; => (1 . 2)

;; Access elements
(car (cons 1 2))    ;; => 1
(cdr (cons 1 2))    ;; => 2

;; Build lists
(list 1 2 3)        ;; => (1 2 3)

;; Type predicates
(consp (cons 1 2))  ;; => T
(null nil)          ;; => T
(atom 42)           ;; => T
(listp '(1 2))      ;; => T

;; Destructive modification
(rplaca x 10)       ;; modify car
(rplacd x 20)       ;; modify cdr
```

## Quick Test

After implementing Phase 1 (MVP), test with:

```bash
cd /home/user/src/clysm-workbench/clysm3
nix develop

# Run specific test
rove tests/integration/list-test.lisp

# Or run all tests
nix flake check
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add compile-cons, compile-car, compile-cdr |
| `src/clysm/compiler/codegen/gc-types.lisp` | Existing $cons type (Type 2) - read only |
| `tests/unit/cons-test.lisp` | Unit tests for Wasm instruction generation |
| `tests/integration/list-test.lisp` | Integration tests for list operations |

## Implementation Steps

### Step 1: Add cons to primitives

In `func-section.lisp`, extend `compile-primitive-call`:

```lisp
(defun compile-primitive-call (op args env)
  (case op
    ;; ... existing cases ...
    (cons (compile-cons args env))
    (car  (compile-car args env))
    (cdr  (compile-cdr args env))
    ;; ... more cases ...
    ))
```

### Step 2: Implement compile-cons

```lisp
(defun compile-cons (args env)
  "Compile (cons x y) to struct.new 2"
  (let ((car-form (first args))
        (cdr-form (second args))
        (result '()))
    (setf result (compile-to-instructions car-form env))
    (setf result (append result (compile-to-instructions cdr-form env)))
    (setf result (append result '((:struct.new 2))))
    result))
```

### Step 3: Implement compile-car with NIL handling

```lisp
(defun compile-car (args env)
  "Compile (car x) with NIL check"
  (let ((result (compile-to-instructions (first args) env)))
    (append result
      '((:local.tee $tmp)
        (:global.get $nil)
        (:ref.eq)
        (:if (:result :anyref)
          ((:global.get $nil))
          ((:local.get $tmp)
           (:ref.cast (:ref 2))
           (:struct.get 2 0)))))))
```

## Wasm Instructions Reference

| Operation | Wasm Instruction | Notes |
|-----------|------------------|-------|
| Create cons | `struct.new 2` | Stack: [car, cdr] → [cons-ref] |
| Get car | `struct.get 2 0` | Stack: [cons-ref] → [anyref] |
| Get cdr | `struct.get 2 1` | Stack: [cons-ref] → [anyref] |
| Set car | `struct.set 2 0` | Stack: [cons-ref, value] → [] |
| Set cdr | `struct.set 2 1` | Stack: [cons-ref, value] → [] |
| Type test | `ref.test (ref 2)` | Stack: [anyref] → [i32] |
| Type cast | `ref.cast (ref 2)` | Stack: [anyref] → [cons-ref] |

## TDD Workflow

1. Write test first (must fail)
2. Run test: `rove tests/unit/cons-test.lisp`
3. Implement minimum code to pass
4. Run test again (must pass)
5. Commit

## Common Issues

### NIL global not found

Ensure the module exports `$nil` global. Check `module.lisp` for global declarations.

### Type cast trap

Always check `ref.test` before `ref.cast` to avoid runtime traps on wrong types.

### Stack ordering

`struct.new` expects `[field0, field1, ...]` on stack (car first, cdr second for cons).

## Next Steps

After completing this feature:
- `append`, `reverse`, `mapcar` (sequence functions)
- `dotted list reader syntax`
- `setf` expansion for car/cdr
