# Data Model: Sequence Runtime Migration

**Branch**: `001-sequence-runtime-migration`
**Date**: 2026-01-01

## Overview

This feature does not introduce new persistent data structures. The "data model" describes the runtime function dispatch mechanism and function signatures used for sequence operations.

## Runtime Function Table Entry

```lisp
;; Structure: symbol -> (runtime-name . arity-or-nil)
;; Example entry in *runtime-function-table*:
'remove -> (:$remove-rt . nil)  ; nil = variadic (keyword args)
```

| Field | Type | Description |
|-------|------|-------------|
| `symbol` | `symbol` | CL function name (e.g., `'remove`) |
| `runtime-name` | `keyword` | Wasm function name (e.g., `:$remove-rt`) |
| `arity` | `(or fixnum null)` | Expected arg count; `nil` for variadic |

## Runtime Function Signatures

### Remove Family

```lisp
;; remove-rt: Remove matching elements (non-destructive)
(defun remove-rt (item list test key start end count from-end)
  ...)

;; remove-if-rt: Remove elements satisfying predicate
(defun remove-if-rt (predicate list key start end count from-end)
  ...)

;; remove-if-not-rt: Keep only elements satisfying predicate
(defun remove-if-not-rt (predicate list key start end count from-end)
  ...)
```

### Delete Family

```lisp
;; delete-rt: Remove matching elements (destructive)
(defun delete-rt (item list test key start end count from-end)
  ...)

;; delete-if-rt: Remove elements satisfying predicate (destructive)
(defun delete-if-rt (predicate list key start end count from-end)
  ...)

;; delete-if-not-rt: Keep only elements satisfying predicate (destructive)
(defun delete-if-not-rt (predicate list key start end count from-end)
  ...)
```

### Count Family

```lisp
;; count-rt: Count matching elements
(defun count-rt (item list test key start end from-end)
  ...)

;; count-if-rt: Count elements satisfying predicate
(defun count-if-rt (predicate list key start end from-end)
  ...)

;; count-if-not-rt: Count elements not satisfying predicate
(defun count-if-not-rt (predicate list key start end from-end)
  ...)
```

### Substitute Family

```lisp
;; substitute-rt: Replace matching elements (non-destructive)
(defun substitute-rt (newitem olditem list test key start end count from-end)
  ...)

;; substitute-if-rt: Replace elements satisfying predicate
(defun substitute-if-rt (newitem predicate list key start end count from-end)
  ...)

;; substitute-if-not-rt: Replace elements not satisfying predicate
(defun substitute-if-not-rt (newitem predicate list key start end count from-end)
  ...)
```

## Parameter Mapping

| ANSI CL Keyword | Runtime Parameter | Default Value | Notes |
|-----------------|-------------------|---------------|-------|
| `:test` | `test` | `nil` → `#'eql` | Comparison function |
| `:test-not` | - | N/A | Deprecated; compiler transforms to `(complement test)` |
| `:key` | `key` | `nil` | Key extraction function |
| `:start` | `start` | `0` | Start index (0-based) |
| `:end` | `end` | `nil` | End index (nil = end of list) |
| `:count` | `count` | `nil` | Max operations (nil = unlimited) |
| `:from-end` | `from-end` | `nil` | Process from end of list |

## Compile-Time Transformation

The compiler transforms ANSI CL calls to runtime calls:

```lisp
;; Source:
(remove 3 mylist :test #'= :start 2)

;; Compiles to (conceptually):
(remove-rt 3 mylist #'= nil 2 nil nil nil)
;;          ^item ^list ^test ^key ^start ^end ^count ^from-end
```

## Registration in func-section.lisp

```lisp
;; Add to initialization section:
(register-runtime-function 'remove :$remove-rt nil)
(register-runtime-function 'remove-if :$remove-if-rt nil)
(register-runtime-function 'remove-if-not :$remove-if-not-rt nil)
(register-runtime-function 'count :$count-rt nil)
(register-runtime-function 'count-if :$count-if-rt nil)
(register-runtime-function 'count-if-not :$count-if-not-rt nil)
(register-runtime-function 'substitute :$substitute-rt nil)
(register-runtime-function 'substitute-if :$substitute-if-rt nil)
(register-runtime-function 'substitute-if-not :$substitute-if-not-rt nil)
(register-runtime-function 'delete :$delete-rt nil)
(register-runtime-function 'delete-if :$delete-if-rt nil)
(register-runtime-function 'delete-if-not :$delete-if-not-rt nil)
```

## Layer 1 Primitives Used

| Primitive | Usage |
|-----------|-------|
| `car` | Access list element |
| `cdr` | Traverse list |
| `cons` | Build result list |
| `consp` | Type check for cons cell |
| `null` | Check for list end |
| `funcall` | Apply `:test` and `:key` functions |
| `eq` | Default equality (symbols) |
| `eql` | Default equality (numbers) |
| `rplaca` | Destructive modification (delete family) |
| `rplacd` | Destructive modification (delete family) |
| `nreverse` | Reverse result list |

## State Transitions

N/A - Runtime functions are pure (or destructive for delete family) with no persistent state.

## Validation Rules

1. **Type Constraints**:
   - `list` must be a proper list (enforced by `consp` checks)
   - `test` and `key` must be functions (or nil)
   - `start` and `end` must be non-negative integers (or nil for end)
   - `count` must be a non-negative integer (or nil)

2. **Error Conditions** (per ANSI CL):
   - `:start` > list length → signal error
   - `:end` < `:start` → signal error
   - `:start` or `:end` negative → signal error

3. **Behavior Guarantees**:
   - Non-destructive functions never modify input list
   - Delete functions may modify input list (implementation choice)
   - Result order preserved (unless `:from-end`)
