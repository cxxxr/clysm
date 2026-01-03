# Quickstart: Instruction Collector

**Date**: 2026-01-03
**Branch**: 001-instruction-collector-refactor

## Overview

The `with-instruction-collector` macro provides an O(n) mechanism for collecting Wasm instructions during code generation, replacing the O(n²) append-based pattern.

## Basic Usage

### Before (Legacy Pattern)

```lisp
(defun compile-something (form env)
  (let ((result '()))
    (setf result (append result (compile-to-instructions (first form) env)))
    (setf result (append result '((:local.set 0))))
    (setf result (append result (compile-to-instructions (second form) env)))
    (setf result (append result '((:local.set 1))))
    result))
```

### After (Collector Pattern)

```lisp
(defun compile-something (form env)
  (with-instruction-collector
    (emit* (compile-to-instructions (first form) env))
    (emit :local.set 0)
    (emit* (compile-to-instructions (second form) env))
    (emit :local.set 1)))
```

## API Reference

### `with-instruction-collector`

Establishes a collection context and returns the collected instructions.

```lisp
(with-instruction-collector
  ;; body forms using emit and emit*
  )
=> list-of-instructions
```

### `emit`

Adds a single instruction. The first argument is the opcode, remaining arguments are operands.

```lisp
(emit :local.get 0)           ; Opcode + operand
(emit :i32.const 42)          ; Opcode + operand
(emit :ref.is_null)           ; Opcode only (no operands)
```

### `emit*`

Adds multiple instructions from a list.

```lisp
(emit* '((:local.get 0) (:local.get 1)))
(emit* (compile-to-instructions expr env))
(emit* nil)  ; No-op
```

## Migration Guide

### Step 1: Identify the Pattern

Look for:
```lisp
(let ((result '()))
  (setf result (append result ...))
  ...
  result)
```

### Step 2: Wrap with Collector

Replace `let` binding with `with-instruction-collector`:
```lisp
(with-instruction-collector
  ...)
```

### Step 3: Replace Appends

| Old Pattern | New Pattern |
|-------------|-------------|
| `(setf result (append result '((...))))` | `(emit ...)` |
| `(setf result (append result (list ...)))` | `(emit ...)` |
| `(setf result (append result <expr>))` | `(emit* <expr>)` |

### Step 4: Remove Return

The `with-instruction-collector` form automatically returns the collected instructions. Remove explicit `result` return.

## Conditional Emission

```lisp
(with-instruction-collector
  (emit :block $start)
  (when condition
    (emit :i32.const 1)
    (emit :br $done))
  (emit :end))
```

## Nested Collectors

Each collector is independent:

```lisp
(with-instruction-collector
  (emit :block $outer)
  (let ((inner-instrs (with-instruction-collector
                        (emit :i32.const 42))))
    (emit* inner-instrs))
  (emit :end))
```

## Common Patterns

### Loop Over Arguments

```lisp
;; Before
(let ((result '()))
  (dolist (arg args)
    (setf result (append result (compile-to-instructions arg env))))
  result)

;; After
(with-instruction-collector
  (dolist (arg args)
    (emit* (compile-to-instructions arg env))))
```

### Conditional Blocks

```lisp
;; Before
(let ((result '()))
  (setf result (append result (compile-to-instructions test-form env)))
  (setf result (append result '((:if))))
  (setf result (append result (compile-to-instructions then-form env)))
  (setf result (append result '((:else))))
  (setf result (append result (compile-to-instructions else-form env)))
  (setf result (append result '((:end))))
  result)

;; After
(with-instruction-collector
  (emit* (compile-to-instructions test-form env))
  (emit :if)
  (emit* (compile-to-instructions then-form env))
  (emit :else)
  (emit* (compile-to-instructions else-form env))
  (emit :end))
```

## Testing Your Migration

After migrating a function:

1. Run unit tests: `sbcl --eval "(asdf:test-system :clysm)"`
2. Verify Wasm output: Compare before/after using `wasm-tools print`
3. Check Stage 1: `sbcl --load build/stage1-complete.lisp`

## Troubleshooting

### Error: "emit called outside with-instruction-collector"

Ensure `emit`/`emit*` is lexically inside a `with-instruction-collector` form.

### Instructions in Wrong Order

Check that you're using `emit*` for lists and `emit` for single instructions.

### Missing Instructions

Verify all `append` calls were converted. Use grep:
```bash
grep -n "append.*result" func-section.lisp
```
