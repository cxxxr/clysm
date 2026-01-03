# Contract: Instruction Collector Macro Interface

**Date**: 2026-01-03
**Type**: Internal Macro API

## Overview

Defines the contract for the `with-instruction-collector` macro and its local `emit`/`emit*` forms.

## Macro: `with-instruction-collector`

### Signature

```lisp
(with-instruction-collector &body body) => instruction-list
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| body | forms | Zero or more Lisp forms to execute |

### Return Value

| Type | Description |
|------|-------------|
| `list` | List of Wasm instructions in execution order |

### Contract

1. **C-001**: Returns empty list when body is empty
2. **C-002**: Returns instructions in order they were emitted
3. **C-003**: Does not evaluate body more than once
4. **C-004**: Establishes lexical scope for `emit` and `emit*`
5. **C-005**: Nested calls create independent collectors

### Examples

```lisp
;; Empty body
(with-instruction-collector)
=> NIL

;; Single instruction
(with-instruction-collector
  (emit :local.get 0))
=> ((:LOCAL.GET 0))

;; Multiple instructions
(with-instruction-collector
  (emit :i32.const 42)
  (emit :local.set 0))
=> ((:I32.CONST 42) (:LOCAL.SET 0))

;; Nested collectors
(with-instruction-collector
  (emit :block $outer)
  (emit* (with-instruction-collector
           (emit :i32.const 1)))
  (emit :end))
=> ((:BLOCK $OUTER) (:I32.CONST 1) (:END))
```

---

## Local Macro: `emit`

### Signature

```lisp
(emit &rest instruction-parts) => unspecified
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| instruction-parts | forms | Opcode keyword followed by operands |

### Contract

1. **C-101**: Adds single instruction to enclosing collector
2. **C-102**: Instruction parts are evaluated left-to-right
3. **C-103**: Must be called within `with-instruction-collector` body
4. **C-104**: Signals error if called outside collector

### Examples

```lisp
(emit :local.get 0)           ; => adds (:LOCAL.GET 0)
(emit :i32.const 42)          ; => adds (:I32.CONST 42)
(emit :struct.new type-idx)   ; => adds (:STRUCT.NEW <value-of-type-idx>)
```

---

## Local Macro: `emit*`

### Signature

```lisp
(emit* instruction-list) => unspecified
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| instruction-list | form | Form evaluating to a list of instructions |

### Contract

1. **C-201**: Adds all instructions from list to enclosing collector
2. **C-202**: Preserves order of instructions in input list
3. **C-203**: Empty list is a no-op
4. **C-204**: Must be called within `with-instruction-collector` body
5. **C-205**: Signals error if called outside collector

### Examples

```lisp
(emit* '((:local.get 0) (:local.get 1)))
; => adds (:LOCAL.GET 0), then (:LOCAL.GET 1)

(emit* (compile-to-instructions expr env))
; => adds all instructions returned by compile-to-instructions

(emit* nil)
; => no-op, adds nothing
```

---

## Error Conditions

### E-001: Emit Outside Collector

**Condition**: `emit` or `emit*` called outside `with-instruction-collector`
**Signal**: `error` with message indicating macro misuse
**Recovery**: None (programming error)

---

## Performance Contract

| Operation | Time Complexity |
|-----------|-----------------|
| `emit` (single instruction) | O(1) |
| `emit*` (n instructions) | O(n) |
| `with-instruction-collector` finalization | O(n) where n = total instructions |
| Total for collecting n instructions | O(n) |

---

## Compatibility Contract

### CC-001: Output Format Unchanged

The instruction list format returned by `with-instruction-collector` MUST be identical to the format previously produced by append-based collection.

### CC-002: Semantic Equivalence

For any function migrated from append-based to collector-based:
- Given identical inputs, the output instruction list MUST be identical
- Wasm bytecode generated from the instruction list MUST be byte-identical
