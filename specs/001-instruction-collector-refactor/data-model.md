# Data Model: Instruction Collector Refactor

**Date**: 2026-01-03
**Branch**: 001-instruction-collector-refactor

## Overview

This feature introduces a macro-based abstraction for instruction collection. The "data model" consists of the internal state managed by the macro and the instruction list format.

## Entities

### Instruction Collector (Runtime State)

**Description**: Lexically-scoped accumulator for Wasm instructions during code generation.

| Field | Type | Description |
|-------|------|-------------|
| accumulator | `list` | Reversed list of instructions (push-based) |

**Lifecycle**:
1. Created: `with-instruction-collector` form entered
2. Active: `emit`/`emit*` forms append instructions (via push)
3. Finalized: `with-instruction-collector` form exits, returns `nreverse` of accumulator

**State Transitions**:
```
[Empty] --emit--> [Accumulating] --exit--> [Finalized (returned)]
                        |
                   --emit*-->
                        |
                   (stays Accumulating)
```

### Wasm Instruction

**Description**: Single Wasm instruction in S-expression format.

| Field | Type | Description |
|-------|------|-------------|
| opcode | `keyword` | Wasm opcode (e.g., `:local.get`, `:i32.const`) |
| operands | `list` | Zero or more operands |

**Examples**:
```lisp
(:local.get 0)           ; Single operand
(:i32.const 42)          ; Single operand
(:struct.new 5)          ; Single operand
(:call_ref 3)            ; Single operand
(:block $label)          ; Symbol operand
:ref.is_null             ; No operands (bare keyword)
```

### Instruction List

**Description**: Ordered sequence of Wasm instructions.

| Property | Value |
|----------|-------|
| Type | `(list-of wasm-instruction)` |
| Ordering | Execution order (first = executed first) |
| Mutability | Immutable after collection finalized |

## Relationships

```
with-instruction-collector
         |
         | creates and owns
         v
  Instruction Collector
         |
         | accumulates via emit/emit*
         v
   Instruction List
         |
         | contains
         v
   Wasm Instructions
```

## Validation Rules

### VR-001: Instruction Format
Each instruction MUST be either:
- A bare keyword (e.g., `:ref.is_null`)
- A list starting with a keyword opcode (e.g., `(:local.get 0)`)

### VR-002: Accumulator Invariant
During collection, the accumulator contains instructions in **reverse** order.

### VR-003: Finalization Invariant
On exit, `nreverse` produces instructions in **forward** (execution) order.

### VR-004: Nesting Independence
Nested `with-instruction-collector` forms MUST have independent accumulators.

## Migration Impact

### Before (O(n²) pattern)
```lisp
(let ((result '()))
  (setf result (append result (compile-to-instructions arg1 env)))
  (setf result (append result '((:local.set 0))))
  (setf result (append result (compile-to-instructions arg2 env)))
  result)
```

### After (O(n) pattern)
```lisp
(with-instruction-collector
  (emit* (compile-to-instructions arg1 env))
  (emit :local.set 0)
  (emit* (compile-to-instructions arg2 env)))
```

**Data Model Unchanged**: The output instruction list format remains identical. Only the collection mechanism changes.
