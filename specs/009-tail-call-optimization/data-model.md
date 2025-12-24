# Data Model: Tail Call Optimization

**Date**: 2025-12-24
**Feature**: 009-tail-call-optimization

## Overview

TCO implementation requires minimal data model changes. The primary change is adding a tail-position context flag to the compilation environment.

## Entities

### Compilation Environment Extension

**Entity**: `cenv` (Compilation Environment)

**New Field**:
| Field | Type | Description |
|-------|------|-------------|
| `tail-position-p` | boolean | True when compiling in tail position |

**Lifecycle**:
- Created as `t` when entering function body
- Set to `nil` when compiling non-tail sub-expressions (e.g., call arguments)
- Preserved/propagated when compiling tail-inheriting forms (progn last, if branches)

### Instruction Type Extension

**Entity**: Wasm Instruction

**New Instructions**:
| Instruction | Opcode | Format | Description |
|-------------|--------|--------|-------------|
| `(:return_call idx)` | 0x12 | `[op][funcidx:u32]` | Tail call to function |
| `(:return_call_ref typeidx)` | 0x15 | `[op][typeidx:u32]` | Tail call through reference |

## State Transitions

### Form-Specific Propagation

| Form Type | Child Forms | Tail Propagation |
|-----------|-------------|------------------|
| `defun` body | `(body*)` | Last form inherits |
| `lambda` body | `(body*)` | Last form inherits |
| `progn` | `(e1 ... en)` | Only `en` inherits |
| `if` | `(test then else)` | `then` and `else` inherit; `test` never |
| `let` | `(bindings body*)` | Last body form inherits; bindings never |
| `let*` | `(bindings body*)` | Last body form inherits; bindings never |
| `block` | `(name body*)` | Last form inherits |
| `call` | `(fn args*)` | None inherit (args are not tail) |
| `funcall` | `(fn args*)` | None inherit (args are not tail) |

## Validation Rules

### Tail Call Generation Conditions

A `return_call` is generated when ALL of:
1. Current expression is a function call (`ast-call`)
2. `tail-position-p` is true in compilation environment
3. Target is a known function (has function index)
4. Not inside `catch` or `unwind-protect` cleanup

A `return_call_ref` is generated when ALL of:
1. Current expression is `funcall` or local function call
2. `tail-position-p` is true in compilation environment
3. Closure reference is on stack
4. Not inside `catch` or `unwind-protect` cleanup

### Type Compatibility

For tail calls, the callee's return type must match the caller's return type:
- All clysm functions return `anyref`
- This constraint is always satisfied in the current design

## Data Volume / Scale

- **Impact**: ~1 additional boolean per compilation environment instance
- **Memory**: Negligible (cenv is stack-allocated in practice)
- **Complexity**: O(1) check per function call compilation
