# Data Model: Special Variables Compiler Integration

**Date**: 2025-12-22
**Feature**: 002-special-vars-compiler

## Overview

This document defines the data structures for special variable support at both compile-time (AST, registry) and runtime (binding frames, symbol structure).

---

## Compile-Time Entities

### AST Node: ast-defvar

Represents a `defvar` declaration in the abstract syntax tree.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `symbol` | The variable name being declared |
| `init-form` | `ast-node or null` | Initial value expression (optional) |
| `docstring` | `string or null` | Documentation string (optional) |
| `source-location` | `source-location` | Source code location for errors |

**Validation Rules**:
- `name` must be a symbol
- `init-form` if present must be a valid AST node
- After parsing, `name` is registered in special variable registry

**State Transitions**: N/A (AST nodes are immutable after parsing)

---

### AST Node: ast-defparameter

Represents a `defparameter` declaration in the abstract syntax tree.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `symbol` | The variable name being declared |
| `init-form` | `ast-node` | Initial value expression (required) |
| `docstring` | `string or null` | Documentation string (optional) |
| `source-location` | `source-location` | Source code location for errors |

**Validation Rules**:
- `name` must be a symbol
- `init-form` is **required** (unlike defvar)
- After parsing, `name` is registered in special variable registry

**State Transitions**: N/A (AST nodes are immutable after parsing)

---

### Special Variable Registry

Compile-time tracking of declared special variables.

| Field | Type | Description |
|-------|------|-------------|
| `variables` | `hash-table<symbol, special-info>` | Map of symbol to special info |

**special-info Structure**:

| Field | Type | Description |
|-------|------|-------------|
| `declared-p` | `boolean` | Whether variable was declared via defvar/defparameter |
| `has-init-form` | `boolean` | Whether an init-form was provided |
| `source-location` | `source-location` | Where it was declared |

**Operations**:
- `register-special(name)` - Mark symbol as special
- `special-p(name)` - Check if symbol is declared special
- `clear-registry()` - Reset for new compilation unit

**Uniqueness Rules**:
- Each symbol appears at most once in the registry
- Re-declaration (multiple defvar for same symbol) is allowed but not recorded twice

---

## Runtime Entities (WasmGC)

### Binding Frame ($binding_frame)

Runtime structure for tracking dynamic bindings during execution.

| Field | WasmGC Type | Description |
|-------|-------------|-------------|
| `$symbol` | `(ref $symbol)` | The bound symbol |
| `$old_value` | `anyref` | Previous value before binding |
| `$prev` | `(ref null $binding_frame)` | Link to previous frame |

**WAT Definition**:
```wat
(type $binding_frame (struct
  (field $symbol (ref $symbol))
  (field $old_value anyref)
  (field $prev (ref null $binding_frame))))
```

**Lifecycle**:
1. **Created**: On dynamic binding entry (`let` with special variable)
2. **Active**: During execution of binding scope
3. **Consumed**: On scope exit (normal or exceptional), frame is popped

**State Transitions**:
```
[None] --push--> [Active on stack] --pop--> [Garbage collected]
```

---

### Binding Stack (Global)

Global variable tracking the current binding context.

| Field | WasmGC Type | Description |
|-------|-------------|-------------|
| `$binding_stack` | `(mut (ref null $binding_frame))` | Top of binding stack |

**WAT Definition**:
```wat
(global $binding_stack (mut (ref null $binding_frame)) (ref.null $binding_frame))
```

**Operations**:
- **Push**: Create new frame, link to current top, update global
- **Pop**: Get frame's prev, update global, return old value

---

### Symbol Structure (Existing - Extended Usage)

Existing `$symbol` structure from `gc-types.lisp`, used for special variable storage.

| Field | WasmGC Type | Description |
|-------|-------------|-------------|
| `$name` | `anyref` | Symbol name (string ref) |
| `$value` | `(mut anyref)` | **Current value (special variable storage)** |
| `$function` | `(mut anyref)` | Function definition |
| `$plist` | `(mut anyref)` | Property list |

**WAT Definition** (existing in codebase):
```wat
(type $symbol (struct
  (field $name anyref)
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))))
```

**Special Variable Usage**:
- `$value` field stores the current dynamic value
- Field index 1 used for `struct.get`/`struct.set`
- Initial value is `UNBOUND` sentinel until defined

---

### UNBOUND Sentinel (Existing)

Singleton object representing unbound state.

| Field | WasmGC Type | Description |
|-------|-------------|-------------|
| `$tag` | `i32` | Type tag for identification |

**WAT Definition** (existing):
```wat
(type $unbound (struct (field $tag i32)))
(global $UNBOUND (ref $unbound) (struct.new $unbound (i32.const 1)))
```

**Usage**:
- Initial value of symbol's `$value` field
- Checked on variable access to detect unbound variables
- Comparison via `ref.eq`

---

## Entity Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                    COMPILE TIME                              │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│   ast-defvar/ast-defparameter                               │
│            │                                                 │
│            │ registers                                       │
│            ▼                                                 │
│   Special Variable Registry                                  │
│            │                                                 │
│            │ informs                                         │
│            ▼                                                 │
│   Code Generation (compile-var-ref, compile-let)            │
│                                                              │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ generates
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     RUNTIME (WasmGC)                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│   $binding_stack (global)                                    │
│        │                                                     │
│        │ points to                                           │
│        ▼                                                     │
│   $binding_frame ──prev──► $binding_frame ──prev──► null    │
│        │                        │                            │
│        │ references             │ references                 │
│        ▼                        ▼                            │
│   $symbol                  $symbol                           │
│   ├─ $name                 ├─ $name                          │
│   ├─ $value ◄──────────────┼─ $value                         │
│   ├─ $function             ├─ $function                      │
│   └─ $plist                └─ $plist                         │
│                                                              │
│   $UNBOUND (sentinel for unbound check)                      │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Validation Summary

| Entity | Validation Rule | Error If Violated |
|--------|-----------------|-------------------|
| ast-defvar | `name` is symbol | Parse error |
| ast-defparameter | `init-form` required | Parse error |
| Special Registry | Symbol not nil | Internal error |
| $binding_frame | `$symbol` is valid ref | Runtime trap |
| $symbol.$value | N/A (any value valid) | - |
| Unbound access | `$value != UNBOUND` | `unbound-variable` condition |
