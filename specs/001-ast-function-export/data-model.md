# Data Model: AST Function Export System

**Feature**: 001-ast-function-export
**Date**: 2026-01-01

## Entities

### Runtime Function Table Entry

Represents a function registered for Wasm dispatch.

| Field | Type | Description |
|-------|------|-------------|
| symbol | symbol | The Lisp function symbol (e.g., `clysm:compile-to-instructions`) |
| runtime-name | keyword | The Wasm function name (e.g., `:$compile-to-instructions-rt`) |
| arity | (or fixnum null) | Argument count, or nil for variadic functions |

**Storage**: Hash table `*runtime-function-table*` keyed by symbol

**Entry Format**: `(cons runtime-name arity)`

### Registered Functions

| Symbol | Runtime Name | Arity | Category |
|--------|-------------|-------|----------|
| `clysm:compile-to-instructions` | `:$compile-to-instructions-rt` | 2 | Compilation |
| `clysm:make-wasm-struct-type` | `:$make-wasm-struct-type-rt` | nil | GC Types |
| `clysm:wasm-struct-type-p` | `:$wasm-struct-type-p-rt` | 1 | GC Types |
| `clysm:wasm-struct-type-fields` | `:$wasm-struct-type-fields-rt` | 1 | GC Types |
| `clysm:make-ast-literal` | `:$make-ast-literal-rt` | nil | AST |
| `clysm:ast-literal-value` | `:$ast-literal-value-rt` | 1 | AST |
| `clysm:ast-literal-p` | `:$ast-literal-p-rt` | 1 | AST |
| `clysm:get-numeric-value` | `:$get-numeric-value-rt` | 1 | AST |

## Relationships

```
*runtime-function-table* (hash-table)
    │
    ├── clysm:compile-to-instructions → (:$compile-to-instructions-rt . 2)
    ├── clysm:make-wasm-struct-type → (:$make-wasm-struct-type-rt . nil)
    ├── clysm:wasm-struct-type-p → (:$wasm-struct-type-p-rt . 1)
    ├── clysm:wasm-struct-type-fields → (:$wasm-struct-type-fields-rt . 1)
    ├── clysm:make-ast-literal → (:$make-ast-literal-rt . nil)
    ├── clysm:ast-literal-value → (:$ast-literal-value-rt . 1)
    ├── clysm:ast-literal-p → (:$ast-literal-p-rt . 1)
    └── clysm:get-numeric-value → (:$get-numeric-value-rt . 1)
```

## Validation Rules

1. **Uniqueness**: Each symbol may only be registered once
2. **Arity constraint**: If arity is non-nil, calls must have exactly that many arguments
3. **Symbol package**: All symbols must be in the `clysm` package
4. **Runtime name format**: Must be keyword starting with `$` and ending with `-rt`

## State Transitions

Registration is one-time at module load. No state transitions during runtime.

```
Module Load → register-ast-runtime-functions() → Functions registered → Compilation uses dispatch
```
