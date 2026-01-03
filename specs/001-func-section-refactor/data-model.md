# Data Model: func-section.lisp Refactoring

**Date**: 2026-01-03
**Feature**: 001-func-section-refactor

## Overview

This document defines the internal data structures for the refactored primitive dispatch and type dispatch systems.

## Entities

### 1. Primitive Dispatch Table

**Purpose**: Map primitive operation symbols to their compiler functions

**Structure**:
```lisp
;; Primary table: symbol → compiler function
*primitive-compilers* : hash-table
  :test eq
  :key   symbol                    ; e.g., '+, 'CAR, 'CONS
  :value (function (list env) → list)  ; compiler function

;; Secondary table: name → compiler function (for %SETF-* ops)
*primitive-compilers-by-name* : hash-table
  :test equal
  :key   string                    ; e.g., "%SETF-AREF", "%SETF-CAR"
  :value (function (list env) → list)  ; compiler function
```

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| `register-primitive-compiler` | `(op compiler &key by-name) → nil` | Register a compiler function |
| `lookup-primitive-compiler` | `(op) → (or function null)` | Find compiler for operation |
| `unregister-primitive-compiler` | `(op &key by-name) → boolean` | Remove registration |
| `list-registered-primitives` | `() → list` | List all registered ops (debugging) |

**Identity Rule**: Operations identified by EQ (symbols) or EQUAL (strings for by-name)

**Lifecycle**:
1. **Init**: Tables created empty at load time
2. **Populate**: Bulk registration during compiler initialization
3. **Extend**: Additional registrations during development
4. **Query**: Lookup on each primitive call compilation

### 2. Runtime Function Table

**Purpose**: Map functions to Lisp runtime implementations (existing infrastructure)

**Structure** (extends existing `*runtime-function-table*`):
```lisp
*runtime-function-table* : hash-table
  :test eq
  :key   symbol                    ; e.g., 'STRING-TRIM, 'PARSE-INTEGER
  :value runtime-function-entry
    :arity         (or fixnum (cons fixnum fixnum))  ; exact or range
    :implementation symbol                           ; runtime function name
    :keyword-args  list                              ; supported keywords
```

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| `register-runtime-function` | `(name &key arity impl keywords) → nil` | Register runtime function |
| `compile-runtime-call` | `(name args env) → list` | Generate call to runtime |

### 3. Instruction Collector

**Purpose**: Efficiently accumulate Wasm instructions during compilation

**Structure** (macro-generated):
```lisp
;; Within with-instruction-collector scope:
<collector-var> : list (reversed)
  ; Instructions pushed in reverse order
  ; nreverse applied at scope exit
```

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| `emit` | `(instr) → nil` | Add single instruction |
| `emit*` | `(&rest instrs) → nil` | Add multiple instructions |

**Invariants**:
- Instructions within collector are in reverse order until finalization
- `nreverse` must be called exactly once at scope exit
- Generated output is identical to append-based approach

### 4. Type Dispatch Infrastructure

**Purpose**: Generate type-checking code for equality predicates

**Structure**:
```lisp
type-dispatch-table : alist
  ; Type tag → dispatch code generator
  ((type-tag . code-generator) ...)

equality-level : (member :eq :eql :equal :equalp)
  ; Determines which type comparisons to generate
```

**Type Tag Constants** (from existing WasmGC types):

| Tag | Type | eq? | eql? | equal? | equalp? |
|-----|------|-----|------|--------|---------|
| i31ref | Fixnum | ref.eq | i31.get_s compare | = | = |
| $cons | Cons | ref.eq | ref.eq | recursive | recursive |
| $symbol | Symbol | ref.eq | ref.eq | ref.eq | ref.eq |
| $string | String | ref.eq | ref.eq | char= loop | char-equal loop |
| $float | Float | ref.eq | f64.eq | f64.eq | f64.eq |
| $ratio | Ratio | ref.eq | num=, den= | num=, den= | = (coerced) |

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| `build-equality-type-dispatch` | `(level) → type-dispatch-table` | Build dispatch for level |
| `compile-type-dispatch` | `(value-local cases env) → list` | Generate dispatch code |

### 5. cXXr Compiler Registry

**Purpose**: Map cXXr function names to their car/cdr operation sequences

**Structure** (compile-time, not runtime):
```lisp
;; Generated via define-cxr-compiler macro:
compile-caar → (compile-cxr-chain '(:car :car) args env)
compile-cadr → (compile-cxr-chain '(:cdr :car) args env)
compile-cdar → (compile-cxr-chain '(:car :cdr) args env)
compile-cddr → (compile-cxr-chain '(:cdr :cdr) args env)
compile-caaar → (compile-cxr-chain '(:car :car :car) args env)
;; ... 12 total variants
```

**Operation Sequence Encoding**:
- `:car` → `struct.get $cons 0` (car field)
- `:cdr` → `struct.get $cons 1` (cdr field)
- Sequences applied right-to-left: `(:cdr :car)` = car of cdr

## Relationships

```
┌─────────────────────────┐
│ compile-primitive-call  │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐     ┌──────────────────────────┐
│ *primitive-compilers*   │────▶│ Individual compile-*     │
│ (hash-table :test eq)   │     │ functions                │
└─────────────────────────┘     └──────────────────────────┘
            │
            │ fallback
            ▼
┌─────────────────────────┐     ┌──────────────────────────┐
│ *runtime-function-table*│────▶│ lib/*-runtime.lisp       │
│ (hash-table :test eq)   │     │ implementations          │
└─────────────────────────┘     └──────────────────────────┘
```

## Validation Rules

1. **Primitive Registration**: No duplicate symbols unless intentional override
2. **Runtime Function**: Arity must match implementation signature
3. **Instruction Collector**: Must call nreverse before returning
4. **Type Dispatch**: All WasmGC types must have handlers at each equality level
5. **cXXr Operations**: Sequence length must be 2-4 (caar through cddddr)

## State Transitions

### Primitive Compiler Lifecycle

```
[Unregistered] ──register──▶ [Registered] ──override──▶ [Updated]
                                   │
                                   ▼
                             [Available for dispatch]
```

### Runtime Function Lifecycle

```
[Inline Wasm] ──migrate──▶ [Runtime Lisp]
     │                          │
     │                          ▼
     │                    [Simpler codegen]
     │                          │
     └──────────────────────────┘
           (same semantics)
```
