# Data Model: Phase 13D - True Self-Hosting Achievement

**Feature**: 001-true-self-hosting
**Date**: 2025-12-28

## Overview

This document defines the data structures used by the Stage 0 interpreter for true self-hosting. All structures are WasmGC types following Constitution Principle I (WasmGC-First Type System).

---

## Core Types

### Fixnum

**Description**: 31-bit signed integer for numeric operations.

| Property | Value |
|----------|-------|
| WasmGC Type | `i31ref` |
| Type Index | N/A (built-in) |
| Range | -2^30 to 2^30-1 |

**Validation Rules**:
- Must fit in 31-bit signed range
- Arithmetic overflow behavior: wrap (Wasm semantics)

---

### Symbol

**Description**: Named identifier with optional value and function bindings.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| name | `(ref $string)` | immutable | Symbol's print name |
| value | `anyref` | mutable | Symbol's value cell |
| function | `anyref` | mutable | Function binding |
| plist | `anyref` | mutable | Property list (unused in bootstrap) |
| package | `anyref` | mutable | Package (unused in bootstrap) |

**Type Definition**:
```wat
(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))
```

**Type Index**: 1

**Identity Rules**:
- Symbols are interned by name (string equality)
- `(eq 'a 'a)` → T (same object)

---

### Cons

**Description**: Pair structure for building lists and trees.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| car | `anyref` | mutable | First element |
| cdr | `anyref` | mutable | Second element / rest of list |

**Type Definition**:
```wat
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))
```

**Type Index**: 0

**Validation Rules**:
- car and cdr can be any Lisp object (including NIL, other cons, fixnum, symbol)

---

### String

**Description**: UTF-8 encoded immutable string.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| bytes | `(ref $u8array)` | immutable | UTF-8 byte array |

**Type Definition**:
```wat
(type $string (struct
  (field $bytes (ref $u8array))))

(type $u8array (array (mut i8)))
```

**Type Index**: 2

---

### Closure

**Description**: Function object with captured environment.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| code_0 | `(ref null $func_0)` | immutable | 0-argument entry |
| code_1 | `(ref null $func_1)` | immutable | 1-argument entry |
| code_2 | `(ref null $func_2)` | immutable | 2-argument entry |
| code_N | `(ref null $func_N)` | immutable | N-argument entry |
| env | `anyref` | mutable | Captured environment |

**Type Definition**:
```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))))
```

**Type Index**: 3

---

## Global Variables

### NIL

**Description**: The null/empty list and boolean false.

| Property | Value |
|----------|-------|
| Global Index | 0 |
| Type | `(ref $symbol)` |
| Identity | Singleton, checked via `ref.eq` |

---

### UNBOUND

**Description**: Sentinel for unbound symbol values.

| Property | Value |
|----------|-------|
| Global Index | 1 |
| Type | `anyref` (opaque marker) |
| Usage | Internal only, not exposed to Lisp |

---

### mv-count

**Description**: Multiple value count (unused in bootstrap).

| Property | Value |
|----------|-------|
| Global Index | 2 |
| Type | `i32` |
| Initial | 1 (single value) |

---

### mv-buffer

**Description**: Multiple value storage (unused in bootstrap).

| Property | Value |
|----------|-------|
| Global Index | 3 |
| Type | `(ref $mv_array)` |

---

## Derived Structures

### Environment

**Description**: Lexical binding context for evaluation.

**Representation**: Association list of cons cells.

```
env = NIL
    | (cons (cons symbol value) env)
```

**Operations**:
| Operation | Input | Output | Description |
|-----------|-------|--------|-------------|
| extend | env, bindings | env' | Add bindings to front |
| lookup | env, symbol | value | Find binding, error if unbound |

---

### Function Body

**Description**: Compiled function representation.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol or NIL | Function name for export |
| params | list of symbols | Parameter names |
| body | form | Body expression |
| env | environment | Captured lexical env (for closures) |

---

### Wasm Module

**Description**: Output structure for compilation.

| Section | Contents |
|---------|----------|
| Type | All type definitions (indices 0-24) |
| Import | FFI imports (fs.*, parse_sexpr) |
| Function | Function type indices |
| Export | compile_form, compile_all, named defuns |
| Code | Function bodies as Wasm bytecode |

---

## Type Relationships

```
anyref (universal root)
├── i31ref (fixnum)
├── (ref $cons) ──→ car: anyref, cdr: anyref
├── (ref $symbol) ──→ name: (ref $string)
├── (ref $string) ──→ bytes: (ref $u8array)
├── (ref $closure) ──→ env: anyref, code_*: funcref
└── (ref null extern) (FFI boundary)
```

---

## State Transitions

### Compilation Pipeline

```
Input: S-expression string
  ↓
[Host Parse] → WasmGC AST (cons, symbol, fixnum)
  ↓
[Stage 0 Eval] → IR (function bodies)
  ↓
[Codegen] → Wasm binary sections
  ↓
Output: Valid .wasm file
```

### Bootstrap Stages

```
Stage 0 (SBCL-generated)
  │ compile_all(compiler-source)
  ↓
Stage 1 (Stage 0 output, >= 1KB)
  │ compile_all(compiler-source)
  ↓
Stage 2 (Stage 1 output, == Stage 1)
  │ [FIXED POINT ACHIEVED]
```
