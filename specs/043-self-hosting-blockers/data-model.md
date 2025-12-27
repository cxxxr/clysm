# Data Model: Self-Hosting Blockers Resolution

**Feature**: 043-self-hosting-blockers
**Date**: 2025-12-28

## Overview

This document defines the data structures for implementing self-hosting blockers resolution. All types are designed for WasmGC compatibility per Constitution Principle I.

---

## 1. Parameter Info (AST Extension)

### ast-param-info

Represents a parsed parameter from a lambda-list with optional default value.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Parameter variable name |
| kind | keyword | :required, :optional, :key, :rest, :aux |
| default-form | ast-node | Default value expression (nil if none) |
| supplied-p | symbol | Variable for presence detection (nil if none) |
| keyword | symbol | Keyword name for &key params (nil for others) |

**Constraints**:
- `name` must be a valid symbol
- `kind` must be one of the allowed keywords
- `supplied-p` only valid for :optional and :key
- `keyword` only valid for :key

**State Transitions**: N/A (immutable AST node)

---

## 2. Hash Table (WasmGC)

### hash-table-entry

A single entry in the hash table bucket chain.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| key | anyref | immutable | The key object |
| value | anyref | mutable | The associated value |
| next | (ref null $hash-entry) | mutable | Next entry in chain (collision handling) |

### hash-table

The hash table structure.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| test | i32 | immutable | Test function: 0=eq, 1=eql, 2=equal, 3=equalp |
| count | i32 | mutable | Number of entries |
| size | i32 | immutable | Number of buckets |
| buckets | (ref $bucket-array) | immutable | Array of bucket heads |

### bucket-array

Array of bucket head pointers.

| Type | Element Type | Description |
|------|--------------|-------------|
| array | (ref null $hash-entry) | Each bucket is a chain head |

**Identity**: Hash tables are reference-equal (same memory location).

**Lifecycle**:
1. Created empty via `make-hash-table`
2. Modified via `(setf gethash)`, `remhash`
3. No explicit deletion (GC handles cleanup)

---

## 3. Loop Clause Structures (Existing)

Already defined in `lib/macros.lisp`:

### loop-iter-clause (abstract base)
| Field | Type | Description |
|-------|------|-------------|
| var | symbol | Iteration variable |
| clause-type | keyword | :in, :on, :across, :equals, :arithmetic, :hash-keys, :hash-values |

### loop-iter-arithmetic
| Field | Type | Description |
|-------|------|-------------|
| from | form | Start value |
| to/below/above | form | End value |
| by | form | Step value |
| upfrom/downfrom | form | Alternative start with direction |

### loop-accumulation-clause
| Field | Type | Description |
|-------|------|-------------|
| type | keyword | :collect, :sum, :count, :maximize, :minimize, :append, :nconc |
| expr | form | Expression to accumulate |
| into | symbol | Target variable (optional) |

### loop-termination-clause
| Field | Type | Description |
|-------|------|-------------|
| type | keyword | :while, :until, :always, :never, :thereis, :return |
| expr | form | Condition/value expression |

---

## 4. Keyword Argument Info

### keyword-arg-info

Used during codegen to track keyword argument positions.

| Field | Type | Description |
|-------|------|-------------|
| keyword | keyword | The keyword symbol (e.g., :test, :key) |
| arg-index | integer | Position in call arguments |
| value-ast | ast-node | Parsed argument value |

---

## 5. Compilation Rate Metrics

### form-compilation-result

Tracks result of compiling a single form.

| Field | Type | Description |
|-------|------|-------------|
| form | form | Original source form |
| success-p | boolean | Whether compilation succeeded |
| error-message | string | Error details if failed |
| operator | symbol | Top-level operator (defun, let, etc.) |

### module-compilation-stats

Aggregated stats for a source module.

| Field | Type | Description |
|-------|------|-------------|
| module-path | pathname | Source file path |
| total-forms | integer | Total compilable forms |
| successful-forms | integer | Forms that compiled |
| failed-forms | integer | Forms that failed |
| rate | float | Success percentage |

---

## WasmGC Type Indices

| Index | Type Name | Description |
|-------|-----------|-------------|
| 25 | $hash-entry | Hash table entry struct |
| 26 | $hash-table | Hash table struct |
| 27 | $bucket-array | Array of bucket heads |

**Note**: Indices 0-24 are already allocated by existing types (see gc-types.lisp).

---

## Relationships

```
hash-table --(has many)--> bucket-array element --(chains to)--> hash-entry

ast-defun --(contains)--> ast-param-info (multiple)

loop-context --(contains)--> loop-iter-clause (multiple)
loop-context --(contains)--> loop-accumulation-clause (multiple)
loop-context --(contains)--> loop-termination-clause (multiple)
```

---

## Validation Rules

1. **Hash Table Size**: Must be positive integer, default 17 (prime)
2. **Hash Table Test**: Must be one of eq, eql, equal, equalp
3. **Parameter Names**: Must not shadow lambda-list keywords (&optional, &key, etc.)
4. **Supplied-p Variables**: Must be distinct from parameter names
5. **LOOP Into Variables**: Must be symbols, may shadow outer bindings
