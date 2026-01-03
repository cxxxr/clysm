# Data Model: Primitive Dispatch Table

**Feature**: 002-primitive-dispatch-table
**Date**: 2026-01-03

## Entity Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Dispatch System                          │
│                                                             │
│  ┌─────────────────────┐    ┌─────────────────────┐        │
│  │ *primitive-symbol-  │    │ *primitive-string-  │        │
│  │      table*         │    │      table*         │        │
│  │  (eq hash-table)    │    │  (equal hash-table) │        │
│  │                     │    │                     │        │
│  │  CAR → entry        │    │  "CAR" → entry      │        │
│  │  CDR → entry        │    │  "%SETF-AREF" → ... │        │
│  └────────┬────────────┘    └────────┬────────────┘        │
│           │                          │                      │
│           └──────────┬───────────────┘                      │
│                      ▼                                      │
│           ┌─────────────────────┐                          │
│           │   primitive-entry   │                          │
│           │  (defstruct)        │                          │
│           │                     │                          │
│           │  compiler-fn: fn    │                          │
│           │  arity: fixnum|nil  │                          │
│           │  flags: plist       │                          │
│           └─────────────────────┘                          │
└─────────────────────────────────────────────────────────────┘
```

## Entities

### 1. Primitive Entry

**Description**: Represents a registered primitive's compilation metadata.

| Field | Type | Nullable | Description |
|-------|------|----------|-------------|
| compiler-fn | function | No | Code generator `(args env) -> instructions` |
| arity | fixnum | Yes | Expected argument count; NIL = variadic |
| flags | plist | Yes | Metadata (category, documentation, etc.) |

**Validation Rules**:
- `compiler-fn` MUST be a function accepting 2 arguments
- `arity` MUST be non-negative integer or NIL
- `flags` MUST be a property list (even-length list)

**State Transitions**: None (immutable after creation)

### 2. Symbol Dispatch Table

**Description**: Primary lookup table keyed by Lisp symbols.

| Attribute | Value |
|-----------|-------|
| Key Type | symbol (SBCL interned) |
| Value Type | primitive-entry |
| Test Function | eq |
| Initial Size | 512 (nearest power of 2 above 240) |

**Identity Rules**:
- Keys are unique by symbol identity (eq)
- Same symbol in different packages = different keys
- Standard primitives use CL package symbols

### 3. String Dispatch Table

**Description**: Secondary lookup table for cross-package symbol matching.

| Attribute | Value |
|-----------|-------|
| Key Type | string (uppercase) |
| Value Type | primitive-entry |
| Test Function | equal |
| Initial Size | 64 |

**Identity Rules**:
- Keys normalized to uppercase before insertion
- Used for symbols like `%SETF-AREF` which may appear in multiple packages
- Lookup falls back here when symbol table returns NIL

## Relationships

```
┌────────────────────┐       N:1        ┌─────────────────┐
│  Symbol Table Key  │ ───────────────▶ │ primitive-entry │
└────────────────────┘                  └─────────────────┘
                                               ▲
┌────────────────────┐       N:1               │
│  String Table Key  │ ────────────────────────┘
└────────────────────┘
```

- Multiple table keys can point to the same entry (aliasing)
- Each entry is stored once; tables hold references

## Lookup Algorithm

```
dispatch-primitive(op, args, env):
  1. entry ← gethash(op, *primitive-symbol-table*)
  2. if entry is NIL:
       name ← symbol-name(op)
       entry ← gethash(name, *primitive-string-table*)
  3. if entry is NIL:
       return NIL  ;; unknown primitive, fall back to general call
  4. fn ← primitive-entry-compiler-fn(entry)
  5. return funcall(fn, args, env)
```

## Data Volume Assumptions

| Metric | Value | Notes |
|--------|-------|-------|
| Total Primitives | ~240 | From research.md analysis |
| Symbol Table Entries | ~240 | One per standard primitive |
| String Table Entries | ~15 | Cross-package symbols only |
| Entry Size | ~24 bytes | struct header + 3 slots |
| Total Memory | ~6 KB | Negligible compile-time overhead |

## Extensibility

New primitives are added via:
```lisp
(register-primitive-compiler 'new-prim #'compile-new-prim
  :arity 2
  :flags '(:category :user-defined))
```

No modification to core dispatch logic required.
