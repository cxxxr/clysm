# Data Model: I/O Print Primitives

**Feature**: 001-io-print-primitives
**Date**: 2025-12-31

## Overview

This document describes the internal data representations used by the I/O print primitives during compilation and execution.

## Entities

### 1. Print Primitive Descriptor

Represents a print function recognized by the compiler as a primitive.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Function name (print, prin1, princ, terpri, write) |
| arity | (min . max) | Argument count range |
| escape-p | boolean | Whether to print with escape characters |
| prefix-newline-p | boolean | Whether to output newline before object |
| suffix-space-p | boolean | Whether to output space after object |
| returns | :object \| :nil | What the function returns |

**Instances**:

| Function | arity | escape-p | prefix-newline | suffix-space | returns |
|----------|-------|----------|----------------|--------------|---------|
| [print](resources/HyperSpec/Body/f_wr_pr.htm) | (1 . 2) | t | t | t | :object |
| [prin1](resources/HyperSpec/Body/f_wr_pr.htm) | (1 . 2) | t | nil | nil | :object |
| [princ](resources/HyperSpec/Body/f_wr_pr.htm) | (1 . 2) | nil | nil | nil | :object |
| [terpri](resources/HyperSpec/Body/f_terpri.htm) | (0 . 1) | N/A | N/A | N/A | :nil |
| [write](resources/HyperSpec/Body/f_wr_pr.htm) | (1 . *) | varies | nil | nil | :object |

---

### 2. Format Directive

Represents a parsed format directive from a format string.

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Directive type (:aesthetic, :standard, :decimal, :newline, :fresh-line, :tilde) |
| position | fixnum | Position in format string |
| arg-count | fixnum | Number of format arguments consumed (0 or 1) |
| literal-text | string \| nil | Literal text between directives |

**Type Mapping**:

| Format Char | Type | arg-count | ANSI Spec |
|-------------|------|-----------|-----------|
| ~A | :aesthetic | 1 | [22.3.4.1](resources/HyperSpec/Body/22_cca.htm) |
| ~S | :standard | 1 | [22.3.4.2](resources/HyperSpec/Body/22_ccb.htm) |
| ~D | :decimal | 1 | [22.3.2.1](resources/HyperSpec/Body/22_cba.htm) |
| ~% | :newline | 0 | [22.3.5.1](resources/HyperSpec/Body/22_cea.htm) |
| ~& | :fresh-line | 0 | [22.3.5.2](resources/HyperSpec/Body/22_ceb.htm) |
| ~~ | :tilde | 0 | [22.3.6.1](resources/HyperSpec/Body/22_cfa.htm) |

---

### 3. Stream (Existing)

Represents an I/O stream. Already defined in `src/clysm/streams/types.lisp`.

| Field | Type | Description |
|-------|------|-------------|
| fd | fixnum | File descriptor (0=stdin, 1=stdout, 2=stderr) |
| direction | fixnum | Input (+direction-input+) or output (+direction-output+) |

**WasmGC Representation**:
```wat
;; Stream is a simple struct with fd
(type $stream (struct
  (field $fd i32)
  (field $direction i32)))
```

---

### 4. Compiled Format Call

Represents the compiled form of a format function call.

| Field | Type | Description |
|-------|------|-------------|
| destination | :nil \| :t \| local-index | Where to send output |
| directives | list<format-directive> | Parsed directives |
| arg-indices | list<local-index> | Wasm local indices for format arguments |
| result-local | local-index \| nil | Local for string accumulation (when dest=nil) |

---

## Relationships

```
Format Call
    │
    ├── has destination ──→ Stream (t) or String Builder (nil)
    │
    └── has directives ──→ [Format Directive]*
                               │
                               └── consumes ──→ Format Argument
```

```
Print Primitive Call
    │
    ├── has object ──→ Lisp Object (anyref)
    │
    └── has stream ──→ Stream (default: *standard-output*)
                           │
                           └── uses ──→ FFI write-char / write-string
```

## Validation Rules

### Format String Validation

1. **Directive balance**: All `~{` must have matching `~}` (not in scope for basic directives)
2. **Argument count**: Number of consuming directives ≤ number of format arguments
3. **Character validity**: Only supported directive characters allowed (~A, ~S, ~D, ~%, ~&, ~~)

### Print Call Validation

1. **Arity check**: Argument count within (min . max) range
2. **Stream type**: Optional stream argument must be stream or t or nil
3. **Object presence**: Object argument must be provided (except terpri)

## State Transitions

### Format Compilation State

```
Format String Literal
    │
    ▼ parse-format-string
Directive List
    │
    ▼ compile-format-directives
Instruction Sequence
    │
    ▼ emit
Wasm Function Body
```

### Print Execution State

```
Object + Stream
    │
    ▼ marshal to Wasm types
(anyref, i32 fd)
    │
    ▼ call FFI write functions
Host I/O performed
    │
    ▼ return
Object (or NIL for terpri)
```
