# Data Model: I/O and List Operations Runtime

**Date**: 2026-01-01
**Feature**: 001-io-list-runtime

## Overview

This document defines the key entities, their attributes, and relationships for the runtime library migration.

---

## Entities

### 1. Runtime I/O Module

**File**: `src/clysm/lib/io-runtime.lisp`

**Purpose**: Provides I/O functions using FFI primitives

| Function | Parameters | Return | ANSI CL Reference |
|----------|------------|--------|-------------------|
| princ | object &optional stream | object | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| prin1 | object &optional stream | object | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| print | object &optional stream | object | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| write | object &key stream escape radix base circle pretty level length | object | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| format | destination control-string &rest args | nil or string | [f_format.htm](resources/HyperSpec/Body/f_format.htm) |
| terpri | &optional stream | nil | [f_terpri.htm](resources/HyperSpec/Body/f_terpri.htm) |

**Dependencies**:
- `%host-write-char` (FFI primitive)
- `%host-write-string` (FFI primitive)
- `princ-to-string` (existing in runtime/printer.lisp)
- `prin1-to-string` (existing in runtime/printer.lisp)

---

### 2. Runtime List Module

**File**: `src/clysm/lib/list-runtime.lisp`

**Purpose**: Provides list search functions using car/cdr/consp primitives

| Function | Parameters | Return | ANSI CL Reference |
|----------|------------|--------|-------------------|
| member | item list &key test test-not key | list or nil | [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm) |
| member-if | predicate list &key key | list or nil | [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm) |
| member-if-not | predicate list &key key | list or nil | [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm) |
| assoc | item alist &key test test-not key | cons or nil | [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm) |
| assoc-if | predicate alist &key key | cons or nil | [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm) |
| rassoc | item alist &key test test-not key | cons or nil | [f_rassoc.htm](resources/HyperSpec/Body/f_rassoc.htm) |
| rassoc-if | predicate alist &key key | cons or nil | [f_rassoc.htm](resources/HyperSpec/Body/f_rassoc.htm) |
| find | item sequence &key test key start end from-end | element or nil | [f_find_.htm](resources/HyperSpec/Body/f_find_.htm) |
| find-if | predicate sequence &key key start end from-end | element or nil | [f_find_.htm](resources/HyperSpec/Body/f_find_.htm) |
| find-if-not | predicate sequence &key key start end from-end | element or nil | [f_find_.htm](resources/HyperSpec/Body/f_find_.htm) |
| position | item sequence &key test key start end from-end | index or nil | [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm) |
| position-if | predicate sequence &key key start end from-end | index or nil | [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm) |
| position-if-not | predicate sequence &key key start end from-end | index or nil | [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm) |

**Dependencies**:
- `car`, `cdr`, `consp`, `null` (primitives)
- `eq`, `eql` (equality predicates)
- `funcall` (for test/key function application)

---

### 3. Compiler Dispatch Table

**File**: `src/clysm/compiler/codegen/func-section.lisp`

**Purpose**: Maps function names to compile-time handlers or runtime calls

**Before migration**:
```lisp
(defparameter *function-dispatch-table*
  '((member . compile-member)
    (assoc . compile-assoc)
    (princ . compile-princ)
    ...))
```

**After migration**:
```lisp
(defparameter *runtime-function-table*
  '(member member-if member-if-not
    assoc assoc-if rassoc rassoc-if
    find find-if find-if-not
    position position-if position-if-not
    princ prin1 print write format terpri))

;; Compile-time: emit runtime call instead of inline code
(defun compile-runtime-call (fn-name args env)
  (let ((compiled-args (mapcar (lambda (a) (compile-to-instructions a env)) args)))
    (append (apply #'append compiled-args)
            `((:call ,(intern (format nil "$~A" fn-name) :keyword))))))
```

---

## Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                      Compiler                                │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ func-section.lisp                                     │  │
│  │                                                       │  │
│  │  (function-call)                                      │  │
│  │       │                                               │  │
│  │       ▼                                               │  │
│  │  [Is runtime function?]──yes──► emit :call :$fn-name  │  │
│  │       │                              │                │  │
│  │       no                             ▼                │  │
│  │       ▼                     Runtime Library           │  │
│  │  [compile-* inline]                  │                │  │
│  │       │                              │                │  │
│  └───────┼──────────────────────────────┼────────────────┘  │
│          ▼                              ▼                    │
│  ┌───────────────┐              ┌──────────────────┐        │
│  │ Wasm inline   │              │ lib/list-runtime │        │
│  │ code output   │              │ lib/io-runtime   │        │
│  └───────────────┘              └────────┬─────────┘        │
│                                          │                   │
└──────────────────────────────────────────┼───────────────────┘
                                           │
                                           ▼
                              ┌─────────────────────────┐
                              │    FFI Primitives       │
                              │  %host-write-char       │
                              │  %host-write-string     │
                              │  car, cdr, consp        │
                              └─────────────────────────┘
```

---

## State Transitions

### Function Call Resolution

```
Source: (member 'a '(a b c))
    │
    ▼
[Parse] → AST: (call member (quote a) (quote (a b c)))
    │
    ▼
[Compile] → Check *runtime-function-table*
    │
    ├─[Found] → emit (:call :$member)
    │               ↓
    │          Link to runtime library
    │               ↓
    │          Execute at runtime
    │
    └─[Not found] → [Error or fallback]
```

---

## Validation Rules

1. **Argument count**: Each function validates minimum required arguments
2. **Type constraints**:
   - list functions: second argument must be a list (for list variants)
   - sequence functions: support both lists and vectors
3. **Keyword defaults**:
   - `:test` defaults to `#'eql`
   - `:key` defaults to `#'identity`
   - `:start` defaults to `0`
   - `:end` defaults to sequence length
   - `:from-end` defaults to `nil`
