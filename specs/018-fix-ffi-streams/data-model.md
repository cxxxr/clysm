# Data Model: Fix FFI Streams Module

**Feature**: 018-fix-ffi-streams
**Date**: 2025-12-24

## Overview

This is a **fix task** for an existing implementation. The data model was defined in 015-ffi-stream-io and is documented here for reference. No changes to the data model are required.

## Entity: Stream

Represents an I/O channel to host file descriptors.

### WasmGC Representation

```wat
(type $stream (struct
  (field $fd i32)        ;; Host file descriptor (0=stdin, 1=stdout, 2=stderr)
  (field $direction i32) ;; 0=input, 1=output, 2=bidirectional
))
```

### Type Index

| Type | Index | Notes |
|------|-------|-------|
| $stream | 19 | Defined in gc-types.lisp as +type-stream+ |

### Direction Constants

| Constant | Value | Meaning |
|----------|-------|---------|
| +direction-input+ | 0 | Read-only stream (stdin) |
| +direction-output+ | 1 | Write-only stream (stdout, stderr) |
| +direction-io+ | 2 | Bidirectional stream |

### Lisp-Level Representation

```lisp
(defstruct (stream (:constructor %make-stream)
                   (:copier nil)
                   (:predicate streamp))
  (fd 0 :type fixnum :read-only t)
  (direction +direction-input+ :type fixnum :read-only t))
```

## Standard Streams (Special Variables)

| Variable | FD | Direction | Description |
|----------|-----|-----------|-------------|
| *standard-input* | 0 | input | Default input stream |
| *standard-output* | 1 | output | Default output stream |
| *error-output* | 2 | output | Error output stream |

## Entity Relationships

```
┌─────────────────────────────────────────────────┐
│                   clysm/streams                  │
├─────────────────────────────────────────────────┤
│  stream (defstruct)                             │
│    ├── fd: fixnum                               │
│    └── direction: fixnum                        │
│                                                 │
│  Standard Streams (special vars)                │
│    ├── *standard-input*  → (make-stream 0 0)   │
│    ├── *standard-output* → (make-stream 1 1)   │
│    └── *error-output*    → (make-stream 2 1)   │
└─────────────────────────────────────────────────┘
          │
          │ uses
          ▼
┌─────────────────────────────────────────────────┐
│                   clysm/ffi                      │
├─────────────────────────────────────────────────┤
│  define-foreign-function                        │
│    └── %host-write-char, %host-read-char, etc. │
└─────────────────────────────────────────────────┘
          │
          │ uses
          ▼
┌─────────────────────────────────────────────────┐
│                clysm/conditions                  │
├─────────────────────────────────────────────────┤
│  type-error (for argument validation)           │
│  stream-error (base for I/O errors)             │
│  end-of-file (for EOF handling)                 │
└─────────────────────────────────────────────────┘
```

## Validation Rules

1. **stream-fd**: Must be non-negative fixnum (0, 1, 2 for standard streams)
2. **stream-direction**: Must be one of 0, 1, 2
3. **write operations**: Stream must have direction = 1 or 2
4. **read operations**: Stream must have direction = 0 or 2

## State Transitions

Streams in this implementation are **immutable** after creation. No state transitions occur.

| State | Description | Next States |
|-------|-------------|-------------|
| Created | Stream initialized with fd and direction | (none - immutable) |

## Changes Required

**None** - The data model is correct as designed in 015-ffi-stream-io.
