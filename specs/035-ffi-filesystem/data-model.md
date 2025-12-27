# Data Model: FFI Filesystem Access

**Feature**: 035-ffi-filesystem
**Date**: 2025-12-27

## Entities

### 1. file-stream

Represents an open file handle with metadata.

**Fields**:
| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| handle | externref | Yes | Opaque host file handle; null when closed |
| direction | symbol | No | `:input` or `:output` |
| pathname | string | No | Original filename used to open |
| open-p | boolean | Yes | T if stream is open, NIL if closed |

**WasmGC Type Definition**:
```wat
(type $file-stream (struct
  (field $handle (mut externref))   ; Host file handle (nullable)
  (field $direction (ref $symbol))  ; :input or :output
  (field $pathname (ref $string))   ; Original filename
  (field $open_p (mut i31ref))      ; Boolean: T or NIL as i31ref
))
```

**Validation Rules**:
- `direction` must be either `:input` or `:output`
- `pathname` must be a non-empty string
- `handle` must not be null when `open-p` is T
- Operations on closed stream (open-p = NIL) signal `file-error`

**Relationships**:
- Created by `open-file` function
- Consumed by `close-file`, `read-file-contents` (when stream variant is used)
- Bound by `with-open-file` macro

---

### 2. file-error (Condition)

Condition signaled for filesystem-related errors.

**Fields**:
| Field | Type | Description |
|-------|------|-------------|
| pathname | string | The pathname that caused the error |

**CLOS Definition**:
```lisp
(defclass file-error (error)
  ((pathname
    :initarg :pathname
    :reader file-error-pathname)))
```

**Relationships**:
- Extends `error` (from condition hierarchy)
- Signaled by `open-file`, `read-file-contents`, `write-file-contents`
- Can be caught with `handler-case` or `handler-bind`

---

### 3. Direction Keyword

Enumeration for file access direction.

**Values**:
| Value | Description |
|-------|-------------|
| `:input` | File opened for reading |
| `:output` | File opened for writing |

**Used by**: `open-file`, `with-open-file`

---

### 4. If-Exists Keyword

Enumeration for handling existing output files.

**Values**:
| Value | Description | Default |
|-------|-------------|---------|
| `:supersede` | Overwrite existing file | Yes (for output) |
| `:error` | Signal file-error if exists | No |

**Used by**: `open-file`, `with-open-file` (when `:direction :output`)

---

### 5. If-Does-Not-Exist Keyword

Enumeration for handling missing files.

**Values**:
| Value | Description | Default |
|-------|-------------|---------|
| `:error` | Signal file-error if not found | Yes (for input) |
| `:create` | Create new file | Yes (for output) |

**Used by**: `open-file`, `with-open-file`

---

## State Transitions

### file-stream Lifecycle

```
                  open-file
    [not exists] ─────────────> [OPEN]
                                  │
                     close-file   │
                    ─────────────>│
                                  v
                               [CLOSED]

States:
- OPEN: handle is valid, open-p = T
- CLOSED: handle is null, open-p = NIL
```

**Transitions**:
| From | To | Trigger | Condition |
|------|----|---------|-----------|
| (none) | OPEN | `open-file` | File exists or created |
| (none) | (error) | `open-file` | File not found, permission denied |
| OPEN | CLOSED | `close-file` | Always succeeds |
| OPEN | OPEN | `read-file-contents` | For input streams |
| OPEN | OPEN | `write-file-contents` | For output streams |
| CLOSED | (error) | Any operation | Signals file-error |

---

## FFI Type Mappings

| Lisp Type | FFI Marshal Type | Wasm Type | Host Type |
|-----------|------------------|-----------|-----------|
| file-stream.handle | :externref | externref | Object (fd wrapper) |
| pathname (string) | :string | (ref $string) | string |
| direction (symbol) | :symbol | (ref $symbol) | string |
| if-exists (symbol) | :symbol | (ref $symbol) | string |
| if-does-not-exist (symbol) | :symbol | (ref $symbol) | string |
| file contents (string) | :string | (ref $string) | string |

---

## Entity-Relationship Diagram

```
                    ┌─────────────────┐
                    │   file-stream   │
                    ├─────────────────┤
                    │ handle          │◄──── Host file handle
                    │ direction       │
                    │ pathname        │
                    │ open-p          │
                    └────────┬────────┘
                             │
                    signals on error
                             │
                             v
                    ┌─────────────────┐
                    │   file-error    │
                    ├─────────────────┤
                    │ pathname        │
                    └─────────────────┘
                             │
                         extends
                             │
                             v
                    ┌─────────────────┐
                    │     error       │
                    └─────────────────┘
```

---

## Invariants

1. **Stream Validity**: A file-stream with `open-p = T` must have a non-null `handle`.
2. **Direction Immutability**: Once created, a file-stream's direction cannot change.
3. **Closed Stream Safety**: All operations on a closed stream must signal `file-error`.
4. **Resource Cleanup**: `with-open-file` must always call `close-file`, even on error.
5. **UTF-8 Encoding**: All string content is UTF-8 encoded; no other encodings supported.
