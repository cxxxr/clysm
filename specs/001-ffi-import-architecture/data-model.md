# Data Model: FFI Import Architecture

**Feature**: 001-ffi-import-architecture
**Date**: 2025-12-31

## Entities

### FFI Analysis Result

Structure returned by the FFI usage analyzer.

```lisp
(defstruct ffi-analysis
  "Result of FFI usage analysis on compiled code."
  (used-ffis nil :type list)        ; List of FFI function names (symbols)
  (has-dynamic-call-p nil :type boolean)  ; True if dynamic funcall/apply detected
  (static-funcalls nil :type list)  ; Quoted symbols in funcall (for debugging)
  (dynamic-sites nil :type list))   ; Source locations of dynamic calls (for warnings)
```

**Fields**:
| Field | Type | Description |
|-------|------|-------------|
| `used-ffis` | `(list symbol)` | FFI function names detected as statically called |
| `has-dynamic-call-p` | `boolean` | True if any dynamic funcall/apply patterns found |
| `static-funcalls` | `(list symbol)` | Symbols from `(funcall 'sym ...)` patterns |
| `dynamic-sites` | `(list form)` | Forms containing dynamic calls (for error messages) |

### Compilation Mode

Enumeration controlling FFI import behavior.

```lisp
(deftype ffi-mode ()
  "Compilation mode for FFI import handling."
  '(member :minimal :full :auto))
```

| Mode | Description |
|------|-------------|
| `:minimal` | Only static FFI imports; error on dynamic calls |
| `:full` | Static FFI imports + always include `$dynamic-call` |
| `:auto` | Static FFI imports + include `$dynamic-call` only if needed |

### Dynamic Call Import

Wasm import for runtime function resolution.

```wat
;; Type signature
(type $dynamic-call-type (func (param anyref anyref) (result anyref)))

;; Import declaration
(import "clysm:runtime" "dynamic-call" (func $dynamic-call (type $dynamic-call-type)))
```

**Parameters**:
| Index | Type | Description |
|-------|------|-------------|
| 0 | `anyref` | Function name (symbol) |
| 1 | `anyref` | Arguments (cons list) |

**Result**: `anyref` - Return value from called function

## State Transitions

### FFI Analysis Flow

```
Source Code
    │
    ▼
Macro Expansion (existing)
    │
    ▼
┌─────────────────────┐
│ analyze-ffi-usage   │
│                     │
│ Walk expanded form: │
│ - Direct FFI calls  │
│ - funcall 'sym      │
│ - funcall expr      │
│ - apply             │
└─────────────────────┘
    │
    ▼
FFI Analysis Result
    │
    ▼
┌─────────────────────────────────────────┐
│ emit-module                              │
│                                          │
│ Mode check:                              │
│ - :minimal + dynamic → ERROR            │
│ - :full → include $dynamic-call         │
│ - :auto + dynamic → include $dynamic-call│
│ - :auto + static-only → no $dynamic-call│
└─────────────────────────────────────────┘
    │
    ▼
Wasm Binary (selective imports)
```

## Relationships

```
*ffi-environment*  ──────────────────┐
    │                                │
    │ (all registered FFI)           │ (filter by)
    │                                │
    ▼                                │
┌──────────────┐                     │
│ FFI Registry │                     │
│ (hash-table) │                     │
└──────────────┘                     │
                                     │
ffi-analysis ───────────────────────►├── used-ffis
    │                                │
    │                                ▼
    │                         emit-selected-ffi-imports
    │                                │
    └──► has-dynamic-call-p ────────►├── emit-dynamic-call-import (conditional)
                                     │
                                     ▼
                              Wasm Import Section
```

## Validation Rules

### FFI Usage Analysis

1. **Complete Walk**: Must walk entire macro-expanded form including:
   - `progn` bodies
   - `let`/`let*` bindings and bodies
   - `if` branches
   - `lambda` bodies
   - `labels`/`flet` local function bodies

2. **FFI Detection**: A function is considered FFI if:
   - Registered in `*ffi-environment*`
   - Called directly: `(ffi-func args...)`
   - Called via quoted funcall: `(funcall 'ffi-func args...)`

3. **Dynamic Detection**: A call is dynamic if:
   - `(funcall expr ...)` where expr is not `'symbol` or `#'symbol`
   - `(apply expr ...)` where expr is not `'symbol` or `#'symbol`
   - Conservatively: `(apply fn ...)` with any non-literal fn

### Compilation Mode Validation

1. **Minimal Mode Error**: If `:ffi-mode :minimal` and `has-dynamic-call-p`:
   - Signal `dynamic-call-in-minimal-mode` error
   - Include first dynamic-site in error message

2. **Auto Mode Upgrade**: If `:ffi-mode :auto` and `has-dynamic-call-p`:
   - Automatically include `$dynamic-call` import
   - Log upgrade decision if verbose

### Import Section Consistency

1. **No Duplicate Imports**: Each FFI function imported at most once
2. **Index Assignment**: Imports get indices 0..N-1, local functions N+
3. **Type Alignment**: Import type indices match function signatures
