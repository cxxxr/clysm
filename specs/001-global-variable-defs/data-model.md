# Data Model: Phase 13D-4 Global Variable Definitions

**Date**: 2025-12-30

## Entities

### GlobalDeclaration

Represents a [defvar](../../resources/HyperSpec/Body/m_defpar.htm) or [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) form being compiled.

| Field | Type | Description |
|-------|------|-------------|
| `name` | symbol | The variable name (e.g., `*foo*`) |
| `kind` | keyword | `:defvar` or `:defparameter` |
| `init-form` | form | Optional initialization expression |
| `init-type` | keyword | `:constant`, `:deferred`, or `:none` |
| `global-index` | integer | Allocated Wasm global index (4+) |
| `docstring` | string | Optional documentation |

**Validation Rules**:
- `name` must be a symbol with earmuffs (`*name*` convention)
- `global-index` must be >= 4 (0-3 reserved)
- `init-type` determined by analyzing `init-form`

### GlobalRegistry

Maps symbol names to Wasm global indices.

| Field | Type | Description |
|-------|------|-------------|
| `entries` | hash-table | symbol → integer mapping |
| `next-index` | integer | Next available index |

**Validation Rules**:
- Keys must be symbols
- Values must be integers >= 4
- No duplicate indices allowed

### GlobalSection

Wasm binary global section (Section ID 6).

| Field | Type | Description |
|-------|------|-------------|
| `section-id` | byte | Always `#x06` |
| `size` | uleb128 | Section size in bytes |
| `count` | uleb128 | Number of globals |
| `globals` | vector | GlobalEntry items |

### GlobalEntry

Single global variable in Wasm binary.

| Field | Type | Description |
|-------|------|-------------|
| `type` | valtype | `(ref null any)` encoded |
| `mutability` | byte | `#x00` (immutable) or `#x01` (mutable) |
| `init-expr` | bytes | Initialization expression |

**Init Expression Patterns**:
```
Constant integer: i32.const N, end
Constant NIL:     global.get 0, end
Constant UNBOUND: global.get 1, end
Deferred:         ref.null any, end (then set in $init)
```

## State Transitions

### GlobalDeclaration Lifecycle

```
┌─────────────┐
│   PARSED    │  defvar/defparameter form parsed from source
└──────┬──────┘
       │ analyze-init-form
       ▼
┌─────────────┐
│  ANALYZED   │  init-type determined (constant/deferred/none)
└──────┬──────┘
       │ allocate-global-index
       ▼
┌─────────────┐
│  ALLOCATED  │  Wasm global index assigned
└──────┬──────┘
       │ emit-global-entry
       ▼
┌─────────────┐
│   EMITTED   │  GlobalEntry added to GlobalSection
└─────────────┘
```

### defvar vs defparameter Behavior

```
defvar semantics:
  IF variable is UNBOUND
    THEN initialize to init-form
    ELSE keep existing value

defparameter semantics:
  ALWAYS initialize to init-form (unconditional)
```

## Relationships

```
GlobalDeclaration 1──────* GlobalRegistry
       │
       │ compiles to
       ▼
GlobalEntry ────────────* GlobalSection
       │
       │ references
       ▼
Reserved Globals (0-3: NIL, UNBOUND, mv-count, mv-buffer)
```

## Wasm Type Encoding

### Global Value Type

For special variables: `(ref null any)`

Encoding:
```
#x63           ; ref type constructor
#x6F           ; any (externref in older spec)
```

### Mutability Flag

```
#x00  ; immutable (const)
#x01  ; mutable (var)
```

All special variables use `#x01` (mutable) for dynamic binding.

## Reserved Global Indices

| Index | Name | Type | Mutability | Init |
|-------|------|------|-----------|------|
| 0 | `$nil` | `(ref $nil)` | immutable | struct.new $nil |
| 1 | `$unbound` | `(ref $unbound)` | immutable | struct.new $unbound |
| 2 | `$mv_count` | `i32` | mutable | i32.const 0 |
| 3 | `$mv_buffer` | `(ref $mv_array)` | mutable | array.new_default |
| 4+ | user globals | `(ref null any)` | mutable | varies |

## Example Compilations

### Simple defvar
```lisp
(defvar *counter* 0)
```
Produces:
```wat
(global $*counter* (mut (ref null any))
  (ref.i31 (i32.const 0)))
```

### defvar with no init
```lisp
(defvar *unset*)
```
Produces:
```wat
(global $*unset* (mut (ref null any))
  (global.get $unbound))
```

### defparameter with complex init
```lisp
(defparameter *registry* (make-hash-table))
```
Produces:
```wat
(global $*registry* (mut (ref null any))
  (ref.null any))  ;; placeholder

(func $init
  ...
  (global.set $*registry*
    (call $make-hash-table)))
```
