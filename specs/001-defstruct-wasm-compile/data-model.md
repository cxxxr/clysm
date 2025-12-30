# Data Model: DEFSTRUCT Wasm Compilation

**Feature**: 001-defstruct-wasm-compile
**Date**: 2025-12-30

## Entities

### 1. defstruct-definition

Parsed representation of a [defstruct](resources/HyperSpec/Body/m_defstr.htm) form.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `symbol` | Structure type name |
| `conc-name` | `(or null symbol string)` | Accessor prefix (nil = no prefix, symbol = prefix) |
| `constructor` | `(or null symbol (cons symbol list))` | Constructor spec or nil to suppress |
| `copier` | `(or null symbol)` | Copier function name (nil = default `copy-NAME`) |
| `predicate` | `(or null symbol)` | Predicate function name (nil = default `NAME-p`) |
| `include` | `(or null symbol)` | Parent structure name |
| `include-slot-overrides` | `list` | Slot defaults to override from parent |
| `slots` | `(list-of slot-definition)` | Direct slot definitions |
| `docstring` | `(or null string)` | Documentation string |

**Validation Rules**:
- `name` MUST be a non-nil symbol
- `include` MUST reference an existing structure type if present
- Slot names MUST be unique within the structure (including inherited)

### 2. slot-definition

Slot specification within a structure.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `symbol` | Slot name |
| `initform` | `form` | Default value form |
| `initform-p` | `boolean` | Whether initform was explicitly provided |
| `type` | `type-specifier` | Declared type (informational) |
| `read-only` | `boolean` | If true, no setf accessor generated |

**Validation Rules**:
- `name` MUST be a non-nil symbol
- `type`, if specified, MUST be a valid type specifier

### 3. structure-class (metaclass)

Structure type metadata stored in class registry.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `symbol` | Structure type name |
| `superclasses` | `list` | Parent classes (from :include) |
| `slots` | `(list-of slot-definition)` | All slots (including inherited) |
| `precedence-list` | `list` | Class precedence list |
| `slot-index-table` | `hash-table` | Slot name → index mapping |
| `copier` | `(or null symbol)` | Copier function name |
| `predicate` | `(or null symbol)` | Predicate function name |
| `constructor` | `symbol` | Constructor function name |

**Relationships**:
- `structure-class` → `standard-class` (inherits from)
- `structure-class` → `slot-definition` (has many)

### 4. structure-instance

Runtime representation of a structure instance.

| Field | Type | Description |
|-------|------|-------------|
| `class` | `structure-class` | Class metadata reference |
| `slots` | `simple-vector` | Slot value storage |

**WasmGC Mapping**:
- Uses `$instance` type (index 6)
- `class` field: `(ref $standard-class)`
- `slots` field: `(ref $slot-vector)`

## Entity Relationships

```
┌─────────────────────┐
│ defstruct-definition│
│     (parse-time)    │
└──────────┬──────────┘
           │ parses to
           ▼
┌─────────────────────┐      creates      ┌─────────────────┐
│   structure-class   │─────────────────►│ constructor-fn  │
│    (compile-time)   │                   │ (make-NAME)     │
└──────────┬──────────┘                   └─────────────────┘
           │
           │ has many
           ▼
┌─────────────────────┐      creates      ┌─────────────────┐
│   slot-definition   │─────────────────►│  accessor-fn    │
│                     │                   │ (NAME-slot)     │
└─────────────────────┘                   └─────────────────┘
                                                  │
                                                  │ registers
                                                  ▼
                                          ┌─────────────────┐
                                          │ setf-expander   │
                                          │ ((setf NAME-x)) │
                                          └─────────────────┘
```

## State Transitions

### defstruct Definition Lifecycle

```
┌──────────┐     parse      ┌──────────────┐     expand     ┌────────────┐
│ Raw Form │───────────────►│ defstruct-   │──────────────►│  progn of  │
│ (sexp)   │                │ definition   │               │  defclass  │
└──────────┘                └──────────────┘               │ + defuns   │
                                                           └─────┬──────┘
                                                                 │
                                                                 │ compile
                                                                 ▼
                                                           ┌────────────┐
                                                           │ WasmGC     │
                                                           │ module     │
                                                           └────────────┘
```

## Type Hierarchy

```
                     structure-object
                           │
        ┌─────────────────┼─────────────────┐
        │                 │                 │
   source-location     ast-node        gc-type
                          │                 │
        ┌────────┬────────┼────────┐       │
        │        │        │        │       │
   ast-literal ast-call ast-if   ...   wasm-struct-type
```

## Glossary

| Term | Definition |
|------|------------|
| **conc-name** | Concatenated name prefix for accessor functions |
| **slot** | Named storage location within a structure instance |
| **predicate** | Type-checking function returning boolean |
| **copier** | Function that creates a shallow copy of an instance |
| **structure-class** | Metaclass distinguishing structures from general CLOS classes |
