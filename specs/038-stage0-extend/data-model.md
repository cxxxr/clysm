# Data Model: Stage 0 Capability Extension

**Date**: 2025-12-27
**Branch**: 038-stage0-extend

## Entities

### AST Nodes

#### ast-defconstant (NEW)

Represents a `(defconstant name value [doc])` form.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Constant name (e.g., `+max-stack+`) |
| value-form | ast-node | AST of the initialization expression |
| docstring | string \| nil | Optional documentation |

**Validation**:
- name must be a symbol
- value-form must be compile-time evaluable

**State Transitions**: N/A (immutable AST node)

#### ast-declare (Existing - Enhanced)

Represents a `(declare ...)` form to be skipped.

| Field | Type | Description |
|-------|------|-------------|
| specifiers | list | List of declaration specifiers |

**Behavior**: Parsed but not compiled; filtered from function bodies.

### Bootstrap Data Structures

#### constant-registry (NEW)

Compile-time mapping of constant names to evaluated values.

| Field | Type | Description |
|-------|------|-------------|
| table | hash-table | symbol → value mapping |

**Operations**:
- `register-constant (name value)`: Add constant to registry
- `lookup-constant (name)`: Get value or error if undefined
- `constant-defined-p (name)`: Check if constant exists

**Lifecycle**:
1. Created empty at bootstrap start
2. Populated as defconstant forms are processed (in order)
3. Queried during constant folding
4. Discarded after bootstrap completes

#### compile-result (Enhanced)

Extended to track per-operator statistics.

| Field | Type | Description |
|-------|------|-------------|
| successful | fixnum | Count of compiled forms |
| failed | fixnum | Count of failed forms |
| failed-forms | list | List of (form . error) pairs |
| bytes | array | Final Wasm binary |
| **operator-failures** | hash-table | NEW: operator-symbol → count |
| **operator-examples** | hash-table | NEW: operator-symbol → (list of form-strings) |

### Expansion Output Structures

#### defstruct-expansion

Represents the expansion of a defstruct form.

| Field | Type | Description |
|-------|------|-------------|
| struct-name | symbol | Original struct name |
| slot-specs | list | List of (slot-name default-form) |
| constructor-name | symbol | Constructor function name |
| generated-forms | list | List of generated defun forms |

**Generated Functions**:
- `make-NAME` or custom constructor
- `NAME-SLOT` for each slot
- `NAME-p` predicate

#### condition-expansion

Represents the expansion of a define-condition form.

| Field | Type | Description |
|-------|------|-------------|
| condition-name | symbol | Condition class name |
| parent-types | list | Parent condition types |
| slot-specs | list | Slot specifications |
| report-form | form \| nil | :report option (stored, not compiled) |
| generated-defclass | form | Resulting defclass form |

## Relationships

```text
bootstrap-context
    ├── constant-registry (1:1, created at start)
    └── compile-result (1:1, accumulated during compilation)

ast-defun
    └── body (may contain ast-declare, filtered during parse)

defstruct-expansion
    └── generated-forms (1:N defun forms)

condition-expansion
    └── generated-defclass (1:1 defclass form)
```

## Data Flow

```text
Source Files
    │
    ▼
[Read] → Raw S-expressions
    │
    ▼
[Filter] → Compilable forms (with defconstant, now)
    │
    ▼
[Expand Macros] → Expanded forms
    │  ├── define-condition → defclass
    │  └── defstruct → defun(s)
    │
    ▼
[Constant Folding] → Forms with constants resolved
    │  └── Uses constant-registry
    │
    ▼
[Parse AST] → AST nodes
    │  └── declare forms filtered here
    │
    ▼
[Compile] → Wasm bytes + compile-result
    │
    ▼
[Validate] → wasm-tools validate
    │
    ▼
[Write] → dist/clysm-stage0.wasm
```

## Wasm Output Entities

### Global Definitions

For each defconstant/defparameter, emit a Wasm global:

```wat
;; defconstant +type-cons+ 2
(global $+type-cons+ (ref i31) (ref.i31 (i32.const 2)))

;; defparameter *debug-level* 0
(global $*debug-level* (mut anyref) (ref.i31 (i32.const 0)))
```

### Type Indices (Unchanged)

Existing type indices from gc-types.lisp are unchanged. No new type indices required for this feature.

## Validation Rules

| Entity | Rule | Error Message |
|--------|------|---------------|
| ast-defconstant | value-form must be constant-foldable | "Cannot evaluate ~A at compile time" |
| constant-registry | lookup must find defined constant | "Undefined constant ~A" |
| defstruct-expansion | struct-name must be a symbol | "Invalid struct name ~A" |
| condition-expansion | parent must be condition subtype | "~A is not a condition type" |
| compile-result | bytes must pass wasm-tools validate | "Wasm validation failed: ~A" |
