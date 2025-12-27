# Data Model: Interpreter Bootstrap Strategy

**Feature**: 044-interpreter-bootstrap
**Date**: 2025-12-28

## Core Entities

### 1. Interpreter Environment (interpreter-env)

Represents the lexical environment for variable bindings during interpretation.

| Field | Type | Description |
|-------|------|-------------|
| bindings | hash-table (eq) | Symbol → value mapping for this frame |
| parent | interpreter-env or null | Parent environment for lexical scoping |
| macros | hash-table (eq) | Symbol → macro-expander mapping (root env only) |

**Relationships:**
- Parent chain forms lexical scope hierarchy
- Root environment contains built-in bindings and macros

**Lifecycle:**
1. Created: `make-interpreter-env` with optional parent
2. Extended: `extend-env` creates child with new bindings
3. Destroyed: GC when no references remain

### 2. Interpreted Closure (interpreted-closure)

Represents a function created by lambda interpretation.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol or null | Function name (nil for anonymous) |
| lambda-list | list | Parameter specification |
| body | list | List of body forms |
| env | interpreter-env | Captured lexical environment |
| docstring | string or null | Documentation string |

**Relationships:**
- Captures env reference at creation time
- Invocation extends env with parameter bindings

**Lifecycle:**
1. Created: `interpret-lambda` or `interpret-defun`
2. Invoked: `apply-interpreted-closure`
3. Destroyed: GC when no references remain

### 3. Macro Expander (macro-expander)

Represents a macro registered for expansion.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Macro name |
| lambda-list | list | Macro lambda-list with &whole, &body, etc. |
| body | list | Macro body forms |
| env | interpreter-env | Definition-time environment |
| whole-var | symbol or null | &whole binding variable |
| env-var | symbol or null | &environment binding variable |

**Relationships:**
- Stored in global `*macro-registry*`
- Expansion produces new form to interpret

**Lifecycle:**
1. Created: `interpret-defmacro`
2. Used: `maybe-macroexpand` during form interpretation
3. Replaced: Re-evaluation of defmacro with same name

### 4. Parsed Lambda-List (lambda-list-info)

Parsed representation of a lambda-list for parameter binding.

| Field | Type | Description |
|-------|------|-------------|
| required | list of symbols | Required parameters |
| optional | list of (name default supplied-p) | &optional parameters |
| rest | symbol or null | &rest parameter |
| keys | list of (keyword name default supplied-p) | &key parameters |
| allow-other-keys | boolean | &allow-other-keys flag |
| aux | list of (name value) | &aux bindings |

**Relationships:**
- Parsed from lambda-list at closure creation
- Used at invocation to bind arguments

### 5. Struct Type (interpreter-struct-type)

Represents a structure type defined by defstruct.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Structure name |
| slots | list of slot-info | Slot definitions |
| constructor | symbol | Constructor function name |
| copier | symbol | Copier function name |
| predicate | symbol | Type predicate name |
| include | symbol or null | Included parent struct |

**Relationships:**
- Stored in `*struct-registry*`
- Constructor/accessor functions stored in interpreter env

### 6. Struct Instance (interpreter-struct-instance)

Runtime instance of a structure.

| Field | Type | Description |
|-------|------|-------------|
| type | interpreter-struct-type | Structure type |
| slots | vector | Slot value storage |

**Relationships:**
- Type links to struct-type definition
- Slots indexed by slot position

### 7. Bootstrap Result (bootstrap-result)

Result of interpreter-based Stage 0 generation.

| Field | Type | Description |
|-------|------|-------------|
| success | boolean | Whether bootstrap succeeded |
| wasm-bytes | vector or null | Generated Wasm binary |
| modules-loaded | integer | Count of modules loaded |
| forms-compiled | integer | Count of forms compiled |
| errors | list | List of (module form error) tuples |
| elapsed-time | number | Total time in seconds |

**Relationships:**
- Contains aggregated compilation results
- Wasm bytes ready for validation and execution

---

## Registries (Global State)

### *macro-registry*

Global hash table mapping macro names to expanders.

```lisp
(defvar *macro-registry* (make-hash-table :test 'eq))
```

### *struct-registry*

Global hash table mapping struct names to type definitions.

```lisp
(defvar *struct-registry* (make-hash-table :test 'eq))
```

### *special-variables*

Global hash table for defvar/defparameter bindings.

```lisp
(defvar *special-variables* (make-hash-table :test 'eq))
```

---

## State Transitions

### Interpreter Session Lifecycle

```
[INIT] → [LOADED] → [RUNNING] → [COMPLETE/ERROR]
   │         │          │
   │         │          └── interpret-form returns or signals
   │         └── interpret-file loads modules
   └── make-interpreter-env creates root
```

### Bootstrap Pipeline

```
[INIT] → [MODULES_LOADED] → [COMPILING] → [VALIDATING] → [DONE]
   │            │               │              │
   │            │               │              └── wasm-tools validate
   │            │               └── compile-to-wasm per form
   │            └── interpret-file per module
   └── generate-stage0-via-interpreter
```

### Macro Expansion Flow

```
[FORM] → [CHECK_MACRO] → [EXPAND] → [RECHECK] → [INTERPRET]
             │              │           │
             │              │           └── Loop until no macro
             │              └── apply-macro-expander
             └── lookup in *macro-registry*
```

---

## Validation Rules

1. **Environment Integrity**: Parent chain must not contain cycles
2. **Closure Validity**: Body forms must be valid S-expressions
3. **Lambda-List Validity**: Must follow CL lambda-list syntax rules
4. **Struct Type Uniqueness**: Each struct name registered once per session
5. **Macro Arity**: Macro invocation must match lambda-list requirements
