# Data Model: Advanced Defmacro and Compile-Time Macro Expansion

**Feature**: 042-advanced-defmacro | **Date**: 2025-12-28

## Entity Overview

This feature introduces and extends entities related to macro definition, expansion, and compile-time environments.

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Macro System Entities                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────┐         ┌─────────────────────┐               │
│  │  macro-registry │◄────────│    macro-function   │               │
│  │    (hash-table) │         │      (closure)      │               │
│  └────────┬────────┘         └──────────┬──────────┘               │
│           │                             │                           │
│           │ contains                    │ stored in                 │
│           ▼                             │                           │
│  ┌─────────────────┐                   │                           │
│  │     symbol      │◄──────────────────┘                           │
│  │   (macro name)  │                                               │
│  └─────────────────┘                                               │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    macro-environment                         │   │
│  │  ┌───────────────┐    ┌───────────────┐    ┌─────────────┐  │   │
│  │  │ local-macros  │    │    parent     │    │ (reserved)  │  │   │
│  │  │  (registry)   │    │    (env)      │    │             │  │   │
│  │  └───────────────┘    └───────────────┘    └─────────────┘  │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                  macro-lambda-list-info                      │   │
│  │  ┌──────────┐  ┌───────────┐  ┌──────────┐  ┌───────────┐   │   │
│  │  │ &whole   │  │ &environment │ │ required │  │ &optional │   │   │
│  │  └──────────┘  └───────────┘  └──────────┘  └───────────┘   │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────────────────┐   │   │
│  │  │ &rest/   │  │  &key    │  │ &allow-other-keys        │   │   │
│  │  │ &body    │  │          │  │                          │   │   │
│  │  └──────────┘  └──────────┘  └──────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Entity Definitions

### 1. macro-registry (Existing - Extended)

**Purpose**: Global and local storage for macro name → expander function mappings.

**Structure** (existing in macro.lisp):
```lisp
(defstruct macro-registry
  "Registry for macro functions."
  (table (make-hash-table :test 'eq) :type hash-table))
```

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| register-macro | (registry name expander) → nil | Store macro expander |
| macro-function* | (registry name) → function/nil | Retrieve macro expander |
| macro-form-p | (registry form) → boolean | Test if form is macro call |
| unregister-macro | (registry name) → boolean | **NEW**: Remove macro |

**Validation Rules**:
- Name must be a symbol
- Expander must be a function accepting (form &optional env)
- Duplicate registration overwrites silently

---

### 2. macro-environment (New)

**Purpose**: Represents the lexical environment at macro expansion time, supporting `&environment` parameter and local macro lookups.

**Structure**:
```lisp
(defstruct macro-environment
  "Compile-time environment for macro expansion."
  (local-macros nil :type (or null macro-registry))
  (parent nil :type (or null macro-environment)))
```

**WasmGC Type** (Type Index 24):
```wat
(type $macro-environment (struct
  (field $local_macros anyref)    ;; macro-registry or null
  (field $parent anyref)))        ;; parent macro-environment or null
```

**Operations**:
| Operation | Signature | Description |
|-----------|-----------|-------------|
| make-macro-environment | (&key local-macros parent) → env | Create environment |
| env-macro-function | (env name) → function/nil | Lookup with parent chain |
| extend-environment | (env registry) → new-env | Create child environment |

**Validation Rules**:
- Parent chain must not be circular (enforced by construction)
- Lookup searches local-macros first, then parent chain
- Global registry is implicit parent of top-level environments

**Lifecycle**:
1. Created at macrolet/defmacro scope entry
2. Passed to macro expanders via &environment parameter
3. Discarded when scope exits (no persistence)

---

### 3. macro-lambda-list-info (New)

**Purpose**: Parsed representation of a macro lambda-list with all ANSI CL parameters.

**Structure**:
```lisp
(defstruct macro-lambda-list-info
  "Parsed macro lambda-list information."
  (whole-var nil :type (or null symbol))           ;; &whole binding
  (env-var nil :type (or null symbol))             ;; &environment binding
  (required nil :type list)                         ;; Required parameters
  (optional nil :type list)                         ;; &optional parameters
  (rest-var nil :type (or null symbol))            ;; &rest variable
  (rest-kind nil :type (or null (member :rest :body))) ;; :rest or :body
  (keys nil :type list)                             ;; &key parameters
  (allow-other-keys nil :type boolean))            ;; &allow-other-keys flag
```

**Validation Rules**:
- &whole must be first (if present)
- &environment can appear anywhere
- &body and &rest are mutually exclusive
- Destructuring patterns allowed in required/optional/key positions

**State Transitions**:
```
Input Lambda-List
       │
       ▼
  ┌────────────┐
  │ &whole?    │──yes──► Extract whole-var
  └─────┬──────┘         Continue with rest
        │ no
        ▼
  ┌────────────┐
  │ &environment? │──yes──► Extract env-var (anywhere)
  └─────┬──────┘           Continue with filtered list
        │ no
        ▼
  ┌────────────┐
  │ Parse rest │──► required, &optional, &rest/&body, &key, &allow-other-keys
  └────────────┘
```

---

### 4. defmacro-result (Existing - Extended)

**Purpose**: Intermediate representation of a parsed defmacro form.

**Structure** (extended):
```lisp
(defstruct defmacro-result
  "Parsed defmacro form."
  (name nil :type symbol)
  (lambda-list nil :type list)              ;; Raw lambda-list
  (lambda-info nil :type macro-lambda-list-info)  ;; NEW: Parsed info
  (body nil :type list)
  (docstring nil :type (or null string)))
```

**State Transitions**:
1. **Unparsed**: Raw defmacro form from reader
2. **Parsed**: defmacro-result with lambda-info populated
3. **Compiled**: Macro expander function registered

---

### 5. macro-function (Closure)

**Purpose**: The actual function that performs macro expansion.

**Signature**:
```lisp
;; Standard ANSI CL signature
(lambda (form &optional env) expanded-form)
```

**Internal Structure** (when &whole/&environment used):
```lisp
;; Generated wrapper
(lambda (form &optional env)
  (let ((,whole-var form)           ;; Bind &whole
        (,env-var env))             ;; Bind &environment
    (destructuring-bind ,remaining-lambda-list (cdr form)
      ,@body)))
```

**Storage**: In macro-registry, keyed by symbol name.

---

## Relationship Diagram

```
                    ┌─────────────────────┐
                    │  *global-macro-     │
                    │     registry*       │
                    └──────────┬──────────┘
                               │
              ┌────────────────┴────────────────┐
              │                                 │
              ▼                                 ▼
    ┌─────────────────┐               ┌─────────────────┐
    │  symbol: when*  │               │  symbol: cond*  │
    │  ───────────────│               │  ───────────────│
    │  expander: λ... │               │  expander: λ... │
    └─────────────────┘               └─────────────────┘
              │                                 │
              │ at expansion time               │
              ▼                                 ▼
    ┌─────────────────────────────────────────────────────┐
    │                  macro-environment                  │
    │  ┌─────────────────────────────────────────────┐   │
    │  │ local-macros: nil (or macrolet bindings)    │   │
    │  │ parent: nil (points to outer env)           │   │
    │  └─────────────────────────────────────────────┘   │
    └─────────────────────────────────────────────────────┘
              │
              │ passed as &environment
              ▼
    ┌─────────────────────────────────────────────────────┐
    │           Macro Expander Invocation                 │
    │  (funcall expander form env)                        │
    │        │                                            │
    │        ▼                                            │
    │  &whole binds to: form                              │
    │  &environment binds to: env                         │
    │  Remaining destructures: (cdr form)                 │
    └─────────────────────────────────────────────────────┘
```

---

## WasmGC Type Mapping

| Entity | WasmGC Type | Index | Notes |
|--------|-------------|-------|-------|
| macro-registry | anyref (hash-table) | N/A | Uses existing anyref hash |
| macro-environment | $macro-environment | 24 | New struct type |
| macro-function | $closure | 5 | Existing closure type |
| symbol (macro name) | $symbol | 3 | Existing symbol type |

---

## Constraints and Invariants

### Invariant 1: &whole Position
```
If &whole appears in a macro lambda-list, it MUST be the first element.
Violation: Compile-time error "MACRO-LAMBDA-LIST-MALFORMED"
```

### Invariant 2: Environment Chain Acyclicity
```
The parent chain of macro-environment structs is always acyclic.
Enforced by: Construction (child never points to self or descendant)
```

### Invariant 3: Macro Function Signature
```
All macro expanders accept exactly two arguments: (form &optional env)
The form is the complete macro call (including macro name).
The env is a macro-environment or nil.
```

### Invariant 4: Expansion Depth Limit
```
macroexpand stops after 1000 expansion steps.
Violation: Signals MACRO-EXPANSION-DEPTH-EXCEEDED condition.
```

### Invariant 5: Two-Value Return
```
macroexpand and macroexpand-1 always return two values:
  1. The (possibly expanded) form
  2. Boolean indicating whether expansion occurred
```
