# Data Model: Setf Macros and Generalized References

**Feature**: 028-setf-generalized-refs
**Date**: 2025-12-27

## Overview

This document defines the data structures and entities for the setf/generalized reference system.

---

## 1. Core Entities

### 1.1 Setf Expander Registry

**Purpose**: Stores mappings from accessor names to their setf expander functions.

```lisp
(defstruct setf-expander-registry
  "Registry for setf expander functions."
  (table (make-hash-table :test 'eq) :type hash-table))
```

**Fields**:
| Field | Type | Description |
|-------|------|-------------|
| table | hash-table | Maps symbol → expander-fn |

**Lifecycle**:
- Created once per compilation unit
- Populated during macro registration
- Consulted during setf macro expansion

---

### 1.2 Setf Expansion (Five-Value Tuple)

**Purpose**: Represents how to read and write a generalized place.

**Structure** (returned as multiple values):
| Value | Name | Type | Description |
|-------|------|------|-------------|
| 1 | temps | list of symbols | Temporary variables for subform values |
| 2 | vals | list of forms | Forms to compute temp values |
| 3 | stores | list of symbols | Store variables (usually 1) |
| 4 | store-form | form | Code to store value and return it |
| 5 | access-form | form | Code to read current value |

**Example for `(car x)`**:
```lisp
temps:       (#:G1)
vals:        (x)
stores:      (#:NEW)
store-form:  (progn (rplaca #:G1 #:NEW) #:NEW)
access-form: (car #:G1)
```

---

### 1.3 Setf Expander Function

**Purpose**: Function that computes a setf expansion for a place form.

**Signature**:
```lisp
(lambda (place env)
  -> (values temps vals stores store-form access-form))
```

**Parameters**:
| Param | Type | Description |
|-------|------|-------------|
| place | form | The complete place form (e.g., `(car x)`) |
| env | compile-env | Compile-time environment |

---

## 2. Macro Data Structures

### 2.1 Setf Macro Form

**Input Form**: `(setf {place value}*)`

**Parsed Structure**:
| Component | Description |
|-----------|-------------|
| pairs | List of (place . value) pairs |

**Validation Rules**:
- Even number of arguments (excluding `setf`)
- Each place must be a valid place form

---

### 2.2 Psetf Macro Form

**Input Form**: `(psetf {place value}*)`

**Parsed Structure**: Same as setf

**Semantic Difference**: All values computed before any stores

---

### 2.3 Incf/Decf Macro Form

**Input Form**: `(incf place [delta])` or `(decf place [delta])`

**Parsed Structure**:
| Component | Type | Default |
|-----------|------|---------|
| place | form | required |
| delta | number-form | 1 |

---

### 2.4 Push/Pop/Pushnew Macro Forms

**Push**: `(push item place)`
| Component | Type |
|-----------|------|
| item | form |
| place | form |

**Pop**: `(pop place)`
| Component | Type |
|-----------|------|
| place | form |

**Pushnew**: `(pushnew item place &key test test-not key)`
| Component | Type | Default |
|-----------|------|---------|
| item | form | required |
| place | form | required |
| test | function | #'eql |
| test-not | function | nil |
| key | function | #'identity |

---

### 2.5 Rotatef/Shiftf Macro Forms

**Rotatef**: `(rotatef {place}+)`
| Component | Type | Count |
|-----------|------|-------|
| places | list of forms | ≥ 1 |

**Shiftf**: `(shiftf {place}+ newvalue)`
| Component | Type | Count |
|-----------|------|-------|
| places | list of forms | ≥ 1 |
| newvalue | form | 1 |

---

## 3. Standard Setf Expander Definitions

### 3.1 Cons Cell Accessors

| Accessor | Store Primitive | Access Primitive |
|----------|-----------------|------------------|
| car | rplaca | car |
| cdr | rplacd | cdr |
| first | rplaca | car |
| rest | rplacd | cdr |
| second | (rplaca (cdr x)) | cadr |
| third | (rplaca (cddr x)) | caddr |
| ... | ... | ... |
| tenth | (rplaca (nthcdr 9 x)) | (nth 9 x) |

### 3.2 List Accessors

| Accessor | Store Form | Notes |
|----------|------------|-------|
| nth | (rplaca (nthcdr n x) new) | Modifies n-th cons |
| nthcdr | N/A | Not typically setf-able |

### 3.3 Sequence Accessors

| Accessor | Underlying Operation |
|----------|---------------------|
| aref | array.set |
| svref | array.set (simple-vector) |
| elt | dispatch by sequence type |

### 3.4 Hash Table Accessor

| Accessor | Underlying Operation |
|----------|---------------------|
| gethash | hash-table-set |

### 3.5 Symbol Accessors

| Accessor | Struct Field |
|----------|-------------|
| symbol-value | $symbol.$value |
| symbol-function | $symbol.$function |
| symbol-plist | $symbol.$plist |

---

## 4. Entity Relationships

```text
┌─────────────────────────────────────┐
│       setf-expander-registry        │
│  ┌────────────────────────────────┐ │
│  │  table: hash-table             │ │
│  │    'car → expander-fn          │ │
│  │    'cdr → expander-fn          │ │
│  │    'aref → expander-fn         │ │
│  │    ...                         │ │
│  └────────────────────────────────┘ │
└─────────────────────────────────────┘
                 │
                 │ lookup
                 ▼
┌─────────────────────────────────────┐
│       setf expander function        │
│  (place, env) → expansion           │
└─────────────────────────────────────┘
                 │
                 │ returns
                 ▼
┌─────────────────────────────────────┐
│       setf expansion (5 values)     │
│  temps, vals, stores,               │
│  store-form, access-form            │
└─────────────────────────────────────┘
                 │
                 │ used by
                 ▼
┌─────────────────────────────────────┐
│       setf macro expansion          │
│  (setf place value) →               │
│  (let ((temps vals))                │
│    store-form)                      │
└─────────────────────────────────────┘
```

---

## 5. Validation Rules

### 5.1 Place Validation
- **Constant Check**: Place must not be a constant (e.g., 'x, 123)
- **Accessor Check**: If place is a call, accessor must have setf expander
- **Subform Check**: Nested places must also be valid places

### 5.2 Argument Count Validation
- setf/psetf: Even number of arguments
- incf/decf: 1 or 2 arguments
- push: 2 arguments
- pop: 1 argument
- rotatef: ≥ 1 arguments
- shiftf: ≥ 2 arguments

### 5.3 Type Validation (Runtime)
- incf/decf: Place value must be a number
- push/pop/pushnew: Place value should be a list (or nil)

---

## 6. State Transitions

### 6.1 Setf Expander Registration

```text
[Unregistered] ─── register-setf-expander ───> [Registered]
                          │
                          │ define-setf-expander / defsetf
                          ▼
                   [Available for setf expansion]
```

### 6.2 Place Modification

```text
[Initial Value] ─── setf ───> [New Value]
                      │
                      │ Returns new value
                      ▼
              [Caller receives new value]
```
