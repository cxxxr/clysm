# Data Model: Destructuring-Bind Macro

**Date**: 2025-12-27
**Feature**: 031-destructuring-bind-macro

## Entity Overview

This feature operates at compile-time (macro expansion). Data structures represent parsed lambda-lists and generated binding code.

## Core Entities

### 1. Parsed Lambda-List

**Purpose**: Intermediate representation of a destructuring lambda-list after parsing.

```text
parsed-lambda-list
├── whole-var         : symbol | nil
│                       Variable bound to entire list (after &whole)
├── required-params   : list of param-spec
│                       Required positional parameters
├── optional-params   : list of optional-param-spec
│                       Parameters after &optional
├── rest-var          : symbol | nil
│                       Variable after &rest or &body
├── key-params        : list of key-param-spec
│                       Parameters after &key
└── allow-other-keys-p: boolean
                        Whether &allow-other-keys was present
```

**Validation Rules**:
- `whole-var` must appear before any other parameters
- Lambda-list keywords must appear in order: `&whole`, required, `&optional`, `&rest`/`&body`, `&key`
- At most one `&rest` or `&body` variable
- No duplicate parameter names across all sections

### 2. Param-Spec (Required Parameter)

**Purpose**: Represents a required positional parameter.

```text
param-spec
├── type         : :variable | :nested
├── var          : symbol (when type = :variable)
└── nested-list  : parsed-lambda-list (when type = :nested)
```

**Examples**:
- `a` → `(:variable . a)`
- `(x y)` → `(:nested . <parsed-lambda-list for (x y)>)`

### 3. Optional-Param-Spec

**Purpose**: Represents an `&optional` parameter with optional default and supplied-p.

```text
optional-param-spec
├── param        : param-spec
│                  The parameter (variable or nested)
├── default-form : form | nil
│                  Expression evaluated if not supplied (default: nil)
└── supplied-p   : symbol | nil
                   Variable bound to T/NIL indicating if supplied
```

**Examples**:
- `b` → `{param: b, default: nil, supplied-p: nil}`
- `(b 10)` → `{param: b, default: 10, supplied-p: nil}`
- `(b 10 b-supplied-p)` → `{param: b, default: 10, supplied-p: b-supplied-p}`
- `((x y) '(1 2))` → Nested with default

### 4. Key-Param-Spec

**Purpose**: Represents a `&key` parameter with keyword name, default, and supplied-p.

```text
key-param-spec
├── keyword      : keyword symbol
│                  The keyword used in the plist (:foo)
├── param        : param-spec
│                  The variable or nested pattern
├── default-form : form | nil
│                  Expression evaluated if not supplied
└── supplied-p   : symbol | nil
                   Variable bound to T/NIL indicating if supplied
```

**Examples**:
- `x` → `{keyword: :x, param: x, default: nil, supplied-p: nil}`
- `(x 10)` → `{keyword: :x, param: x, default: 10, supplied-p: nil}`
- `(((:my-key x) 10) x-p)` → `{keyword: :my-key, param: x, default: 10, supplied-p: x-p}`

## State Transitions

### Lambda-List Parsing State Machine

```text
┌─────────────────────────────────────────────────────────────────┐
│                       Parser States                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   START ─┬─(&whole)──► AFTER-WHOLE ─┬─(symbol/list)──► REQUIRED│
│          │                          │                          │
│          └─(symbol/list)───────────►└───────────────┐          │
│                                                      ▼          │
│                                                  REQUIRED ──────┤
│                                                      │          │
│                                      ┌───(&optional)─┘          │
│                                      ▼                          │
│                                   OPTIONAL ─────────────────────┤
│                                      │                          │
│                       ┌───(&rest/&body)─┘                       │
│                       ▼                                         │
│                     REST ───────────────────────────────────────┤
│                       │                                         │
│                ┌───(&key)─┘                                     │
│                ▼                                                │
│              KEY ───────────────────────────────────────────────┤
│                │                                                │
│     ┌───(&allow-other-keys)                                     │
│     ▼                                                           │
│  ALLOW-KEYS ────────────────────────────────────────────► END   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Transition Rules**:
- Forward-only transitions (no going back to earlier states)
- Unknown lambda-list keyword → `program-error`
- Duplicate keyword → `program-error`
- `&rest` and `&body` mutually exclusive

## Expansion Output Structure

The macro expands to nested `let` forms with runtime checks:

```text
(destructuring-bind (a (b c) &optional d &rest e) form
  body)

Expands to:

(let ((#:list form))
  ;; Required parameter 'a'
  (unless (consp #:list)
    (error 'program-error ...))
  (let ((a (car #:list)))
    (let ((#:list-1 (cdr #:list)))
      ;; Nested required (b c)
      (unless (consp #:list-1)
        (error 'program-error ...))
      (let ((#:temp (car #:list-1)))
        (unless (consp #:temp)
          (error 'program-error ...))
        (let ((b (car #:temp))
              (c (cadr #:temp)))
          (let ((#:list-2 (cdr #:list-1)))
            ;; Optional 'd'
            (let ((d (if (consp #:list-2) (car #:list-2) nil)))
              (let ((#:list-3 (if (consp #:list-2) (cdr #:list-2) nil)))
                ;; Rest 'e'
                (let ((e #:list-3))
                  body)))))))))
```

## Error Conditions

| Condition | When Signaled | Arguments |
|-----------|---------------|-----------|
| `program-error` | Not enough elements | `:format-control`, `:format-arguments` |
| `program-error` | Too many elements (no &rest) | `:format-control`, `:format-arguments` |
| `program-error` | Unknown keyword (no &allow-other-keys) | `:format-control`, `:format-arguments` |
