# Data Model: Lexical Environment Function Export

**Date**: 2026-01-01
**Feature**: 001-lexenv-function-export

## Overview

This feature exports internal compiler functions to the public clysm package. No new data structures are introduced; existing structures are documented.

## Entities

### 1. Runtime Function Table Entry

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

```lisp
;; Storage: *runtime-function-table* (hash-table :test 'eq)
;; Key: symbol (function name)
;; Value: (cons runtime-name arity)

;; Example entry:
;; 'env-add-local -> (:$env-add-local-rt . 3)
```

| Field | Type | Description |
|-------|------|-------------|
| symbol | symbol | Function name (e.g., `env-add-local`) |
| runtime-name | keyword | Wasm function name (e.g., `:$env-add-local-rt`) |
| arity | (or integer null) | Parameter count, nil for variadic |

### 2. Package Export Declaration

**Location**: `src/clysm/package.lisp`

```lisp
;; In defpackage clysm:
(:import-from #:source-package #:symbol-name)
(:export #:symbol-name)
```

| Component | Description |
|-----------|-------------|
| source-package | Internal package where function is defined |
| symbol-name | Symbol to import and re-export |

## Functions to Export

### env-add-local

```lisp
(defun env-add-local (env name &optional (type :anyref))
  "Add a local variable with optional type and return its index."
  ...)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| env | compilation-env | Compilation environment |
| name | symbol | Variable name |
| type | keyword | Wasm type (default `:anyref`) |
| **Returns** | integer | Local variable index |

**Source**: `clysm/compiler/codegen/func-section`

### loop-keyword-eq

```lisp
(defun loop-keyword-eq (form keyword)
  "Check if FORM is equivalent to LOOP keyword."
  ...)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| form | t | Form to check |
| keyword | symbol | LOOP keyword to match |
| **Returns** | boolean | T if match, NIL otherwise |

**Source**: `clysm/lib/macros`

### numeric-literal-p

```lisp
(defun numeric-literal-p (ast)
  "Check if an AST node is a numeric literal."
  ...)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| ast | ast-node | AST node to check |
| **Returns** | boolean | T if numeric literal, NIL otherwise |

**Source**: `clysm/compiler/ast`

## State Transitions

N/A - Functions are stateless; no lifecycle management required.

## Validation Rules

1. **Symbol uniqueness**: Each exported symbol must not conflict with existing exports
2. **Arity consistency**: Runtime table arity must match function signature
3. **Package existence**: Source packages must be defined before import

## Relationships

```
clysm (public package)
├── imports from: clysm/compiler/codegen/func-section
│   └── env-add-local
├── imports from: clysm/lib/macros
│   └── loop-keyword-eq
└── imports from: clysm/compiler/ast
    └── numeric-literal-p

*runtime-function-table*
├── env-add-local -> (:$env-add-local-rt . 3)
├── loop-keyword-eq -> (:$loop-keyword-eq-rt . 2)
└── numeric-literal-p -> (:$numeric-literal-p-rt . 1)
```
