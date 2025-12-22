# AST Parsing Contracts

**Feature**: 002-special-vars-compiler

## Contract: parse-defvar

**Input**: S-expression `(defvar name [init-form] [docstring])`

**Output**: `ast-defvar` node

### Scenarios

| Input | Expected Output |
|-------|-----------------|
| `(defvar *x*)` | `(make-ast-defvar :name '*x* :init-form nil :docstring nil)` |
| `(defvar *x* 10)` | `(make-ast-defvar :name '*x* :init-form (fixnum 10) :docstring nil)` |
| `(defvar *x* 10 "doc")` | `(make-ast-defvar :name '*x* :init-form (fixnum 10) :docstring "doc")` |

### Side Effects

- Registers `name` in `*special-variables*` registry

### Error Conditions

| Condition | Error |
|-----------|-------|
| `name` is not a symbol | Parse error: "defvar name must be a symbol" |
| More than 3 args | Parse error: "defvar takes at most 3 arguments" |

---

## Contract: parse-defparameter

**Input**: S-expression `(defparameter name init-form [docstring])`

**Output**: `ast-defparameter` node

### Scenarios

| Input | Expected Output |
|-------|-----------------|
| `(defparameter *x* 10)` | `(make-ast-defparameter :name '*x* :init-form (fixnum 10) :docstring nil)` |
| `(defparameter *x* 10 "doc")` | `(make-ast-defparameter :name '*x* :init-form (fixnum 10) :docstring "doc")` |

### Side Effects

- Registers `name` in `*special-variables*` registry

### Error Conditions

| Condition | Error |
|-----------|-------|
| `name` is not a symbol | Parse error: "defparameter name must be a symbol" |
| Missing init-form | Parse error: "defparameter requires an initial value" |
| More than 3 args | Parse error: "defparameter takes at most 3 arguments" |

---

## Contract: special-variable-p

**Input**: Symbol `name`, Compilation environment `env`

**Output**: Boolean

### Scenarios

| Context | Input | Output |
|---------|-------|--------|
| After `(defvar *x* 10)` | `'*x*` | `t` |
| No declaration | `'y` | `nil` |
| After `(defparameter *p* 1)` | `'*p*` | `t` |

### Implementation Note

Lookup in `*special-variables*` hash-table of compilation environment.
