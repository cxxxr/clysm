# Internal API Contracts: Stage 0 Capability Extension

**Date**: 2025-12-27
**Branch**: 038-stage0-extend

## Overview

This feature introduces internal APIs for:
1. Constant handling in AST and compilation
2. Form expansion (defstruct, define-condition)
3. Declaration filtering
4. Enhanced error reporting

## AST Module Contracts

### parse-defconstant-form

**Location**: `src/clysm/compiler/ast.lisp`

```lisp
(defun parse-defconstant-form (args) ...)
```

**Input**:
- `args`: CDR of `(defconstant name value [doc])` form

**Output**:
- `ast-defconstant` struct

**Errors**:
- `parse-error` if name is not a symbol
- `parse-error` if value-form is missing

**Contract**:
```lisp
;; Preconditions
(assert (listp args))
(assert (>= (length args) 2))  ; name + value required

;; Postconditions
(assert (typep result 'ast-defconstant))
(assert (symbolp (ast-defconstant-name result)))
(assert (typep (ast-defconstant-value-form result) 'ast-node))
```

### filter-declare-forms

**Location**: `src/clysm/compiler/ast.lisp`

```lisp
(defun filter-declare-forms (body-forms) ...)
```

**Input**:
- `body-forms`: List of forms from function/let body

**Output**:
- `(values filtered-body declarations)` where:
  - `filtered-body`: Body forms without declare
  - `declarations`: List of extracted declare specifiers

**Contract**:
```lisp
;; Preconditions
(assert (listp body-forms))

;; Postconditions
(assert (notany (lambda (f) (and (consp f) (eq (car f) 'declare)))
                filtered-body))
```

## Compilation Module Contracts

### compile-defconstant

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

```lisp
(defun compile-defconstant (ast env) ...)
```

**Input**:
- `ast`: `ast-defconstant` node
- `env`: Compilation environment

**Output**:
- Wasm instructions list for global definition

**Side Effects**:
- Adds constant to environment's constant registry

**Contract**:
```lisp
;; Preconditions
(assert (typep ast 'ast-defconstant))
(assert (typep env 'compilation-env))

;; Postconditions
;; Result is valid Wasm instruction sequence
(assert (listp result))
;; Constant is registered
(assert (constant-defined-p (ast-defconstant-name ast) env))
```

### fold-constant-expression

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

```lisp
(defun fold-constant-expression (form env) ...)
```

**Input**:
- `form`: S-expression to evaluate
- `env`: Environment with constant registry

**Output**:
- `(values value foldable-p)` where:
  - `value`: Computed value if foldable
  - `foldable-p`: T if successfully folded

**Supported Operations**:
- Arithmetic: `+`, `-`, `*`, `/`, `mod`, `rem`
- Comparison: `<`, `>`, `<=`, `>=`, `=`
- Logical: `and`, `or`, `not`
- Literals: numbers, characters, strings, symbols
- Constants: symbols with `+name+` convention

**Contract**:
```lisp
;; Preconditions
;; form is an S-expression

;; Postconditions
;; If foldable-p is T, value is a Lisp literal
;; If foldable-p is NIL, value is NIL
```

## Bootstrap Module Contracts

### expand-defstruct

**Location**: `build/bootstrap.lisp`

```lisp
(defun expand-defstruct (form) ...)
```

**Input**:
- `form`: `(defstruct name-and-options . slots)` form

**Output**:
- List of generated defun forms

**Contract**:
```lisp
;; Preconditions
(assert (eq (car form) 'defstruct))
(assert (>= (length form) 2))  ; At least (defstruct name)

;; Postconditions
;; Result is a list of defun forms
(assert (every (lambda (f) (eq (car f) 'defun)) result))
;; Includes constructor
(assert (some (lambda (f)
                (string-prefix-p "MAKE-" (symbol-name (cadr f))))
              result))
```

### expand-define-condition

**Location**: `build/bootstrap.lisp`

```lisp
(defun expand-define-condition (form) ...)
```

**Input**:
- `form`: `(define-condition name parents slots . options)` form

**Output**:
- `defclass` form

**Contract**:
```lisp
;; Preconditions
(assert (eq (car form) 'define-condition))
(assert (>= (length form) 3))  ; name, parents, slots

;; Postconditions
(assert (eq (car result) 'defclass))
(assert (eq (cadr result) (cadr form)))  ; Same name
```

### register-constant

**Location**: `build/bootstrap.lisp`

```lisp
(defun register-constant (name value registry) ...)
```

**Input**:
- `name`: Symbol for the constant
- `value`: Evaluated value
- `registry`: Constant registry hash-table

**Output**:
- `value` (for convenience)

**Side Effects**:
- Modifies registry

**Contract**:
```lisp
;; Preconditions
(assert (symbolp name))
(assert (hash-table-p registry))

;; Postconditions
(assert (eq (gethash name registry) value))
```

### lookup-constant

**Location**: `build/bootstrap.lisp`

```lisp
(defun lookup-constant (name registry) ...)
```

**Input**:
- `name`: Symbol to look up
- `registry`: Constant registry hash-table

**Output**:
- `(values value found-p)`

**Contract**:
```lisp
;; Preconditions
(assert (symbolp name))
(assert (hash-table-p registry))

;; Postconditions
;; If found-p is NIL, value is NIL
```

## Error Reporting Contracts

### record-failure

**Location**: `build/bootstrap.lisp`

```lisp
(defun record-failure (form error result) ...)
```

**Input**:
- `form`: Failed form
- `error`: Error condition
- `result`: `compile-result` struct

**Side Effects**:
- Increments `compile-result-failed`
- Adds to `operator-failures` hash-table
- Optionally adds to `operator-examples`

**Contract**:
```lisp
;; Preconditions
(assert (consp form))
(assert (typep result 'compile-result))

;; Postconditions
(let ((op (car form)))
  (assert (>= (gethash op (compile-result-operator-failures result) 0) 1)))
```

### generate-failure-report

**Location**: `build/bootstrap.lisp`

```lisp
(defun generate-failure-report (result stream) ...)
```

**Input**:
- `result`: `compile-result` struct with statistics
- `stream`: Output stream

**Output**:
- Writes formatted report to stream

**Report Sections**:
1. Summary line with percentage
2. Failures by operator (sorted by count)
3. Sample failures (first 3 per operator)

**Contract**:
```lisp
;; Preconditions
(assert (typep result 'compile-result))
(assert (output-stream-p stream))

;; Postconditions
;; Report contains percentage
;; Report lists operators with failures
```
