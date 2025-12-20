# Bootstrap Blockers Analysis

Date: 2025-12-20 (Updated)

## Overview

This document catalogs the blockers preventing the clysm compiler from being fully self-hosting.

## Test Results (Latest)

With extended prerequisites (types.lisp, instructions.lisp, module.lisp, utf8.lisp, environment.lisp, compiler.lisp) and filtering SBCL-specific code:

| File | Forms Total | Forms Compiled | Success Rate | First Blocker |
|------|-------------|----------------|--------------|---------------|
| special-forms.lisp | 363 | 315 | 87% | COMPILE-DEFUN (user function) |
| primitives.lisp | 353 | 315 | 89% | COMPILE-DEFUN (user function) |
| codegen.lisp | 290 | 290 | 100% | N/A (fully compiles) |

## Fixed Issues (This Session)

### 1. codegen.lisp #'append Usage (FIXED)
- Replaced `#'append` with `(lambda (a b) (append a b))`
- Replaced `#'generate-code` with `(lambda (n) (generate-code n))`
- Reordered functions to put `primop-to-wasm-op` before `generate-code`
- Result: 100% compilation (290/290 forms)

### 2. DECLARE Special Form (FIXED)
- Added DECLARE special form that returns nil (ignores declarations)

### 3. Mutual Recursion in compiler.lisp (FIXED)
- Inlined `normalize-binding*` into `normalize-sbcl-internals`
- Added `#+sbcl` conditionals to SBCL-specific code paths
- Reordered `*self-hosting-mode*` check to come first

### 4. Missing CxR Accessors (FIXED)
- Added CAAR, CADR, CDAR, CDDR primitives

### 5. Missing Primitives (FIXED)
- Added STRING-TO-UTF8 (stub)
- Added INTERN-COMPILE-TIME-STRING (stub)
- Added INTERN-COMPILE-TIME-SYMBOL (stub)
- Added REMOVE-IF, REMOVE-IF-NOT

### 6. BLOCK in Lambda (catch/throw) (FIXED)
- Changed `find-primitive` to use catch/throw instead of return-from inside lambda
- return-from inside lambda requires non-local exit which WASM doesn't support directly

## Previously Fixed Issues

### Destructuring in defmacro Lambda Lists
- Added `generate-destructuring-bindings` function in macroexpand.lisp
- Recursively handles nested destructuring patterns like `(form env)`

### Missing Hash Table Primitives
- Added MAPHASH, HASH-TABLE-P

### Float Primitives
- Added FLOATP, FLOAT

### FUNCTION Special Form
- Added support for `#'name` and `#'(lambda ...)`
- Detects primitives and provides helpful error message

### IR Accessor Bug
- Fixed `ir-local-index` to `ir-local-ref-index` in codegen.lisp

## Remaining Blockers

### 1. User-Defined Function Recognition

**Affected Files:**
- All compiler source files

**Problem:**

When incrementally compiling forms, user-defined functions (via DEFUN) are not
recognized by later forms. The compiler starts fresh for each compilation and
doesn't maintain a registry of defined functions across forms.

**Example:**

```lisp
;; Form 100: (defun foo ...)
;; Form 200: (defun bar ... (foo x) ...)  ; Error: Unknown function FOO
```

**Solution:**

Implement function forward declaration or multi-pass compilation:
1. First pass: Collect all function signatures
2. Second pass: Compile function bodies with all signatures known

### 2. etypecase Type Specifiers (WARNING)

**Affected Files:**
- `src/compiler/codegen.lisp` (etypecase with IR types)

**Problem:**

`etypecase` uses IR struct types (ir-const, ir-local-ref, etc.) that aren't
registered as known types in `type-to-predicate`.

**Current Behavior:**

Generates warnings but continues. At runtime, the type checks would fail.

**Solution:**

Register struct types automatically when defstruct is processed, or check for
`typename-p` predicate existence.

### 3. SBCL-Specific Code

**Affected Files:**
- `src/compiler/compiler.lisp` (normalize-sbcl-internals)

**Problem:**

Code that handles SBCL internal forms (sb-impl::*, sb-int::*, etc.) cannot be
compiled when not running on SBCL.

**Solution:**

Use `#+sbcl` reader conditionals (already implemented) to exclude SBCL-specific
code when compiling for self-hosting.

## Test Command

```bash
sbcl --noinform --non-interactive --load /tmp/test-with-module.lisp
```
