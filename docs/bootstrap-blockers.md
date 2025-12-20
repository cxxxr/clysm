# Bootstrap Blockers Analysis

Date: 2025-12-20 (Updated)

## Overview

This document catalogs the blockers preventing the clysm compiler from being fully self-hosting.

## Test Results (Latest)

With proper prerequisites (types.lisp, instructions.lisp, environment.lisp, ir.lisp):

| File | Forms Total | Forms Compiled | Success Rate | First Blocker |
|------|-------------|----------------|--------------|---------------|
| codegen.lisp | 290 | 288 | 99% | #'append (primitive as function) |
| special-forms.lisp | 362 | 284 | 78% | ADD-FUNC-TYPE (module.lisp) |
| primitives.lisp | 434 | 284 | 65% | ADD-FUNC-TYPE (module.lisp) |

## Fixed Issues

### 1. Destructuring in defmacro Lambda Lists (FIXED)
- Added `generate-destructuring-bindings` function in macroexpand.lisp
- Recursively handles nested destructuring patterns like `(form env)`

### 2. Missing Primitives (FIXED)
- Added MAPHASH, HASH-TABLE-P
- Added FLOATP, FLOAT

### 3. FUNCTION Special Form (FIXED)
- Added support for `#'name` and `#'(lambda ...)`
- Detects primitives and provides helpful error message

### 4. IR Accessor Bug (FIXED)
- Fixed `ir-local-index` to `ir-local-ref-index` in codegen.lisp

## Remaining Blockers

### 1. Primitives as First-Class Functions

**Affected Files:**
- `src/compiler/codegen.lisp` (uses `#'append` with reduce/mapcar)

**Problem:**

Code like `(reduce #'append ...)` tries to use APPEND as a first-class function.
Primitives are compiled inline at compile-time and don't have runtime function objects.

**Workaround:**

Replace `#'primitive` with a lambda wrapper:
```lisp
;; Instead of:
(reduce #'append lists)
;; Use:
(reduce (lambda (a b) (append a b)) lists)
```

### 2. ADD-FUNC-TYPE Dependency

**Affected Files:**
- `src/compiler/special-forms.lisp`
- `src/compiler/primitives.lisp`

**Problem:**

Both files use functions from `module.lisp` (like ADD-FUNC-TYPE) which isn't
included in the prerequisite files.

**Solution:**

Add `module.lisp` to prerequisites, which requires resolving its dependencies first.

### 3. etypecase Type Specifiers (WARNING)

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

## Test Command

```bash
sbcl --noinform --non-interactive --load /tmp/test-with-deps.lisp
```
