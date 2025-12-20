# Bootstrap Blockers Analysis

Date: 2025-12-20 (Updated - Session 3)

## Overview

This document catalogs the blockers preventing the clysm compiler from being fully self-hosting.

## Test Results (Latest)

Compiling core compiler files (environment, special-forms, primitives, compiler, codegen):

| Metric | Value |
|--------|-------|
| Total forms | 275 |
| expand-macros pass | 100% (275/275) |
| compile-module | Progressing (stopped at WASM infrastructure) |
| First blocker | Unknown function: ADD-FUNC-TYPE (from wasm/module.lisp) |

## Fixed Issues (Session 3)

### 1. Backquote Expansion (Major Fix)

**Problem:** Compiler was failing with "The value ,(REST BINDINGS) is not of type LIST" when processing forms containing backquotes.

**Root Causes:**
1. SBCL's backquote reader creates comma objects as elements in lists
2. The LET/LET* case in `normalize-sbcl-internals` was matching forms with comma objects in the bindings position
3. `(unquote-splicing ...)` as a literal list was treated as an error, not as a form to quote

**Fixes Applied:**
- Added check `(listp (second form))` to LET/LET* case to skip forms with comma objects
- Updated `expand-backquote` to treat `(unquote-splicing ...)` as a regular list in bootstrap mode
- Added `FUNCTION` to the list of special forms in `expand-macros`
- Added `SB-INT:NAMED-LAMBDA` handling in `expand-macros` (converts to LAMBDA)

### 2. Infinite Recursion Fix

**Problem:** `expand-macros` was recursing infinitely on `(FUNCTION (SB-INT:NAMED-LAMBDA ...))` forms.

**Fix:** Added `function` to the special forms list and added explicit handling for `sb-int:named-lambda`.

### 3. Moved *self-hosting-mode* Definition

**Problem:** `*self-hosting-mode*` was undefined in macroexpand.lisp due to load order.

**Fix:** Moved the definition to environment.lisp which loads first.

## Fixed Issues (Session 2)

### 1. SBCL-Specific Filter Improvements
- Changed test filter to only block truly internal SBCL packages (SB-IMPL, SB-INT, etc.)
- Allow SB-EXT symbols (public extensions like `GLOBAL` variable name)
- Allow `sb-int:quasiquote` as it's standard backquote syntax

### 2. Restored normalize-binding* Function
- Re-added `normalize-binding*` function with `#+sbcl` conditional
- Function is called from both `normalize-sbcl-internals` and `expand-macros`

### 3. New Primitives Added
- **LOGNOT**: Bitwise NOT using XOR with -1
- **KEYWORDP**: Check if symbol is a keyword
- **COERCE**: Stub for bootstrap (runs at compile time on host)
- **AREF**: Stub for bootstrap (runs at compile time on host)

### 4. Code Movement for Dependencies
- Moved `*heap-pointer-global*` earlier in compiler.lisp (before compile-string-literal)
- Moved `compile-string-literal` to compiler.lisp (from special-forms.lisp)
- Moved `compile-quoted-value` to compiler.lisp (from special-forms.lisp)

### 5. compile-string-literal Rewritten
- Changed from using AREF to using DOLIST over a list
- Uses `(coerce bytes-vec 'list)` then iterates with DOLIST
- Avoids direct array access for bootstrap compatibility

## Previously Fixed Issues (Session 1)

### codegen.lisp Fixes
- Replaced `#'append` with `(lambda (a b) (append a b))`
- Replaced `#'generate-code` with `(lambda (n) (generate-code n))`
- Reordered functions to put `primop-to-wasm-op` before `generate-code`
- Result: 100% compilation (290/290 forms)

### Special Form Additions
- Added DECLARE special form (ignores declarations)

### Mutual Recursion in compiler.lisp
- Inlined `normalize-binding*` into `normalize-sbcl-internals`
- Added `#+sbcl` conditionals to SBCL-specific code paths
- Moved `*self-hosting-mode*` check to top of cond

### Missing Primitives
- CAAR, CADR, CDAR, CDDR
- STRING-TO-UTF8 (stub)
- INTERN-COMPILE-TIME-STRING (stub)
- INTERN-COMPILE-TIME-SYMBOL (stub)
- REMOVE-IF, REMOVE-IF-NOT

### BLOCK in Lambda Fix
- Changed `find-primitive` to use catch/throw instead of return-from inside lambda

## Remaining Blockers

### 1. WASM Infrastructure Functions

**Problem:**

The compiler calls functions from the WASM module (e.g., `add-func-type`, `add-function`) that are defined in `src/wasm/module.lisp`.

**Current Behavior:**

When compiling only the core compiler files (environment, special-forms, primitives, compiler, codegen), these functions are unknown.

**Solution:**

Include the WASM infrastructure files in the bootstrap compilation:
- `src/wasm/types.lisp`
- `src/wasm/instructions.lisp`
- `src/wasm/module.lisp`
- `src/wasm/utf8.lisp`

### 2. Mutual Dependencies Between Functions

**Status:** Partially resolved - the two-pass system handles this when all forms are compiled together.

### 3. Compile-Time vs Runtime Operations

**Problem:**

Functions like `compile-string-literal` use operations (COERCE, etc.) that:
- Run at compile time on the host (SBCL)
- But the self-hosted compiler still needs to "compile" them
- We've added stubs but they don't do actual work at WASM runtime

**Implication:**

The compiler can compile itself, but the resulting WASM code won't actually
implement COERCE/AREF properly until we add full array support.

### 4. etypecase Type Specifiers (WARNING)

`etypecase` uses IR struct types (ir-const, ir-local-ref, etc.) that aren't
registered as known types in `type-to-predicate`.

**Solution:**

Register struct types automatically when defstruct is processed.

## Test Command

```bash
sbcl --noinform --non-interactive --load /tmp/test-compiler-bootstrap.lisp
```
