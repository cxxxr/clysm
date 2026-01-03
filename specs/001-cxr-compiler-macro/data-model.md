# Data Model: CXR Compiler Macro Consolidation

**Branch**: `001-cxr-compiler-macro`
**Date**: 2026-01-03

## Overview

This feature introduces no new data structures. It defines a macro interface for generating existing function patterns.

## Entities

### E1: define-cxr-compiler (Macro)

A compile-time macro that generates compiler functions for [cXr accessors](resources/HyperSpec/Body/f_car_c.htm).

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| name | symbol | Yes | The cXr name (e.g., CAAR, CADR, CADDR) |
| ops | string | Yes | Operation sequence ("aa", "da", "dda") |

**Validation Rules**:
- `ops` must be a string (check-type)
- `ops` length must be > 0 (non-empty)
- `ops` must contain only 'a' and 'd' characters

**Output**: Generates a `defun` form with:
- Function name: `compile-{name}` (e.g., compile-caar)
- Signature: `(args env)`
- Body: `(compile-cxr-chain ops args env)`
- Docstring: Descriptive expansion

### E2: compile-cxr-chain (Existing Function)

Unchanged. Implements the actual Wasm code generation.

**Signature**: `(ops args env) → instruction-list`

**Parameters**:

| Parameter | Type | Description |
|-----------|------|-------------|
| ops | string | Operation sequence (left-to-right application) |
| args | list | Compiler arguments (must be exactly 1 element) |
| env | lexical-env | Compilation environment |

## Operation String Encoding

The operation string encodes CAR/CDR operations:
- `a` = [car](resources/HyperSpec/Body/f_car_c.htm) (struct field 0)
- `d` = [cdr](resources/HyperSpec/Body/f_car_c.htm) (struct field 1)

**Reading Order**: Left-to-right = innermost-to-outermost
- "da" = (car (cdr x)) = [cadr](resources/HyperSpec/Body/f_car_c.htm)
- "dda" = (car (cdr (cdr x))) = [caddr](resources/HyperSpec/Body/f_car_c.htm)

## Function Mapping

| cXr Name | Operation String | Expansion |
|----------|-----------------|-----------|
| [caar](resources/HyperSpec/Body/f_car_c.htm) | "aa" | (car (car x)) |
| [cadr](resources/HyperSpec/Body/f_car_c.htm) | "da" | (car (cdr x)) |
| [cdar](resources/HyperSpec/Body/f_car_c.htm) | "ad" | (cdr (car x)) |
| [cddr](resources/HyperSpec/Body/f_car_c.htm) | "dd" | (cdr (cdr x)) |
| [caaar](resources/HyperSpec/Body/f_car_c.htm) | "aaa" | (car (car (car x))) |
| [caadr](resources/HyperSpec/Body/f_car_c.htm) | "daa" | (car (car (cdr x))) |
| [cadar](resources/HyperSpec/Body/f_car_c.htm) | "ada" | (car (cdr (car x))) |
| [caddr](resources/HyperSpec/Body/f_car_c.htm) | "dda" | (car (cdr (cdr x))) |
| [cdaar](resources/HyperSpec/Body/f_car_c.htm) | "aad" | (cdr (car (car x))) |
| [cdadr](resources/HyperSpec/Body/f_car_c.htm) | "dad" | (cdr (car (cdr x))) |
| [cddar](resources/HyperSpec/Body/f_car_c.htm) | "add" | (cdr (cdr (car x))) |
| [cdddr](resources/HyperSpec/Body/f_car_c.htm) | "ddd" | (cdr (cdr (cdr x))) |

## State Transitions

N/A - This feature is stateless (compile-time macro).

## Relationships

```
define-cxr-compiler ──generates──► compile-{name} function
                                        │
                                        └──calls──► compile-cxr-chain
```
