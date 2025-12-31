# Compilation API Contract: Rounding Functions

**Created**: 2025-12-31

## Overview

This document defines the internal API contract for compiling rounding functions in the Clysm compiler.

## Functions

### compile-floor

```lisp
(defun compile-floor (args env) ...)
```

**Purpose**: Generate Wasm instructions for `(floor dividend divisor)` or `(floor number)`.

**Parameters**:
- `args`: List of 1 or 2 AST nodes
- `env`: Compilation environment

**Returns**: List of Wasm instructions

**Contract**:
- MUST set `$mv_count` to 2
- MUST store remainder in `$mv_buffer[0]`
- MUST return quotient on stack (rounded toward -∞)
- MUST handle both i31ref and $float arguments
- WHEN args has 1 element, MUST treat divisor as 1

### compile-ceiling

```lisp
(defun compile-ceiling (args env) ...)
```

**Purpose**: Generate Wasm instructions for `(ceiling dividend divisor)` or `(ceiling number)`.

**Parameters**: Same as `compile-floor`

**Contract**:
- Same as `compile-floor` except quotient rounded toward +∞

### compile-round

```lisp
(defun compile-round (args env) ...)
```

**Purpose**: Generate Wasm instructions for `(round dividend divisor)` or `(round number)`.

**Parameters**: Same as `compile-floor`

**Contract**:
- Same as `compile-floor` except quotient rounded to nearest (ties to even)

### compile-ffloor / compile-fceiling / compile-fround

```lisp
(defun compile-ffloor (args env) ...)
(defun compile-fceiling (args env) ...)
(defun compile-fround (args env) ...)
```

**Purpose**: Generate Wasm instructions for float-result rounding functions.

**Contract**:
- Same as integer variants except:
  - Quotient MUST be returned as $float (not i31ref)
  - Arguments MUST be converted to f64 for computation

## Wasm Output Contract

### Instruction Sequence Pattern

All rounding functions MUST generate instructions following this pattern:

```wasm
;; 1. Set multiple value count
(i32.const 2)
(global.set $mv_count)

;; 2. Evaluate and unbox arguments
<compile dividend>
<unbox to i32 or f64>
<compile divisor>  ;; or (i32.const 1) for single-arg
<unbox to i32 or f64>

;; 3. Compute quotient
<apply rounding operation>

;; 4. Compute remainder = dividend - quotient * divisor
<remainder computation>

;; 5. Store remainder in mv-buffer
(global.get $mv_buffer)
(i32.const 0)
<remainder value>
(array.set $mv_array)

;; 6. Box and return quotient
<box quotient>
```

### Type Handling Contract

| Input Types | Computation Type | Quotient Output |
|------------|-----------------|-----------------|
| Both i31ref | i32 | i31ref |
| Either $float | f64 | i31ref (floor/ceiling/round) |
| Either $float | f64 | $float (ffloor/fceiling/fround) |

## Validation Contract

### Unit Tests MUST Verify

1. `(floor 7 2)` → quotient=3, remainder=1
2. `(floor -7 2)` → quotient=-4, remainder=1
3. `(ceiling 7 2)` → quotient=4, remainder=-1
4. `(ceiling -7 2)` → quotient=-3, remainder=-1
5. `(round 7 4)` → quotient=2, remainder=-1
6. `(round 9 4)` → quotient=2, remainder=1 (banker's rounding)
7. Single-argument forms
8. Float argument handling
9. ffloor/fceiling/fround return float quotients

### Contract Tests MUST Verify

1. Generated Wasm passes `wasm-tools validate`
2. `$mv_count` is set to 2
3. `$mv_buffer` access uses correct type index (20)
4. Correct Wasm opcodes used:
   - `f64.floor` (0x9B) for floor/ffloor
   - `f64.ceil` (0x9C) for ceiling/fceiling
   - `f64.nearest` (0x9E) for round/fround

## Error Handling Contract

| Error Condition | Behavior |
|----------------|----------|
| Zero divisor | Signal division-by-zero error |
| Wrong argument count | Compile-time error |
| Non-numeric argument | Type error at runtime |

## Integration Points

### AST Parser (existing)

The AST parser already recognizes floor/ceiling/round in `parse-rounding-form` (ast.lisp:998-1010).

### Builtin Dispatcher (to be modified)

Add cases to `compile-builtin` in func-section.lisp:

```lisp
(floor (compile-floor args env))
(ceiling (compile-ceiling args env))
(round (compile-round args env))
(ffloor (compile-ffloor args env))
(fceiling (compile-fceiling args env))
(fround (compile-fround args env))
```

### Primitive Operators (to be modified)

Add to `*primitive-operators*` in analyzer/free-vars.lisp:

```lisp
floor ceiling round ffloor fceiling fround
```
