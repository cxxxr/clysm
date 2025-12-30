# Contract: Stage 1 Wasm Exports

**Created**: 2025-12-30
**Feature**: 001-bootstrap-fixpoint

## Overview

Stage 1 Wasm module must export the following functions for self-hosting bootstrap.

## Exported Functions

### compile_form

Compiles a single Lisp form to Wasm bytecode.

**Export Name**: `compile_form`
**WAT Signature**: `(func $compile_form (param anyref) (result anyref))`
**Export Index**: 4 (after FFI imports 0-3)

**Parameters**:
- Input: `anyref` - Lisp S-expression (parsed form)

**Returns**:
- `anyref` - On success: Array of bytes (Wasm bytecode)
- `anyref` - On failure: Condition object with error details

**Behavior**:
1. Validate input is valid Lisp form
2. Compile form to Wasm IR
3. Encode IR to Wasm binary format
4. Return bytecode or error condition

### compile_all

Compiles all compiler source modules.

**Export Name**: `compile_all`
**WAT Signature**: `(func $compile_all (result anyref))`
**Export Index**: 5

**Parameters**: None

**Returns**:
- `anyref` - Complete Stage N+1 Wasm module as byte array

**Behavior**:
1. Load all source files in dependency order
2. Compile each form via internal compile logic
3. Aggregate into single Wasm module
4. Return complete binary

### _initialize

Runtime initialization function.

**Export Name**: `_initialize`
**WAT Signature**: `(func $_initialize)`
**Export Index**: 6

**Parameters**: None
**Returns**: None (void)

**Behavior**:
1. Initialize global state
2. Set up NIL, T, UNBOUND singletons
3. Initialize symbol table
4. Called automatically before _start or first compile_form

### _start

Main entry point (WASI convention).

**Export Name**: `_start`
**WAT Signature**: `(func $_start)`
**Export Index**: 8 (after user exports)

**Parameters**: None
**Returns**: None (void)

**Behavior**:
1. Call _initialize if not already done
2. Enter REPL or process command-line args
3. Exit with appropriate code

## FFI Imports (Indices 0-3)

Stage 1 requires these imports from the host:

| Index | Module | Name | Signature |
|-------|--------|------|-----------|
| 0 | clysm:io | write_string | `(func (param anyref))` |
| 1 | clysm:io | read_line | `(func (result anyref))` |
| 2 | clysm:fs | read_file | `(func (param anyref) (result anyref))` |
| 3 | clysm:fs | write_file | `(func (param anyref anyref) (result i32))` |

## Export Section Binary Format

```wat
(export "compile_form" (func 4))
(export "compile_all" (func 5))
(export "_initialize" (func 6))
(export "_start" (func 8))
```

**Section ID**: 7 (Export Section)

**Binary Structure**:
```
07          ; Section ID
<size>      ; LEB128 section size
04          ; Number of exports

0C          ; Name length: 12
compile_form
00 04       ; Kind=func, Index=4

0B          ; Name length: 11
compile_all
00 05       ; Kind=func, Index=5

0B          ; Name length: 11
_initialize
00 06       ; Kind=func, Index=6

06          ; Name length: 6
_start
00 08       ; Kind=func, Index=8
```

## Validation

Use `wasm-tools print` to verify exports:

```bash
wasm-tools print dist/clysm-stage1.wasm | grep -A1 '(export'
```

Expected output:
```wat
(export "compile_form" (func $compile_form))
(export "compile_all" (func $compile_all))
(export "_initialize" (func $_initialize))
(export "_start" (func $_start))
```
