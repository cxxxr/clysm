# Wasm Export Contracts: Phase 13D - True Self-Hosting

**Feature**: 001-true-self-hosting
**Date**: 2025-12-28

## Overview

This document defines the Wasm module interface for Stage 0 compiler. These exports enable the bootstrap pipeline and fixed-point verification.

---

## Module Metadata

| Property | Value |
|----------|-------|
| Module Name | clysm-stage0 |
| Target | WasmGC |
| Minimum Size | 1024 bytes (Stage 1/2) |
| Validation | wasm-tools validate |

---

## Exports

### compile_form

**Description**: Compile a single Lisp S-expression to Wasm bytecode.

**Signature**:
```wat
(export "compile_form" (func $compile_form
  (param $sexpr externref)
  (result externref)))
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| sexpr | externref | Pre-parsed S-expression (cons/symbol/fixnum tree) |

**Returns**:
| Type | Description |
|------|-------------|
| externref | Compiled Wasm bytes as byte array, or NIL on error |

**Behavior**:
1. Receives pre-parsed AST from host
2. Evaluates expression in empty environment
3. Generates Wasm IR from evaluation result
4. Emits binary Wasm bytes
5. Returns byte array or NIL if compilation fails

**Error Cases**:
| Condition | Behavior |
|-----------|----------|
| Unsupported form | Return NIL, log to console |
| Type error | Return NIL, log details |
| Division by zero | Return NIL, signal error |

**Example**:
```javascript
// Host-side invocation
const ast = parse_sexpr("(+ 1 2)");
const result = instance.exports.compile_form(ast);
// result is Uint8Array of valid Wasm
```

---

### compile_all

**Description**: Compile multiple forms into a complete Wasm module.

**Signature**:
```wat
(export "compile_all" (func $compile_all
  (param $forms externref)
  (result externref)))
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| forms | externref | List of S-expressions (cons cell chain) |

**Returns**:
| Type | Description |
|------|-------------|
| externref | Complete Wasm module binary, or NIL on error |

**Behavior**:
1. Receives list of pre-parsed forms
2. Compiles each form, collecting function bodies
3. Builds complete Wasm module with:
   - Type section (standard type indices)
   - Import section (FFI dependencies)
   - Function section
   - Export section (compile_form, compile_all, named defuns)
   - Code section
4. Returns valid Wasm binary

**Section Ordering** (per Wasm spec):
1. Type (1)
2. Import (2)
3. Function (3)
4. Export (7)
5. Code (10)

**Example**:
```javascript
// Compile minimal compiler source
const forms = parse_sexpr(`
  (progn
    (defun prim-add (a b) (+ a b))
    (defun compile-form (sexpr) ...)
    ...)
`);
const wasm = instance.exports.compile_all(forms);
// wasm is complete Stage 1 module
```

---

## Imports

### FFI Requirements

Stage 0 requires these host-provided functions:

| Import | Signature | Description |
|--------|-----------|-------------|
| `ffi.parse_sexpr` | `(string) -> externref` | Parse S-expression string to AST |
| `ffi.console_log` | `(externref) -> ()` | Debug logging |
| `ffi.bytes_to_extern` | `(ref $u8array) -> externref` | Export bytes to host |

**Import Section**:
```wat
(import "ffi" "parse_sexpr" (func $parse_sexpr
  (param externref) (result externref)))
(import "ffi" "console_log" (func $console_log
  (param externref)))
(import "ffi" "bytes_to_extern" (func $bytes_to_extern
  (param (ref $u8array)) (result externref)))
```

---

## Validation Contract

### Pre-conditions

| Condition | Verification |
|-----------|--------------|
| Input is valid AST | Host parser responsibility |
| Types are correct | ref.test before ref.cast |

### Post-conditions

| Condition | Verification |
|-----------|--------------|
| Output passes wasm-tools validate | `wasm-tools validate output.wasm` |
| Exports are callable | wasmtime can instantiate and invoke |
| Size >= 1KB (Stage 1) | `stat --format=%s dist/clysm-stage1.wasm` |
| Fixed-point (Stage 1 == Stage 2) | `cmp stage1.wasm stage2.wasm` |

---

## Test Cases

### Contract Tests

```lisp
;; test: compile_form returns valid Wasm
(deftest compile-form-valid-wasm
  (let ((result (compile-form '(+ 1 2))))
    (ok (not (null result)))
    (ok (valid-wasm-p result))))

;; test: compile_all produces >= 1KB
(deftest compile-all-minimum-size
  (let ((result (compile-all *bootstrap-forms*)))
    (ok (>= (length result) 1024))))

;; test: compile_all output has exports
(deftest compile-all-has-exports
  (let ((wasm (compile-all *bootstrap-forms*)))
    (ok (has-export-p wasm "compile_form"))
    (ok (has-export-p wasm "compile_all"))))
```

### Integration Tests

```bash
# Fixed-point verification
./scripts/verify-fixpoint.sh --json
# Expected: {"status": "ACHIEVED", "stage1_size": ">= 1024"}
```
