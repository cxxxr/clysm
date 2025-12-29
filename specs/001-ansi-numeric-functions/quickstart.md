# Quickstart: ANSI Common Lisp Numeric Functions

**Branch**: `001-ansi-numeric-functions` | **Date**: 2025-12-29

## Overview

This feature implements ANSI Common Lisp numeric functions for the clysm compiler. After implementation, you can use standard Lisp numeric operations in compiled Wasm code.

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify tools are available
sbcl --version
wasmtime --version
wasm-tools --version
```

## Quick Test

### Trigonometric Functions

```lisp
;; In clysm REPL or compiled code
(sin 0.5)        ; => 0.479425538604203
(cos 0)          ; => 1.0
(atan 1 1)       ; => 0.7853981633974483 (pi/4)
```

### Bitwise Operations

```lisp
(logand #b1100 #b1010)  ; => 8 (#b1000)
(ash 1 10)               ; => 1024
(logcount 255)           ; => 8
```

### Complex Numbers

```lisp
(complex 3 4)            ; => #C(3 4)
(abs #C(3 4))            ; => 5.0
(sqrt -1)                ; => #C(0 1)
```

### Mathematical Functions

```lisp
(exp 1)                  ; => 2.718281828459045
(log 10)                 ; => 2.302585092994046
(expt 2 10)              ; => 1024
(gcd 48 18)              ; => 6
```

## Running Tests

```bash
# Run all numeric function tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# Run ANSI compliance tests
sbcl --load "tests/ansi/numbers-runner.lisp" --quit

# Check test compliance rate
sbcl --eval "(clysm/ansi-test:report-category :numbers)" --quit
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/lib/numeric.lisp` | Numeric function implementations |
| `src/clysm/runtime/complex.lisp` | Complex number type support |
| `src/clysm/compiler/codegen/func-section.lisp` | Codegen for numeric ops |
| `src/clysm/ffi/import-gen.lisp` | FFI math imports |
| `host-shim/math-shim.js` | JavaScript Math wrappers |

## Compilation Example

```lisp
;; Compile a function using numeric operations
(clysm:compile-to-wasm
  '(defun magnitude (x y)
     (sqrt (+ (* x x) (* y y)))))

;; Run with wasmtime
;; wasmtime run --preload math=host-shim/math-shim.js output.wasm
```

## Architecture Notes

1. **Transcendental functions** (sin, cos, exp, log, sqrt): Implemented via FFI calls to JavaScript Math object
2. **Bitwise operations** (logand, ash, etc.): Native Wasm i32/i64 instructions
3. **Complex numbers**: WasmGC struct type with real/imag float fields
4. **Integer functions** (gcd, lcm): Euclidean algorithm in compiled Lisp

## Troubleshooting

### "FFI function not found"

Ensure math imports are registered:
```lisp
(clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
```

### Complex number support

Complex type requires WasmGC. Check your runtime:
```bash
wasmtime --version  # Needs 14.0+ for WasmGC
```

### Type errors

Numeric functions perform automatic type coercion. If you see type errors, ensure inputs are numeric:
```lisp
(sin "0.5")  ; ERROR - string not number
(sin 0.5)    ; OK
(sin 1)      ; OK - integer coerced to float
```
