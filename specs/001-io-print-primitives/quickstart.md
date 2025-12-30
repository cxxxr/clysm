# Quickstart: I/O Print Primitives

**Feature**: 001-io-print-primitives
**Date**: 2025-12-31

## Overview

This guide shows how to use the I/O print primitives after implementation.

## Prerequisites

```bash
# Enter Nix development environment
nix develop

# Verify SBCL is available
sbcl --version
```

## Basic Usage

### Compiling Print Functions

```lisp
;; Load the Clysm compiler
(asdf:load-system :clysm)

;; Compile a function using print
(clysm:compile-to-wasm
  '(defun greet (name)
     (print name)
     name)
  :output-file "greet.wasm")

;; Validate the output
;; $ wasm-tools validate greet.wasm
```

### Format String Compilation

```lisp
;; Format to string (most common for self-hosting)
(clysm:compile-to-wasm
  '(defun format-greeting (name age)
     (format nil "Hello, ~A! You are ~D years old." name age))
  :output-file "format-greeting.wasm")

;; Format to stdout
(clysm:compile-to-wasm
  '(defun say-hello ()
     (format t "Hello, World!~%"))
  :output-file "say-hello.wasm")
```

### Supported Format Directives

| Directive | Description | Example |
|-----------|-------------|---------|
| `~A` | Aesthetic (human-readable) | `(format nil "~A" 42)` → `"42"` |
| `~S` | Standard (machine-readable) | `(format nil "~S" "hi")` → `"\"hi\""` |
| `~D` | Decimal integer | `(format nil "~D" 255)` → `"255"` |
| `~%` | Newline | `(format nil "a~%b")` → `"a\nb"` |
| `~&` | Fresh line (conditional newline) | `(format nil "~&x")` → `"\nx"` or `"x"` |
| `~~` | Literal tilde | `(format nil "100~~")` → `"100~"` |

## Running Compiled Code

### With Node.js Host Shim

```javascript
// run-print-example.js
import { readFileSync } from 'fs';
import { getImports } from './host-shim/io-shim.js';

const wasm = readFileSync('greet.wasm');
const imports = getImports();

const { instance } = await WebAssembly.instantiate(wasm, imports);
instance.exports.greet("World");
// Output:
// World
```

### With wasmtime

```bash
# Run with wasmtime (requires WASI shim)
wasmtime run --invoke greet greet.wasm "World"
```

## Testing

### Running Unit Tests

```bash
sbcl --eval "(asdf:test-system :clysm)"
```

### Specific Print Primitive Tests

```lisp
;; Test print compilation
(rove:run-test 'clysm/tests/io:print-compilation-test)

;; Test format compilation
(rove:run-test 'clysm/tests/io:format-nil-test)
```

### Contract Tests

```bash
# Validate generated Wasm structure
node tests/contract/io/print-wasm-test.js
```

## Verification Checklist

After implementation, verify these success criteria:

```lisp
;; SC-002: (format nil "~A" 42) => "42"
(let ((result (clysm:compile-and-run '(format nil "~A" 42))))
  (assert (string= result "42")))

;; SC-001: DEFUN with print compiles
(clysm:compile-to-wasm
  '(defun test-print (x) (print x) x)
  :output-file "/tmp/test-print.wasm")
(assert (zerop (uiop:run-program "wasm-tools validate /tmp/test-print.wasm")))
```

## Common Issues

### "FFI function not found"

Ensure the FFI environment is initialized:

```lisp
(clysm/ffi:initialize-ffi-environment)
```

### "Unknown format directive"

Only basic directives (~A, ~S, ~D, ~%, ~&, ~~) are supported. Advanced directives like ~{...~} are not yet implemented.

### Wasm validation failure

Check that all required imports are included in the module. Use:

```bash
wasm-tools print output.wasm | grep import
```

## Next Steps

After basic print primitives work:

1. Run Stage 1 generation to measure compilation rate improvement
2. Verify self-hosting progress with `sbcl --load build/stage1-complete.lisp`
3. Check `dist/stage1-report.json` for updated metrics
