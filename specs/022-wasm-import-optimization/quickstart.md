# Quickstart: Wasm Import Optimization

**Feature**: 022-wasm-import-optimization
**Date**: 2025-12-25

## What This Feature Does

Enables Clysm-compiled Wasm modules to run directly in wasmtime without requiring I/O shims, when the code doesn't use I/O operations.

**Before**: All modules required host-shim due to unconditional FFI imports
**After**: Non-I/O modules run directly in wasmtime; I/O modules still work with shim

---

## Quick Test

### 1. Compile a Simple Expression

```lisp
;; In SBCL REPL
(ql:quickload :clysm)
(clysm/compiler:compile-to-wasm '(+ 1 2) :output "/tmp/add.wasm")
```

### 2. Execute with wasmtime

```bash
# Direct execution - no shim needed!
wasmtime --invoke _start /tmp/add.wasm
# Output: 3
```

### 3. Verify No Import Section

```bash
wasm-tools print /tmp/add.wasm | grep import
# No output = no import section
```

---

## Run ANSI Tests

With this feature, basic ANSI tests should now pass:

```lisp
(ql:quickload :clysm/ansi-test)
(clysm/ansi-test:run-ansi-tests :category "numbers")
;; Expected: ≥10% pass rate (was 0%)
```

---

## I/O Code Still Works

Code using I/O requires the host-shim as before:

```lisp
(clysm/compiler:compile-to-wasm '(print "hello") :output "/tmp/hello.wasm")
```

```bash
# Requires shim for I/O
node host-shim/run-wasm.js /tmp/hello.wasm
# Output: hello
```

---

## API (Unchanged)

The public API remains the same:

```lisp
;; Compile to bytes
(clysm/compiler:compile-to-wasm expr)

;; Compile to file
(clysm/compiler:compile-to-wasm expr :output "path.wasm")

;; Compile to WAT (debug)
(clysm/compiler:compile-to-wat expr)
```

The optimization is automatic - no flags needed.

---

## Implementation Notes

- I/O usage is detected by analyzing the compiled AST
- The detection is conservative: if any I/O function is called, imports are included
- Performance impact: negligible (single pass over function bodies)
- Module size: ~50-100 bytes smaller for non-I/O code

---

## Troubleshooting

### Module still requires shim

Check if your code indirectly uses I/O:
- `print`, `format`, `write-*` functions
- Debug output in macros
- Standard library functions that print

### wasmtime error: "unknown import"

Your code uses I/O. Use the host-shim:
```bash
node host-shim/run-wasm.js module.wasm
```
