# Quickstart: Complete FFI Foundation

**Feature**: 027-complete-ffi
**Date**: 2025-12-27

## Prerequisites

```bash
# Enter development shell
nix develop

# Verify tools are available
sbcl --version
wasmtime --version
wasm-tools --version
```

## Basic Usage

### 1. Declare a Host Function

```lisp
(in-package :cl-user)
(use-package :clysm/ffi)

;; Declare host.log for console output
(ffi:define-foreign-function console-log "host.log" (:string) :void)

;; Declare host.add for arithmetic
(ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum)
```

### 2. Use the Function in Lisp Code

```lisp
(defun greet (name)
  (console-log (concatenate 'string "Hello, " name "!")))

(defun calculate ()
  (host-add 40 2))  ; Returns 42
```

### 3. Compile to Wasm

```lisp
(require :clysm/compiler)
(clysm/compiler:compile-to-wasm
  '(progn
     (ffi:define-foreign-function console-log "host.log" (:string) :void)
     (console-log "Hello from Lisp!"))
  :output "hello.wasm")
```

### 4. Run with Host Environment

**JavaScript (Node.js)**:
```javascript
const { readFileSync } = require('fs');
const bytes = readFileSync('hello.wasm');

const imports = {
  host: {
    log: (msg) => console.log(msg),
    add: (a, b) => a + b
  }
};

WebAssembly.instantiate(bytes, imports).then(({ instance }) => {
  instance.exports._start();
});
```

**wasmtime**:
```bash
# Requires host shim implementation
wasmtime --invoke _start hello.wasm
```

## Export a Lisp Function

```lisp
;; Define the Lisp function
(defun my-add (a b)
  (+ a b))

;; Export it for host access
(ffi:export-function my-add :as "add" :signature ((:fixnum :fixnum) :fixnum))
```

Host can then call:
```javascript
const result = instance.exports.add(10, 20);  // Returns 30
```

## Dynamic Host Calls

For functions not known at compile time:

```lisp
;; Call any host function by name
(ffi:call-host "host.random")           ; → 0.42
(ffi:call-host "host.add" 1 2)          ; → 3
```

## Error Handling

```lisp
(handler-case
    (console-log "test")
  (ffi:ffi-host-error (e)
    (format t "Host error: ~A~%" (ffi:ffi-host-error-message e)))
  (ffi:ffi-type-error (e)
    (format t "Type error: expected ~A~%" (ffi:ffi-type-error-expected-type e))))
```

## Marshal Types Reference

| Type | Lisp Value | Host Value | Notes |
|------|------------|------------|-------|
| `:fixnum` | integer | number | 31-bit signed range |
| `:float` | double-float | number | IEEE 754 double |
| `:string` | string | string | UTF-8 encoded |
| `:boolean` | t/nil | boolean | nil=false |
| `:anyref` | any | any | Passthrough |
| `:void` | N/A | undefined | No return |

## Running Tests

```bash
# Run all FFI tests
nix develop -c sbcl --eval "(asdf:test-system :clysm/ffi)"

# Or individually
nix develop -c sbcl --eval "(rove:run :clysm/ffi-test)"
```

## Verifying Wasm Output

```bash
# Validate generated module
wasm-tools validate hello.wasm

# Inspect as WAT
wasm-tools print hello.wasm > hello.wat
```

## Troubleshooting

### "FFI function called before Wasm compilation"

The function stub was called in interpreter mode. FFI functions only work in compiled Wasm modules.

### "Unknown function: ..."

Dynamic `call-host` couldn't find the function. Ensure host provides it in imports.

### "ffi-type-error: expected :fixnum"

Value too large for 31-bit fixnum. Use `:anyref` for arbitrary values or check bounds.

## Next Steps

1. Review [API Contract](./contracts/ffi-api.md) for full interface details
2. See [Data Model](./data-model.md) for entity relationships
3. Check [Research](./research.md) for design decisions
