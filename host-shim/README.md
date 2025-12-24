# Host Shim for Clysm Stream I/O

This directory contains the JavaScript host shim that provides I/O functionality
for Clysm-compiled WebAssembly modules running in wasmtime or similar runtimes.

## Overview

Clysm compiles Common Lisp to WebAssembly using WasmGC (Garbage Collection proposal).
Stream I/O operations are implemented via FFI calls to host-provided functions,
avoiding WebAssembly linear memory entirely.

## Files

- `io-shim.js` - Host I/O function implementations
- `run-wasm.js` - Module loader and runner script

## Host Functions

The shim provides these functions under the `clysm:io` import module:

| Function | Signature | Description |
|----------|-----------|-------------|
| `write-char` | `(i32, i32) -> void` | Write Unicode codepoint to fd |
| `write-string` | `(i32, externref) -> void` | Write WasmGC string to fd |
| `read-char` | `(i32) -> i32` | Read codepoint from fd (-1 = EOF) |
| `read-line` | `(i32) -> externref` | Read line from fd (null = EOF) |

## File Descriptors

- 0: stdin (`*standard-input*`)
- 1: stdout (`*standard-output*`)
- 2: stderr (`*error-output*`)

## Usage

```bash
# Run a compiled Wasm module with the host shim
node run-wasm.js path/to/module.wasm

# Or with wasmtime (requires WASI shim adapter)
wasmtime --preload clysm:io=io-shim.wasm module.wasm
```

## Requirements

- Node.js 20+ (for WebAssembly GC support)
- wasmtime 14+ (for WasmGC)

## Character Encoding

All string data uses UTF-8 encoding at the FFI boundary (FR-022).

## Testing

The host shim includes support for testing via stdin simulation:

```javascript
import { setStdinContent } from './io-shim.js';

// Set simulated stdin for testing
setStdinContent("line1\nline2\nline3");

// Now read-char and read-line will read from this content
```

For command-line testing with a file as stdin:

```bash
node run-wasm.js module.wasm --stdin input.txt
```

## Architecture

```
┌─────────────────────────┐
│   Clysm Lisp Program    │
│  (write-char, format,   │
│   read-line, etc.)      │
└───────────┬─────────────┘
            │ FFI call
            ▼
┌─────────────────────────┐
│   WasmGC Module         │
│  (import "clysm:io")    │
└───────────┬─────────────┘
            │ Host function call
            ▼
┌─────────────────────────┐
│   Host Shim (io-shim)   │
│  Node.js / wasmtime     │
└───────────┬─────────────┘
            │
            ▼
    Host stdin/stdout/stderr
```

## No Linear Memory

This implementation follows the clysm constitution principle of avoiding linear memory:

- Strings pass through WasmGC `externref` type
- Characters pass as Unicode codepoints (`i32`)
- No shared memory buffers are used
- All data marshalling happens via WasmGC types
