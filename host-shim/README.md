# Host Shim for Clysm Stream I/O

This directory contains the JavaScript host shim that provides I/O functionality
for Clysm-compiled WebAssembly modules running in wasmtime or similar runtimes.

## Overview

Clysm compiles Common Lisp to WebAssembly using WasmGC (Garbage Collection proposal).
Stream I/O operations are implemented via FFI calls to host-provided functions,
avoiding WebAssembly linear memory entirely.

## Files

- `io-shim.js` - Host I/O function implementations (clysm:io namespace)
- `fs-shim.js` - Host filesystem function implementations (clysm:fs namespace)
- `run-wasm.js` - General module loader and runner script
- `stage1-runner.js` - Stage 1 compiler execution environment

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

## Stage 1 Runner

The `stage1-runner.js` script provides the execution environment for the Stage 1
Clysm compiler (dist/clysm-stage1.wasm). It merges all required FFI imports and
handles the bootstrap pipeline.

### Usage

```bash
# Run Stage 1 with defaults
node stage1-runner.js

# Run with verbose output
node stage1-runner.js --verbose

# Run with custom Wasm path
node stage1-runner.js ./custom-stage1.wasm

# Compile a Lisp expression (if compile_form is exported)
node stage1-runner.js --expr "(+ 1 2)"
```

### Exit Codes

| Code | Constant | Description |
|------|----------|-------------|
| 0 | EXIT_SUCCESS | Operation completed successfully |
| 1 | EXIT_PARTIAL | Partial success (some operations failed) |
| 2 | EXIT_FAILURE | Operation failed |
| 3 | EXIT_MISSING_DEP | Required dependency not found |
| 77 | EXIT_SKIP | Known limitation (export not available) |

### Convenience Scripts

Shell script wrappers are provided in `scripts/`:

```bash
# Run Stage 1
./scripts/run-stage1.sh

# Generate Stage 2 (may exit 77 if compile_all not exported)
./scripts/generate-stage2.sh
```

### FFI Requirements

Stage 1 requires these FFI namespaces:

- `clysm:io`: write-char, write-string, read-char, read-line
- `clysm:fs`: open, close, read-all, write-all
