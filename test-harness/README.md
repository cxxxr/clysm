# FFI Test Harnesses

This directory contains host-side test harnesses for FFI integration testing.

## Structure

- `wasmtime/` - Rust harness for wasmtime testing (WasmGC support required)
- `node/` - JavaScript harness for Node.js testing

## Running Tests

```bash
# Run all FFI tests across both environments
./run-ffi-tests.sh

# Run wasmtime tests only
cd wasmtime && cargo test

# Run Node.js tests only
node node/ffi-host.js
```
