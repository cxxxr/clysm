# Quickstart: Stage 0 Complete Compiler

**Date**: 2025-12-28
**Feature**: 045-stage0-complete-compiler

## Prerequisites

Ensure the following tools are available:

```bash
# Enter Nix development shell (provides all tools)
nix develop

# Verify tools
sbcl --version          # SBCL 2.4+
wasmtime --version      # wasmtime 14+
wasm-tools --version    # wasm-tools 1.0+
node --version          # Node.js 18+
```

## Quick Verification

Check current Stage 0 status:

```bash
# Check Stage 0 binary
ls -la dist/clysm-stage0.wasm

# Validate current binary
wasm-tools validate dist/clysm-stage0.wasm

# Check exports (will be empty for placeholder)
wasm-tools print dist/clysm-stage0.wasm | grep -A5 "(export"
```

## Development Workflow

### 1. Generate Stage 0

**Using SBCL (recommended)**:
```bash
sbcl --load build/stage0-complete.lisp
```

**Using Interpreter (SBCL-free)**:
```bash
sbcl --load build/bootstrap-interp.lisp
```

### 2. Validate Stage 0

```bash
# Validate Wasm binary
wasm-tools validate dist/clysm-stage0.wasm

# Check exports
wasm-tools print dist/clysm-stage0.wasm | grep "(export"

# Expected exports:
# (export "compile_form" (func $compile_form))
# (export "compile_all" (func $compile_all))
# (export "_initialize" (func $_initialize))
```

### 3. Test Single Expression Compilation

```bash
# Compile a simple expression via CLI
wasmtime run dist/clysm-stage0.wasm --invoke compile_form '(+ 1 2)'

# Or via Node.js host shim
node host-shim/stage1-host.js --stage0 dist/clysm-stage0.wasm \
  --mode compile --input /dev/stdin <<< '(+ 1 2)'
```

### 4. Generate Stage 1

```bash
# Run Stage 0 to compile all source modules
node host-shim/stage1-host.js \
  --stage0 dist/clysm-stage0.wasm \
  --output dist/clysm-stage1.wasm \
  --report dist/stage1-report.json

# Or using the shell script
./scripts/run-stage1-gen.sh
```

### 5. Generate Stage 2

```bash
# Run Stage 1 to generate Stage 2
node host-shim/stage1-host.js \
  --mode compile \
  --stage1 dist/clysm-stage1.wasm \
  --output dist/clysm-stage2.wasm
```

### 6. Verify Fixed-Point

```bash
# Run fixpoint verification
./scripts/verify-fixpoint.sh

# Expected output for success:
# Status: ACHIEVED
# Exit code: 0
```

## Running Tests

### Unit Tests

```bash
# Run all Stage 0 unit tests
sbcl --eval '(ql:quickload :clysm/tests)' \
     --eval '(rove:run :clysm/tests/stage0)' \
     --quit

# Run specific test file
sbcl --eval '(ql:quickload :clysm/tests)' \
     --eval '(rove:run-test :clysm/tests/stage0 :test-compile-form)' \
     --quit
```

### Contract Tests

```bash
# Validate Wasm output structure
sbcl --eval '(ql:quickload :clysm/tests)' \
     --eval '(rove:run :clysm/tests/contract/stage0)' \
     --quit
```

### Integration Tests

```bash
# Full end-to-end test
sbcl --eval '(ql:quickload :clysm/tests)' \
     --eval '(rove:run :clysm/tests/integration/stage0)' \
     --quit

# Or via shell
./scripts/test-stage0.sh
```

## Common Tasks

### Check Compilation Progress

```bash
# View Stage 1 generation report
cat dist/stage1-report.json | jq '.summary'

# View compilation rate
cat dist/stage1-report.json | jq '.summary.coverage_pct'
```

### Debug Compilation Failures

```bash
# View failed modules
cat dist/stage1-report.json | jq '.modules[] | select(.failed > 0)'

# View specific failures
cat dist/stage1-report.json | jq '.modules[].failures'
```

### Examine Binary Differences

```bash
# If fixpoint not achieved, find first difference
./scripts/diff-stages.sh dist/clysm-stage1.wasm dist/clysm-stage2.wasm

# Compare binary sizes
ls -la dist/clysm-stage*.wasm
```

## Troubleshooting

### Stage 0 is 8 bytes (placeholder)

The bootstrap script couldn't compile enough forms. Check:
1. Run `sbcl --load build/stage0-complete.lisp` and check output
2. Look for unsupported CL features in error messages
3. Verify blessed subset coverage in `docs/blessed-subset.lisp`

### wasmtime fails with "import not found"

The host shim isn't providing required FFI functions:
1. Check `host-shim/stage1-host.js` is up to date
2. Verify import namespaces match (clysm.fs, clysm.compile, clysm.output)
3. Run with verbose: `node host-shim/stage1-host.js --verbose ...`

### wasm-tools validation fails

The generated Wasm is malformed:
1. Check `wasm-tools print dist/clysm-stage0.wasm` for syntax
2. Verify section ordering (ID 1-13 ascending)
3. Check LEB128 encoding in `src/clysm/backend/leb128.lisp`

### Fixpoint not achieved (exit code 1)

Stage 1 and Stage 2 binaries differ:
1. Run `./scripts/diff-stages.sh` to find differences
2. Check for non-deterministic compilation (timestamps, random)
3. Verify same source used for both compilations

## File Reference

| File | Description |
|------|-------------|
| `build/stage0-complete.lisp` | Stage 0 generation script |
| `dist/clysm-stage0.wasm` | Stage 0 binary |
| `dist/clysm-stage1.wasm` | Stage 1 binary |
| `dist/clysm-stage2.wasm` | Stage 2 binary |
| `dist/stage1-report.json` | Compilation progress report |
| `host-shim/stage1-host.js` | Node.js FFI host |
| `scripts/verify-fixpoint.sh` | Fixpoint verification |
| `src/clysm/validation/compiler-order.lisp` | Module compilation order |
