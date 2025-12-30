# Quickstart: Stage 1 Runtime Environment

**Date**: 2025-12-30
**Feature**: 001-stage1-runtime

## Prerequisites

1. **Node.js 20+** (for WasmGC support)
   ```bash
   node --version  # Should be v20.x or higher
   ```

2. **Stage 1 Wasm** exists at `dist/clysm-stage1.wasm`
   ```bash
   ls -la dist/clysm-stage1.wasm  # Should be ~24.5KB
   ```

3. **wasm-tools** for validation (optional but recommended)
   ```bash
   wasm-tools --version
   ```

## Quick Start

### 1. Run Stage 1

```bash
# Using the convenience script
./scripts/run-stage1.sh

# Or directly with Node.js
node host-shim/stage1-runner.js
```

**Expected output**: Stage 1 initializes and prints startup messages (if any), then exits with code 0.

### 2. Verify FFI Functions

```bash
# The runner loads io-shim.js and fs-shim.js automatically
# Verify by checking verbose output
./scripts/run-stage1.sh --verbose
```

**Expected output**: FFI imports resolved for `clysm:io` and `clysm:fs` namespaces.

### 3. Generate Stage 2 (Optional)

```bash
# Attempt to use Stage 1 to compile Clysm itself
./scripts/generate-stage2.sh

# Check output
ls -la dist/clysm-stage2.wasm
cat dist/stage2-report.json | jq .summary
```

**Note**: This may exit with code 77 if Stage 1 lacks `compile_all` export.

## Common Operations

### Check Stage 1 Exports

```bash
wasm-tools print dist/clysm-stage1.wasm | grep "(export"
```

### Check Stage 1 Imports

```bash
wasm-tools print dist/clysm-stage1.wasm | grep "(import"
```

### Validate Stage 1 Binary

```bash
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0
```

## Troubleshooting

### Exit Code 3: Missing Dependency

```
ERROR: Stage 1 binary not found
```

**Solution**: Generate Stage 1 first:
```bash
sbcl --load build/stage1-complete.lisp
```

### Exit Code 77: Known Limitation

```
[KNOWN LIMITATION] Stage 1 does not export compile_form.
```

**Solution**: This is expected with current 14.1% compilation rate. Focus on improving compilation rate in separate feature.

### WebAssembly.instantiate Error

```
TypeError: WebAssembly.instantiate: Import clysm:io::write-char: ...
```

**Solution**: Ensure you're using `stage1-runner.js` (not `run-wasm.js`), which includes all required shims.

### Node.js Version Error

```
SyntaxError: Unexpected token
```

**Solution**: Upgrade to Node.js 20+:
```bash
nix develop  # Or: nvm use 20
```

## File Locations

| File | Purpose |
|------|---------|
| `host-shim/stage1-runner.js` | Main Stage 1 execution script |
| `host-shim/io-shim.js` | clysm:io FFI implementations |
| `host-shim/fs-shim.js` | clysm:fs FFI implementations |
| `scripts/run-stage1.sh` | Convenience shell wrapper |
| `scripts/generate-stage2.sh` | Stage 2 generation script |
| `dist/clysm-stage1.wasm` | Stage 1 binary (input) |
| `dist/clysm-stage2.wasm` | Stage 2 binary (output) |
| `dist/stage2-report.json` | Compilation report |

## Next Steps

1. **Verify Stage 1 execution**: Run `./scripts/run-stage1.sh` successfully
2. **Check exports**: Confirm available exports with `wasm-tools print`
3. **Attempt Stage 2**: Try `./scripts/generate-stage2.sh` (expect exit 77)
4. **Review report**: Check `dist/stage2-report.json` for blockers
5. **Fixpoint (future)**: When Stage 2 is meaningful, run `./scripts/verify-fixpoint.sh`
