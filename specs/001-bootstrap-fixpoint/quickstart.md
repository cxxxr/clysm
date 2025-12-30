# Quickstart: Bootstrap Fixpoint Achievement

**Feature**: 001-bootstrap-fixpoint

## Prerequisites

1. SBCL 2.4+ installed
2. Node.js 20+ with WasmGC support
3. wasm-tools installed
4. Clysm ASDF system loadable

```bash
# Verify prerequisites
sbcl --version
node --version
wasm-tools --version
```

## Step 1: Generate Stage 1

Generate Stage 1 Wasm binary with compile_form exported:

```bash
sbcl --load build/stage1-complete.lisp
```

**Output**: `dist/clysm-stage1.wasm` (~24KB)

Verify compile_form is exported:

```bash
wasm-tools print dist/clysm-stage1.wasm | grep 'export.*compile_form'
```

## Step 2: Run Stage 1

Load and test Stage 1 in Node.js:

```bash
node host-shim/stage1-runner.js --verbose
```

Test compile_form with simple expression:

```bash
node host-shim/stage1-runner.js --expr "(+ 1 2)" --output /tmp/test.wasm
wasm-tools validate /tmp/test.wasm
```

## Step 3: Generate Stage 2

Use Stage 1 to compile Clysm into Stage 2:

```bash
node host-shim/stage2-gen.js --output dist/clysm-stage2.wasm
```

**Output**: `dist/clysm-stage2.wasm`

## Step 4: Verify Fixpoint

Compare Stage 1 and Stage 2:

```bash
./scripts/verify-fixpoint.sh
```

**Expected Results**:

| Scenario | Exit Code | Message |
|----------|-----------|---------|
| Fixpoint achieved | 0 | "FIXPOINT ACHIEVED: Stage 1 == Stage 2" |
| Sizes differ | 1 | "MISMATCH: Stage 1 (24500 bytes) != Stage 2 (12300 bytes)" |
| Content differs | 1 | "MISMATCH: First difference at offset 1234" |

## Step 5: Review Blocker Report

If fixpoint not achieved, examine the blocker report:

```bash
cat dist/stage2-report.json | jq '.top_5_blockers'
```

## Quick Verification Commands

```bash
# Validate all Wasm outputs
wasm-tools validate dist/clysm-stage1.wasm
wasm-tools validate dist/clysm-stage2.wasm

# Compare sizes
ls -la dist/clysm-stage*.wasm

# View exports
wasm-tools print dist/clysm-stage1.wasm | grep '(export'

# Check compilation rate
cat dist/stage1-report.json | jq '.compilation_rate'
```

## Troubleshooting

### compile_form not exported

```
[KNOWN LIMITATION] Stage 1 does not export compile_form.
```

**Fix**: Rebuild Stage 1 with updated generator.lisp that uses `generate-exports()`.

### WebAssembly.LinkError

```
FFI import mismatch: ...
```

**Fix**: Ensure host-shim provides all required imports (clysm:io, clysm:fs).

### Wasm validation fails

```
error: type mismatch ...
```

**Fix**: Check blocker report for type errors. May indicate unsupported form type.

## File Locations

| File | Purpose |
|------|---------|
| `dist/clysm-stage1.wasm` | Stage 1 binary |
| `dist/clysm-stage2.wasm` | Stage 2 binary |
| `dist/stage1-report.json` | Stage 1 compilation stats |
| `dist/stage2-report.json` | Stage 2 blocker report |
| `host-shim/stage2-gen.js` | Stage 2 generation script |
| `scripts/verify-fixpoint.sh` | Fixpoint verification |
