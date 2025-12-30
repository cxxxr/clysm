# CLI Interface Contracts: Stage 1 Runtime

**Date**: 2025-12-30
**Feature**: 001-stage1-runtime

## stage1-runner.js

Execute Stage 1 Wasm module with FFI support.

### Usage

```bash
node host-shim/stage1-runner.js [options] [wasm-path]
```

### Arguments

| Argument | Required | Default | Description |
|----------|----------|---------|-------------|
| wasm-path | No | dist/clysm-stage1.wasm | Path to Stage 1 Wasm file |

### Options

| Option | Value | Description |
|--------|-------|-------------|
| --help | - | Show usage information |
| --expr | string | Lisp expression to compile (requires compile_form export) |
| --output | path | Output path for compiled Wasm (with --expr) |
| --stdin | path | File to use as stdin content |
| --verbose | - | Enable verbose logging |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success - _start executed successfully |
| 1 | Partial success - execution completed with warnings |
| 2 | Failure - execution failed |
| 3 | Missing dependency - Wasm file not found |
| 77 | Known limitation - requested export not available |

### Output

**stdout**: Runtime output from Stage 1 execution
**stderr**: Error messages and diagnostics
**Files**: None (unless --output specified)

### Examples

```bash
# Run Stage 1 with default path
node host-shim/stage1-runner.js

# Run specific Wasm file
node host-shim/stage1-runner.js dist/clysm-stage1.wasm

# Compile expression (if compile_form exported)
node host-shim/stage1-runner.js --expr "(+ 1 2)" --output /tmp/test.wasm

# Run with stdin from file
node host-shim/stage1-runner.js --stdin input.lisp
```

---

## run-stage1.sh

Convenience wrapper for stage1-runner.js.

### Usage

```bash
./scripts/run-stage1.sh [options]
```

### Options

| Option | Value | Description |
|--------|-------|-------------|
| --help | - | Show usage information |
| --wasm | path | Path to Stage 1 Wasm (default: dist/clysm-stage1.wasm) |
| --verbose | - | Enable verbose output |

### Exit Codes

Same as stage1-runner.js

### Examples

```bash
# Run Stage 1 with defaults
./scripts/run-stage1.sh

# Run specific Wasm file
./scripts/run-stage1.sh --wasm dist/clysm-stage1.wasm

# Verbose mode
./scripts/run-stage1.sh --verbose
```

---

## generate-stage2.sh

Use Stage 1 to compile Clysm source code into Stage 2.

### Usage

```bash
./scripts/generate-stage2.sh [options]
```

### Options

| Option | Value | Description |
|--------|-------|-------------|
| --help | - | Show usage information |
| --stage1 | path | Path to Stage 1 Wasm (default: dist/clysm-stage1.wasm) |
| --output | path | Path for Stage 2 output (default: dist/clysm-stage2.wasm) |
| --report | path | Path for JSON report (default: dist/stage2-report.json) |
| --source-dir | path | Directory containing Clysm source (default: src/clysm) |
| --verbose | - | Enable verbose output |
| --json | - | Output machine-readable JSON status |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success - Stage 2 generated successfully |
| 1 | Partial success - Stage 2 generated but incomplete |
| 2 | Failure - Stage 2 generation failed |
| 3 | Missing dependency - Stage 1 not found |
| 77 | Known limitation - compile_all not exported |

### Output

**stdout**: Progress messages (human-readable or JSON with --json)
**stderr**: Error messages
**Files**:
- `dist/clysm-stage2.wasm`: Generated Stage 2 binary
- `dist/stage2-report.json`: Compilation report

### Examples

```bash
# Generate Stage 2 with defaults
./scripts/generate-stage2.sh

# Custom paths
./scripts/generate-stage2.sh --stage1 my-stage1.wasm --output my-stage2.wasm

# JSON output for automation
./scripts/generate-stage2.sh --json

# Verbose mode
./scripts/generate-stage2.sh --verbose
```

### JSON Output Format (with --json)

```json
{
  "status": "SUCCESS|PARTIAL|FAILURE|SKIP",
  "timestamp": "2025-12-30T12:00:00Z",
  "stage1_path": "dist/clysm-stage1.wasm",
  "stage2_path": "dist/clysm-stage2.wasm",
  "stage2_size": 25000,
  "compilation_rate": 0.142,
  "modules_compiled": 44,
  "modules_total": 44,
  "error": null
}
```

---

## Integration with verify-fixpoint.sh

The existing `scripts/verify-fixpoint.sh` can be used after Stage 2 generation:

```bash
# Generate Stage 2
./scripts/generate-stage2.sh

# Verify fixpoint (Stage 1 == Stage 2)
./scripts/verify-fixpoint.sh

# Combined workflow
./scripts/generate-stage2.sh && ./scripts/verify-fixpoint.sh
```
