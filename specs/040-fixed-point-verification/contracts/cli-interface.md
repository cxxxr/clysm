# CLI Interface Contract: Fixed-Point Verification

**Feature**: 040-fixed-point-verification
**Date**: 2025-12-27

## Commands

### 1. verify-fixpoint.sh

Primary verification script for CLI and CI usage.

**Synopsis**:
```bash
./scripts/verify-fixpoint.sh [OPTIONS]
```

**Options**:
| Option | Default | Description |
|--------|---------|-------------|
| `--stage1 PATH` | `dist/clysm-stage1.wasm` | Path to Stage 1 binary |
| `--stage2 PATH` | `dist/clysm-stage2.wasm` | Path to Stage 2 binary (output) |
| `--source-dir PATH` | `src/clysm/` | Directory containing source modules |
| `--json` | (off) | Output result as JSON |
| `--skip-generate` | (off) | Skip Stage 2 generation, compare existing |
| `--history` | (off) | Append result to history log |
| `--help` | - | Show help and exit |

**Exit Codes**:
| Code | Status | Description |
|------|--------|-------------|
| 0 | ACHIEVED | Stage 1 == Stage 2 (byte-identical) |
| 1 | NOT_ACHIEVED | Binaries differ |
| 2 | COMPILATION_ERROR | Stage 2 generation failed |
| 3 | MISSING_DEPENDENCY | wasmtime/wasm-tools/Stage 1 missing |

**Output (JSON mode)**:
```json
{
  "status": "ACHIEVED",
  "timestamp": "2025-12-27T12:00:00Z",
  "stage1": {
    "path": "dist/clysm-stage1.wasm",
    "size_bytes": 1584,
    "valid": true
  },
  "stage2": {
    "path": "dist/clysm-stage2.wasm",
    "size_bytes": 1584,
    "valid": true,
    "compilation_rate": 1.0,
    "modules_compiled": 45,
    "modules_total": 45
  },
  "comparison": {
    "identical": true,
    "first_diff_offset": null,
    "diff_bytes": 0
  },
  "timing": {
    "stage2_generation_ms": 120000,
    "comparison_ms": 50,
    "total_ms": 120050
  }
}
```

**Output (text mode, success)**:
```
=== Fixed-Point Verification ===

Stage 1: dist/clysm-stage1.wasm (1584 bytes, valid)
Stage 2: dist/clysm-stage2.wasm (1584 bytes, valid)

Compilation: 45/45 modules (100.0%)
Time: Stage 2 generation 120.0s, comparison 0.05s

Result: FIXED-POINT ACHIEVED

Binaries are byte-identical. Self-hosting verified.
```

**Output (text mode, failure)**:
```
=== Fixed-Point Verification ===

Stage 1: dist/clysm-stage1.wasm (1584 bytes, valid)
Stage 2: dist/clysm-stage2.wasm (1200 bytes, valid)

Compilation: 40/45 modules (88.9%)
Time: Stage 2 generation 115.0s, comparison 0.02s

Result: FIXED-POINT NOT ACHIEVED

First difference at byte offset: 0x100
Total differing bytes: 384
Size difference: -384 bytes (-24.2%)

Run with --json for detailed diff report.
```

---

### 2. run-stage2-gen.sh

Generate Stage 2 binary without verification.

**Synopsis**:
```bash
./scripts/run-stage2-gen.sh [OPTIONS]
```

**Options**:
| Option | Default | Description |
|--------|---------|-------------|
| `--stage1 PATH` | `dist/clysm-stage1.wasm` | Path to Stage 1 binary |
| `--output PATH` | `dist/clysm-stage2.wasm` | Output path for Stage 2 |
| `--source-dir PATH` | `src/clysm/` | Source directory |
| `--json` | (off) | Output progress as JSON |
| `--help` | - | Show help and exit |

**Exit Codes**:
| Code | Description |
|------|-------------|
| 0 | Stage 2 generated successfully |
| 1 | Compilation error |
| 2 | Missing dependency |

---

### 3. diff-stages.sh (Extended)

Compare two stage binaries.

**Synopsis**:
```bash
./scripts/diff-stages.sh [OPTIONS] STAGE1 STAGE2
```

**Options**:
| Option | Default | Description |
|--------|---------|-------------|
| `--json` | (off) | Output as JSON |
| `--byte-level` | (off) | Include byte-level diff info |
| `--help` | - | Show help and exit |

**Exit Codes**:
| Code | Description |
|------|-------------|
| 0 | Binaries are identical |
| 1 | Binaries differ |
| 2 | File not found or invalid |

---

## SBCL API

### Main Entry Points

```lisp
;; Full verification workflow
(clysm/stage2:verify-fixpoint
  &key (stage1-path "dist/clysm-stage1.wasm")
       (stage2-path "dist/clysm-stage2.wasm")
       (source-dir "src/clysm/")
       (skip-generate nil)
       (output-format :text)) ; :text or :json
=> verification-result

;; Stage 2 generation only
(clysm/stage2:generate-stage2
  &key (stage1-path "dist/clysm-stage1.wasm")
       (output-path "dist/clysm-stage2.wasm")
       (source-dir "src/clysm/"))
=> (values success-p stage2-info error-message)

;; Byte-level comparison only
(clysm/stage1:binaries-identical-p path1 path2)
=> (values identical-p first-diff-offset)

;; Generate byte diff report
(clysm/stage1:generate-byte-diff-report path1 path2
  &key (stream *standard-output*)
       (format :json))
=> byte-diff-info
```

---

## Host Shim Interface

### stage1-host.js Extensions

```javascript
// Module: stage1-host.js

/**
 * Compile all source modules and produce Stage 2 binary.
 * @param {string} sourceDir - Path to source directory
 * @param {string} outputPath - Path for Stage 2 output
 * @returns {Object} Compilation result
 */
async function compileToStage2(sourceDir, outputPath) {
  return {
    success: true,
    modules_compiled: 45,
    modules_total: 45,
    output_path: outputPath,
    output_size: 1584,
    errors: []
  };
}

// CLI interface
// node host-shim/stage1-host.js --mode compile \
//   --stage1 dist/clysm-stage1.wasm \
//   --source-dir src/clysm/ \
//   --output dist/clysm-stage2.wasm
```

---

## Error Responses

### Missing Dependency (exit 3)

```json
{
  "status": "MISSING_DEPENDENCY",
  "timestamp": "2025-12-27T12:00:00Z",
  "error": {
    "type": "wasmtime_not_found",
    "message": "wasmtime not found in PATH. Install with: curl https://wasmtime.dev/install.sh -sSf | bash"
  }
}
```

### Compilation Error (exit 2)

```json
{
  "status": "COMPILATION_ERROR",
  "timestamp": "2025-12-27T12:00:00Z",
  "stage2": {
    "compilation_rate": 0.889,
    "modules_compiled": 40,
    "modules_total": 45
  },
  "errors": [
    {
      "module": "src/clysm/compiler/ast.lisp",
      "form_id": "5:12",
      "error": "Unknown operator: defstruct"
    }
  ]
}
```
