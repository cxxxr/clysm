# Quickstart: Phase 13D Milestone M2 - Blocker Analysis

**Date**: 2025-12-31
**Related**: [plan.md](./plan.md), [spec.md](./spec.md)

## Prerequisites

- SBCL 2.4+ installed
- Nix with flakes enabled (for development shell)
- wasm-tools installed (for validation)

## Quick Commands

### Enter Development Environment

```bash
cd /home/user/src/clysm
nix develop
```

### Regenerate Stage 1 Report

```bash
# Standard generation
sbcl --load build/stage1-complete.lisp

# With verbose output
sbcl --load build/stage1-complete.lisp --verbose

# Custom output paths
sbcl --load build/stage1-complete.lisp -- --output=dist/custom.wasm --report=dist/custom-report.json
```

### View Current Report

```bash
cat dist/stage1-report.json | jq .summary
```

### Validate Generated Wasm

```bash
wasm-tools validate dist/clysm-stage1.wasm
```

### Run Tests

```bash
# All tests
sbcl --eval "(asdf:test-system :clysm)"

# Contract tests for Stage 1 exports
node tests/contract/fixpoint/test-exports.js

# Integration tests
tests/integration/bootstrap/test-fixpoint.sh
```

## Workflow

### 1. Generate Baseline Report

```bash
sbcl --load build/stage1-complete.lisp
cat dist/stage1-report.json | jq '{total: .summary.total_forms, compiled: .summary.compiled, rate: .summary.coverage_pct}'
```

Expected output (current baseline):
```json
{
  "total": 26571,
  "compiled": 3631,
  "rate": 13.9
}
```

### 2. Identify Top Blockers

```bash
cat dist/stage1-report.json | jq '.summary.top_blockers'
```

### 3. Analyze Specific Blocker

```bash
# Load blocker analysis tool
sbcl --load build/analyze-defun-failures.lisp
```

### 4. Apply Fix and Verify

```bash
# After implementing a fix, regenerate and compare
sbcl --load build/stage1-complete.lisp
cat dist/stage1-report.json | jq '.summary.coverage_pct'
```

Target: >= 25.0

### 5. Validate No Regressions

```bash
wasm-tools validate dist/clysm-stage1.wasm
sbcl --eval "(asdf:test-system :clysm)"
```

## Success Criteria Verification

| Criterion | Command | Expected |
|-----------|---------|----------|
| SC-001: Rate >= 25% | `jq '.summary.coverage_pct' dist/stage1-report.json` | >= 25.0 |
| SC-002: Failures categorized | `jq '.summary.top_blockers | length' dist/stage1-report.json` | > 0 |
| SC-003: Top 5 documented | `jq '.summary.top_blockers | length' dist/stage1-report.json` | <= 5 |
| SC-005: Valid Wasm | `wasm-tools validate dist/clysm-stage1.wasm` | exit 0 |
| SC-006: No regressions | `sbcl --eval "(asdf:test-system :clysm)"` | all pass |

## Troubleshooting

### Report Not Generated

```bash
# Check if Clysm loads correctly
sbcl --eval "(require :asdf)" --eval "(asdf:load-system :clysm)" --eval "(sb-ext:exit)"
```

### Validation Fails

```bash
# Get detailed validation errors
wasm-tools validate dist/clysm-stage1.wasm 2>&1 | head -50
```

### Low Compilation Rate

```bash
# Check which modules have most failures
cat dist/stage1-report.json | jq '.modules | sort_by(-.failed) | .[0:5]'
```
