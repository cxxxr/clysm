# Quickstart: Internal Function Export System

**Feature**: `001-internal-function-export`
**Date**: 2026-01-01

## Prerequisites

- SBCL 2.4+ installed
- Nix environment (`nix develop`)
- wasm-tools installed

## Quick Verification

### Step 1: Check Current State

```bash
# View current error patterns
cat dist/stage1-report.json | jq '.error_patterns[] | select(.pattern | contains("Undefined function"))'
```

Expected: Multiple undefined function errors (P114, P944, P321, P543, P464, P951, P457, P626)

### Step 2: Build Stage 1 (Before)

```bash
sbcl --load build/stage1-complete.lisp
```

Check current compilation rate:
```bash
cat dist/stage1-report.json | jq '.summary.coverage_pct'
```

Expected: ~21.57%

### Step 3: Apply Changes

After implementing the feature:

1. **Verify symbol accessibility**:
```lisp
(ql:quickload :clysm)
(describe 'clysm:lexical-env-parent)
(describe 'clysm:compile-to-instructions)
(describe 'clysm:make-wasm-struct-type)
(describe 'clysm:ast-literal-value)
```

2. **Run unit tests**:
```bash
sbcl --eval "(asdf:test-system :clysm)"
```

### Step 4: Regenerate Stage 1 (After)

```bash
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm
```

### Step 5: Verify Results

```bash
# Check new compilation rate
cat dist/stage1-report.json | jq '.summary.coverage_pct'

# Verify eliminated error patterns
cat dist/stage1-report.json | jq '.error_patterns[] | select(.pattern | contains("LEXICAL-ENV-PARENT"))'
# Expected: No results (pattern eliminated)
```

## Success Criteria Checklist

| Criterion | Command | Expected |
|-----------|---------|----------|
| Compilation rate ≥35% | `jq '.summary.coverage_pct'` | ≥35.0 |
| P114 eliminated | `jq '.error_patterns[] | select(.pattern_id == "P114")'` | No results |
| P944 eliminated | `jq '.error_patterns[] | select(.pattern_id == "P944")'` | No results |
| P321 eliminated | `jq '.error_patterns[] | select(.pattern_id == "P321")'` | No results |
| P543 eliminated | `jq '.error_patterns[] | select(.pattern_id == "P543")'` | No results |
| P464 eliminated | `jq '.error_patterns[] | select(.pattern_id == "P464")'` | No results |
| Wasm valid | `wasm-tools validate dist/clysm-stage1.wasm` | Exit 0 |

## Troubleshooting

### "Package CLYSM does not export symbol X"

The symbol wasn't added to the `:import-from` clause. Check `src/clysm/package.lisp`.

### "Symbol X already accessible"

Use `:shadowing-import-from` instead of `:import-from` if there's a conflict.

### Compilation rate still below 35%

Check `dist/stage1-report.json` for new top blockers:
```bash
cat dist/stage1-report.json | jq '.summary.top_blockers'
```

These may require additional features to address.

## Development Workflow

```bash
# 1. Make changes to package.lisp
# 2. Reload the system
sbcl --eval "(asdf:load-system :clysm :force t)"

# 3. Run quick test
sbcl --eval "(ql:quickload :clysm)" --eval "(clysm:lexical-env-parent (clysm:make-lexical-env))"

# 4. Full Stage 1 regeneration
sbcl --load build/stage1-complete.lisp

# 5. Validate output
wasm-tools validate dist/clysm-stage1.wasm
```
