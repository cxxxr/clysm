# Quickstart: Stage 1 Compiler Generation

**Date**: 2025-12-30
**Branch**: `040-stage1-compiler-gen`

## Prerequisites

### Required Tools

```bash
# Verify SBCL is installed (2.4+ required)
sbcl --version

# Verify wasm-tools is installed
wasm-tools --version

# Enter Nix development shell (recommended)
nix develop
```

### Required Dependencies

Ensure the Clysm ASDF system is loadable:

```lisp
;; In SBCL REPL
(require :asdf)
(asdf:load-system :clysm)
```

## Generate Stage 1 Binary

### Quick Method (Recommended)

```bash
# From repository root
sbcl --load build/stage1-complete.lisp
```

Expected output:
```
=== Stage 1 Compiler Generation ===
Loading Clysm system...
Clysm system loaded.

Configuration:
  Output: /path/to/dist/clysm-stage1.wasm
  Report: /path/to/dist/stage1-report.json
  Validate: T

Generating Stage 1 binary...
Classifying 1157 forms...
Classification: 164 succeeded, 993 failed
Finding largest valid bundle from 164 forms...
Valid bundle contains 164 forms
Output size: 24554 bytes
Final validation...
Validation PASSED

=== Generation Complete ===
Forms processed: 1157
Compiled: 1157
Failed: 0
Coverage: 100.0%
Time: 1.13 seconds
Output: /path/to/dist/clysm-stage1.wasm
Report: /path/to/dist/stage1-report.json
Size: 24554 bytes

Done.
```

**Note**: Current compilation rate is 14.2% (164/1157 forms). The 80%+ target requires implementing DEFSTRUCT and fixing codegen type issues.

### Verbose Mode

For detailed per-form output:

```bash
sbcl --load build/stage1-complete.lisp -- --verbose
```

### Custom Output Path

```bash
sbcl --load build/stage1-complete.lisp -- --output=/custom/path/stage1.wasm
```

### Skip Validation

```bash
sbcl --load build/stage1-complete.lisp -- --no-validate
```

## Verify Output

### Check File Size

```bash
ls -la dist/clysm-stage1.wasm
# Current: ~24.5KB (24,554 bytes)
# Target: >= 100KB (requires compiler improvements)
```

### Validate Wasm Structure

```bash
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0
```

### Inspect Module Structure

```bash
# Print as WAT (WebAssembly Text format)
wasm-tools print dist/clysm-stage1.wasm | head -100

# Dump module info
wasm-tools dump dist/clysm-stage1.wasm
```

### Check Compilation Report

```bash
# View summary
cat dist/stage1-report.json | jq '.summary'

# View top blockers
cat dist/stage1-report.json | jq '.summary.top_blockers'
```

## Troubleshooting

### Compilation Rate < 80%

If coverage is below 80%, analyze blockers:

```bash
# View blockers
cat dist/stage1-report.json | jq '.summary.top_blockers'
```

Common blockers and fixes:

| Blocker | Recommendation |
|---------|----------------|
| DEFSTRUCT | Ensure defstruct expansion works |
| FORMAT | Check format directive support |
| LOOP | Verify LOOP macro expansion (Phase 13D-5) |
| DECLARE | Handle or skip declarations |

### Wasm Validation Fails

```bash
# Get detailed error
wasm-tools validate dist/clysm-stage1.wasm 2>&1

# Check section order
wasm-tools print dist/clysm-stage1.wasm | grep -E '^\(type|^\(func|^\(global'
```

### Out of Memory

For large codebases, increase SBCL dynamic space:

```bash
sbcl --dynamic-space-size 4096 --load build/stage1-complete.lisp
```

### ASDF System Not Found

```bash
# Ensure clysm.asd is loadable
export CL_SOURCE_REGISTRY="$PWD//"

# Or explicitly add to ASDF
sbcl --eval "(push #p\"$PWD/\" asdf:*central-registry*)" \
     --load build/stage1-complete.lisp
```

## Development Workflow

### 1. Run Initial Build

```bash
sbcl --load build/stage1-complete.lisp
```

### 2. Analyze Blockers

```bash
cat dist/stage1-report.json | jq '.summary.top_blockers[:5]'
```

### 3. Fix Top Blocker

Edit relevant compiler code in `src/clysm/`.

### 4. Re-run Build

```bash
sbcl --load build/stage1-complete.lisp
```

### 5. Repeat Until 80%+ Coverage

Monitor `coverage_pct` in report:

```bash
cat dist/stage1-report.json | jq '.summary.coverage_pct'
```

## Output Files

| File | Description |
|------|-------------|
| `dist/clysm-stage1.wasm` | Stage 1 compiler binary |
| `dist/stage1-report.json` | Compilation statistics and blocker analysis |

## Next Steps

After achieving 80%+ compilation rate:

1. **Phase 13D-8**: Runtime execution of Stage 1 with Node.js host-shim
2. **Phase 13D-9**: Fixpoint verification (Stage 1 compiles Stage 2 == Stage 1)

## Related Commands

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Build Stage 0 (stub module)
sbcl --load build/stage0-complete.lisp

# Verify fixpoint (future)
./scripts/verify-fixpoint.sh
```
