# Quickstart: Stage 1 Compiler Generation

**Feature**: 039-stage1-compiler-gen
**Date**: 2025-12-27

## Prerequisites

1. **Nix environment**: Ensure you have entered the development shell
   ```bash
   nix develop
   ```

2. **Stage 0 binary**: Verify the Stage 0 binary exists
   ```bash
   ls dist/clysm-stage0.wasm
   wasm-tools validate dist/clysm-stage0.wasm
   ```

3. **Node.js**: Required for host shim execution
   ```bash
   node --version  # Should be 18+
   ```

## Quick Verification

### Step 1: Verify Stage 0 Loads

```bash
# Check Stage 0 binary exists and validates
node host-shim/verify-stage0.js --check

# Expected output:
# Stage 0 binary exists: /path/to/dist/clysm-stage0.wasm
```

### Step 2: Run Stage 0 Verification Tests

```bash
# Run arithmetic test (currently skips due to missing exports)
node host-shim/verify-stage0.js V001

# Expected: Exit code 77 (SKIP) - compile export not yet functional
```

### Step 3: Generate Stage 1 (After Implementation)

```bash
# Run Stage 1 generation script
./scripts/run-stage1-gen.sh

# Expected outputs:
# - dist/clysm-stage1.wasm (generated binary)
# - dist/stage1-report.json (progress report)
```

### Step 4: Validate Stage 1

```bash
# Validate generated binary
wasm-tools validate dist/clysm-stage1.wasm

# Compare with Stage 0
./scripts/diff-stages.sh
```

## Development Workflow

### Running Tests

```bash
# Unit tests
sbcl --load clysm.asd --eval '(asdf:test-system :clysm/tests/stage1)'

# Integration tests
./scripts/verify-stage1.sh
```

### Viewing Progress Reports

```bash
# View latest progress report
cat dist/stage1-report.json | jq '.summary'

# View top blockers
cat dist/stage1-report.json | jq '.summary.top_blockers'
```

### Analyzing Blockers

```bash
# Generate blocker analysis
sbcl --load build/stage1-gen.lisp --eval '(clysm/stage1:analyze-blockers)'

# View blocker report
cat dist/blocker-report.json | jq '.'
```

## File Structure After Implementation

```text
dist/
├── clysm-stage0.wasm      # Input: Bootstrap compiler
├── clysm-stage1.wasm      # Output: Self-compiled compiler
├── stage1-report.json     # Compilation progress report
└── blocker-report.json    # Blocker analysis

host-shim/
├── verify-stage0.js       # Existing verification
├── stage1-host.js         # NEW: Stage 1 compilation host
└── fs-shim.js             # Existing: File system FFI

scripts/
├── run-stage1-gen.sh      # NEW: Stage 1 generation
├── diff-stages.sh         # NEW: Binary comparison
└── verify-stage1.sh       # NEW: Stage 1 verification
```

## Common Issues

### Issue: Stage 0 binary not found

**Solution**: Run bootstrap first
```bash
sbcl --load build/bootstrap.lisp
```

### Issue: "Unknown instruction" error

**Explanation**: Current Clysm limitation - combined form compilation fails.
**Workaround**: Forms are compiled individually; this error is expected for combined output.

### Issue: Low compilation coverage

**Explanation**: Stage 0 supports ~19.6% of CL features used in Clysm source.
**Check blockers**: View `dist/blocker-report.json` for top issues.

## Key Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Form compilation coverage | 25% | 19.6% |
| Stage 1 binary validates | Yes | TBD |
| Progress report generation | <5s | TBD |
