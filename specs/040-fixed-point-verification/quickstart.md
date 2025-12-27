# Quickstart: Fixed-Point Verification

**Feature**: 040-fixed-point-verification
**Date**: 2025-12-27

## Prerequisites

Ensure you have the following installed:

```bash
# Check wasmtime
wasmtime --version
# Expected: wasmtime CLI x.x.x

# Check wasm-tools
wasm-tools --version
# Expected: wasm-tools x.x.x

# Check Node.js
node --version
# Expected: v18.x or higher

# Check Stage 1 binary exists (from feature 039)
ls -la dist/clysm-stage1.wasm
# Expected: file exists with size > 0
```

If using Nix:
```bash
nix develop
# All tools are available in the devShell
```

---

## Quick Verification (Developer Workflow)

### 1. Generate Stage 2 and Verify

```bash
# Full verification (generate + compare)
./scripts/verify-fixpoint.sh

# Example output:
# === Fixed-Point Verification ===
# Stage 1: dist/clysm-stage1.wasm (1584 bytes, valid)
# Stage 2: dist/clysm-stage2.wasm (1584 bytes, valid)
# Result: FIXED-POINT ACHIEVED
```

### 2. Quick Check (Skip Generation)

If Stage 2 already exists:
```bash
./scripts/verify-fixpoint.sh --skip-generate
```

### 3. JSON Output for Scripting

```bash
./scripts/verify-fixpoint.sh --json | jq '.status'
# Expected: "ACHIEVED" or "NOT_ACHIEVED"
```

---

## Understanding Results

### Fixed-Point Achieved (exit 0)

```
Result: FIXED-POINT ACHIEVED
Binaries are byte-identical. Self-hosting verified.
```

This means:
- Stage 1 successfully compiled all Clysm source modules
- The output (Stage 2) is byte-identical to Stage 1
- The compiler can reproduce itself

### Fixed-Point Not Achieved (exit 1)

```
Result: FIXED-POINT NOT ACHIEVED
First difference at byte offset: 0x100
Total differing bytes: 384
```

This means:
- Stage 2 was generated but differs from Stage 1
- Check the diff report for details
- Common causes:
  - Stage 1 doesn't support all CL features used in source
  - Non-deterministic compilation (unlikely)

### Compilation Error (exit 2)

```
Result: COMPILATION_ERROR
Compilation: 40/45 modules (88.9%)
```

This means:
- Stage 1 failed to compile some source modules
- Fixed-point is not achievable until all modules compile
- Check error log for specific failures

---

## Debugging Failures

### 1. View Diff Report

```bash
./scripts/diff-stages.sh dist/clysm-stage1.wasm dist/clysm-stage2.wasm
```

### 2. Detailed JSON Diff

```bash
./scripts/verify-fixpoint.sh --json > result.json
jq '.comparison' result.json
```

### 3. Check Compilation Errors

```bash
./scripts/verify-fixpoint.sh --json | jq '.errors'
```

### 4. Track Progress Over Time

```bash
# Enable history logging
./scripts/verify-fixpoint.sh --history

# View history
cat dist/verification-history.jsonl | jq -s '.'
```

---

## CI Integration

### GitHub Actions Example

```yaml
jobs:
  verify-fixpoint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main

      - name: Verify Fixed-Point
        run: |
          nix develop --command ./scripts/verify-fixpoint.sh --json
        id: verify

      - name: Upload Verification Result
        uses: actions/upload-artifact@v4
        with:
          name: verification-result
          path: |
            dist/clysm-stage2.wasm
            dist/verification-history.jsonl
```

### Exit Code Handling

```bash
./scripts/verify-fixpoint.sh
case $? in
  0) echo "Self-hosting verified!" ;;
  1) echo "Binaries differ, check diff report" ;;
  2) echo "Compilation failed, extend CL subset" ;;
  3) echo "Missing dependencies, check setup" ;;
esac
```

---

## Development Iteration

### Typical Workflow

1. **Make compiler changes** (extend CL subset support)
2. **Regenerate Stage 1**:
   ```bash
   sbcl --load build/stage1-gen.lisp
   ```
3. **Verify fixed-point**:
   ```bash
   ./scripts/verify-fixpoint.sh --history
   ```
4. **Check progress**:
   ```bash
   cat dist/verification-history.jsonl | jq -s 'map(.compilation_rate)'
   ```

### Expected Progression

| Iteration | Compilation Rate | Diff Bytes | Status |
|-----------|------------------|------------|--------|
| 1 | 19.6% | N/A | COMPILATION_ERROR |
| 2 | 35.0% | N/A | COMPILATION_ERROR |
| ... | ... | ... | ... |
| N | 100.0% | 1000 | NOT_ACHIEVED |
| N+1 | 100.0% | 0 | ACHIEVED |

---

## Troubleshooting

### "wasmtime not found"

```bash
# Install wasmtime
curl https://wasmtime.dev/install.sh -sSf | bash
source ~/.bashrc

# Or with Nix
nix develop
```

### "Stage 1 binary not found"

```bash
# Generate Stage 1 first (feature 039)
sbcl --load build/stage1-gen.lisp
```

### "Host shim error"

```bash
# Check Node.js version
node --version  # Should be v18+

# Check shim syntax
node --check host-shim/stage1-host.js
```

### "Out of memory during compilation"

```bash
# Increase wasmtime memory limit
export WASMTIME_MEMORY_LIMIT=2G
./scripts/verify-fixpoint.sh
```
