# Quickstart: Compiler Internal Function Consolidation

**Date**: 2026-01-01
**Feature**: 001-compiler-internal-consolidation

## Prerequisites

- SBCL 2.4+ installed
- Nix environment active (`nix develop`)
- wasm-tools, wasmtime available

## Development Environment

```bash
# Enter Nix shell
cd /home/user/src/clysm-workbench/clysm3
nix develop

# Verify tools
sbcl --version    # SBCL 2.4+
wasm-tools --version
wasmtime --version
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Main compilation, runtime dispatch |
| `src/clysm/lib/list-runtime.lisp` | Runtime list functions |
| `src/clysm/lib/io-runtime.lisp` | Runtime I/O functions (reference) |
| `build/stage1-complete.lisp` | Stage 1 generation script |
| `dist/stage1-report.json` | Compilation statistics |

## Quick Commands

### Run Stage 1 Generation

```bash
# Generate Stage 1 Wasm
sbcl --load build/stage1-complete.lisp

# With verbose output
sbcl --load build/stage1-complete.lisp --verbose

# Check output
ls -la dist/clysm-stage1.wasm
```

### Validate Wasm Output

```bash
wasm-tools validate dist/clysm-stage1.wasm
# Exit code 0 = success
```

### Run Tests

```bash
# All tests
sbcl --eval "(asdf:test-system :clysm)"

# Specific test file
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(rove:run :clysm/tests/unit/list-runtime)"
```

### Check Compilation Statistics

```bash
# View report
cat dist/stage1-report.json | jq '.summary'

# Check top blockers
cat dist/stage1-report.json | jq '.top_blockers[:5]'
```

## Implementation Workflow

### Step 1: Baseline Measurement

```bash
# Run Stage 1, record baseline
sbcl --load build/stage1-complete.lisp
cp dist/stage1-report.json dist/baseline-report.json
```

### Step 2: Remove Inline Compile Functions

Edit `func-section.lisp`:
1. Locate `compile-member` (~line 12079)
2. Locate `compile-assoc` (~line 12160)
3. Comment out or delete functions
4. Ensure runtime dispatch is checked first in `compile-call`

### Step 3: Verify No Regressions

```bash
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm

# Compare reports
diff <(jq '.compiled_count' dist/baseline-report.json) \
     <(jq '.compiled_count' dist/stage1-report.json)
```

### Step 4: Add Package Stubs

Create `src/clysm/lib/package-stubs.lisp`:

```lisp
(in-package :clysm)

(defun find-package* (name)
  "Find package by name via FFI"
  (%host-find-package name))

(defun intern* (name &optional (package *package*))
  "Intern symbol via FFI"
  (%host-intern name package))

(defun packagep* (obj)
  "Check if object is package via FFI"
  (%host-packagep obj))
```

### Step 5: Add Environment Runtime

Create `src/clysm/lib/env-runtime.lisp`:

```lisp
(in-package :clysm)

(defun env-add-local-wasm (env name &optional type)
  "Wasm-compilable version of env-add-local"
  ;; Implementation using only Wasm-compilable constructs
  )
```

### Step 6: Final Validation

```bash
# Full regeneration
sbcl --load build/stage1-complete.lisp

# Validate
wasm-tools validate dist/clysm-stage1.wasm

# Check metrics
cat dist/stage1-report.json | jq '{
  rate: .compilation_rate,
  compiled: .compiled_count,
  total: .total_count
}'
```

## Debugging Tips

### Compilation Errors

```lisp
;; In SBCL REPL
(asdf:load-system :clysm)

;; Try compiling a single form
(clysm:compile-to-wasm '(defun test-fn (x) (member x '(1 2 3))))
```

### Runtime Function Dispatch

```lisp
;; Check if function is registered
(gethash 'member clysm/compiler/codegen/func-section::*runtime-function-table*)
;; => (:$member-rt . nil)

;; Check dispatch priority in compile-call
;; Should check runtime-function-p BEFORE inline codegen
```

### Wasm Validation Errors

```bash
# Get detailed error
wasm-tools validate -v dist/clysm-stage1.wasm

# Disassemble to WAT for inspection
wasm-tools print dist/clysm-stage1.wasm > /tmp/stage1.wat
```

## Success Criteria Checklist

- [ ] Stage 1 compiles without errors
- [ ] `wasm-tools validate` passes (exit code 0)
- [ ] Compilation rate >= 30%
- [ ] No regressions (compiled_count >= baseline)
- [ ] ENV-ADD-LOCAL not in top blockers
- [ ] COMPILE-TO-INSTRUCTIONS not in top blockers
- [ ] compile-member removed from func-section.lisp
- [ ] compile-assoc removed from func-section.lisp
