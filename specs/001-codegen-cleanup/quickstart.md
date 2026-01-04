# Quickstart: Compiler Code Generation Cleanup

**Date**: 2026-01-04
**Feature**: 001-codegen-cleanup

## Prerequisites

- SBCL 2.4+ installed
- Nix development shell active (`nix develop`)
- wasm-tools available in PATH

## Quick Validation Check

Before starting any cleanup, verify the baseline:

```bash
# Enter nix shell
nix develop

# Run tests (should pass)
sbcl --eval "(asdf:test-system :clysm)"

# Compile Stage 1 (should succeed)
sbcl --load build/stage1-complete.lisp

# Validate Wasm (should exit 0)
wasm-tools validate dist/clysm-stage1.wasm

# Check current line count
wc -l src/clysm/compiler/codegen/func-section.lisp
# Expected: 15973
```

## Dead Code Detection

### Find migrated functions
```bash
# List all functions registered in *runtime-function-table*
grep -n "register-runtime-function" src/clysm/compiler/codegen/func-section.lisp
```

### Find corresponding compile-* functions
```bash
# For each migrated function, check if compile-* exists
grep -n "defun compile-string-trim" src/clysm/compiler/codegen/func-section.lisp
```

### Check for callers
```bash
# Before removing, verify no live callers
grep -n "compile-string-trim" src/clysm/compiler/codegen/*.lisp
```

## Removal Workflow

### Step 1: Identify dead function boundaries
```bash
# Find function start
grep -n "^(defun compile-string-trim" func-section.lisp

# Find function end (next defun or end of file)
# Use editor to identify the complete function body
```

### Step 2: Remove function
```lisp
;; Delete the entire defun form from func-section.lisp
;; Save file
```

### Step 3: Validate
```bash
# Run validation script
./specs/001-codegen-cleanup/contracts/validate-batch.sh

# Or manually:
sbcl --eval "(asdf:test-system :clysm)" && \
sbcl --load build/stage1-complete.lisp && \
wasm-tools validate dist/clysm-stage1.wasm && \
wc -l src/clysm/compiler/codegen/func-section.lisp
```

### Step 4: Commit if passing
```bash
git add src/clysm/compiler/codegen/func-section.lisp
git commit -m "refactor: remove dead code - compile-string-trim

Migrated to runtime library (001-string-runtime-migration).
Lines removed: 45
"
```

## Quasiquote Migration

### Find patterns to migrate
```bash
# Count remaining patterns
grep -c ",@" src/clysm/compiler/codegen/func-section.lisp

# List locations
grep -n ",@" src/clysm/compiler/codegen/func-section.lisp
```

### Transformation pattern
```lisp
;; Before
(defun compile-foo (args env)
  `(,@(compile-expr (car args) env)
    ,@(compile-expr (cadr args) env)
    (i32.add)))

;; After
(defun compile-foo (args env)
  (with-instruction-collector
    (emit* (compile-expr (car args) env))
    (emit* (compile-expr (cadr args) env))
    (emit :i32.add)))
```

### Validate after migration
Same as dead code removal - run full validation suite.

## Progress Tracking

### Current Status
```bash
# Check line count
wc -l src/clysm/compiler/codegen/func-section.lisp

# Check quasiquote patterns remaining
grep -c ",@" src/clysm/compiler/codegen/func-section.lisp
```

### Target Metrics
| Metric | Current | Target |
|--------|---------|--------|
| func-section.lisp lines | 15,973 | <8,000 |
| Quasiquote patterns | 111 | 0 |
| Test pass rate | 100% | 100% |
| Wasm validation | PASS | PASS |

## Troubleshooting

### Test failure after removal
1. Check test error message
2. Find which function the test expects
3. Restore the function: `git checkout -- func-section.lisp`
4. Investigate why function was needed

### Stage 1 compilation failure
1. Check error for undefined function
2. The function may have live callers not visible in grep
3. Check macro expansions that might call the function
4. Restore and investigate

### Wasm validation failure
1. Run `wasm-tools print dist/clysm-stage1.wasm` to inspect
2. Look for type mismatches or missing exports
3. Usually indicates a function was called but removed
4. Restore and investigate call paths

## Next Steps

After completing this cleanup:
1. Update CLAUDE.md with new line counts
2. Create PR with all changes
3. Run `/speckit.tasks` for detailed task breakdown
