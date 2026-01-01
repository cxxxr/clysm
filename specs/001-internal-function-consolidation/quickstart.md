# Quickstart: Compiler Internal Function Consolidation

**Date**: 2026-01-01
**Branch**: `001-internal-function-consolidation`

## Prerequisites

- SBCL 2.4+ installed
- Nix development environment: `nix develop`
- wasm-tools available in PATH

## Quick Verification Commands

### Check Current State

```bash
# Enter development environment
nix develop

# Check current func-section.lisp line count
wc -l src/clysm/compiler/codegen/func-section.lisp
# Expected: 18351

# Run current Stage 1 generation to get baseline
sbcl --load build/stage1-complete.lisp 2>&1 | grep "compilation rate"
# Expected: ~22.15%

# Check for undefined function errors
sbcl --load build/stage1-complete.lisp 2>&1 | grep -E "ENV-ADD-LOCAL|COMPILE-TO-INSTRUCTIONS|MAKE-WASM-STRUCT-TYPE|COMPILE-UNARY-MATH-FFI|AST-LITERAL-VALUE|COMPILE-CXR-CHAIN|LOOP-KEYWORD-EQ"
```

### Verify Exports

```bash
# Check if functions are exported from clysm package
sbcl --eval '(ql:quickload :clysm)' \
     --eval '(print (find-symbol "ENV-ADD-LOCAL" :clysm))' \
     --eval '(print (find-symbol "COMPILE-UNARY-MATH-FFI" :clysm))' \
     --eval '(print (find-symbol "COMPILE-CXR-CHAIN" :clysm))' \
     --quit
```

### Run Tests

```bash
# Full test suite
sbcl --eval "(asdf:test-system :clysm)"

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
```

## Implementation Workflow

### Step 1: Add Missing Exports

Edit `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
(defpackage #:clysm/compiler/codegen/func-section
  (:use #:cl)
  (:export
   ;; ... existing exports ...
   #:compile-unary-math-ffi    ; ADD THIS
   #:compile-cxr-chain))       ; ADD THIS
```

### Step 2: Re-export from clysm Package

Edit `src/clysm/clysm.lisp` or package definition:

```lisp
;; Add to clysm package re-exports
(:export #:compile-unary-math-ffi
         #:compile-cxr-chain)
```

### Step 3: Remove Dead Code

For each category (io, list, sequence), remove the compile-* functions:

```bash
# Example: Find compile-princ in func-section.lisp
grep -n "defun compile-princ" src/clysm/compiler/codegen/func-section.lisp
# Delete the entire function definition
```

### Step 4: Verify Changes

```bash
# Run tests after each removal
sbcl --eval "(asdf:test-system :clysm)"

# Check line count progress
wc -l src/clysm/compiler/codegen/func-section.lisp

# Final Stage 1 verification
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm
cat dist/stage1-report.json | jq '.compilation_rate'
```

## Success Criteria Verification

| Criterion | Command | Expected |
|-----------|---------|----------|
| SC-001: Compilation rate | `jq '.compilation_rate' dist/stage1-report.json` | ≥ 0.25 |
| SC-002: Zero undefined errors | `grep "undefined" stage1.log \| grep -E "7 functions"` | No matches |
| SC-003: Line count | `wc -l func-section.lisp` | < 12000 |
| SC-004: Wasm validation | `wasm-tools validate dist/clysm-stage1.wasm` | Exit 0 |
| SC-005: Tests pass | `sbcl --eval "(asdf:test-system :clysm)"` | All pass |

## Troubleshooting

### "Package X does not export Y"

The symbol needs to be added to the package's `:export` list.

### Tests fail after dead code removal

A function that appeared dead is actually still called. Restore the function and search for callers:

```lisp
(apropos "compile-function-name")
```

### Stage 1 compilation rate didn't improve

The undefined function errors may not have been the only blockers. Check the stage1-report.json for remaining blockers:

```bash
cat dist/stage1-report.json | jq '.blockers'
```
