# Quickstart: Type Constant and Package Primitive Export

**Feature Branch**: `001-type-package-export`
**Date**: 2026-01-01

## Overview

This feature exports type index constants and package primitives to enable Stage 1 self-compilation. After implementation, P846 (unbound type constants) and P951 (undefined PACKAGEP*) errors will be eliminated.

## Prerequisites

```bash
# Ensure you're in the Nix development environment
nix develop

# Verify tools
sbcl --version    # Should be 2.4+
wasm-tools --version
```

## Quick Verification

### Before Implementation

```bash
# Generate Stage 1 and check current error patterns
sbcl --load build/stage1-complete.lisp

# View error counts in report
jq '.error_patterns[] | select(.pattern_id == "P846" or .pattern_id == "P951")' dist/stage1-report.json
```

Expected output (before):
```json
{"pattern_id": "P846", "count": 25, ...}
{"pattern_id": "P951", "count": 79, ...}
```

### After Implementation

```bash
# Regenerate Stage 1
sbcl --load build/stage1-complete.lisp

# Verify Wasm is valid
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0

# Check errors are eliminated
jq '.error_patterns[] | select(.pattern_id == "P846" or .pattern_id == "P951")' dist/stage1-report.json
```

Expected output (after):
```json
# Empty or count: 0 for both patterns
```

## Key Files Modified

| File | Change |
|------|--------|
| `src/clysm/package.lisp` | Add type constant re-exports to :clysm package |
| `src/clysm/compiler/directive.lisp` | Add DEFCONSTANT handler and *constant-bindings* |
| `src/clysm/compiler/codegen/func-section.lisp` | Add SYMBOL-PACKAGE* to runtime table |
| `src/clysm/lib/package-stubs.lisp` | Implement package primitive bodies |

## Running Tests

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific test suites
sbcl --eval "(asdf:test-system :clysm/tests/unit/constant-export)"
sbcl --eval "(asdf:test-system :clysm/tests/unit/package-primitive)"
```

## Example Usage

### Type Constants in Compiled Code

```lisp
;; Before: Would cause P846 error
(defun check-cons-p (obj)
  (ref.test +TYPE-CONS+ obj))

;; After: Compiles correctly
;; +TYPE-CONS+ is folded to i32.const 2 at compile-time
```

### Package Primitives

```lisp
;; Before: Would cause P951 error
(defun validate-package (x)
  (when (packagep* x)
    (find-package* (package-name x))))

;; After: Compiles to runtime function calls
;; (call $packagep*-rt)
;; (call $find-package*-rt)
```

## Troubleshooting

### P846 Still Occurring

1. Verify DEFCONSTANT handler is loaded:
   ```lisp
   (boundp 'clysm/compiler/directive::*constant-bindings*)
   ```

2. Check constant is registered:
   ```lisp
   (gethash '+type-cons+ clysm/compiler/directive::*constant-bindings*)
   ```

### P951 Still Occurring

1. Verify runtime function registration:
   ```lisp
   (gethash 'clysm:packagep* clysm/compiler/codegen/func-section::*runtime-function-table*)
   ```

2. Check function is exported:
   ```lisp
   (find-symbol "PACKAGEP*" :clysm)
   ```

## Coverage Metrics

| Metric | Before | After |
|--------|--------|-------|
| P846 errors | 25 | 0 |
| P951 errors | 79 | 0 |
| Stage 1 coverage | 21.39% | ~26%+ |
| Total blocked forms | ~104 | ~0 (for these patterns) |
