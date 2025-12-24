# Quickstart: Tail Call Optimization

**Date**: 2025-12-24
**Feature**: 009-tail-call-optimization
**Status**: ✅ IMPLEMENTED

## Verification

TCO is now fully implemented. Use these examples to verify correct behavior.

### 1. Run Tests

```bash
# All 42 tests should pass including TCO tests
nix flake check

# Or run tests directly
sbcl --noinform --eval '(asdf:test-system :clysm/tests)' --quit
```

### 2. Verify WAT Output Contains return_call

```lisp
;; Load the system
(asdf:load-system :clysm)

;; Compile a tail-recursive function
(print (clysm/compiler:compile-to-wat
        '(progn
           (defun count-down (n)
             (if (= n 0) 0 (count-down (- n 1))))
           (count-down 5))))
```

Expected output contains:
```wat
return_call 1
```

### 3. Verify return_call_ref for funcall

```lisp
(print (clysm/compiler:compile-to-wat
        '(defun apply-f (f x)
           (funcall f x))))
```

Expected output contains:
```wat
return_call_ref 9
```

### 4. Test Deep Recursion (10,000+ calls)

```lisp
;; This runs without stack overflow due to TCO
(clysm/tests:compile-and-run
 '(progn
    (defun go-deep (n)
      (if (= n 0) 0 (go-deep (- n 1))))
    (go-deep 10000)))
;; => 0
```

### 5. Test Tail-Recursive Factorial

```lisp
;; Accumulator-style factorial
(clysm/tests:compile-and-run
 '(progn
    (defun fact-iter (n acc)
      (if (= n 0) acc (fact-iter (- n 1) (* n acc))))
    (defun fact (n) (fact-iter n 1))
    (fact 5)))
;; => 120
```

### 6. Test Mutual Recursion

```lisp
(clysm/tests:compile-and-run
 '(progn
    (defun f (n) (if (= n 0) 0 (g (- n 1))))
    (defun g (n) (if (= n 0) 0 (f (- n 1))))
    (f 1000)))
;; => 0
```

## Implementation Summary

| Component | Description |
|-----------|-------------|
| `in-tail-position` slot in cenv | Tracks whether current form is in tail position |
| `env-with-tail` / `env-with-non-tail` | Helper functions to set/clear tail position |
| `compile-if` | Propagates tail position to both branches |
| `compile-progn` | Propagates tail position only to last form |
| `compile-let` | Propagates tail position to last body form |
| `compile-regular-call` | Emits `:return_call` when in tail position |
| `compile-funcall` | Emits `:return_call_ref` when in tail position |
| `compile-local-function-call` | Emits `:return_call_ref` when in tail position |
| Opcodes | 0x12 (`return_call`), 0x15 (`return_call_ref`) |

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Tail position tracking and propagation |
| `src/clysm/compiler/compiler.lisp` | Instruction emission for return_call/return_call_ref |
| `tests/unit/tail-position-test.lisp` | Unit tests for tail position detection |
| `tests/integration/tco-test.lisp` | Integration tests for TCO behavior |

## Debugging

```lisp
;; Inspect generated instructions
(clysm/compiler:compile-expression '(defun f (n) (f (- n 1))))
;; Look for :return_call in the instruction list

;; Or check WAT output
(clysm/compiler:compile-to-wat '(defun f (n) (f (- n 1))))
```
