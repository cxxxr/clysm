# Quickstart: Numeric Accessors and Float Special Values

**Feature**: 019-numeric-accessors
**Date**: 2025-12-24

## Overview

This feature adds:
1. `numerator` and `denominator` accessor functions for ratios and integers
2. Correct IEEE 754 special value handling (NaN, ±Infinity) in comparisons
3. Double-float precision preservation

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify existing tests (should have some failures)
./run-tests.sh integration
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add numerator/denominator compilation |
| `src/clysm/compiler/codegen/gc-types.lisp` | Existing $RATIO type (index 15) |
| `tests/integration/ratio-test.lisp` | Tests for numerator/denominator |
| `tests/integration/float-test.lisp` | Tests for float special values |

## Implementation Steps

### Step 1: Implement `numerator`

Add to func-section.lisp:

```lisp
(defun compile-numerator (args env)
  "Compile (numerator x) - extract numerator from rational."
  (let ((ratio-type clysm/compiler/codegen/gc-types:+type-ratio+)
        (bignum-type clysm/compiler/codegen/gc-types:+type-bignum+))
    (append
     (compile-to-instructions (first args) env)
     `((:block (:result :anyref)
         ;; Check if fixnum - return self
         (:block
           (:local.get 0)
           (:br_on_cast_fail 0 :anyref :i31)
           (:br 1))
         ;; Check if bignum - return self
         (:block
           (:local.get 0)
           (:br_on_cast_fail 0 :anyref (:ref ,bignum-type))
           (:br 1))
         ;; Check if ratio - return numerator field
         (:local.get 0)
         (:ref.cast (:ref ,ratio-type))
         (:struct.get ,ratio-type 0))))))
```

### Step 2: Implement `denominator`

```lisp
(defun compile-denominator (args env)
  "Compile (denominator x) - extract denominator from rational."
  (let ((ratio-type clysm/compiler/codegen/gc-types:+type-ratio+)
        (bignum-type clysm/compiler/codegen/gc-types:+type-bignum+))
    (append
     (compile-to-instructions (first args) env)
     `((:block (:result :anyref)
         ;; Check if fixnum - return 1
         (:block
           (:local.get 0)
           (:br_on_cast_fail 0 :anyref :i31)
           :drop
           (:i32.const 1) :ref.i31
           (:br 1))
         ;; Check if bignum - return 1
         (:block
           (:local.get 0)
           (:br_on_cast_fail 0 :anyref (:ref ,bignum-type))
           :drop
           (:i32.const 1) :ref.i31
           (:br 1))
         ;; Check if ratio - return denominator field
         (:local.get 0)
         (:ref.cast (:ref ,ratio-type))
         (:struct.get ,ratio-type 1))))))
```

### Step 3: Register in compile-call

Add to the `compile-call` function:

```lisp
((string= fn-name "NUMERATOR")
 (compile-numerator args env))
((string= fn-name "DENOMINATOR")
 (compile-denominator args env))
```

### Step 4: Verify Float Comparisons

Extend `compile-comparison-op` to handle floats:

```lisp
;; In compile-comparison-op, add float dispatch:
(:block
  ;; Check if both are floats
  (:local.get 0)
  (:ref.test (:ref ,float-type))
  (:local.get 1)
  (:ref.test (:ref ,float-type))
  :i32.and
  (:br_if 0)
  ;; Both floats - use f64 comparison
  (:local.get 0)
  (:ref.cast (:ref ,float-type))
  (:struct.get ,float-type 0)
  (:local.get 1)
  (:ref.cast (:ref ,float-type))
  (:struct.get ,float-type 0)
  :f64.eq  ; or f64.lt, f64.gt, etc.
  ...)
```

## Testing

```bash
# Run ratio tests (should now pass)
./run-tests.sh integration ratio

# Run float tests (should now pass)
./run-tests.sh integration float

# Run all tests
nix flake check
```

## Expected Test Results

After implementation:

```
test-ratio-numerator ... PASS
test-ratio-denominator ... PASS
test-positive-infinity ... PASS
test-negative-infinity ... PASS
test-nan-arithmetic ... PASS
test-double-precision ... PASS
```

## Common Issues

1. **Type cast failures**: Ensure proper type index in `ref.cast`
2. **Field index mismatch**: Ratio numerator=0, denominator=1
3. **NaN comparison returning T**: Use `f64.eq` not custom equality
4. **Precision loss**: Ensure all float ops use `f64`
