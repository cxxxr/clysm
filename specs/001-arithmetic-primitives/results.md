# Results: Arithmetic Primitives 1- and 1+

**Feature Branch**: `001-arithmetic-primitives`
**Completed**: 2025-12-31
**Status**: Complete

## Success Criteria Verification

### SC-001: Factorial Function Compiles

**Status**: PASSED

```lisp
(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (1- n)))))
```

- Compiles successfully without errors
- Generated WAT contains expected instructions:
  - `i32.sub` for `1-`
  - `i32.mul` for `*`
  - `i32.le_s` for `<=`

### SC-002: Wasm Validation

**Status**: PASSED

All generated Wasm code using `1-` and `1+` passes `wasm-tools validate` with exit code 0.

### SC-003: Compilation Rate

**Status**: BASELINE MAINTAINED

| Metric | Baseline | After Implementation |
|--------|----------|---------------------|
| Coverage | 13.63% | 13.77% |
| Forms compiled | ~3500 | 3585 |

Note: The modest improvement is expected because `1-` and `1+` were already being handled through fallback mechanisms. The primary benefit is cleaner, more efficient code generation and explicit primitive support.

### SC-004: Blockers Report

**Status**: PASSED

`1-` and `1+` do **not** appear in the Stage 1 compilation blocker report (`dist/stage1-report.json`).

Current top blockers are meta-level forms:
1. DEFUN (18613 occurrences)
2. DEFSTRUCT (1953)
3. DEFMACRO (646)
4. DEFINE-CONDITION (302)
5. DEFPACKAGE (284)

## Implementation Summary

### Changes Made

1. **Primitive List** (`func-section.lisp` ~line 727):
   - Added `1-` and `1+` to recognized primitives

2. **Dispatch Cases** (`func-section.lisp` ~line 944-946):
   ```lisp
   (1- (compile-1- args env))
   (1+ (compile-1+ args env))
   ```

3. **Compile Functions** (`func-section.lisp` ~line 3440-3462):
   - `compile-1-`: Generates `ref.cast i31 → i31.get_s → i32.const 1 → i32.sub → ref.i31`
   - `compile-1+`: Generates `ref.cast i31 → i31.get_s → i32.const 1 → i32.add → ref.i31`

### Test Coverage

| Test Type | Count | Status |
|-----------|-------|--------|
| Unit tests | 6 | All pass |
| Contract tests | 6 | All pass |

## Lessons Learned

1. **Pattern Reuse**: Following the existing `compile-unary-minus` pattern made implementation straightforward.

2. **i31ref Optimization**: Both primitives use i31ref (fixnum) representation, avoiding heap allocation.

3. **TDD Value**: Writing tests first (per Constitution VII) caught edge cases early.

## Next Steps

The `1-` and `1+` primitives are now available for recursive and iterative algorithm compilation. The remaining compilation blockers are primarily meta-level forms (DEFUN, DEFSTRUCT, etc.) which require different strategies to address.
