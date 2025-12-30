# Quickstart: Phase 14A - Basic Arithmetic Function Extension

**Date**: 2025-12-30
**Branch**: `002-numeric-functions`

## Overview

This guide provides a quick start for implementing the 26 numeric functions in Phase 14A. The implementation follows TDD methodology as required by the constitution.

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify tools are available
wasm-tools --version
wasmtime --version
sbcl --version
```

## Implementation Workflow (TDD)

For each function, follow this cycle:

1. **Write test** (Red)
2. **Get user approval**
3. **Verify test fails**
4. **Implement function** (Green)
5. **Refactor if needed**
6. **Commit**

## Quick Implementation Reference

### Category 1: Trigonometric Functions (FFI)

These use existing FFI infrastructure. Main work is compile-time dispatch.

**Files to modify**:
- `src/clysm/compiler/codegen/func-section.lisp`

**Pattern**:
```lisp
;; In compile-call, add case for trig functions
(defun compile-trig-call (name args env)
  "Compile trigonometric function call"
  ;; 1. Ensure math imports registered
  (register-math-imports env)
  ;; 2. Compile argument to f64
  (compile-to-f64 (first args) env)
  ;; 3. Call imported function
  (emit-call-import (math-import-index name env)))
```

**Test example**:
```lisp
(deftest test-sin-basic
  (ok (approx= 0.0 (compile-and-run '(sin 0)) 1e-10))
  (ok (approx= 1.0 (compile-and-run '(sin (/ pi 2))) 1e-10)))
```

### Category 2: Bit Operations (Native Wasm)

These use native Wasm i32/i64 instructions.

**Files to modify**:
- `src/clysm/compiler/codegen/func-section.lisp`

**Pattern for variadic (logand, logior, logxor)**:
```lisp
(defun compile-logand (args env)
  "Compile bitwise AND"
  (cond
    ((null args)
     ;; Identity: -1
     (emit-i32-const -1)
     (emit-ref-i31))
    ((= 1 (length args))
     (compile-to-i32 (first args) env))
    (t
     ;; Fold with i32.and
     (compile-to-i32 (first args) env)
     (dolist (arg (rest args))
       (compile-to-i32 arg env)
       (emit :i32.and))
     (emit-ref-i31))))
```

**Pattern for ash**:
```lisp
(defun compile-ash (args env)
  "Compile arithmetic shift"
  (let ((integer (first args))
        (count (second args)))
    (compile-to-i32 integer env)
    (compile-to-i32 count env)
    ;; Check sign of count at runtime
    (emit-if-else
      ;; Condition: count >= 0
      (lambda ()
        (emit :local.get count-local)
        (emit-i32-const 0)
        (emit :i32.ge_s))
      ;; Then: left shift
      (lambda ()
        (emit :local.get int-local)
        (emit :local.get count-local)
        (emit :i32.shl))
      ;; Else: right shift (negate count)
      (lambda ()
        (emit :local.get int-local)
        (emit-i32-const 0)
        (emit :local.get count-local)
        (emit :i32.sub)
        (emit :i32.shr_s)))
    (emit-ref-i31)))
```

**Test example**:
```lisp
(deftest test-ash-basic
  (ok (= 1024 (compile-and-run '(ash 1 10))))
  (ok (= 1 (compile-and-run '(ash 1024 -10))))
  (ok (= -1024 (compile-and-run '(ash -1 10)))))
```

### Category 3: Mathematical Functions

Mix of native Wasm and FFI.

**sqrt - Native Wasm**:
```lisp
(defun compile-sqrt (args env)
  "Compile square root using f64.sqrt"
  (compile-to-f64 (first args) env)
  (emit :f64.sqrt)
  (emit-wrap-f64))
```

**exp, log, expt - FFI**:
```lisp
(defun compile-log (args env)
  "Compile logarithm"
  (register-math-imports env)
  (case (length args)
    (1 ;; Natural log
     (compile-to-f64 (first args) env)
     (emit-call-import (math-import-index 'log env)))
    (2 ;; Log with base: log(x)/log(base)
     (compile-to-f64 (first args) env)
     (emit-call-import (math-import-index 'log env))
     (compile-to-f64 (second args) env)
     (emit-call-import (math-import-index 'log env))
     (emit :f64.div)))
  (emit-wrap-f64))
```

### Category 4: Type Dispatch Functions

**abs**:
```lisp
(defun compile-abs (arg env)
  "Compile absolute value with type dispatch"
  (compile-to-instructions arg env)
  (emit-type-dispatch
    ;; Fixnum case
    (lambda ()
      (emit :i31.get_s)
      (emit-if-else
        (lambda () (emit :i32.const 0) (emit :i32.lt_s))
        (lambda () (emit :i32.const 0) (emit :i32.sub))
        #'identity)
      (emit :ref.i31))
    ;; Float case
    (lambda ()
      (emit :struct.get :$float 0)
      (emit :f64.abs)
      (emit-struct-new :$float))))
```

### Category 5: Parse-Integer

Most complex function. Implement state machine.

**Skeleton**:
```lisp
(defun compile-parse-integer (args env)
  "Compile parse-integer with full ANSI CL compliance"
  ;; Parse keyword arguments
  (multiple-value-bind (string start end radix junk-allowed)
      (parse-parse-integer-args args)
    ;; Generate state machine
    ;; See data-model.md for state transitions
    ...))
```

## Testing Commands

```bash
# Run all numeric tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# Run specific test file
sbcl --load "tests/unit/math-functions-test.lisp" --eval "(rove:run-test 'test-sin-basic)" --quit

# Validate generated Wasm
wasm-tools validate dist/output.wasm

# Run integration test
node tests/integration/run-numeric-tests.js
```

## Priority Order

Implement in this order (based on spec priorities):

### P1 (Highest Priority)
1. **Trigonometric**: sin, cos, tan, asin, acos, atan
2. **Bit Operations**: ash, logand, logior, logxor, lognot, logcount

### P2 (Medium Priority)
3. **Mathematical**: exp, log, sqrt, expt, abs, signum
4. **Hyperbolic**: sinh, cosh, tanh, asinh, acosh, atanh

### P3 (Lower Priority)
5. **Type Conversion**: float, rational
6. **String Parsing**: parse-integer

## Verification Checklist

After implementing each function:

- [ ] Test compiles and fails initially (Red)
- [ ] Implementation makes test pass (Green)
- [ ] Generated Wasm passes `wasm-tools validate`
- [ ] HyperSpec link included in code comments
- [ ] Edge cases tested (NaN, infinity, boundaries)
- [ ] Committed with passing tests

## Reference Files

| Purpose | Location |
|---------|----------|
| Existing arithmetic | `src/clysm/compiler/codegen/func-section.lisp:904-912` |
| FFI imports | `src/clysm/ffi/import-gen.lisp:370-423` |
| Math shim | `host-shim/math-shim.js` |
| Existing tests | `tests/unit/math-functions-test.lisp` |
| Function contracts | `specs/002-numeric-functions/contracts/function-signatures.lisp` |
| Data model | `specs/002-numeric-functions/data-model.md` |

## Common Issues

### FFI Function Not Found
Ensure function is registered in `*math-function-specs*` and `register-math-imports` is called before use.

### Type Mismatch
Ensure numeric type conversion before Wasm instructions:
- Use `compile-to-f64` for float operations
- Use `compile-to-i32` for integer operations
- Use `emit-ref-i31` to wrap i32 results

### Overflow in Bit Operations
For values exceeding 32-bit range, promote to i64 or bignum operations.

## Next Steps

After completing implementation:
1. Run `/speckit.tasks` to generate task breakdown
2. Create feature branch commits following TDD cycle
3. Verify 50%+ ANSI CL numeric test compliance
4. Create PR for review
