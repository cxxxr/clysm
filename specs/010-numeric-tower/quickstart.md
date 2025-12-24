# Quickstart: Numeric Tower Implementation

**Feature**: 010-numeric-tower | **Date**: 2025-12-24

## Prerequisites

1. **Development environment**:
   ```bash
   nix develop
   ```

2. **Verify wasmtime supports required features**:
   ```bash
   wasmtime --wasm gc --wasm function-references --wasm exceptions --version
   ```

3. **Run existing tests to ensure baseline**:
   ```bash
   nix flake check
   ```

## Development Workflow

### 1. TDD Cycle (per Constitution VII)

```
Red → Green → Refactor
```

For each new numeric operation:
1. Write failing test first
2. Get user approval on test
3. Implement minimal code to pass
4. Refactor if needed
5. Commit when green

### 2. Type Definition Order

Add types in dependency order:

1. **$limb_array** (type 18) - Array type for bignum limbs
2. **$bignum** (type 14) - Depends on $limb_array
3. **$ratio** (type 15) - Depends on integer types
4. **$float** (type 16) - Standalone
5. **$complex** (type 17) - Depends on all real types

### 3. Implementation Priority

Follow spec user story priorities:

| Priority | Component | Test File |
|----------|-----------|-----------|
| P1 | Bignum type + arithmetic | `tests/integration/bignum-test.lisp` |
| P1 | Ratio type + arithmetic | `tests/integration/ratio-test.lisp` |
| P2 | Float type + arithmetic | `tests/integration/float-test.lisp` |
| P2 | Complex type + arithmetic | `tests/integration/complex-test.lisp` |
| P2 | Math functions | `tests/unit/math-functions-test.lisp` |
| P3 | Type predicates | `tests/unit/numeric-predicates-test.lisp` |

## Key Files to Modify

### gc-types.lisp

Add type constants and constructors:

```lisp
;; Add after existing constants
(defconstant +type-bignum+ 14)
(defconstant +type-ratio+ 15)
(defconstant +type-float+ 16)
(defconstant +type-complex+ 17)
(defconstant +type-limb-array+ 18)

;; Add type constructors
(defun make-bignum-type () ...)
(defun make-ratio-type () ...)
(defun make-float-type () ...)
(defun make-complex-type () ...)
```

### func-section.lisp

Extend arithmetic compilation:

```lisp
;; In compile-primitive
(defun compile-numeric-add (args env)
  "Compile addition with type dispatch"
  (cond
    ((all-fixnums? args) (compile-fixnum-add args env))
    (t (compile-generic-add args env))))
```

### compiler.lisp

Add type coercion:

```lisp
(defun coerce-to-common-type (type1 type2)
  "Determine common type for mixed arithmetic"
  ...)
```

## Test Template

```lisp
(deftest test-bignum-addition
  "Verify bignum addition works correctly"
  ;; Overflow fixnum range
  (ok (= 9223372036854775808
         (clysm/tests:compile-and-run
           '(+ 9223372036854775807 1)))
      "Fixnum overflow should produce correct bignum"))
```

## Validation Commands

```bash
# Run specific test file
sbcl --eval "(ql:quickload :clysm/tests)" \
     --eval "(rove:run :clysm/tests/integration/bignum-test)"

# Validate generated Wasm
wasm-tools validate output.wasm

# Execute with wasmtime
wasmtime --wasm gc output.wasm
```

## Common Issues

### Issue: Type index mismatch

**Symptom**: `wasm-tools validate` fails with type error

**Fix**: Ensure `generate-type-definitions` returns types in correct index order

### Issue: i31ref overflow not detected

**Symptom**: Incorrect results for large numbers

**Fix**: Check overflow after i32 arithmetic, before `ref.i31`

### Issue: Ratio not simplified

**Symptom**: `(/ 6 3)` returns ratio instead of 2

**Fix**: Call `simplify-ratio` after every ratio operation

## Reference

- [CLHS 12.1 Number Concepts](https://lisp-docs.github.io/cl-language-reference/chap-12/bc-b-number-concepts)
- [WasmGC Specification](https://webassembly.github.io/gc/core/)
- [research.md](./research.md) - Design decisions
- [data-model.md](./data-model.md) - Type structures
- [contracts/](./contracts/) - API contracts
