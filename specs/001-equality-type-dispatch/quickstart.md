# Quickstart: Equality Predicate Type-Dispatch Consolidation

**Date**: 2026-01-03

## Prerequisites

- SBCL 2.4+
- wasm-tools (for validation)
- nix develop (recommended for consistent environment)

## Development Workflow

### 1. Baseline Capture

Before any changes, capture the current Wasm output for regression testing:

```bash
# Enter nix shell
nix develop

# Run existing tests to ensure baseline works
sbcl --eval "(asdf:test-system :clysm)"

# Capture baseline Wasm for equality predicates
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(clysm:compile-to-wasm '(eq 'a 'b))" \
     --eval "(clysm:compile-to-wasm '(eql 1 1))" \
     --eval "(clysm:compile-to-wasm '(equal \"a\" \"a\"))" \
     --eval "(clysm:compile-to-wasm '(equalp \"A\" \"a\"))"
```

### 2. Implementation Order

1. **Create helper functions** (new code, insert before line 4339):
   - `emit-null-comparison`
   - `emit-i31-comparison`
   - `emit-float-comparison`
   - `emit-ratio-comparison`
   - `emit-string-comparison`
   - `emit-cons-comparison`
   - `emit-default-comparison`
   - `compile-type-dispatch`
   - `compile-equality-predicate`

2. **Test each helper** in isolation with unit tests

3. **Replace implementations** one at a time:
   - Replace `compile-eq` → verify tests pass
   - Replace `compile-eql` → verify tests pass
   - Replace `compile-equal` → verify tests pass
   - Replace `compile-equalp` → verify tests pass

4. **Remove old code** after all replacements verified

### 3. Verification Commands

```bash
# Run unit tests
sbcl --eval "(asdf:test-system :clysm)"

# Run Stage 1 compilation
sbcl --load build/stage1-complete.lisp

# Validate Wasm output
wasm-tools validate dist/clysm-stage1.wasm

# Check line counts
wc -l src/clysm/compiler/codegen/func-section.lisp
# Target: < 15,700

# Check equality code lines (after refactoring)
sed -n '4339,4739p' src/clysm/compiler/codegen/func-section.lisp | wc -l
# Target: < 400
```

### 4. Test Commands

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific equality tests
sbcl --eval "(asdf:load-system :clysm/test)" \
     --eval "(rove:run :clysm/test/equality-predicates)"

# Run contract tests
node tests/contract/equality-wasm-test.lisp

# Run integration tests
tests/integration/equality-ansi-test.lisp
```

### 5. Wasm Binary Comparison

```bash
# Generate Wasm before and after
# Before:
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(with-open-file (f \"before.wasm\" :direction :output :element-type '(unsigned-byte 8)) (clysm:compile-to-wasm-stream '(eq 'a 'b) f))"

# After refactoring:
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(with-open-file (f \"after.wasm\" :direction :output :element-type '(unsigned-byte 8)) (clysm:compile-to-wasm-stream '(eq 'a 'b) f))"

# Compare
cmp before.wasm after.wasm
# Should output nothing (files identical)
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Main implementation |
| `src/clysm/compiler/codegen/primitive-registry.lisp` | Primitive registration |
| `tests/unit/equality-predicates-test.lisp` | Unit tests |
| `tests/contract/equality-wasm-test.lisp` | Contract tests |
| `tests/integration/equality-ansi-test.lisp` | Integration tests |

## Expected Outcomes

| Metric | Before | After |
|--------|--------|-------|
| func-section.lisp lines | 16,097 | < 15,700 |
| Equality code lines | 840 | < 400 |
| Tests passing | All | All |
| Wasm validation | Pass | Pass |
| Wasm output | Baseline | Identical to baseline |
