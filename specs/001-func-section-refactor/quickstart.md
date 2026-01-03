# Quickstart: func-section.lisp Refactoring

**Feature**: 001-func-section-refactor
**Date**: 2026-01-03

## Prerequisites

1. **Environment**: Nix development shell
   ```bash
   nix develop
   ```

2. **Verify baseline**: Ensure all tests pass before starting
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   ```

3. **Measure baseline**: Record current metrics
   ```bash
   wc -l src/clysm/compiler/codegen/func-section.lisp
   # Expected: ~16,483 lines

   sbcl --load build/stage1-complete.lisp
   wasm-tools validate dist/clysm-stage1.wasm
   # Expected: exit code 0
   ```

## Development Workflow

### Phase R4-1: Instruction Collector (First)

1. **Create test file**:
   ```bash
   touch tests/unit/instruction-collector-test.lisp
   ```

2. **Write failing test** (TDD):
   ```lisp
   (deftest test-instruction-collector
     (let ((result (with-instruction-collector (instrs)
                     (emit '(:i32.const 42))
                     (emit '(:local.set 0)))))
       (ok (equal result '((:i32.const 42) (:local.set 0))))))
   ```

3. **Run test (should fail)**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   ```

4. **Implement macro** in func-section.lisp

5. **Run test (should pass)**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   ```

6. **Convert one function** to use new macro, verify tests pass

7. **Iterate** until all append patterns converted

### Phase R4-2: Dispatch Table

1. **Create new file**:
   ```bash
   touch src/clysm/compiler/codegen/primitive-dispatch.lisp
   ```

2. **Write tests** in `tests/unit/primitive-dispatch-test.lisp`

3. **Implement tables and registration**

4. **Migrate primitives incrementally**:
   - Start with arithmetic (+, -, *, /)
   - Then type predicates (consp, symbolp, etc.)
   - Then list operations (car, cdr, cons)
   - Verify tests pass after each group

5. **Remove legacy case branches** as primitives migrate

### Phase R4-3: cXXr Consolidation

1. **Export compile-cxr-chain**:
   ```lisp
   ;; In clysm/compiler/codegen package
   (:export #:compile-cxr-chain)
   ```

2. **Create define-cxr-compiler macro**

3. **Replace 12 individual functions** with macro calls

4. **Verify P626 error pattern eliminated**:
   ```bash
   sbcl --load build/stage1-complete.lisp 2>&1 | grep -c "COMPILE-CXR-CHAIN"
   # Expected: 0
   ```

### Phase R4-4 through R4-7: Runtime Migration

For each function family:

1. **Create runtime implementation** in `lib/*-runtime.lisp`

2. **Register in `*runtime-function-table*`**

3. **Remove inline Wasm codegen** from func-section.lisp

4. **Verify**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   sbcl --load build/stage1-complete.lisp
   wasm-tools validate dist/clysm-stage1.wasm
   ```

## Verification Commands

### After Each Phase

```bash
# 1. Run tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# 2. Generate Stage 1
sbcl --load build/stage1-complete.lisp

# 3. Validate Wasm
wasm-tools validate dist/clysm-stage1.wasm

# 4. Check line count
wc -l src/clysm/compiler/codegen/func-section.lisp

# 5. Check compilation rate (from stage1-report.json)
cat dist/stage1-report.json | jq '.compilation_rate'
```

### Final Verification

```bash
# Full test suite
sbcl --eval "(asdf:test-system :clysm)" --quit

# Stage 1 generation with verbose output
sbcl --load build/stage1-complete.lisp --verbose

# Wasm validation
wasm-tools validate dist/clysm-stage1.wasm

# Line count target
wc -l src/clysm/compiler/codegen/func-section.lisp
# Target: < 8,000 lines

# Error pattern check
sbcl --load build/stage1-complete.lisp 2>&1 | grep -E "P457|P626|P334"
# Expected: no matches
```

## Rollback Procedure

If a phase causes regression:

1. **Identify failing tests**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit 2>&1 | grep FAIL
   ```

2. **Git stash or revert**:
   ```bash
   git stash  # or git checkout -- <file>
   ```

3. **Verify baseline restored**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   ```

4. **Investigate in isolation** before re-attempting

## Success Metrics

| Metric | Baseline | Target | How to Measure |
|--------|----------|--------|----------------|
| func-section.lisp lines | 16,483 | <8,000 | `wc -l` |
| Tests passing | 100% | 100% | `asdf:test-system` |
| Wasm validation | Pass | Pass | `wasm-tools validate` |
| Compilation rate | 19.20% | ≥19.20% | stage1-report.json |
| P457 errors | >0 | 0 | grep compile log |
| P626 errors | >0 | 0 | grep compile log |
| P334 errors | >0 | 0 | grep compile log |

## Common Issues

### "Undefined function COMPILE-CXR-CHAIN"
**Solution**: Export from clysm/compiler/codegen package

### Hash table lookup returns NIL
**Solution**: Verify registration happened before lookup (load order)

### Tests pass but Stage 1 fails
**Solution**: Check runtime function table registration for Wasm dispatch

### Wasm validation fails
**Solution**: Run `wasm-tools print dist/clysm-stage1.wasm` to identify malformed section
