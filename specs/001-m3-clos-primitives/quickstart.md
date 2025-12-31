# Quickstart: Phase 13D M3 - CLOS Primitives for Wasm

**Date**: 2025-12-31
**Feature**: 001-m3-clos-primitives

## Prerequisites

- SBCL 2.4+ installed
- Nix environment configured (see `flake.nix`)
- `wasm-tools` available (via Nix)

## Development Environment

```bash
# Enter Nix development shell
nix develop

# Load the compiler in SBCL
sbcl --load "clysm.asd"
```

## Key Files to Modify

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add CLOS primitive compilation handlers |
| `src/clysm/compiler/codegen/gc-types.lisp` | Reference only (no changes needed) |

## Implementation Steps

### 1. Add Array Opcodes (if missing)

Check `*wasm-opcodes*` in `func-section.lisp` for:
```lisp
(:array.get . (#xFB #x0B))
(:array.set . (#xFB #x0E))
(:array.new_default . (#xFB #x1B))
```

### 2. Add Primitive Handlers

In `compile-call` function, add cases:
```lisp
(case name
  ((clysm/clos/slot-access:slot-value*)
   (compile-slot-value-read ast env))
  ((clysm/clos/instance:make-instance*)
   (compile-make-instance ast env))
  ((clysm/clos/mop:standard-instance-p)
   (compile-standard-instance-p ast env))
  ...)
```

### 3. Implement Handlers

See `research.md` for detailed instruction sequences.

## Testing Commands

### Unit Tests
```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific test file
sbcl --eval "(asdf:test-system :clysm/tests/unit/clos-primitives-test)"
```

### Contract Tests
```bash
# Verify Wasm bytecode structure
sbcl --eval "(asdf:test-system :clysm/tests/contract/clos-wasm-test)"
```

### Stage 1 Generation
```bash
# Generate Stage 1 with compilation report
sbcl --load build/stage1-complete.lisp --verbose

# Check compilation rate in report
cat dist/stage1-report.json | jq '.compilation_rate'
```

### Wasm Validation
```bash
# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0
```

## Verification Checklist

- [ ] `(defstruct point x y)` compiles without errors
- [ ] Generated Wasm passes `wasm-tools validate`
- [ ] Stage 1 compilation rate >= 25%
- [ ] DEFSTRUCT failures < 100 (down from 1953)
- [ ] DEFINE-CONDITION failures < 50 (down from 302)
- [ ] All existing tests pass (no regressions)

## Quick Test

```lisp
;; In SBCL REPL after loading clysm
(clysm:compile-to-wasm '(defstruct point x y))
;; Should return Wasm bytecode without errors
```

## Debugging

### Check Generated Instructions
```lisp
;; Trace compilation to see IR
(trace clysm/compiler/codegen/func-section:compile-to-instructions)
(clysm:compile-to-wasm '(slot-value* instance 'x))
```

### Inspect Wasm Output
```bash
# Disassemble generated Wasm
wasm-tools print dist/clysm-stage1.wasm > stage1.wat
# Search for struct operations
grep -n "struct\." stage1.wat
```

## Success Criteria

| Metric | Target | How to Verify |
|--------|--------|---------------|
| Compilation Rate | >= 25% | `dist/stage1-report.json` |
| DEFSTRUCT Failures | < 100 | `dist/stage1-report.json` |
| Wasm Validation | Exit 0 | `wasm-tools validate` |
| Test Suite | All Pass | `asdf:test-system` |
