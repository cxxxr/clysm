# Quickstart: Phase 13D - True Self-Hosting Achievement

**Feature**: 001-true-self-hosting
**Date**: 2025-12-28

## Prerequisites

- SBCL 2.4+ installed
- Nix with flakes enabled
- wasmtime CLI
- wasm-tools CLI

## Quick Verification

```bash
# Enter dev environment
nix develop

# Current state check
ls -la dist/clysm-stage*.wasm

# Run fixpoint verification
./scripts/verify-fixpoint.sh --json
```

**Expected current output** (pre-implementation):
```json
{
  "status": "ACHIEVED",
  "stage1": { "size_bytes": 17, "valid": true },
  "stage2": { "size_bytes": 17, "valid": true }
}
```

**Target output** (post-implementation):
```json
{
  "status": "ACHIEVED",
  "stage1": { "size_bytes": ">= 1024", "valid": true },
  "stage2": { "size_bytes": ">= 1024", "valid": true }
}
```

---

## Development Workflow

### 1. Run Tests

```bash
# All tests
sbcl --eval "(asdf:test-system :clysm)"

# Stage 0 specific tests (after implementation)
sbcl --eval "(asdf:test-system :clysm/stage0)"
```

### 2. Build Stage 0

```bash
# Generate Stage 0 from SBCL
sbcl --load build/stage0-complete.lisp

# Verify output
wasm-tools validate dist/clysm-stage0.wasm
ls -la dist/clysm-stage0.wasm
```

### 3. Test Compilation

```bash
# Test compile_form (using wasmtime)
wasmtime run --invoke compile_form dist/clysm-stage0.wasm "(+ 1 2)"

# Test with host shim (Node.js)
node host-shim/stage1-host.js --compile "(+ 1 2)"
```

### 4. Generate Stages

```bash
# Generate Stage 1 from Stage 0
sbcl --load build/stage1-gen.lisp

# Generate Stage 2 from Stage 1
sbcl --load build/stage2-gen.lisp

# Verify fixed-point
./scripts/verify-fixpoint.sh --json
```

---

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/stage0/eval.lisp` | Core evaluator (NEW) |
| `src/clysm/stage0/primitives.lisp` | Primitive implementations (NEW) |
| `src/clysm/stage0/compiler.lisp` | Compilation orchestration |
| `src/clysm/stage0/codegen.lisp` | Wasm code generation |
| `build/stage0-complete.lisp` | Stage 0 build script |
| `build/stage1-gen.lisp` | Stage 1 generation |
| `scripts/verify-fixpoint.sh` | Fixed-point verification |

---

## Debugging

### WAT Output

```bash
# Convert Wasm to readable WAT
wasm-tools print dist/clysm-stage0.wasm > stage0.wat

# Inspect exports
grep -A2 "(export" stage0.wat
```

### REPL Testing

```lisp
;; Load Stage 0 infrastructure
(ql:quickload :clysm)
(in-package :clysm/stage0)

;; Test evaluator manually
(eval-form '(+ 1 2) nil)
;; => 3

;; Test with environment
(eval-form 'x '((x . 42)))
;; => 42
```

---

## Success Criteria Checklist

- [ ] `(+ 1 2)` compiles and returns 3
- [ ] `(defun f (x) x)` produces exported function
- [ ] Stage 1 size >= 1024 bytes
- [ ] Stage 1 == Stage 2 (byte-identical)
- [ ] All wasm-tools validate passes
- [ ] Primitives functional: car, cdr, cons, +, -, *, /, <, >, =, eq
- [ ] Control structures: if, let, let*, defun, lambda, quote
- [ ] Nested expressions work: `(+ 1 (* 2 3))` → 7
