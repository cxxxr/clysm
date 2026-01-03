# Quickstart: Primitive Dispatch Table

**Feature**: 002-primitive-dispatch-table
**Date**: 2026-01-03

## Overview

This feature migrates 240+ primitive compilers from a 538-line case statement to a hash-table driven dispatch system, enabling O(1) lookup and extensible primitive registration.

## Prerequisites

- SBCL 2.4+ (host Lisp)
- Nix environment (`nix develop`)
- wasm-tools (validation)

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/primitive-dispatch.lisp` | Dispatch infrastructure |
| `src/clysm/compiler/codegen/primitive-registry.lisp` | Primitive registrations |
| `src/clysm/compiler/codegen/func-section.lisp` | Original case statement |
| `tests/unit/primitive-dispatch-test.lisp` | Unit tests |
| `tests/contract/primitive-dispatch-wasm-test.lisp` | Wasm validation tests |

## Quick Verification

```bash
# Enter Nix environment
nix develop

# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
```

## Registering a New Primitive

```lisp
;; In primitive-registry.lisp or your extension file

(register-primitive-compiler 'my-primitive
  #'compile-my-primitive
  :arity 2
  :flags '(:category :custom))

;; Define the compiler function
(defun compile-my-primitive (args env)
  "Compile (my-primitive x y).
   Stack: [] -> [result]"
  (when (/= (length args) 2)
    (error "my-primitive requires 2 arguments"))
  (with-instruction-collector
    (emit* (compile-to-instructions (first args) env))
    (emit* (compile-to-instructions (second args) env))
    ;; Emit Wasm instructions here
    (emit :i32.add)))
```

## Querying the Dispatch Table

```lisp
;; Check if registered
(primitive-registered-p 'car)  ; => T

;; List all primitives
(list-registered-primitives)  ; => (CAR CDR CONS ...)

;; Get primitive info
(get-primitive-info 'cons)
; => #S(PRIMITIVE-ENTRY :COMPILER-FN #<FUNCTION> :ARITY 2 :FLAGS (:CATEGORY :LIST))
```

## Migration Pattern

For each primitive category:

1. **Baseline**: Compile test expressions with current case statement
2. **Register**: Add primitives to `primitive-registry.lisp`
3. **Verify**: Compare Wasm output byte-for-byte
4. **Remove**: Delete corresponding case branches

Example test:
```lisp
(deftest list-primitives-byte-identical
  (let ((baseline (compile-with-case-statement '(cons 1 2)))
        (dispatch (compile-with-dispatch-table '(cons 1 2))))
    (ok (equalp baseline dispatch))))
```

## Development Workflow

1. **Load system**: `(asdf:load-system :clysm)`
2. **Run tests**: `(asdf:test-system :clysm)`
3. **Check primitive**: `(primitive-registered-p 'your-prim)`
4. **Validate Wasm**: `wasm-tools validate output.wasm`

## Common Issues

| Issue | Solution |
|-------|----------|
| "Unknown primitive" | Check registration in primitive-registry.lisp |
| Wasm validation fails | Verify instruction sequence with `wasm-tools print` |
| Arity mismatch | Update `:arity` in registration |
| Cross-package lookup | Use `:string-name` parameter |

## Next Steps

After completing migration:

1. Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
2. Generate Stage 1: `sbcl --load build/stage1-complete.lisp`
3. Verify byte-identical: Compare with pre-migration baseline
4. Update CLAUDE.md with completion status
