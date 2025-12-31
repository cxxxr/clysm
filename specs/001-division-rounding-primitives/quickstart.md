# Quickstart: Division/Rounding Function Primitives

**Created**: 2025-12-31

## Overview

This feature adds compilation support for ANSI CL rounding functions: [floor](resources/HyperSpec/Body/f_floorc.htm), [ceiling](resources/HyperSpec/Body/f_floorc.htm), [round](resources/HyperSpec/Body/f_floorc.htm), and their float-result variants (ffloor, fceiling, fround).

## Prerequisites

- SBCL 2.4+ with Clysm loaded
- Nix development environment (`nix develop`)
- Existing familiarity with `func-section.lisp` code generation patterns

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Main implementation (new compile-* functions) |
| `src/clysm/compiler/ast.lisp` | AST parsing (already handles floor/ceiling/round) |
| `src/clysm/compiler/analyzer/free-vars.lisp` | Primitive operators list |
| `tests/unit/rounding-functions.lisp` | Unit tests |
| `tests/contract/rounding-wasm.lisp` | Wasm validation tests |

## Implementation Pattern

All rounding functions follow this pattern (see `compile-truncate` as reference):

```lisp
(defun compile-floor (args env)
  "Compile (floor number &optional divisor)."
  (let ((mv-count-global 2)
        (mv-buffer-global 3)
        (mv-array-type 20))
    ;; 1. Set mv-count = 2
    ;; 2. Compile dividend and divisor
    ;; 3. Compute quotient using f64.floor
    ;; 4. Compute remainder = dividend - quotient * divisor
    ;; 5. Store remainder in mv-buffer[0]
    ;; 6. Return quotient on stack
    ...))
```

## Wasm Instructions Used

| CL Function | Wasm Instruction | Opcode |
|-------------|-----------------|--------|
| floor, ffloor | f64.floor | 0x9B |
| ceiling, fceiling | f64.ceil | 0x9C |
| round, fround | f64.nearest | 0x9E |

## Testing

### Run Unit Tests

```bash
sbcl --eval "(asdf:test-system :clysm/tests)"
```

### Validate Wasm Output

```bash
wasm-tools validate dist/clysm-stage1.wasm
```

### Test Individual Function

```lisp
;; In REPL
(clysm:compile-to-wasm '(floor 7 2))
;; Should return valid Wasm bytecode
```

## Common Issues

### Issue: Remainder has wrong sign

**Cause**: Using wrong rounding direction
**Solution**: Ensure floor rounds toward -∞, ceiling toward +∞

### Issue: Single-argument form fails

**Cause**: Missing divisor handling
**Solution**: Check `(length args)` and use `(:i32.const 1)` when divisor missing

### Issue: Float result not boxed

**Cause**: Missing $float struct creation for ffloor/fceiling/fround
**Solution**: Use `(struct.new $float)` to box f64 result

## ANSI CL Reference

All functions are defined in the HyperSpec:
- [floor, ceiling, truncate, round](resources/HyperSpec/Body/f_floorc.htm)

Key semantics:
- Returns 2 values: quotient and remainder
- Invariant: `number = quotient * divisor + remainder`
- `round` uses banker's rounding (ties to even)
