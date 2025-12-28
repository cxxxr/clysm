# Quickstart: ANSI Numeric Functions Implementation

**Branch**: `001-numeric-functions`
**Date**: 2025-12-28

## Prerequisites

1. Nix development environment:
   ```bash
   nix develop
   ```

2. Verify existing tests pass:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)"
   ```

## Implementation Order

### Phase 1: Basic Functions (No Dependencies)

Start with functions that have no external dependencies:

```lisp
;; Add to src/clysm/compiler/codegen/func-section.lisp
;; in compile-primitive-call case statement

;; 1. abs - uses conditional or f64.abs
(abs (compile-abs args env))

;; 2. signum - comparison chain
(signum (compile-signum args env))

;; 3. max/min - pairwise comparison
(max (compile-max args env))
(min (compile-min args env))

;; 4. gcd/lcm - Euclidean algorithm
(gcd (compile-gcd args env))
(lcm (compile-lcm args env))
```

### Phase 2: Bitwise Extensions

Add missing bitwise operations:

```lisp
;; Uses native Wasm instructions
(logcount (compile-logcount args env))      ; i32.popcnt
(integer-length (compile-integer-length args env))  ; 32 - i32.clz
```

### Phase 3: Complex Number Infrastructure

Before transcendental functions, implement complex number support:

```lisp
;; Complex type is already defined (index 17)
(complex (compile-complex args env))
(realpart (compile-realpart args env))
(imagpart (compile-imagpart args env))
(conjugate (compile-conjugate args env))
(phase (compile-phase args env))
```

### Phase 4: Math Host Shim

Create the host shim for transcendental functions:

```javascript
// host-shim/math-shim.js
export const mathImports = {
  math: {
    sin: Math.sin,
    cos: Math.cos,
    tan: Math.tan,
    asin: Math.asin,
    acos: Math.acos,
    atan: Math.atan,
    atan2: Math.atan2,
    sinh: Math.sinh,
    cosh: Math.cosh,
    tanh: Math.tanh,
    asinh: Math.asinh,
    acosh: Math.acosh,
    atanh: Math.atanh,
    exp: Math.exp,
    log: Math.log,
    pow: Math.pow,
    PI: Math.PI,
    E: Math.E
  }
};
```

### Phase 5: Trigonometric Functions

Register FFI imports and compile function calls:

```lisp
;; Register imports (in compiler initialization)
(register-math-import "sin" '(:f64) :f64)
(register-math-import "cos" '(:f64) :f64)
;; etc.

;; Compile calls
(sin (compile-trig-func 'sin args env))
(cos (compile-trig-func 'cos args env))
(tan (compile-trig-func 'tan args env))
```

### Phase 6: Remaining Functions

Implement remaining functions following the same patterns.

## Test-Driven Development

For each function, follow TDD:

```lisp
;; 1. Write test first (tests/unit/*)
(deftest test-abs-negative
  "abs of negative number"
  (ok (= 5 (clysm/tests:compile-and-run '(abs -5)))
      "(abs -5) should be 5"))

;; 2. Run test (should fail)
(asdf:test-system :clysm)

;; 3. Implement function

;; 4. Run test (should pass)
```

## Verification Checkpoints

After each phase, verify:

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Validate Wasm output
wasm-tools validate dist/output.wasm

# Check specific function
sbcl --load build/repl.lisp
;; Then in REPL:
(clysm:compile-and-run '(sin (/ pi 2)))
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add primitive cases |
| `src/clysm/ffi/import-gen.lisp` | Register math imports |
| `host-shim/math-shim.js` | Host math functions |
| `tests/unit/trig-functions-test.lisp` | Trigonometric tests |
| `tests/unit/math-functions-test.lisp` | Mathematical function tests |

## Success Criteria Verification

```lisp
;; Must pass these verification expressions:
(assert (approx= 1.0 (sin (/ pi 2))))
(assert (= 1024 (ash 1 10)))
(assert (= #x0F00 (logand #xFF00 #x0FF0)))
```

## Troubleshooting

### Import not found
Check that math-shim.js is loaded before Wasm module instantiation.

### NaN results
Verify input is in valid domain (e.g., asin requires |x| <= 1).

### Type errors
Ensure proper type coercion between i31ref, $float, and $complex.
