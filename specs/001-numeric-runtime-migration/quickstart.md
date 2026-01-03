# Quickstart: Numeric Runtime Migration

**Feature**: 001-numeric-runtime-migration
**Date**: 2026-01-04

## Prerequisites

- SBCL 2.4+
- Nix development environment (`nix develop`)
- wasm-tools for validation

## Quick Verification

After implementation, verify the feature works:

```bash
# Run unit tests
sbcl --eval "(asdf:test-system :clysm)"

# Generate Stage 1 and validate
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm
```

## Usage Examples

### parse-integer
```lisp
;; Basic parsing
(parse-integer "123")           ; → 123, 3

;; With radix
(parse-integer "FF" :radix 16)  ; → 255, 2
(parse-integer "1010" :radix 2) ; → 10, 4

;; With bounds
(parse-integer "abc123def" :start 3 :end 6) ; → 123, 6

;; With junk-allowed
(parse-integer "42xyz" :junk-allowed t) ; → 42, 2
```

### write-to-string
```lisp
;; Default base 10
(write-to-string 255)           ; → "255"

;; Hexadecimal
(write-to-string 255 :base 16)  ; → "FF"

;; Binary
(write-to-string 10 :base 2)    ; → "1010"

;; Negative numbers
(write-to-string -42)           ; → "-42"
```

### rationalize
```lisp
;; Integers unchanged
(rationalize 5)                 ; → 5

;; Float to ratio
(rationalize 0.5)               ; → 1/2
(rationalize 0.333)             ; → 1/3 (approximately)

;; Already rational
(rationalize 2/3)               ; → 2/3
```

### signum
```lisp
;; Integer sign
(signum 42)                     ; → 1
(signum -5)                     ; → -1
(signum 0)                      ; → 0

;; Float sign (type-congruent)
(signum 3.14)                   ; → 1.0
(signum -2.5)                   ; → -1.0

;; Complex (unit vector)
(signum #C(3 4))                ; → #C(0.6 0.8)
```

### phase
```lisp
;; Real numbers
(phase 1)                       ; → 0.0
(phase -1)                      ; → 3.141592653589793

;; Complex numbers
(phase #C(0 1))                 ; → 1.5707963267948966 (π/2)
(phase #C(1 1))                 ; → 0.7853981633974483 (π/4)
(phase #C(-1 0))                ; → 3.141592653589793 (π)
```

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/lib/numeric-runtime.lisp` | Runtime library implementation |
| `src/clysm/compiler/codegen/func-section.lisp` | Registration function |
| `tests/unit/numeric-runtime-test.lisp` | Unit tests |

## Testing Individual Functions

```lisp
;; Load the system
(asdf:load-system :clysm)

;; Test specific functions
(parse-integer "123")
(write-to-string 255 :base 16)
(rationalize 0.5)
(signum -3.14)
(phase #C(1 1))
```

## Troubleshooting

### Wasm validation fails
1. Check `dist/stage1-report.json` for compilation errors
2. Verify all runtime functions are registered
3. Run `wasm-tools validate --verbose dist/clysm-stage1.wasm`

### Tests fail
1. Run tests individually to isolate failures
2. Check HyperSpec for expected ANSI CL behavior
3. Verify Layer 1 primitive availability
