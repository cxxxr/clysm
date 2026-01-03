# Quickstart: String Runtime Migration

**Feature**: 001-string-runtime-migration
**Date**: 2026-01-03

## Prerequisites

1. SBCL 2.4+ installed
2. Nix flake environment (`nix develop`)
3. wasm-tools available
4. Project dependencies loaded

## Quick Verification

```bash
# Enter development environment
nix develop

# Run all tests (before changes)
sbcl --eval "(asdf:test-system :clysm)"

# Verify Stage 1 compilation works
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm
```

## Development Workflow

### 1. Create Runtime Library

Create `src/clysm/lib/string-runtime.lisp`:

```lisp
;;;; string-runtime.lisp - Runtime library string functions
(in-package #:clysm)

;; Start with simplest function
(defun string-char-rt (string index)
  "Return character at INDEX in STRING."
  ;; Implementation here
  )
```

### 2. Register Functions

Add to `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
;; Near other runtime function registrations
(register-runtime-function 'char :$string-char-rt 2)
(register-runtime-function 'schar :$string-char-rt 2)
```

### 3. Write Tests First (TDD)

Create `tests/unit/string-runtime-test.lisp`:

```lisp
(defpackage #:clysm/tests/string-runtime
  (:use #:cl #:rove))

(in-package #:clysm/tests/string-runtime)

(deftest string-char-rt-basic
  (ok (char= (clysm::string-char-rt "hello" 0) #\h))
  (ok (char= (clysm::string-char-rt "hello" 4) #\o)))
```

### 4. Run Tests

```bash
# Run specific test file
sbcl --eval "(asdf:test-system :clysm)" \
     --eval "(rove:run :clysm/tests/string-runtime)"
```

### 5. Remove Old Codegen

After all tests pass, remove from `func-section.lisp`:
- `compile-string-char` (lines 12954-13148)
- `compile-string-trim` (lines 14581-14715)
- `compile-string-capitalize` (lines 14131-14250)
- `compile-string-compare-ci` (lines 13434-13647)
- `compile-nstring-capitalize` (lines 14860-14979)

### 6. Final Verification

```bash
# Full test suite
sbcl --eval "(asdf:test-system :clysm)"

# Stage 1 compilation
sbcl --load build/stage1-complete.lisp

# Wasm validation
wasm-tools validate dist/clysm-stage1.wasm

# Check line count reduction
wc -l src/clysm/compiler/codegen/func-section.lisp
# Should be ~700 lines less than before
```

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/lib/string-runtime.lisp` | NEW: Runtime implementations |
| `src/clysm/compiler/codegen/func-section.lisp` | MODIFY: Remove compile-* functions |
| `tests/unit/string-runtime-test.lisp` | NEW: Unit tests |

## Reference Material

- Pattern example: `src/clysm/lib/sequence-runtime.lisp`
- Dispatch mechanism: `func-section.lisp:69-110`
- ANSI specs: `resources/HyperSpec/Body/f_*.htm`

## Common Issues

### "Unknown runtime function"
Ensure `register-runtime-function` is called before compilation.

### Test failures after removal
Check that all callers use the registered function symbol, not compile-* directly.

### Wasm validation errors
Run `wasm-tools print dist/clysm-stage1.wasm | head -100` to inspect output.

## Success Criteria

- [x] All existing tests pass
- [x] Stage 1 compiles successfully (21,263 bytes)
- [x] Wasm validates
- [ ] func-section.lisp reduced by ~700 lines (deferred: inline codegen removal pending)
- [x] string-runtime.lisp created (~300-400 lines) - 361 lines
