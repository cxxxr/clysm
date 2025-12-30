# Quickstart: Character Literal Compilation Support

**Branch**: `001-char-literal-compile` | **Date**: 2025-12-31

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify tools available
sbcl --version      # SBCL 2.4+
wasm-tools --version # For Wasm validation
```

## Testing the Feature

### 1. Run Unit Tests

```bash
# Run character literal compilation tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# Or run specific test file (after implementation)
sbcl --load tests/unit/char-literal-test.lisp --quit
```

### 2. Manual REPL Verification

```lisp
;; Start SBCL and load clysm
(ql:quickload :clysm)

;; Test compile-quoted-element directly
(clysm/compiler/codegen/func-section::compile-quoted-element #\Space)
;; Expected: ((:I32.CONST 32) :REF.I31)

(clysm/compiler/codegen/func-section::compile-quoted-element #\Tab)
;; Expected: ((:I32.CONST 9) :REF.I31)

(clysm/compiler/codegen/func-section::compile-quoted-element #\a)
;; Expected: ((:I32.CONST 97) :REF.I31)
```

### 3. Compile a Test Form

```lisp
;; Compile a form with character literals
(clysm:compile-to-wasm
  '(defun whitespace-p (c)
     (member c '(#\Space #\Tab #\Newline))))

;; Should complete without errors
```

### 4. Validate Generated Wasm

```bash
# After generating Wasm output
wasm-tools validate dist/test-output.wasm
# Should exit with code 0
```

### 5. Measure Compilation Rate Improvement

```bash
# Generate Stage 1 and check compilation statistics
sbcl --load build/stage1-complete.lisp

# Check dist/stage1-report.json for compilation rate
# Target: 14% → 20%+
```

## Expected Outputs

| Test | Expected Result |
|------|-----------------|
| `compile-quoted-element #\Space` | `((:I32.CONST 32) :REF.I31)` |
| `compile-quoted-element #\Tab` | `((:I32.CONST 9) :REF.I31)` |
| `compile-quoted-element #\Newline` | `((:I32.CONST 10) :REF.I31)` |
| `compile-quoted-element #\Return` | `((:I32.CONST 13) :REF.I31)` |
| `compile-quoted-element #\a` | `((:I32.CONST 97) :REF.I31)` |
| Form with `'(#\Space #\Tab)` | Compiles without error |
| wasm-tools validate | Exit code 0 |

## Troubleshooting

### "Cannot compile quoted element: #\Space"

The characterp branch has not been added to `compile-quoted-element`. Check `src/clysm/compiler/codegen/func-section.lisp` line ~520.

### Wasm validation fails

Check that the generated instructions are:
1. `i32.const <number>` (not negative for characters)
2. `ref.i31` immediately follows

### Compilation rate unchanged

Ensure Stage 1 generation is running against the updated compiler. Check that character-heavy functions are being compiled:
- Look for functions using `member` with character lists
- Check `standard-char-p`, `whitespace-char-p` type functions
