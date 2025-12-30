# Quickstart: Phase 16A - ANSI Character Functions

**Date**: 2025-12-31
**Feature**: 001-ansi-char-functions

## Prerequisites

- SBCL 2.4+ with Clysm loaded
- wasm-tools for validation
- Nix environment (optional, recommended)

```bash
# Enter development environment
nix develop

# Or manually ensure SBCL and wasm-tools are available
sbcl --version
wasm-tools --version
```

## Quick Test

After implementation, verify all functions compile and work:

```lisp
;; Load Clysm
(asdf:load-system :clysm)

;; Test character predicates
(clysm:compile-to-wasm '(graphic-char-p #\A))      ; → valid wasm
(clysm:compile-to-wasm '(standard-char-p #\a))    ; → valid wasm
(clysm:compile-to-wasm '(both-case-p #\Z))        ; → valid wasm

;; Test character name functions
(clysm:compile-to-wasm '(char-name #\Space))      ; → valid wasm
(clysm:compile-to-wasm '(name-char "Newline"))    ; → valid wasm

;; Test digit conversion
(clysm:compile-to-wasm '(digit-char 10 16))       ; → valid wasm

;; Test char-int
(clysm:compile-to-wasm '(char-int #\A))           ; → valid wasm
```

## Validation

```bash
# Run unit tests
sbcl --eval "(asdf:test-system :clysm)"

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
```

## Usage Examples

### Character Classification

```lisp
;; graphic-char-p: printable characters
(graphic-char-p #\A)       ; → T (printable)
(graphic-char-p #\Space)   ; → T (printable)
(graphic-char-p #\Newline) ; → NIL (control char)
(graphic-char-p #\Null)    ; → NIL (control char)

;; standard-char-p: 96 standard characters
(standard-char-p #\a)      ; → T (standard)
(standard-char-p #\!)      ; → T (semi-standard)
(standard-char-p #\あ)     ; → NIL (non-standard)

;; both-case-p: has upper/lower variants
(both-case-p #\A)          ; → T (has lowercase)
(both-case-p #\5)          ; → NIL (no case)
(both-case-p #\!)          ; → NIL (no case)
```

### Character Name Conversion

```lisp
;; char-name: character → name
(char-name #\Space)        ; → "Space"
(char-name #\Newline)      ; → "Newline"
(char-name #\A)            ; → NIL (no special name)

;; name-char: name → character (case-insensitive)
(name-char "Space")        ; → #\Space
(name-char "NEWLINE")      ; → #\Newline
(name-char "space")        ; → #\Space
(name-char "Invalid")      ; → NIL
```

### Digit Conversion

```lisp
;; digit-char: weight + radix → character
(digit-char 5)             ; → #\5 (radix 10 default)
(digit-char 10 16)         ; → #\A (hex)
(digit-char 15 16)         ; → #\F (hex)
(digit-char 35 36)         ; → #\Z (base 36)
(digit-char 10 10)         ; → NIL (weight >= radix)

;; char-int: character → integer
(char-int #\A)             ; → 65
(char-int #\Space)         ; → 32
```

## Development Workflow

1. **Write tests first** (TDD per Constitution VII):
   ```lisp
   ;; tests/unit/character-functions.lisp
   (deftest test-graphic-char-p
     (ok (graphic-char-p #\A))
     (ok (not (graphic-char-p #\Null))))
   ```

2. **Implement compile function**:
   ```lisp
   ;; src/clysm/compiler/codegen/func-section.lisp
   (defun compile-graphic-char-p (args env)
     ...)
   ```

3. **Register in builtins**:
   ```lisp
   ;; Add to *builtin-function-compilers*
   (graphic-char-p . compile-graphic-char-p)
   ```

4. **Validate**:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)"
   wasm-tools validate dist/clysm-stage1.wasm
   ```

## Related Functions

Existing character functions (already implemented):
- `char-upcase` - convert to uppercase
- `char-downcase` - convert to lowercase
- `alpha-char-p` - alphabetic predicate
- `digit-char-p` - digit predicate (returns weight)
- `char-code` - character to code point
- `code-char` - code point to character
