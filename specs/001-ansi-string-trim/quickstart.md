# Quickstart: ANSI String Trim Functions (Phase 16B)

**Feature**: `001-ansi-string-trim`
**Date**: 2025-12-31

## Prerequisites

1. SBCL 2.4+ with Clysm loaded
2. wasm-tools installed (`nix develop` provides this)
3. Existing string infrastructure from Phase 16A

## Development Setup

```bash
# Enter development shell
nix develop

# Load Clysm
sbcl --load build/bootstrap.lisp

# Or in REPL
(asdf:load-system :clysm)
```

## Implementation Locations

### Primary File
`src/clysm/compiler/codegen/func-section.lisp`

Add new functions after existing string functions (~line 15400):
- `compile-string-trim`
- `compile-string-left-trim`
- `compile-string-right-trim`
- `compile-nstring-upcase`
- `compile-nstring-downcase`
- `compile-nstring-capitalize`

### Dispatcher Registration
Add to `compile-call` function (~line 1132):
```lisp
((member fn '(string-trim string-left-trim string-right-trim))
 (compile-string-trim form env))
((member fn '(nstring-upcase nstring-downcase nstring-capitalize))
 (compile-nstring-case form env))
```

### Test Files
- `tests/unit/string-trim-test.lisp` (NEW)
- `tests/integration/string-test.lisp` (EXTEND)

## TDD Workflow

### Step 1: Write Failing Test
```lisp
;; tests/unit/string-trim-test.lisp
(deftest test-string-trim-basic
  "Verify (string-trim \" \" \" test \") returns \"test\""
  (let ((result (clysm/tests:compile-and-run
                  '(string-trim " " " test "))))
    (ok (string= result "test")
        "string-trim should remove leading and trailing spaces")))
```

### Step 2: Run Test (Expect Failure)
```bash
sbcl --eval "(asdf:test-system :clysm)"
```

### Step 3: Implement Function
Follow existing patterns from `compile-string-upcase`.

### Step 4: Run Test (Expect Pass)
```bash
sbcl --eval "(asdf:test-system :clysm)"
```

### Step 5: Validate Wasm Output
```bash
sbcl --eval "(clysm:compile-to-wasm '(string-trim \" \" \" test \") :output \"test.wasm\")"
wasm-tools validate test.wasm
```

## Key Patterns to Copy

### Keyword Argument Parsing (from compile-make-string)
```lisp
(let* ((args (cdr form))
       (string-arg (first args))
       (keyword-args (rest args))
       (start-value (getf keyword-args :start 0))
       (end-value (getf keyword-args :end nil)))
  ...)
```

### String Loop Pattern (from compile-string-upcase)
```lisp
;; Generate loop over string bytes
(emit-block :$break
  (emit-loop :$continue
    ;; Read byte
    (emit-array-get-u :$string (local :$str) (local :$idx))
    ;; Process byte
    ...
    ;; Increment and continue
    (emit-i32-add (local :$idx) (emit-i32-const 1))
    (emit-local-set :$idx)
    (emit-br-if :$continue
      (emit-i32-lt-u (local :$idx) (local :$end)))))
```

### In-Place Modification (for nstring functions)
```lisp
;; Modify string in-place
(emit-array-set :$string (local :$str) (local :$idx) (local :$new-byte))
;; Return same string
(local :$str)
```

## Verification Checklist

- [ ] `(string-trim " " " test ")` → `"test"`
- [ ] `(string-left-trim " " " test ")` → `"test "`
- [ ] `(string-right-trim " " " test ")` → `" test"`
- [ ] `(nstring-upcase (copy-seq "hello"))` → `"HELLO"` (same object)
- [ ] `(nstring-downcase (copy-seq "HELLO"))` → `"hello"` (same object)
- [ ] `(nstring-capitalize (copy-seq "hello world"))` → `"Hello World"` (same object)
- [ ] All functions compile to valid WasmGC
- [ ] Strings category test suite ≥ 70% pass rate

## Common Issues

### Issue: Character bag not recognized
**Solution**: Ensure character bag is coerced to sequence before iteration. Use `ref.test` to check type (string, cons, vector).

### Issue: :end nil handling
**Solution**: When :end is nil (default), use string length. Check with `ref.eq $nil` before using.

### Issue: Off-by-one errors in trim
**Solution**: Remember `:end` is exclusive. `(subseq str start end)` extracts `[start, end)`.

### Issue: nstring returns different object
**Solution**: Ensure the original string reference is returned, not a copy. Store in local at start, return same local.

## HyperSpec References

- [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)
- [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm)
- [string](resources/HyperSpec/Body/t_string.htm) (type)
