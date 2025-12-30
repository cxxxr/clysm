# Quickstart: Arithmetic Primitives 1- and 1+

**Feature**: 001-arithmetic-primitives

## Usage

After implementation, the `1-` and `1+` primitives will be available in Clysm code:

```lisp
;; Decrement by 1
(1- 5)      ; => 4
(1- n)      ; => (- n 1)

;; Increment by 1
(1+ 5)      ; => 6
(1+ n)      ; => (+ n 1)

;; Common use case: factorial
(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (1- n)))))
```

## Testing

### Run Unit Tests

```bash
nix develop
sbcl --eval "(asdf:test-system :clysm)"
```

### Manual Verification

```bash
# Compile a test function
sbcl --load build/test-arithmetic-primitives.lisp

# Validate generated Wasm
wasm-tools validate dist/test-arithmetic.wasm
```

### Compilation Rate Check

```bash
# Generate Stage 1 and check compilation statistics
sbcl --load build/stage1-complete.lisp --verbose

# Check that 1- and 1+ are no longer blockers
cat dist/stage1-report.json | grep -E '"1-"|"1\+"'
```

## Expected Outcomes

1. `(1- n)` compiles to Wasm instructions: `arg | ref.cast i31 | i31.get_s | i32.const 1 | i32.sub | ref.i31`
2. `(1+ n)` compiles to Wasm instructions: `arg | ref.cast i31 | i31.get_s | i32.const 1 | i32.add | ref.i31`
3. Factorial function compiles successfully
4. All generated Wasm passes `wasm-tools validate`
