# Contract: eq Predicate

**Feature**: 024-equality-predicates

## Function Signature

```lisp
(eq x y) → generalized-boolean
```

## Expected Wasm Output

### Minimal Case: Symbol Comparison

Input:
```lisp
(eq 'a 'a)
```

Expected WAT (semantic):
```wat
(func $main (result anyref)
  (global.get $symbol-a)  ;; Interned symbol 'a'
  (global.get $symbol-a)  ;; Same interned symbol
  (ref.eq)                ;; Compare references
  (if (result anyref)
    (then (ref.i31 (i32.const 1)))  ;; T
    (else (global.get $nil))))       ;; NIL
```

Expected Result: `T`

### Negative Case: Different Symbols

Input:
```lisp
(eq 'a 'b)
```

Expected Result: `NIL`

### Identity Case: Same Object

Input:
```lisp
(let ((x (cons 1 2)))
  (eq x x))
```

Expected Result: `T`

### Non-Identity Case: Different Allocations

Input:
```lisp
(eq (cons 1 2) (cons 1 2))
```

Expected Result: `NIL` (different heap allocations)

## Validation Criteria

1. Wasm output MUST contain `ref.eq` instruction
2. Result MUST be `T` (i31ref with value 1) or `NIL` (symbol singleton)
3. Compilation MUST NOT generate function calls for simple eq
4. Module MUST pass `wasm-tools validate`

## Test Cases for Contract Verification

```lisp
;; Contract test: eq generates ref.eq
(deftest eq-generates-ref-eq
  (let ((wat (compile-to-wat '(eq 'a 'b))))
    (ok (search "ref.eq" wat))))

;; Contract test: eq returns valid boolean
(deftest eq-returns-boolean
  (let ((result (compile-and-run '(eq 'a 'a))))
    (ok (or (eq result t) (null result)))))
```
