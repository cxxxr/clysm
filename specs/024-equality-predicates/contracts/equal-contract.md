# Contract: equal Predicate

**Feature**: 024-equality-predicates

## Function Signature

```lisp
(equal x y) → generalized-boolean
```

## Expected Wasm Output

### List Comparison

Input:
```lisp
(equal '(1 2 3) '(1 2 3))
```

Expected WAT (semantic):
```wat
(func $equal (param $x anyref) (param $y anyref) (result anyref)
  ;; Check if both are cons
  (local.get $x)
  (ref.test (:ref $cons))
  (if (result anyref)
    (then
      (local.get $y)
      (ref.test (:ref $cons))
      (if (result anyref)
        (then
          ;; Both cons: compare car and cdr recursively
          (call $equal
            (struct.get $cons $car (ref.cast (:ref $cons) (local.get $x)))
            (struct.get $cons $car (ref.cast (:ref $cons) (local.get $y))))
          (global.get $nil)
          (ref.eq)
          (if (result anyref)
            (then (global.get $nil))  ;; car not equal
            (else
              ;; car equal, check cdr
              (call $equal
                (struct.get $cons $cdr (ref.cast (:ref $cons) (local.get $x)))
                (struct.get $cons $cdr (ref.cast (:ref $cons) (local.get $y)))))))
        (else (global.get $nil))))  ;; y not cons
    (else
      ;; x not cons: check string or fall back to eql
      ...)))
```

Expected Result: `T`

### String Comparison

Input:
```lisp
(equal "hello" "hello")
```

Expected WAT (semantic):
```wat
;; Delegate to string= for string comparison
(call $string= (local.get $x) (local.get $y))
```

Expected Result: `T`

### Nested Structure

Input:
```lisp
(equal '((1 2) (3 4)) '((1 2) (3 4)))
```

Expected Result: `T`

### Non-Equal Structures

Input:
```lisp
(equal '(1 2 3) '(1 2 4))
```

Expected Result: `NIL`

## Validation Criteria

1. Wasm output MUST define `$equal` as a callable function
2. Recursive calls MUST use `call $equal`
3. String comparison MUST delegate to `string=` (or equivalent)
4. Non-cons, non-string comparison MUST fall back to `eql`
5. Module MUST pass `wasm-tools validate`

## Test Cases for Contract Verification

```lisp
;; Contract test: equal is recursive function
(deftest equal-is-function
  (let ((wat (compile-to-wat '(equal '(1) '(1)))))
    (ok (search "call $equal" wat))))

;; Contract test: equal cons cells
(deftest equal-cons-recursive
  (ok (eql t (compile-and-run '(equal '(1 2 3) '(1 2 3))))))

;; Contract test: equal nested
(deftest equal-nested-cons
  (ok (eql t (compile-and-run '(equal '((a b) (c d)) '((a b) (c d)))))))

;; Contract test: equal strings
(deftest equal-strings
  (ok (eql t (compile-and-run '(equal "hello" "hello")))))

;; Contract test: equal falls back to eql
(deftest equal-fallback-eql
  (ok (eql t (compile-and-run '(equal 42 42)))))
```
