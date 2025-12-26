# Contract: eql Predicate

**Feature**: 024-equality-predicates

## Function Signature

```lisp
(eql x y) → generalized-boolean
```

## Expected Wasm Output

### Fixnum Comparison

Input:
```lisp
(eql 42 42)
```

Expected WAT (semantic):
```wat
(func $main (result anyref)
  (ref.i31 (i32.const 42))   ;; Fixnum 42
  (ref.i31 (i32.const 42))   ;; Fixnum 42
  ;; For i31ref, ref.eq compares values
  (ref.eq)
  (if (result anyref)
    (then (ref.i31 (i32.const 1)))
    (else (global.get $nil))))
```

Expected Result: `T`

### Cross-Type Comparison (Fixnum vs Float)

Input:
```lisp
(eql 1 1.0)
```

Expected WAT (semantic):
```wat
(func $main (result anyref)
  (ref.i31 (i32.const 1))     ;; Fixnum 1
  (call $box-float (f64.const 1.0))  ;; Float 1.0
  ;; Type dispatch: first is i31, second is struct
  (ref.test :i31 ...)         ;; x is fixnum
  (ref.test (:ref $float) ...)  ;; y is float (different type!)
  ;; Different types → NIL
  (global.get $nil))
```

Expected Result: `NIL`

### Float Comparison

Input:
```lisp
(eql 3.14 3.14)
```

Expected WAT (semantic):
```wat
(func $main (result anyref)
  (call $box-float (f64.const 3.14))
  (call $box-float (f64.const 3.14))
  ;; Type dispatch: both are floats
  (ref.test (:ref $float) ...)
  ;; Extract f64 values and compare
  (struct.get $float $value ...)
  (struct.get $float $value ...)
  (f64.eq)
  (if (result anyref)
    (then (ref.i31 (i32.const 1)))
    (else (global.get $nil))))
```

Expected Result: `T`

### Character Comparison

Input:
```lisp
(eql #\a #\a)
```

Expected Result: `T`

Input:
```lisp
(eql #\a #\A)
```

Expected Result: `NIL` (case-sensitive)

## Validation Criteria

1. Wasm output MUST contain `ref.test` for type dispatch
2. Wasm output MUST contain `f64.eq` for float comparison
3. Different numeric types MUST return NIL (not value comparison)
4. Same-type numbers MUST compare by value
5. Module MUST pass `wasm-tools validate`

## Test Cases for Contract Verification

```lisp
;; Contract test: eql type-dispatch
(deftest eql-uses-type-dispatch
  (let ((wat (compile-to-wat '(eql 1 1.0))))
    (ok (search "ref.test" wat))))

;; Contract test: eql cross-type returns nil
(deftest eql-cross-type-nil
  (ok (null (compile-and-run '(eql 1 1.0)))))

;; Contract test: eql same-type fixnum
(deftest eql-same-type-fixnum
  (ok (eql t (compile-and-run '(eql 42 42)))))

;; Contract test: eql same-type float
(deftest eql-same-type-float
  (ok (eql t (compile-and-run '(eql 3.14 3.14)))))
```
