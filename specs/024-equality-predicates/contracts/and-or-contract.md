# Contract: and/or Special Forms

**Feature**: 024-equality-predicates

## Syntax

```lisp
(and {form}*)  → result
(or {form}*)   → result
```

## Expected Compilation

### and with Short-Circuit

Input:
```lisp
(and t nil (error "should not reach"))
```

Expected AST transformation:
```lisp
;; (and a b c) → (if a (if b c nil) nil)
(if t
    (if nil
        (error "should not reach")
        nil)
    nil)
```

Expected WAT (semantic):
```wat
(func $main (result anyref)
  ;; Evaluate first form: t
  (ref.i31 (i32.const 1))  ;; T
  (global.get $nil)
  (ref.eq)
  (if (result anyref)
    (then (global.get $nil))  ;; First form was NIL, return NIL
    (else
      ;; Evaluate second form: nil
      (global.get $nil)
      (global.get $nil)
      (ref.eq)
      (if (result anyref)
        (then (global.get $nil))  ;; Second form was NIL, return NIL
        (else
          ;; Evaluate third form (not reached in this case)
          (call $error ...))))))
```

Expected Result: `NIL` (short-circuits at second form)

### or with Short-Circuit

Input:
```lisp
(or nil 42 (error "should not reach"))
```

Expected AST transformation:
```lisp
;; (or a b c) → (let ((#g a)) (if #g #g (let ((#h b)) (if #h #h c))))
(let ((#g1 nil))
  (if #g1
      #g1
      (let ((#g2 42))
        (if #g2
            #g2
            (error "should not reach")))))
```

Expected Result: `42` (returns first non-NIL value)

### Empty Forms

Input:
```lisp
(and)
```

Expected Result: `T`

Input:
```lisp
(or)
```

Expected Result: `NIL`

## Validation Criteria

1. Wasm output MUST NOT evaluate forms after short-circuit point
2. `(and)` MUST return T
3. `(or)` MUST return NIL
4. `and` MUST return the value of the last form if all are non-NIL
5. `or` MUST return the value of the first non-NIL form
6. Module MUST pass `wasm-tools validate`

## Test Cases for Contract Verification

```lisp
;; Contract test: and short-circuits
(deftest and-short-circuits
  (let ((counter 0))
    (compile-and-run
      '(and nil (progn (setq *test-counter* 1) t)))
    (ok (= counter 0))))  ;; Counter unchanged = short-circuited

;; Contract test: or short-circuits
(deftest or-short-circuits
  (ok (= 5 (compile-and-run '(or nil 5 (error "boom"))))))

;; Contract test: and returns last value
(deftest and-returns-last
  (ok (= 3 (compile-and-run '(and 1 2 3)))))

;; Contract test: or returns first non-nil
(deftest or-returns-first-non-nil
  (ok (= 2 (compile-and-run '(or nil 2 3)))))

;; Contract test: empty and
(deftest and-empty
  (ok (eql t (compile-and-run '(and)))))

;; Contract test: empty or
(deftest or-empty
  (ok (null (compile-and-run '(or)))))
```
