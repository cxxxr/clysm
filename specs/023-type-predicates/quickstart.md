# Quickstart: Implementing Type Predicates

**Feature**: 023-type-predicates
**Date**: 2025-12-26

## Prerequisites

1. Nix development environment active:
   ```bash
   nix develop
   ```

2. Verify existing tests pass:
   ```bash
   nix flake check
   ```

3. Understand existing predicates in `func-section.lisp`:
   - `compile-consp` (lines ~1261)
   - `compile-null` (lines ~1282)
   - `compile-listp` (lines ~1322)

## Implementation Steps

### Step 1: Add Test First (TDD)

Create or update test file `tests/unit/type-predicates-test.lisp`:

```lisp
(defpackage #:clysm/tests/unit/type-predicates
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

(in-package #:clysm/tests/unit/type-predicates)

(deftest integerp-fixnum
  (ok (eql 1 (compile-and-run '(integerp 42)))
      "integerp should return T for fixnum"))

(deftest integerp-float
  (ok (null (compile-and-run '(integerp 3.14)))
      "integerp should return NIL for float"))
```

### Step 2: Run Test (Should Fail)

```bash
sbcl --eval '(ql:quickload :clysm)' \
     --eval '(ql:quickload :rove)' \
     --eval '(rove:run :clysm/tests/unit/type-predicates)' \
     --quit
```

Expected: Test fails with "Unknown function INTEGERP"

### Step 3: Implement Predicate

In `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
(defun compile-integerp (args env)
  "Compile (integerp x) - returns T if x is an integer, NIL otherwise."
  (when (/= (length args) 1)
    (error "integerp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "INTEGERP-TMP"))))
    (append
     ;; Compile argument and save to local
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Is fixnum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,+type-bignum+))
       (:if (:result :anyref))
       ;; Is bignum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Neither -> NIL
       (:ref.null :none)
       :end
       :end))))
```

### Step 4: Register in Dispatcher

In `compile-primitive-call` function (around line 700):

```lisp
(case op
  ;; ... existing cases ...
  (integerp (compile-integerp args env))
  ;; ... more cases ...
  )
```

### Step 5: Run Test (Should Pass)

```bash
sbcl --eval '(ql:quickload :clysm)' \
     --eval '(ql:quickload :rove)' \
     --eval '(rove:run :clysm/tests/unit/type-predicates)' \
     --quit
```

### Step 6: Verify Wasm Output

```bash
# Compile to Wasm
sbcl --eval '(ql:quickload :clysm)' \
     --eval '(clysm/compiler:compile-to-wasm '\''(integerp 42) :output "/tmp/test.wasm")' \
     --quit

# Validate
wasm-tools validate /tmp/test.wasm

# Execute
wasmtime --wasm-features=gc,exceptions,tail-call /tmp/test.wasm
```

## Common Patterns

### Simple Type Check (single type)

```lisp
(defun compile-floatp (args env)
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,+type-float+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))
```

### Union Type Check (multiple types)

```lisp
(defun compile-numberp (args env)
  (let ((temp (env-add-local env (gensym "NUMBERP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp))
     ;; Check fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Check bignum
       (:local.get ,temp)
       (:ref.test (:ref ,+type-bignum+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Check ratio
       (:local.get ,temp)
       (:ref.test (:ref ,+type-ratio+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Check float
       (:local.get ,temp)
       (:ref.test (:ref ,+type-float+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Check complex
       (:local.get ,temp)
       (:ref.test (:ref ,+type-complex+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end :end :end :end :end))))
```

### Numeric Comparison (value extraction)

```lisp
(defun compile-zerop (args env)
  (let ((temp (env-add-local env (gensym "ZEROP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp))
     ;; Check fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare
       (:local.get ,temp)
       (:ref.cast :i31)
       (:i31.get_s)
       (:i32.eqz)
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Check float
       (:local.get ,temp)
       (:ref.test (:ref ,+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare
       (:local.get ,temp)
       (:ref.cast (:ref ,+type-float+))
       (:struct.get ,+type-float+ 0)
       (:f64.const 0.0)
       (:f64.eq)
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))
```

## Debugging Tips

1. **Wasm validation errors**: Use `wasm-tools print` to see generated WAT
2. **Type mismatch**: Check `ref.cast` matches `ref.test` type
3. **Stack issues**: Ensure each branch produces exactly one `anyref`
4. **Local index**: Use `env-add-local` for temporary storage

## Checklist per Predicate

- [ ] Write unit test (positive + negative cases)
- [ ] Run test, verify failure
- [ ] Implement `compile-<predicate>` function
- [ ] Add to `compile-primitive-call` dispatcher
- [ ] Run test, verify pass
- [ ] Validate Wasm output with `wasm-tools`
- [ ] Test in wasmtime
- [ ] Run `nix flake check`
