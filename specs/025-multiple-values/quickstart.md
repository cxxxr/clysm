# Quickstart: Multiple Values Support

**Feature**: 025-multiple-values
**Date**: 2025-12-26

## Overview

This feature adds ANSI Common Lisp multiple values support to the clysm compiler. Multiple values allow a function to return more than one value, with the caller able to receive all or a subset of them.

## Key Components

### 1. Runtime Globals

Two new Wasm globals are added:

```lisp
;; In src/clysm/runtime/multi-value.lisp or objects.lisp
(defun make-mv-count-global ()
  (make-wasm-global
   :name '$mv-count
   :type :i32
   :mutability :var
   :init-expr '((:i32.const 1))))

(defun make-mv-buffer-global ()
  (make-wasm-global
   :name '$mv-buffer
   :type '(:ref-mut $mv-array)
   :mutability :var
   :init-expr '((:array.new_default <mv-array-type-idx> (:i32.const 20)))))
```

### 2. Values Form

Compile `(values v1 v2 v3 ...)`:

```lisp
(defun compile-values (args env)
  (cond
    ;; (values) -> return NIL, set count to 0
    ((null args)
     `((:i32.const 0)
       (:global.set ,*mv-count-global-index*)
       (:global.get ,*nil-global-index*)))

    ;; (values v1) -> return v1, set count to 1
    ((null (cdr args))
     `(,@(compile-expr (car args) env)
       (:i32.const 1)
       (:global.set ,*mv-count-global-index*)))

    ;; (values v1 v2 ...) -> store secondary values, return primary
    (t
     (let ((count (length args))
           (instrs '()))
       ;; Compile and store secondary values in buffer
       (loop for (v . rest) on (cdr args)
             for i from 0
             do (push `((:global.get ,*mv-buffer-global-index*)
                        (:i32.const ,i)
                        ,@(compile-expr v env)
                        (:array.set <mv-array-type-idx>))
                      instrs))
       ;; Set count
       (push `((:i32.const ,count)
               (:global.set ,*mv-count-global-index*))
             instrs)
       ;; Compile primary value last (left on stack)
       `(,@(compile-expr (car args) env)
         ,@(apply #'append (nreverse instrs)))))))
```

### 3. Multiple-Value-Bind

Compile `(multiple-value-bind (vars...) form body...)`:

```lisp
(defun compile-multiple-value-bind (form env)
  (destructuring-bind (vars value-form &rest body) (cdr form)
    (let ((primary-local (gensym "mv-primary")))
      ;; Compile value form, bind primary
      `(,@(compile-expr value-form env)
        (:local.set ,primary-local)

        ;; For each variable, bind from buffer or NIL
        ,@(loop for var in vars
                for i from 0
                collect (if (zerop i)
                           `(:local.get ,primary-local)
                           `((:global.get ,*mv-count-global-index*)
                             (:i32.const ,i)
                             (:i32.gt_u)
                             (:if (:result anyref)
                               (:then
                                 (:global.get ,*mv-buffer-global-index*)
                                 (:i32.const ,(1- i))
                                 (:array.get <mv-array-type-idx>))
                               (:else
                                 (:global.get ,*nil-global-index*))))))

        ;; Compile body with extended environment
        ,@(compile-body body (extend-env vars env))))))
```

### 4. Updating floor/truncate/ceiling/round

Each function now stores remainder as second value:

```lisp
(defun compile-floor (args env)
  (destructuring-bind (dividend &optional (divisor 1)) args
    `(,@(compile-expr dividend env)
      ,@(compile-expr divisor env)
      ;; Compute quotient and remainder
      (:local.tee $dividend)
      (:local.tee $divisor)
      (:i32.div_s)         ;; quotient on stack
      (:local.tee $quotient)

      ;; Compute remainder: dividend - quotient * divisor
      (:local.get $dividend)
      (:local.get $quotient)
      (:local.get $divisor)
      (:i32.mul)
      (:i32.sub)

      ;; Store remainder in buffer[0]
      (:global.get ,*mv-buffer-global-index*)
      (:i32.const 0)
      (:ref.i31)           ;; wrap as i31ref
      (:array.set <mv-array-type-idx>)

      ;; Set count to 2
      (:i32.const 2)
      (:global.set ,*mv-count-global-index*)

      ;; Return quotient as i31ref
      (:local.get $quotient)
      (:ref.i31))))
```

## Test Examples

```lisp
;; Unit test for values
(deftest test-values-basic
  (ok (= 1 (compile-and-run '(values 1 2 3))))
  (ok (= 3 (compile-and-run '(multiple-value-bind (a b c) (values 1 2 3) c)))))

;; Unit test for floor
(deftest test-floor-mv
  (ok (equal '(2 1) (compile-and-run '(multiple-value-list (floor 7 3))))))

;; Contract test
(deftest test-mv-globals-exist
  (let ((wasm (compile-to-wasm '(values 1))))
    (ok (find-global wasm '$mv-count))
    (ok (find-global wasm '$mv-buffer))))
```

## Implementation Order

1. **Add globals**: mv-count and mv-buffer in objects.lisp/multi-value.lisp
2. **Update compiler.lisp**: Include new globals in module initialization
3. **Implement values**: compile-values in func-section.lisp
4. **Implement multiple-value-bind**: As special form or macro
5. **Update floor/truncate/ceiling/round**: Add second value
6. **Implement remaining forms**: multiple-value-list, nth-value, values-list, multiple-value-prog1, multiple-value-call

## Verification Commands

```bash
# Run unit tests
nix develop -c rove tests/unit/multiple-values-test.lisp

# Validate Wasm output
nix develop -c wasm-tools validate output.wasm

# Run integration tests
nix develop -c rove tests/integration/mv-ansi-test.lisp
```
