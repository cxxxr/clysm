# Quickstart: Implementing Equality Predicates

**Feature**: 024-equality-predicates
**Date**: 2025-12-26

## Prerequisites

Ensure you have completed:
- Feature 007-sequence-functions (cons cell implementation)
- Feature 008-character-string (`char=`, `char-equal`, `string=`, `string-equal`)
- Feature 010-numeric-tower (numeric `=` comparator)
- Feature 023-type-predicates (type predicate patterns)

## Implementation Order

Follow TDD methodology for each step.

### Step 1: Implement `not` (Simplest)

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

**Test First**:
```lisp
;; tests/unit/equality-predicates-test.lisp
(deftest not-nil
  (ok (eql t (compile-and-run '(not nil)))))

(deftest not-t
  (ok (null (compile-and-run '(not t)))))

(deftest not-non-nil
  (ok (null (compile-and-run '(not 42)))))
```

**Implementation**:
```lisp
(defun compile-not (args env)
  "Compile (not x) - returns T if x is NIL, NIL otherwise"
  (let ((arg (first args)))
    `(,@(compile-to-instructions arg env)
      (global.get ,$nil)
      (ref.eq)
      (if (result anyref)
        (then (ref.i31 (i32.const 1)))   ;; T
        (else (global.get ,$nil))))))     ;; NIL
```

**Add to dispatcher**:
```lisp
(defun compile-primitive-call (op args env)
  (case op
    ...
    (not (compile-not args env))
    ...))
```

### Step 2: Implement `eq` (Foundation)

**Test First**:
```lisp
(deftest eq-same-symbol
  (ok (eql t (compile-and-run '(eq 'a 'a)))))

(deftest eq-different-symbols
  (ok (null (compile-and-run '(eq 'a 'b)))))

(deftest eq-same-fixnum
  (ok (eql t (compile-and-run '(eq 42 42)))))

(deftest eq-different-cons
  (ok (null (compile-and-run '(eq (cons 1 2) (cons 1 2))))))
```

**Implementation**:
```lisp
(defun compile-eq (args env)
  "Compile (eq x y) - reference identity comparison"
  (let ((x (first args))
        (y (second args)))
    `(,@(compile-to-instructions x env)
      ,@(compile-to-instructions y env)
      (ref.eq)
      (if (result anyref)
        (then (ref.i31 (i32.const 1)))
        (else (global.get ,$nil))))))
```

### Step 3: Implement `eql` (Type-Aware)

**Test First**:
```lisp
(deftest eql-same-fixnum
  (ok (eql t (compile-and-run '(eql 42 42)))))

(deftest eql-cross-type-fixnum-float
  (ok (null (compile-and-run '(eql 1 1.0)))))

(deftest eql-same-float
  (ok (eql t (compile-and-run '(eql 3.14 3.14)))))

(deftest eql-same-ratio
  (ok (eql t (compile-and-run '(eql 1/2 1/2)))))

(deftest eql-same-char
  (ok (eql t (compile-and-run '(eql #\\a #\\a)))))
```

**Implementation Pattern**:
```lisp
(defun compile-eql (args env)
  "Compile (eql x y) - type-aware value equality"
  (with-gensyms ($x $y $result)
    `((local.set ,$x ,@(compile-to-instructions (first args) env))
      (local.set ,$y ,@(compile-to-instructions (second args) env))
      (block ,$result (result anyref)
        ;; Check if both fixnum (i31ref)
        (local.get ,$x) (ref.test :i31)
        (if
          (then
            (local.get ,$y) (ref.test :i31)
            (if
              (then
                (local.get ,$x) (local.get ,$y) (ref.eq)
                (if (result anyref)
                  (then (ref.i31 (i32.const 1)))
                  (else (global.get ,$nil)))
                (br ,$result))
              (else (global.get ,$nil) (br ,$result)))))
        ;; Check if both float
        (local.get ,$x) (ref.test (:ref ,+type-float+))
        (if
          (then
            (local.get ,$y) (ref.test (:ref ,+type-float+))
            (if
              (then
                (struct.get $float $value (ref.cast (:ref $float) (local.get ,$x)))
                (struct.get $float $value (ref.cast (:ref $float) (local.get ,$y)))
                (f64.eq)
                (if (result anyref)
                  (then (ref.i31 (i32.const 1)))
                  (else (global.get ,$nil)))
                (br ,$result))
              (else (global.get ,$nil) (br ,$result)))))
        ;; Continue for ratio, character...
        ;; Fall back to ref.eq for other types
        (local.get ,$x) (local.get ,$y) (ref.eq)
        (if (result anyref)
          (then (ref.i31 (i32.const 1)))
          (else (global.get ,$nil)))))))
```

### Step 4: Implement `equal` (Recursive)

**Test First**:
```lisp
(deftest equal-same-list
  (ok (eql t (compile-and-run '(equal '(1 2 3) '(1 2 3))))))

(deftest equal-different-list
  (ok (null (compile-and-run '(equal '(1 2 3) '(1 2 4))))))

(deftest equal-strings
  (ok (eql t (compile-and-run '(equal "hello" "hello")))))

(deftest equal-nested
  (ok (eql t (compile-and-run '(equal '((a b) c) '((a b) c))))))
```

**Implementation**: Define as a Wasm function that can call itself recursively.

### Step 5: Implement `equalp` (Case-Insensitive)

**Test First**:
```lisp
(deftest equalp-case-insensitive-string
  (ok (eql t (compile-and-run '(equalp "HELLO" "hello")))))

(deftest equalp-numeric-coercion
  (ok (eql t (compile-and-run '(equalp 1 1.0)))))

(deftest equalp-case-insensitive-char
  (ok (eql t (compile-and-run '(equalp #\\A #\\a)))))
```

**Implementation**: Similar to `equal` but uses `char-equal`, `string-equal`, and numeric `=`.

### Step 6: Verify `and`/`or` Compilation

**Test First**:
```lisp
(deftest and-short-circuit
  (ok (null (compile-and-run '(and t nil t)))))

(deftest and-returns-last
  (ok (= 3 (compile-and-run '(and 1 2 3)))))

(deftest or-short-circuit
  (ok (= 5 (compile-and-run '(or nil 5 99)))))

(deftest and-empty
  (ok (eql t (compile-and-run '(and)))))

(deftest or-empty
  (ok (null (compile-and-run '(or)))))
```

**Verification**: Confirm that `parse-and-form` and `parse-or-form` in `ast.lisp` correctly expand to nested `if` forms, and the `if` compilation handles them correctly.

## Running Tests

```bash
# Run unit tests
nix develop -c rove tests/unit/equality-predicates-test.lisp

# Run contract tests
nix develop -c rove tests/contract/equality-wasm-test.lisp

# Validate Wasm output
nix develop -c wasm-tools validate output.wasm

# Run full test suite
nix flake check
```

## Common Patterns

### Boolean Result Wrapper

```lisp
(defun wrap-boolean-result (instructions)
  "Convert i32 (0/1) to T/NIL"
  `(,@instructions
    (if (result anyref)
      (then (ref.i31 (i32.const 1)))
      (else (global.get ,$nil)))))
```

### Type Dispatch Skeleton

```lisp
(defun compile-type-dispatch (x-instr y-instr type-id comparison-instr)
  `(,@x-instr
    (ref.test (:ref ,type-id))
    (if
      (then
        ,@y-instr
        (ref.test (:ref ,type-id))
        (if
          (then ,@comparison-instr)
          (else (global.get ,$nil))))
      (else ...))))
```

## Checklist

- [ ] `not` compiles to `ref.eq` with NIL
- [ ] `eq` compiles to single `ref.eq`
- [ ] `eql` handles fixnum, float, ratio, character with type dispatch
- [ ] `eql` returns NIL for cross-type numeric comparison
- [ ] `equal` recursively compares cons cells
- [ ] `equal` uses `string=` for strings
- [ ] `equalp` uses `char-equal` and `string-equal`
- [ ] `equalp` uses numeric `=` for type coercion
- [ ] `and`/`or` short-circuit correctly
- [ ] `(and)` returns T, `(or)` returns NIL
- [ ] All Wasm output passes `wasm-tools validate`
- [ ] ANSI CL test suite tests pass
