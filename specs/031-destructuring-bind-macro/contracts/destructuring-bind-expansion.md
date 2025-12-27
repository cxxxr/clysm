# Contract: Destructuring-Bind Macro Expansion

**Date**: 2025-12-27
**Feature**: 031-destructuring-bind-macro

## Overview

This contract defines the expected expansion format for `destructuring-bind` macro calls.

## Input Format

```lisp
(destructuring-bind lambda-list expression &body body)
```

- `lambda-list`: Destructuring lambda-list (may be nested)
- `expression`: Form that evaluates to a list
- `body`: Implicit progn of forms

## Output Format

The macro expands to nested `let` forms with runtime checks.

### Required Parameters

**Input**:
```lisp
(destructuring-bind (a b c) list body)
```

**Output**:
```lisp
(let ((#:DB-LIST-0 list))
  (unless (consp #:DB-LIST-0)
    (error 'program-error
           :format-control "Not enough elements for required parameter ~S"
           :format-arguments '(a)))
  (let ((a (car #:DB-LIST-0)))
    (let ((#:DB-LIST-1 (cdr #:DB-LIST-0)))
      (unless (consp #:DB-LIST-1)
        (error 'program-error
               :format-control "Not enough elements for required parameter ~S"
               :format-arguments '(b)))
      (let ((b (car #:DB-LIST-1)))
        (let ((#:DB-LIST-2 (cdr #:DB-LIST-1)))
          (unless (consp #:DB-LIST-2)
            (error 'program-error
                   :format-control "Not enough elements for required parameter ~S"
                   :format-arguments '(c)))
          (let ((c (car #:DB-LIST-2)))
            (let ((#:DB-LIST-3 (cdr #:DB-LIST-2)))
              (unless (null #:DB-LIST-3)
                (error 'program-error
                       :format-control "Too many elements in list"
                       :format-arguments nil))
              body)))))))
```

### &optional Parameters

**Input**:
```lisp
(destructuring-bind (a &optional (b 10 b-p)) list body)
```

**Output**:
```lisp
(let ((#:DB-LIST-0 list))
  ;; Required 'a' binding
  (unless (consp #:DB-LIST-0)
    (error 'program-error ...))
  (let ((a (car #:DB-LIST-0)))
    (let ((#:DB-LIST-1 (cdr #:DB-LIST-0)))
      ;; Optional 'b' with default and supplied-p
      (let* ((#:DB-SUPPLIED (consp #:DB-LIST-1))
             (b (if #:DB-SUPPLIED (car #:DB-LIST-1) 10))
             (b-p #:DB-SUPPLIED))
        (let ((#:DB-LIST-2 (if #:DB-SUPPLIED (cdr #:DB-LIST-1) nil)))
          body)))))
```

### &rest / &body Parameters

**Input**:
```lisp
(destructuring-bind (a &rest rest) list body)
```

**Output**:
```lisp
(let ((#:DB-LIST-0 list))
  (unless (consp #:DB-LIST-0)
    (error 'program-error ...))
  (let ((a (car #:DB-LIST-0)))
    (let ((rest (cdr #:DB-LIST-0)))
      body)))
```

### &key Parameters

**Input**:
```lisp
(destructuring-bind (&key x (y 10) (((:z-key z)) 20 z-p)) list body)
```

**Output**:
```lisp
(let ((#:DB-LIST-0 list))
  ;; Extract keys from plist
  (let* ((#:DB-KEYS (list :x :y :z-key))
         (x (getf #:DB-LIST-0 :x nil))
         (y (getf #:DB-LIST-0 :y 10))
         (#:DB-Z-SUPPLIED (not (eq '#:DB-MISSING (getf #:DB-LIST-0 :z-key '#:DB-MISSING))))
         (z (if #:DB-Z-SUPPLIED (getf #:DB-LIST-0 :z-key) 20))
         (z-p #:DB-Z-SUPPLIED))
    ;; Unknown key check (unless &allow-other-keys)
    (loop for (key val) on #:DB-LIST-0 by #'cddr
          unless (member key #:DB-KEYS)
          do (error 'program-error
                    :format-control "Unknown keyword argument: ~S"
                    :format-arguments (list key)))
    body))
```

### &whole Parameter

**Input**:
```lisp
(destructuring-bind (&whole w a b) list body)
```

**Output**:
```lisp
(let ((w list))
  (let ((#:DB-LIST-0 list))
    (unless (consp #:DB-LIST-0)
      (error 'program-error ...))
    (let ((a (car #:DB-LIST-0)))
      (let ((#:DB-LIST-1 (cdr #:DB-LIST-0)))
        (unless (consp #:DB-LIST-1)
          (error 'program-error ...))
        (let ((b (car #:DB-LIST-1)))
          body)))))
```

### Nested Patterns

**Input**:
```lisp
(destructuring-bind ((a b) c) list body)
```

**Output**:
```lisp
(let ((#:DB-LIST-0 list))
  (unless (consp #:DB-LIST-0)
    (error 'program-error ...))
  (let ((#:DB-NESTED-0 (car #:DB-LIST-0)))
    (unless (consp #:DB-NESTED-0)
      (error 'program-error ...))
    (let ((a (car #:DB-NESTED-0)))
      (let ((#:DB-NESTED-1 (cdr #:DB-NESTED-0)))
        (unless (consp #:DB-NESTED-1)
          (error 'program-error ...))
        (let ((b (car #:DB-NESTED-1)))
          (let ((#:DB-LIST-1 (cdr #:DB-LIST-0)))
            (unless (consp #:DB-LIST-1)
              (error 'program-error ...))
            (let ((c (car #:DB-LIST-1)))
              body)))))))
```

## Invariants

1. **Single evaluation**: Expression is evaluated exactly once
2. **Left-to-right**: Parameters are bound left-to-right
3. **Default evaluation**: Default forms evaluated only when parameter is missing
4. **Gensym uniqueness**: All generated temporaries use unique gensym prefixes
5. **Error locality**: Error messages include parameter name for debugging

## Test Scenarios

| Scenario | Input | Expected Behavior |
|----------|-------|-------------------|
| Basic | `(a b c)` + `(1 2 3)` | Binds correctly |
| Missing required | `(a b c)` + `(1 2)` | Signals program-error |
| Excess elements | `(a b)` + `(1 2 3)` | Signals program-error |
| With &rest | `(a &rest r)` + `(1 2 3)` | `r = (2 3)` |
| With &optional | `(a &optional b)` + `(1)` | `b = nil` |
| Optional default | `(&optional (x 5))` + `()` | `x = 5` |
| Supplied-p true | `(&optional (x nil x-p))` + `(1)` | `x-p = T` |
| Supplied-p false | `(&optional (x nil x-p))` + `()` | `x-p = NIL` |
| &key basic | `(&key x y)` + `(:x 1 :y 2)` | `x=1, y=2` |
| &key missing | `(&key x y)` + `(:x 1)` | `y = nil` |
| Unknown key | `(&key x)` + `(:x 1 :z 3)` | Signals program-error |
| &allow-other-keys | `(&key x &allow-other-keys)` + `(:x 1 :z 3)` | Succeeds |
| Nested 1 level | `((a b) c)` + `((1 2) 3)` | Binds correctly |
| Nested 3 levels | `(((a)))` + `(((1)))` | Binds correctly |
| &whole | `(&whole w a b)` + `(1 2)` | `w=(1 2), a=1, b=2` |
