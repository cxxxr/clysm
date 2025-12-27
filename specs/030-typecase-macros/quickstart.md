# Quickstart: Type Dispatch Macros

**Feature**: 030-typecase-macros
**Time to First Test**: ~30 minutes

## Overview

This feature implements four ANSI CL type dispatch macros as compile-time expansions. All macros expand to primitive predicate calls - no runtime `typep` function needed.

## Prerequisites

- Working development environment (`nix develop`)
- Existing test infrastructure (`tests/` directory)
- Understanding of macro expander pattern (see `make-case-expander` in `src/clysm/lib/macros.lisp`)

## Quick Start Steps

### Step 1: Create Test File (TDD)

```bash
touch tests/unit/typecase/typecase-test.lisp
```

Write initial failing test:

```lisp
;;;; typecase-test.lisp
(in-package #:clysm-test)

(deftest typecase-basic-dispatch
  "Test basic typecase type dispatch"
  (let* ((form '(typecase x
                  (integer "int")
                  (symbol "sym")
                  (otherwise "other")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Should expand to let + nested if
    (ok (eq (car expanded) 'let))
    (ok (member 'integerp (flatten expanded)))
    (ok (member 'symbolp (flatten expanded)))))
```

### Step 2: Run Failing Test

```bash
nix flake check  # Should fail - make-typecase-expander not found
```

### Step 3: Implement make-typecase-expander

Add to `src/clysm/lib/macros.lisp`:

```lisp
;;; ============================================================
;;; Typecase Macro Expanders (030-typecase-macros)
;;; ============================================================

(defun type-specifier-to-predicate (typespec value-sym)
  "Convert type specifier to predicate form."
  (cond
    ;; Atomic type specifiers
    ((eq typespec 'integer) (list 'integerp value-sym))
    ((eq typespec 'symbol) (list 'symbolp value-sym))
    ((eq typespec 'cons) (list 'consp value-sym))
    ((eq typespec 'null) (list 'null value-sym))
    ((eq typespec 'list) (list 'listp value-sym))
    ((eq typespec 'number) (list 'numberp value-sym))
    ((eq typespec 'float) (list 'floatp value-sym))
    ((eq typespec 'ratio) (list 'rationalp value-sym))
    ((eq typespec 'character) (list 'characterp value-sym))
    ((eq typespec 'function) (list 'functionp value-sym))
    ((eq typespec 'string) (list 'stringp value-sym))
    ((eq typespec 't) t)
    ;; Compound type specifiers
    ((and (listp typespec) (eq (car typespec) 'or))
     (cons 'or (mapcar (lambda (t) (type-specifier-to-predicate t value-sym))
                       (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'and))
     (cons 'and (mapcar (lambda (t) (type-specifier-to-predicate t value-sym))
                        (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'not))
     (list 'not (type-specifier-to-predicate (cadr typespec) value-sym)))
    ((and (listp typespec) (eq (car typespec) 'member))
     (cons 'or (mapcar (lambda (item) (list 'eql value-sym (list 'quote item)))
                       (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'satisfies))
     (list 'funcall (list 'function (cadr typespec)) value-sym))
    ;; Unknown - try as class name
    (t (list 'typep value-sym (list 'quote typespec)))))

(defun make-typecase-expander ()
  "Create a macro expander for TYPECASE."
  (lambda (form)
    (let ((keyform (second form))
          (clauses (cddr form))
          (key-var (gensym "KEY-")))
      (labels ((otherwise-clause-p (clause)
                 (let ((keys (first clause)))
                   (or (eq keys 'otherwise)
                       (eq keys 't))))
               (make-type-test (key-var types)
                 (if (listp types)
                     (if (and (= 1 (length types)) (not (listp (first types))))
                         (type-specifier-to-predicate (first types) key-var)
                         (cons 'or (mapcar (lambda (t)
                                             (type-specifier-to-predicate t key-var))
                                           types)))
                     (type-specifier-to-predicate types key-var)))
               (expand-clauses (clauses)
                 (if (null clauses)
                     nil
                     (let* ((clause (first clauses))
                            (types (first clause))
                            (body (rest clause)))
                       (if (otherwise-clause-p clause)
                           (cons 'progn body)
                           (list 'if
                                 (make-type-test key-var types)
                                 (cons 'progn body)
                                 (expand-clauses (rest clauses))))))))
        (list 'let (list (list key-var keyform))
              (expand-clauses clauses))))))
```

### Step 4: Register Macro

Add to `install-standard-macros`:

```lisp
(clysm/compiler/transform/macro:register-macro
 registry 'typecase (make-typecase-expander))
```

### Step 5: Run Test - Should Pass

```bash
nix flake check
```

### Step 6: Repeat for Other Macros

Follow same TDD cycle for:
1. `etypecase` - add type-error signaling
2. `check-type` - add store-value restart
3. `ctypecase` - combine typecase + check-type patterns

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/lib/macros.lisp` | Add expander functions |
| `tests/unit/typecase/*.lisp` | Unit tests |
| `tests/contract/typecase-wasm-test.lisp` | Wasm validation |
| `tests/integration/typecase-ansi-test.lisp` | ANSI compliance |

## Key Patterns

### Expander Factory Pattern

```lisp
(defun make-XXX-expander ()
  "Create a macro expander for XXX."
  (lambda (form)
    ;; Parse form
    ;; Generate expanded form
    expanded-form))
```

### Gensym for Single Evaluation

```lisp
(let ((key-var (gensym "KEY-")))
  (list 'let (list (list key-var keyform))
        ...use key-var...))
```

### Error Signaling Pattern

```lisp
(error 'type-error
       :datum key-var
       :expected-type '(or type1 type2))
```

### Store-Value Restart Pattern

```lisp
(restart-case
    (error 'type-error :datum x :expected-type 'integer)
  (store-value (new-value)
    :report "Supply a new value"
    :interactive (lambda () (list (read)))
    (setf place new-value)))
```

## Common Issues

1. **Circular dependency**: Don't use typecase in the macro expander itself
2. **Gensym collision**: Always use unique prefix for gensym
3. **Multiple type specifiers**: Handle `((t1 t2) body)` vs `(t1 body)`
4. **otherwise/t in e/ctypecase**: Must reject at expansion time

## Next Steps After Basic Implementation

1. Add compound type specifier tests
2. Verify 892 compiler typecase usages work
3. Run `nix flake check` for full validation
4. Create PR with all tests passing
