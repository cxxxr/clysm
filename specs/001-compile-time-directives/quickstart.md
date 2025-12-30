# Quickstart: Compile-Time Directive Processing

**Date**: 2025-12-30
**Feature**: 001-compile-time-directives

## Goal

Enable the Clysm compiler to handle `in-package`, `defpackage`, `declaim`, and `proclaim` forms by evaluating them at compile-time and skipping AST/Wasm generation.

## Implementation Steps

### Step 1: Create Directive Predicate

```lisp
;; src/clysm/compiler/directive.lisp

(defun directive-form-p (form)
  "Return T if FORM is a compile-time directive that should not generate code."
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(in-package defpackage declaim proclaim)
               :test #'eq)))
```

### Step 2: Create Compile-Toplevel-Form Function

```lisp
(defun compile-toplevel-form (form)
  "Process a toplevel form. Return nil for directives, AST otherwise."
  (if (directive-form-p form)
      (progn
        (eval form)  ; Evaluate in host environment
        nil)         ; Signal no AST generation
      (parse-expr form)))  ; Normal parsing
```

### Step 3: Integrate Into compile-to-module

Modify `src/clysm/compiler/compiler.lisp` around line 80:

```lisp
;; In the toplevel form processing loop:
(dolist (form forms)
  (let ((ast (compile-toplevel-form form)))
    (when ast  ; Skip nil (directives)
      (push ast collected-asts))))
```

### Step 4: Add Error Handling

```lisp
(defun compile-toplevel-form (form)
  (if (directive-form-p form)
      (handler-case
          (progn (eval form) nil)
        (error (e)
          (error "Compile-time directive error in ~S:~%  ~A" form e)))
      (parse-expr form)))
```

## Testing

### Unit Test: Directive Detection

```lisp
(deftest directive-form-p-test
  (ok (directive-form-p '(in-package :clysm)))
  (ok (directive-form-p '(defpackage :foo (:use :cl))))
  (ok (directive-form-p '(declaim (optimize (speed 3)))))
  (ok (directive-form-p '(proclaim '(special *x*))))
  (ok (not (directive-form-p '(defun foo () nil))))
  (ok (not (directive-form-p '(+ 1 2)))))
```

### Unit Test: Package Context

```lisp
(deftest in-package-compilation-test
  (let ((*package* (find-package :cl-user)))
    (compile-toplevel-form '(in-package :clysm))
    (ok (eq *package* (find-package :clysm)))))
```

### Contract Test: No Wasm Output

```lisp
(deftest directive-no-wasm-test
  (let ((result (compile-toplevel-form '(in-package :clysm))))
    (ok (null result) "in-package returns nil (no AST)")))
```

## Success Verification

After implementation, run:

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Verify compilation of codebase
sbcl --load build/stage0-complete.lisp

# Check error count reduction (should be 61 fewer errors)
```

## Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `src/clysm/compiler/directive.lisp` | CREATE | Directive handling functions |
| `src/clysm/compiler/compiler.lisp` | MODIFY | Integrate compile-toplevel-form |
| `clysm.asd` | MODIFY | Add directive.lisp to system |
| `tests/unit/directive-test.lisp` | CREATE | Unit tests |
| `tests/contract/directive-output-test.lisp` | CREATE | Contract tests |

## HyperSpec References

- [in-package](resources/HyperSpec/Body/m_in_pkg.htm)
- [defpackage](resources/HyperSpec/Body/m_defpkg.htm)
- [declaim](resources/HyperSpec/Body/m_declai.htm)
- [proclaim](resources/HyperSpec/Body/f_procla.htm)
