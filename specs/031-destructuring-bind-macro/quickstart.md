# Quickstart: Destructuring-Bind Macro

**Date**: 2025-12-27
**Feature**: 031-destructuring-bind-macro

## Prerequisites

- Nix development environment: `nix develop`
- SBCL 2.4+ (provided by Nix)
- Understanding of existing macro system in `src/clysm/lib/macros.lisp`

## Development Setup

```bash
# Enter Nix development shell
nix develop

# Run existing tests to verify baseline
nix flake check

# Or run tests directly
sbcl --load tests/main.lisp --eval '(rove:run :clysm/tests)'
```

## Quick Implementation Steps

### Step 1: Create Lambda-List Parser

Create `src/clysm/lib/destructuring.lisp`:

```lisp
(in-package #:clysm/lib/macros)

(defstruct parsed-lambda-list
  whole-var
  required-params
  optional-params
  rest-var
  key-params
  allow-other-keys-p)

(defun parse-destructuring-lambda-list (lambda-list)
  "Parse a destructuring lambda-list into structured form."
  ;; Implementation here
  ...)
```

### Step 2: Add Expander to macros.lisp

Add after the LOOP expander section:

```lisp
;;; ============================================================
;;; Destructuring-Bind Macro (031-destructuring-bind-macro)
;;; ============================================================

(defun make-destructuring-bind-expander ()
  "Create a macro expander for DESTRUCTURING-BIND."
  (lambda (form)
    (let ((lambda-list (second form))
          (expression (third form))
          (body (cdddr form)))
      ;; Parse and generate expansion
      ...)))
```

### Step 3: Register the Macro

In `install-standard-macros`:

```lisp
(clysm/compiler/transform/macro:register-macro
 registry 'destructuring-bind (make-destructuring-bind-expander))
```

### Step 4: Export Symbols

In `src/clysm/package.lisp`, add to `clysm/lib/macros` exports:

```lisp
#:make-destructuring-bind-expander
```

## Test-First Development

Following TDD (Constitution Principle VII), create tests first:

### Unit Tests

Create `tests/unit/destructuring-bind-test.lisp`:

```lisp
(defpackage #:clysm/tests/unit/destructuring-bind
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/destructuring-bind)

(deftest required-params-test
  "Test basic required parameter destructuring."
  (let ((expansion (funcall (clysm/lib/macros:make-destructuring-bind-expander)
                            '(destructuring-bind (a b c) list body))))
    (ok (consp expansion))
    (ok (eq 'let (first expansion)))))
```

### Integration Tests

Create `tests/integration/destructuring-ansi-test.lisp`:

```lisp
(deftest basic-destructuring
  "ANSI CL compliance: basic destructuring."
  (ok (equal '(1 2 3)
             (destructuring-bind (a b c) '(1 2 3)
               (list a b c)))))
```

## Key Files to Modify

| File | Changes |
|------|---------|
| `src/clysm/lib/macros.lisp` | Add `make-destructuring-bind-expander` and helpers |
| `src/clysm/package.lisp` | Export new symbols |
| `tests/unit/destructuring-bind-test.lisp` | New unit tests |
| `tests/integration/destructuring-ansi-test.lisp` | New ANSI compliance tests |

## Verification Commands

```bash
# Run all tests
nix flake check

# Run specific test file
sbcl --load tests/main.lisp \
     --eval '(rove:run-test #\*:clysm/tests/unit/destructuring-bind)'

# Verify compiler self-hosting (after implementation)
grep -n 'destructuring-bind' src/clysm/**/*.lisp
```

## Common Pitfalls

1. **Gensym collision**: Use unique prefixes like `"DB-LIST-"` for generated variables
2. **Evaluation order**: Default forms must only be evaluated when parameter is missing
3. **Nested patterns**: Each nesting level needs its own temporary variable
4. **&key plist**: Must handle both odd and even plist lengths gracefully
5. **Error messages**: Include parameter name in error messages for debugging

## Next Steps

After basic implementation:

1. Add `&optional` support with defaults and supplied-p
2. Add `&rest` / `&body` support
3. Add `&key` support with keyword validation
4. Add `&whole` support
5. Validate all 9 compiler locations work
