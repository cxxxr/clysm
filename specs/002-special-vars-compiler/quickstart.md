# Quick Start: Special Variables Compiler Integration

**Feature**: 002-special-vars-compiler

## Prerequisites

```bash
# Enter Nix development environment
nix develop

# Verify SBCL is available
sbcl --version  # Should be 2.4+

# Load the project
sbcl --load clysm.asd --eval '(asdf:load-system :clysm)'
```

## Development Workflow (TDD)

### 1. Run Existing Tests (Baseline)

```bash
# Run all tests - should pass
sbcl --load clysm.asd --eval '(asdf:test-system :clysm)'

# Or via Nix
nix flake check
```

### 2. Start with Unit Tests (Red Phase)

Edit `tests/unit/special-vars-ast-test.lisp` (create if not exists):

```lisp
;;;; special-vars-ast-test.lisp
(in-package #:clysm/tests)

(deftest ast-defvar-parsing
  (testing "defvar with init creates correct AST"
    (let ((ast (parse-expr '(defvar *x* 10))))
      (ok (typep ast 'ast-defvar))
      (ok (eq (ast-defvar-name ast) '*x*))
      (ok (ast-defvar-init-form ast)))))

(deftest ast-defparameter-parsing
  (testing "defparameter requires init-form"
    (let ((ast (parse-expr '(defparameter *y* 20))))
      (ok (typep ast 'ast-defparameter))
      (ok (eq (ast-defparameter-name ast) '*y*)))))
```

### 3. Implement AST Nodes (Green Phase)

Edit `src/clysm/compiler/ast.lisp`:

```lisp
;;; ============================================================
;;; Special Variable Declarations
;;; ============================================================

(defstruct (ast-defvar (:include ast-node) (:conc-name ast-defvar-))
  "defvar declaration node."
  (name nil :type symbol)
  (init-form nil :type t)
  (docstring nil :type (or null string)))

(defstruct (ast-defparameter (:include ast-node) (:conc-name ast-defparameter-))
  "defparameter declaration node."
  (name nil :type symbol)
  (init-form nil :type t)
  (docstring nil :type (or null string)))
```

### 4. Add Parsing Support

In `parse-compound-form` in `ast.lisp`:

```lisp
(case op
  ;; ... existing cases ...
  (defvar (parse-defvar-form args))
  (defparameter (parse-defparameter-form args))
  ;; ...
  )

(defun parse-defvar-form (args)
  "Parse (defvar name [init-form] [docstring])."
  (let ((name (first args))
        (init-form (second args))
        (docstring (third args)))
    (unless (symbolp name)
      (error "defvar name must be a symbol: ~A" name))
    (make-ast-defvar
     :name name
     :init-form (when init-form (parse-expr init-form))
     :docstring docstring)))

(defun parse-defparameter-form (args)
  "Parse (defparameter name init-form [docstring])."
  (let ((name (first args))
        (init-form (second args))
        (docstring (third args)))
    (unless (symbolp name)
      (error "defparameter name must be a symbol: ~A" name))
    (unless init-form
      (error "defparameter requires an initial value"))
    (make-ast-defparameter
     :name name
     :init-form (parse-expr init-form)
     :docstring docstring)))
```

### 5. Add Special Variable Registry

Edit `src/clysm/compiler/env.lisp`:

```lisp
(defvar *special-variables* (make-hash-table :test 'eq)
  "Registry of declared special variables.")

(defun register-special-variable (name)
  "Mark a symbol as a special variable."
  (setf (gethash name *special-variables*) t))

(defun special-variable-p (name)
  "Check if a symbol is declared as special."
  (gethash name *special-variables*))

(defun clear-special-variables ()
  "Clear the special variable registry."
  (clrhash *special-variables*))
```

### 6. Modify Code Generation

Edit `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
(defun compile-var-ref (ast env)
  "Compile a variable reference."
  (let* ((name (ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable
      (local-idx
       (list (list :local.get local-idx)))
      ;; Special variable
      ((special-variable-p name)
       (compile-special-var-ref name))
      ;; Captured variable
      ((env-lookup-captured env name)
       (compile-captured-var-access name env))
      ;; Unknown
      (t
       (error "Unbound variable: ~A" name)))))

(defun compile-special-var-ref (name)
  "Generate code to read a special variable's value."
  (list
   (list :global.get (symbol-global-name name))
   (list :struct.get +type-symbol+ 1)))  ;; $value is field 1
```

### 7. Run Tests (Verify Green)

```bash
sbcl --load clysm.asd --eval '(asdf:test-system :clysm)'
```

### 8. Refactor (if needed)

- Extract common patterns
- Add documentation
- Ensure no regressions

### 9. Integration Tests

Update `tests/integration/special-var-test.lisp` with real implementations replacing placeholders.

## Key Files to Modify

| File | Changes |
|------|---------|
| `src/clysm/compiler/ast.lisp` | Add ast-defvar, ast-defparameter |
| `src/clysm/compiler/env.lisp` | Add special variable registry |
| `src/clysm/compiler/codegen/func-section.lisp` | Modify compile-var-ref, compile-let, compile-setq |
| `src/clysm/compiler/codegen/gc-types.lisp` | Add $binding_frame type |
| `tests/unit/special-vars-ast-test.lisp` | New unit tests |
| `tests/integration/special-var-test.lisp` | Complete placeholder tests |

## Verification Commands

```bash
# Unit tests only
sbcl --eval '(ql:quickload :rove)' \
     --eval '(rove:run :clysm/tests :unit/special-vars-ast-test)'

# Full test suite
nix flake check

# Validate generated Wasm
wasm-tools validate output.wasm
```

## Common Issues

### "Symbol not found in environment"

- Ensure `register-special-variable` is called during parsing
- Check that `*special-variables*` persists across compilation phases

### "Type mismatch in struct.get"

- Verify symbol global is `(ref $symbol)` type
- Check field index matches (field 1 for $value)

### "Binding not restored on exception"

- Ensure `try_table` wraps the body
- Verify `catch_all` calls restore-binding before rethrow

## Next Steps

After completing this quickstart:

1. Run `/speckit.tasks` to generate detailed task breakdown
2. Commit with: `feat(compiler): implement special variables support`
3. Open PR when all tests pass
