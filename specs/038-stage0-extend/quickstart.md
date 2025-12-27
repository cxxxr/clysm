# Quickstart: Stage 0 Capability Extension

**Date**: 2025-12-27
**Branch**: 038-stage0-extend

## Prerequisites

1. **SBCL 2.4+** installed
2. **Nix** with flakes enabled
3. **wasm-tools** available in PATH
4. Clysm repository cloned

## Development Environment

```bash
# Enter development shell
cd /home/user/src/clysm-workbench/clysm3
nix develop

# Load the Clysm system
sbcl --load clysm.asd
# (asdf:load-system :clysm)
```

## Running Tests

### Unit Tests (TDD - Write First)

```bash
# Run all unit tests
sbcl --eval '(asdf:test-system :clysm)'

# Run specific test file
sbcl --eval '(asdf:load-system :clysm/tests)' \
     --eval '(rove:run :clysm/tests/unit/defconstant-test)'
```

### Contract Tests (Wasm Validation)

```bash
# Generate Wasm and validate
sbcl --load build/bootstrap.lisp

# Validate output
wasm-tools validate dist/clysm-stage0.wasm
wasm-tools print dist/clysm-stage0.wasm > /tmp/stage0.wat
```

### Integration Tests

```bash
# Run compilation rate verification
sbcl --eval '(asdf:load-system :clysm/tests)' \
     --eval '(rove:run :clysm/tests/integration/stage0-compile-rate-test)'
```

## Implementation Workflow

### Phase 1: defconstant/defparameter (P1)

1. **Write tests first**:
   ```lisp
   ;; tests/unit/defconstant-test.lisp
   (deftest defconstant-simple-value
     (let ((ast (parse-form '(defconstant +max+ 100))))
       (ok (typep ast 'ast-defconstant))
       (ok (eq (ast-defconstant-name ast) '+max+))))
   ```

2. **Implement AST**:
   ```lisp
   ;; src/clysm/compiler/ast.lisp
   (defstruct (ast-defconstant (:include ast-node))
     name
     value-form
     docstring)
   ```

3. **Implement compilation**:
   ```lisp
   ;; src/clysm/compiler/codegen/func-section.lisp
   (defun compile-defconstant (ast env)
     ...)
   ```

4. **Verify**: Run tests, then bootstrap

### Phase 2: declare handling (P2)

1. **Write tests**:
   ```lisp
   (deftest declare-in-defun-skipped
     (let ((ast (parse-form '(defun foo (x)
                               (declare (type fixnum x))
                               (+ x 1)))))
       ;; declare should not appear in body
       (ok (notany #'declare-form-p (ast-defun-body ast)))))
   ```

2. **Implement filtering** in AST parser

### Phase 3: define-condition (P2)

1. **Write tests**:
   ```lisp
   (deftest define-condition-expands-to-defclass
     (let ((expanded (expand-define-condition
                       '(define-condition my-error (error)
                          ((msg :initarg :msg))))))
       (ok (eq (car expanded) 'defclass))))
   ```

2. **Implement expansion** in bootstrap.lisp

### Phase 4: defstruct (P3)

1. **Write tests**:
   ```lisp
   (deftest defstruct-generates-constructor
     (let ((forms (expand-defstruct '(defstruct point x y))))
       (ok (some (lambda (f)
                   (eq (cadr f) 'make-point))
                 forms))))
   ```

2. **Implement expansion** in bootstrap.lisp

### Phase 5: Error Reporting (P3)

1. **Enhance `compile-result`** struct
2. **Add operator tracking** in `record-failure`
3. **Generate report** in `generate-failure-report`

## Verification Commands

```bash
# Full bootstrap with enhanced reporting
sbcl --load build/bootstrap.lisp

# Expected output format:
# === Compilation Statistics ===
# Compiled: 453/849 forms (53.4%)
#
# Failures by Operator:
#   handler-case: 45 failures
#   loop: 38 failures
#   ...
```

## Debugging Tips

### Constant Folding Issues

```lisp
;; Test constant folding in REPL
(fold-constant-expression '(* 8 1024) *constant-registry*)
;; Should return (values 8192 t)
```

### AST Parsing Issues

```lisp
;; Parse and inspect AST
(describe (parse-form '(defconstant +test+ 42)))
```

### Wasm Validation Failures

```bash
# Get detailed error
wasm-tools validate dist/clysm-stage0.wasm 2>&1 | head -20

# Convert to WAT for inspection
wasm-tools print dist/clysm-stage0.wasm | less
```

## Success Criteria Checklist

- [ ] `wasm-tools validate dist/clysm-stage0.wasm` passes
- [ ] Compilation rate ≥ 50% (shown in bootstrap output)
- [ ] All unit tests pass: `(rove:run :clysm/tests)`
- [ ] `nix flake check` passes
