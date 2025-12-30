# Quickstart: Control Structure Extensions

**Branch**: `001-control-structure-extension` | **Date**: 2025-12-30

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify tools are available
sbcl --version
wasm-tools --version
```

## Running Tests

### Full Test Suite

```bash
sbcl --eval "(asdf:test-system :clysm)"
```

### Contract Tests Only

```bash
sbcl --eval "(asdf:test-system :clysm/contracts)"
```

### Individual Control Structure Tests

```lisp
;; In SBCL REPL
(asdf:load-system :clysm/contracts)
(in-package :clysm-test)

;; Run values tests
(rove:run-test 'values-zero-returns-nil)
(rove:run-test 'values-multiple-stores-in-buffer)

;; Run handler-case tests
(rove:run-test 'handler-case-catches-error)
(rove:run-test 'handler-case-generates-try-table)

;; Run labels tests
(rove:run-test 'labels-mutual-recursion)

;; Run the tests
(rove:run-test 'the-simple-type-compiles)
```

## Verifying Compilation

### Compile a Single Form

```lisp
;; Load the compiler
(asdf:load-system :clysm)
(in-package :clysm)

;; Test values compilation
(compile-to-wasm '(values 1 2 3))

;; Test the compilation
(compile-to-wasm '(the fixnum 42))

;; Test labels compilation
(compile-to-wasm '(labels ((f () (g)) (g () (f))) (f)))

;; Test handler-case compilation
(compile-to-wasm '(handler-case (error "test")
                    (error (e) e)))
```

### Validate Generated Wasm

```bash
# After generating a .wasm file
wasm-tools validate output.wasm

# View WAT format for debugging
wasm-tools print output.wasm
```

## Debugging Compilation Failures

### Check AST Parsing

```lisp
;; Verify AST node creation
(clysm/compiler:parse-expr '(values 1 2 3))
;; Should return: #S(AST-VALUES :VALUES (#S(AST-LITERAL ...) ...))

(clysm/compiler:parse-expr '(the fixnum 42))
;; Should return: #S(AST-LITERAL :VALUE 42)

(clysm/compiler:parse-expr '(handler-case (f) (error (e) e)))
;; Should return: #S(AST-HANDLER-CASE ...)
```

### Check Codegen Dispatch

```lisp
;; If AST parses correctly but compilation fails,
;; check compile-to-instructions dispatch in func-section.lisp
;; Look for etypecase clause matching the AST node type
```

## Key Files

| Component | File | Purpose |
|-----------|------|---------|
| AST definitions | `src/clysm/compiler/ast.lisp` | Add/modify AST nodes |
| AST parsing | `src/clysm/compiler/ast.lisp` | `parse-compound-form` case clauses |
| Codegen | `src/clysm/compiler/codegen/func-section.lisp` | `compile-to-instructions` dispatch |
| MV infrastructure | `src/clysm/runtime/multi-value.lisp` | Multiple values support |
| Handler macro | `src/clysm/conditions/handlers.lisp` | Original handler-case macro |

## Success Verification

### Count Compilation Failures

```lisp
;; Run self-hosting analysis to count failures
(asdf:load-system :clysm/bootstrap)
(clysm/bootstrap:analyze-compilation-rate)

;; Target: 45 fewer failures than baseline
;; Baseline compilation rate: ~23%
;; Target compilation rate: Higher than baseline
```

### Validate All New Constructs

```lisp
;; Each of these should:
;; 1. Return a valid Wasm module
;; 2. Pass wasm-tools validate

(defun test-all-new-constructs ()
  (let ((forms '((values)
                 (values 1)
                 (values 1 2 3)
                 (the fixnum 42)
                 (the (or null cons) nil)
                 (labels ((f () (g)) (g () (f))) (f))
                 (handler-case 42 (error (e) e)))))
    (dolist (form forms)
      (let ((wasm (compile-to-wasm form)))
        (assert wasm)
        (assert (wasm-valid-p wasm))
        (format t "~&PASS: ~S~%" form)))))
```

## Common Issues

### "Unknown special form" Error
- Check `parse-compound-form` case clause for the form
- Verify symbol comparison is correct (use `eq` or case)

### "Unknown AST type" Error
- Check `compile-to-instructions` etypecase in func-section.lisp
- Add clause for new AST node type

### Wasm Validation Failure
- Run `wasm-tools print output.wasm` to see WAT
- Check stack types match expected types
- Verify all referenced functions/globals exist
