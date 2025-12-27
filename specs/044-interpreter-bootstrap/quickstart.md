# Quickstart: Interpreter Bootstrap Strategy

**Feature**: 044-interpreter-bootstrap
**Date**: 2025-12-28

## Prerequisites

- SBCL 2.4+ (for initial bootstrap only)
- wasmtime (for Wasm execution)
- wasm-tools (for Wasm validation)
- Nix (recommended for reproducible environment)

```bash
# Enter Nix development shell
nix develop

# Verify tools
sbcl --version     # 2.4.0+
wasmtime --version
wasm-tools --version
```

## Quick Start

### 1. Load the Extended Interpreter

```lisp
;; In SBCL REPL
(asdf:load-system :clysm)
(in-package :clysm/eval/interpreter)

;; Create interpreter environment
(defvar *env* (make-interpreter-env))
```

### 2. Basic Interpretation

```lisp
;; Interpret simple forms
(interpret '(+ 1 2 3))           ; → 6
(interpret '(if t "yes" "no"))   ; → "yes"

;; Define and call functions
(interpret '(defun square (x) (* x x)))
(interpret '(square 5))          ; → 25

;; Define and use macros
(interpret '(defmacro when (test &body body)
              `(if ,test (progn ,@body))))
(interpret '(when t 1 2 3))      ; → 3
```

### 3. Load Compiler Modules

```lisp
;; Load a single module
(interpret-file #p"src/clysm/backend/leb128.lisp")

;; Load all compiler modules in order
(dolist (module clysm-validation:*compilation-order*)
  (interpret-file (merge-pathnames module (asdf:system-source-directory :clysm))))
```

### 4. Generate Stage 0 via Interpreter

```lisp
(in-package :clysm/bootstrap)

;; Generate Stage 0 Wasm binary
(let ((result (generate-stage0-via-interpreter
               :output-path #p"dist/clysm-stage0-interp.wasm"
               :verbose t)))
  (if (bootstrap-result-success result)
      (format t "Stage 0 generated: ~D forms compiled~%"
              (bootstrap-result-forms-compiled result))
      (format t "Bootstrap failed: ~A~%"
              (bootstrap-result-errors result))))
```

### 5. Verify Stage 0 Binary

```bash
# Validate Wasm binary
wasm-tools validate dist/clysm-stage0-interp.wasm

# Test basic compilation
node host-shim/verify-stage0.js dist/clysm-stage0-interp.wasm "(+ 1 2)"
```

### 6. Run Fixed-Point Verification

```bash
# Full verification script
./scripts/verify-fixpoint-interp.sh

# Or programmatically
```

```lisp
(in-package :clysm/bootstrap)

(multiple-value-bind (status report)
    (run-full-bootstrap
     :stage0-path #p"dist/clysm-stage0-interp.wasm"
     :output-dir #p"dist/"
     :json-output nil)
  (case status
    (:achieved (format t "Fixed-point achieved!~%"))
    (:not-achieved (format t "Stage 1 ≠ Stage 2~%"))
    (t (format t "Error: ~A~%" report))))
```

## Testing

### Run Interpreter Unit Tests

```bash
# All interpreter tests
sbcl --eval "(asdf:test-system :clysm/test)" --quit

# Specific test file
sbcl --eval "(rove:run :clysm/test/unit/interpreter/defun-test)" --quit
```

### Test Individual Features

```lisp
;; Test defstruct
(interpret '(defstruct point x y))
(interpret '(let ((p (make-point :x 1 :y 2)))
              (point-x p)))  ; → 1

;; Test loop
(interpret '(loop for i from 1 to 5 collect (* i i)))
; → (1 4 9 16 25)

;; Test handler-case
(interpret '(handler-case
                (error "test error")
              (error (c) "caught")))
; → "caught"
```

## Troubleshooting

### "Unsupported feature: XXX"

The interpreter encountered a CL feature not yet implemented. Check:
1. Is the feature in the blessed subset? (`docs/blessed-subset.lisp`)
2. If yes, implementation is pending for this feature
3. If no, the compiler source may need modification

### "Unbound variable: XXX"

A variable or function is not defined:
1. Ensure all modules are loaded in order
2. Check for circular dependencies
3. Verify the symbol is exported from its package

### Wasm Validation Fails

The generated binary has structural issues:
1. Check compiler output for errors
2. Run `wasm-tools print output.wasm` to inspect
3. Compare with SBCL-generated Stage 0

### Fixed-Point Not Achieved

Stage 1 ≠ Stage 2 indicates non-determinism:
1. Check for timestamp/random dependencies
2. Verify compilation order is consistent
3. Use `./scripts/diff-stages.sh` to identify differences

## Next Steps

1. **Extend interpreter**: Add missing special forms
2. **Add built-ins**: Implement required 100+ functions
3. **Bootstrap loop**: Iterate until fixed-point achieved
4. **CI integration**: Add to `nix flake check`
