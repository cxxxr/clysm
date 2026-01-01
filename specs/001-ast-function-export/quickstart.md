# Quickstart: AST Function Export System

**Feature**: 001-ast-function-export
**Time estimate**: 30-45 minutes

## Prerequisites

- SBCL 2.4+ with clysm loaded
- Familiarity with `*runtime-function-table*` mechanism
- Access to `tests/unit/` directory

## Step 1: Export Missing Functions (package.lisp)

Add imports and exports for `make-ast-literal` and `get-numeric-value`:

```lisp
;; In (:import-from #:clysm/compiler/ast ...) section, add:
#:make-ast-literal
#:get-numeric-value

;; In (:export ...) section, add:
#:make-ast-literal
#:get-numeric-value
```

**Location**: `src/clysm/package.lisp`, around lines 1417-1420 and 1441-1446

## Step 2: Create Registration Function (func-section.lisp)

Add after existing registration functions (around line 203):

```lisp
(defun register-ast-runtime-functions ()
  "Register AST manipulation functions for runtime dispatch."
  (register-runtime-function 'clysm:compile-to-instructions :$compile-to-instructions-rt 2)
  (register-runtime-function 'clysm:make-wasm-struct-type :$make-wasm-struct-type-rt nil)
  (register-runtime-function 'clysm:wasm-struct-type-p :$wasm-struct-type-p-rt 1)
  (register-runtime-function 'clysm:wasm-struct-type-fields :$wasm-struct-type-fields-rt 1)
  (register-runtime-function 'clysm:make-ast-literal :$make-ast-literal-rt nil)
  (register-runtime-function 'clysm:ast-literal-value :$ast-literal-value-rt 1)
  (register-runtime-function 'clysm:ast-literal-p :$ast-literal-p-rt 1)
  (register-runtime-function 'clysm:get-numeric-value :$get-numeric-value-rt 1))
```

Call it at module load time (around line 219):

```lisp
(register-ast-runtime-functions)
```

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

## Step 3: Add Unit Tests

Create `tests/unit/ast-export-test.lisp`:

```lisp
(defpackage #:clysm/tests/ast-export
  (:use #:cl #:rove))
(in-package #:clysm/tests/ast-export)

(deftest ast-functions-exported
  "Test that AST functions are exported from clysm package."
  (dolist (sym '(compile-to-instructions
                 make-wasm-struct-type
                 wasm-struct-type-p
                 wasm-struct-type-fields
                 make-ast-literal
                 ast-literal-value
                 ast-literal-p
                 get-numeric-value))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name sym) :clysm)
      (ok symbol (format nil "~A should exist" sym))
      (ok (eq status :external) (format nil "~A should be external" sym)))))

(deftest ast-functions-registered
  "Test that AST functions are registered in runtime table."
  (dolist (entry '((clysm:compile-to-instructions . 2)
                   (clysm:make-wasm-struct-type . nil)
                   (clysm:wasm-struct-type-p . 1)
                   (clysm:wasm-struct-type-fields . 1)
                   (clysm:make-ast-literal . nil)
                   (clysm:ast-literal-value . 1)
                   (clysm:ast-literal-p . 1)
                   (clysm:get-numeric-value . 1)))
    (let* ((sym (car entry))
           (expected-arity (cdr entry))
           (table-entry (clysm/compiler/codegen/func-section::runtime-function-p sym)))
      (ok table-entry (format nil "~A should be in runtime table" sym))
      (when table-entry
        (ok (eql (cdr table-entry) expected-arity)
            (format nil "~A arity should be ~A" sym expected-arity))))))
```

## Step 4: Verify

```bash
# Run unit tests
sbcl --eval "(asdf:test-system :clysm)"

# Generate Stage 1
sbcl --load build/stage1-complete.lisp

# Validate Wasm
wasm-tools validate dist/clysm-stage1.wasm

# Check error patterns eliminated
grep -E "P944|P321|P543|P106" dist/stage1-report.json
```

## Expected Results

- Unit tests pass (100%)
- Stage 1 Wasm validates successfully
- Error patterns P944, P321, P543, P106: 0 occurrences
- Compilation rate: 25%+ (up from 19%)

## Troubleshooting

**Symbol not found**: Ensure package.lisp is reloaded after changes

**Registration failed**: Check symbol is in clysm package (use `clysm:symbol-name`)

**Arity mismatch**: Verify function signature matches registered arity
