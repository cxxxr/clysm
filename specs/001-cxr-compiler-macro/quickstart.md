# Quickstart: CXR Compiler Macro Consolidation

**Branch**: `001-cxr-compiler-macro`
**Date**: 2026-01-03

## Overview

This guide covers implementing the `define-cxr-compiler` macro to consolidate 12 cXr compiler functions.

## Prerequisites

- SBCL 2.4+ installed
- clysm project loaded: `(asdf:load-system :clysm)`
- wasm-tools available for validation

## Step 1: Create Unit Tests (TDD - Red Phase)

Create `tests/unit/cxr-macro-test.lisp`:

```lisp
(defpackage :clysm/tests/cxr-macro
  (:use :cl :rove))
(in-package :clysm/tests/cxr-macro)

(deftest define-cxr-compiler-expansion
  ;; Test that macro expands to correct defun form
  (let ((expansion (macroexpand-1 '(define-cxr-compiler caddr "dda"))))
    (ok (eq (first expansion) 'defun))
    (ok (eq (second expansion) 'compile-caddr))
    (ok (equal (third expansion) '(args env)))))

(deftest define-cxr-compiler-rejects-empty-string
  ;; Test validation rejects empty operation string
  (ok (signals error (macroexpand-1 '(define-cxr-compiler foo "")))))

(deftest define-cxr-compiler-rejects-invalid-chars
  ;; Test validation rejects invalid characters
  (ok (signals error (macroexpand-1 '(define-cxr-compiler foo "xyz")))))
```

Run tests (expect failures):
```bash
sbcl --eval "(asdf:test-system :clysm)"
```

## Step 2: Implement the Macro (TDD - Green Phase)

Add to `src/clysm/compiler/codegen/func-section.lisp` before the cXr section:

```lisp
;;; cXXr Accessor Macro (001-cxr-compiler-macro)
;;; ============================================================

(defun ops-to-expansion (name ops)
  "Convert operation string to human-readable expansion.
   Example: 'dda' → '(car (cdr (cdr x)))'"
  (declare (ignore name))
  (let ((result "x"))
    (loop for c across (reverse ops)
          do (setf result (format nil "(~A ~A)"
                                  (if (char= c #\a) "car" "cdr")
                                  result)))
    result))

(defmacro define-cxr-compiler (name ops)
  "Generate a compile-cXXr function for the given operation sequence.
   NAME: Symbol like CAAR, CADR, CADDR
   OPS: String like \"aa\", \"da\", \"dda\" (a=car, d=cdr)
   Feature: 001-cxr-compiler-macro"
  (check-type ops string)
  (assert (plusp (length ops)) (ops)
          "Operation string must be non-empty")
  (assert (every (lambda (c) (member c '(#\a #\d))) ops) (ops)
          "Operation string must contain only 'a' and 'd'")
  (let ((func-name (intern (format nil "COMPILE-~A" name)
                           (symbol-package 'compile-cxr-chain)))
        (docstring (format nil "Compile (~(~A~) x) = ~A.~%   Feature: 001-cxr-compiler-macro"
                           name (ops-to-expansion name ops))))
    `(defun ,func-name (args env)
       ,docstring
       (compile-cxr-chain ,ops args env))))
```

Run tests (expect pass):
```bash
sbcl --eval "(asdf:test-system :clysm)"
```

## Step 3: Replace Function Definitions (TDD - Refactor Phase)

Replace the 12 defun forms with macro invocations:

```lisp
;;; cXXr Accessors (001-cxr-compiler-macro)
;;; ============================================================

;; 2-level accessors
(define-cxr-compiler caar "aa")
(define-cxr-compiler cadr "da")
(define-cxr-compiler cdar "ad")
(define-cxr-compiler cddr "dd")

;; 3-level accessors
(define-cxr-compiler caaar "aaa")
(define-cxr-compiler caadr "daa")
(define-cxr-compiler cadar "ada")
(define-cxr-compiler caddr "dda")
(define-cxr-compiler cdaar "aad")
(define-cxr-compiler cdadr "dad")
(define-cxr-compiler cddar "add")
(define-cxr-compiler cdddr "ddd")
```

## Step 4: Verify

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run Stage 1 compilation
sbcl --load build/stage1-complete.lisp

# Validate Wasm output
wasm-tools validate dist/clysm-stage1.wasm

# Check line count reduction
wc -l src/clysm/compiler/codegen/func-section.lisp
```

## Expected Results

- All tests pass
- Stage 1 compilation succeeds
- wasm-tools validate exits with code 0
- cXr section reduced from ~60 lines to ~20 lines

## Troubleshooting

**Error: Package issue with function name**
- Ensure `intern` uses correct package: `(symbol-package 'compile-cxr-chain)`

**Error: Docstring format wrong**
- Check `ops-to-expansion` produces correct expansion string

**Error: Tests fail after refactoring**
- Compare macroexpansion with original defun forms character-by-character
