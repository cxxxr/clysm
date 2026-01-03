# Quickstart: Primitive Dispatch Table

**Branch**: `001-primitive-dispatch-table`
**Date**: 2026-01-03

## Overview

This feature replaces the monolithic 248-branch case statement in `compile-primitive-call` with hash-table driven dispatch. After implementation, adding new primitives requires only a single registration call.

## Quick Setup

```bash
# Enter development environment
nix develop

# Run tests to verify baseline
sbcl --eval "(asdf:test-system :clysm)"
```

## Registering a New Primitive

### Basic Registration

```lisp
;; In primitive-registry.lisp or your feature file

(register-primitive-compiler 'my-new-op #'compile-my-new-op
  :arity 2)
```

### With String Lookup (for cross-package symbols)

```lisp
(register-primitive-compiler '%setf-my-slot #'compile-setf-my-slot
  :arity 2
  :string-name "%SETF-MY-SLOT")
```

### With Optional Flags

```lisp
(register-primitive-compiler 'pure-fn #'compile-pure-fn
  :arity 1
  :flags '(:pure t :foldable t))
```

## Implementing a Primitive Compiler

Every primitive compiler has the same signature:

```lisp
(defun compile-my-new-op (args env)
  "Compile a call to MY-NEW-OP.
   ARGS: List of argument forms
   ENV: Compilation environment
   Returns: List of Wasm instructions"
  (declare (type list args)
           (type compilation-env env))
  ;; Implementation here
  (with-instruction-collector
    ;; Compile arguments
    (dolist (arg args)
      (emit* (compile-to-instructions arg env)))
    ;; Emit operation
    (emit :i32.add)))  ; example
```

## Querying Registrations

```lisp
;; Check if primitive is registered
(primitive-registered-p 'cons)  ; => T

;; Get the entry
(primitive-compiler-entry 'cons)
;; => #S(PRIMITIVE-ENTRY :COMPILER-FN #<FUNCTION COMPILE-CONS> ...)

;; List all registered primitives
(list-registered-primitives)
;; => (+ - * / CONS CAR CDR ...)

;; List only string-table entries
(list-registered-primitives :table :string)
;; => ("%SETF-AREF" "%SETF-SVREF" ...)
```

## Testing a New Primitive

### Unit Test

```lisp
;; tests/unit/primitive-dispatch-test.lisp

(deftest my-new-op-registration
  (testing "my-new-op is registered"
    (ok (primitive-registered-p 'my-new-op))
    (ok (= 2 (primitive-entry-arity
               (primitive-compiler-entry 'my-new-op))))))
```

### Contract Test (Wasm Output)

```lisp
;; tests/contract/primitive-dispatch-wasm-test.lisp

(deftest my-new-op-wasm-output
  (testing "my-new-op generates valid Wasm"
    (let ((wasm (compile-to-wasm '(my-new-op 1 2))))
      (ok (wasm-validates-p wasm))
      (ok (contains-instruction-p wasm :i32.add)))))
```

## Development Workflow

1. **Write failing test** (TDD)
   ```lisp
   (deftest my-primitive-test
     (ok (equal '(expected) (compile-primitive-call 'my-op '(1 2) env))))
   ```

2. **Implement compiler function**
   ```lisp
   (defun compile-my-op (args env)
     ...)
   ```

3. **Register primitive**
   ```lisp
   (register-primitive-compiler 'my-op #'compile-my-op :arity 2)
   ```

4. **Verify test passes**
   ```bash
   sbcl --eval "(asdf:test-system :clysm)"
   ```

5. **Validate Wasm output**
   ```bash
   wasm-tools validate dist/test-output.wasm
   ```

## Migration Notes

During migration from the case statement:

1. Both dispatch paths work in parallel
2. Hash-table lookup is tried first
3. Case statement is fallback (gradually emptied)
4. Monitor for regressions with existing test suite

## Troubleshooting

### Primitive not found

```lisp
;; Check registration
(primitive-registered-p 'my-op)  ; Should be T

;; Check symbol identity (common issue!)
(eq 'my-op (find-symbol "MY-OP" :clysm))
```

### Wrong package

Symbols must be in the expected package. Use string-name for cross-package matching:

```lisp
;; Wrong: Different symbol identity
(register-primitive-compiler 'other-package:my-op ...)

;; Right: Use string lookup
(register-primitive-compiler 'my-op ...
  :string-name "MY-OP")
```

### Arity mismatch

If arity is specified, it's enforced:

```lisp
;; This will work
(my-op 1 2)  ; arity 2

;; This will error at compile time
(my-op 1)    ; wrong number of arguments
```

## File Locations

| Purpose | File |
|---------|------|
| Dispatch tables & API | `src/clysm/compiler/codegen/primitive-dispatch.lisp` |
| All registrations | `src/clysm/compiler/codegen/primitive-registry.lisp` |
| Unit tests | `tests/unit/primitive-dispatch-test.lisp` |
| Contract tests | `tests/contract/primitive-dispatch-wasm-test.lisp` |
