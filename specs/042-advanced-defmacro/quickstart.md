# Quickstart: Advanced Defmacro and Compile-Time Macro Expansion

**Feature**: 042-advanced-defmacro | **Date**: 2025-12-28

## Overview

This feature extends the Clysm macro system with `&whole` and `&environment` support, enabling full ANSI CL macro compliance and runtime macro expansion in compiled Wasm code.

## Prerequisites

- Clysm compiler built and functional
- Feature 016 (Macro System) complete
- Feature 017 (Eval/JIT) complete
- Feature 025 (Multiple Values) complete

## Quick Verification

After implementation, verify with these tests:

```bash
# Run unit tests
cd /home/user/src/clysm-workbench/clysm3
nix develop
rove tests/unit/macro/whole-test.lisp
rove tests/unit/macro/environment-test.lisp
rove tests/unit/macro/macro-function-test.lisp

# Run contract tests (Wasm validation)
rove tests/contract/macro-wasm-test.lisp

# Run integration tests
rove tests/integration/macro-ansi-test.lisp

# Verify all tests pass
nix flake check
```

## Usage Examples

### Example 1: Using &whole

```lisp
;; Define a macro that captures the whole form for error reporting
(defmacro checked-add (&whole form a b)
  `(let ((x ,a) (y ,b))
     (unless (and (numberp x) (numberp y))
       (error "Invalid arguments to CHECKED-ADD: ~S" ',form))
     (+ x y)))

;; Usage
(checked-add 1 2)      ; => 3
(checked-add 1 "two")  ; => Error: Invalid arguments to CHECKED-ADD: (CHECKED-ADD 1 "two")
```

### Example 2: Using &environment

```lisp
;; Define a macro that checks for local macro bindings
(defmacro expand-if-macro (form &environment env)
  (if (macro-function (car form) env)
      (macroexpand-1 form env)
      form))

;; Usage with macrolet
(macrolet ((local-double (x) `(* 2 ,x)))
  (expand-if-macro (local-double 5)))  ; => (* 2 5)
```

### Example 3: Runtime macroexpand

```lisp
;; Define a simple macro
(defmacro triple (x)
  `(* 3 ,x))

;; Use macroexpand at runtime in compiled code
(defun show-expansion (form)
  (multiple-value-bind (expanded expanded-p)
      (macroexpand-1 form)
    (format t "Form: ~S~%Expanded: ~S~%Did expand: ~S~%"
            form expanded expanded-p)))

;; Call
(show-expansion '(triple 10))
;; Output:
;; Form: (TRIPLE 10)
;; Expanded: (* 3 10)
;; Did expand: T
```

### Example 4: macro-function and setf

```lisp
;; Get a macro's expander function
(macro-function 'when*)  ; => #<FUNCTION ...>

;; Define a macro programmatically
(setf (macro-function 'my-incf)
      (lambda (form &optional env)
        (declare (ignore env))
        `(setf ,(second form) (1+ ,(second form)))))

;; Use the new macro
(let ((x 5))
  (my-incf x)
  x)  ; => 6
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/transform/macro.lisp` | Core macro system with &whole/&environment |
| `src/clysm/compiler/codegen/gc-types.lisp` | WasmGC type for macro-environment |
| `src/clysm/compiler/codegen/func-section.lisp` | Runtime macroexpand compilation |
| `src/clysm/runtime/macro-runtime.lisp` | Runtime support functions |

## API Summary

### Macro Lambda-List Parameters

| Parameter | Position | Description |
|-----------|----------|-------------|
| `&whole var` | First only | Binds VAR to complete macro call form |
| `&environment var` | Anywhere | Binds VAR to lexical environment |
| `&optional` | After required | Optional parameters with defaults |
| `&rest`/`&body` | After optional | Collect remaining arguments |
| `&key` | After rest | Keyword parameters |
| `&allow-other-keys` | After key | Allow unrecognized keywords |

### Functions

| Function | Signature | Returns |
|----------|-----------|---------|
| `macro-function` | `(symbol &optional env)` | Function or NIL |
| `(setf macro-function)` | `(fn symbol &optional env)` | Function |
| `macroexpand-1` | `(form &optional env)` | Form, expanded-p |
| `macroexpand` | `(form &optional env)` | Form, expanded-p |

## Error Conditions

| Condition | When Signaled |
|-----------|---------------|
| `macro-lambda-list-malformed` | &whole not first, missing variable |
| `macro-expansion-depth-exceeded` | >1000 expansion steps (circular) |

## Testing Strategy

### Unit Tests
- Lambda-list parsing with &whole/&environment
- Environment creation and lookup
- macro-function getter/setter

### Contract Tests
- WasmGC type definition validation
- Runtime macroexpand Wasm IR generation

### Integration Tests
- Self-compilation of Clysm's 27 defmacro forms
- Expansion equivalence with SBCL
- macrolet + &environment interaction

## Performance Targets

| Operation | Target |
|-----------|--------|
| Single macro expansion | <1ms |
| 100-step expansion chain | <10ms |
| macro-function lookup | <0.1ms |

## Self-Hosting Verification

After implementation, verify self-compilation:

```bash
# Count successful defmacro compilations
sbcl --load build/bootstrap.lisp 2>&1 | grep "defmacro.*compiled"

# Expected: 27 defmacro forms compile successfully
```
