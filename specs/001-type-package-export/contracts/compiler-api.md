# Compiler API Contracts: Type Constant and Package Primitive Export

**Feature Branch**: `001-type-package-export`
**Date**: 2026-01-01

## Overview

This document defines the contracts for the compiler extensions that enable type constant access and package primitive compilation.

---

## 1. Constant Binding Registry

### Interface: `*constant-bindings*`

Global hash-table storing compile-time constant bindings.

**Location**: `src/clysm/compiler/directive.lisp`

```lisp
(defparameter *constant-bindings* (make-hash-table :test 'eq)
  "Maps constant symbols to their compile-time evaluated values.
   Key: symbol (e.g., +TYPE-CONS+)
   Value: evaluated constant value (integer for type indices)")
```

### Function: `register-constant`

```lisp
(defun register-constant (name value)
  "Register a compile-time constant binding.

   Parameters:
     NAME  - Symbol naming the constant
     VALUE - Evaluated constant value (must be self-evaluating)

   Returns:
     VALUE (for chaining)

   Side Effects:
     Updates *constant-bindings* hash-table

   Example:
     (register-constant '+TYPE-CONS+ 2) => 2")
```

### Function: `lookup-constant`

```lisp
(defun lookup-constant (name)
  "Look up a compile-time constant value.

   Parameters:
     NAME - Symbol to look up

   Returns:
     (values VALUE T) if found
     (values NIL NIL) if not found

   Example:
     (lookup-constant '+TYPE-CONS+) => 2, T
     (lookup-constant 'undefined)   => NIL, NIL")
```

---

## 2. DEFCONSTANT Directive Handler

### Function: `handle-defconstant`

```lisp
(defun handle-defconstant (form env)
  "Process DEFCONSTANT form at compile-time.

   Parameters:
     FORM - (defconstant name value-form [doc])
     ENV  - Compilation environment

   Returns:
     :SKIPPED (form handled, no Wasm output needed)

   Behavior:
     1. Extract name, value-form, optional doc from FORM
     2. Evaluate value-form at compile-time using EVAL
     3. Store binding in *constant-bindings*
     4. Return :SKIPPED to indicate successful processing

   Errors:
     Signals error if value-form cannot be evaluated at compile-time

   Example:
     (handle-defconstant '(defconstant +FOO+ 42) env) => :SKIPPED
     ;; Side effect: (+FOO+ . 42) added to *constant-bindings*")
```

---

## 3. Runtime Function Registration

### Function: `register-package-runtime-functions`

Extended to include SYMBOL-PACKAGE*.

```lisp
(defun register-package-runtime-functions ()
  "Register package functions to use runtime library dispatch.

   Registers:
     PACKAGEP*       - arity 1, :$packagep*-rt
     FIND-PACKAGE*   - arity 1, :$find-package*-rt
     INTERN*         - arity nil (variadic), :$intern*-rt
     SYMBOL-PACKAGE* - arity 1, :$symbol-package*-rt

   Called at module load time.

   HyperSpec References:
     - resources/HyperSpec/Body/f_pkgp.htm
     - resources/HyperSpec/Body/f_find_p.htm
     - resources/HyperSpec/Body/f_intern.htm
     - resources/HyperSpec/Body/f_symb_2.htm")
```

---

## 4. Symbol Reference Compilation (Extended)

### Function: `compile-symbol-reference`

Extended to check constant bindings first.

```lisp
(defun compile-symbol-reference (symbol env)
  "Compile a symbol reference to Wasm instructions.

   Parameters:
     SYMBOL - Symbol being referenced
     ENV    - Compilation environment

   Returns:
     List of Wasm instructions

   Resolution Order:
     1. Check *constant-bindings* → emit (i32.const value)
     2. Check local variables → emit (local.get index)
     3. Check global/special variables → emit (global.get name)
     4. Signal unbound variable error

   Example:
     (compile-symbol-reference '+TYPE-CONS+ env)
     => ((i32.const 2))")
```

---

## 5. Package Primitive Wasm Contracts

### PACKAGEP* Wasm Implementation

```wat
;; Input: anyref on stack
;; Output: anyref (T or NIL) on stack

(func $packagep*-rt (param $obj anyref) (result anyref)
  (if (result anyref)
    (ref.test $package (local.get $obj))
    (then (global.get $T))
    (else (global.get $NIL))))
```

**Contract**:
- Input: Single anyref argument
- Output: Global reference T if input is package struct, NIL otherwise
- Side effects: None
- Type safety: Uses ref.test for safe type checking

### SYMBOL-PACKAGE* Wasm Implementation

```wat
;; Input: symbol ref on stack
;; Output: anyref (package or NIL) on stack

(func $symbol-package*-rt (param $sym anyref) (result anyref)
  (if (result anyref)
    (ref.test $symbol (local.get $sym))
    (then
      (struct.get $symbol $package
        (ref.cast $symbol (local.get $sym))))
    (else (global.get $NIL))))
```

**Contract**:
- Input: Single anyref (expected to be symbol)
- Output: Package reference from symbol's package field, or NIL if not a symbol
- Side effects: None
- Type safety: Uses ref.test before ref.cast

---

## 6. Package Export Contract

### Required Exports from `:clysm` Package

The following symbols MUST be exported from the `:clysm` package:

```lisp
;; Type Index Constants (re-exported from clysm/compiler/codegen/gc-types)
#:+type-nil+
#:+type-unbound+
#:+type-cons+
#:+type-symbol+
#:+type-string+
#:+type-closure+
#:+type-instance+
#:+type-standard-class+
#:+type-func-0+
#:+type-func-1+
#:+type-func-2+
#:+type-func-3+
#:+type-func-n+
#:+type-binding-frame+
#:+type-bignum+
#:+type-ratio+
#:+type-float+
#:+type-complex+
#:+type-limb-array+
#:+type-stream+
#:+type-mv-array+
#:+type-slot-vector+
#:+type-keyword-array+
#:+type-closure-array+
#:+type-anyref-array+
#:+type-macro-environment+
#:+type-hash-entry+
#:+type-hash-table+
#:+type-bucket-array+
#:+type-mdarray+

;; Package Primitives
#:packagep*
#:find-package*
#:intern*
#:symbol-package*
```

---

## Verification

### Contract Tests

Each contract should be verified by tests:

1. **Constant binding**: Test that DEFCONSTANT forms populate `*constant-bindings*`
2. **Constant folding**: Test that constant references compile to `i32.const`
3. **Package primitives**: Test that function calls compile to runtime dispatches
4. **Wasm validation**: Test that generated Wasm passes `wasm-tools validate`
5. **Error patterns**: Test that P846 and P951 errors are eliminated
