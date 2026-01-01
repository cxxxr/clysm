# Data Model: Compiler Internal Function Consolidation

**Date**: 2026-01-01
**Feature**: 001-compiler-internal-consolidation

## Entity Overview

This feature works with compiler-internal data structures. No persistent storage is involved; all entities exist only during compilation.

## Core Entities

### 1. Compilation Environment (compilation-env)

Tracks lexical scope during compilation. Host version uses `defstruct`; Wasm version uses alist/plist representation.

**Host Definition** (func-section.lisp:10-27):
```lisp
(defstruct (compilation-env (:conc-name cenv-))
  (locals nil :type list)              ; ((name . local-index) ...)
  (local-counter-box nil :type list)   ; (counter) - mutable box
  (local-types-box nil :type list)     ; ((index . type) ...)
  (functions nil :type list)           ; ((name . func-index) ...)
  (captured-vars nil :type list)       ; ((name . closure-slot) ...)
  (local-functions nil :type list)     ; ((name . local-idx) ...)
  (blocks nil :type list)              ; Block labels for return-from
  (tagbody-frames nil :type list)      ; Tagbody frames for go
  (catch-frames nil :type list)        ; Catch handlers
  (unwind-protect-depth 0 :type fixnum) ; Current cleanup depth
  (tail-position-p nil :type boolean)) ; Whether in tail position
```

**Wasm-Compatible Representation**:
```lisp
;; Use cons cells instead of struct
(defun make-env-wasm ()
  (list nil   ; locals
        (list 0)  ; counter box
        nil   ; types
        nil   ; functions
        nil)) ; captured

(defun env-locals (env) (first env))
(defun env-counter-box (env) (second env))
;; ... etc
```

**Validation Rules**:
- Local indices must be unique within environment
- Counter box must be shared across extended environments
- Captured vars reference outer scope bindings

**State Transitions**:
```
Empty → env-add-local → Extended
Extended → env-add-local → More Extended
Extended → extend-env → New scope (shared counter)
Extended → env-add-closure-var → Closure-aware
```

---

### 2. Runtime Function Entry

Maps Lisp function symbols to runtime library implementations.

**Structure** (in *runtime-function-table*):
```lisp
;; Key: symbol (e.g., 'member)
;; Value: (runtime-name . arity-or-nil)
;;   runtime-name: keyword like :$member-rt
;;   arity: nil = variadic, N = fixed arity
```

**Examples**:
```lisp
'member     -> (:$member-rt . nil)    ; variadic (accepts :test :key)
'assoc      -> (:$assoc-rt . nil)
'princ      -> (:$princ-rt . 1)       ; fixed 1 arg
'terpri     -> (:$terpri-rt . nil)    ; 0-1 args
```

**Validation Rules**:
- Runtime name must match exported function in runtime library
- If arity specified, compile-time arg count must match
- Symbol must not also have compile-* inline generator (either/or)

---

### 3. FFI Function Declaration

Describes imported host functions for package operations.

**Structure** (from ffi/types.lisp):
```lisp
(defstruct foreign-function-decl
  (lisp-name nil :type symbol)        ; e.g., %host-find-package
  (module-name nil :type string)      ; e.g., "host"
  (field-name nil :type string)       ; e.g., "findPackage"
  (param-types nil :type list)        ; e.g., (:string)
  (result-type nil :type marshal-type)) ; e.g., :anyref
```

**Package Operation FFI Declarations**:

| Lisp Name | Module | Field | Params | Result |
|-----------|--------|-------|--------|--------|
| %host-find-package | "host" | "findPackage" | (:string) | :anyref |
| %host-intern | "host" | "internSymbol" | (:string :anyref) | :anyref |
| %host-packagep | "host" | "isPackage" | (:anyref) | :boolean |

---

### 4. WasmGC Type Index

Integer reference to type definition in module's type section.

**Existing Indices** (gc-types.lisp):
```
 0: +type-nil+           - NIL singleton
 1: +type-unbound+       - UNBOUND marker
 2: +type-cons+          - (car . cdr)
 3: +type-symbol+        - (name value function plist)
 4: +type-string+        - UTF-8 string (i8 array)
 5: +type-closure+       - (code_0 code_1 code_2 code_N env)
 6: +type-instance+      - CLOS instance
 7: +type-standard-class+ - CLOS class
...
28: +type-mdarray+       - Multidimensional array
```

**Validation Rules**:
- All type indices must be declared before use
- New types would start at index 29 (not implemented in this feature)
- Struct field access requires matching type index and field number

---

### 5. AST Node Types

Input to compile-to-instructions dispatcher.

**Core AST Types** (compiler/ast.lisp):
```lisp
ast-literal      ; Constants: numbers, strings, symbols
ast-var-ref      ; Variable references
ast-call         ; Function calls
ast-if           ; Conditionals
ast-let          ; Let bindings
ast-lambda       ; Lambda expressions
ast-progn        ; Sequences
ast-block        ; Named blocks
ast-return-from  ; Non-local returns
ast-tagbody      ; Tags and go
ast-go           ; Go to tag
ast-catch        ; Exception handlers
ast-throw        ; Raise exception
ast-unwind-protect ; Cleanup forms
ast-multiple-value-call ; MV dispatch
ast-values       ; Return multiple values
```

**Dispatch Pattern**:
```lisp
(defun compile-to-instructions (ast env)
  (etypecase ast
    (ast-literal (compile-literal ast env))
    (ast-var-ref (compile-var-ref ast env))
    (ast-call    (compile-call ast env))
    ;; ... 15+ more cases
    ))
```

---

## Relationships

```
┌─────────────────────┐
│   AST Node          │
└─────────┬───────────┘
          │ dispatch to
          ▼
┌─────────────────────┐     uses     ┌──────────────────────┐
│compile-to-instructions│───────────►│ Compilation Env      │
└─────────┬───────────┘              └──────────────────────┘
          │ checks
          ▼
┌─────────────────────┐
│ *runtime-function-  │     maps to  ┌──────────────────────┐
│  table*             │─────────────►│ Runtime Function     │
└─────────┬───────────┘              └──────────────────────┘
          │ or emits
          ▼
┌─────────────────────┐
│ Wasm Instructions   │
│ (using Type Index)  │
└─────────────────────┘
```

## Data Flows

### Compilation Flow

```
Source Form
    │
    ▼ parse
AST Node
    │
    ▼ compile-to-instructions
Instructions + Env Updates
    │
    ▼ emit-function
Wasm Bytecode
```

### Runtime Dispatch Flow

```
(member item list :test #'equal)
    │
    ▼ compile-call
Check: (runtime-function-p 'member) → T
    │
    ▼ compile-runtime-call
1. Compile args: item, list
2. Compile keyword args if any
3. Emit: (:call :$member-rt)
    │
    ▼ runtime execution
member-rt in list-runtime.lisp executes
```

### FFI Call Flow

```
(find-package* "CL-USER")
    │
    ▼ compile-call
1. Compile arg: "CL-USER"
2. Emit: (:call ,$host-find-package)
    │
    ▼ Wasm execution
1. Marshal string to host
2. Call host.findPackage(str)
3. Return package ref (or nil)
```
