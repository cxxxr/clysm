# Data Model: Control Structure Extensions

**Branch**: `001-control-structure-extension` | **Date**: 2025-12-30

## Overview

This document defines AST extensions and Wasm codegen patterns for control structure compilation.

## AST Node Extensions

### Existing Nodes (Verify Dispatch)

These nodes already exist and should be verified for correct dispatch:

#### `ast-values`

**Location**: `src/clysm/compiler/ast.lisp:462-467`

```lisp
(defstruct (ast-values (:include ast-node))
  "Multiple value return"
  (values nil :type list))  ; List of ast-node expressions
```

**Parser**: Direct construction in `parse-compound-form` at case `values`

#### `ast-flet` / `ast-labels`

**Location**: `src/clysm/compiler/ast.lisp:424-430`

```lisp
(defstruct (ast-flet (:include ast-node))
  "Local non-recursive function bindings"
  (bindings nil :type list)   ; List of (name lambda-ast) pairs
  (body nil :type list))      ; Body forms

(defstruct (ast-labels (:include ast-node))
  "Local recursive function bindings"
  (bindings nil :type list)   ; List of (name lambda-ast) pairs
  (body nil :type list))      ; Body forms
```

### New Node: `ast-handler-case`

**Purpose**: Represent handler-case for direct Wasm compilation

```lisp
(defstruct (ast-handler-case (:include ast-node))
  "Exception handling with typed handlers"
  (expression nil :type ast-node)     ; Protected expression
  (handlers nil :type list))          ; List of handler-clause structures

(defstruct handler-clause
  "A single handler clause in handler-case"
  (type nil :type t)                  ; Condition type specifier
  (var nil :type (or null symbol))    ; Binding variable (or nil if unused)
  (body nil :type list))              ; Handler body forms
```

**Parser**: New `parse-handler-case-form` function

```lisp
(defun parse-handler-case-form (args)
  "Parse (handler-case expression {(type ([var]) . body)}*)"
  (when (null args)
    (error "HANDLER-CASE requires at least an expression"))
  (make-ast-handler-case
   :expression (parse-expr (first args))
   :handlers (mapcar #'parse-handler-clause (rest args))))

(defun parse-handler-clause (clause)
  "Parse a single (type ([var]) . body) clause"
  (destructuring-bind (type lambda-list &body body) clause
    (make-handler-clause
     :type type
     :var (and lambda-list (first lambda-list))
     :body (mapcar #'parse-expr body))))
```

### `the` Form (No Node Needed)

The `the` special form is a pass-through - it evaluates to the AST of its second argument. No dedicated `ast-the` node is needed.

**Current Implementation** (`ast.lisp:825-830`):
```lisp
(the (if (>= (length args) 2)
         (parse-expr (second args))
         (error "THE requires type and form")))
```

## Wasm Type Extensions

### Exception Tag Definition

Add to type section (one-time setup):

```wat
;; Exception tag for all Lisp conditions
(tag $lisp-error (param (ref $condition)))
```

Where `$condition` is the condition object type (extends `$instance` for CLOS conditions).

### Global Indices Reference

| Index | Name | Type | Purpose |
|-------|------|------|---------|
| 2 | mv-count | i32 | Number of multiple values |
| 3 | mv-buffer | ref $mv_array | Secondary values storage |

## Codegen Patterns

### `values` Compilation Pattern

```wat
;; (values) - zero values
(global.set $mv-count (i32.const 0))
(global.get $nil)

;; (values x) - single value
<compile x>
(global.set $mv-count (i32.const 1))

;; (values x y z) - multiple values
<compile z>
(global.get $mv-buffer)
(i32.const 1)
<compile y>
(array.set $mv_array)
(global.get $mv-buffer)
(i32.const 0)
(array.set $mv_array)
(global.set $mv-count (i32.const 3))
<compile x>  ;; Primary value stays on stack
```

### `handler-case` Compilation Pattern

```wat
;; (handler-case expression (error-type (e) body))
(block $handler-result (result anyref)
  (block $catch-handler (param exnref) (result anyref)
    (try_table (result anyref)
      (catch $lisp-error $catch-handler)
      ;; Protected expression
      <compile expression>
    )
    (br $handler-result)
  )
  ;; Handler dispatch block
  ;; Stack has: exnref
  (local.set $exception-ref)
  (local.get $exception-ref)
  (call $get-condition-from-exnref)  ;; Extract condition object
  (local.set $condition)
  ;; Type dispatch
  (local.get $condition)
  (call $condition-type-p)
  (if (param anyref) (result anyref)
    (then
      ;; Handler body with e bound to condition
      (local.get $condition)
      (local.set $e)
      <compile body>
    )
    (else
      ;; No match - rethrow
      (local.get $exception-ref)
      (throw_ref)
    )
  )
)
```

### `labels` Forward Reference Pattern

```wat
;; Two-phase closure creation for mutual recursion
;; Phase 1: Allocate closure structs with null code pointers
(ref.null $func_N)
(ref.null $func_0)
(ref.null $func_1)
(ref.null $func_2)
(global.get $unbound)  ;; Placeholder env
(struct.new $closure)
(local.set $closure-a)

(ref.null $func_N)
(ref.null $func_0)
(ref.null $func_1)
(ref.null $func_2)
(global.get $unbound)  ;; Placeholder env
(struct.new $closure)
(local.set $closure-b)

;; Phase 2: Create environment with both closures
(local.get $closure-a)
(local.get $closure-b)
(struct.new $labels-env)
(local.set $env)

;; Update closure environments
(local.get $closure-a)
(local.get $env)
(struct.set $closure $env)

(local.get $closure-b)
(local.get $env)
(struct.set $closure $env)

;; Phase 3: Set actual function pointers
(local.get $closure-a)
(ref.func $func-a-impl)
(struct.set $closure $code_N)

(local.get $closure-b)
(ref.func $func-b-impl)
(struct.set $closure $code_N)

;; Now closures can call each other via env lookup
```

## Validation Rules

### AST Validation

1. `ast-values`: `values` list may be empty (zero values)
2. `ast-handler-case`: At least one handler clause expected (but zero is valid - acts as progn)
3. `handler-clause`: `var` may be nil if condition not needed
4. `ast-labels`: All referenced local functions must be defined in same labels form

### Wasm Output Validation

All generated Wasm must pass:
```bash
wasm-tools validate output.wasm
```

Specific checks:
1. Exception tag `$lisp-error` declared in tag section
2. `try_table` result type matches handler block result type
3. All function references resolve within module
4. Multiple value buffer access stays within array bounds

## State Transitions

### Exception Flow

```
Normal execution → exception thrown → catch by try_table → handler dispatch
                                   → no catch → propagate to outer handler
                                             → no outer → terminate
```

### Multiple Values Flow

```
(values a b c) → mv-count=3, mv-buffer[0]=c, mv-buffer[1]=b, stack=a
             → consumer reads mv-count
             → extracts secondary values from mv-buffer
             → primary value from stack
```
