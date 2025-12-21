# Data Model: Clysm - WebAssembly GC Common Lisp Compiler

**Date**: 2025-12-21
**Plan Reference**: [plan.md](./plan.md)
**Constitution**: v1.0.0

## Overview

Clysmにおけるデータモデル定義。Common LispオブジェクトのWasmGC型表現と、
コンパイラ内部データ構造を定義する。

---

## 1. WasmGC Type System (Runtime Objects)

### 1.1 Type Hierarchy

```
anyref (universal root)
├── i31ref (Fixnum)
├── (ref $nil) (NIL singleton)
├── (ref $unbound) (UNBOUND sentinel)
├── (ref $cons) (Cons cell)
├── (ref $symbol) (Symbol)
├── (ref $closure) (Function/Closure)
├── (ref $string) (String)
├── (ref $vector) (Simple vector)
├── (ref $instance) (CLOS instance)
└── (ref $standard-class) (Class metaobject)
```

### 1.2 Fixnum (i31ref)

**WasmGC Type**: `i31ref`
**Range**: -2^30 to 2^30 - 1 (31-bit signed)

```wat
;; Fixnum creation
(ref.i31 (i32.const 42))

;; Fixnum extraction
(i31.get_s (local.get $fixnum))  ;; signed
(i31.get_u (local.get $fixnum))  ;; unsigned
```

**Operations**:
| Operation | Implementation |
|-----------|----------------|
| Addition | `i32.add` on extracted values |
| Subtraction | `i32.sub` |
| Multiplication | `i32.mul` (overflow check needed) |
| Division | `i32.div_s` |
| Comparison | `i32.lt_s`, `i32.gt_s`, etc. |

### 1.3 NIL Singleton

**WasmGC Type**: `(ref $nil)`
**Constitution Ref**: II (NIL must NOT be Wasm null)

```wat
(type $nil (struct))

(global $NIL (ref $nil)
  (struct.new $nil))
```

**Properties**:
- Singleton object (pointer equality)
- Acts as: symbol, empty list, false
- Check: `(ref.eq (local.get $x) (global.get $NIL))`

### 1.4 UNBOUND Sentinel

**WasmGC Type**: `(ref $unbound)`
**Constitution Ref**: II (internal sentinel)

```wat
(type $unbound (struct))

(global $UNBOUND (ref $unbound)
  (struct.new $unbound))
```

**Usage**:
- Initial value for symbol's `$value` slot
- Not accessible from Lisp level
- Check for unbound: `(ref.eq slot-value $UNBOUND)`

### 1.5 Cons Cell

**WasmGC Type**: `(ref $cons)`

```wat
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))
```

**Fields**:
| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| $car | anyref | mutable | First element |
| $cdr | anyref | mutable | Rest (usually cons or NIL) |

**Operations**:
```wat
;; cons creation
(struct.new $cons (local.get $car) (local.get $cdr))

;; car access
(struct.get $cons $car (local.get $cell))

;; rplaca
(struct.set $cons $car (local.get $cell) (local.get $new-value))
```

### 1.6 Symbol

**WasmGC Type**: `(ref $symbol)`

```wat
(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))
```

**Fields**:
| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| $name | (ref $string) | immutable | Symbol name |
| $value | anyref | mutable | Symbol value (dynamic scope) |
| $function | anyref | mutable | Function binding |
| $plist | anyref | mutable | Property list |
| $package | anyref | mutable | Home package |

**Initial State**:
- `$value`: `$UNBOUND`
- `$function`: `$UNBOUND`
- `$plist`: `$NIL`
- `$package`: package reference or `$NIL`

### 1.7 Closure

**WasmGC Type**: `(ref $closure)`
**Constitution Ref**: III (arity dispatch structure)

```wat
(type $func_0 (func (param (ref $closure)) (result anyref)))
(type $func_1 (func (param (ref $closure) anyref) (result anyref)))
(type $func_2 (func (param (ref $closure) anyref anyref) (result anyref)))
(type $func_N (func (param (ref $closure) (ref $list)) (result anyref)))

(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))))
```

**Fields**:
| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| $code_0 | (ref null $func_0) | immutable | 0-arg entry point |
| $code_1 | (ref null $func_1) | immutable | 1-arg entry point |
| $code_2 | (ref null $func_2) | immutable | 2-arg entry point |
| $code_N | (ref null $func_N) | immutable | N-arg entry point (list) |
| $env | anyref | mutable | Captured environment |

**Dispatch Logic**:
1. Check arity-specific slot (`$code_0`, `$code_1`, `$code_2`)
2. If null, fall back to `$code_N` with argument list
3. Call via `call_ref`

### 1.8 String

**WasmGC Type**: `(ref $string)`

```wat
(type $string (array (mut i8)))
```

**Encoding**: UTF-8

**Operations**:
```wat
;; string creation
(array.new $string (i32.const 0) (local.get $length))

;; char access
(array.get $string (local.get $str) (local.get $index))

;; string length
(array.len (local.get $str))
```

### 1.9 Vector

**WasmGC Type**: `(ref $vector)`

```wat
(type $vector (array (mut anyref)))
```

**Operations**:
```wat
;; vector creation
(array.new $vector (ref.null any) (local.get $length))

;; element access
(array.get $vector (local.get $vec) (local.get $index))

;; element set
(array.set $vector (local.get $vec) (local.get $index) (local.get $value))
```

### 1.10 CLOS Instance

**WasmGC Type**: `(ref $instance)`

```wat
(type $slot_vector (array (mut anyref)))

(type $instance (struct
  (field $class (ref $standard-class))
  (field $slots (ref $slot_vector))))
```

**Fields**:
| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| $class | (ref $standard-class) | immutable | Class metaobject |
| $slots | (ref $slot_vector) | immutable | Slot values array |

### 1.11 Standard Class

**WasmGC Type**: `(ref $standard-class)`

```wat
(type $standard-class (struct
  (field $name (ref $symbol))
  (field $superclasses (ref $list))
  (field $direct-slots (ref $list))
  (field $effective-slots (ref $list))
  (field $precedence-list (ref $list))))
```

---

## 2. Compiler Internal Data Structures

### 2.1 Abstract Syntax Tree (AST)

```lisp
;; Base AST node
(defstruct ast-node
  (source-location nil :type (or null source-location)))

;; Literal values
(defstruct (ast-literal (:include ast-node))
  (value nil :type t))

;; Variable reference
(defstruct (ast-var-ref (:include ast-node))
  (name nil :type symbol)
  (binding nil :type (or null binding)))

;; Function call
(defstruct (ast-call (:include ast-node))
  (function nil :type ast-node)
  (arguments nil :type list))

;; Lambda expression
(defstruct (ast-lambda (:include ast-node))
  (parameters nil :type list)
  (body nil :type list)
  (free-vars nil :type list))

;; Let binding
(defstruct (ast-let (:include ast-node))
  (bindings nil :type list)  ; ((name . value-ast) ...)
  (body nil :type list))

;; If expression
(defstruct (ast-if (:include ast-node))
  (test nil :type ast-node)
  (then nil :type ast-node)
  (else nil :type (or null ast-node)))

;; Block
(defstruct (ast-block (:include ast-node))
  (name nil :type symbol)
  (body nil :type list))

;; Return-from
(defstruct (ast-return-from (:include ast-node))
  (block-name nil :type symbol)
  (value nil :type (or null ast-node)))
```

### 2.2 Binding Information

```lisp
(defstruct binding
  (name nil :type symbol)
  (kind nil :type (member :lexical :special :function :macro))
  (index nil :type (or null fixnum))  ; stack/env index
  (mutable-p nil :type boolean))

(defstruct lexical-env
  (bindings nil :type list)
  (parent nil :type (or null lexical-env)))
```

### 2.3 Wasm IR (Intermediate Representation)

```lisp
;; Wasm instruction
(defstruct wasm-instr
  (opcode nil :type keyword)
  (operands nil :type list))

;; Wasm function
(defstruct wasm-func
  (name nil :type symbol)
  (type-index nil :type fixnum)
  (locals nil :type list)  ; (type ...)
  (body nil :type list))   ; (wasm-instr ...)

;; Wasm type
(defstruct wasm-type
  (params nil :type list)
  (results nil :type list))

;; Wasm global
(defstruct wasm-global
  (name nil :type symbol)
  (type nil :type keyword)
  (mutable-p nil :type boolean)
  (init nil :type list))

;; Wasm module
(defstruct wasm-module
  (types nil :type list)
  (imports nil :type list)
  (functions nil :type list)
  (tables nil :type list)
  (globals nil :type list)
  (exports nil :type list)
  (start nil :type (or null fixnum))
  (elements nil :type list)
  (code nil :type list)
  (tags nil :type list))
```

### 2.4 Symbol Table

```lisp
(defstruct symbol-table
  (symbols (make-hash-table :test 'equal) :type hash-table)
  (packages (make-hash-table :test 'equal) :type hash-table))

(defstruct package-info
  (name nil :type string)
  (symbols (make-hash-table :test 'equal) :type hash-table)
  (use-list nil :type list)
  (exported-symbols (make-hash-table :test 'equal) :type hash-table))
```

---

## 3. Runtime Data Structures

### 3.1 Binding Stack (Shallow Binding)

**Constitution Ref**: V (shallow binding for dynamic scope)

```wat
(type $binding-frame (struct
  (field $symbol (ref $symbol))
  (field $old-value anyref)
  (field $next (ref null $binding-frame))))

(global $binding-stack (mut (ref null $binding-frame))
  (ref.null $binding-frame))
```

**Operations**:
```lisp
;; Push binding
(defun push-binding (symbol old-value)
  (setf $binding-stack
        (struct.new $binding-frame symbol old-value $binding-stack)))

;; Pop binding (restore)
(defun pop-binding ()
  (let ((frame $binding-stack))
    (struct.set $symbol $value (struct.get $binding-frame $symbol frame)
                               (struct.get $binding-frame $old-value frame))
    (setf $binding-stack (struct.get $binding-frame $next frame))))
```

### 3.2 Multiple Values Buffer

**Constitution Ref**: III (multiple values via global buffer)

```wat
(global $mv-count (mut i32) (i32.const 1))
(global $mv-buffer (ref $vector)
  (array.new $vector (ref.null any) (i32.const 20)))
```

**Usage**:
- Primary value: returned normally
- Secondary values: stored in `$mv-buffer`
- `$mv-count`: number of values returned

### 3.3 Exception Tags

```wat
;; block/return-from tag
(tag $block-tag (param anyref anyref))  ; (block-id value)

;; catch/throw tag
(tag $throw-tag (param anyref anyref))  ; (catch-tag value)

;; general error
(tag $error-tag (param anyref))  ; (condition)
```

---

## 4. Type Checking

### 4.1 Runtime Type Predicates

```wat
;; fixnump
(func $fixnump (param $x anyref) (result i32)
  (ref.test i31 (local.get $x)))

;; consp
(func $consp (param $x anyref) (result i32)
  (ref.test (ref $cons) (local.get $x)))

;; symbolp
(func $symbolp (param $x anyref) (result i32)
  (ref.test (ref $symbol) (local.get $x)))

;; null (is NIL)
(func $null (param $x anyref) (result i32)
  (ref.eq (local.get $x) (global.get $NIL)))

;; atom (not cons)
(func $atom (param $x anyref) (result i32)
  (i32.eqz (call $consp (local.get $x))))
```

### 4.2 Type Casting

```wat
;; Safe cast to cons
(func $as-cons (param $x anyref) (result (ref $cons))
  (ref.cast (ref $cons) (local.get $x)))

;; Safe cast to symbol
(func $as-symbol (param $x anyref) (result (ref $symbol))
  (ref.cast (ref $symbol) (local.get $x)))
```

---

## 5. Validation Rules

### 5.1 Type Constraints

- All Lisp objects must be representable as `anyref`
- Fixnum must fit in i31ref range (-2^30 to 2^30-1)
- NIL must never be Wasm null
- Symbols must have non-null `$name` field

### 5.2 Structural Constraints

- Cons cells form proper lists (CDR is cons or NIL)
- Closures have at least one non-null code pointer
- CLOS instances have valid class references

### 5.3 State Transitions

**Symbol Value States**:
```
UNBOUND ─────► BOUND (via setq, defvar)
   ▲              │
   │              │
   └──────────────┘ (via makunbound)
```

**Binding Stack States**:
```
EMPTY ─────► PUSHED (via let with special)
   ▲            │
   │            │
   └────────────┘ (via unwind-protect cleanup)
```

---

## 6. Entity Relationship

```
┌─────────┐     ┌─────────┐     ┌──────────┐
│ Package │────►│ Symbol  │────►│ Function │
└─────────┘     └─────────┘     └──────────┘
                    │                 │
                    │                 ▼
                    │           ┌──────────┐
                    │           │ Closure  │
                    │           └──────────┘
                    ▼                 │
              ┌──────────┐            │
              │  Value   │◄───────────┘
              └──────────┘
                    │
        ┌───────────┼───────────┐
        ▼           ▼           ▼
   ┌─────────┐ ┌─────────┐ ┌──────────┐
   │ Fixnum  │ │  Cons   │ │ Instance │
   └─────────┘ └─────────┘ └──────────┘
                               │
                               ▼
                        ┌─────────────┐
                        │    Class    │
                        └─────────────┘
```
