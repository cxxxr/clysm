# Data Model: Phase 13D M3 - CLOS Primitives for Wasm

**Date**: 2025-12-31
**Feature**: 001-m3-clos-primitives

## Overview

This feature adds compiler codegen for CLOS primitive functions. No new data types are introduced - the implementation uses existing WasmGC types defined in `gc-types.lisp`.

## Existing Types (No Changes)

### $instance (Type Index 6)

CLOS instance representation.

```wat
(type $instance (struct
  (field $class (ref $standard-class))  ;; index 0: class metadata
  (field $slots (ref $slot-vector))))   ;; index 1: slot values
```

| Field | Index | Type | Mutable | Description |
|-------|-------|------|---------|-------------|
| `$class` | 0 | `(ref $standard-class)` | No | Reference to class metadata |
| `$slots` | 1 | `(ref $slot-vector)` | No | Reference to slot value array |

**Invariants**:
- `$class` is never null (always references valid class)
- `$slots` length equals `$slot_count` from class

### $standard-class (Type Index 7)

Class metadata structure.

```wat
(type $standard-class (struct
  (field $name (ref $symbol))           ;; index 0: class name
  (field $superclass (ref null $standard-class)) ;; index 1: parent class
  (field $slot_count i32)               ;; index 2: total slot count
  (field $initargs (ref $keyword-array)) ;; index 3: initarg keywords
  (field $initforms (ref $closure-array)) ;; index 4: initform closures
  (field $class_id i32)))               ;; index 5: unique dispatch ID
```

| Field | Index | Type | Mutable | Description |
|-------|-------|------|---------|-------------|
| `$name` | 0 | `(ref $symbol)` | No | Class name symbol |
| `$superclass` | 1 | `(ref null $standard-class)` | No | Parent class (nullable) |
| `$slot_count` | 2 | `i32` | No | Total slots including inherited |
| `$initargs` | 3 | `(ref $keyword-array)` | No | Keyword symbols for initargs |
| `$initforms` | 4 | `(ref $closure-array)` | No | Initform thunks |
| `$class_id` | 5 | `i32` | No | Unique ID for dispatch |

### $slot-vector (Type Index 21)

Slot value storage array.

```wat
(type $slot-vector (array (mut anyref)))
```

| Property | Value |
|----------|-------|
| Element Type | `anyref` |
| Mutable | Yes |
| Length | Variable (equals `$slot_count`) |

**Operations**:
- `array.get $slot-vector` - Read slot value
- `array.set $slot-vector` - Write slot value
- `array.new_default $slot-vector` - Create with nulls

## Compile-Time Data Structures

### Slot Index Resolution

The compiler maintains a mapping from slot names to indices during compilation.

```lisp
;; At compile time, slot-value* with literal slot name
(slot-value* instance 'x)
;; Resolves to slot index via defclass/defstruct definition
```

**Slot Index Calculation**:
1. DEFSTRUCT slots numbered 0 to N-1 in declaration order
2. :include slots come first (inherited from parent)
3. Direct slots follow inherited slots

Example:
```lisp
(defstruct parent a b)        ;; slots: a=0, b=1
(defstruct (child (:include parent)) c d)  ;; slots: a=0, b=1, c=2, d=3
```

### Class Metadata Globals

Each class/struct creates a global for its metadata:

```wat
(global $class-point (ref $standard-class) ...)
(global $class-child (ref $standard-class) ...)
```

## Wasm Instruction Sequences

### slot-value* Read

```
Input:  (slot-value* instance 'slot-name)
Output:
  <compile instance>          ;; anyref on stack
  ref.cast (ref $instance)    ;; cast to $instance
  struct.get $instance 1      ;; get $slots field (ref $slot-vector)
  i32.const <slot-index>      ;; push slot index
  array.get $slot-vector      ;; read slot value (anyref)
```

### slot-value* Write (setf)

```
Input:  (setf (slot-value* instance 'slot-name) value)
Output:
  <compile instance>          ;; anyref on stack
  ref.cast (ref $instance)    ;; cast to $instance
  struct.get $instance 1      ;; get $slots field
  i32.const <slot-index>      ;; push slot index
  <compile value>             ;; push new value
  array.set $slot-vector      ;; write slot value
  <compile value>             ;; return value (setf semantics)
```

### make-instance*

```
Input:  (make-instance* 'class-name &key ...)
Output:
  global.get $class-<name>    ;; class metadata (ref $standard-class)
  i32.const <slot-count>      ;; number of slots
  array.new_default $slot-vector ;; create slot array
  struct.new $instance        ;; create instance struct
  ;; Initialize slots from initargs/initforms...
```

### standard-instance-p

```
Input:  (standard-instance-p obj)
Output:
  <compile obj>               ;; anyref on stack
  ref.test (ref $instance)    ;; test if $instance type (i32 result)
  if (result anyref)          ;; convert to Lisp boolean
    global.get $t
  else
    global.get $nil
  end
```

## Validation Rules

### FR-005: Slot Index Determination

- Slot name must be a compile-time literal symbol
- Slot must exist in the target class/struct
- Index calculated from slot order in definition

### FR-008: Wasm Validation

All generated code must pass:
```bash
wasm-tools validate dist/clysm-stage1.wasm
```

### Edge Cases

| Case | Behavior |
|------|----------|
| Invalid slot name | Compile-time error |
| Nil instance | Runtime trap (ref.cast fails) |
| Dynamic slot name | Fallback to runtime lookup (not optimized) |
| Unknown class | Compile-time error |
