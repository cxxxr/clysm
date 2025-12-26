# Contract: CLOS WasmGC Types

**Date**: 2025-12-27
**Feature**: 026-clos-foundation

## Overview

This contract defines the WasmGC type representations for CLOS entities in the generated Wasm modules.

## Type Section Requirements

### Reserved Type Indices

| Index | Name | Purpose | Status |
|-------|------|---------|--------|
| 6 | `$instance` | Runtime CLOS instances | NEW |
| 7 | `$standard-class` | Class metadata objects | NEW |
| 21 | `$slot-vector` | Mutable slot storage | NEW |
| 22 | `$keyword-array` | Initarg keyword storage | NEW |
| 23 | `$closure-array` | Initform closure storage | NEW |

### Type Definitions (WAT Format)

```wat
;; ============================================================
;; CLOS Type Definitions
;; ============================================================

;; Type 6: Instance - runtime object with class reference and slot storage
(type $instance (struct
  (field $class (ref $standard-class))    ;; non-null class reference
  (field $slots (ref $slot-vector))       ;; non-null slot array
))

;; Type 7: Standard-Class - class metadata
(type $standard-class (struct
  (field $name (ref $symbol))             ;; class name symbol
  (field $superclass (ref null $standard-class))  ;; nullable parent
  (field $slot_count i32)                 ;; total slots including inherited
  (field $initargs (ref $keyword-array))  ;; initarg keywords per slot
  (field $initforms (ref $closure-array)) ;; initform closures per slot
  (field $class_id i32)                   ;; unique dispatch index
))

;; Type 21: Slot-Vector - mutable array for slot values
(type $slot-vector (array (mut anyref)))

;; Type 22: Keyword-Array - immutable keyword array
(type $keyword-array (array (ref $symbol)))

;; Type 23: Closure-Array - nullable closure array for initforms
(type $closure-array (array (ref null $closure)))
```

### Binary Encoding

```
Type Section (ID 1)
├── Type 6 ($instance)
│   ├── 0x5F (struct prefix)
│   ├── 0x02 (2 fields)
│   ├── Field 0: (ref 7)  ;; $class - reference to type 7
│   │   └── 0x64 0x07     ;; ref type, index 7
│   └── Field 1: (ref 21) ;; $slots - reference to type 21
│       └── 0x64 0x15     ;; ref type, index 21
│
├── Type 7 ($standard-class)
│   ├── 0x5F (struct prefix)
│   ├── 0x06 (6 fields)
│   ├── Field 0: (ref 3)  ;; $name - symbol reference
│   ├── Field 1: (ref null 7)  ;; $superclass - nullable class
│   ├── Field 2: i32      ;; $slot_count
│   ├── Field 3: (ref 22) ;; $initargs
│   ├── Field 4: (ref 23) ;; $initforms
│   └── Field 5: i32      ;; $class_id
│
├── Type 21 ($slot-vector)
│   ├── 0x5E (array prefix)
│   └── (mut anyref)      ;; mutable anyref elements
│
├── Type 22 ($keyword-array)
│   ├── 0x5E (array prefix)
│   └── (ref 3)           ;; symbol references
│
└── Type 23 ($closure-array)
    ├── 0x5E (array prefix)
    └── (ref null 5)      ;; nullable closure references
```

## Instruction Contracts

### Instance Creation

```wat
;; make-instance compilation pattern
;; Input: class-name symbol, initargs on stack
;; Output: (ref $instance) on stack

;; 1. Look up class global
(global.get $class-{name})        ;; → (ref $standard-class)

;; 2. Get slot count
(struct.get $standard-class $slot_count)  ;; → i32

;; 3. Create slot vector
(array.new_default $slot-vector)  ;; → (ref $slot-vector)

;; 4. Initialize slots (loop over initargs/initforms)
;; ... slot initialization code ...

;; 5. Create instance
(struct.new $instance)            ;; → (ref $instance)
```

### Slot Access

```wat
;; Reader: (point-x instance)
;; Input: instance on stack
;; Output: slot value on stack

(local.get $instance)
(struct.get $instance $slots)     ;; → (ref $slot-vector)
(i32.const {slot-index})
(array.get $slot-vector)          ;; → anyref (slot value)

;; Writer: (setf (point-x instance) value)
;; Input: value, instance on stack
;; Output: value on stack (after store)

(local.get $instance)
(struct.get $instance $slots)
(i32.const {slot-index})
(local.get $value)
(array.set $slot-vector)
```

### Type Checking for Dispatch

```wat
;; instance-of check: is instance of class or subclass?
;; Input: instance, target-class
;; Output: i32 (0 or 1)

(func $instance-of-class
  (param $inst (ref $instance))
  (param $target (ref $standard-class))
  (result i32)

  (local $current (ref null $standard-class))

  ;; Get instance's class
  (local.set $current
    (struct.get $instance $class (local.get $inst)))

  ;; Walk superclass chain
  (block $not-found
    (loop $check
      ;; Exact match?
      (if (ref.eq (local.get $current) (local.get $target))
        (then (return (i32.const 1))))

      ;; Move to superclass
      (local.set $current
        (struct.get $standard-class $superclass
          (ref.cast (ref $standard-class) (local.get $current))))

      ;; Null means end of chain
      (br_if $not-found (ref.is_null (local.get $current)))
      (br $check)))

  (i32.const 0))
```

## Validation Criteria

### wasm-tools validate Checks

1. **Type section ordering**: Types 6, 7, 21, 22, 23 must appear in correct order
2. **Field type validity**: All field references must point to valid type indices
3. **Mutability**: Only `$slot-vector` elements are mutable
4. **Nullability**: `$superclass` field nullable, others non-null

### Test Cases

```lisp
;; Test 1: Instance type structure
(deftest instance-type-structure
  (let ((module (compile-form '(defclass point () ((x) (y))))))
    ;; Type 6 should be struct with 2 fields
    (ok (= 2 (type-field-count module 6)))
    ;; Field 0 should reference type 7
    (ok (type-references module 6 0 7))
    ;; Field 1 should reference type 21
    (ok (type-references module 6 1 21))))

;; Test 2: Class type structure
(deftest class-type-structure
  (let ((module (compile-form '(defclass point () ((x) (y))))))
    ;; Type 7 should be struct with 6 fields
    (ok (= 6 (type-field-count module 7)))
    ;; Field 1 (superclass) should be nullable
    (ok (field-nullable-p module 7 1))))

;; Test 3: Slot vector type
(deftest slot-vector-type
  (let ((module (compile-form '(defclass point () ((x) (y))))))
    ;; Type 21 should be array
    (ok (array-type-p module 21))
    ;; Element type should be anyref
    (ok (eq :anyref (array-element-type module 21)))
    ;; Should be mutable
    (ok (array-mutable-p module 21))))
```

## Global Section Requirements

### Class Global Pattern

Each defined class generates a global variable:

```wat
;; Global for class 'point'
(global $class-point (ref $standard-class)
  (struct.new $standard-class
    (global.get $sym-point)       ;; name
    (ref.null $standard-class)    ;; superclass (none)
    (i32.const 2)                 ;; slot-count
    (array.new_fixed $keyword-array 2
      (global.get $kw-x)
      (global.get $kw-y))         ;; initargs
    (array.new_fixed $closure-array 2
      (ref.null $closure)
      (ref.null $closure))        ;; initforms (none)
    (i32.const 0)))               ;; class-id
```

### Naming Convention

| Entity | Global Name Pattern |
|--------|---------------------|
| Class | `$class-{class-name}` |
| Keyword | `$kw-{keyword-name}` |
| Symbol | `$sym-{symbol-name}` |

## Error Conditions

| Condition | Detection Point | Error Type |
|-----------|-----------------|------------|
| Undefined class in make-instance | Runtime | `simple-error` |
| No applicable method | Dispatch | `no-applicable-method` |
| Unbound slot access | Runtime | `unbound-slot` |
| Type mismatch in accessor | Runtime | `type-error` |
