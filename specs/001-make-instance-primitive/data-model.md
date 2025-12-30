# Data Model: make-instance* Primitive

**Date**: 2025-12-31
**Feature**: 001-make-instance-primitive

## Entities

### 1. Instance ($instance)

The runtime representation of a CLOS/structure instance.

| Field | Type | Description |
|-------|------|-------------|
| class | ref $standard-class | Reference to class metadata |
| slots | ref $slot-vector | Array of slot values |

**Type Index**: 6

### 2. Standard Class ($standard-class)

Metadata for a CLOS class or structure.

| Field | Type | Description |
|-------|------|-------------|
| name | ref $symbol | Class name symbol |
| superclass | ref null $standard-class | Parent class (nullable) |
| slot_count | i32 | Total slots including inherited |
| initargs | ref $keyword-array | Initarg keywords per slot |
| initforms | ref $closure-array | Initform closures per slot |
| class_id | i32 | Unique dispatch index |

**Type Index**: 7

### 3. Slot Vector ($slot-vector)

Array storage for instance slot values.

| Property | Value |
|----------|-------|
| Element Type | mut anyref |
| Initialization | nil (global 0) |
| Size | Determined by class slot_count |

**Type Index**: 21

## Relationships

```
┌─────────────┐
│   Instance  │
│   ($instance)│
└──────┬──────┘
       │ class (ref)
       ▼
┌─────────────────┐
│ Standard Class  │
│($standard-class)│
└─────────────────┘
       │ slots (ref)
       ▼
┌─────────────┐
│ Slot Vector │
│($slot-vector)│
└─────────────┘
```

## Compile-Time Entities

### 4. Compile-Time Class Registry

Hash table mapping class names to class metadata.

| Key | Value |
|-----|-------|
| symbol (class name) | class-info struct |

**Location**: `*compile-time-class-registry*` in func-section.lisp

### 5. Primitive List Entry

Symbol entry in the primitive dispatch list.

| Property | Value |
|----------|-------|
| Symbol | make-instance* |
| Matching | symbol-name (cross-package) |
| Handler | compile-make-instance* |

## State Transitions

```
Compilation:
  Source Code: (make-instance* 'point :x 1 :y 2)
       │
       ▼
  AST: call with function=make-instance*, args=[quote, keyword-args...]
       │
       ▼
  Primitive Dispatch: compile-make-instance*
       │
       ▼
  Wasm Instructions: struct.new $instance
```

## Validation Rules

1. **Class must exist**: Class name must be registered at compile time
2. **Slot count must match**: Slot vector size must equal class slot_count
3. **Type safety**: Instance type must be $instance (index 6)
