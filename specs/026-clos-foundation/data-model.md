# Data Model: CLOS Foundation

**Date**: 2025-12-27
**Feature**: 026-clos-foundation

## Entity Overview

```
┌─────────────────┐       1    ┌─────────────────┐
│ Standard-Class  │◄──────────│    Instance     │
├─────────────────┤       *   ├─────────────────┤
│ name            │           │ class           │
│ superclass      │───────┐   │ slots           │
│ slot-count      │       │   └─────────────────┘
│ slot-defs       │       │
│ initargs        │       │  0..1
│ initforms       │       └───────────────┐
│ precedence-list │                       │
└─────────────────┘                       ▼
        │                          ┌─────────────────┐
        │ 1                        │ Standard-Class  │
        │                          │   (superclass)  │
        ▼ *                        └─────────────────┘
┌─────────────────┐
│ Slot-Definition │
├─────────────────┤
│ name            │
│ initarg         │
│ initform        │
│ accessor        │
│ index           │
└─────────────────┘

┌─────────────────┐       1    ┌─────────────────┐
│Generic-Function │◄──────────│     Method      │
├─────────────────┤       *   ├─────────────────┤
│ name            │           │ specializers    │
│ methods         │           │ function        │
│ lambda-list     │           │ lambda-list     │
└─────────────────┘           └─────────────────┘
```

## Entities

### Standard-Class

Represents a user-defined class at runtime.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| name | symbol | immutable | Class name symbol |
| superclass | standard-class? | immutable | Parent class (null if none) |
| slot-count | i32 | immutable | Total slots including inherited |
| slot-defs | list<slot-definition> | immutable | Slot metadata |
| initargs | array<keyword> | immutable | Initarg keywords per slot |
| initforms | array<closure> | immutable | Default value thunks |
| precedence-list | list<standard-class> | immutable | Computed at finalization |
| class-id | i32 | immutable | Unique index for dispatch tables |

**Identity Rules**:
- Classes are uniquely identified by their name symbol
- Two classes with same name cannot coexist (redefinition replaces)
- Class-id is assigned sequentially at compile-time

**Lifecycle**:
```
┌─────────┐    defclass     ┌─────────────┐    finalize    ┌───────────┐
│ (none)  │ ─────────────► │ Unfinalized │ ────────────► │ Finalized │
└─────────┘                 └─────────────┘                └───────────┘
                                  │                              │
                            (CPL not computed)            (CPL computed,
                                                          dispatch ready)
```

**Validation Rules**:
- Name must be a non-nil symbol
- Superclass must be nil or a finalized class
- Slot names must be unique (including inherited)
- Circular inheritance is prohibited

### Instance

Represents a runtime object instantiated from a class.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| class | standard-class | immutable | Reference to class metadata |
| slots | slot-vector | mutable | Array of slot values |

**Identity Rules**:
- Each instance has unique identity (pointer equality)
- Instance identity persists across slot mutations
- `eq` uses pointer comparison

**Lifecycle**:
```
┌─────────┐   make-instance   ┌─────────────┐
│ (none)  │ ───────────────► │ Initialized │
└─────────┘                   └─────────────┘
                                    │
                              (slots filled,
                               ready for use)
```

**Validation Rules**:
- Class reference must be non-null and finalized
- Slot vector size must equal class.slot-count
- Slot values initialized from initargs or initforms

### Slot-Definition

Compile-time metadata describing a single slot.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| name | symbol | immutable | Slot name |
| initarg | keyword? | immutable | Initialization keyword |
| initform | expression? | immutable | Default value form |
| accessor | symbol? | immutable | Accessor function name |
| index | i32 | immutable | Position in slot-vector |

**Validation Rules**:
- Name must be a symbol
- Initarg must be a keyword if present
- Accessor must be a symbol if present
- Index must be in range [0, class.slot-count)

### Slot-Vector

Runtime storage for instance slot values.

| Property | Value |
|----------|-------|
| WasmGC Type | `(array (mut anyref))` |
| Element Type | anyref |
| Mutability | mutable |

**Special Values**:
- UNBOUND: Sentinel indicating slot has no value
- NIL: Valid slot value (not same as unbound)

### Generic-Function

Container for polymorphic method dispatch.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| name | symbol | immutable | Function name |
| methods | list<method> | mutable | Registered methods |
| lambda-list | list | immutable | Parameter specification |
| method-table | array<method?> | mutable | Class-ID indexed lookup |

**Identity Rules**:
- Generic functions are uniquely identified by name
- First defmethod creates the generic function
- Subsequent defmethods add to existing GF

**Lifecycle**:
```
┌─────────┐    defmethod    ┌──────────┐   add-method   ┌─────────────────┐
│ (none)  │ ─────────────► │ Created  │ ─────────────► │ Methods Added   │
└─────────┘                 └──────────┘                └─────────────────┘
                                  │                            │
                           (1 method,                   (multiple methods,
                            dispatch ready)              table populated)
```

### Method

A specialized implementation within a generic function.

| Field | Type | Mutability | Description |
|-------|------|------------|-------------|
| specializers | list<class> | immutable | Parameter type constraints |
| function | closure | immutable | Implementation body |
| lambda-list | list | immutable | Parameter specification |

**Validation Rules**:
- Specializers length must match GF parameter count
- Each specializer must be a class (or t for any type)
- Function must be a callable closure

## Relationships

### Class → Superclass (0..1)
- Each class has at most one superclass (single inheritance)
- Root classes have null superclass
- Creates linear inheritance chain

### Class → Slot-Definitions (1..*)
- Each class owns its directly-defined slots
- Inherits parent slots (prepended to slot list)
- Slot indices are globally unique per class hierarchy

### Instance → Class (1)
- Each instance has exactly one class
- Class reference is immutable
- Determines slot layout and method dispatch

### Generic-Function → Methods (1..*)
- GF contains one or more methods
- Methods ordered by specificity for dispatch
- Same specializer combination not allowed twice

## WasmGC Type Mapping

| Entity | WasmGC Type | Type Index |
|--------|-------------|------------|
| Standard-Class | struct | 7 |
| Instance | struct | 6 |
| Slot-Vector | array (mut anyref) | 21 |
| Generic-Function | closure (existing) | 5 |
| Method | closure (existing) | 5 |

### Type Definitions

```wat
;; Type index 6: Instance
(type $instance (sub $gc-object (struct
  (field $class (ref $standard-class))
  (field $slots (ref $slot-vector))
)))

;; Type index 7: Standard-Class
(type $standard-class (sub $gc-object (struct
  (field $name (ref $symbol))
  (field $superclass (ref null $standard-class))
  (field $slot-count i32)
  (field $initargs (ref $keyword-array))
  (field $initforms (ref $closure-array))
  (field $class-id i32)
)))

;; Type index 21: Slot-Vector
(type $slot-vector (array (mut anyref)))

;; Type index 22: Keyword-Array (for initargs)
(type $keyword-array (array (ref $symbol)))

;; Type index 23: Closure-Array (for initforms)
(type $closure-array (array (ref null $closure)))
```

## State Transitions

### Class Definition Flow

```
1. Parse defclass form
   ├─ Extract class name, superclass name, slot specs
   └─ Validate syntax

2. Resolve superclass
   ├─ If superclass name provided, look up in class registry
   ├─ If not found → ERROR: undefined superclass
   └─ If found, verify finalized

3. Process slot definitions
   ├─ For each slot spec, create slot-definition
   ├─ Merge with inherited slots (prepend parent slots)
   ├─ Check for duplicate names → ERROR if found
   └─ Assign slot indices

4. Create class object
   ├─ Allocate standard-class struct
   ├─ Compute class precedence list
   ├─ Assign unique class-id
   └─ Register in class registry

5. Generate accessor functions
   ├─ For each slot with :accessor
   ├─ Create reader generic function (or add method)
   └─ Create writer generic function (or add method)
```

### Instance Creation Flow

```
1. Receive make-instance call
   ├─ Extract class name and initargs
   └─ Validate class exists → ERROR if not

2. Allocate instance
   ├─ Create slot-vector of size class.slot-count
   └─ Create instance struct with class + slots

3. Initialize slots
   ├─ For each slot in precedence list order:
   │   ├─ If initarg provided in initargs → use value
   │   ├─ Else if initform exists → evaluate and use
   │   └─ Else → set UNBOUND
   └─ Return initialized instance
```

### Method Dispatch Flow

```
1. Receive generic function call
   ├─ Extract function name and arguments
   └─ Look up generic function → ERROR if not found

2. Compute applicable methods
   ├─ For each method in GF:
   │   ├─ For each specializer and argument:
   │   │   └─ Check instance-of relationship
   │   └─ If all match → method is applicable
   └─ Collect all applicable methods

3. Sort by specificity
   ├─ Compare specializers pairwise
   ├─ More specific class wins
   └─ Result: most-specific-first order

4. Invoke most specific method
   ├─ If no applicable methods → ERROR: no-applicable-method
   ├─ Call method function with arguments
   └─ Return result
```
