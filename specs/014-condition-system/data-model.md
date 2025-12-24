# Data Model: ANSI Common Lisp Condition System

**Date**: 2025-12-24
**Feature**: 014-condition-system

## Entity Overview

```text
┌─────────────────────────────────────────────────────────────────────┐
│                       Condition System Entities                      │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌─────────────┐        ┌─────────────────┐                         │
│  │  Condition  │◀───────│ Handler-Cluster │                         │
│  │  (CLOS)     │        │                 │                         │
│  └──────┬──────┘        └────────┬────────┘                         │
│         │                        │                                   │
│         │ is-a                   │ contains                          │
│         ▼                        ▼                                   │
│  ┌─────────────┐        ┌─────────────────┐                         │
│  │ Condition   │        │    Handler      │                         │
│  │  Hierarchy  │        │                 │                         │
│  └─────────────┘        └─────────────────┘                         │
│                                                                      │
│  ┌─────────────────┐    ┌─────────────────┐                         │
│  │ Restart-Cluster │───▶│    Restart      │                         │
│  │                 │    │                 │                         │
│  └─────────────────┘    └─────────────────┘                         │
│                                                                      │
│                    Global Stacks (Dynamic Scope)                     │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ *handler-clusters* : Stack of Handler-Cluster                   │ │
│  │ *restart-clusters* : Stack of Restart-Cluster                   │ │
│  └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

## Entity Definitions

### 1. Condition (CLOS Class Hierarchy)

**Description**: Base class for all exceptional situations.

```text
condition
├── serious-condition
│   └── error
│       ├── simple-error
│       ├── type-error
│       ├── cell-error
│       │   ├── unbound-variable
│       │   └── undefined-function
│       ├── control-error
│       └── program-error
└── warning
    └── simple-warning

simple-condition (mixin)
├── simple-error (+ error)
└── simple-warning (+ warning)
```

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| (CLOS class) | class | Condition type for handler matching |

### 2. Simple-Condition (Mixin)

**Description**: Mixin providing format-control and format-arguments for condition messages.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| format-control | string or nil | Format string for condition message |
| format-arguments | list | Arguments for format string |

### 3. Type-Error

**Description**: Signaled when an object is not of the expected type.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| datum | any | The object that failed the type check |
| expected-type | type-specifier | The type that was expected |

### 4. Cell-Error

**Description**: Base for errors related to cells (variables, functions).

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| name | symbol | The name of the cell |

### 5. Handler

**Description**: Associates a condition type with a handler function.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| condition-type | type-specifier | Type of conditions this handler handles |
| function | function | Handler function (condition) -> any |
| cluster | handler-cluster | Parent cluster (for unbinding during execution) |

**Validation Rules**:
- condition-type must be a valid type specifier
- function must accept one argument (the condition)

### 6. Handler-Cluster

**Description**: A group of handlers established by a single handler-bind or handler-case.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| handlers | list of Handler | Handlers in this cluster |

**Lifecycle**:
1. Created: At handler-bind/handler-case entry
2. Active: Pushed onto *handler-clusters*
3. Searchable: While on stack
4. Destroyed: Popped on scope exit (normal or non-local)

### 7. Restart

**Description**: A named recovery point that can be invoked to continue from an error.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| name | symbol or nil | Restart name for find-restart |
| function | function | Function to invoke when restart is selected |
| report-function | function or nil | Function to describe restart to user |
| interactive-function | function or nil | Function to gather arguments interactively |
| test-function | function or nil | Predicate for restart applicability |
| associated-conditions | list | Conditions this restart is associated with |

**Validation Rules**:
- name should be a symbol (nil for anonymous restarts)
- function must accept appropriate arguments for this restart type

**Standard Restart Signatures**:

| Restart | Function Signature |
|---------|-------------------|
| abort | () -> no-return |
| continue | () -> nil |
| muffle-warning | () -> no-return (transfers control) |
| use-value | (value) -> value |
| store-value | (value) -> value |

### 8. Restart-Cluster

**Description**: A group of restarts established by a single restart-case or restart-bind.

**Attributes**:

| Attribute | Type | Description |
|-----------|------|-------------|
| restarts | list of Restart | Restarts in this cluster |
| catch-tag | symbol | Unique tag for throw-based control transfer |

**Lifecycle**:
1. Created: At restart-case/restart-bind entry
2. Active: Pushed onto *restart-clusters*
3. Invocable: While on stack
4. Destroyed: Popped on scope exit

## Global State (Dynamic Variables)

### *handler-clusters*

**Type**: List of Handler-Cluster
**Initial Value**: NIL
**Scope**: Dynamic (shallow binding)

**Description**: Stack of active handler clusters. Innermost cluster is at the front of the list.

**Operations**:
- Push cluster on handler-bind/handler-case entry
- Pop cluster on scope exit (via unwind-protect)
- Search from front to back when signaling

### *restart-clusters*

**Type**: List of Restart-Cluster
**Initial Value**: NIL
**Scope**: Dynamic (shallow binding)

**Description**: Stack of active restart clusters. Innermost cluster is at the front of the list.

**Operations**:
- Push cluster on restart-case/restart-bind entry
- Pop cluster on scope exit (via unwind-protect)
- Search from front to back in find-restart/compute-restarts

## WasmGC Type Mapping

### Condition Instance (via CLOS)

```wat
;; Condition is a standard CLOS instance
;; Uses existing $instance type from clos/instance.lisp
(type $condition (sub $instance
  (struct
    (field $class (ref $class))
    (field $slots (ref $slot-vector)))))
```

### Handler Struct

```wat
(type $handler (struct
  (field $type anyref)          ;; Condition type specifier
  (field $function (ref $closure))  ;; Handler closure
  (field $cluster (ref $handler-cluster))))
```

### Handler-Cluster Struct

```wat
(type $handler-cluster (struct
  (field $handlers (ref $handler-array))))

(type $handler-array (array (mut (ref $handler))))
```

### Restart Struct

```wat
(type $restart (struct
  (field $name anyref)          ;; Symbol or nil
  (field $function (ref $closure))
  (field $report-function anyref)      ;; Closure or nil
  (field $interactive-function anyref) ;; Closure or nil
  (field $test-function anyref)        ;; Closure or nil
  (field $conditions (ref $cons))))    ;; List of associated conditions
```

### Restart-Cluster Struct

```wat
(type $restart-cluster (struct
  (field $restarts (ref $restart-array))
  (field $catch-tag anyref)))  ;; Symbol for throw

(type $restart-array (array (mut (ref $restart))))
```

## State Transitions

### Handler Lifecycle

```text
    ┌─────────────────┐
    │   Not Exists    │
    └────────┬────────┘
             │ handler-bind/handler-case
             ▼
    ┌─────────────────┐
    │     Active      │ ◀──────────────────┐
    │ (on cluster     │                    │
    │   stack)        │────────────────────┤
    └────────┬────────┘   condition        │
             │            signaled,        │
             │            handler          │
             │            declines         │
             ▼                             │
    ┌─────────────────┐                    │
    │   Invoked       │────────────────────┘
    │ (handler fn     │   returns normally
    │   called)       │   (decline)
    └────────┬────────┘
             │ non-local transfer
             ▼
    ┌─────────────────┐
    │   Unwound       │
    │ (removed from   │
    │   stack)        │
    └─────────────────┘
```

### Restart Lifecycle

```text
    ┌─────────────────┐
    │   Not Exists    │
    └────────┬────────┘
             │ restart-case/restart-bind
             ▼
    ┌─────────────────┐
    │    Available    │ ◀──────────────────┐
    │ (on cluster     │                    │
    │   stack)        │────────────────────┤
    └────────┬────────┘   condition        │
             │            signaled         │
             │                             │
             ▼                             │
    ┌─────────────────┐                    │
    │   Found         │                    │
    │ (via find-      │────────────────────┘
    │   restart)      │   not invoked
    └────────┬────────┘
             │ invoke-restart
             ▼
    ┌─────────────────┐
    │   Invoked       │
    │ (throws to      │
    │   catch-tag)    │
    └────────┬────────┘
             │ catch receives throw
             ▼
    ┌─────────────────┐
    │  Transferred    │
    │ (control at     │
    │   restart-case) │
    └─────────────────┘
```

## Relationships

```text
Condition ──[is-type-of]──▶ Handler.condition-type
    │
    └──[associated-with]──▶ Restart.associated-conditions

Handler ──[belongs-to]──▶ Handler-Cluster
    │
    └──[references]──▶ Closure (handler function)

Handler-Cluster ──[on-stack]──▶ *handler-clusters*

Restart ──[belongs-to]──▶ Restart-Cluster
    │
    └──[references]──▶ Closure (restart function)

Restart-Cluster ──[on-stack]──▶ *restart-clusters*
    │
    └──[has]──▶ catch-tag (for throw)
```

## Invariants

1. **Stack Discipline**: Handler and restart clusters are always paired with unwind-protect cleanup
2. **Innermost First**: Handler search proceeds from innermost (front of list) to outermost
3. **Handler Unbinding**: During handler execution, the handler's cluster is unbound (prevents infinite recursion)
4. **Restart Visibility**: Restarts remain visible during handler execution (unlike handlers)
5. **Type Hierarchy**: Condition types form a proper subtype hierarchy via CLOS
6. **Unique Catch Tags**: Each restart-case generates a unique catch tag (gensym)
