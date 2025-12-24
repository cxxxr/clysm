# Data Model: Eval/JIT Compile System

**Branch**: `017-eval-jit-compile` | **Date**: 2025-12-24

## Entities

### 1. Compiled Function (Tiered)

A callable function produced by `compile*`, with tier tracking and promotion capability.

```lisp
(defstruct tiered-function
  "A function that can be promoted from Tier 1 to Tier 2."
  (name nil :type (or null symbol))           ; NIL for anonymous, symbol for named
  (definition nil :type cons)                  ; Original lambda expression
  (tier :tier-1 :type (member :tier-1 :tier-2))
  (implementation nil :type function)          ; Current callable function
  (invocation-count 0 :type fixnum)
  (promoted-p nil :type boolean))              ; Has been promoted to Tier 2?
```

**State Transitions**:
```
[Created] → tier=:tier-1, invocation-count=0, promoted-p=NIL
     ↓ (each call)
[Counting] → invocation-count++
     ↓ (count > threshold)
[Promotion Attempted]
     ↓ (success)
[Promoted] → tier=:tier-2, implementation=JIT-fn, promoted-p=T
     ↓ (failure)
[Stuck at Tier 1] → tier=:tier-1, promoted-p=NIL (never retry)
```

### 2. Invocation Counter

Tracks call counts per function for hot spot detection.

```lisp
(defvar *invocation-counts* (make-hash-table :test 'eq)
  "Maps tiered-function → invocation count.")
```

**Key**: `tiered-function` instance (by `eq`)
**Value**: Fixnum count

### 3. Function Slot Registry

Maps symbols to their current function implementation.

```lisp
(defvar *function-slots* (make-hash-table :test 'eq)
  "Maps symbol → tiered-function for named functions.")
```

**Key**: Symbol name
**Value**: `tiered-function` instance

**Invariant**: When a function is promoted, the function slot is updated atomically.

### 4. Wasm Instance

Represents an instantiated JIT-compiled Wasm module.

```lisp
(defstruct wasm-instance
  "An instantiated Wasm module."
  (binary nil :type (or null (vector (unsigned-byte 8))))
  (exports (make-hash-table :test 'equal) :type hash-table)
  (main-function nil :type (or null function)))
```

**Lifecycle**:
1. Created by `instantiate-wasm`
2. `main-function` extracted by `extract-function`
3. Used as `implementation` in `tiered-function`

### 5. Runtime Import Registry

Maps import names to host functions for Wasm modules.

```lisp
(defvar *runtime-imports* (make-hash-table :test 'equal)
  "Maps import-name (string) → host function.")
```

**Pre-registered Imports**:
- `"add"`, `"sub"`, `"mul"`, `"div"` → arithmetic
- `"lt"`, `"gt"`, `"eq"` → comparisons
- `"cons"`, `"car"`, `"cdr"` → list operations

### 6. Compilation Threshold

Global configuration for tier promotion.

```lisp
(defvar *compilation-threshold* 10
  "Number of invocations before promoting to Tier 2.")
```

**Validation**: Must be positive fixnum.

## Relationships

```
                    ┌─────────────────┐
                    │ *function-slots*│
                    │   (hash-table)  │
                    └────────┬────────┘
                             │ maps symbol →
                             ▼
┌───────────────┐      ┌─────────────────┐      ┌──────────────────┐
│   compile*    │─────▶│ tiered-function │─────▶│  implementation  │
└───────────────┘      └────────┬────────┘      │    (function)    │
                                │               └──────────────────┘
                                │                      ▲
                                │ tracks count         │ hot-patched on promotion
                                ▼                      │
                    ┌─────────────────────┐    ┌──────┴──────────┐
                    │ *invocation-counts* │    │   jit-compile   │
                    │    (hash-table)     │    └─────────────────┘
                    └─────────────────────┘            ▲
                                                       │ generates
                                                       │
                                               ┌───────┴───────┐
                                               │ wasm-instance │
                                               └───────────────┘
```

## Data Flow

### Compile Flow

```
(compile name lambda-expr)
           │
           ▼
┌──────────────────────────┐
│ 1. Validate lambda-expr  │
└───────────┬──────────────┘
            │
            ▼
┌──────────────────────────┐
│ 2. Create Tier 1 impl    │
│    via interpret         │
└───────────┬──────────────┘
            │
            ▼
┌──────────────────────────┐
│ 3. Create tiered-function│
│    wrapper               │
└───────────┬──────────────┘
            │
            ▼
┌──────────────────────────┐
│ 4. If name: register in  │
│    *function-slots*      │
└───────────┬──────────────┘
            │
            ▼
┌──────────────────────────┐
│ 5. Return wrapper fn     │
└──────────────────────────┘
```

### Invocation Flow (with Promotion)

```
(funcall compiled-fn args...)
           │
           ▼
┌──────────────────────────┐
│ 1. Increment count       │
└───────────┬──────────────┘
            │
            ▼
┌──────────────────────────┐
│ 2. Check promotion       │
│    threshold             │
└───────────┬──────────────┘
            │
      ┌─────┴─────┐
      │ exceed?   │
      └─────┬─────┘
            │
   ┌────────┴────────┐
   ▼ No              ▼ Yes
┌──────────┐   ┌─────────────────┐
│ Execute  │   │ Attempt Tier 2  │
│ current  │   │ compilation     │
│ impl     │   └────────┬────────┘
└──────────┘            │
                  ┌─────┴─────┐
                  │ success?  │
                  └─────┬─────┘
                        │
               ┌────────┴────────┐
               ▼ No              ▼ Yes
         ┌──────────┐   ┌─────────────────┐
         │ Keep     │   │ Hot-patch impl  │
         │ Tier 1   │   │ with JIT fn     │
         └──────────┘   └────────┬────────┘
                                 │
                                 ▼
                        ┌─────────────────┐
                        │ Execute JIT fn  │
                        └─────────────────┘
```
