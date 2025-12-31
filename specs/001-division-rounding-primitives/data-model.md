# Data Model: Division/Rounding Function Primitives

**Created**: 2025-12-31

## Overview

This feature extends the compiler's code generation to handle rounding functions. No new persistent data structures are introduced; the model describes the runtime data flow during compilation and execution.

## Entities

### 1. Rounding Function Call

Represents a parsed rounding function invocation in the AST.

| Field | Type | Description |
|-------|------|-------------|
| operator | symbol | One of: floor, ceiling, round, ffloor, fceiling, fround |
| dividend | AST node | First argument (number to be divided) |
| divisor | AST node \| nil | Second argument (nil for single-argument form) |

**Validation Rules**:
- operator must be one of the 6 supported functions
- dividend is required
- divisor defaults to 1 if nil

### 2. Rounding Result

Represents the 2-value result of a rounding operation.

| Field | Type | Description |
|-------|------|-------------|
| quotient | anyref | Rounded quotient (i31ref or $float) |
| remainder | anyref | Remainder value (i31ref or $float) |

**Invariant**: `dividend = quotient * divisor + remainder`

**Type Rules**:

| Function | Quotient Type | Remainder Type |
|----------|---------------|----------------|
| floor, ceiling, round | integer (i31ref) | same as argument type |
| ffloor, fceiling, fround | float ($float) | same as argument type |

### 3. Multiple Value Return Context

Uses existing global infrastructure (Feature 025).

| Global Index | Name | Type | Purpose |
|-------------|------|------|---------|
| 2 | $mv_count | i32 | Set to 2 for rounding functions |
| 3 | $mv_buffer | ref $mv_array | remainder stored at index 0 |

## Data Flow

### Compilation Flow

```
Lisp Source          AST Parser           Code Generator         Wasm Output
     │                    │                      │                    │
(floor 7 2)  ──────▶  ast-call      ──────▶  compile-floor   ──────▶  bytecode
                    operator: floor
                    args: [7, 2]
```

### Runtime Execution Flow

```
Arguments           Wasm Instructions              Results
    │                      │                          │
 [7, 2]  ──────▶  i31.get_s (unbox)         ┌──▶  quotient = 3 (on stack)
                  i32.div_s                 │
                  compute remainder   ──────┤
                  global.set $mv_count = 2  │
                  array.set mv_buffer[0]  ──┴──▶  remainder = 1 (in buffer)
```

## Type Representations

### Integer (i31ref)

Used for fixnum arguments and integer quotients.

```wasm
;; Unbox: i31ref → i32
(ref.cast :i31)
(i31.get_s)

;; Box: i32 → i31ref
(ref.i31)
```

### Float ($float struct)

Used for floating-point arguments and ffloor/fceiling/fround quotients.

```wasm
;; Type definition (existing)
(type $float (struct (field $value f64)))

;; Unbox: $float → f64
(ref.cast (ref $float))
(struct.get $float 0)

;; Box: f64 → $float
(struct.new $float)
```

## State Transitions

### Rounding Operation States

```
┌─────────────────┐
│  Initial State  │
│  (args on stack)│
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Type Dispatch  │
│  (int vs float) │
└────────┬────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌───────┐ ┌───────┐
│Integer│ │ Float │
│  Path │ │  Path │
└───┬───┘ └───┬───┘
    │         │
    ▼         ▼
┌─────────────────┐
│ Compute Quotient│
│ (floor/ceil/...) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│Compute Remainder│
│ dividend - q*d  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Store in MV    │
│  (count=2)      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Return Quotient│
│  (on stack)     │
└─────────────────┘
```

## Relationships

```
AST Parsing (ast.lisp)
       │
       │ already recognizes floor/ceiling/round
       ▼
Code Generator (func-section.lisp)
       │
       │ NEW: compile-floor, compile-ceiling, etc.
       ▼
Wasm Instructions
       │
       │ uses existing infrastructure
       ▼
┌──────┴──────┐
│             │
▼             ▼
Multiple      Type
Values        System
(Global 2,3)  (i31ref, $float)
```

## Volume Assumptions

- These functions are called frequently in numeric code
- Runtime performance is critical
- Compile-time overhead is acceptable
- No persistence required (pure computation)
