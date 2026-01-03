# Data Model: Numeric Runtime Migration

**Feature**: 001-numeric-runtime-migration
**Date**: 2026-01-04

## Entities

### Runtime Function Entry

Registration entry in `*runtime-function-table*`.

| Field | Type | Description |
|-------|------|-------------|
| symbol | symbol | Lisp function name (e.g., `parse-integer`) |
| runtime-name | keyword | Wasm function identifier (e.g., `:$parse-integer-rt`) |
| arity | (or fixnum null) | Expected argument count, nil for variadic |

### Numeric Types (Existing)

The numeric tower types used by the runtime functions.

| Type | WasmGC Index | Description |
|------|--------------|-------------|
| fixnum | i31ref | Small integers (tagged immediate) |
| bignum | (not implemented) | Large integers (heap allocated) |
| ratio | $ratio (5) | Rational numbers (numerator/denominator) |
| float | $float (4) | IEEE 754 double-precision |
| complex | (via $cons) | Complex numbers (real, imag pair) |

### Function Signatures

| Function | Lisp Signature | Runtime Name | Arity |
|----------|---------------|--------------|-------|
| [parse-integer](resources/HyperSpec/Body/f_parse_.htm) | `(string &key start end radix junk-allowed)` | `:$parse-integer-rt` | nil (variadic) |
| [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm) | `(object &key base ...)` | `:$write-to-string-rt` | nil (variadic) |
| [rationalize](resources/HyperSpec/Body/f_ration.htm) | `(number)` | `:$rationalize-rt` | 1 |
| [signum](resources/HyperSpec/Body/f_signum.htm) | `(number)` | `:$signum-rt` | 1 |
| [phase](resources/HyperSpec/Body/f_phase.htm) | `(number)` | `:$phase-rt` | 1 |

## Relationships

```
*runtime-function-table*
    │
    ├── 'parse-integer  → (:$parse-integer-rt . nil)
    ├── 'write-to-string → (:$write-to-string-rt . nil)
    ├── 'rationalize    → (:$rationalize-rt . 1)
    ├── 'signum         → (:$signum-rt . 1)
    └── 'phase          → (:$phase-rt . 1)
```

## Validation Rules

1. **Arity Validation**: For fixed-arity functions (rationalize, signum, phase), `compile-runtime-call` validates argument count matches
2. **Type Validation**: Runtime functions perform type checks at execution time using Layer 1 predicates
3. **Radix Validation**: `parse-integer` and `write-to-string` validate radix is in range 2-36

## State Transitions

None. These are pure functions with no state management.

## Constants

| Constant | Value | Usage |
|----------|-------|-------|
| +min-radix+ | 2 | Minimum allowed radix |
| +max-radix+ | 36 | Maximum allowed radix |
| +default-radix+ | 10 | Default radix for parse-integer |
| +pi+ | 3.141592653589793d0 | Used by phase for negative reals |
