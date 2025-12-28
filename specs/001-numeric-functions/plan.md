# Implementation Plan: ANSI Numeric Functions Extension

**Branch**: `001-numeric-functions` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-numeric-functions/spec.md`

## Summary

Extend the clysm3 Common Lisp compiler with comprehensive ANSI numeric functions including trigonometric (sin, cos, tan, asin, acos, atan), hyperbolic (sinh, cosh, tanh, asinh, acosh, atanh), bitwise (logcount, integer-length), mathematical (exp, log, sqrt, expt), complex number operations, and basic functions (abs, signum, max, min, gcd, lcm). Implementation leverages WasmGC f64 instructions for floating-point operations and i32/i64 instructions for bitwise operations. Target: 50%+ numbers category test pass rate.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler feature, no persistence)
**Testing**: Rove framework with `compile-and-run-numeric` helper for numeric validation
**Target Platform**: WasmGC (WebAssembly with GC proposal), executed via wasmtime
**Project Type**: Single (compiler library)
**Performance Goals**: Numeric operations should match native f64/i32 performance within 1.5x
**Constraints**: 64-bit integer precision for bitwise ops initially; IEEE 754 double for floats
**Scale/Scope**: 36 function implementations across 6 categories

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | Uses f64 for floats, i31ref for fixnums, struct for complex |
| II. Lisp Object Representation | ✅ PASS | Follows existing $float, $complex, $ratio type indices |
| III. Function/Closure Strategy | ✅ PASS | Functions added to primitive-call dispatch (no closures needed) |
| IV. Wasm Control Flow | ✅ PASS | No control flow changes required |
| V. Shallow Binding | N/A | No special variables involved |
| VI. Tiered Eval/JIT | ✅ PASS | Functions available in both interpreter and compiled code |
| VII. TDD | ✅ PASS | All functions require tests before implementation |
| VIII. Nix-First | ✅ PASS | Uses existing nix develop environment |

**Gate Result**: ALL PASS - proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-numeric-functions/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── func-section.lisp      # Primary: add primitive-call cases
│   │   ├── numeric-runtime.lisp   # Secondary: complex number helpers
│   │   └── gc-types.lisp          # Reference: type indices (16=$float, 17=$complex)
│   └── transform/                 # If macro expansion needed for variadic functions
├── lib/
│   └── macros.lisp                # Add pi constant if needed
└── runtime/                       # Runtime helpers if needed

tests/
├── unit/
│   ├── trig-functions-test.lisp   # NEW: trigonometric tests
│   ├── hyperbolic-functions-test.lisp  # NEW: hyperbolic tests
│   ├── bitwise-functions-test.lisp     # NEW: bitwise tests
│   ├── complex-functions-test.lisp     # NEW: complex number tests
│   └── math-functions-test.lisp        # EXTEND: exp, log, sqrt, expt
└── integration/
    └── numeric-suite-test.lisp    # NEW: ANSI compliance suite
```

**Structure Decision**: Extends existing single-project structure. Primary changes in `func-section.lisp` following established pattern for `compile-primitive-call`.

## Complexity Tracking

No violations - implementation follows established patterns.

## Implementation Notes

### Existing Infrastructure to Leverage

1. **Arithmetic compilation pattern** (`func-section.lisp:862-876`):
   - Already handles +, -, *, /, mod, rem
   - logand, logior, logxor, lognot, ash already implemented
   - Pattern: compile args → extract i32 → apply Wasm op → wrap result

2. **Float comparison infrastructure** (`func-section.lisp:2311-2359`):
   - Type dispatch between i31 and $float
   - Uses f64.lt, f64.gt, f64.eq Wasm instructions
   - Pattern reusable for trig/math functions

3. **Numeric type indices** (`gc-types.lisp`):
   - 16 = $float (IEEE 754 double)
   - 17 = $complex (real + imaginary parts)
   - Both structs already defined

4. **Test helpers** (`tests/helpers.lisp`):
   - `compile-and-run-numeric` handles non-fixnum results
   - Masks float traps for IEEE 754 special values

### Wasm Instructions for New Functions

| Category | Wasm Instructions |
|----------|-------------------|
| Trig (sin, cos, tan) | NOT native; require runtime import or Taylor series |
| Inverse trig (asin, acos, atan) | NOT native; require runtime implementation |
| Hyperbolic | NOT native; expressible via exp/log |
| exp, log, sqrt | f64.sqrt native; exp/log require runtime |
| Bitwise | i32.clz (leading zeros), i32.ctz (trailing zeros), i32.popcnt (popcount) |
| Basic (abs) | f64.abs native; i32 requires conditional |

### Critical Decision: Transcendental Functions

WasmGC does NOT have native sin/cos/tan/exp/log. Options:
1. **Import from host** via FFI (fastest, requires host cooperation)
2. **Taylor series** in Wasm (slower, fully self-contained)
3. **Lookup tables + interpolation** (middle ground)

Research needed in Phase 0 to determine optimal approach.
