# Implementation Plan: ANSI Common Lisp Numeric Functions

**Branch**: `001-ansi-numeric-functions` | **Date**: 2025-12-29 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-ansi-numeric-functions/spec.md`

## Summary

Implement ANSI Common Lisp numeric functions for the clysm compiler targeting 50%+ numbers category test compliance. This includes trigonometric functions (sin/cos/tan/asin/acos/atan), hyperbolic functions (sinh/cosh/tanh/asinh/acosh/atanh), bitwise operations (ash/logand/logior/logxor/lognot/logcount), complex number operations (complex/realpart/imagpart/conjugate/phase), and mathematical functions (exp/log/sqrt/expt/abs/signum/max/min/gcd/lcm).

Technical approach: Leverage existing FFI infrastructure to call JavaScript Math functions for transcendental operations. Implement integer-only operations (bitwise, gcd/lcm) directly in Wasm codegen. Add complex number type representation using WasmGC structs.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), wasmtime, wasm-tools
**Storage**: N/A (in-memory compilation only)
**Testing**: Rove (unit tests), ANSI CL test suite (compliance), wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal), Node.js host
**Project Type**: Single project (compiler + runtime)
**Performance Goals**: Per Constitution - Fixnum arithmetic 1.5x native, function call <5ns
**Constraints**: WasmGC type hierarchy, no linear memory, FFI for transcendental functions
**Scale/Scope**: 35 functions across 5 categories, targeting 50%+ ANSI numbers test compliance

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | Complex numbers will use WasmGC struct type |
| II. Lisp Object Representation | PASS | Numeric types follow established Float/Integer representation |
| III. Function/Closure Strategy | PASS | Numeric functions are regular closures |
| IV. Wasm Control Flow | PASS | No special control flow requirements |
| V. Shallow Binding | N/A | No dynamic scope in numeric functions |
| VI. Tiered Eval/JIT | PASS | Functions available in both interpreter and compiled code |
| VII. TDD (Non-Negotiable) | PASS | Tests written first per Rove framework |
| VIII. Nix-First Workflow | PASS | Using existing flake.nix development environment |

**Gate Result**: PASS - All constitution principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-numeric-functions/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── numeric-api.lisp # Function contracts
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # Add numeric function codegen
├── ffi/
│   └── import-gen.lisp          # Extend math imports
├── lib/
│   ├── macros.lisp              # Add signum, abs variants
│   └── numeric.lisp             # NEW: Numeric function implementations
├── runtime/
│   └── complex.lisp             # NEW: Complex number type support
└── ansi-test/
    └── numbers-test.lisp        # NEW: ANSI numbers category tests

host-shim/
└── math-shim.js                 # Extend with abs, signum (if needed)

tests/
├── unit/
│   └── numeric-test.lisp        # Unit tests for numeric functions
└── contract/
    └── numeric-contract-test.lisp  # Contract tests
```

**Structure Decision**: Single project structure. New files for complex number support and numeric function implementations. Extends existing FFI infrastructure (import-gen.lisp, math-shim.js).

## Complexity Tracking

No constitution violations. All implementations follow established patterns.

| Aspect | Approach | Justification |
|--------|----------|---------------|
| Transcendental functions | FFI to JS Math | 10x faster than Taylor series in Wasm (per research) |
| Bitwise operations | Wasm i32/i64 ops | Native Wasm instructions, no FFI overhead |
| Complex numbers | WasmGC struct | Follows Constitution I (WasmGC-First) |
| gcd/lcm | Euclidean algorithm | Standard implementation, no dependencies |

## Implementation Phases

### Phase 0: Research Complete

See [research.md](./research.md) for detailed findings on:
- Existing FFI math infrastructure (already implemented for sin/cos/exp/log/sqrt)
- Bitwise operation implementation patterns
- Complex number representation options
- ANSI CL edge case handling

### Phase 1: Design Complete

See [data-model.md](./data-model.md) for:
- Complex number WasmGC type definition
- Function signatures and type coercion rules
- Edge case handling matrix

See [contracts/](./contracts/) for:
- API contracts for all 35 functions
- Type dispatch rules
- Error conditions

### Phase 2: Tasks (Generated by /speckit.tasks)

Task breakdown will be generated in tasks.md.
