# Implementation Plan: Phase 14A - Basic Arithmetic Function Extension

**Branch**: `002-numeric-functions` | **Date**: 2025-12-30 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-numeric-functions/spec.md`

## Summary

Implement 26+ ANSI CL numeric functions across 6 categories to achieve 50%+ numeric test compliance:
- **Trigonometric** (6): [sin, cos, tan](resources/HyperSpec/Body/f_sin_c.htm), [asin, acos, atan](resources/HyperSpec/Body/f_asin_.htm)
- **Hyperbolic** (6): [sinh, cosh, tanh, asinh, acosh, atanh](resources/HyperSpec/Body/f_sinh_.htm)
- **Bit Operations** (6): [ash](resources/HyperSpec/Body/f_ash.htm), [logand, logior, logxor, lognot](resources/HyperSpec/Body/f_logand.htm), [logcount](resources/HyperSpec/Body/f_logcou.htm)
- **Mathematical** (6): [exp, expt](resources/HyperSpec/Body/f_exp_e.htm), [log](resources/HyperSpec/Body/f_log.htm), [sqrt](resources/HyperSpec/Body/f_sqrt_.htm), [abs](resources/HyperSpec/Body/f_abs.htm), [signum](resources/HyperSpec/Body/f_signum.htm)
- **Type Conversion** (2): [float](resources/HyperSpec/Body/f_float.htm), [rational](resources/HyperSpec/Body/f_ration.htm)
- **String Parsing** (1): [parse-integer](resources/HyperSpec/Body/f_parse_.htm)

Technical approach: Leverage existing FFI infrastructure for transcendental functions via JavaScript Math API. Implement bit operations using native Wasm i32/i64 instructions. Extend the numeric type dispatch system for new functions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Runtime Dependencies**: wasmtime, wasm-tools (validation), Node.js 20+ (FFI host shim)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove test framework, contract tests, integration tests via wasmtime
**Project Type**: single (compiler)
**Performance Goals**: Match existing arithmetic operation performance; FFI calls < 1000ns per invocation
**Constraints**: Must use WasmGC type system; transcendental functions via FFI only; bit operations must handle 64-bit range
**Scale/Scope**: 26 functions, ~2000 LOC, ~50 test cases

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First型システム | PASS | Float uses `$float` struct with f64 field; Integer uses `i31ref` for fixnum, `$bignum` for large values |
| II. Lispオブジェクト表現規約 | PASS | NIL handled correctly in numeric operations; UNBOUND not involved |
| III. 関数・クロージャ実装戦略 | PASS | New functions use existing closure dispatch; multi-value returns for parse-integer via mv-buffer |
| IV. Wasm制御フロー活用 | PASS | No new control flow needed; uses existing exception handling |
| V. シャローバインディング | N/A | No special variables introduced |
| VI. 段階的動的コンパイル | PASS | Functions compile to Wasm; no interpreter-only code |
| VII. TDD（非交渉） | REQUIRED | Must write tests before implementation per Red-Green-Refactor |
| VIII. Nix-Firstワークフロー | PASS | flake.nix includes wasm-tools, wasmtime; no new dependencies |
| IX. ANSI CL仕様参照規約 | REQUIRED | All function docs must link to local HyperSpec |

**Gate Status**: PASS - No violations. TDD and HyperSpec linking are process requirements to enforce during implementation.

## Project Structure

### Documentation (this feature)

```text
specs/002-numeric-functions/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── function-signatures.lisp
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── func-section.lisp      # Add trigonometric, hyperbolic, math compile functions
│       └── numeric-runtime.lisp   # Extend type dispatch for new operations
├── lib/
│   └── numeric-functions.lisp     # New file: Lisp-level function definitions
└── ffi/
    └── import-gen.lisp            # Extend math imports (already has sin/cos/etc)

host-shim/
└── math-shim.js                   # Verify/extend JavaScript Math API bindings

tests/
├── unit/
│   └── math-functions-test.lisp   # Extend existing test suite
├── contract/
│   └── numeric-contracts-test.lisp
└── integration/
    └── numeric-compliance-test.lisp
```

**Structure Decision**: Single project structure. New functions integrate into existing compiler infrastructure at `src/clysm/compiler/codegen/`. Tests extend existing test suites in `tests/unit/`.

## Complexity Tracking

No violations to track. Feature uses existing infrastructure patterns:
- FFI import system already supports math functions
- Bit operations follow existing `lognot` implementation pattern
- Type conversion follows existing coercion patterns

## Implementation Categories

### Category 1: FFI-Based Functions (Transcendental)

Functions implemented via JavaScript Math API calls through existing FFI infrastructure:

| Function | JS Binding | Wasm Type | Notes |
|----------|------------|-----------|-------|
| sin | Math.sin | f64→f64 | Already in import-gen.lisp |
| cos | Math.cos | f64→f64 | Already in import-gen.lisp |
| tan | Math.tan | f64→f64 | Already in import-gen.lisp |
| asin | Math.asin | f64→f64 | Already in import-gen.lisp |
| acos | Math.acos | f64→f64 | Already in import-gen.lisp |
| atan | Math.atan/atan2 | f64→f64, (f64,f64)→f64 | Two-arg form needs atan2 |
| sinh | Math.sinh | f64→f64 | Already in import-gen.lisp |
| cosh | Math.cosh | f64→f64 | Already in import-gen.lisp |
| tanh | Math.tanh | f64→f64 | Already in import-gen.lisp |
| asinh | Math.asinh | f64→f64 | Already in import-gen.lisp |
| acosh | Math.acosh | f64→f64 | Already in import-gen.lisp |
| atanh | Math.atanh | f64→f64 | Already in import-gen.lisp |
| exp | Math.exp | f64→f64 | Already in import-gen.lisp |
| log | Math.log | f64→f64 | Two-arg form needs log(x)/log(base) |
| sqrt | f64.sqrt | f64→f64 | Native Wasm instruction available |
| expt | Math.pow | (f64,f64)→f64 | Already in import-gen.lisp |

### Category 2: Native Wasm Instructions (Bit Operations)

Functions implemented directly with Wasm i32/i64 instructions:

| Function | Wasm Instruction | Notes |
|----------|------------------|-------|
| ash | i32.shl / i32.shr_s | Direction by sign of count |
| logand | i32.and | Variadic with identity -1 |
| logior | i32.or | Variadic with identity 0 |
| logxor | i32.xor | Variadic with identity 0 |
| lognot | i32.xor -1 | Already implemented |
| logcount | i32.popcnt | Already has infrastructure |

### Category 3: Type Dispatch Functions

Functions requiring numeric type dispatch:

| Function | Types Handled | Notes |
|----------|---------------|-------|
| abs | fixnum, float, ratio, complex | Already partially implemented |
| signum | fixnum, float, ratio | Returns -1, 0, or 1 of same type |
| float | integer, ratio → float | Coercion function |
| rational | float → ratio | May need continued fraction algorithm |

### Category 4: Complex Implementation

| Function | Complexity | Notes |
|----------|------------|-------|
| parse-integer | High | String parsing, radix, keywords, MV return |

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Bignum support for bit ops | Medium | Limit initial scope to 64-bit; document limitation |
| Float→Rational precision | Medium | Use standard continued fraction algorithm; document precision limits |
| Parse-integer edge cases | High | Comprehensive test suite from ANSI CL examples |
| FFI latency | Low | Already proven in existing trig functions |

## Next Steps

1. **Phase 0**: Generate research.md resolving remaining unknowns
2. **Phase 1**: Generate data-model.md, contracts/, quickstart.md
3. **Phase 2**: Generate tasks.md via `/speckit.tasks`
