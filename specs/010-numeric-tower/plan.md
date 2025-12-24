# Implementation Plan: Numeric Tower

**Branch**: `010-numeric-tower` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/010-numeric-tower/spec.md`

## Summary

Implement Common Lisp numeric tower with four additional types (Bignum, Ratio, Float, Complex) on top of existing Fixnum. Generate WasmGC code with proper type coercion, arithmetic operations, mathematical functions, and type predicates. All numeric types must integrate with the existing WasmGC type system and be executable by wasmtime.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler, WasmGC for output
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: Rove framework with wasmtime execution via `compile-and-run` helper
**Target Platform**: WasmGC (wasmtime with --wasm gc, --wasm function-references, --wasm exceptions)
**Project Type**: Single project (src/ + tests/)
**Performance Goals**: Fixnum arithmetic within 1.5x of native i32; bignum 1000+ digits supported
**Constraints**: WasmGC type system, i31ref for fixnum, no linear memory access
**Scale/Scope**: 4 new numeric types, ~15 arithmetic/math functions, ~10 type predicates

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Numeric types will use WasmGC structs/arrays; no linear memory |
| II. Lispオブジェクト表現規約 | PASS | Numeric types follow existing struct patterns |
| III. 関数・クロージャ実装戦略 | PASS | Math functions use existing closure mechanism |
| IV. Wasm制御フロー活用 | PASS | Error handling for division-by-zero uses exception handling |
| V. シャローバインディングによる動的スコープ | N/A | No special variables introduced by this feature |
| VI. 段階的動的コンパイル（Tiered Eval/JIT） | N/A | Numeric operations are AOT compiled |
| VII. テスト駆動開発（TDD）（非交渉） | REQUIRED | All numeric types/operations must have tests first |
| VIII. Nix-Firstワークフロー | PASS | Existing flake.nix with wasmtime already configured |

**Gate Status**: PASS - No violations requiring justification

## Project Structure

### Documentation (this feature)

```text
specs/010-numeric-tower/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── gc-types.lisp       # ADD: Bignum, Ratio, Float, Complex type definitions
│   │   └── func-section.lisp   # MODIFY: Numeric operations for all types
│   └── compiler.lisp           # MODIFY: Type coercion, numeric literal handling
├── runtime/
│   └── numeric.lisp            # ADD: Numeric type predicates, conversions
└── lib/
    └── math.lisp               # ADD: Mathematical functions (sqrt, expt, gcd, lcm, etc.)

tests/
├── contract/
│   └── numeric-types-test.lisp # ADD: WasmGC type structure validation
├── integration/
│   ├── bignum-test.lisp        # ADD: Bignum arithmetic end-to-end
│   ├── ratio-test.lisp         # ADD: Ratio arithmetic end-to-end
│   ├── float-test.lisp         # ADD: Float arithmetic end-to-end
│   ├── complex-test.lisp       # ADD: Complex arithmetic end-to-end
│   └── mixed-arithmetic-test.lisp  # ADD: Type coercion tests
└── unit/
    ├── numeric-predicates-test.lisp  # ADD: Type predicate tests
    └── math-functions-test.lisp      # ADD: Mathematical function tests
```

**Structure Decision**: Single project structure (Option 1) - extending existing compiler with new numeric types and operations.

## Complexity Tracking

> No violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | - | - |
