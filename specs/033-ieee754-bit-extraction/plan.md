# Implementation Plan: IEEE 754 Bit Extraction

**Branch**: `033-ieee754-bit-extraction` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/033-ieee754-bit-extraction/spec.md`

## Summary

Replace SBCL-specific float-to-bits functions (`sb-kernel:single-float-bits`, `sb-kernel:double-float-bits`, `sb-int:with-float-traps-masked`) with portable Common Lisp implementations. This enables the Clysm compiler to run on non-SBCL implementations (CCL, ECL, CLISP, etc.) while maintaining bit-identical Wasm output for IEEE 754 float encoding.

**Technical Approach**: Implement pure Common Lisp bit extraction using IEEE 754 decomposition via `decode-float` and `integer-decode-float`, with portable trap masking using `handler-case` for float exceptions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+, CCL, ECL target support)
**Primary Dependencies**: None (portable CL only - no SBCL internals)
**Storage**: N/A (compile-time only)
**Testing**: Rove (existing test framework)
**Target Platform**: WasmGC output (compiler runs on any ANSI CL)
**Project Type**: Single project (compiler)
**Performance Goals**: Compile-time only; no runtime impact
**Constraints**: Must produce bit-identical Wasm output; must handle all IEEE 754 special values
**Scale/Scope**: 3 files to modify, ~100 lines of code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | No impact - compiler-side change only |
| II. Lisp Object Representation | ✅ PASS | No impact - float encoding unchanged |
| III. Function/Closure Strategy | ✅ PASS | No impact |
| IV. Wasm Control Flow | ✅ PASS | No impact |
| V. Shallow Binding | ✅ PASS | No impact |
| VI. Tiered Eval/JIT | ✅ PASS | No impact |
| VII. TDD (Non-negotiable) | ✅ PASS | Tests first for portable implementation |
| VIII. Nix-First Workflow | ✅ PASS | No flake.nix changes required |

**Gate Result**: PASS - No constitution violations.

## Project Structure

### Documentation (this feature)

```text
specs/033-ieee754-bit-extraction/
├── plan.md              # This file
├── research.md          # Phase 0: Portable float encoding research
├── data-model.md        # Phase 1: Float encoding data structures
├── quickstart.md        # Phase 1: Quick implementation guide
├── contracts/           # Phase 1: API contracts
│   └── float-bits.md    # Float-to-bits function contracts
└── tasks.md             # Phase 2: Implementation tasks
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── compiler.lisp    # MODIFY: emit-f32, emit-f64
│   └── ast.lisp         # MODIFY: fold-arithmetic
└── lib/
    └── float-bits.lisp  # NEW: Portable float-to-bits utilities

tests/
├── helpers.lisp         # MODIFY: compile-and-run-numeric
└── unit/
    └── float-bits-test.lisp  # NEW: Portable implementation tests
```

**Structure Decision**: Single project structure. New utility module `float-bits.lisp` provides portable float-to-bits functions used by compiler and test harness.

## Complexity Tracking

> No constitution violations - table not required.
