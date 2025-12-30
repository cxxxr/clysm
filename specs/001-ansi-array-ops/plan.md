# Implementation Plan: Phase 15C - ANSI Array Operations Enhancement

**Branch**: `001-ansi-array-ops` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-ansi-array-ops/spec.md`

## Summary

Implement 9 ANSI Common Lisp array functions to enable full array metadata access and manipulation:
- Metadata queries: [array-rank](../../resources/HyperSpec/Body/f_ar_ran.htm), [array-dimension](../../resources/HyperSpec/Body/f_ar_dim.htm), [array-dimensions](../../resources/HyperSpec/Body/f_ar_di1.htm), [array-total-size](../../resources/HyperSpec/Body/f_ar_tot.htm)
- Row-major access: [array-row-major-index](../../resources/HyperSpec/Body/f_ar_row.htm), [row-major-aref](../../resources/HyperSpec/Body/f_row_ma.htm), [(setf row-major-aref)](../../resources/HyperSpec/Body/f_row_ma.htm)
- Adjustability: [adjustable-array-p](../../resources/HyperSpec/Body/f_adjust.htm), [adjust-array](../../resources/HyperSpec/Body/f_adj_ar.htm)

Technical approach: Represent multidimensional arrays as 1D WasmGC arrays with dimension metadata stored in a new struct type. Simple-vectors remain as raw `$mv_array` for backward compatibility.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams; existing array infrastructure from `001-ansi-array-primitives`
**Storage**: N/A (in-memory compilation)
**Testing**: Rove test framework; unit tests, contract tests (Wasm validation), integration tests
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler infrastructure)
**Performance Goals**: Array metadata access in O(1), row-major index computation matching host CL performance
**Constraints**: Must preserve backward compatibility with existing simple-vector code; displaced arrays and fill-pointers deferred to future phase
**Scale/Scope**: 9 functions, ~400 lines of codegen, ~200 lines of tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Use `struct` for array metadata, `array` for storage |
| II. Lispオブジェクト表現規約 | ✅ PASS | Arrays are distinct from NIL; no null confusion |
| III. 関数・クロージャ実装戦略 | N/A | Array functions, not closures |
| IV. Wasm制御フロー活用 | ✅ PASS | No special control flow needed |
| V. シャローバインディング | N/A | No dynamic scope in this feature |
| VI. 段階的動的コンパイル | ✅ PASS | Already available in interpreter; this adds Wasm codegen |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Tests before implementation |
| VIII. Nix-Firstワークフロー | ✅ PASS | Use existing flake.nix |
| IX. ANSI Common Lisp仕様参照規約 | ✅ REQUIRED | HyperSpec links included above |

**Gate Status**: ✅ PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-array-ops/
├── plan.md              # This file
├── research.md          # Phase 0: Implementation research
├── data-model.md        # Phase 1: Array metadata struct design
├── quickstart.md        # Phase 1: Developer guide
├── contracts/           # Phase 1: Function signatures
│   └── array-ops.lisp   # Codegen function contracts
└── tasks.md             # Phase 2 output (from /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── gc-types.lisp        # ADD: $mdarray type (type index 28)
│       └── func-section.lisp    # ADD: 9 array function compilers
└── lib/
    └── setf-expanders.lisp      # ADD: (setf row-major-aref) expander

tests/
├── unit/
│   └── array-ops-test.lisp      # NEW: Unit tests for array ops
├── contract/
│   └── array-ops-wasm-test.lisp # NEW: Wasm validation tests
└── integration/
    └── array-ops-test.lisp      # NEW: E2E runtime tests
```

**Structure Decision**: Single project structure following existing clysm conventions. Array operation functions added to existing `func-section.lisp`, new WasmGC struct type in `gc-types.lisp`.

## Complexity Tracking

No constitution violations requiring justification.
