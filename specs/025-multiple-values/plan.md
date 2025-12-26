# Implementation Plan: ANSI Common Lisp Multiple Values Support

**Branch**: `025-multiple-values` | **Date**: 2025-12-26 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/025-multiple-values/spec.md`

## Summary

Implement ANSI Common Lisp multiple values support in the clysm compiler. The feature enables functions to return multiple values and callers to receive them through forms like `values`, `multiple-value-bind`, `multiple-value-list`, `nth-value`, `values-list`, `multiple-value-prog1`, and `multiple-value-call`. Secondary values are stored in a global buffer (array of anyref) with a count global tracking value quantity.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory globals within Wasm module)
**Testing**: rove (unit, contract, integration tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (Common Lisp compiler)
**Performance Goals**: No regression for single-value code paths (SC-008)
**Constraints**: Wasm sandbox; GC-only memory (no linear memory); tail-call compatible

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Value buffer uses `(array anyref)` type; no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL handling via global index 0; values as anyref |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Constitution mandates 多値バッファ; this feature implements it |
| IV. Wasm制御フロー活用 | ✅ PASS | Uses block/loop for iteration; no recursion limits |
| V. シャローバインディング | N/A | No special variable interaction |
| VI. 段階的動的コンパイル | ✅ PASS | JIT modules can import mv-count/mv-buffer globals |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Unit→Contract→Integration test progression |
| VIII. Nix-Firstワークフロー | ✅ REQUIRED | Must pass `nix flake check` |

**Gate Status**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/025-multiple-values/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Wasm module contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # AST structs for mv forms (values, mvb, mvl, etc.)
│   ├── compiler.lisp               # Wasm opcodes (array.get, etc.)
│   └── codegen/
│       └── func-section.lisp       # Compilation logic for all mv forms
├── runtime/
│   └── multi-value.lisp            # Global definitions (mv-count, mv-buffer)
└── package.lisp                    # Exported AST symbols

tests/
├── unit/
│   └── multiple-values/
│       └── multiple-values-test.lisp    # Unit tests for 8 mv forms
├── contract/
│   └── multiple-values/
│       └── multiple-values-wasm-test.lisp  # Wasm validation tests
└── integration/
    └── multiple-values/
        └── multiple-values-ansi-test.lisp  # ANSI CL conformance tests
```

**Structure Decision**: Single-project layout following existing clysm structure. Compiler source in `src/clysm/`, tests in `tests/` with unit/contract/integration hierarchy.

## Complexity Tracking

No constitution violations requiring justification. Implementation follows established patterns:
- Global variables pattern from existing NIL/UNBOUND globals
- Array type pattern from existing string/vector implementations
- Compilation pattern from existing special forms
