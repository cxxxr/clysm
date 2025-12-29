# Implementation Plan: ANSI CL Array/Sequence Primitives

**Branch**: `001-ansi-array-primitives` | **Date**: 2025-12-29 | **Spec**: [spec.md](spec.md)
**Input**: Phase 13D-1: ANSI CL配列・シーケンスプリミティブを実装する。目標はセルフホスティングのブロッカー解消。

## Summary

Implement ANSI CL array and sequence primitives ([aref](../../resources/HyperSpec/Body/f_aref.htm), [svref](../../resources/HyperSpec/Body/f_svref.htm), [schar](../../resources/HyperSpec/Body/f_schar.htm), [elt](../../resources/HyperSpec/Body/f_elt.htm), [coerce](../../resources/HyperSpec/Body/f_coerce.htm)) as Wasm primitives, compiling to `array.get`/`array.set` instructions. This enables compilation of defstruct-generated accessors (90 forms) and leb128.lisp, increasing compilation rate from 12.9% to 30%+.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), wasmtime, wasm-tools
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit/integration tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Compiler (single project structure)
**Performance Goals**: Compilation correctness is priority; runtime performance optimization deferred
**Constraints**: Single-dimensional arrays only; WasmGC type indices 0-27 already defined
**Scale/Scope**: 5 primitives + 4 setf expanders; target: 90+ defstruct forms compiling

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Arrays use WasmGC `array` type; uses existing type 22 ($mv_array) for simple-vectors |
| II. Lispオブジェクト表現規約 | PASS | NIL handled via `ref.is_null` check before array access |
| III. 関数・クロージャ実装戦略 | N/A | Primitives are inline; no new closure patterns |
| IV. Wasm制御フロー活用 | PASS | Error signaling uses existing `throw` mechanism |
| V. シャローバインディング | N/A | No dynamic variables involved |
| VI. 段階的動的コンパイル | PASS | Primitives work in both interpreter and compiled paths |
| VII. テスト駆動開発（TDD） | REQUIRED | Tests must be written before implementation |
| VIII. Nix-Firstワークフロー | PASS | `nix flake check` must pass |
| IX. ANSI CL仕様参照規約 | REQUIRED | HyperSpec links must be included for all primitives |

**Gate Status**: PASS (with TDD and HyperSpec link requirements)

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-array-primitives/
├── plan.md              # This file
├── research.md          # Phase 0: WasmGC array instructions research
├── data-model.md        # Phase 1: Type definitions and representations
├── quickstart.md        # Phase 1: Implementation quickstart guide
├── contracts/           # Phase 1: API contracts for primitives
└── tasks.md             # Phase 2: Implementation tasks
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── gc-types.lisp         # Type 22 ($mv_array) reused for simple-vectors
│   │   ├── func-section.lisp     # Add array primitive codegen
│   │   └── primitives/
│   │       └── array.lisp        # NEW: Array primitive compilation (aref, svref, schar, elt)
│   └── transform/
│       └── coerce.lisp           # NEW: Coerce macro/function expansion
├── lib/
│   └── setf-expanders.lisp       # Add setf expanders for array primitives
└── runtime/
    └── sequences.lisp            # NEW: Runtime support for elt on lists

tests/
├── unit/
│   └── array-primitives-test.lisp    # NEW: Unit tests for codegen
├── contract/
│   └── array-wasm-test.lisp          # NEW: Wasm validation tests
└── integration/
    └── array-test.lisp               # NEW: End-to-end tests
```

**Structure Decision**: Extend existing compiler structure with new primitives/array.lisp module for array codegen, following pattern of existing primitive handlers. Reuse type 22 ($mv_array) for simple-vectors as it's already an `(array (mut anyref))`.

## Complexity Tracking

No violations to justify. Implementation uses existing WasmGC infrastructure.

## Key Technical Decisions

### Type Mapping

| Lisp Type | WasmGC Type | Type Index | Notes |
|-----------|-------------|------------|-------|
| simple-vector | `(array (mut anyref))` | 22 | Reuse $mv_array type |
| string | `(array (mut i8))` | 4 | Existing $string type |
| character | `i31ref` | N/A | Inline representation |

### Instruction Mapping

| Lisp Function | Wasm Instruction | Notes |
|---------------|------------------|-------|
| `aref`/`svref` | `array.get 22` | Simple-vector element access |
| `schar` | `array.get_u 4` | Get unsigned byte, wrap in i31ref |
| `(setf aref)` | `array.set 22` | Simple-vector element mutation |
| `(setf schar)` | `array.set 4` | String character mutation |
| `elt` (vector) | `array.get 22` | After type dispatch |
| `elt` (list) | Runtime function | Uses `nth` equivalent |
| `coerce` | Compile-time expansion | Type-directed code generation |

### Error Handling

- Out-of-bounds: Runtime check with `array.len`, signal via existing `throw` mechanism
- Type errors: `ref.test` for type checking, signal `type-error` condition
