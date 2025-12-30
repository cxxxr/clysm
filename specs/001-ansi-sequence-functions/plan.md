# Implementation Plan: ANSI Sequence Generic Functions (Phase 15B)

**Branch**: `001-ansi-sequence-functions` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-ansi-sequence-functions/spec.md`

## Summary

Implement 21 ANSI CL sequence generic functions (count, find, position, mismatch, search, substitute, remove-duplicates, replace, fill variants) with full keyword argument support (:test, :key, :start, :end, :from-end, :count) for all sequence types (lists, vectors, strings). Target: 60%+ ANSI sequences test compliance rate.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) host compiler, WasmGC target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing), existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation, no persistence)
**Testing**: rove (Lisp-side unit tests), wasm-tools validate (Wasm validation), ANSI CL test suite (compliance)
**Target Platform**: WasmGC (WebAssembly with GC proposal), wasmtime runtime
**Project Type**: Single compiler project with standard library extension
**Performance Goals**: Operations on sequences up to 10,000 elements complete without noticeable delay
**Constraints**: Must follow ANSI CL specification precisely, no linear memory usage (WasmGC-First)
**Scale/Scope**: 21 functions, ~63 function variants (3 sequence types each), 32 functional requirements

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Sequence functions operate on WasmGC heap objects (cons, array, string structs) |
| II. Lispオブジェクト表現規約 | PASS | NIL handling per spec (singleton, not null); proper list termination |
| III. 関数・クロージャ実装戦略 | PASS | :test, :key predicates handled as closures with arity dispatch |
| IV. Wasm制御フロー活用 | PASS | Iteration via worklist pattern to avoid recursion limits |
| V. シャローバインディングによる動的スコープ | N/A | No special variables introduced |
| VI. 段階的動的コンパイル | N/A | Standard library functions, no eval/compile interaction |
| VII. テスト駆動開発（TDD） | PASS | Unit tests required for all 21 functions before implementation |
| VIII. Nix-Firstワークフロー | PASS | Existing flake.nix provides development environment |
| IX. ANSI Common Lisp仕様参照規約 | PASS | HyperSpec links required for all function implementations |

**Gate Result**: PASS - No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-sequence-functions/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (API signatures)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   ├── sequences.lisp       # New: Sequence generic functions implementation
│   ├── sequences-util.lisp  # New: Shared utilities (bounds checking, iteration)
│   └── macros.lisp          # Existing: May need :test/:key macro support
├── compiler/
│   └── codegen/
│       └── builtins.lisp    # Register new sequence functions as builtins

tests/
├── unit/
│   └── sequences/           # New: Per-function unit tests
│       ├── count-test.lisp
│       ├── find-test.lisp
│       ├── position-test.lisp
│       ├── mismatch-test.lisp
│       ├── search-test.lisp
│       ├── substitute-test.lisp
│       ├── remove-duplicates-test.lisp
│       ├── replace-test.lisp
│       └── fill-test.lisp
└── contract/
    └── sequences/           # ANSI compliance contract tests
```

**Structure Decision**: Single project structure following existing clysm layout. New sequence functions go in `src/clysm/lib/sequences.lisp` with unit tests in `tests/unit/sequences/`.

## Complexity Tracking

No violations to justify. Implementation follows existing patterns for standard library functions.
