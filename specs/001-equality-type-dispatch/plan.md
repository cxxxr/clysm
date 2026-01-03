# Implementation Plan: Equality Predicate Type-Dispatch Consolidation

**Branch**: `001-equality-type-dispatch` | **Date**: 2026-01-03 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-equality-type-dispatch/spec.md`

## Summary

Consolidate four equality predicate compiler functions ([eq](resources/HyperSpec/Body/f_eq.htm), [eql](resources/HyperSpec/Body/f_eql.htm), [equal](resources/HyperSpec/Body/f_equal.htm), [equalp](resources/HyperSpec/Body/f_equalp.htm)) from 840+ lines to under 400 lines using a unified type-dispatch infrastructure. The refactoring introduces `compile-type-dispatch` for runtime type checking and `compile-equality-predicate` as a master function accepting an equality level parameter.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, existing primitive-dispatch.lisp infrastructure
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (unit tests), wasm-tools (contract tests), integration tests
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler infrastructure)
**Performance Goals**: Compile-time performance unchanged; generated Wasm identical
**Constraints**: Byte-identical Wasm output, < 400 lines equality code, func-section.lisp < 15,700 lines
**Scale/Scope**: 4 functions consolidated, ~440 lines reduction

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses ref.test/ref.cast for type dispatch |
| II. Lispオブジェクト表現規約 | PASS | NIL handled as singleton, UNBOUND considered |
| III. 関数・クロージャ実装戦略 | N/A | Not modifying closure implementation |
| IV. Wasm制御フロー活用 | PASS | Nested if/else structure preserved |
| V. シャローバインディング | N/A | Not modifying dynamic scope |
| VI. 段階的動的コンパイル | N/A | Not modifying eval/JIT |
| VII. テスト駆動開発（TDD） | REQUIRED | Must run existing tests before/after refactoring |
| VIII. Nix-Firstワークフロー | REQUIRED | `nix flake check` must pass |
| IX. ANSI Common Lisp仕様参照 | PASS | HyperSpec links included in this document |

## Project Structure

### Documentation (this feature)

```text
specs/001-equality-type-dispatch/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
├── func-section.lisp       # Main target: compile-eq/eql/equal/equalp (lines 4339-5179)
├── primitive-dispatch.lisp # register-primitive-compiler infrastructure
└── primitive-registry.lisp # Equality primitives registered (lines 195-198)

tests/
├── unit/equality-predicates-test.lisp
├── integration/equality-ansi-test.lisp
└── contract/equality-wasm-test.lisp
```

**Structure Decision**: Single project structure. The refactoring is localized to `func-section.lisp` with registrations in `primitive-registry.lisp`.

## Complexity Tracking

No constitution violations requiring justification.
