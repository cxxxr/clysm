# Implementation Plan: Compiler Code Generation Cleanup

**Branch**: `001-codegen-cleanup` | **Date**: 2026-01-04 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-codegen-cleanup/spec.md`

## Summary

Systematically identify and remove dead code from func-section.lisp after runtime library migration. The system will detect compile-* functions whose functionality has been migrated to *runtime-function-table*, identify transitive dead helper functions via call graph analysis, migrate remaining quasiquote splice patterns (,@) to with-instruction-collector macro, and validate each removal batch via test suite and Stage 1 Wasm validation. Target: reduce func-section.lisp from 15,973 lines to under 8,000 lines while maintaining 100% test compatibility.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to dist/)
**Testing**: rove test framework (`sbcl --eval "(asdf:test-system :clysm)"`)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler codebase)
**Performance Goals**: N/A (refactoring task, no runtime performance changes)
**Constraints**: 100% test compatibility, Stage 1 Wasm validation must pass
**Scale/Scope**: ~15,973 lines → <8,000 lines, 309 compile-* functions, 111 quasiquote patterns

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | N/A | Refactoring only, no new type system changes |
| II. Lispオブジェクト表現規約 | N/A | No changes to object representation |
| III. 関数・クロージャ実装戦略 | N/A | No changes to closure implementation |
| IV. Wasm制御フロー活用 | N/A | No changes to control flow |
| V. シャローバインディングによる動的スコープ | N/A | No changes to dynamic scoping |
| VI. 段階的動的コンパイル（Tiered Eval/JIT） | N/A | No changes to eval/JIT |
| VII. テスト駆動開発（TDD）（非交渉） | ✅ PASS | Each removal batch validated by full test suite |
| VIII. Nix-Firstワークフロー | ✅ PASS | wasm-tools validate in devShell |
| IX. ANSI Common Lisp仕様参照規約 | ✅ PASS | HyperSpec links for any referenced CL functions |

**Constitution Status**: PASS - This is a code cleanup/refactoring task that doesn't introduce new functionality. TDD compliance ensured via batch validation.

## Project Structure

### Documentation (this feature)

```text
specs/001-codegen-cleanup/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (validation contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── func-section.lisp    # TARGET: 15,973 → <8,000 lines
│   │   ├── instruction-collector.lisp  # with-instruction-collector macro
│   │   └── primitive-dispatch.lisp     # *runtime-function-table*
│   └── ...
├── lib/
│   ├── string-runtime.lisp      # Migrated string functions
│   ├── numeric-runtime.lisp     # Migrated numeric functions
│   ├── sequence-runtime.lisp    # Migrated sequence functions
│   ├── list-runtime.lisp        # Migrated list functions
│   ├── io-runtime.lisp          # Migrated I/O functions
│   └── ffi-runtime.lisp         # Migrated FFI functions
└── ...

tests/
├── unit/                        # Unit tests (rove)
├── contract/                    # Wasm validation tests
└── integration/                 # End-to-end tests

dist/
├── clysm-stage1.wasm           # Stage 1 output (validation target)
└── stage1-report.json          # Compilation statistics
```

**Structure Decision**: Single project layout. All changes concentrated in `src/clysm/compiler/codegen/func-section.lisp` with validation against existing test suite and Stage 1 compilation.

## Complexity Tracking

No violations. This is a simplification/reduction task that removes complexity rather than adding it.
