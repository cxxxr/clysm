# Implementation Plan: Sequence and Array Runtime Migration

**Branch**: `001-sequence-array-runtime` | **Date**: 2026-01-04 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-sequence-array-runtime/spec.md`

## Summary

Migrate `subseq` and `adjust-array` from inline Wasm codegen (~175 lines in func-section.lisp) to pure Lisp runtime library functions. The migration follows the established pattern from 001-string-runtime-migration and 001-sequence-runtime-migration, using `*runtime-function-table*` for dispatch. Key challenges include UTF-8 character boundary handling for strings and type-specific behavior for lists, vectors, and arrays.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target runtime
**Primary Dependencies**: alexandria, babel (UTF-8), existing clysm compiler infrastructure, string-runtime.lisp (UTF-8 helpers)
**Storage**: N/A (in-memory compilation, no persistence)
**Testing**: rove (unit tests), wasm-tools validate (Wasm validation), Stage 1 compilation verification
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler codebase)
**Performance Goals**: Runtime functions should be equivalent to inline codegen; no measurable regression
**Constraints**: Layer 1 primitives only (char, aref, length, make-array, cons, etc.); MVP for adjust-array is 1D arrays only
**Scale/Scope**: ~300 lines of new Lisp code; ~175 lines removed from func-section.lisp

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Runtime functions compile to WasmGC; uses anyref, struct types |
| II. Lispオブジェクト表現規約 | PASS | NIL handling via singleton pattern; sequences use proper type representation |
| III. 関数・クロージャ実装戦略 | PASS | Runtime functions registered in closure structure via dispatch table |
| IV. Wasm制御フロー活用 | PASS | No special control flow requirements; standard function calls |
| V. シャローバインディングによる動的スコープ | N/A | No dynamic variable usage in these functions |
| VI. 段階的動的コンパイル | PASS | Functions compile normally via Tier 1/2; no special eval behavior |
| VII. テスト駆動開発（TDD） | PASS | Unit tests required before implementation per SC-006 |
| VIII. Nix-Firstワークフロー | PASS | Uses existing Nix development environment; wasm-tools in devShell |
| IX. ANSI Common Lisp仕様参照規約 | PASS | HyperSpec links required for [subseq](resources/HyperSpec/Body/f_subseq.htm) and [adjust-array](resources/HyperSpec/Body/f_adjust.htm) |

**Gate Result**: PASS - All applicable principles satisfied

## Project Structure

### Documentation (this feature)

```text
specs/001-sequence-array-runtime/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (function signatures)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   ├── sequence-runtime.lisp       # Existing - may extend with subseq
│   ├── string-runtime.lisp         # UTF-8 helpers (reuse)
│   └── [new or extended file]      # subseq-rt, adjust-array-rt
├── compiler/
│   └── codegen/
│       ├── func-section.lisp       # Remove compile-subseq, compile-adjust-array
│       └── primitive-registry.lisp # May need updates

tests/
├── unit/
│   └── sequence-array-runtime-test.lisp  # New test file
├── contract/
└── integration/
```

**Structure Decision**: Extend existing `src/clysm/lib/sequence-runtime.lisp` with `subseq-rt` and `adjust-array-rt` functions, following the pattern established by other runtime migrations. Unit tests in `tests/unit/sequence-array-runtime-test.lisp`.

## Complexity Tracking

> No constitution violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | N/A | N/A |
