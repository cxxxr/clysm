# Implementation Plan: func-section.lisp Refactoring

**Branch**: `001-func-section-refactor` | **Date**: 2026-01-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-func-section-refactor/spec.md`

## Summary

Refactor `src/clysm/compiler/codegen/func-section.lisp` from 16,483 lines to under 8,000 lines through:
1. Table-driven primitive dispatch replacing 200+ case branches
2. Runtime function migration for complex standard library functions
3. Efficient instruction collection (O(n) vs O(n²))
4. cXXr function consolidation via macro generation
5. Unified equality predicate infrastructure

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit/contract/integration tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single compiler project
**Performance Goals**: Compile 1000 lines/100ms; O(n) instruction collection
**Constraints**: Backward compatible; Stage 1 compilation rate ≥19.20%; ANSI CL semantics preserved
**Scale/Scope**: 16,483 lines → <8,000 lines; 318 compile-* functions → <150

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Refactoring preserves existing WasmGC type usage |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL/UNBOUND semantics unchanged |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Closure structure preserved |
| IV. Wasm制御フロー活用 | ✅ PASS | Control flow compilation unchanged |
| V. シャローバインディング | ✅ PASS | Special variable handling unchanged |
| VI. 段階的動的コンパイル | ✅ PASS | Eval/JIT infrastructure unchanged |
| VII. TDD（非交渉） | ✅ PASS | Each phase requires tests to pass |
| VIII. Nix-Firstワークフロー | ✅ PASS | Build environment unchanged |
| IX. ANSI CL仕様参照規約 | ✅ PASS | HyperSpec links required for migrated functions |

**Gate Status**: PASSED - No violations. Refactoring is internal architecture change.

## Project Structure

### Documentation (this feature)

```text
specs/001-func-section-refactor/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A - internal refactoring)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
├── func-section.lisp          # PRIMARY TARGET: 16,483 → <8,000 lines
├── primitive-dispatch.lisp    # NEW: Hash table dispatcher
├── primitive-registry.lisp    # NEW: Registration API
└── type-dispatch.lisp         # NEW: Unified type checking

src/clysm/lib/
├── io-runtime.lisp            # EXISTING: I/O functions
├── list-runtime.lisp          # EXISTING: List functions
├── sequence-runtime.lisp      # EXISTING: Sequence functions
├── string-runtime.lisp        # NEW: String functions migration
└── numeric-runtime.lisp       # NEW: Numeric functions migration

tests/
├── unit/
│   ├── primitive-dispatch-test.lisp    # NEW
│   ├── instruction-collector-test.lisp # NEW
│   └── cxr-consolidation-test.lisp     # NEW
├── contract/
│   └── primitive-dispatch-wasm-test.lisp # NEW
└── integration/
    └── func-section-refactor-test.lisp  # NEW
```

**Structure Decision**: Single project structure. Refactoring is internal to the compiler codegen module. New files extracted from func-section.lisp follow existing directory conventions.

## Complexity Tracking

> No constitution violations requiring justification. This is a simplification effort.

| Change | Complexity Impact | Rationale |
|--------|-------------------|-----------|
| Add primitive-dispatch.lisp | +1 file | Extracts 363-line function to dedicated module |
| Add primitive-registry.lisp | +1 file | Separates registration API from dispatch |
| Add type-dispatch.lisp | +1 file | Consolidates 800+ lines of equality code |
| Add string-runtime.lisp | +1 file | Migrates ~600 lines to runtime |
| Add numeric-runtime.lisp | +1 file | Migrates ~800 lines to runtime |
| Net reduction | -8,600 lines | func-section.lisp: 16,483 → ~7,883 lines |

## Phase Dependencies

```
Phase R4-1 (Code Generation Efficiency)
    │
    ▼
Phase R4-2 (Dispatch Table)
    │
    ├──→ Phase R4-3 (cXXr Consolidation)
    │
    ▼
Phase R4-4 (Equality Unification)
    │
    ▼
Phase R4-5 (String Runtime Migration)
    │
    ▼
Phase R4-6 (Numeric Runtime Migration)
    │
    ▼
Phase R4-7 (Sequence/Array Migration)
```
