# Implementation Plan: Instruction Collector Refactor

**Branch**: `001-instruction-collector-refactor` | **Date**: 2026-01-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-instruction-collector-refactor/spec.md`

## Summary

Refactor the instruction collection pattern in `func-section.lisp` from O(n²) append-based accumulation to O(n) push+nreverse pattern using a new `with-instruction-collector` macro. The macro provides local `emit` and `emit*` macros for ergonomic instruction emission. Migration proceeds incrementally starting with the largest functions (`compile-equalp` at 374 lines, `compile-primitive-call` at 363 lines), targeting 500+ lines of code reduction while maintaining 100% test compatibility and 19%+ Stage 1 compilation rate.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation, no persistence)
**Testing**: rove (unit tests in `tests/unit/`, contract tests in `tests/contract/`)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler project)
**Performance Goals**: O(n) instruction collection (vs current O(n²)), 500+ lines code reduction
**Constraints**: 19%+ Stage 1 compilation rate (current baseline), all existing tests pass, byte-identical Wasm output
**Scale/Scope**: ~16,500 lines in func-section.lisp, ~525-675 append patterns to migrate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | N/A | Internal refactor, no type system changes |
| II. Lispオブジェクト表現規約 | N/A | Internal refactor, no object representation changes |
| III. 関数・クロージャ実装戦略 | N/A | No closure changes |
| IV. Wasm制御フロー活用 | N/A | No control flow changes |
| V. シャローバインディング | N/A | No dynamic scope changes |
| VI. 段階的動的コンパイル | N/A | No eval/JIT changes |
| VII. TDD (MUST) | **REQUIRES** | Tests must be written before migration; macro tests required first |
| VIII. Nix-First | PASS | Using existing `nix flake check` infrastructure |
| IX. ANSI CL参照規約 | PASS | N/A for internal macro (no ANSI CL function being implemented) |

**Gate Result**: PASS (with TDD requirement noted)

## Project Structure

### Documentation (this feature)

```text
specs/001-instruction-collector-refactor/
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
│   │   ├── func-section.lisp    # PRIMARY TARGET (~16,500 lines, ~525 patterns)
│   │   └── instruction-collector.lisp  # NEW: with-instruction-collector macro
│   └── ...
└── lib/
    └── macros.lisp              # Alternative location for macro

tests/
├── unit/
│   └── instruction-collector-test.lisp  # NEW: macro unit tests
├── contract/
│   └── instruction-collector/           # NEW: behavior contract tests
└── integration/
    └── func-section-migration-test.lisp # NEW: migration validation tests
```

**Structure Decision**: Single project structure. New `instruction-collector.lisp` file in `src/clysm/compiler/codegen/` (colocated with `func-section.lisp` for easy import). Tests follow existing patterns with unit tests for macro behavior and contract tests for migration correctness.

## Complexity Tracking

No Constitution violations requiring justification. This is a pure refactoring with:
- No new external dependencies
- No architectural changes
- No new abstractions beyond the single macro

## Migration Strategy

### Phase 1: Macro Infrastructure
1. Create `with-instruction-collector` macro with `emit`/`emit*` local macros
2. Write unit tests validating macro behavior
3. Export from `clysm` package

### Phase 2: Pilot Migration
1. Migrate `compile-equalp` (line 4881, 374 lines)
2. Migrate `compile-primitive-call` (line 1173, 363 lines)
3. Verify all tests pass after each function

### Phase 3: Incremental Migration
1. Migrate remaining functions by size (largest first)
2. Run tests after each batch
3. Track line count reduction

### Phase 4: Cleanup & Verification
1. Remove any legacy patterns
2. Final line count measurement
3. Stage 1 compilation rate verification
4. Byte-identical Wasm output verification
