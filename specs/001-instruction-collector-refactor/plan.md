# Implementation Plan: Instruction Collector Refactor

**Branch**: `001-instruction-collector-refactor` | **Date**: 2026-01-03 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-instruction-collector-refactor/spec.md`

## Summary

Migrate the two largest functions in func-section.lisp (`compile-equalp` at 374 lines and `compile-primitive-call` at 363 lines) from O(n²) append-based list construction to O(n) push+nreverse pattern using the existing `with-instruction-collector` macro. The migration must maintain byte-identical Wasm output verified by contract tests.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), contract tests (Wasm bytecode comparison), wasm-tools (validation)
**Target Platform**: WasmGC
**Project Type**: Single (compiler)
**Performance Goals**: O(n) instruction collection vs O(n²) append-based
**Constraints**: Byte-identical Wasm output, Stage 1 compilation rate >= 24%
**Scale/Scope**: 128 append patterns (`,@`) in func-section.lisp (16,137 lines)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | No change to type system |
| II. Lispオブジェクト表現規約 | PASS | No change to object representation |
| III. 関数・クロージャ実装戦略 | PASS | No change to closure handling |
| IV. Wasm制御フロー活用 | PASS | No change to control flow |
| V. シャローバインディング | PASS | No change to dynamic scoping |
| VI. 段階的動的コンパイル | PASS | Refactoring only, no JIT changes |
| VII. テスト駆動開発（TDD） | PASS | Unit tests exist, contract tests planned |
| VIII. Nix-Firstワークフロー | PASS | No build system changes |
| IX. ANSI CL仕様参照規約 | N/A | No new ANSI CL features implemented |

**Gate Result**: PASS - All constitution principles satisfied or not applicable.

## Project Structure

### Documentation (this feature)

```text
specs/001-instruction-collector-refactor/
├── plan.md              # This file
├── research.md          # Phase 0 output (complete)
├── data-model.md        # Phase 1 output (complete)
├── quickstart.md        # Phase 1 output (complete)
├── contracts/
│   └── macro-interface.md  # Macro contract (complete)
├── checklists/
│   └── requirements.md  # Quality checklist (complete)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── func-section.lisp      # Target: 16,137 lines, 128 append patterns
│       └── instruction-collector.lisp  # Macro implementation (50 lines)
└── package.lisp                    # Exports with-instruction-collector

tests/
├── unit/
│   └── instruction-collector-test.lisp  # 7 tests for macro
├── contract/
│   └── instruction-collector/           # Wasm comparison tests (TBD)
└── integration/
    └── bootstrap/                       # Stage 1 verification
```

**Structure Decision**: Single project layout. All changes confined to `src/clysm/compiler/codegen/` with corresponding tests.

## Complexity Tracking

No constitution violations requiring justification.

## Phase 0: Research (Complete)

Research artifacts generated and validated:
- [research.md](research.md) - All NEEDS CLARIFICATION resolved

Key decisions:
1. **Pattern identification**: 128 `,@` (unquote-splicing) patterns identified
2. **Macro design**: `macrolet` with `emit`/`emit*` local macros (implemented)
3. **Implementation**: push + nreverse for O(n) complexity (implemented)
4. **Migration order**: Size-based, largest first (`compile-equalp`, `compile-primitive-call`)
5. **Test strategy**: Unit → Contract → Integration

## Phase 1: Design (Complete)

Design artifacts generated:
- [data-model.md](data-model.md) - Instruction collector entity model
- [contracts/macro-interface.md](contracts/macro-interface.md) - Macro API contract
- [quickstart.md](quickstart.md) - Migration guide

Key entities:
1. **Instruction Collector**: Lexically-scoped accumulator (push-based)
2. **Wasm Instruction**: S-expression format (`(:opcode operand...)` or bare `:opcode`)
3. **Instruction List**: Ordered sequence returned by collector

## Next Steps

Run `/speckit.tasks` to generate task breakdown for implementation phase:
1. Contract tests for `compile-equalp` baseline capture
2. Contract tests for `compile-primitive-call` baseline capture
3. Migrate `compile-equalp` to instruction collector
4. Migrate `compile-primitive-call` to instruction collector
5. Verify byte-identical output
6. Measure and report append pattern reduction
