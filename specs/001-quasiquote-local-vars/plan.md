# Implementation Plan: Quasiquote Local Variable Compilation

**Branch**: `001-quasiquote-local-vars` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-quasiquote-local-vars/spec.md`

## Summary

Enable Clysm compiler to properly compile [quasiquote](resources/HyperSpec/Body/02_df.htm) expressions containing local variable references. When backquote expressions like `` `(,x ,y) `` contain unquoted variables, the compiler must resolve them from the lexical environment and emit Wasm `local.get` instructions. Supports both [unquote](resources/HyperSpec/Body/02_df.htm) (`,var`) and unquote-splicing (`,@list`) forms. Eliminates "Cannot compile quoted element" errors blocking macro-generated code patterns.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: Rove test framework with contract tests for Wasm validation
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single compiler project
**Performance Goals**: Compile quasiquote forms without measurable overhead vs regular list construction
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: Impacts ~16 quasiquote-related compilation failures in Stage 1 generation

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses existing WasmGC cons/list primitives |
| II. Lispオブジェクト表現規約 | PASS | NIL/symbols handled via existing conventions |
| III. 関数・クロージャ実装戦略 | N/A | Feature doesn't modify closure handling |
| IV. Wasm制御フロー活用 | N/A | No control flow changes |
| V. シャローバインディング | N/A | Only lexical variables affected |
| VI. 段階的動的コンパイル | N/A | Static compilation only |
| VII. TDD（非交渉） | PASS | Tests first, then implementation |
| VIII. Nix-Firstワークフロー | PASS | Uses existing Nix environment |
| IX. ANSI CL仕様参照 | PASS | HyperSpec links included |

**Gate Status**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/001-quasiquote-local-vars/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Wasm validation contracts)
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # AST node definitions (existing)
│   ├── env.lisp                    # Lexical environment (existing)
│   ├── transform/
│   │   └── macro.lisp              # Quasiquote expansion (MODIFY: lines 264-358)
│   └── codegen/
│       └── func-section.lisp       # Code generation (MODIFY: lines 694-729)
└── lib/
    └── list-runtime.lisp           # List construction helpers (existing)

tests/
├── unit/
│   └── quasiquote-local-test.lisp  # NEW: Unit tests for this feature
├── contract/
│   └── quasiquote-wasm-test.lisp   # NEW: Wasm validation contracts
└── integration/
    └── quasiquote-runtime-test.lisp # NEW: Runtime correctness tests
```

**Structure Decision**: Single compiler project structure. Modifications concentrated in `transform/macro.lisp` (expansion) and `codegen/func-section.lisp` (code generation). Three new test files following TDD.

## Complexity Tracking

> No Constitution Check violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | - | - |
