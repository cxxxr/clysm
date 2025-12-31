# Implementation Plan: Phase 13D M3 - CLOS Primitives for Wasm

**Branch**: `001-m3-clos-primitives` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-m3-clos-primitives/spec.md`

## Summary

Implement Wasm codegen for CLOS primitive functions ([slot-value](resources/HyperSpec/Body/f_slt_va.htm), [make-instance](resources/HyperSpec/Body/f_mk_ins.htm), type predicates) to enable DEFSTRUCT and DEFINE-CONDITION compilation. The compiler will emit WasmGC struct opcodes (`struct.get`, `struct.set`, `struct.new`, `ref.test`) directly, targeting the existing $instance (type 6) and $slot-vector (type 21) types.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), existing Clysm compiler infrastructure
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: Rove for unit tests, wasm-tools validate for Wasm validation
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler infrastructure)
**Performance Goals**: Maintain current Stage 1 generation speed
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: Target 25%+ compilation rate (up from 14.26%)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses `struct.get`/`struct.set`/`struct.new`/`ref.test` - native WasmGC opcodes |
| II. Lispオブジェクト表現規約 | PASS | Uses existing $instance (type 6) and $slot-vector (type 21) structs |
| III. 関数・クロージャ実装戦略 | N/A | Not modifying closure implementation |
| IV. Wasm制御フロー活用 | N/A | Not modifying control flow |
| V. シャローバインディング | N/A | Not modifying dynamic scoping |
| VI. 段階的動的コンパイル | N/A | Compile-time feature |
| VII. TDD (非交渉) | PASS | Tests required before implementation |
| VIII. Nix-Firstワークフロー | PASS | Uses existing Nix environment |
| IX. ANSI CL仕様参照規約 | PASS | HyperSpec links included for slot-value, make-instance |

## Project Structure

### Documentation (this feature)

```text
specs/001-m3-clos-primitives/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A - compiler internal)
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── func-section.lisp   # Add CLOS primitive compilation
│       ├── gc-types.lisp       # Existing type definitions (no changes)
│       └── emitter.lisp        # May need struct.* opcode support
├── clos/
│   ├── slot-access.lisp        # slot-value* (already exists)
│   ├── instance.lisp           # make-instance* (already exists)
│   └── mop.lisp                # standard-instance-p (already exists)
└── lib/
    └── defstruct.lisp          # DEFSTRUCT macro (already expands to CLOS primitives)

tests/
├── unit/
│   └── clos-primitives-test.lisp  # NEW: Unit tests for primitive compilation
├── contract/
│   └── clos-wasm-test.lisp        # NEW: Wasm bytecode contract tests
└── integration/
    └── defstruct-compile-test.lisp # NEW: DEFSTRUCT end-to-end tests
```

**Structure Decision**: Single project - this is a compiler enhancement adding codegen handlers for CLOS primitives within the existing `src/clysm/compiler/codegen/` structure.

## Complexity Tracking

> No violations - implementation uses existing type system and patterns.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
