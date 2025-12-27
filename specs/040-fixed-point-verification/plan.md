# Implementation Plan: Fixed-Point Verification (Phase 13B)

**Branch**: `040-fixed-point-verification` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/040-fixed-point-verification/spec.md`

## Summary

Implement fixed-point verification to prove Clysm compiler self-hosting capability. The system runs Stage 1 Wasm binary (from feature 039) on wasmtime to compile Clysm source code, producing Stage 2. Byte-identical match between Stage 1 and Stage 2 proves the compiler can reproduce itself. Extends existing `src/clysm/stage1/` infrastructure with Stage 2 generation, byte-level comparison, and automated verification workflow.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host tooling; WasmGC for Stage 1/2 output
**Primary Dependencies**: wasmtime (Wasm runtime), wasm-tools (validation), Node.js (FFI host shim), alexandria, uiop
**Storage**: File-based (source files → Wasm binaries, JSON reports)
**Testing**: rove (unit tests), contract tests (Wasm validation), integration tests (end-to-end)
**Target Platform**: Linux/macOS CLI environment with wasmtime installed
**Project Type**: Single project (extends existing clysm/stage1 module)
**Performance Goals**: Full verification completes within 5 minutes (SC-001)
**Constraints**: Byte-identical comparison (no structural comparison fallback)
**Scale/Scope**: ~45 compiler source modules, ~1-5MB Wasm binaries

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Stage 1/2 are WasmGC binaries, no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | Not directly relevant - verification tooling |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Not directly relevant - verification tooling |
| IV. Wasm制御フロー活用 | ✅ PASS | Not directly relevant - verification tooling |
| V. シャローバインディング | ✅ PASS | Not directly relevant - verification tooling |
| VI. 段階的動的コンパイル | ✅ PASS | Stage 1 executes on wasmtime per Tier 2 model |
| VII. テスト駆動開発（TDD） | ✅ PASS | Unit/contract/integration tests required |
| VIII. Nix-Firstワークフロー | ✅ PASS | wasmtime, wasm-tools in devShell |
| WAT/WASM検証 | ✅ PASS | FR-008 requires wasm-tools validation |

**Result**: All gates PASS. No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/040-fixed-point-verification/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (CLI interface contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   └── stage1/                  # Existing module (from 039)
│       ├── package.lisp         # Package definition (extend)
│       ├── types.lisp           # Data structures (extend)
│       ├── conditions.lisp      # Error conditions (extend)
│       ├── diff.lisp            # Binary diff (extend for byte-level)
│       ├── runner.lisp          # Wasmtime execution (extend)
│       ├── generator.lisp       # Stage generation (extend for Stage 2)
│       └── fixpoint.lisp        # NEW: Fixed-point verification logic
│   └── stage2/                  # NEW: Stage 2 generation module
│       ├── package.lisp         # Package definition
│       ├── generator.lisp       # Stage 2 binary generation
│       └── verifier.lisp        # Fixed-point verification

build/
├── stage1-gen.lisp              # Existing: Stage 1 generation entry point
└── stage2-gen.lisp              # NEW: Stage 2 generation entry point

scripts/
├── verify-fixpoint.sh           # NEW: Fixed-point verification script
├── run-stage2-gen.sh            # NEW: Stage 2 generation script
└── diff-stages.sh               # Existing: Binary diff script (extend)

host-shim/
├── stage1-host.js               # Existing: FFI host shim (extend for Stage 2)
└── stage2-host.js               # NEW: Stage 2 execution host shim

dist/
├── clysm-stage1.wasm            # Stage 1 binary (from 039)
└── clysm-stage2.wasm            # NEW: Stage 2 binary output

tests/
├── unit/
│   └── fixpoint/                # NEW: Fixed-point unit tests
│       ├── byte-compare-test.lisp
│       ├── stage2-gen-test.lisp
│       └── verifier-test.lisp
├── contract/
│   └── fixpoint-*.lisp          # NEW: Wasm validation tests
└── integration/
    └── fixpoint-*.lisp          # NEW: End-to-end verification tests
```

**Structure Decision**: Extends existing `src/clysm/stage1/` module with new `fixpoint.lisp` for verification logic. Adds `src/clysm/stage2/` submodule for Stage 2 generation. Uses existing infrastructure from 039 (diff.lisp, runner.lisp, generator.lisp) with extensions.

## Complexity Tracking

> No violations requiring justification.

| Area | Decision | Rationale |
|------|----------|-----------|
| Stage 2 module | New `stage2/` submodule | Cleaner separation from Stage 1 generation logic |
| Host shim | Extend existing | Minimal changes needed for Stage 2 execution |
