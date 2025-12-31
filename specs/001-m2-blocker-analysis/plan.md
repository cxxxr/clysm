# Implementation Plan: Phase 13D Milestone M2 - Blocker Analysis

**Branch**: `001-m2-blocker-analysis` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-m2-blocker-analysis/spec.md`

## Summary

Improve Clysm's self-hosting compilation rate from 13.9% to 25%+ by regenerating the Stage 1 report, analyzing DEFUN compilation failures to identify root causes (TAGBODY/GO, complex lambda-lists, unregistered primitives), and implementing fixes for the highest-impact blockers.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing), wasm-tools (validation)
**Storage**: N/A (in-memory compilation, Wasm binary output to dist/)
**Testing**: rove (unit tests), wasm-tools validate (contract tests), integration tests via shell scripts
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler project)
**Performance Goals**: N/A for this milestone (analysis and fix task)
**Constraints**: Compilation rate must reach 25%+ (from 13.9% baseline)
**Scale/Scope**: 26,571 total forms, currently 3,631 compiled, target ~6,643 compiled (25%)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Compiler generates WasmGC types |
| II. Lispオブジェクト表現規約 | PASS | NIL/UNBOUND handling in place |
| III. 関数・クロージャ実装戦略 | PASS | Closure conversion implemented |
| IV. Wasm制御フロー活用 | APPLICABLE | TAGBODY/GO fix needs `block`/`br` instructions |
| V. シャローバインディング | N/A | Not directly affected |
| VI. 段階的動的コンパイル | N/A | Not directly affected |
| VII. TDD（非交渉） | MUST | All fixes must have tests first |
| VIII. Nix-Firstワークフロー | MUST | Use `nix develop` for all work |
| IX. ANSI CL仕様参照規約 | MUST | HyperSpec links for TAGBODY, GO, lambda-list |

**Gate Status**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/001-m2-blocker-analysis/
├── plan.md              # This file
├── research.md          # Phase 0 output - blocker analysis
├── data-model.md        # Phase 1 output - report structure
├── quickstart.md        # Phase 1 output - how to run analysis
├── contracts/           # Phase 1 output - report JSON schema
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/         # Wasm code generation (fix targets)
│   ├── transform/       # AST transformations
│   └── analyzer/        # Code analysis passes
├── lib/
│   └── macros.lisp      # TAGBODY/GO macro support (if applicable)
├── stage0/              # Stage 0 compiler infrastructure
└── stage1/              # Stage 1 generation and verification

build/
├── stage1-complete.lisp # Stage 1 generation entry point
└── analyze-defun-failures.lisp # DEFUN failure analysis tool

dist/
├── clysm-stage1.wasm    # Generated Stage 1 binary (27KB current)
└── stage1-report.json   # Compilation report (current baseline)

tests/
├── unit/                # Unit tests for fixes
├── contract/            # Wasm validation tests
└── integration/         # End-to-end bootstrap tests
```

**Structure Decision**: Single project structure matching existing Clysm layout. No new directories needed; fixes go into existing codegen modules.

## Complexity Tracking

> No Constitution Check violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none)    | -          | -                                   |
