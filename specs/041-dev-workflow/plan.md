# Implementation Plan: Development Workflow Establishment

**Branch**: `041-dev-workflow` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/041-dev-workflow/spec.md`

## Summary

Implement a self-hosting development workflow for Clysm, enabling developers to compile source files without SBCL. The feature provides:
1. CLI interface (`./clysm compile src/**/*.lisp -o output.wasm`)
2. Incremental compilation with dependency tracking
3. Error recovery (continue on failure)
4. REPL integration (`compile-file`)

Building on existing Stage 1 infrastructure (41-module pipeline, form-level compilation, error recovery), this feature adds CLI argument parsing, glob pattern expansion, dependency graph construction, and persistent caching.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host; WasmGC for Stage 1+ binaries
**Primary Dependencies**:
- Existing: clysm/stage1 (reader, generator, runner), clysm/filesystem (FFI), clysm/compiler
- New: Command-line argument parsing, glob pattern matching
**Storage**: Compilation cache in `.clysm-cache/` directory (S-expression format)
**Testing**: rove (test framework), wasmtime (Wasm execution), wasm-tools (validation)
**Target Platform**: Wasm (wasmtime runtime on Linux/macOS/Windows)
**Project Type**: Single project (Common Lisp compiler producing Wasm)
**Performance Goals**:
- Incremental compilation: <5 seconds for single file change
- Full compilation (45+ modules): <60 seconds
**Constraints**:
- WasmGC-only (no linear memory)
- Must work without SBCL when using Stage 1+ binary
- UTF-8 source files only
**Scale/Scope**: 45+ modules, ~15,000 LOC of Clysm source

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | All compilation output uses GC types, no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL/UNBOUND handling preserved from existing infrastructure |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Closure structure unchanged, compilation reuses existing codegen |
| IV. Wasm制御フロー活用 | ✅ PASS | Tail call, exception handling patterns preserved |
| V. シャローバインディング | ✅ PASS | Special variable handling unchanged |
| VI. 段階的動的コンパイル | ✅ PASS | Feature extends batch compilation, Tier 1/2 preserved for REPL |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | All new code must follow Red-Green-Refactor |
| VIII. Nix-Firstワークフロー | ✅ REQUIRED | CLI wrapper must work within `nix develop` environment |

**Security Constraints Check**:
- ✅ Wasm sandbox maintained (no linear memory access)
- ✅ Filesystem access via FFI shim only (host-controlled)
- ✅ No arbitrary code execution outside compilation

**All gates PASS. Proceed to Phase 0.**

## Project Structure

### Documentation (this feature)

```text
specs/041-dev-workflow/
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
│   ├── workflow/              # NEW: Development workflow module
│   │   ├── package.lisp       # Package definition
│   │   ├── types.lisp         # SourceModule, DependencyGraph, etc.
│   │   ├── platform.lisp      # Platform abstraction (glob via FFI)
│   │   ├── deps.lisp          # Dependency analysis & topological sort
│   │   ├── cache.lisp         # Compilation cache persistence
│   │   ├── compiler.lisp      # Main compile-project function
│   │   └── repl.lisp          # compile-file REPL integration
│   ├── cli/                   # NEW: CLI interface
│   │   ├── package.lisp       # Package definition
│   │   ├── args.lisp          # Argument parsing
│   │   └── main.lisp          # Entry point for ./clysm command
│   ├── stage1/                # EXISTING: Extended for self-hosting
│   ├── stage2/                # EXISTING: Extended for self-hosting
│   └── filesystem/            # EXISTING: FFI for file I/O
├── build/
│   └── workflow.lisp          # SBCL-based CLI entry (bootstrapping)
└── bin/
    └── clysm                  # Shell wrapper script

host-shim/
├── workflow-host.js           # Node.js host shim for Stage 1+ execution
└── fs-shim.js                 # EXISTING: Filesystem FFI

scripts/
└── clysm-compile.sh           # CLI wrapper for wasmtime invocation

tests/
├── unit/
│   └── workflow/              # Unit tests for workflow module
├── contract/
│   └── workflow-*.lisp        # Contract tests for CLI & caching
└── integration/
    └── workflow-*.lisp        # End-to-end compilation tests
```

**Structure Decision**: Single project extending existing `src/clysm/` hierarchy. New `workflow/` and `cli/` submodules provide clean separation while reusing existing `stage1/`, `stage2/`, and `filesystem/` infrastructure.

## Complexity Tracking

No Constitution violations requiring justification. Feature extends existing architecture without adding new abstraction layers.

## Phase 0 Research Areas

### R1: CLI Argument Parsing Strategy

**Question**: How to parse CLI arguments in Clysm without external dependencies?
- Option A: Minimal hand-written parser (LL(1) grammar for --flag=value)
- Option B: Lisp macros generating argument specs
- Option C: Port of an existing CL library (e.g., clingon subset)

**Impact**: Affects CLI usability and maintenance burden

### R2: Glob Pattern Implementation

**Question**: How to implement glob pattern matching (`**/*.lisp`) in Wasm?
- Option A: Port of cl-ppcre subset for pattern matching
- Option B: Simple recursive wildcard matching (home-grown)
- Option C: Host-side glob expansion via FFI

**Impact**: Affects both SBCL bootstrap and Stage 1+ execution paths

### R3: Dependency Discovery Algorithm

**Question**: How to extract dependencies from source files?
- Option A: Parse `in-package` and `require/load` forms only
- Option B: Full symbol reference tracking
- Option C: Explicit dependency declarations (defmodule-style)

**Impact**: Affects accuracy of incremental recompilation

### R4: Cache Serialization Format

**Question**: What format for compilation cache persistence?
- Option A: S-expression (simple, Lisp-native)
- Option B: JSON (portable, tool-friendly)
- Option C: Binary (compact, fast load)

**Impact**: Affects startup time and cross-platform compatibility

### R5: Self-Hosting Bootstrap Strategy

**Question**: How to transition from SBCL-based to Stage 1-based compilation?
- Existing SBCL path: `sbcl --load build/bootstrap.lisp`
- Need: Equivalent functionality in Stage 1 binary

**Impact**: Critical for self-hosting goal

## Phase 1 Design Outputs

**Status**: ✅ Complete (2025-12-27)

All research areas resolved in [research.md](./research.md). Generated artifacts:
1. **[data-model.md](./data-model.md)**: SourceModule, DependencyGraph, CompilationCache schemas
2. **[contracts/cli.md](./contracts/cli.md)**: Command-line interface specification
3. **[contracts/compile-file.md](./contracts/compile-file.md)**: REPL API specification
4. **[quickstart.md](./quickstart.md)**: Developer onboarding guide

## Constitution Check (Post-Design)

*Re-evaluation after Phase 1 design completion.*

| Principle | Status | Verification |
|-----------|--------|--------------|
| I. WasmGC-First型システム設計 | ✅ PASS | Data model uses structs/arrays, no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | No changes to NIL/UNBOUND handling |
| III. 関数・クロージャ実装戦略 | ✅ PASS | CLI uses standard function calls |
| IV. Wasm制御フロー活用 | ✅ PASS | Error handling uses condition system |
| V. シャローバインディング | ✅ PASS | Cache uses normal lexical bindings |
| VI. 段階的動的コンパイル | ✅ PASS | compile-file integrates with Tier 1/2 |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Test plan in contracts defines acceptance criteria |
| VIII. Nix-Firstワークフロー | ✅ REQUIRED | quickstart.md prioritizes `nix develop` |

**Security Re-check**:
- ✅ S-expression cache format is human-readable and auditable
- ✅ Host-side glob expansion prevents path traversal via FFI boundary
- ✅ compile-file restarts are opt-in, no automatic code execution

**All gates PASS. Ready for Phase 2 task generation (/speckit.tasks).**

## Next Steps

Run `/speckit.tasks` to generate the implementation task breakdown in `tasks.md`.
