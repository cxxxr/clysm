# Implementation Plan: Stage 1 Compiler Generation

**Branch**: `039-stage1-compiler-gen` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/039-stage1-compiler-gen/spec.md`

## Summary

Implement infrastructure to execute Stage 0 Wasm compiler on wasmtime, enable self-compilation of Clysm source code, and generate a Stage 1 binary. Key components: Stage 0 runtime verification, source file reading protocol, compilation progress measurement, blocker analysis, and binary diff tools. Success metrics: 25% form compilation coverage (up from 19.6%), valid Stage 1 binary generation.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host tooling; WasmGC for Stage 0 output
**Primary Dependencies**: wasmtime (Wasm runtime), wasm-tools (validation), Node.js (FFI host shim)
**Storage**: File-based (source files, Wasm binaries, JSON progress reports)
**Testing**: Rove (unit tests), shell scripts (integration verification)
**Target Platform**: wasmtime on Linux (WASI-compatible)
**Project Type**: Single project (compiler infrastructure extension)
**Performance Goals**: Compile 849+ forms in <60 seconds; reports in <5 seconds
**Constraints**: Stage 0 binary limited to 19.6% CL subset; must handle unsupported forms gracefully
**Scale/Scope**: 41 source modules, ~849 top-level forms

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Stage 0/1 use anyref, i31ref, GC types |
| II. NIL/UNBOUND Representation | PASS | Singleton structs in Stage 0 |
| III. Closure Implementation | PASS | Arity-dispatch structure in Stage 0 |
| IV. Wasm Control Flow | PASS | tail_call, try_table in Stage 0 |
| V. Shallow Binding | PASS | Special vars use symbol $value slot |
| VI. Tiered Eval/JIT | N/A | This feature extends compilation, not eval |
| VII. TDD (Non-Negotiable) | PASS | Tests before implementation |
| VIII. Nix-First | PASS | All tools via nix develop |

**All gates pass. Proceeding to Phase 0.**

## Project Structure

### Documentation (this feature)

```text
specs/039-stage1-compiler-gen/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── compiler/          # Existing compiler (used by Stage 0)
│   ├── validation/        # Existing: compiler-order.lisp, analyzer.lisp
│   └── stage1/            # NEW: Stage 1 generation infrastructure
│       ├── package.lisp
│       ├── runner.lisp    # wasmtime execution wrapper
│       ├── progress.lisp  # Compilation progress tracking
│       ├── blocker.lisp   # Blocker analysis
│       └── diff.lisp      # Binary diff analysis
├── ...

build/
├── bootstrap.lisp         # Existing: Stage 0 bootstrap script
└── stage1-gen.lisp        # NEW: Stage 1 generation script

host-shim/
├── verify-stage0.js       # Existing: Stage 0 verification
├── fs-shim.js             # Existing: Filesystem FFI
├── stage1-host.js         # NEW: Stage 1 compilation host
└── ...

dist/
├── clysm-stage0.wasm      # Existing: Stage 0 binary
└── clysm-stage1.wasm      # NEW: Stage 1 binary (output)

scripts/
├── verify-stage0.sh       # Existing
├── run-stage1-gen.sh      # NEW: Stage 1 generation script
└── diff-stages.sh         # NEW: Binary diff script

tests/
├── unit/
│   └── stage1/            # NEW: Stage 1 unit tests
├── contract/
│   └── stage1-*.lisp      # NEW: Wasm validation tests
└── integration/
    └── stage1-*.lisp      # NEW: End-to-end tests
```

**Structure Decision**: Extends existing single project structure. New `src/clysm/stage1/` module for Stage 1 infrastructure, `build/stage1-gen.lisp` for generation script, `host-shim/stage1-host.js` for FFI host during self-compilation.

## Complexity Tracking

No constitution violations requiring justification. All components align with existing patterns.
