# Implementation Plan: Stage 0 Complete Compiler

**Branch**: `045-stage0-complete-compiler` | **Date**: 2025-12-28 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/045-stage0-complete-compiler/spec.md`

## Summary

Implement a complete Stage 0 compiler that runs on wasmtime and can compile Clysm source code to produce Stage 1, enabling fixed-point verification (Stage 1 == Stage 2). The current Stage 0 is an 8-byte placeholder; this feature replaces it with a fully functional compiler binary that exports `compile_form` and `compile_all` functions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for bootstrap, WasmGC for Stage 0 output
**Primary Dependencies**: wasmtime (Wasm runtime), wasm-tools (validation), Node.js (host shim)
**Storage**: File-based (source files → Wasm binaries)
**Testing**: rove (CL unit tests), wasm-tools validate (contract tests), shell scripts (integration)
**Target Platform**: WebAssembly GC (wasmtime)
**Project Type**: Single (compiler project)
**Performance Goals**: Compile 45 modules in < 5 minutes, produce binary < 5MB
**Constraints**: WasmGC-only (no linear memory), blessed subset of CL features
**Scale/Scope**: 45 source modules, ~900 top-level forms to compile

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | All Lisp objects use WasmGC types (i31ref, struct, array) |
| II. Lisp Object Representation | PASS | NIL as singleton struct, UNBOUND sentinel |
| III. Function/Closure Strategy | PASS | Closure struct with multi-arity dispatch |
| IV. Wasm Control Flow | PASS | Uses return_call, try_table for TCO and exceptions |
| V. Shallow Binding | PASS | Special vars via symbol $value field |
| VI. Tiered Eval/JIT | PASS | Stage 0 uses Tier 2 (compiled Wasm) |
| VII. TDD (Non-negotiable) | PASS | Tests written before implementation |
| VIII. Nix-First Workflow | PASS | nix develop provides all tools |

**Gate Result**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/045-stage0-complete-compiler/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── bootstrap/           # Stage 0 generation (Feature 044)
│   │   ├── interpreter-stage0.lisp
│   │   └── fixpoint.lisp
│   ├── stage0/              # NEW: Stage 0 complete compiler
│   │   ├── package.lisp     # Package definition
│   │   ├── entry.lisp       # compile_form, compile_all exports
│   │   ├── reader.lisp      # S-expression reader (Wasm)
│   │   ├── compiler.lisp    # Core compilation (Wasm)
│   │   ├── codegen.lisp     # Wasm binary generation
│   │   └── runtime.lisp     # Runtime initialization
│   ├── compiler/            # Existing compiler (SBCL)
│   │   ├── codegen/
│   │   │   ├── gc-types.lisp
│   │   │   ├── func-section.lisp
│   │   │   └── type-section.lisp
│   │   └── compiler.lisp
│   ├── stage1/              # Stage 1 generation infrastructure
│   └── stage2/              # Stage 2/fixpoint verification

build/
├── bootstrap.lisp           # SBCL-based Stage 0 generation
├── bootstrap-interp.lisp    # Interpreter-based Stage 0 generation
└── stage0-complete.lisp     # NEW: Complete Stage 0 generation

host-shim/
├── stage1-host.js           # FFI host for wasmtime
└── fs-shim.js               # Filesystem FFI

dist/
├── clysm-stage0.wasm        # Stage 0 binary (currently 8 bytes)
├── clysm-stage1.wasm        # Stage 1 binary
└── clysm-stage2.wasm        # Stage 2 binary

scripts/
├── verify-fixpoint.sh       # Fixpoint verification
└── verify-fixpoint-interp.sh

tests/
├── unit/stage0/             # NEW: Stage 0 unit tests
├── contract/stage0-*.lisp   # NEW: Contract tests
└── integration/stage0-*.lisp # NEW: Integration tests
```

**Structure Decision**: Single project structure. New code goes in `src/clysm/stage0/` for the complete Stage 0 compiler implementation. Build scripts in `build/`. Tests follow existing pattern in `tests/`.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Full compiler in Wasm | Self-hosting requires compiler to run on wasmtime | Partial compilation doesn't achieve fixed-point |
| New src/clysm/stage0/ | Separates Wasm-native code from SBCL-hosted code | Mixing in existing dirs would conflate concerns |
