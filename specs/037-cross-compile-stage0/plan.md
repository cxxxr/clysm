# Implementation Plan: Cross-Compile Stage 0 (Lisp-11)

**Branch**: `037-cross-compile-stage0` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/037-cross-compile-stage0/spec.md`

## Summary

Cross-compile the entire Clysm compiler (41 modules) from SBCL to a single WebAssembly binary (`clysm-stage0.wasm`) that can execute on wasmtime. The bootstrap script orchestrates compilation in dependency order, merges all functions/globals into a unified module, and exports a `compile` entry point. Verification confirms basic arithmetic, function definition, and control flow work correctly.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compilation; WasmGC for target output
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/validation modules
**Storage**: N/A (file-based: source files → single .wasm binary)
**Testing**: rove for unit tests; wasm-tools validate for Wasm validation; wasmtime for execution verification
**Target Platform**: WasmGC (WebAssembly with GC proposal) executed via wasmtime CLI
**Project Type**: Single project (compiler toolchain)
**Performance Goals**: Build completes within 5 minutes; Stage 0 compiles 1000 lines/100ms (per constitution)
**Constraints**: WasmGC-only (no linear memory); all 41 modules must compile; single unified binary
**Scale/Scope**: 41 source modules (~15,000 LOC); single binary output; 3 verification tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Output uses anyref, i31ref, struct, array - no linear memory |
| II. Lisp Object Representation | PASS | NIL singleton, UNBOUND sentinel already implemented |
| III. Function/Closure Strategy | PASS | Closure conversion with arity dispatch already works |
| IV. Wasm Control Flow | PASS | tail-call, try_table already implemented |
| V. Shallow Binding | PASS | Special variables with binding stack implemented |
| VI. Tiered Eval/JIT | DEFERRED | Stage 0 is compile-only; JIT deferred to Stage 1 |
| VII. TDD | PASS | Unit/contract/integration tests required |
| VIII. Nix-First | PASS | flake.nix exists with sbcl, wasm-tools, wasmtime |

**Violations requiring justification**: None. JIT is explicitly out-of-scope per spec.

## Project Structure

### Documentation (this feature)

```text
specs/037-cross-compile-stage0/
├── plan.md              # This file
├── research.md          # Phase 0: Technical decisions
├── data-model.md        # Phase 1: Module/binary structure
├── quickstart.md        # Phase 1: Build instructions
└── tasks.md             # Phase 2: Implementation tasks
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── backend/         # Wasm emission (leb128, sections, wasm-emit, wat-print)
│   ├── reader/          # Lisp reader (tokenizer, parser, reader)
│   ├── compiler/        # Compiler core (ast, env, analyzer/*, transform/*, codegen/*, compiler)
│   ├── runtime/         # Runtime support (objects, special-vars, multi-value, printer, condition-runtime)
│   ├── clos/            # CLOS implementation (9 modules)
│   ├── conditions/      # Condition system (6 modules)
│   └── validation/      # Existing: compiler-order.lisp, analyzer.lisp, etc.
└── build/
    └── bootstrap.lisp   # NEW: Bootstrap build script

dist/
└── clysm-stage0.wasm    # NEW: Output binary

tests/
├── unit/
│   └── bootstrap/       # NEW: Bootstrap script tests
├── contract/
│   └── stage0-wasm-test.lisp  # NEW: Wasm validation tests
└── integration/
    └── stage0-verify-test.lisp # NEW: Verification tests
```

**Structure Decision**: Single project layout with new `build/` and `dist/` directories for bootstrap artifacts.

## Complexity Tracking

> No violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | - | - |
