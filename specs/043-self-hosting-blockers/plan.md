# Implementation Plan: Self-Hosting Blockers Resolution

**Branch**: `043-self-hosting-blockers` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/043-self-hosting-blockers/spec.md`

## Summary

Extend the Clysm compiler to achieve 50%+ Stage 0 compilation rate by implementing missing CL features that block self-hosting: LOOP macro completion, &optional/:key default values, hash table operations, list functions, and sequence functions with :key/:test options. All implementations use WasmGC types per constitution.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler; WasmGC for target output
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/lib/macros
**Storage**: N/A (in-memory compile-time; WasmGC struct/array for runtime hash tables)
**Testing**: rove (unit, contract, integration tests); wasm-tools validate; wasmtime execution
**Target Platform**: WebAssembly GC (WasmGC)
**Project Type**: single
**Performance Goals**: Compilation: 1000 lines/100ms; Runtime: hash table O(1) access, list functions O(n)
**Constraints**: WasmGC-First (no linear memory), ANSI CL compatibility, existing LOOP infrastructure in lib/macros.lisp
**Scale/Scope**: ~45 compiler source modules, 50%+ form compilation rate target (current baseline ~20%)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Hash tables use WasmGC struct/array types (not linear memory) |
| II. Lisp Object Representation | PASS | NIL as singleton struct, UNBOUND for hash table missing keys |
| III. Closure Implementation | PASS | No new closure patterns required |
| IV. Wasm Control Flow | PASS | Existing tagbody/go from LOOP infrastructure |
| V. Shallow Binding | PASS | No special variable changes |
| VI. Tiered Eval/JIT | PASS | No changes to eval/jit system |
| VII. TDD (Non-Negotiable) | PASS | Unit/contract/integration tests for all features |
| VIII. Nix-First | PASS | Uses existing flake.nix, wasm-tools, wasmtime |

**Gate Result**: PASS - No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/043-self-hosting-blockers/
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
│   ├── lib/
│   │   ├── macros.lisp      # LOOP macro extension (existing file)
│   │   ├── hash-table.lisp  # NEW: Hash table operations
│   │   ├── list-ops.lisp    # NEW: List functions (assoc, member, etc.)
│   │   └── sequence-ext.lisp # NEW: Extended sequence functions
│   └── compiler/
│       ├── ast.lisp         # AST nodes for new forms
│       └── codegen/
│           └── func-section.lisp # Codegen for new functions

tests/
├── contract/
│   └── self-hosting-wasm-test.lisp  # Wasm validation
├── integration/
│   └── self-hosting-test.lisp       # End-to-end compilation rate test
└── unit/
    ├── loop-extended-test.lisp      # LOOP clause tests
    ├── default-params-test.lisp     # &optional/:key defaults
    ├── hash-table-test.lisp         # Hash table operations
    ├── list-ops-test.lisp           # List function tests
    └── sequence-ext-test.lisp       # Sequence function tests
```

**Structure Decision**: Single project structure. All new library functions in `src/clysm/lib/`. Compiler extensions in existing `codegen/func-section.lisp`. Tests organized by layer (unit, contract, integration).

## Complexity Tracking

> **No violations requiring justification**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | - | - |
