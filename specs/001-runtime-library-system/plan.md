# Implementation Plan: Runtime Library System

**Branch**: `001-runtime-library-system` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-runtime-library-system/spec.md`

## Summary

Implement a two-layer runtime library system that separates primitive operations (Layer 1: direct Wasm compilation) from standard library functions (Layer 2: Lisp source compiled by Clysm). This enables standard library growth without codegen complexity and targets 40% reduction in func-section.lisp (18,229 → <11,000 lines).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Target**: WasmGC (WebAssembly with GC proposal)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove + wasm-tools validate + wasmtime execution
**Target Platform**: WasmGC runtimes (wasmtime, Node.js with WasmGC support)
**Project Type**: Single compiler project with multi-stage bootstrap
**Performance Goals**: Build time increase <20%, runtime parity with current codegen
**Constraints**: Must maintain backward compatibility with existing Wasm calling conventions
**Scale/Scope**: Migrate ~7,300 lines of I/O codegen to runtime library

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Runtime library compiles to WasmGC using existing infrastructure |
| II. Lispオブジェクト表現規約 | ✅ PASS | Uses existing NIL singleton, UNBOUND sentinel |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Runtime functions compile as closures with standard arity dispatch |
| IV. Wasm制御フロー活用 | ✅ PASS | Uses existing tail-call and exception handling infrastructure |
| V. シャローバインディング | ✅ PASS | No changes to special variable implementation |
| VI. 段階的動的コンパイル | ✅ PASS | Runtime library pre-compiled, integrated with Tier 2 JIT path |
| VII. テスト駆動開発 | ✅ PASS | Contract tests for migrated functions, behavioral parity tests |
| VIII. Nix-Firstワークフロー | ✅ PASS | Uses existing nix develop environment |
| IX. ANSI CL仕様参照規約 | ✅ PASS | HyperSpec links for all implemented functions |

**All gates pass. Proceeding to Phase 0.**

## Project Structure

### Documentation (this feature)

```text
specs/001-runtime-library-system/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── primitives.md    # Primitive interface contracts
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── func-section.lisp    # Reduced by 40% after migration
│       └── primitives.lisp      # NEW: Layer 1 primitive registry
├── runtime/                      # NEW: Layer 2 runtime library
│   ├── package.lisp             # Runtime library package definition
│   ├── io.lisp                  # I/O functions (princ, print, format, write)
│   ├── list-ops.lisp            # List operations (assoc, member, find, position)
│   ├── sequences.lisp           # Sequence operations (remove, count, substitute)
│   └── strings.lisp             # String operations (princ-to-string, etc.)
├── lib/                          # Existing library (to be integrated)
│   ├── list-ops.lisp            # Migrate to runtime/list-ops.lisp
│   └── sequences.lisp           # Migrate to runtime/sequences.lisp
└── stage0/
    └── primitives.lisp          # Reference implementation (already exists)

tests/
├── contract/
│   └── runtime/
│       ├── primitives-test.lisp    # Layer 1 primitive validation
│       └── runtime-compile-test.lisp # Runtime library compilation tests
├── unit/
│   └── runtime/
│       ├── io-test.lisp            # I/O function unit tests
│       └── list-ops-test.lisp      # List operation unit tests
└── integration/
    └── runtime/
        ├── migration-test.lisp     # Behavioral parity tests
        └── self-host-test.lisp     # Self-hosting integration tests
```

**Structure Decision**: Single project with new `src/clysm/runtime/` module for Layer 2 runtime library. Primitives registry added to `src/clysm/compiler/codegen/primitives.lisp`. Existing `src/clysm/lib/` functions migrated to runtime library.

## Complexity Tracking

No violations requiring justification. The design follows existing patterns:
- Uses existing compilation infrastructure
- No new abstraction layers beyond the primitives registry
- Runtime library files follow existing lib/ structure
