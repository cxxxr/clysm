# Implementation Plan: I/O and List Operations Runtime Migration

**Branch**: `001-io-list-runtime` | **Date**: 2026-01-01 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-io-list-runtime/spec.md`

## Summary

Migrate I/O functions ([princ](resources/HyperSpec/Body/f_wr_pr.htm), [print](resources/HyperSpec/Body/f_wr_pr.htm), [format](resources/HyperSpec/Body/f_format.htm), [write](resources/HyperSpec/Body/f_wr_pr.htm)) and list search functions ([member](resources/HyperSpec/Body/f_mem_m.htm), [assoc](resources/HyperSpec/Body/f_assocc.htm), [find](resources/HyperSpec/Body/f_find_.htm), [position](resources/HyperSpec/Body/f_pos_p.htm)) from inline Wasm codegen in func-section.lisp to runtime library implementations. I/O uses FFI primitives (`%host-write-char`, `%host-write-string`), list operations use primitives (car, cdr, consp). Target: reduce func-section.lisp from 18,233 to under 11,000 lines.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target runtime
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams; existing FFI infrastructure (feature 027)
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools (validation), Node.js host-shim (integration)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Compiler runtime library
**Performance Goals**: Output speed within 10% of current codegen, compilation time increase < 5%
**Constraints**: Runtime library < 2,000 lines total, ANSI CL conformance required
**Scale/Scope**: ~7,000+ lines to migrate (func-section.lisp reduction target)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Runtime uses WasmGC types (anyref, struct, array) |
| II. Lispオブジェクト表現規約 | PASS | NIL as singleton, not null; UNBOUND sentinel preserved |
| III. 関数・クロージャ実装戦略 | PASS | Runtime functions follow closure structure |
| IV. Wasm制御フロー活用 | PASS | Uses block/loop for iteration, no recursion limits |
| V. シャローバインディング | N/A | No dynamic variables in migrated functions |
| VI. 段階的動的コンパイル | N/A | Not JIT-related |
| VII. テスト駆動開発（TDD） | PASS | Tests written before implementation |
| VIII. Nix-Firstワークフロー | PASS | Uses flake.nix for build validation |
| IX. ANSI CL仕様参照規約 | PASS | HyperSpec links included throughout |

## Project Structure

### Documentation (this feature)

```text
specs/001-io-list-runtime/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   ├── list-ops.lisp         # Existing: member*, assoc*, etc. (reference)
│   ├── io-runtime.lisp       # NEW: princ, print, write, format runtime
│   └── list-runtime.lisp     # NEW: member, assoc, find, position runtime
├── compiler/
│   └── codegen/
│       └── func-section.lisp # MODIFY: Remove compile-* for migrated functions
├── streams/
│   └── ffi-io.lisp           # Existing: %host-write-char, %host-write-string
└── runtime/
    └── printer.lisp          # Reference: princ-to-string, prin1-to-string

tests/
├── unit/
│   ├── io-runtime/           # NEW: Unit tests for I/O runtime
│   └── list-runtime/         # NEW: Unit tests for list runtime
├── contract/
│   └── runtime-migration/    # NEW: Verify identical behavior
└── integration/
    └── io-list/              # NEW: End-to-end tests
```

**Structure Decision**: Single project layout. New runtime files go in `src/clysm/lib/`, modifications to `func-section.lisp`, tests parallel structure in `tests/`.

## Complexity Tracking

> No Constitution violations requiring justification. All gates pass.

| Item | Decision | Rationale |
|------|----------|-----------|
| New runtime files | 2 files (io-runtime.lisp, list-runtime.lisp) | Separation of concerns; I/O and list ops are independent |
| Reuse existing list-ops.lisp | Reference only | Existing member*, assoc* use LOOP; runtime needs car/cdr/consp primitives |
