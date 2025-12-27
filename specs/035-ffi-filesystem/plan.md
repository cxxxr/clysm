# Implementation Plan: FFI Filesystem Access

**Branch**: `035-ffi-filesystem` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/035-ffi-filesystem/spec.md`

## Summary

Implement filesystem access for Clysm programs via FFI to host environments (wasmtime WASI Preview2 and browser Virtual FS). Provides `read-file-contents`, `write-file-contents`, `open-file`, `close-file`, and `with-open-file*` macro with `unwind-protect` for safe resource management. Uses the existing FFI infrastructure (Feature 027) to declare host function imports under the `clysm:fs` namespace.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: clysm/ffi (027), clysm/conditions (014), clysm/lib/macros (028), babel (UTF-8)
**Storage**: N/A (host filesystem via FFI)
**Testing**: rove (test framework), wasmtime (Wasm runtime for contract tests)
**Target Platform**: WasmGC (wasmtime with WASI Preview2, browser with Virtual FS shim)
**Project Type**: single (compiler project)
**Performance Goals**: File read/write for files up to 1MB without memory issues
**Constraints**: UTF-8 text files only; no binary I/O; paths resolved by host environment
**Scale/Scope**: Basic file I/O operations (read, write, open, close, with-open-file*)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | File handles are `anyref` (externref from host); no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | file-stream struct follows standard pattern; file-error is condition class |
| III. 関数・クロージャ実装戦略 | ✅ N/A | No closures in this feature |
| IV. Wasm制御フロー活用 | ✅ PASS | `unwind-protect` via `try_table` for with-open-file* cleanup |
| V. シャローバインディング | ✅ N/A | No special variables in this feature |
| VI. 段階的動的コンパイル | ✅ N/A | Static compilation only |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Unit, contract, integration tests required |
| VIII. Nix-Firstワークフロー | ✅ PASS | Uses existing flake.nix devShell |

**WASI Compliance**: FR-008/FR-009 require WASI-compatible FFI interface, which aligns with constitution's WASI Preview 2 requirements.

## Project Structure

### Documentation (this feature)

```text
specs/035-ffi-filesystem/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── filesystem-ffi.md
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/filesystem/
├── package.lisp         # Package definition with exports
├── types.lisp           # file-stream struct definition
├── ffi.lisp             # FFI declarations (clysm:fs namespace)
├── open.lisp            # open-file, close-file functions
├── read.lisp            # read-file-contents function
├── write.lisp           # write-file-contents function
└── macros.lisp          # with-open-file* macro

src/clysm/conditions/
└── types.lisp           # file-error condition class (existing)

host-shim/
└── fs-shim.js           # Host filesystem shim for wasmtime

tests/
├── unit/filesystem/     # Unit tests
│   ├── types-test.lisp
│   ├── open-test.lisp
│   ├── read-test.lisp
│   ├── write-test.lisp
│   └── macros-test.lisp
├── contract/
│   ├── filesystem-ffi-test.lisp
│   └── filesystem-wasm-test.lisp
└── integration/
    └── filesystem-test.lisp
```

**Structure Decision**: Single project structure. Filesystem module is a new package `clysm/filesystem` within the existing Clysm compiler codebase. Uses established patterns from clysm/ffi and clysm/conditions.

## Complexity Tracking

No violations requiring justification. Feature follows established patterns:
- FFI declarations via `clysm/ffi:define-foreign-function`
- Condition signaling via `clysm/conditions:file-error`
- Resource cleanup via `unwind-protect` macro infrastructure
