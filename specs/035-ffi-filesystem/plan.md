# Implementation Plan: FFI Filesystem Access

**Branch**: `035-ffi-filesystem` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/035-ffi-filesystem/spec.md`

## Summary

Implement file system access for Clysm via FFI to host environments. Provides four core functions (`open-file`, `close-file`, `read-file-contents`, `write-file-contents`) and a `with-open-file` macro using `unwind-protect` for safe resource management. The unified interface abstracts over WASI Preview2 (wasmtime) and Virtual FS (browser) backends.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: clysm/ffi (027), clysm/conditions (014), clysm/lib/macros (028), babel (UTF-8)
**Storage**: N/A (host filesystem via FFI)
**Testing**: Rove (unit, contract, integration tests)
**Target Platform**: WasmGC (wasmtime with WASI Preview2, browser with Virtual FS shim)
**Project Type**: Single (compiler library extension)
**Performance Goals**: File operations complete with minimal FFI overhead; 1MB file I/O within acceptable latency
**Constraints**: WasmGC-First (no linear memory), UTF-8 text only, WASI Preview2 compliance
**Scale/Scope**: 4 core functions + 1 macro + 1 condition type

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | File handles as opaque externref; no linear memory |
| II. Lisp Object Representation | PASS | file-error extends existing condition hierarchy |
| III. Function/Closure Strategy | N/A | Uses standard FFI function calls |
| IV. Wasm Control Flow | PASS | Uses try_table for error handling via FFI infrastructure |
| V. Shallow Binding | N/A | No special variables introduced |
| VI. Tiered Eval/JIT | N/A | Static compilation only |
| VII. TDD | REQUIRED | All functions must have tests before implementation |
| VIII. Nix-First | REQUIRED | Tests run via `nix flake check` |
| WASI Interop | PASS | Uses `wasi:filesystem` as specified in constitution |
| Security | PASS | All file access via WASI sandbox; no direct memory access |

**Gate Result**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/035-ffi-filesystem/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── filesystem.lisp  # API contracts
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── ffi/
│   └── filesystem.lisp      # FFI import declarations for file operations
├── streams/
│   └── file-stream.lisp     # File stream struct and operations
├── conditions/
│   └── file-error.lisp      # file-error condition definition
└── lib/
    └── file-macros.lisp     # with-open-file macro

host-shim/
├── wasi-filesystem.js       # WASI Preview2 adapter for wasmtime
└── virtual-fs.js            # Browser Virtual FS implementation

tests/
├── unit/
│   └── filesystem/
│       ├── open-close-test.lisp
│       ├── read-contents-test.lisp
│       ├── write-contents-test.lisp
│       └── with-open-file-test.lisp
├── contract/
│   └── filesystem-wasm-test.lisp
└── integration/
    └── filesystem-test.lisp
```

**Structure Decision**: Single project extension to existing clysm compiler. Files organized under existing module structure (ffi/, streams/, conditions/, lib/). Host shims in host-shim/ for both runtime environments.

## Complexity Tracking

No constitution violations requiring justification.
