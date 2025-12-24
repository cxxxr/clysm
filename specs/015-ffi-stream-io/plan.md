# Implementation Plan: FFI-based Stream I/O

**Branch**: `015-ffi-stream-io` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/015-ffi-stream-io/spec.md`

## Summary

Implement Common Lisp stream I/O operations via FFI to host environment, avoiding WebAssembly linear memory. Provides `write-char`, `write-string`, `read-char`, `read-line` via FFI calls, `format` with basic directives (~A, ~S, ~D, ~%, ~~), and stream special variables (`*standard-input*`, `*standard-output*`, `*error-output*`).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing FFI foundation (012), condition system (014), special variables (002), character/string types (008)
**Storage**: N/A (in-memory streams to host file descriptors)
**Testing**: rove (Lisp-side), wasmtime (Wasm execution)
**Target Platform**: WasmGC, wasmtime runtime with host JavaScript/WASI shim
**Project Type**: single (compiler extension)
**Performance Goals**: I/O latency dominated by host; FFI call overhead <100ns per call
**Constraints**: No linear memory usage; all data via WasmGC types and FFI
**Scale/Scope**: 4 I/O functions, 1 format function, 3 special variables, 1 stream type

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | **PASS** | Stream struct uses WasmGC, no linear memory |
| II. Lisp Object Representation | **PASS** | Stream as WasmGC struct with host handle |
| III. Closure Implementation | N/A | No closures in I/O primitives |
| IV. Wasm Control Flow | N/A | No tail calls/exceptions in I/O |
| V. Shallow Binding | **PASS** | Stream specials use existing binding mechanism |
| VI. Tiered Eval | N/A | I/O is runtime, not eval |
| VII. TDD | **REQUIRED** | Tests before implementation |
| VIII. Nix-First | **REQUIRED** | flake.nix must include wasmtime for testing |

**Constitution Violations**: None

## Project Structure

### Documentation (this feature)

```text
specs/015-ffi-stream-io/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── streams/                 # NEW: Stream module
│   │   ├── package.lisp         # Package definition
│   │   ├── types.lisp           # Stream WasmGC type definitions
│   │   ├── ffi-io.lisp          # FFI imports for host I/O
│   │   ├── write.lisp           # write-char, write-string
│   │   ├── read.lisp            # read-char, read-line
│   │   └── format.lisp          # format function with basic directives
│   ├── runtime/
│   │   └── stream-vars.lisp     # NEW: *standard-input/output/error-output*
│   ├── ffi/
│   │   └── types.lisp           # EXTEND: Add :stream marshal type if needed
│   └── compiler/
│       └── codegen/
│           └── gc-types.lisp    # EXTEND: Add $stream type (type index 19)
tests/
├── streams/                     # NEW: Stream tests
│   ├── write-test.lisp          # write-char, write-string tests
│   ├── read-test.lisp           # read-char, read-line tests
│   ├── format-test.lisp         # format directive tests
│   └── integration-test.lisp    # End-to-end with wasmtime
host-shim/                       # NEW: Host shim for wasmtime
├── io-shim.js                   # JavaScript I/O implementation
└── README.md                    # Usage documentation
```

**Structure Decision**: Single project extension - adding new `streams/` module with FFI-based I/O, extending existing `gc-types.lisp` for stream type, and providing host-shim for wasmtime testing.

## Complexity Tracking

No violations to justify - design follows existing patterns.
