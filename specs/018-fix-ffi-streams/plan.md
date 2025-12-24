# Implementation Plan: Fix FFI Streams Module

**Branch**: `018-fix-ffi-streams` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/018-fix-ffi-streams/spec.md`

**Note**: This is a **fix/debugging task**, not new feature implementation. The streams module was implemented in 015-ffi-stream-io but disabled due to unspecified "pre-existing issues".

## Summary

Re-enable the streams module (clysm/streams) that provides FFI-based I/O functionality including `write-char`, `write-string`, `read-char`, `read-line`, and `format`. The module is fully implemented but was commented out in clysm.asd due to pre-existing issues. This plan covers investigation, issue resolution, and verification.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing), clysm/ffi (012), clysm/conditions (014)
**Storage**: N/A (in-memory streams to host file descriptors)
**Testing**: rove test framework via `(asdf:test-system :clysm)`
**Target Platform**: WebAssembly (WasmGC) running on wasmtime with WASI shim
**Project Type**: Single (Common Lisp compiler project)
**Performance Goals**: N/A (fix task, not optimization)
**Constraints**: No linear memory (WasmGC-only per constitution), UTF-8 at FFI boundary
**Scale/Scope**: 6 source files, 4 unit test files, 1 integration test file

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | **PASS** | Streams use WasmGC struct ($stream) with fd and direction fields |
| II. Lisp Object Representation | **PASS** | Stream is a defstruct (maps to WasmGC struct), not null-based |
| III. Function/Closure Strategy | **N/A** | No new closures introduced |
| IV. Wasm Control Flow | **N/A** | No new control flow constructs |
| V. Shallow Binding | **PASS** | *standard-input*, *standard-output*, *error-output* use existing special var mechanism |
| VI. Tiered Eval/JIT | **N/A** | No eval/compile changes |
| VII. TDD (Non-negotiable) | **PASS** | Existing tests will be re-enabled and must pass |
| VIII. Nix-First Workflow | **PASS** | Use `nix develop` and `nix flake check` |

**Security Constraints**:
- No linear memory access (PASS - FFI uses anyref/fixnum, no i32 pointers)
- WASI interface for I/O (PASS - uses wasi:cli for stdin/stdout/stderr)

**Gate Status**: ✅ All applicable gates pass

## Project Structure

### Documentation (this feature)

```text
specs/018-fix-ffi-streams/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0: Investigation findings
├── data-model.md        # Phase 1: Entity relationships (minimal for fix)
├── quickstart.md        # Phase 1: Verification commands
├── contracts/           # Phase 1: N/A for fix task
│   └── README.md        # Placeholder explaining N/A
└── tasks.md             # Phase 2: Implementation tasks
```

### Source Code (repository root)

```text
src/clysm/
├── streams/             # TARGET: Module to re-enable
│   ├── package.lisp     # Package definition (shadows CL symbols)
│   ├── types.lisp       # Stream struct, predicates, standard streams
│   ├── ffi-io.lisp      # FFI imports for host I/O
│   ├── write.lisp       # write-char, write-string
│   ├── read.lisp        # read-char, read-line
│   └── format.lisp      # format function implementation
├── ffi/                 # Dependency: FFI foundation (012)
├── conditions/          # Dependency: Condition system (014)
└── ...

tests/
├── unit/
│   ├── stream-types-test.lisp    # Disabled, to re-enable
│   ├── stream-write-test.lisp    # Disabled, to re-enable
│   ├── stream-read-test.lisp     # Disabled, to re-enable
│   └── stream-format-test.lisp   # Disabled, to re-enable
└── streams/
    ├── package.lisp              # Test package definition
    └── stream-test.lisp          # Integration tests (disabled)
```

**Structure Decision**: Existing single-project structure. No new directories needed; only uncommenting and fixing existing code.

## Complexity Tracking

> No constitution violations. This is a fix task for existing code.

| Aspect | Complexity | Justification |
|--------|------------|---------------|
| Scope | Low | Fix existing implementation, not new design |
| Risk | Medium | Unknown "pre-existing issues" require investigation |
| Changes | Targeted | Likely package conflicts or FFI macro issues |
