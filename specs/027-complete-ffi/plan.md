# Implementation Plan: Complete FFI Foundation

**Branch**: `027-complete-ffi` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/027-complete-ffi/spec.md`

## Summary

Complete the FFI foundation to establish full host environment interoperability. The existing FFI module (`src/clysm/ffi/`) provides type definitions, marshalling, and macros. This feature completes:

1. **Compiler pipeline integration** - Process FFI declarations during compilation
2. **Error handling** - Replace placeholder try_table/catch wrappers with real implementation
3. **Dynamic call-host** - Implement runtime host function invocation
4. **Re-entrant callbacks** - Support host calling back into Lisp during FFI execution

Technical approach: Extend the existing FFI module and integrate it into the compilation pipeline at AST and codegen levels, using WasmGC exception handling (try_table/catch) for error translation.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing), existing clysm/ffi, clysm/compiler modules
**Storage**: N/A (in-memory compile-time registries)
**Testing**: rove (Common Lisp test framework)
**Target Platform**: WasmGC (WebAssembly GC)
**Project Type**: Single project (compiler)
**Performance Goals**: FFI overhead < 2x native Wasm call_ref
**Constraints**: WasmGC-First (no linear memory access), exception handling via try_table
**Scale/Scope**: 6 marshal types, ~20 FFI functions to integrate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | FR-010 explicitly prohibits linear memory; all marshalling uses GC types (i31ref, anyref, externref) |
| II. Lisp Object Representation | ✅ PASS | NIL/UNBOUND handling already established; FFI uses existing object model |
| III. Function/Closure Strategy | ✅ PASS | Export wrappers use closure struct pattern for callbacks |
| IV. Wasm Control Flow | ✅ PASS | Error handling uses try_table/catch per Constitution IV |
| V. Shallow Binding | N/A | FFI doesn't modify special variable implementation |
| VI. Tiered Eval/JIT | ⚠️ PARTIAL | call-host relates to dynamic invocation; deferred to P3 |
| VII. TDD | ✅ PASS | Test-first approach required for all implementation |
| VIII. Nix-First | ✅ PASS | `nix flake check` must pass; wasmtime/wasm-tools in devShell |

**Gate Result**: PASS - All critical principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/027-complete-ffi/
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
│   ├── ffi/                   # Existing FFI module (to be extended)
│   │   ├── package.lisp       # Package definition (exports)
│   │   ├── types.lisp         # Type definitions (existing)
│   │   ├── marshalling.lisp   # Marshalling functions (existing)
│   │   ├── macros.lisp        # define-foreign-function, export-function (extend)
│   │   ├── import-gen.lisp    # Import generation (extend error handling)
│   │   └── export-gen.lisp    # Export generation (existing)
│   ├── compiler/
│   │   ├── ast.lisp           # Add AST nodes for FFI calls
│   │   ├── compiler.lisp      # FFI section emission (partial, extend)
│   │   └── codegen/
│   │       └── func-section.lisp  # FFI call compilation
│   └── runtime/
│       └── ffi-dispatch.lisp  # NEW: Dynamic call-host dispatch

tests/
├── unit/
│   └── ffi/                   # Unit tests for FFI functions
├── contract/
│   └── ffi-wasm-test.lisp     # Wasm validation tests
└── integration/
    └── ffi-test.lisp          # End-to-end FFI tests

host-shim/                     # Test host environment
└── ffi-test-host.js           # JavaScript host functions for testing
```

**Structure Decision**: Single project structure. FFI extends existing `src/clysm/ffi/` module with new runtime dispatch and compiler integration points.

## Complexity Tracking

No constitution violations requiring justification.
