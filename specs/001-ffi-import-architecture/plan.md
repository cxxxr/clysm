# Implementation Plan: FFI Import Architecture

**Branch**: `001-ffi-import-architecture` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-ffi-import-architecture/spec.md`

## Summary

This feature fixes a critical bug where all registered FFI functions are included in the Wasm import section regardless of whether they are actually used, causing "unknown import" errors when running standalone. The solution implements a 2-layer architecture: static analysis detects directly called FFI functions (emitting only those imports), while dynamic calls route through a single `$dynamic-call` host import for runtime resolution.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit), wasm-tools validate (contract), wasmtime (integration)
**Target Platform**: WasmGC (Node.js with WasmGC support, wasmtime)
**Project Type**: single
**Performance Goals**: Compilation overhead <15% vs current
**Constraints**: Must maintain backward compatibility with existing compiled modules
**Scale/Scope**: Compiler modification affecting ~5 files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Uses existing WasmGC type system |
| II. Lisp Object Representation | PASS | No changes to object representation |
| III. Closure Strategy | PASS | No changes to closure implementation |
| IV. Wasm Control Flow | PASS | No changes to control flow |
| V. Shallow Binding | PASS | No changes to dynamic scoping |
| VI. Tiered Eval/JIT | PASS | Enhances JIT by reducing unnecessary imports |
| VII. TDD (Non-negotiable) | REQUIRED | Tests before implementation |
| VIII. Nix-First | REQUIRED | Must pass `nix flake check` |
| IX. HyperSpec References | REQUIRED | Link [funcall](resources/HyperSpec/Body/f_funcal.htm), [apply](resources/HyperSpec/Body/f_apply.htm) |

## Project Structure

### Documentation (this feature)

```text
specs/001-ffi-import-architecture/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── analyzer/
│   │   ├── io-usage.lisp      # MODIFY: Extend to ffi-usage.lisp functionality
│   │   └── ffi-usage.lisp     # NEW: FFI usage analyzer (static/dynamic detection)
│   └── compiler.lisp          # MODIFY: Integrate ffi-usage analysis, use used-ffis
├── ffi/
│   ├── import-gen.lisp        # MODIFY: emit-selected-ffi-imports function
│   └── macros.lisp            # Review: *ffi-environment* handling
└── runtime/
    └── ffi-dispatch.lisp      # MODIFY: dynamic-call support

host-shim/
└── runtime.js                 # NEW: dynamic-call host implementation

tests/
├── unit/
│   ├── ffi-usage-test.lisp    # NEW: FFI usage analyzer tests
│   └── io-usage-test.lisp     # MODIFY: Add regression tests
├── contract/
│   ├── import-section-test.lisp  # MODIFY: Verify selective imports
│   └── ffi-import-wasm-test.lisp # MODIFY: Add dynamic-call tests
└── integration/
    ├── wasmtime-test.lisp     # MODIFY: Standalone execution tests
    └── dynamic-call-test.lisp # NEW: Dynamic call integration tests
```

**Structure Decision**: Single project with compiler modifications. No new packages required; extending existing `clysm/compiler/analyzer` and `clysm/ffi` modules.

## Complexity Tracking

No violations. This feature reduces complexity by:
1. Eliminating unnecessary imports (simpler Wasm modules)
2. Making FFI dependencies explicit (better debuggability)
3. Providing clear compilation modes (predictable behavior)
