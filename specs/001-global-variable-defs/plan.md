# Implementation Plan: Phase 13D-4 Global Variable Definitions

**Branch**: `001-global-variable-defs` | **Date**: 2025-12-30 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-global-variable-defs/spec.md`

## Summary

Enable the Clysm compiler to compile Common Lisp global variable definitions ([defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm)) to WasmGC. The implementation must support all 58 compiler-used global variables across runtime registries, package system, I/O streams, and condition system categories. Technical approach: Generate Wasm global declarations with `(ref null any)` type, support both constant and deferred initialization via module `$init` function, and preserve shallow binding semantics for dynamic scoping.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), wasmtime, wasm-tools
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (Common Lisp testing framework)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler infrastructure)
**Performance Goals**: Compilation rate increase from ~23% baseline; no runtime performance regression for compiled globals
**Constraints**: Generated Wasm must pass `wasm-tools validate`; reserved global indices 0-3
**Scale/Scope**: 58 global variables in 6 categories

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Globals use `(ref null any)` type, no linear memory |
| II. Lisp Object Representation | PASS | NIL/UNBOUND markers properly handled (indices 0, 1) |
| III. Function/Closure Strategy | N/A | Not applicable to global variable compilation |
| IV. Wasm Control Flow | N/A | Globals don't involve control flow |
| V. Shallow Binding | PASS | Special variables use symbol `$value` slot with binding stack |
| VI. Tiered Eval/JIT | N/A | Static compilation of defvar/defparameter forms |
| VII. TDD (Non-negotiable) | MUST COMPLY | Tests first for each global category |
| VIII. Nix-First Workflow | MUST COMPLY | All changes must pass `nix flake check` |
| IX. ANSI CL HyperSpec | MUST COMPLY | Link to [defvar/defparameter](../../resources/HyperSpec/Body/m_defpar.htm) |

**Gate Result**: PASS - No violations requiring justification

## Project Structure

### Documentation (this feature)

```text
specs/001-global-variable-defs/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Wasm type contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── globals.lisp      # Global variable code generation (NEW)
│   │   └── wasm-global.lisp  # Wasm global section helpers (NEW)
│   └── ast.lisp              # AST extensions for defvar/defparameter
├── lib/
│   └── specials.lisp         # Standard special variable definitions
└── stage0/
    └── globals.lisp          # Stage 0 global registry

tests/
├── unit/
│   └── globals-test.lisp     # Unit tests for global compilation
├── contract/
│   └── wasm-globals-test.lisp # Wasm output validation
└── integration/
    └── special-vars-test.lisp # Dynamic binding integration tests
```

**Structure Decision**: Single project structure following existing Clysm layout. New files added to `compiler/codegen/` for global-specific code generation.

## Complexity Tracking

No violations to justify - all gates pass.
