# Implementation Plan: DEFSTRUCT Wasm Compilation

**Branch**: `001-defstruct-wasm-compile` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-defstruct-wasm-compile/spec.md`

## Summary

Implement [defstruct](resources/HyperSpec/Body/m_defstr.htm) compilation to WasmGC by expanding defstruct forms into equivalent [defclass](resources/HyperSpec/Body/m_defcla.htm) definitions. The macro generates constructors, accessors, setf expanders, predicates, and copiers. Target is to increase compilation rate from 14.2% to 25%+ and Stage 1 binary size from 24.5KB to 50KB+.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove for unit tests, wasm-tools validate for contract tests, Node.js for integration tests
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project - compiler extension
**Performance Goals**: Compilation of all compiler defstruct forms without error
**Constraints**: Must leverage existing CLOS (defclass) infrastructure; follow CL standard semantics
**Scale/Scope**: ~20 defstruct forms in compiler source, target 25% overall compilation rate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | ✅ PASS | Structures compile to WasmGC struct types via defclass |
| II. Lisp Object Representation | ✅ PASS | Uses existing $instance type (index 6) for structure instances |
| III. Function/Closure Strategy | ✅ PASS | Generated accessors/constructors follow closure convention |
| IV. Wasm Control Flow | ✅ PASS | No special control flow; uses standard function calls |
| V. Shallow Binding | N/A | No dynamic variables in defstruct expansion |
| VI. Tiered Eval/JIT | N/A | Compile-time macro expansion only |
| VII. TDD (Non-Negotiable) | ✅ REQUIRED | Tests first: unit tests for macro expansion, contract tests for Wasm output |
| VIII. Nix-First Workflow | ✅ PASS | Uses existing flake.nix devShell with wasm-tools |
| IX. ANSI CL Spec Reference | ✅ REQUIRED | HyperSpec links: [defstruct](resources/HyperSpec/Body/m_defstr.htm) |

**Gate Result**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/001-defstruct-wasm-compile/
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
├── lib/
│   └── defstruct.lisp          # NEW: defstruct macro implementation
├── compiler/
│   ├── macros.lisp             # Existing: register defstruct macro
│   └── codegen/
│       └── defclass.lisp       # Existing: defclass → Wasm (reused)
└── clos/
    └── structure-class.lisp    # NEW: structure-class metaclass

tests/
├── unit/
│   └── defstruct-test.lisp     # NEW: macro expansion tests
├── contract/
│   └── defstruct-wasm-test.lisp # NEW: Wasm output validation
└── integration/
    └── defstruct-usage-test.lisp # NEW: end-to-end tests
```

**Structure Decision**: Single project extension. New files in `src/clysm/lib/` for the defstruct macro, with tests mirroring the source structure. Reuses existing CLOS infrastructure (`defclass` compilation path).

## Complexity Tracking

> No violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
