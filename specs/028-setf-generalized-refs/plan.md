# Implementation Plan: Setf Macros and Generalized References

**Branch**: `028-setf-generalized-refs` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/028-setf-generalized-refs/spec.md`

## Summary

Implement ANSI Common Lisp generalized reference (place) system including setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf macros, standard setf expanders for car/cdr/nth/aref/gethash/symbol-value, and user-defined setf expanders via define-setf-expander. This feature builds on the existing macro system (Feature 016) and integrates with CLOS slot accessors (Feature 026).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: clysm/compiler, clysm/lib/macros, clysm/clos (existing modules)
**Storage**: N/A (compile-time registry for setf expanders)
**Testing**: rove (existing test framework)
**Target Platform**: WebAssembly with GC proposal
**Project Type**: single (existing monorepo)
**Performance Goals**: Setf operations compile to minimal Wasm instructions; no runtime overhead vs. direct setter calls
**Constraints**: Must conform to ANSI CL specification for evaluation order; all generated Wasm must validate with wasm-tools
**Scale/Scope**: ~21 functional requirements; extends existing macro registry

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ Pass | setf expanders generate struct.set, array.set, etc. |
| II. Lisp Object Representation | ✅ Pass | Uses existing cons, array, hash-table types |
| III. Closure Implementation | ✅ Pass | No new closure requirements |
| IV. Wasm Control Flow | ✅ Pass | No new control flow primitives needed |
| V. Shallow Binding | ✅ Pass | symbol-value setf uses existing mechanism |
| VI. Tiered Eval/JIT | ✅ Pass | Macros expand at compile time |
| VII. TDD | ✅ Pass | Unit → Contract → Integration test flow |
| VIII. Nix-First | ✅ Pass | Uses existing flake.nix infrastructure |

**Gate Result**: PASS - No violations detected.

## Project Structure

### Documentation (this feature)

```text
specs/028-setf-generalized-refs/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   ├── macros.lisp          # Add setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf
│   └── setf-expanders.lisp  # NEW: Standard setf expanders and registry
├── compiler/
│   ├── ast.lisp             # Add AST nodes if needed
│   ├── transform/
│   │   └── macro.lisp       # Extend macro registry
│   └── codegen/
│       └── func-section.lisp # Compile rplaca, rplacd, etc.
└── clos/
    └── slot-access.lisp     # Already has setf slot-value support

tests/
├── unit/
│   ├── setf-test.lisp       # NEW: setf macro unit tests
│   └── setf-expander-test.lisp # NEW: expander unit tests
├── contract/
│   └── setf-wasm-test.lisp  # NEW: Wasm validation tests
└── integration/
    └── setf-ansi-test.lisp  # NEW: ANSI CL compliance tests
```

**Structure Decision**: Single project with existing directory layout. New files only for setf-expanders.lisp and corresponding tests.

## Complexity Tracking

*No Constitution violations require justification.*
