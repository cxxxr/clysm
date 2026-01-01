# Implementation Plan: Type Constant and Package Primitive Export

**Branch**: `001-type-package-export` | **Date**: 2026-01-01 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-type-package-export/spec.md`

## Summary

Export type index constants (+TYPE-CONS+, +TYPE-SYMBOL+, etc.) and package primitives ([packagep*](resources/HyperSpec/Body/f_pkgp.htm), [find-package*](resources/HyperSpec/Body/f_find_p.htm), [intern*](resources/HyperSpec/Body/f_intern.htm), symbol-package*) to enable Stage 1 self-compilation. This resolves P846 (unbound type constants, 25 errors) and P951 (undefined PACKAGEP*, 79 errors) by adding compile-time [defconstant](resources/HyperSpec/Body/m_defcon.htm) handling and implementing package primitives as Wasm runtime functions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, trivial-gray-streams, wasm-tools (validation)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (unit/contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single
**Performance Goals**: Compile-time constant folding (negligible overhead)
**Constraints**: wasm-tools validate passes on all Stage 1 output
**Scale/Scope**: 28 type constants, 4 package primitives, ~100 affected compilation errors

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Type constants map directly to WasmGC type section indices |
| II. Lisp Object Representation | PASS | NIL/UNBOUND handled per spec; constants are indices only |
| III. Function/Closure Implementation | PASS | Package primitives use standard closure calling convention |
| IV. Wasm Control Flow | N/A | No control flow changes |
| V. Shallow Binding | N/A | No dynamic variable changes |
| VI. Tiered Eval/JIT | PASS | Runtime functions callable from JIT-compiled code |
| VII. TDD | PASS | Unit tests for constant export, package primitive compilation |
| VIII. Nix-First | PASS | Uses existing nix develop environment |
| IX. ANSI CL Spec Reference | PASS | HyperSpec links included for packagep, find-package, intern |

**Gate Result**: PASS - All relevant principles satisfied

## Project Structure

### Documentation (this feature)

```text
specs/001-type-package-export/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── gc-types.lisp         # Type constants already defined here
│   │   └── func-section.lisp     # Runtime function table
│   └── directive.lisp            # Compile-time directive handling (DEFCONSTANT)
├── lib/
│   └── package-stubs.lisp        # Package primitive implementations
└── package.lisp                  # Package exports

tests/
├── unit/
│   ├── constant-export-test.lisp     # New: constant compilation tests
│   └── package-primitive-test.lisp   # New: package function tests
└── contract/
    └── stage1-coverage-test.lisp     # Verify P846/P951 reduction
```

**Structure Decision**: Single project structure. All changes are within existing compiler/codegen and lib modules. New test files added to existing test directories.

## Complexity Tracking

No constitution violations requiring justification. Feature uses existing infrastructure:
- Type constants already exported from clysm/compiler/codegen/gc-types package
- Runtime function table already exists in func-section.lisp
- DEFCONSTANT handling extends existing directive.lisp
