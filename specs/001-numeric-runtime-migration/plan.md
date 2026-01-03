# Implementation Plan: Numeric Runtime Migration

**Branch**: `001-numeric-runtime-migration` | **Date**: 2026-01-04 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-numeric-runtime-migration/spec.md`

## Summary

Migrate 5 numeric functions ([parse-integer](resources/HyperSpec/Body/f_parse_.htm), [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm), [rationalize](resources/HyperSpec/Body/f_ration.htm), [signum](resources/HyperSpec/Body/f_signum.htm), [phase](resources/HyperSpec/Body/f_phase.htm)) from inline Wasm codegen to a Lisp runtime library. Functions will be implemented using only Layer 1 primitives, registered in `*runtime-function-table*`, and dispatched via `compile-runtime-call`. This follows the established pattern from `001-string-runtime-migration`.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation)
**Testing**: rove via `(asdf:test-system :clysm)`, wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Stage 1 compilation succeeds, Wasm validation passes
**Constraints**: Layer 1 primitives only (no Layer 2+ dependencies for bootstrapping)
**Scale/Scope**: 5 functions, ~300-400 lines of runtime library code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | Uses existing numeric tower types (i31ref for fixnum, $float, $ratio) |
| II. Lisp Object Representation | PASS | Uses existing NIL/UNBOUND conventions |
| III. Function/Closure Strategy | PASS | Runtime functions follow standard closure structure |
| IV. Wasm Control Flow | PASS | No special control flow requirements |
| V. Shallow Binding | N/A | No special variables introduced |
| VI. Tiered Eval/JIT | PASS | Runtime library compatible with both tiers |
| VII. TDD (Non-negotiable) | PASS | Unit tests required per FR-013 |
| VIII. Nix-First Workflow | PASS | Uses existing Nix environment |
| IX. ANSI CL Spec References | PASS | HyperSpec links included in this document |

## Project Structure

### Documentation (this feature)

```text
specs/001-numeric-runtime-migration/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   └── numeric-runtime.lisp     # NEW: Runtime library (5 functions)
├── compiler/codegen/
│   └── func-section.lisp        # MODIFY: Add register-numeric-runtime-functions

tests/
└── unit/
    └── numeric-runtime-test.lisp  # NEW: Unit tests (25+ tests)
```

**Structure Decision**: Single project structure following existing pattern from `001-string-runtime-migration`. New runtime library file in `src/clysm/lib/`, new test file in `tests/unit/`.

## Complexity Tracking

No violations identified. Implementation follows established patterns.
