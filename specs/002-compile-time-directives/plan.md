# Implementation Plan: Compile-Time Directive Skip Integration

**Branch**: `002-compile-time-directives` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-compile-time-directives/spec.md`

## Summary

Integrate compile-time directive skip handling into Stage 1 generation. The existing `directive.lisp` infrastructure correctly returns `nil` from `compile-to-wasm` for directives ([in-package](resources/HyperSpec/Body/m_in_pkg.htm), [defpackage](resources/HyperSpec/Body/m_defpkg.htm), [declaim](resources/HyperSpec/Body/m_declai.htm), [proclaim](resources/HyperSpec/Body/f_procla.htm)), but `test-form-compilation` in `generator.lisp` treats `nil` bytes as validation failure. The fix involves detecting `nil` returns and classifying them as "skipped" rather than "failed", updating the report format, and excluding skipped forms from top_blockers.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove (unit), shell scripts (integration), wasm-tools validate (contract)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Stage 1 generation completes in reasonable time (current ~30s)
**Constraints**: Must maintain wasm-tools validate compatibility
**Scale/Scope**: 26,353 total forms, 284 DEFPACKAGE forms to be reclassified

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Applicable | Status |
|-----------|------------|--------|
| I. WasmGC-First | No | N/A - Compile-time only, no Wasm output |
| II. Lisp Object Representation | No | N/A - No runtime objects involved |
| III. Function/Closure | No | N/A - No function compilation |
| IV. Wasm Control Flow | No | N/A - Directives produce no code |
| V. Shallow Binding | No | N/A - No dynamic scope involved |
| VI. Tiered Eval/JIT | No | N/A - Host-side (SBCL) only |
| VII. TDD | **Yes** | MUST follow Red-Green-Refactor |
| VIII. Nix-First | **Yes** | MUST use `nix develop` environment |
| IX. ANSI CL Spec Reference | **Yes** | MUST link HyperSpec for directives |

**Gate Status**: ✅ PASS - All applicable principles can be satisfied

## Project Structure

### Documentation (this feature)

```text
specs/002-compile-time-directives/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── checklists/
    └── requirements.md  # Quality checklist
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── directive.lisp     # Existing directive detection/evaluation
├── stage1/
│   ├── generator.lisp     # Target: test-form-compilation, classify-forms
│   └── package.lisp       # Exports
└── package.lisp           # Main package exports

tests/
├── contract/              # Wasm structure validation
├── integration/           # End-to-end Stage 1 tests
└── unit/                  # Unit tests for classify-forms

build/
└── stage1-complete.lisp   # Stage 1 entry script

dist/
├── clysm-stage1.wasm      # Output binary
└── stage1-report.json     # Report with skipped count
```

**Structure Decision**: Single project structure. Changes are localized to `src/clysm/stage1/generator.lisp` and `dist/stage1-report.json` format.

## Complexity Tracking

No constitution violations. Feature is a straightforward integration with minimal complexity.
