# Implementation Plan: Compiler Subset Validation (Phase 11)

**Branch**: `036-compiler-subset-validation` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/036-compiler-subset-validation/spec.md`

## Summary

Implement a validation system to verify that Clysm compiler modules can be compiled by Clysm itself. This involves:
1. Static analysis to identify CL features used in each module
2. Dependency-order compilation of modules
3. Wasm validation tests using `wasm-tools validate`
4. Documentation of the self-compilable "blessed subset" of Common Lisp

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, rove (testing), wasm-tools (validation)
**Storage**: N/A (in-memory analysis, file-based reports)
**Testing**: rove (unit/contract/integration tests)
**Target Platform**: N/A (validation tool, not runtime target)
**Project Type**: Single project
**Performance Goals**: N/A (one-time analysis tool)
**Constraints**: All compiler module Wasms MUST pass `wasm-tools validate`
**Scale/Scope**: 6 module directories (~80 Lisp source files)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Applies | Status | Notes |
|-----------|---------|--------|-------|
| I. WasmGC-First型システム設計 | No | N/A | Static analysis tool, not compiler output |
| II. Lispオブジェクト表現規約 | No | N/A | Static analysis tool |
| III. 関数・クロージャ実装戦略 | No | N/A | Static analysis tool |
| IV. Wasm制御フロー活用 | No | N/A | Static analysis tool |
| V. シャローバインディング | No | N/A | Static analysis tool |
| VI. 段階的動的コンパイル | No | N/A | Static analysis tool |
| VII. テスト駆動開発（TDD） | **Yes** | **MUST** | All validation tests follow TDD |
| VIII. Nix-Firstワークフロー | **Yes** | **MUST** | wasm-tools in devShell |

**Gate Status**: ✅ PASS - No violations. TDD and Nix principles will be followed.

## Project Structure

### Documentation (this feature)

```text
specs/036-compiler-subset-validation/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── validation/              # NEW: Validation tools
│   │   ├── package.lisp         # Package definition
│   │   ├── analyzer.lisp        # Static CL feature analyzer
│   │   ├── feature-registry.lisp # Clysm supported features registry
│   │   ├── reporter.lisp        # Coverage report generator
│   │   └── compiler-order.lisp  # Dependency-order compilation
│   └── [existing modules]       # Target modules for validation
│       ├── backend/             # leb128.lisp, sections.lisp, etc.
│       ├── reader/              # tokenizer.lisp, parser.lisp
│       ├── compiler/            # ast.lisp, codegen/*, compiler.lisp
│       ├── runtime/             # multi-value.lisp, special-vars.lisp
│       ├── clos/                # defclass.lisp, defmethod.lisp, etc.
│       └── conditions/          # handlers.lisp, signaling.lisp, etc.

tests/
├── unit/
│   └── validation/              # NEW: Validation unit tests
│       ├── analyzer-test.lisp
│       ├── feature-registry-test.lisp
│       └── reporter-test.lisp
├── contract/
│   └── validation/              # NEW: Wasm validation tests
│       └── module-wasm-test.lisp
└── integration/
    └── validation/              # NEW: End-to-end validation tests
        └── self-compile-test.lisp

docs/
└── blessed-subset.lisp          # NEW: Self-compilable CL subset documentation
```

**Structure Decision**: Single project structure. Validation tools added to `src/clysm/validation/` with corresponding test directories.

## Complexity Tracking

No violations requiring justification. The design follows existing project patterns.

## Modules to Validate

Based on codebase analysis, the 6 target directories contain:

| Directory | Files | Key Modules |
|-----------|-------|-------------|
| backend/ | 4 | leb128.lisp, sections.lisp, wasm-emit.lisp, wat-print.lisp |
| reader/ | 4 | package.lisp, tokenizer.lisp, parser.lisp, reader.lisp |
| compiler/ | 14 | ast.lisp, compiler.lisp, env.lisp, codegen/*, analyzer/*, transform/* |
| runtime/ | 6 | objects.lisp, multi-value.lisp, special-vars.lisp, etc. |
| clos/ | 8 | defclass.lisp, defmethod.lisp, instance.lisp, dispatch.lisp, etc. |
| conditions/ | 6 | package.lisp, types.lisp, handlers.lisp, signaling.lisp, etc. |

**Total**: ~42 core Lisp files to validate

## Dependency Order (FR-006)

Based on the spec and typical compiler architecture:

1. `src/clysm/backend/leb128.lisp` (no dependencies)
2. `src/clysm/backend/sections.lisp` (depends on leb128)
3. `src/clysm/reader/tokenizer.lisp` (minimal dependencies)
4. `src/clysm/reader/parser.lisp` (depends on tokenizer)
5. `src/clysm/compiler/ast.lisp` (depends on parser)
6. `src/clysm/compiler/codegen/*.lisp` (depends on ast)
7. `src/clysm/compiler/compiler.lisp` (depends on all above)

## Constitution Check (Post-Design)

*Re-evaluation after Phase 1 design completion.*

| Principle | Status | Verification |
|-----------|--------|--------------|
| VII. TDD | ✅ PASS | Test structure defined: unit/, contract/, integration/ test directories with specific test files per component |
| VIII. Nix-First | ✅ PASS | wasm-tools already in devShell; no additional dependencies needed |

**Post-Design Gate Status**: ✅ PASS

The design adheres to all applicable constitution principles:
- Tests will be written before implementation (TDD cycle)
- All validation uses `wasm-tools validate` from Nix devShell
- No new external dependencies introduced
- Report format is Markdown (human-readable, CI-friendly)

## Generated Artifacts

| Artifact | Path | Status |
|----------|------|--------|
| Implementation Plan | `specs/036-compiler-subset-validation/plan.md` | ✅ Complete |
| Research | `specs/036-compiler-subset-validation/research.md` | ✅ Complete |
| Data Model | `specs/036-compiler-subset-validation/data-model.md` | ✅ Complete |
| Quickstart | `specs/036-compiler-subset-validation/quickstart.md` | ✅ Complete |
| Tasks | `specs/036-compiler-subset-validation/tasks.md` | ⏳ Pending (/speckit.tasks) |

## Next Steps

Run `/speckit.tasks` to generate the implementation task list.
