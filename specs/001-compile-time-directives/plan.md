# Implementation Plan: Compile-Time Directive Processing

**Branch**: `001-compile-time-directives` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-compile-time-directives/spec.md`

## Summary

Implement compile-time directive processing for [in-package](resources/HyperSpec/Body/m_in_pkg.htm), [defpackage](resources/HyperSpec/Body/m_defpkg.htm), [declaim](resources/HyperSpec/Body/m_declai.htm), and [proclaim](resources/HyperSpec/Body/f_procla.htm) forms. These forms will be evaluated at compile-time using the host SBCL environment and will not generate AST nodes or Wasm bytecode. This resolves 61 compilation errors (49 in-package, 9 defpackage, 3 declaim) and improves compilation rate toward self-hosting.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8)
**Storage**: N/A (in-memory compilation)
**Testing**: Rove test framework (`asdf:test-system :clysm`)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler infrastructure)
**Performance Goals**: No measurable increase in compile-time overhead for directive processing
**Constraints**: Must maintain ANSI CL compliance; directives evaluated in host environment
**Scale/Scope**: 61 directive forms to process without error (49+9+3)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Directives do not generate Wasm; no impact on type system |
| II. Lispオブジェクト表現規約 | PASS | No object representation changes; uses host packages |
| III. 関数・クロージャ実装戦略 | PASS | No function/closure changes |
| IV. Wasm制御フロー活用 | PASS | No control flow changes |
| V. シャローバインディング | PASS | No dynamic scope changes |
| VI. 段階的動的コンパイル | PASS | Compile-time eval uses host, not Wasm interpreter |
| VII. TDD（非交渉） | REQUIRED | Tests MUST be written before implementation |
| VIII. Nix-Firstワークフロー | REQUIRED | Must pass `nix flake check` |
| IX. ANSI CL仕様参照規約 | REQUIRED | HyperSpec links included above for all 4 directives |

**Gate Result**: PASS (no violations requiring justification)

## Project Structure

### Documentation (this feature)

```text
specs/001-compile-time-directives/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A - no API contracts)
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── compiler.lisp       # compile-to-module (main entry, lines 65-135)
│   ├── ast.lisp            # parse-expr, parse-compound-form (lines 692-833)
│   ├── env.lisp            # Compilation environment
│   └── directive.lisp      # NEW: Compile-time directive processing
└── ...

tests/
├── unit/
│   └── directive-test.lisp # NEW: Unit tests for directive processing
└── contract/
    └── directive-output-test.lisp # NEW: Verify no Wasm output for directives
```

**Structure Decision**: Add new `src/clysm/compiler/directive.lisp` file for directive handling logic. Integrate into existing `compiler.lisp` via function call before AST generation.

## Complexity Tracking

> No Constitution Check violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
