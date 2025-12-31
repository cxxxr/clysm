# Implementation Plan: Division/Rounding Function Primitives

**Branch**: `001-division-rounding-primitives` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-division-rounding-primitives/spec.md`

## Summary

Implement ANSI Common Lisp rounding functions ([floor](resources/HyperSpec/Body/f_floorc.htm), [ceiling](resources/HyperSpec/Body/f_floorc.htm), [round](resources/HyperSpec/Body/f_floorc.htm), [ffloor](resources/HyperSpec/Body/f_floorc.htm), [fceiling](resources/HyperSpec/Body/f_floorc.htm), [fround](resources/HyperSpec/Body/f_floorc.htm)) as WasmGC primitives in the Clysm compiler. These functions return 2 values (quotient and remainder) and use Wasm F64 instructions (`f64.floor`, `f64.ceil`, `f64.nearest`) for floating-point operations. This unblocks a significant portion of DEFUN compilation failures (part of 18,933 failures).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), contract tests for Wasm validation
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler project)
**Performance Goals**: Compile-time performance not critical; runtime arithmetic should match native Wasm f64/i32 speed
**Constraints**: Must use WasmGC types only (Constitution I), multiple values via global buffer (Constitution III)
**Scale/Scope**: 6 functions × 2 forms (single/two-arg) = 12 compilation paths

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✓ PASS | Uses i31ref for fixnum results, $float struct for float results |
| II. Lispオブジェクト表現規約 | ✓ PASS | Results use standard Lisp object representations |
| III. 関数・クロージャ実装戦略 | ✓ PASS | Multiple values use mv-count (global 2) and mv-buffer (global 3) |
| IV. Wasm制御フロー活用 | N/A | No tail calls or exception handling needed |
| V. シャローバインディング | N/A | No dynamic scoping involved |
| VI. 段階的動的コンパイル | N/A | Not applicable (static compilation) |
| VII. TDD | ✓ REQUIRED | Tests must be written before implementation |
| VIII. Nix-First | ✓ PASS | Uses existing Nix environment |
| IX. ANSI CL仕様参照規約 | ✓ REQUIRED | HyperSpec links included in this plan |

## Project Structure

### Documentation (this feature)

```text
specs/001-division-rounding-primitives/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # AST parsing (already has floor/ceiling/round)
│   └── codegen/
│       └── func-section.lisp       # NEW: compile-floor, compile-ceiling, compile-round,
│                                   #      compile-ffloor, compile-fceiling, compile-fround
│
└── lib/
    └── numbers.lisp                # (if needed) Helper definitions

tests/
├── contract/
│   └── rounding-wasm.lisp          # Wasm output validation tests
└── unit/
    └── rounding-functions.lisp     # Unit tests for rounding compilation
```

**Structure Decision**: Single project structure. All changes are within the existing compiler codegen module (`func-section.lisp`) following the pattern of `compile-truncate`.

## Complexity Tracking

> No constitution violations - no justification needed.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |
