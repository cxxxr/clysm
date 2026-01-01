# Implementation Plan: Compiler Internal Function Consolidation

**Branch**: `001-internal-function-consolidation` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-internal-function-consolidation/spec.md`

## Summary

Export 7 categories of internal compiler functions (ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, MAKE-WASM-STRUCT-TYPE, COMPILE-UNARY-MATH-FFI, AST-LITERAL-VALUE, COMPILE-CXR-CHAIN, LOOP-KEYWORD-EQ) to resolve 242+ undefined function errors during Stage 1 compilation. Additionally, remove dead code from func-section.lisp for functions already migrated to runtime libraries, reducing the file from 18,351 lines to under 12,000 lines. Target: Stage 1 compilation rate increase from 22.15% to 25%+.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to dist/)
**Testing**: rove (sbcl --eval "(asdf:test-system :clysm)"), wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler infrastructure)
**Performance Goals**: Stage 1 compilation rate 22.15% → 25%+
**Constraints**: All existing tests must pass, generated Wasm must validate
**Scale/Scope**: 7 functions to export, ~6,000 lines of dead code to remove from 18,351-line file

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | No changes to type system; exports existing functions |
| II. Lispオブジェクト表現規約 | PASS | No changes to NIL/UNBOUND representation |
| III. 関数・クロージャ実装戦略 | PASS | No changes to closure structure |
| IV. Wasm制御フロー活用 | PASS | No changes to control flow |
| V. シャローバインディング | PASS | No changes to dynamic scoping |
| VI. 段階的動的コンパイル | PASS | Supports Stage 1 generation |
| VII. テスト駆動開発（TDD） | PASS | Existing tests verify non-regression |
| VIII. Nix-Firstワークフロー | PASS | Uses existing nix develop environment |
| IX. ANSI CL仕様参照規約 | PASS | Dead code removal for ANSI functions already migrated |

**Gate Result**: PASS - No violations. This feature adds exports and removes dead code without modifying compiler semantics.

## Project Structure

### Documentation (this feature)

```text
specs/001-internal-function-consolidation/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                           # AST-LITERAL-VALUE defined here (defstruct accessor)
│   └── codegen/
│       └── func-section.lisp              # Main target: 18,351 lines → <12,000 lines
│                                          # Contains: ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS,
│                                          # COMPILE-UNARY-MATH-FFI, COMPILE-CXR-CHAIN
├── lib/
│   ├── macros.lisp                        # LOOP-KEYWORD-EQ defined here
│   ├── type-construction.lisp             # MAKE-WASM-STRUCT-TYPE* defined here
│   ├── io-runtime.lisp                    # Runtime: princ, prin1, print, write, format, terpri
│   ├── list-runtime.lisp                  # Runtime: member, assoc, rassoc, find, position
│   └── sequence-runtime.lisp              # Runtime: remove, count, substitute, delete families
└── clysm.asd                              # ASDF system definition with exports

tests/
├── contract/                              # Wasm output validation tests
├── integration/                           # Full compilation pipeline tests
└── unit/                                  # Individual function tests
```

**Structure Decision**: Existing single-project structure. Modifications to package exports and dead code removal in func-section.lisp.

## Complexity Tracking

> No violations requiring justification.

## Internal Function Export Analysis

| Function | Package | File | Line | Currently Exported | Action |
|----------|---------|------|------|-------------------|--------|
| ENV-ADD-LOCAL | clysm/compiler/codegen/func-section | func-section.lisp | 196 | YES (via clysm) | Verify export chain |
| COMPILE-TO-INSTRUCTIONS | clysm/compiler/codegen/func-section | func-section.lisp | 351 | YES (via clysm) | Verify export chain |
| MAKE-WASM-STRUCT-TYPE | clysm | type-construction.lisp | 101 | YES (as make-wasm-struct-type*) | Verify export |
| COMPILE-UNARY-MATH-FFI | clysm/compiler/codegen/func-section | func-section.lisp | 6758 | NO | Export needed |
| AST-LITERAL-VALUE | clysm/compiler/ast | ast.lisp | 28 | YES | Verify export chain |
| COMPILE-CXR-CHAIN | clysm/compiler/codegen/func-section | func-section.lisp | 4059 | NO | Export needed |
| LOOP-KEYWORD-EQ | clysm/lib/macros | macros.lisp | 818 | YES | Verify export chain |

## Dead Code Removal Candidates

Functions in func-section.lisp that have been migrated to runtime libraries:

### List Operations (→ list-runtime.lisp)
- compile-find (line ~11210)
- compile-find-if (line ~11292)
- compile-member (line ~12103)
- compile-member-if (line ~12934)
- compile-member-if-not (line ~12998)
- compile-assoc (line ~12184)
- compile-assoc-if (line ~13061)
- compile-rassoc (estimated based on pattern)
- compile-position (estimated based on pattern)

### Sequence Operations (→ sequence-runtime.lisp)
- compile-remove (line ~11509)
- compile-remove-if (line ~11615)
- compile-remove-if-not (line ~11702)
- compile-delete (pattern-based)
- compile-delete-if (pattern-based)
- compile-delete-if-not (pattern-based)
- compile-count (pattern-based)
- compile-count-if (pattern-based)
- compile-count-if-not (pattern-based)
- compile-substitute (pattern-based)
- compile-substitute-if (pattern-based)
- compile-substitute-if-not (pattern-based)

### I/O Operations (→ io-runtime.lisp)
- compile-princ (line ~17936)
- compile-prin1 (estimated)
- compile-print (estimated)
- compile-write (estimated)
- compile-format (line ~18111)
- compile-terpri (estimated)

**Estimated removable lines**: ~6,000-7,000 lines (target: 18,351 → <12,000)
