# Implementation Plan: Control Structure Extensions

**Branch**: `001-control-structure-extension` | **Date**: 2025-12-30 | **Spec**: [spec.md](spec.md)
**Input**: Phase 13D-6: Control structure extensions for Wasm compilation support

## Summary

Extend the Clysm compiler to fully support control structures needed for self-hosting: `values`, `the`, `labels`/`flet` mutual recursion, and `handler-case`. The exploration reveals partial existing support—this plan addresses gaps causing 45 compilation failures.

**Key Technical Findings**:
- `values`: Implementation exists at `src/clysm/compiler/codegen/func-section.lisp:12930` - investigate why failures occur
- `the`: Simple pass-through at `src/clysm/compiler/ast.lisp:825-830` - likely AST dispatch issue
- `labels`/`flet`: Two-phase closure creation exists at `func-section.lisp:6459-6580` - forward reference resolution may have edge cases
- `handler-case`: **Not implemented in codegen** - only interpreter support exists

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) host compiler
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Testing**: Rove framework (`sbcl --eval "(asdf:test-system :clysm)"`)
**Storage**: N/A (in-memory compilation)
**Performance Goals**: N/A (compiler correctness is priority)
**Constraints**: Wasm validation must pass; use try_table/catch for exceptions
**Scale/Scope**: 45 compilation failures to resolve

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Requirement | Status | Notes |
|-----------|-------------|--------|-------|
| III. 関数・クロージャ実装戦略 | 多値バッファ使用 | PASS | mv-count (Global 2), mv-buffer (Global 3) already exist |
| IV. Wasm制御フロー活用 | handler-case → try_table | PENDING | Will implement per this principle |
| VII. TDD | Tests before implementation | PASS | Will follow Red-Green-Refactor |
| IX. HyperSpec参照 | Link to spec for CL forms | PASS | Will include in all documentation |

## Project Structure

### Documentation (this feature)

```text
specs/001-control-structure-extension/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output (AST extensions)
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (test contracts)
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # AST node definitions (modify for handler-case)
│   └── codegen/
│       └── func-section.lisp       # Wasm codegen (modify for handler-case)
├── conditions/
│   ├── handlers.lisp               # handler-case macro definition
│   └── types.lisp                  # Condition type hierarchy
├── runtime/
│   └── multi-value.lisp            # MV infrastructure (verify sufficiency)
└── lib/
    └── macros.lisp                 # Standard macro definitions

tests/
├── contract/                       # Wasm output validation tests
├── integration/                    # Full compilation pipeline tests
└── unit/                          # Unit tests for specific forms
```

**Structure Decision**: Single project layout; modifications to existing compiler files, no new directories needed.

## Complexity Tracking

> No violations requiring justification. Feature uses existing infrastructure.

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| handler-case | Use try_table/catch | Per Constitution IV, Wasm EH proposal |
| Type hierarchy | Use $lisp-error tag | Single exception type, dispatch in catch body |
| Forward references | Pre-allocate function indices | Standard Wasm pattern for mutual recursion |
