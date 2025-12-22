# Implementation Plan: Special Variables Compiler Integration

**Branch**: `002-special-vars-compiler` | **Date**: 2025-12-22 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-special-vars-compiler/spec.md`

## Summary

Implement compiler integration for special variables (dynamic scope variables) in Clysm WasmGC Common Lisp compiler. This includes:
- AST nodes for `defvar` and `defparameter`
- Compiler recognition of special variables vs lexical variables
- Code generation for symbol-value access and dynamic binding with `let`
- Integration with `unwind-protect` for exception-safe binding restoration

The implementation follows Constitution Principle V (シャローバインディングによる動的スコープ) using shallow binding with symbol's `$value` field and a binding stack for restoration.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: Rove test framework (`tests/` directory structure)
**Target Platform**: WebAssembly GC (WasmGC) - browsers, wasmtime
**Project Type**: Single project (compiler)
**Performance Goals**: O(1) special variable access (shallow binding per Constitution V)
**Constraints**: WasmGC-only (no linear memory), Wasm sandbox (per Constitution security)
**Scale/Scope**: Compiler supporting Common Lisp subset for macro system support

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Symbol struct already has `$value` field in WasmGC |
| II. Lisp Object Representation | PASS | Uses existing symbol struct, UNBOUND sentinel |
| III. Function/Closure Strategy | N/A | No changes to closure representation |
| IV. Wasm Control Flow | PASS | Uses existing `try_table`/`unwind-protect` for restoration |
| **V. Shallow Binding** | **CORE** | This feature implements Principle V exactly |
| VI. Tiered Eval | N/A | No changes to eval model |
| VII. TDD | REQUIRED | All implementation must follow Red-Green-Refactor |
| VIII. Nix-First | REQUIRED | `nix flake check` must pass |

**Constitution V Implementation Mapping**:
1. Each symbol has `$value` field → Already exists in `gc-types.lisp` (`+type-symbol+`)
2. Bind: Push old value to Trail, write new value → Will implement in codegen
3. Read: Direct `$value` field access (O(1)) → Will implement in `compile-var-ref`
4. Restore: Use `try_table`/`unwind-protect` → Will leverage existing exception handling

**Gate Result**: PASS - All applicable principles satisfied or will be implemented per spec.

## Project Structure

### Documentation (this feature)

```text
specs/002-special-vars-compiler/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (internal contracts)
└── tasks.md             # Phase 2 output (NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp              # ADD: ast-defvar, ast-defparameter nodes
│   ├── env.lisp              # MODIFY: special variable registry
│   └── codegen/
│       └── func-section.lisp # MODIFY: compile-var-ref, compile-let, compile-setq
├── runtime/
│   └── special-vars.lisp     # MODIFY: WasmGC binding stack structure

tests/
├── unit/
│   └── special-vars-ast-test.lisp   # ADD: AST node tests
├── contract/
│   └── special-vars-codegen-test.lisp # ADD: Generated code validation
└── integration/
    └── special-var-test.lisp         # MODIFY: Fill in placeholder tests
```

**Structure Decision**: Extends existing single-project compiler structure. No new modules needed; modifications to existing AST, codegen, and runtime components.

## Complexity Tracking

No constitution violations requiring justification. Implementation follows Constitution V directly.
