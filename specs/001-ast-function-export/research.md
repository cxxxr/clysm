# Research: AST Function Export System

**Feature**: 001-ast-function-export
**Date**: 2026-01-01
**Status**: Complete

## Research Questions

### Q1: Where are the target functions defined?

**Decision**: Functions are defined in three source files

**Findings**:

| Function | File | Line | Type |
|----------|------|------|------|
| compile-to-instructions | src/clysm/compiler/codegen/func-section.lisp | 393 | defun |
| make-wasm-struct-type | src/clysm/compiler/codegen/gc-types.lisp | 125 | defstruct constructor |
| wasm-struct-type-p | src/clysm/compiler/codegen/gc-types.lisp | 125 | defstruct predicate |
| wasm-struct-type-fields | src/clysm/compiler/codegen/gc-types.lisp | 125 | defstruct accessor |
| make-ast-literal | src/clysm/compiler/ast.lisp | 28 | defstruct constructor |
| ast-literal-value | src/clysm/compiler/ast.lisp | 28 | defstruct accessor |
| ast-literal-p | src/clysm/compiler/ast.lisp | 28 | defstruct predicate |
| get-numeric-value | src/clysm/compiler/ast.lisp | 916 | defun |

### Q2: What is the current package export status?

**Decision**: Most functions already exported, two missing

**Findings**:

| Function | Exported to clysm | Runtime Registered |
|----------|-------------------|-------------------|
| compile-to-instructions | YES (line 1434) | NO |
| make-wasm-struct-type | YES (line 1414) | NO |
| wasm-struct-type-p | YES (line 1415) | NO |
| wasm-struct-type-fields | YES (line 1416) | NO |
| make-ast-literal | **NO** | NO |
| ast-literal-value | YES (line 1418) | NO |
| ast-literal-p | YES (line 1419) | NO |
| get-numeric-value | **NO** | NO |

**Rationale**: Package exports in `package.lisp` allow symbol access, but `*runtime-function-table*` registration is required for Wasm dispatch.

### Q3: How does the runtime function table work?

**Decision**: Use existing registration pattern from 001-io-list-runtime

**Findings**:

The `*runtime-function-table*` (func-section.lisp:66-77) maps Lisp function symbols to runtime function names for Wasm dispatch:

```lisp
;; Structure: symbol -> (runtime-name . arity-or-nil)
(defparameter *runtime-function-table* (make-hash-table :test 'eq))
```

Key functions:
- `register-runtime-function` (lines 78-84): Registers a function
- `runtime-function-p` (lines 86-88): Checks if registered
- `compile-runtime-call` (lines 90-107): Compiles call instruction

Existing registration groups:
- `register-io-runtime-functions` (lines 113-125)
- `register-list-runtime-functions` (lines 127-148)
- `register-sequence-runtime-functions` (lines 150-172)
- `register-package-runtime-functions` (lines 174-189)
- `register-lexenv-runtime-functions` (lines 191-203)

All called at module load time (lines 215-219).

**Rationale**: Follow established pattern for consistency and maintainability.

### Q4: What arities should be used?

**Decision**: Derive from function signatures

**Findings**:

| Function | Signature | Arity | Notes |
|----------|-----------|-------|-------|
| compile-to-instructions | `(ast env)` | 2 | Required positional args |
| make-wasm-struct-type | `(&key name fields super)` | nil | Keyword args = variadic |
| wasm-struct-type-p | `(object)` | 1 | Single arg predicate |
| wasm-struct-type-fields | `(struct)` | 1 | Single arg accessor |
| make-ast-literal | `(&key value literal-type)` | nil | Keyword args = variadic |
| ast-literal-value | `(ast)` | 1 | Single arg accessor |
| ast-literal-p | `(object)` | 1 | Single arg predicate |
| get-numeric-value | `(ast)` | 1 | Single arg |

**Rationale**: `nil` arity for variadic functions (keyword args), specific arity for fixed-arg functions.

## Alternatives Considered

### Alternative 1: Inline Wasm codegen instead of runtime dispatch

**Rejected because**: Would require duplicating complex logic in the code generator. Runtime dispatch is simpler and matches existing patterns.

### Alternative 2: Export to separate package (clysm/internal)

**Rejected because**: Would require updating all call sites. Using existing clysm package is simpler and matches prior features (001-internal-function-export).

## Implementation Recommendations

1. Add `register-ast-runtime-functions` after existing registration functions
2. Export `make-ast-literal` and `get-numeric-value` via package.lisp
3. Register all 8 functions with appropriate arities
4. Add unit tests following pattern from `lexenv-export-test.lisp`
