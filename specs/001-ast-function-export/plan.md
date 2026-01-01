# Implementation Plan: AST Function Export System

**Branch**: `001-ast-function-export` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-ast-function-export/spec.md`

## Summary

Export internal AST manipulation functions to the clysm package and register them in `*runtime-function-table*` for Wasm dispatch. Most functions are already package-exported but lack runtime registration, causing Stage 1 compilation failures (P944/P321/P543/P106 errors). Additionally, `get-numeric-value` is completely unexported.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: N/A (compile-time feature, no runtime performance impact)
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: 9 functions to export/register, ~50 LOC changes, unit tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Exported functions work with WasmGC types |
| II. Lisp Object Representation | PASS | N/A - no new object types |
| III. Function/Closure Strategy | PASS | Uses existing closure arity dispatch |
| IV. Wasm Control Flow | PASS | N/A - no control flow changes |
| V. Shallow Binding | PASS | N/A - no dynamic scope changes |
| VI. Tiered Eval/JIT | PASS | N/A - compile-time feature |
| VII. TDD (non-negotiable) | PASS | FR-011/FR-012 require unit tests |
| VIII. Nix-First Workflow | PASS | Works within existing nix environment |
| IX. ANSI CL HyperSpec Links | PASS | N/A - internal functions, not ANSI CL |

**Gate Result**: PASS - No violations, proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/001-ast-function-export/
├── plan.md              # This file
├── research.md          # Phase 0 output - function analysis
├── data-model.md        # Phase 1 output - runtime table entries
├── quickstart.md        # Phase 1 output - implementation guide
└── checklists/
    └── requirements.md  # Spec validation checklist
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # AST literal functions, get-numeric-value
│   └── codegen/
│       ├── func-section.lisp       # *runtime-function-table*, registration functions
│       └── gc-types.lisp           # wasm-struct-type definitions
└── package.lisp                    # Package exports

tests/unit/
├── ast-export-test.lisp            # NEW: Function availability tests
└── ast-dispatch-test.lisp          # NEW: Arity registration tests
```

**Structure Decision**: Single project layout. Changes touch existing compiler source files and add new unit tests.

## Complexity Tracking

No violations to justify. Feature uses existing patterns from 001-io-list-runtime.

## Phase 0: Research Summary

### Key Findings

| Function | Package Export | Runtime Registration | Action Needed |
|----------|----------------|---------------------|---------------|
| compile-to-instructions | YES | NO | Register with arity 2 |
| make-wasm-struct-type | YES | NO | Register (defstruct constructor) |
| wasm-struct-type-p | YES | NO | Register with arity 1 |
| wasm-struct-type-fields | YES | NO | Register with arity 1 |
| make-ast-literal | NO | NO | Export + Register |
| ast-literal-value | YES | NO | Register with arity 1 |
| ast-literal-p | YES | NO | Register with arity 1 |
| get-numeric-value | NO | NO | Export + Register with arity 1 |

### Implementation Pattern

From `func-section.lisp:191-203`, existing registration pattern:

```lisp
(defun register-ast-runtime-functions ()
  "Register AST manipulation functions for runtime dispatch."
  (register-runtime-function 'clysm:compile-to-instructions :$compile-to-instructions-rt 2)
  (register-runtime-function 'clysm:make-wasm-struct-type :$make-wasm-struct-type-rt nil)
  (register-runtime-function 'clysm:wasm-struct-type-p :$wasm-struct-type-p-rt 1)
  (register-runtime-function 'clysm:wasm-struct-type-fields :$wasm-struct-type-fields-rt 1)
  (register-runtime-function 'clysm:make-ast-literal :$make-ast-literal-rt nil)
  (register-runtime-function 'clysm:ast-literal-value :$ast-literal-value-rt 1)
  (register-runtime-function 'clysm:ast-literal-p :$ast-literal-p-rt 1)
  (register-runtime-function 'clysm:get-numeric-value :$get-numeric-value-rt 1))
```

### Arity Determination

| Function | Signature | Arity |
|----------|-----------|-------|
| compile-to-instructions | `(ast env)` | 2 |
| make-wasm-struct-type | `(&key name fields super)` | nil (variadic) |
| wasm-struct-type-p | `(object)` | 1 |
| wasm-struct-type-fields | `(struct)` | 1 |
| make-ast-literal | `(&key value literal-type)` | nil (variadic) |
| ast-literal-value | `(ast)` | 1 |
| ast-literal-p | `(object)` | 1 |
| get-numeric-value | `(ast)` | 1 |

## Phase 1: Design

### Data Model

See [data-model.md](./data-model.md) for runtime function table entry specifications.

### Contracts

No external API contracts - internal compiler functions only.

### Implementation Steps

1. **Export missing functions** in `package.lisp`:
   - Import `make-ast-literal` from `clysm/compiler/ast`
   - Import `get-numeric-value` from `clysm/compiler/ast`
   - Re-export both in `:export` list

2. **Create registration function** in `func-section.lisp`:
   - Add `register-ast-runtime-functions` after existing registration functions
   - Register all 8 functions with correct arities
   - Call registration function at module load time

3. **Add unit tests**:
   - `tests/unit/ast-export-test.lisp`: Function availability tests
   - `tests/unit/ast-dispatch-test.lisp`: Arity registration tests

4. **Verify Stage 1 compilation**:
   - Run `sbcl --load build/stage1-complete.lisp`
   - Confirm P944/P321/P543/P106 errors eliminated
   - Confirm compilation rate increase to 25%+
   - Validate Wasm output

### Success Validation

```bash
# Unit tests
sbcl --eval "(asdf:test-system :clysm)"

# Stage 1 generation
sbcl --load build/stage1-complete.lisp

# Wasm validation
wasm-tools validate dist/clysm-stage1.wasm

# Error pattern check (should be 0)
grep -c "P944\|P321\|P543\|P106" dist/stage1-report.json
```
