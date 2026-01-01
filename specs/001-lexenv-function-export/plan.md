# Implementation Plan: Lexical Environment Function Export System

**Branch**: `001-lexenv-function-export` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-lexenv-function-export/spec.md`

## Summary

Export internal compiler functions (`env-add-local`, `env-lookup`, `make-lexical-env`, `loop-keyword-eq`, `numeric-literal-p`) to the public clysm package and register them in the runtime function table. This enables Stage 1 compilation to access these functions, eliminating 118 ENV-ADD-LOCAL undefined function errors and increasing coverage from 21.43% to 25%+.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove for unit tests, wasm-tools for validation, Stage 1 report for coverage metrics
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (Common Lisp compiler)
**Performance Goals**: Stage 1 coverage ≥25%, ENV-ADD-LOCAL errors = 0
**Constraints**: Wasm validation must pass (`wasm-tools validate`), no regression in existing compilation
**Scale/Scope**: 5 functions to export, ~50 lines of package definition changes

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ Pass | No type system changes; uses existing WasmGC infrastructure |
| II. Lispオブジェクト表現規約 | ✅ Pass | No object representation changes |
| III. 関数・クロージャ実装戦略 | ✅ Pass | Exports existing functions; no new closure patterns |
| IV. Wasm制御フロー活用 | ✅ Pass | No control flow changes |
| V. シャローバインディング | ✅ Pass | No dynamic scope changes |
| VI. 段階的動的コンパイル | ✅ Pass | Supports Stage 1 compilation; no JIT changes |
| VII. TDD (非交渉) | ⚠️ Required | Must write tests before implementation |
| VIII. Nix-First | ⚠️ Required | `wasm-tools validate` must pass |
| IX. ANSI CL仕様参照 | ✅ Pass | Internal compiler functions; no ANSI CL standard functions affected |

**Gate Result**: PASS (with TDD and validation requirements)

## Project Structure

### Documentation (this feature)

```text
specs/001-lexenv-function-export/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A - no API contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── package.lisp                          # MODIFY: Add :import-from and :export
├── compiler/
│   ├── env.lisp                          # READ: make-lexical-env already exported
│   ├── ast.lisp                          # READ: numeric-literal-p definition
│   └── codegen/
│       └── func-section.lisp             # MODIFY: Register functions in runtime table
└── lib/
    └── macros.lisp                       # READ: loop-keyword-eq definition

tests/
├── unit/
│   └── lexenv-export-test.lisp           # NEW: Unit tests for exported functions
└── contract/
    └── stage1-coverage-test.lisp         # MODIFY: Add coverage assertions
```

**Structure Decision**: Single project structure. Modifications are limited to package.lisp (exports) and func-section.lisp (runtime registration). Tests added to existing test directories.

## Complexity Tracking

No constitution violations requiring justification.

## Research Findings

### Function Locations and Current Export Status

| Function | Definition File | Package | Currently Exported to clysm? |
|----------|----------------|---------|------------------------------|
| `env-add-local` | `compiler/codegen/func-section.lisp:220` | clysm/compiler/codegen/func-section | Yes (to func-section package), No (to clysm) |
| `env-lookup` | `eval/interpreter.lisp:249` | clysm/eval/interpreter | Yes (to interpreter package), No (to clysm) |
| `make-lexical-env` | `compiler/env.lisp` (defstruct) | clysm/compiler/env | **Already exported to clysm** (001-internal-function-export) |
| `loop-keyword-eq` | `lib/macros.lisp:818` | clysm/lib/macros | No |
| `numeric-literal-p` | `compiler/ast.lisp:910` | clysm/compiler/ast | No |

### Runtime Function Table Pattern

From `func-section.lisp:69-107`:
```lisp
(defparameter *runtime-function-table* (make-hash-table :test 'eq))

(defun register-runtime-function (symbol runtime-name &optional arity)
  (setf (gethash symbol *runtime-function-table*)
        (cons runtime-name arity)))

(defun runtime-function-p (symbol)
  (gethash symbol *runtime-function-table*))

(defun compile-runtime-call (form args env)
  ;; Compiles arguments and emits :call instruction
  ...)
```

### Export Pattern (from 001-internal-function-export)

```lisp
;; In clysm package definition (package.lisp):
(:import-from #:clysm/compiler/env
              #:make-lexical-env)

(:export #:make-lexical-env)
```

### Key Insight: make-lexical-env Already Exported

The previous feature (001-internal-function-export, commit 55d1387) already exported `make-lexical-env`. This reduces scope to 4 functions:
- `env-add-local`
- `env-lookup` (interpreter version) or `lookup-binding` (compiler version)
- `loop-keyword-eq`
- `numeric-literal-p`

### Note on env-lookup vs lookup-binding

Two separate functions exist:
1. **`env-lookup`** (interpreter.lisp:249) - For interpreter environment
2. **`lookup-binding`** (env.lisp:25) - For compiler lexical environment

The Stage 1 errors likely reference `env-add-local` (compiler function), suggesting the compiler environment functions are needed. Need to verify which exact symbol is causing the 118 errors.
