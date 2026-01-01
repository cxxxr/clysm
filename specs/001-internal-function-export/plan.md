# Implementation Plan: Internal Function Export System

**Branch**: `001-internal-function-export` | **Date**: 2026-01-01 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-internal-function-export/spec.md`

## Summary

Export internal compiler functions from subpackages to the main `clysm` package to resolve undefined function errors during Stage 1 compilation. Functions like [lexical-env-parent](../../src/clysm/compiler/env.lisp), [compile-to-instructions](../../src/clysm/compiler/codegen/func-section.lisp), and [make-wasm-struct-type](../../src/clysm/compiler/codegen/gc-types.lisp) are defined and exported from their internal packages but not re-exported to the main `clysm` package. Additionally, implement `PACKAGEP*` as a Wasm type predicate and fix quasiquote expansion to handle backquote syntax properly.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove (testing framework), wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler)
**Performance Goals**: Stage 1 compilation rate 35%+ (from 21.57%)
**Constraints**: Must pass `wasm-tools validate`, no runtime changes
**Scale/Scope**: 8 functions to re-export, 1 type predicate to add, 1 quasiquote fix

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | PACKAGEP* uses `ref.test` for type checking |
| II. Lispオブジェクト表現規約 | ✅ PASS | No changes to NIL/UNBOUND representation |
| III. 関数・クロージャ実装戦略 | ✅ PASS | No closure structure changes |
| IV. Wasm制御フロー活用 | ✅ PASS | No control flow changes |
| V. シャローバインディング | ✅ PASS | No binding changes |
| VI. 段階的動的コンパイル | ✅ PASS | No eval/JIT changes |
| VII. テスト駆動開発（TDD） | ✅ PASS | Tests before implementation |
| VIII. Nix-Firstワークフロー | ✅ PASS | Uses existing Nix environment |
| IX. ANSI CL仕様参照規約 | ✅ PASS | [packagep](resources/HyperSpec/Body/f_pkgp.htm) reference included |

**Gate Result**: PASS - No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/001-internal-function-export/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A for this feature)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── package.lisp                    # Main package - ADD re-exports here
├── compiler/
│   ├── env.lisp                    # LEXICAL-ENV-PARENT defined
│   ├── ast.lisp                    # AST-LITERAL-VALUE defined
│   └── codegen/
│       ├── func-section.lisp       # COMPILE-TO-INSTRUCTIONS, type predicates
│       └── gc-types.lisp           # MAKE-WASM-STRUCT-TYPE defined

tests/
├── unit/
│   └── internal-export-test.lisp   # New: unit tests for exports
└── contract/
    └── stage1-exports-test.lisp    # Existing: Stage 1 export validation
```

**Structure Decision**: Single project structure - modifying existing `package.lisp` and `func-section.lisp`

## Complexity Tracking

No violations to justify. All changes follow existing patterns.

## Implementation Approach

### Phase 1: Re-export Internal Functions (P1)

**Finding**: Functions ARE exported from their internal packages but NOT re-exported to main `clysm` package.

**Solution**: Add `:import-from` and `:export` clauses to `src/clysm/package.lisp`:

```lisp
;; In clysm package definition
(:import-from #:clysm/compiler/env
              #:lexical-env-parent
              #:lexical-env-bindings
              #:make-lexical-env)

(:import-from #:clysm/compiler/codegen/func-section
              #:compile-to-instructions)

(:import-from #:clysm/compiler/codegen/gc-types
              #:make-wasm-struct-type
              #:wasm-struct-type-p
              #:wasm-struct-type-fields)

(:import-from #:clysm/compiler/ast
              #:ast-literal-value
              #:ast-literal-p)
```

### Phase 2: Implement PACKAGEP* Type Predicate (P2)

**Pattern**: Follow existing CONSP* implementation in `func-section.lisp`:

```lisp
(defun compile-packagep* (args env)
  "Compile (packagep* x) - returns T if x is a package, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "packagep* requires exactly 1 argument"))
  (let ((result '()))
    (setf result (append result (compile-to-instructions (first args) env)))
    ;; Test if it's a package (use package type index)
    (setf result (append result
                         (list (list :ref.test
                                     (list :ref +type-package+)))))
    ;; Convert i32 boolean to Lisp boolean
    (setf result (append result
                         `((:if (:result :anyref))
                           (:i32.const 1) :ref.i31
                           :else
                           (:ref.null :none)
                           :end)))
    result))
```

Add dispatch case in `compile-function-call`:
```lisp
((eq fn 'packagep*) (compile-packagep* args env))
```

### Phase 3: Fix QUASIQUOTE Handling (P3)

**Finding**: `expand-backquote` is exported but quasiquote reader macro form isn't being handled during compilation.

**Solution**: In `src/clysm/compiler/codegen/func-section.lisp`, add special handling for quasiquote:

```lisp
;; In compile-expression or compile-function-call
((eq head 'quasiquote)
 (compile-to-instructions (clysm/compiler/transform/macro:expand-backquote form) env))
```

### Phase 4: Validate and Regenerate Stage 1 (P4)

1. Run `sbcl --load build/stage1-complete.lisp`
2. Verify `wasm-tools validate dist/clysm-stage1.wasm`
3. Check `dist/stage1-report.json` for error pattern elimination
4. Confirm compilation rate ≥ 35%

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Symbol conflicts on re-export | Low | Medium | Use explicit `:import-from` |
| Package type index undefined | Low | High | Verify +type-package+ exists in gc-types |
| Quasiquote expansion loops | Medium | Medium | Add depth limit to expansion |
| Compilation rate < 35% | Medium | Low | Identify next blockers for future iteration |
