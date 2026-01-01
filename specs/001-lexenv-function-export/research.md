# Research: Lexical Environment Function Export

**Date**: 2026-01-01
**Feature**: 001-lexenv-function-export

## Overview

Research to resolve unknowns from the Technical Context and validate function export requirements.

## Findings

### 1. Current Stage 1 Error Analysis

From `dist/stage1-report.json`:

| Pattern ID | Error Pattern | Count | Percentage | Priority |
|------------|--------------|-------|------------|----------|
| P114 | Undefined function: ENV-ADD-LOCAL | 118 | 13.52% | HIGH |
| P628 | Undefined function: LOOP-KEYWORD-EQ | 10 | 1.15% | MEDIUM |
| P369 | Undefined function: NUMERIC-LITERAL-P | 10 | 1.15% | MEDIUM |

**Note**: `env-lookup` does not appear in the error patterns, suggesting it is either:
- Not referenced in compiled forms, or
- Already accessible through a different mechanism

### 2. Function Definitions and Export Status

| Function | File | Line | Package | Status |
|----------|------|------|---------|--------|
| `env-add-local` | `src/clysm/compiler/codegen/func-section.lisp` | 220 | clysm/compiler/codegen/func-section | **Needs export** |
| `env-lookup` | `src/clysm/eval/interpreter.lisp` | 249 | clysm/eval/interpreter | Not in errors |
| `make-lexical-env` | `src/clysm/compiler/env.lisp` | defstruct | clysm/compiler/env | **Already exported** |
| `loop-keyword-eq` | `src/clysm/lib/macros.lisp` | 818 | clysm/lib/macros | **Needs export** |
| `numeric-literal-p` | `src/clysm/compiler/ast.lisp` | 910 | clysm/compiler/ast | **Needs export** |

### 3. Related Error Patterns (Already Addressed or Different Features)

| Pattern | Function | Status |
|---------|----------|--------|
| P944 | COMPILE-TO-INSTRUCTIONS | Exported in 001-internal-function-export |
| P321 | MAKE-WASM-STRUCT-TYPE | Exported in 001-internal-function-export |
| P457 | COMPILE-UNARY-MATH-FFI | Exported in 001-internal-function-consolidation |
| P626 | COMPILE-CXR-CHAIN | Exported in 001-internal-function-consolidation |
| P543 | AST-LITERAL-VALUE | Exported in 001-internal-function-export |
| P951 | PACKAGEP* | Exported in 001-internal-function-export |

### 4. Runtime Function Table Registration

**Decision**: Register compiler internal functions in `*runtime-function-table*` for Stage 1 dispatch.

**Rationale**: The runtime function table (`*runtime-function-table*`) provides the dispatch mechanism for compiled code to call runtime functions. Functions must be registered with:
- Symbol (function name)
- Runtime name (keyword like `:$env-add-local-rt`)
- Arity (parameter count or nil for variadic)

**Alternatives Considered**:
1. Direct FFI import - Rejected: Would require host shim modification
2. Inline Wasm generation - Rejected: Functions have complex logic unsuitable for inlining

### 5. Package Export Pattern

**Decision**: Use `:import-from` + `:export` pattern in clysm package definition.

**Rationale**: Consistent with 001-internal-function-export which successfully exported 9 functions using this pattern.

**Implementation**:
```lisp
;; In defpackage clysm:
(:import-from #:clysm/compiler/codegen/func-section
              #:env-add-local)
(:import-from #:clysm/lib/macros
              #:loop-keyword-eq)
(:import-from #:clysm/compiler/ast
              #:numeric-literal-p)

(:export #:env-add-local
         #:loop-keyword-eq
         #:numeric-literal-p)
```

### 6. Expected Impact

**Before** (current baseline):
- Stage 1 coverage: 21.43%
- ENV-ADD-LOCAL errors: 118
- LOOP-KEYWORD-EQ errors: 10
- NUMERIC-LITERAL-P errors: 10
- **Total addressable errors**: 138

**After** (projected):
- ENV-ADD-LOCAL errors: 0
- LOOP-KEYWORD-EQ errors: 0
- NUMERIC-LITERAL-P errors: 0
- **Errors eliminated**: 138

**Coverage calculation**:
- Current compiled: 5328
- Current failed: 19537
- Errors to eliminate: 138
- New compiled estimate: 5328 + 138 = 5466
- New coverage: 5466 / (26271 - 1406) ≈ 22.0%

**Note**: The 25%+ target may require additional function exports beyond this scope.

### 7. Scope Refinement

Based on research, the spec's 5 functions reduce to 3 actionable items:

| Original Spec | Action |
|--------------|--------|
| `env-add-local` | **Export and register** |
| `env-lookup` | Skip (not in error patterns) |
| `make-lexical-env` | Skip (already exported) |
| `loop-keyword-eq` | **Export and register** |
| `numeric-literal-p` | **Export and register** |

### 8. Dependencies

No transitive dependencies identified. Each function is self-contained:
- `env-add-local`: Uses cenv-* accessors (already internal to func-section)
- `loop-keyword-eq`: Uses only `symbolp`, `symbol-name`, `string-equal` (CL builtins)
- `numeric-literal-p`: Uses `typep`, `member`, `ast-literal-literal-type` (ast.lisp internal)

### 9. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Runtime table registration breaks existing dispatch | Low | High | Unit test before/after registration |
| Package export causes symbol conflicts | Low | Medium | Check for existing exports before adding |
| Insufficient to reach 25% target | Medium | Low | Document as partial improvement; identify next blockers |

## Conclusions

1. **Reduced scope**: 3 functions to export (not 5 as originally specified)
2. **Pattern established**: Follow 001-internal-function-export precedent
3. **Expected improvement**: ~138 errors eliminated, coverage ~22%
4. **Next steps**: May need additional feature for 25%+ target
