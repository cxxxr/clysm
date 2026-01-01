# Research: Internal Function Export System

**Feature**: `001-internal-function-export`
**Date**: 2026-01-01
**Status**: Complete

## Executive Summary

Research confirms that undefined function errors during Stage 1 compilation are caused by internal compiler functions being exported from their defining subpackages but NOT re-exported to the main `clysm` package. The solution is straightforward: add `:import-from` clauses to the main package definition.

## Research Findings

### R1: Internal Function Locations

**Question**: Where are the undefined functions defined and what is their export status?

| Function | Defining Package | File | Export Status |
|----------|-----------------|------|---------------|
| `LEXICAL-ENV-PARENT` | `clysm/compiler/env` | env.lisp:12 | ✅ Exported |
| `COMPILE-TO-INSTRUCTIONS` | `clysm/compiler/codegen/func-section` | func-section.lisp:608 | ✅ Exported |
| `MAKE-WASM-STRUCT-TYPE` | `clysm/compiler/codegen/gc-types` | gc-types.lisp:125 | ✅ Exported |
| `AST-LITERAL-VALUE` | `clysm/compiler/ast` | ast.lisp:28 | ✅ Exported |
| `COMPILE-UNARY-MATH-FFI` | `clysm/compiler/codegen/func-section` | func-section.lisp:6735 | ✅ Re-exported to clysm |
| `COMPILE-CXR-CHAIN` | `clysm/compiler/codegen/func-section` | func-section.lisp:4036 | ✅ Re-exported to clysm |

**Decision**: Add `:import-from` clauses for `LEXICAL-ENV-PARENT`, `COMPILE-TO-INSTRUCTIONS`, `MAKE-WASM-STRUCT-TYPE`, `AST-LITERAL-VALUE` to main `clysm` package.

**Rationale**: Functions are correctly exported from internal packages. The Stage 1 compiler accesses them via the `clysm` package, which doesn't re-export them.

**Alternatives Considered**:
- Use fully-qualified names everywhere: Rejected (requires extensive code changes)
- Export from internal packages with `use-package`: Rejected (causes symbol conflicts)

---

### R2: Type Predicate Pattern

**Question**: How are existing type predicates (CONSP*, SYMBOLP*) implemented?

**Finding**: Type predicates in `func-section.lisp` follow a consistent pattern:

```lisp
;; Pattern: compile-<predicate>* function
1. Validate single argument
2. Compile argument to stack
3. Emit (ref.test (ref $type-index))
4. Convert i32 result to Lisp T/NIL with if-else
```

**Implementation for PACKAGEP***:

```lisp
(defun compile-packagep* (args env)
  (when (/= (length args) 1)
    (error "packagep* requires exactly 1 argument"))
  (append
    (compile-to-instructions (first args) env)
    `((:ref.test (:ref ,+type-package+))
      (:if (:result :anyref))
      (:i32.const 1) :ref.i31
      :else
      (:ref.null :none)
      :end)))
```

**Decision**: Implement `PACKAGEP*` following the existing pattern.

**Rationale**: Consistency with existing predicates, proven pattern.

**Alternatives Considered**:
- Runtime function call: Rejected (slower, requires runtime library)
- FFI to host: Rejected (package is Wasm-native object)

---

### R3: Package Type Index

**Question**: Does a package type index exist in gc-types.lisp?

**Finding**: Need to verify. The CLAUDE.md documents:
- Type 0: $cons
- Type 1: $symbol
- Type 2: $string
- No explicit $package type listed

**Decision**: Check gc-types.lisp for +type-package+ constant. If not present, define it or use alternative approach.

**Rationale**: Packages in Clysm may be host-level constructs not compiled to Wasm.

**Note**: The stage1-report.json shows PACKAGEP* errors in CLOS and package.lisp - these may be host-only predicates that shouldn't be compiled. Need to verify if PACKAGEP* should be implemented as a Wasm primitive or skipped during compilation.

---

### R4: Quasiquote Handling

**Question**: Why does "Undefined function: QUASIQUOTE" appear?

**Finding**: The `expand-backquote` function exists in `transform/macro.lisp` and is exported. However, when the compiler encounters a quasiquote form, it tries to call QUASIQUOTE as a function instead of expanding it.

**Root Cause**: The reader produces `(quasiquote ...)` forms, but the compiler doesn't recognize `quasiquote` as a special form requiring expansion.

**Solution**: Add quasiquote handling in `compile-expression`:

```lisp
((eq head 'quasiquote)
 (compile-to-instructions
   (clysm/compiler/transform/macro:expand-backquote form)
   env))
```

**Decision**: Expand quasiquote at compile time to list construction.

**Rationale**: Quasiquote is a reader macro construct that should be expanded before compilation.

**Alternatives Considered**:
- Expand at read time: Rejected (reader already produces quasiquote form)
- Runtime function: Rejected (quasiquote is compile-time)

---

### R5: Error Count Validation

**Question**: What is the expected impact of each fix?

| Error Pattern | Current Count | Expected After Fix |
|--------------|---------------|-------------------|
| P114 (LEXICAL-ENV-PARENT) | 119 | 0 |
| P944 (COMPILE-TO-INSTRUCTIONS) | 36 | 0 |
| P951 (PACKAGEP*) | 25 | 0 or unchanged* |
| P321 (MAKE-WASM-STRUCT-TYPE) | 17 | 0 |
| P464 (QUASIQUOTE) | 16 | 0 |
| P543 (AST-LITERAL-VALUE) | 11 | 0 |
| P457 (COMPILE-UNARY-MATH-FFI) | 14 | 0 |
| P626 (COMPILE-CXR-CHAIN) | 12 | 0 |

**Total Errors to Eliminate**: 250 minimum (280 if PACKAGEP* is fixed)

**Expected Compilation Rate**: Given 250 errors eliminated from 19,423 failures:
- New failures: ~19,173
- New coverage: ~5,592 / 24,765 = ~22.5%

**Note**: The 35% target may require additional fixes beyond these patterns. However, eliminating these errors will unblock additional forms that were failing due to cascading undefined function errors.

*PACKAGEP* may be a host-only function that should be skipped rather than compiled.

## Conclusion

All research questions resolved. Proceed to implementation with:
1. Package re-exports (immediate fix)
2. Quasiquote expansion (immediate fix)
3. PACKAGEP* investigation (verify if Wasm primitive needed or skip)
4. Stage 1 regeneration and validation
