# Research: Compiler Code Generation Cleanup

**Date**: 2026-01-04
**Feature**: 001-codegen-cleanup

## Overview

This research document analyzes the dead code cleanup opportunity in func-section.lisp after runtime library migration, and establishes the methodology for safe removal.

## Research Area 1: Runtime Function Migration Status

### Decision
Functions registered in `*runtime-function-table*` via `register-runtime-function` have their functionality migrated to Lisp runtime libraries. The original `compile-*` implementations in func-section.lisp are now dead code.

### Current Registrations

| Category | Functions | Registration Function |
|----------|-----------|----------------------|
| I/O | princ, prin1, print, write, terpri, format | `register-io-runtime-functions` |
| List | member, member-if, member-if-not, assoc, assoc-if, rassoc, rassoc-if, find, find-if, find-if-not, position, position-if, position-if-not | `register-list-runtime-functions` |
| Sequence | remove, remove-if, remove-if-not, count, count-if, count-if-not, substitute, substitute-if, substitute-if-not, delete, delete-if, delete-if-not, subseq, copy-seq, adjust-array | `register-sequence-runtime-functions` |
| String | char, schar, string-trim, string-left-trim, string-right-trim, string-capitalize, nstring-capitalize, string-equal, string-not-equal, string-lessp, string-greaterp, string-not-lessp, string-not-greaterp | `register-string-runtime-functions` |
| Numeric | parse-integer, write-to-string, rationalize, signum, phase | `register-numeric-runtime-functions` |
| Package | packagep*, find-package*, intern*, symbol-package* | `register-package-runtime-functions` |
| Lexenv | env-add-local, loop-keyword-eq, numeric-literal-p | `register-lexenv-runtime-functions` |
| AST | compile-to-instructions, make-wasm-struct-type, wasm-struct-type-p, wasm-struct-type-fields, make-ast-literal, ast-literal-value, ast-literal-p, get-numeric-value | `register-ast-runtime-functions` |
| Parser | advance-token, current-token, make-parser-state | `register-parser-runtime-functions` |
| Backend | emit-module-header | `register-backend-runtime-functions` |

**Total migrated functions**: 64

### Rationale
Runtime library dispatch via `*runtime-function-table*` provides:
1. Centralized implementation in maintainable Lisp code
2. Easier testing and debugging
3. Single point of change for bug fixes
4. Consistent behavior across compiler and interpreter

### Alternatives Considered
- Keep inline Wasm codegen: Rejected - duplicates logic, harder to maintain
- Remove dispatch table: Rejected - would revert all migration benefits

## Research Area 2: Dead Code Detection Strategy

### Decision
Use static analysis based on `*runtime-function-table*` registrations combined with grep-based call graph analysis.

### Detection Algorithm

1. **Primary Detection**: Any `compile-*` function whose corresponding CL function is registered in `*runtime-function-table*` is dead
2. **Call Graph Analysis**: Build callee→caller map for func-section.lisp
3. **Transitive Analysis**: Helper functions only called by dead `compile-*` functions are also dead

### Implementation Approach
```lisp
;; Pseudo-code for dead code detection
(defun find-dead-compile-functions ()
  "Find compile-* functions that are now dead after migration."
  (let ((migrated (hash-table-keys *runtime-function-table*)))
    (loop for func in migrated
          for compile-fn = (intern (format nil "COMPILE-~A" func))
          when (fboundp compile-fn)
          collect compile-fn)))
```

### Rationale
Static analysis is sufficient because:
1. Lisp's `defun` forms are explicit
2. Function calls use symbols that can be grepped
3. Macro-expanded code can be traced

### Alternatives Considered
- Runtime instrumentation: Rejected - overhead, doesn't catch all paths
- Manual audit only: Rejected - error-prone for 15K lines

## Research Area 3: Quasiquote to with-instruction-collector Migration

### Decision
Replace `,@` patterns with `with-instruction-collector` macro for consistent instruction building.

### Pattern Transformation

**Before (quasiquote)**:
```lisp
(defun compile-foo (args env)
  `(,@(compile-expr (car args) env)
    ,@(compile-expr (cadr args) env)
    (i32.add)))
```

**After (with-instruction-collector)**:
```lisp
(defun compile-foo (args env)
  (with-instruction-collector
    (emit* (compile-expr (car args) env))
    (emit* (compile-expr (cadr args) env))
    (emit :i32.add)))
```

### Current Status
- 82 existing uses of `with-instruction-collector`
- 111 remaining `,@` patterns to migrate
- Patterns are semantically equivalent

### Rationale
- Consistent idiom across codebase
- Explicit `emit` calls are more readable
- Macro handles instruction buffer management

### Alternatives Considered
- Leave mixed patterns: Rejected - inconsistent, confusing
- Create new macro: Rejected - `with-instruction-collector` already exists and works

## Research Area 4: Batch Validation Strategy

### Decision
Remove dead code in logical batches grouped by functionality, validating after each batch.

### Batch Organization

| Batch | Category | Estimated Functions | Estimated Lines |
|-------|----------|---------------------|-----------------|
| 1 | String functions | ~15 | ~1,500 |
| 2 | Numeric functions | ~10 | ~800 |
| 3 | Sequence functions | ~20 | ~2,000 |
| 4 | List functions | ~15 | ~1,200 |
| 5 | I/O functions | ~8 | ~600 |
| 6 | Misc helpers | ~30 | ~1,500 |

### Validation Protocol

After each batch:
1. Run `sbcl --eval "(asdf:test-system :clysm)"` - all tests must pass
2. Run `sbcl --load build/stage1-complete.lisp` - must complete
3. Run `wasm-tools validate dist/clysm-stage1.wasm` - must exit 0
4. Compare line count: `wc -l func-section.lisp`

### Rollback Protocol
If any validation step fails:
1. Identify the specific function removal that caused failure
2. Restore that function
3. Re-run validation to confirm fix
4. Continue with remaining batch items

### Rationale
Batch by category because:
1. Related functions often share helpers
2. Test failures easier to diagnose
3. Natural stopping points for review

### Alternatives Considered
- Remove all at once: Rejected - too risky, hard to diagnose failures
- Remove one function at a time: Rejected - too slow, 64+ functions

## Research Area 5: Helper Function Analysis

### Decision
Build transitive dependency graph to identify helpers that become dead after primary function removal.

### Helper Patterns to Analyze

1. **Emit helpers**: `emit-*` functions used only by dead compile-* functions
2. **Type helpers**: `compile-*-type` functions for removed operations
3. **Argument helpers**: `compile-*-args` for variadic handling
4. **Validation helpers**: `check-*` functions for removed operations

### Analysis Method

```bash
# For each dead compile-* function, find its callees
grep -n "compile-string-trim" func-section.lisp
# Then check if those callees have other live callers
grep -n "emit-string-trim-helper" func-section.lisp
```

### Rationale
Helpers may account for 30-40% of removable code. Without transitive analysis, significant dead code would remain.

### Alternatives Considered
- Remove only compile-* functions: Rejected - leaves orphaned helpers
- Remove all helpers aggressively: Rejected - may break live code

## Summary

| Research Area | Key Decision |
|---------------|--------------|
| Migration Status | 64 functions migrated to runtime libraries |
| Detection Strategy | Static analysis via *runtime-function-table* |
| Quasiquote Migration | Transform `,@` to `with-instruction-collector` |
| Batch Strategy | Group by functionality, validate after each |
| Helper Analysis | Transitive dependency graph for orphan detection |

All research areas resolved. Ready for Phase 1 design artifacts.
