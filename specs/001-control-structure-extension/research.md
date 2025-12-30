# Research: Control Structure Extensions

**Branch**: `001-control-structure-extension` | **Date**: 2025-12-30

## Summary

Research into the 45 compilation failures reveals that most infrastructure exists but has gaps or dispatch issues. The primary work is implementing `handler-case` codegen using Wasm EH proposal.

## Findings by Control Structure

### 1. `values` Special Form (18 failures)

**Decision**: Investigate and fix existing implementation

**Current State**:
- AST node: `ast-values` defined at `src/clysm/compiler/ast.lisp:462-467`
- Codegen: `compile-values` at `func-section.lisp:12930-12981`
- Infrastructure: mv-count (Global 2), mv-buffer (Global 3, type `$mv_array`)

**HyperSpec Reference**: [values](resources/HyperSpec/Body/f_values.htm)

**Implementation Analysis** (from `compile-values`):
```lisp
(zerop count)      → Return NIL, set mv-count=0
(= count 1)        → Set mv-count=1, return value directly
(count > 1)        → Primary value on stack, rest in mv-buffer
```

**Likely Failure Causes**:
1. AST dispatch issue in `compile-to-instructions` typecase
2. Missing AST node creation in `parse-compound-form`
3. Edge cases with non-tail position values propagation

**Rationale**: Implementation exists and looks correct. Failures likely stem from AST parsing not creating `ast-values` nodes for all `values` forms.

**Alternatives Considered**: None - existing implementation is sound.

---

### 2. `the` Type Declaration (18 failures)

**Decision**: Verify AST dispatch handles `the` correctly

**Current State** (`ast.lisp:825-830`):
```lisp
(the (if (>= (length args) 2)
         (parse-expr (second args))
         (error "THE requires type and form")))
```

**HyperSpec Reference**: [the](resources/HyperSpec/Body/s_the.htm)

**Analysis**:
- `the` correctly parses the inner expression, discarding type info
- Returns an `ast-*` node directly (pass-through)
- No dedicated `ast-the` node needed

**Likely Failure Causes**:
1. The `parse-compound-form` case clause may not be reached
2. Possible shadowing by earlier case clause
3. Verify `the` symbol matching in the case statement

**Rationale**: Simple pass-through is correct per Constitution (no runtime type checking for this phase).

**Alternatives Considered**:
- Runtime type checking: Rejected (out of scope per spec)
- Type-based optimization: Rejected (future enhancement)

---

### 3. `labels`/`flet` Mutual Recursion (6 failures)

**Decision**: Fix forward reference resolution in two-phase closure creation

**Current State**:
- `compile-flet` at `func-section.lisp:6418-6457`
- `compile-labels` at `func-section.lisp:6459-6580+`

**HyperSpec References**:
- [flet](resources/HyperSpec/Body/s_flet_.htm)
- [labels](resources/HyperSpec/Body/s_flet_.htm) (same page)

**Implementation Analysis**:
`compile-labels` uses two-phase approach:
1. Phase 1: Allocate locals for closures (forward references)
2. Phase 2: Compile lambdas and update closure environments

**Likely Failure Causes**:
1. Function index not pre-allocated before body compilation
2. Mutual recursion creates dependency cycle during compilation
3. Closure environment not correctly capturing forward-referenced functions

**Rationale**: Wasm requires function indices to be known at reference time. Two-phase approach is correct pattern; implementation may have edge cases.

**Alternatives Considered**:
- Single-pass compilation: Rejected (Wasm requires forward resolution)
- Indirect call via table: Considered (more complex, not needed)

---

### 4. `handler-case` Exception Handling (3 failures)

**Decision**: Implement Wasm EH codegen using try_table/catch

**Current State**:
- Macro defined at `src/clysm/conditions/handlers.lisp:19-45`
- Runtime support at `src/clysm/runtime/condition-runtime.lisp`
- **No Wasm codegen exists** - only interpreter support

**HyperSpec Reference**: [handler-case](resources/HyperSpec/Body/m_hand_1.htm)

**Current Macro Expansion**:
```lisp
(handler-case expr
  (error-type (e) body))
→
(block #:handler-case-block
  (let ((#:condition nil))
    (with-handler-cluster
        (make-handler-cluster
         :handlers (list (make-handler
                          :type 'error-type
                          :function (lambda (c)
                                      (return-from #:handler-case-block
                                        (let ((e c)) body))))))
      expr)))
```

**Problem**: This relies on runtime handler stack (`*handler-clusters*`) which requires:
- Dynamic variable binding (uses defvar)
- unwind-protect for cleanup
- Higher-order function calls
- None of these compile efficiently to Wasm

**Solution - Wasm EH Proposal**:

Per [WebAssembly Exception Handling Proposal](https://github.com/WebAssembly/exception-handling/blob/main/proposals/exception-handling/Exceptions.md):

```wat
;; handler-case compilation pattern
(try_table (result anyref)
  (catch $lisp-error $handler-block)
  ;; Protected expression
  <compile expression>
)
;; Handler block
(block $handler-block (param exnref) (result anyref)
  ;; Extract condition from exnref
  ;; Type dispatch to appropriate handler
  ;; Execute handler body
)
```

**Key Wasm EH Instructions**:
- `try_table` - establishes exception handlers for lexical scope
- `catch <tag> <label>` - catches exceptions of type `<tag>`, branches to `<label>`
- `catch_all <label>` - catches any exception
- `throw <tag>` - throws an exception
- `throw_ref` - rethrows an exception reference

**Implementation Strategy**:
1. Add `ast-handler-case` node to AST
2. Add `compile-handler-case` to codegen
3. Use single `$lisp-error` exception tag for all Lisp conditions
4. Dispatch to handlers in catch block based on condition type

**Rationale**: Wasm EH proposal is supported in wasmtime (per Constitution requirement). Direct compilation avoids runtime overhead.

**Alternatives Considered**:
- Continue using macro + runtime: Rejected (doesn't compile to Wasm)
- Setjmp/longjmp emulation: Rejected (not available in Wasm)
- CPS transformation: Rejected (complex, performance impact)

---

## Wasm EH Support Status

**Sources**:
- [WebAssembly Exception Handling Proposal](https://github.com/WebAssembly/exception-handling)
- [Wasmtime EH Support PR #11326](https://github.com/bytecodealliance/wasmtime/pull/11326)

**Status (2024-2025)**:
- Exception handling with `exnref` is Phase 4 (near standardization)
- Live in Firefox, behind flag in Chrome/Safari/Edge
- wasmtime supports `try_table` via feature flag

**Legacy vs New**:
- Legacy: `try`/`catch`/`catch_all` blocks
- New: `try_table` with `exnref` for improved exception identity

**Recommendation**: Use the new `try_table` approach as it's the forward-looking standard.

---

## Constitution Compliance

| Principle | Compliance | Implementation |
|-----------|------------|----------------|
| III. 多値バッファ | ✅ | Use existing mv-count/mv-buffer |
| IV. Wasm制御フロー活用 | ✅ | Use try_table/catch for handler-case |
| VII. TDD | ✅ | Tests will be written before implementation |
| IX. HyperSpec参照 | ✅ | Links included in this document |

---

## Implementation Priority

Based on impact (number of failures × implementation complexity):

1. **P1: `values` (18 failures)** - Debug existing, likely quick fix
2. **P1: `the` (18 failures)** - Debug existing, likely quick fix
3. **P2: `labels` mutual recursion (6 failures)** - Fix forward reference edge cases
4. **P2: `handler-case` (3 failures)** - New codegen implementation required

**Expected Effort**:
- `values` + `the`: Debugging existing code
- `labels`: Minor modifications to existing two-phase approach
- `handler-case`: New AST node + new codegen function + tests
