# Research: Quasiquote Local Variable Compilation

**Date**: 2026-01-01
**Feature**: 001-quasiquote-local-vars

## Research Summary

This document consolidates findings from investigating the Clysm compiler's quasiquote handling to determine the best approach for supporting local variable references within backquote expressions.

---

## 1. Current Quasiquote Expansion Mechanism

### Decision
The existing two-phase quasiquote expansion (parse-time expansion to `list`/`append` forms + compile-time code generation) is sound and should be preserved. The issue lies in `compile-quoted-element` not handling variable forms.

### Rationale
- **Parse-time expansion** in `transform/macro.lisp:expand-backquote` (lines 264-358) correctly transforms quasiquote syntax:
  - `` `(a ,x b) `` → `(list 'a x 'b)` when `x` is a variable
  - `` `(a ,@xs b) `` → `(append (list 'a) xs (list 'b))` when `xs` needs splicing
  - Constant lists are optimized to `(quote ...)` form
- The expansion already produces compilable forms when variables are simple symbols
- **BUT**: There's a code path through `compile-quoted-list` that calls `compile-quoted-element` directly, bypassing expansion when the form looks like a quoted literal

### Key Code Locations
| File | Lines | Function | Purpose |
|------|-------|----------|---------|
| `transform/macro.lisp` | 264-358 | `expand-backquote`, `expand-bq`, `expand-bq-list` | Parse-time expansion |
| `ast.lisp` | 762-769 | `parse-compound-form` | Integration point for quasiquote expansion |
| `func-section.lisp` | 694-729 | `compile-quoted-list`, `compile-quoted-element` | Code generation (error source) |

### Alternatives Considered
1. **Runtime-only expansion**: Defer all quasiquote handling to runtime
   - Rejected: Performance cost, prevents compile-time optimization of constant parts
2. **Full macro expansion before codegen**: Expand all macros including quasiquote before any codegen
   - Rejected: Already implemented, but some paths bypass it

---

## 2. Root Cause of "Cannot compile quoted element" Error

### Decision
The error occurs in `compile-quoted-element` (func-section.lisp:729) when it encounters an AST node (from unquote) instead of a raw literal. The function must be enhanced to detect and compile AST nodes representing variable references.

### Rationale
- `compile-quoted-element` handles: nil, integers, ratios, floats, complex, characters, lists, symbols
- It does NOT handle: AST nodes like `ast-var-ref` that result from unquote expansion
- When quasiquote expansion produces `(list 'a x 'b)`, the `x` becomes an `ast-var-ref` node
- The `compile-quoted-list` function iterates over elements and calls `compile-quoted-element`
- If an element is an AST node (not a raw Lisp value), it falls through to the error case

### Evidence
```lisp
;; func-section.lisp:710-729
(defun compile-quoted-element (elem)
  (cond
    ((null elem) '((:ref.null :none)))
    ((integerp elem) ...)
    ((symbolp elem) ...)
    (t (error "Cannot compile quoted element: ~A" elem))))  ;; AST nodes hit this
```

### Alternatives Considered
1. **Pre-process quasiquote at AST level**: Transform before codegen sees it
   - Viable but requires careful handling of nesting depth
2. **Enhance compile-quoted-element**: Add cases for AST node types
   - **CHOSEN**: Most targeted fix, preserves existing constant optimization

---

## 3. List Construction Strategy for Mixed Content

### Decision
Use the existing `list`/`cons`/`append` expansion from quasiquote, ensuring the codegen compiles the expanded forms correctly.

### Rationale
The quasiquote expander already produces appropriate list construction forms:

| Pattern | Expanded Form | Codegen Strategy |
|---------|---------------|------------------|
| `` `(a ,x b) `` | `(list 'a x 'b)` | Compile each arg, call `list` builtin |
| `` `(,@xs y) `` | `(append xs (list 'y))` | Compile append with list arg |
| `` `(a . ,x) `` | `(cons 'a x)` | Compile cons with 2 args |
| `` `(a b c) `` | `'(a b c)` | Use compile-quoted-list (constant) |

The `list` and `append` functions are already implemented in the runtime and registered in `*runtime-function-table*`.

### Key Dependencies
- `list-runtime.lisp`: Runtime `list` and `append` implementations
- `*runtime-function-table*`: Function dispatch for runtime calls
- `compile-funcall`: Existing function call compilation

---

## 4. Variable Resolution via Lexical Environment

### Decision
Reuse the existing `compile-var-ref` function for compiling unquoted variable references. No new infrastructure needed.

### Rationale
- `compile-var-ref` (func-section.lisp:836-860) already handles:
  - Compile-time constants → `i32.const`
  - Local variables → `local.get idx`
  - Captured variables → Closure environment access
  - Special variables → Symbol value slot access
  - Unbound variables → Compile-time error
- When quasiquote expands `` `,x`` to just `x`, the parser creates an `ast-var-ref` node
- The codegen path for `ast-var-ref` calls `compile-var-ref` automatically

### Evidence
```lisp
;; func-section.lisp:836-860
(defun compile-var-ref (ast env)
  (let* ((name (ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    (cond
      (local-idx
       (list (list :local.get local-idx)))  ;; Emits local.get
      ...)))
```

---

## 5. Nesting Depth Handling for Nested Quasiquotes

### Decision
Track quasiquote nesting depth during expansion. Decrement on unquote, increment on nested backquote. Only evaluate when depth reaches 0.

### Rationale
- Nested quasiquotes like `` `(a `(b ,x)) `` should preserve the inner backquote as data
- The inner `,x` should NOT be evaluated at outer level (depth 1, not 0)
- Only `` `,x`` at depth 1 (after decrement from outer quasiquote) should evaluate

### Implementation Approach
```lisp
(defun expand-bq (form depth)
  (cond
    ((zerop depth) form)  ;; Base case: evaluate as-is
    ((unquote-p form)
     (if (= depth 1)
         (second form)  ;; Depth 1 → evaluate the unquoted form
         (list 'list ''unquote (expand-bq (second form) (1- depth)))))
    ((quasiquote-p form)
     (list 'list ''quasiquote (expand-bq (second form) (1+ depth))))
    ...))
```

### Existing Support
Current `expand-bq` does not track depth explicitly. Enhancement needed for FR-006.

---

## 6. Unquote-Splicing Compilation

### Decision
Use `append` for splicing. The expanded form `` `(,@xs y) `` → `(append xs (list 'y))` is already correct. Ensure `append` is properly compiled.

### Rationale
- `append` concatenates lists, exactly what splicing requires
- Empty list splicing: `(append nil '(a b))` → `(a b)` (correct behavior)
- Multiple splices: `(append xs ys zs)` handles naturally
- The `append` function exists in `list-runtime.lisp` and is registered

### Edge Cases
| Input | Expanded | Runtime Result |
|-------|----------|----------------|
| `` `(,@nil a) `` | `(append nil (list 'a))` | `(a)` |
| `` `(,@'(1 2) ,@'(3)) `` | `(append '(1 2) '(3))` | `(1 2 3)` |
| `` `(a ,@xs) `` where xs=nil | `(append (list 'a) nil)` | `(a)` |

---

## 7. Error Handling Strategy

### Decision
Emit compile-time errors for:
1. Unquote outside quasiquote context (FR-007)
2. Undefined variables in unquoted positions (FR-008)

### Rationale
- Compile-time detection prevents runtime surprises
- Error messages should include source location if available
- Existing `error` calls in codegen already provide this pattern

### Implementation Points
- Check for unquote/unquote-splicing at AST parse level before quasiquote context
- Variable lookup failure in `compile-var-ref` already throws "Unbound variable" error

---

## Conclusions

| Area | Decision | Rationale |
|------|----------|-----------|
| Expansion mechanism | Keep existing two-phase | Sound design, issue is in codegen |
| Error root cause | Fix `compile-quoted-element` | Most targeted fix |
| List construction | Use expanded list/append forms | Already implemented |
| Variable resolution | Reuse `compile-var-ref` | No new infrastructure needed |
| Nesting depth | Add depth tracking to expand-bq | Required for FR-006 |
| Splicing | Use append expansion | Already correct |
| Error handling | Compile-time detection | Better UX |

**Primary modification targets**:
1. `compile-quoted-element` in `func-section.lisp` - Add AST node handling
2. `expand-bq` in `macro.lisp` - Add depth parameter for nested quasiquotes
3. New test files for TDD validation
