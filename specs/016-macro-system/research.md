# Research: Macro System (Lisp-4)

**Date**: 2025-12-24
**Branch**: 016-macro-system

## Executive Summary

The macro system is substantially implemented. This feature primarily involves:
1. Adding missing standard macros (`case`, `prog1`, `prog2`, `do`)
2. Enhancing lambda list destructuring (adding `&key`, `&whole`, `&environment` if needed)
3. Adding expansion depth limit detection
4. Exposing `macroexpand-1` and `macroexpand` as compilable functions

## Research Findings

### 1. Current Macro Infrastructure

**Decision**: Extend existing implementation, don't rewrite.

**Rationale**: The existing `macro.lisp` (354 lines) provides:
- Macro registry with `register-macro`, `macro-function*`
- Expansion functions: `macroexpand-1*`, `macroexpand*`, `macroexpand-all`
- Backquote expansion: `expand-backquote`, `expand-bq`, `expand-bq-list`
- Defmacro parsing: `parse-defmacro`, `parse-lambda-list`, `compile-defmacro`

**Alternatives Considered**:
- Full rewrite: Rejected - existing code is well-structured and tested
- SBCL defmacro reuse: Rejected - we need host-executable expanders

### 2. Backquote Implementation Location

**Decision**: Compiler-level (current implementation).

**Rationale**:
- Reader produces `(quasiquote ...)`, `(unquote ...)`, `(unquote-splicing ...)` forms
- Compiler's `macroexpand-all` calls `expand-backquote` when it sees `quasiquote`
- This separation allows macro bodies to contain literal quasiquote forms

**Current Flow**:
```
Reader tokenizer:  `  →  :backquote token
                   ,  →  :unquote token
                   ,@ →  :unquote-splicing token

Reader parser:     :backquote  →  (quasiquote <form>)
                   :unquote    →  (unquote <expr>)
                   :unquote-splicing → (unquote-splicing <expr>)

Compiler:          (quasiquote <form>) → expand-backquote → list/quote/append forms
```

### 3. Lambda List Features Assessment

**Current Support** (in `parse-lambda-list`):
- Required parameters: YES
- `&optional`: YES
- `&rest`: YES
- `&body`: YES (treated as &rest)

**Missing**:
- `&key`: NO - not implemented
- `&whole`: NO - not implemented
- `&environment`: NO - not implemented

**Decision**: Add `&key` support; defer `&whole` and `&environment`.

**Rationale**:
- `&key` is commonly used in macros like `defclass` options
- `&whole` and `&environment` are rarely needed and can be added later
- Standard macros in scope (when, unless, cond, case, do, dolist, dotimes, prog1, prog2) don't require them

### 4. Missing Standard Macros

**Current**: when, unless, cond, and, or, dolist, dotimes

**Missing per spec FR-009 to FR-011**:
| Macro | Required | Complexity |
|-------|----------|------------|
| `case` | YES | Medium - key matching with `eql` |
| `prog1` | YES | Simple - evaluate all, return first |
| `prog2` | YES | Simple - evaluate all, return second |
| `do` | YES | Complex - parallel binding with step forms |

**Decision**: Implement all four.

**Implementation Notes**:
- `case`: Expand to nested `if`/`eql` or optimized jump table
- `prog1`/`prog2`: Straightforward let + progn
- `do`: Most complex - need `tagbody`/`go` expansion similar to dolist/dotimes

### 5. Expansion Depth Limiting

**Current**: No limit (infinite expansion would hang).

**Decision**: Add 1000-iteration limit with clear error.

**Rationale**:
- SBCL default is ~1000 expansions
- Catches accidental infinite recursion quickly
- Error message should show macro name and depth

**Implementation Location**: Modify `macroexpand*` in macro.lisp.

### 6. Macroexpand Functions as Compiled Code

**Current**: `macroexpand-1*` and `macroexpand*` exist as host functions.

**Decision**: Add AST support for macroexpand-1 and macroexpand as special forms.

**Rationale**:
- Tier 1 interpreter needs macroexpand for REPL
- Tier 2 JIT needs them for `eval` support
- Should behave identically to Common Lisp

**Implementation**:
- Add `ast-macroexpand-1` and `ast-macroexpand` nodes
- Compile to runtime that maintains macro registry access

## Gap Analysis

| Requirement | Current Status | Action Needed |
|-------------|----------------|---------------|
| FR-001 defmacro | Partial (no &key) | Add &key support |
| FR-002 macro detection | DONE | - |
| FR-003 host execution | DONE | - |
| FR-004 recursive expansion | DONE | - |
| FR-005 backquote | DONE | Verify edge cases |
| FR-006 compile-time env | DONE | - |
| FR-007 macroexpand-1 | Host only | Add compiled form |
| FR-008 macroexpand | Host only | Add compiled form |
| FR-009 control macros | Partial | Add case |
| FR-010 sequence macros | MISSING | Add prog1, prog2 |
| FR-011 iteration macros | Partial | Add do |
| FR-012 destructuring | Partial | Add &key |
| FR-013 undefined error | DONE | - |
| FR-014 depth limit | MISSING | Add to macroexpand* |

## Test Coverage Analysis

**Existing Tests**:
- `tests/unit/macro-test.lisp`: Registry, basic expansion
- `tests/unit/backquote-test.lisp`: Quasiquote expansion
- `tests/integration/macro-test.lisp`: End-to-end compilation

**Tests Needed**:
- case macro: key matching, default clause, multiple keys
- prog1/prog2: return value correctness
- do macro: parallel binding, step forms, end test
- &key in lambda lists
- Expansion depth limit error
- Nested backquote edge cases (,',@ patterns)

## Dependencies

- Existing: `if`, `progn`, `let`, `let*`, `setq`, `block`, `tagbody`, `go`, `eql`
- All dependencies satisfied by current compiler.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| do macro complexity | Medium | Careful expansion to tagbody/go, comprehensive tests |
| Backquote edge cases | Low | Add specific test cases for nested ,',@ |
| Performance regression | Low | Benchmark macro-heavy code before/after |
