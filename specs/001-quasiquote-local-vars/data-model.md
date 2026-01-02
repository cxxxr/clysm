# Data Model: Quasiquote Local Variable Compilation

**Feature**: 001-quasiquote-local-vars
**Date**: 2026-01-01

## Overview

This document defines the AST node structures and their relationships for quasiquote compilation. The model builds on existing Clysm AST infrastructure in `src/clysm/compiler/ast.lisp`.

---

## Entity Definitions

### 1. Quasiquote AST Node (Existing)

Represented as a compound form with `QUASIQUOTE` as operator.

```text
Entity: Quasiquote Expression
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Fields:
  - operator: QUASIQUOTE symbol
  - body: Single form (the template)

Relationships:
  - Contains: Any valid Lisp form (literal, variable, list, nested quasiquote)

Lifecycle:
  1. PARSED → Reader produces (QUASIQUOTE <body>)
  2. EXPANDED → expand-backquote transforms to list/append/quote forms
  3. COMPILED → Standard codegen for expanded form
```

### 2. Unquote AST Node (Existing)

Represented as a compound form with `UNQUOTE` as operator.

```text
Entity: Unquote Expression
━━━━━━━━━━━━━━━━━━━━━━━━━━
Fields:
  - operator: UNQUOTE symbol
  - expression: Form to evaluate

Relationships:
  - Must occur within: Quasiquote context
  - Contains: Variable reference, function call, or arbitrary expression

Lifecycle:
  1. PARSED → Reader produces (UNQUOTE <expr>)
  2. EXPANDED → Returns bare expression at depth 1
  3. COMPILED → Expression compiled via normal codegen path
```

### 3. Unquote-Splicing AST Node (Existing)

Represented as a compound form with `UNQUOTE-SPLICING` as operator.

```text
Entity: Unquote-Splicing Expression
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Fields:
  - operator: UNQUOTE-SPLICING symbol
  - expression: Form evaluating to a list

Relationships:
  - Must occur within: Quasiquote list context (not as template root)
  - Contains: Variable reference or expression evaluating to list

Lifecycle:
  1. PARSED → Reader produces (UNQUOTE-SPLICING <expr>)
  2. EXPANDED → Wrapped for append: expression itself
  3. COMPILED → List value spliced into surrounding list structure
```

### 4. Variable Reference (Existing - ast-var-ref)

```text
Entity: Variable Reference
━━━━━━━━━━━━━━━━━━━━━━━━━━
Fields:
  - name: Symbol (variable name)
  - binding: Binding info (filled during analysis)
  - scope: :LOCAL | :GLOBAL | :SPECIAL | NIL

Relationships:
  - Resolved via: Lexical Environment lookup
  - Produces: local.get instruction (for locals)

Validation Rules:
  - Name must exist in lexical environment (compile-time check)
  - Scope determines codegen strategy
```

### 5. Lexical Environment (Existing)

```text
Entity: Lexical Environment
━━━━━━━━━━━━━━━━━━━━━━━━━━━
Fields:
  - bindings: List of (name . binding) pairs
  - parent: Parent environment or NIL

Operations:
  - lookup-binding(env, name) → binding or NIL
  - extend-env(env, bindings) → new child environment
  - env-lookup-local(env, name) → Wasm local index or NIL

Invariants:
  - Bindings shadow parent scope bindings
  - Local indices are unique within function scope
```

---

## Expansion State Machine

```text
┌─────────────────┐
│  QUASIQUOTE     │
│  (depth=1)      │
└────────┬────────┘
         │ expand-bq
         ▼
┌─────────────────────────────────────────────────────┐
│  Element Analysis                                    │
├─────────────────────────────────────────────────────┤
│  atom?          → (quote atom) if non-self-eval    │
│  unquote?       → bare expression (depth-1=0)      │
│  unquote-splice?→ wrap for append                  │
│  nested-qq?     → increment depth, recurse         │
│  list?          → expand-bq-list                   │
└─────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────┐
│  Output Form Selection                               │
├─────────────────────────────────────────────────────┤
│  all-constant?  → (quote <evaluated-list>)         │
│  has-splice?    → (append <parts...>)              │
│  otherwise      → (list <elements...>)              │
└─────────────────────────────────────────────────────┘
```

---

## Wasm Instruction Sequences

### Local Variable in Quasiquote

Input: `` `(foo ,x) `` where `x` is local at index 3

Expanded: `(list 'foo x)`

Generated Wasm:
```wat
;; Push 'foo symbol
(i32.const <foo-hash>)
(ref.i31)
;; Push x value
(local.get 3)         ;; FR-004: local.get for variable
;; Push nil for list terminator
(ref.null none)
;; Build list (right to left)
(struct.new $cons)    ;; (x . nil)
(struct.new $cons)    ;; (foo . (x . nil))
```

### Splicing in Quasiquote

Input: `` `(a ,@items b) `` where `items` is local at index 5

Expanded: `(append (list 'a) items (list 'b))`

Generated Wasm:
```wat
;; Build (list 'a)
(i32.const <a-hash>)
(ref.i31)
(ref.null none)
(struct.new $cons)
;; Get items
(local.get 5)
;; Build (list 'b)
(i32.const <b-hash>)
(ref.i31)
(ref.null none)
(struct.new $cons)
;; Call append with 3 args
(call $append-3)
```

---

## Validation Rules

| Rule | Scope | Error Message |
|------|-------|---------------|
| V1 | Unquote context | "Unquote outside of quasiquote context" |
| V2 | Variable binding | "Unbound variable: <name>" |
| V3 | Splice context | "Unquote-splicing not in list context" |
| V4 | Nesting sanity | (depth >= 0 invariant maintained) |

---

## Type Indices Reference

From CLAUDE.md, relevant WasmGC types:

| Index | Type | Usage in Quasiquote |
|-------|------|---------------------|
| 0 | $cons | List cells from quasiquote |
| 1 | $symbol | Quoted symbols |
| 2 | $string | Quoted strings |
