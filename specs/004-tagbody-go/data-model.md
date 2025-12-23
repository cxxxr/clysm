# Data Model: Tagbody/Go Control Flow

**Feature Branch**: `004-tagbody-go`
**Date**: 2025-12-23

## Overview

This document defines the data structures used for compiling `tagbody`/`go` control flow.

---

## Core Entities

### 1. tagbody-context

Compilation context for tagbody/go control flow. Stored in `compilation-env` during tagbody compilation.

```lisp
(defstruct tagbody-context
  "Context for compiling tagbody/go."
  (strategy nil :type keyword)
  ;; :sequential | :simple-loop | :dispatch

  (tags nil :type list)
  ;; Association list: ((tag-symbol . segment-index) ...)
  ;; Maps tag names to their segment indices for go target resolution

  (pc-local nil :type (or null fixnum))
  ;; Index of $pc local variable (dispatch strategy only)
  ;; NIL for sequential and simple-loop strategies

  (dispatch-depth nil :type (or null fixnum))
  ;; br depth from segment code to $dispatch loop (dispatch strategy only)
  ;; Computed per-segment as: num-segments - segment-index

  (loop-label nil :type (or null symbol))
  ;; Wasm label for loop target (simple-loop and dispatch strategies)
  ;; e.g., $LOOP for simple-loop, $dispatch for dispatch

  (num-segments nil :type (or null fixnum))
  ;; Total number of segments (used for depth calculation)
  )
```

**Lifecycle**:
1. Created when entering `compile-tagbody`
2. Stored in `compilation-env` for nested form compilation
3. Accessed by `compile-go` to determine jump target
4. Discarded after `compile-tagbody` completes

**Validation Rules**:
- `strategy` must be one of `:sequential`, `:simple-loop`, `:dispatch`
- `tags` must contain unique tag symbols
- `pc-local` must be set if `strategy` is `:dispatch`
- `num-segments` must be >= 1

---

### 2. segment

A portion of tagbody code starting with an optional tag. Represented as a cons cell.

```lisp
;; Type: (tag-or-nil . forms)
;; - tag-or-nil: SYMBOL or NIL (for initial segment before first tag)
;; - forms: LIST of AST nodes

;; Example:
;; (tagbody
;;    (form1)        ;; segment: (NIL . ((form1)))
;;  A (form2)        ;; segment: (A . ((form2) (form3)))
;;    (form3)
;;  B (form4))       ;; segment: (B . ((form4)))
;;
;; segments = ((NIL . (form1-ast))
;;             (A . (form2-ast form3-ast))
;;             (B . (form4-ast)))
```

**Invariants**:
- Segments are ordered as they appear in source code
- First segment may have NIL as tag (forms before first explicit tag)
- Each tag appears in exactly one segment

---

### 3. ast-tagbody (existing, reference)

AST node for tagbody form. Already defined in `ast.lisp`.

```lisp
(defstruct (ast-tagbody (:include ast-node) (:conc-name ast-tagbody-))
  "Tagbody node for goto-based control flow."
  (tags nil :type list)
  ;; List of tag symbols in order of appearance

  (segments nil :type list))
  ;; List of (tag-or-nil . forms) as described above
```

---

### 4. ast-go (existing, reference)

AST node for go form. Already defined in `ast.lisp`.

```lisp
(defstruct (ast-go (:include ast-node) (:conc-name ast-go-))
  "Go node for transfer to a tag."
  (tag nil :type symbol))
  ;; Target tag symbol
```

---

### 5. compilation-env extension

Add tagbody-context slot to existing compilation environment structure.

```lisp
;; Extension to existing defstruct:
(defstruct (compilation-env ...)
  ...existing slots...
  (tagbody-context nil :type (or null tagbody-context)))
```

**Note**: The `copy-compilation-env` function must be updated to properly copy (or preserve reference to) `tagbody-context` for nested compilation.

---

## Entity Relationships

```
┌─────────────────┐
│  compilation-env │
└────────┬────────┘
         │ contains
         ▼
┌─────────────────┐
│ tagbody-context │
└────────┬────────┘
         │ references
         ▼
┌─────────────────┐     ┌──────────┐
│     segment     │ ◄───│ ast-go   │
│ (tag . forms)   │     │ (target) │
└─────────────────┘     └──────────┘
         ▲
         │ contained in
┌─────────────────┐
│  ast-tagbody    │
└─────────────────┘
```

---

## State Transitions

### tagbody-context Strategy

```
┌────────────────────────────────────────────────────┐
│                  analyze-tagbody                    │
│                                                    │
│   ┌─────────────┐                                  │
│   │ go targets? │─── No ──► :sequential            │
│   └──────┬──────┘                                  │
│          │ Yes                                     │
│          ▼                                         │
│   ┌─────────────┐                                  │
│   │ single tag? │─── No ──► :dispatch              │
│   └──────┬──────┘                                  │
│          │ Yes                                     │
│          ▼                                         │
│   ┌──────────────────┐                             │
│   │ all backward go? │─── No ──► :dispatch         │
│   └────────┬─────────┘                             │
│            │ Yes                                   │
│            ▼                                       │
│      :simple-loop                                  │
└────────────────────────────────────────────────────┘
```

---

## Validation Rules Summary

| Entity | Rule | Error Condition |
|--------|------|-----------------|
| ast-go | Tag must exist in enclosing tagbody | Compile-time error: undefined tag |
| ast-go | Must be lexically within a tagbody | Compile-time error: go outside tagbody |
| segment | Tag symbols must be unique | Parse-time error: duplicate tag |
| tagbody-context | Strategy must match actual pattern | Internal error (should not occur) |

---

## Generated Wasm Structures

### pc Local Variable (dispatch strategy)

```wat
;; Added to function locals section
(local $pc_N i32)  ;; where N is unique index
```

### br_table Target Array

```wat
;; For N segments:
(br_table
  N       ;; segment 0 → branch depth N
  N-1     ;; segment 1 → branch depth N-1
  ...
  1       ;; segment N-1 → branch depth 1
  0       ;; default → exit (branch depth 0)
  (local.get $pc))
```
