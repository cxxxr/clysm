# Data Model: Compiler Code Generation Cleanup

**Date**: 2026-01-04
**Feature**: 001-codegen-cleanup

## Overview

This document defines the data entities involved in the dead code cleanup process. Since this is a refactoring task, the entities are conceptual analysis structures rather than persistent data.

## Entities

### Dead Function

A `compile-*` function in func-section.lisp whose functionality has been migrated to the runtime library.

| Attribute | Type | Description |
|-----------|------|-------------|
| name | symbol | Function name (e.g., `compile-string-trim`) |
| line-start | integer | Starting line number in func-section.lisp |
| line-end | integer | Ending line number |
| line-count | integer | Total lines (line-end - line-start + 1) |
| migrated-to | symbol | Runtime function name (e.g., `:$string-trim-rt`) |
| helpers | list | Helper functions called by this function |
| status | keyword | `:identified`, `:removed`, `:validated` |

### Helper Function

An internal function used to support code generation, potentially dead if only called by dead functions.

| Attribute | Type | Description |
|-----------|------|-------------|
| name | symbol | Function name |
| line-start | integer | Starting line number |
| line-end | integer | Ending line number |
| line-count | integer | Total lines |
| callers | list | Functions that call this helper |
| live-callers | list | Non-dead functions that call this helper |
| is-dead | boolean | True if no live callers exist |

### Removal Batch

A set of related dead functions removed together for validation.

| Attribute | Type | Description |
|-----------|------|-------------|
| id | integer | Batch number (1-6) |
| category | string | Functional category (e.g., "string", "numeric") |
| functions | list | List of Dead Function entities |
| helpers | list | List of Helper Function entities |
| total-lines | integer | Sum of all line-counts |
| status | keyword | `:pending`, `:in-progress`, `:validated`, `:failed` |
| validation-result | object | Test/Wasm validation results |

### Quasiquote Pattern

A code location using `,@` for instruction list building that should be migrated.

| Attribute | Type | Description |
|-----------|------|-------------|
| line-number | integer | Line in func-section.lisp |
| function-name | symbol | Containing function |
| pattern-text | string | The `,@` expression |
| is-live | boolean | Whether containing function is live (not dead) |
| migrated | boolean | Whether converted to with-instruction-collector |

### Validation Result

The outcome of running validation after a batch removal.

| Attribute | Type | Description |
|-----------|------|-------------|
| batch-id | integer | Which batch was validated |
| tests-passed | boolean | All unit tests pass |
| tests-output | string | Test runner output |
| stage1-compiled | boolean | Stage 1 generation succeeded |
| wasm-valid | boolean | wasm-tools validate exit 0 |
| line-count-before | integer | func-section.lisp lines before |
| line-count-after | integer | func-section.lisp lines after |
| timestamp | datetime | When validation ran |

## Relationships

```
Removal Batch 1───* Dead Function
                      │
                      │ calls
                      ▼
                  Helper Function
                      ▲
                      │ calls
                      │
              Live Function (not removed)

Dead Function ───migrated-to──▶ Runtime Function (in *runtime-function-table*)

Quasiquote Pattern ───contained-in──▶ Function (live or dead)
```

## State Transitions

### Dead Function Status
```
identified → removed → validated
                ↓
              failed → restored → identified
```

### Removal Batch Status
```
pending → in-progress → validated
              ↓
           failed → pending (retry)
```

## Constraints

1. A function cannot be removed if it has any live callers
2. A batch cannot be marked validated until all three checks pass (tests, stage1, wasm)
3. Quasiquote patterns in dead functions should be removed, not migrated
4. Only quasiquote patterns in live functions need migration
