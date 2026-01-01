# Data Model: Compiler Internal Function Consolidation

**Date**: 2026-01-01
**Branch**: `001-internal-function-consolidation`

## Overview

This feature primarily involves package exports and dead code removal. The "data model" represents the entities involved in the consolidation process rather than persistent data structures.

## Entities

### 1. InternalFunction

Represents a compiler-internal function that may need export.

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Function name (e.g., ENV-ADD-LOCAL) |
| package | package | Defining package |
| file | pathname | Source file location |
| line | integer | Line number in source |
| exported | boolean | Whether currently exported from clysm |
| action | keyword | :verify or :export-needed |

**Instances (7)**:
```lisp
(:name env-add-local :package func-section :exported t :action :verify)
(:name compile-to-instructions :package func-section :exported t :action :verify)
(:name make-wasm-struct-type* :package clysm :exported t :action :verify)
(:name compile-unary-math-ffi :package func-section :exported nil :action :export-needed)
(:name ast-literal-value :package ast :exported t :action :verify)
(:name compile-cxr-chain :package func-section :exported nil :action :export-needed)
(:name loop-keyword-eq :package macros :exported t :action :verify)
```

### 2. DeadCodeBlock

Represents a section of func-section.lisp to be removed.

| Field | Type | Description |
|-------|------|-------------|
| function-name | symbol | Name of compile-* function |
| start-line | integer | First line of function definition |
| end-line | integer | Last line of function definition |
| lines | integer | Total lines in block |
| category | keyword | :io, :list, or :sequence |
| runtime-file | pathname | Corresponding runtime library |
| runtime-function | symbol | Runtime function that replaces it |

**Categories**:
- `:io` → io-runtime.lisp
- `:list` → list-runtime.lisp
- `:sequence` → sequence-runtime.lisp

### 3. RuntimeLibrary

Represents a runtime library file containing migrated functions.

| Field | Type | Description |
|-------|------|-------------|
| file | pathname | Library file path |
| lines | integer | Total lines in file |
| functions | list | List of function names implemented |
| category | keyword | :io, :list, or :sequence |

**Instances (3)**:
```lisp
(:file "io-runtime.lisp" :lines 282 :category :io
 :functions (princ prin1 print write format terpri))

(:file "list-runtime.lisp" :lines 320 :category :list
 :functions (member member-if member-if-not
             assoc assoc-if assoc-if-not
             rassoc rassoc-if rassoc-if-not
             find find-if find-if-not
             position position-if position-if-not))

(:file "sequence-runtime.lisp" :lines 454 :category :sequence
 :functions (remove remove-if remove-if-not
             delete delete-if delete-if-not
             count count-if count-if-not
             substitute substitute-if substitute-if-not))
```

## Relationships

```
InternalFunction --[defined-in]--> Package
InternalFunction --[requires-export]--> clysm package

DeadCodeBlock --[replaced-by]--> RuntimeLibrary
DeadCodeBlock --[located-in]--> func-section.lisp

RuntimeLibrary --[replaces]--> DeadCodeBlock
```

## State Transitions

### Export Process
```
Function State: NOT-EXPORTED → EXPORT-ADDED → VERIFIED
                    ↓              ↓            ↓
Actions:        identify     add to export   test passes
```

### Dead Code Removal Process
```
Code State: IDENTIFIED → VERIFIED-DEAD → REMOVED → TESTED
                ↓            ↓             ↓         ↓
Actions:    find block   no callers    delete    tests pass
```

## Validation Rules

1. **Export Verification**: After export, function must be accessible via `(symbol-function 'clysm:function-name)`
2. **Dead Code Verification**: Before removal, `(apropos "compile-function-name")` should show only func-section.lisp definition with no callers
3. **Line Count Validation**: After all removals, `wc -l func-section.lisp` must return < 12,000

## Metrics

| Metric | Before | Target | Validation |
|--------|--------|--------|------------|
| Exported functions | 5/7 | 7/7 | Stage 1 undefined function errors = 0 |
| func-section.lisp lines | 18,351 | < 12,000 | wc -l |
| Stage 1 compilation rate | 22.15% | ≥ 25% | dist/stage1-report.json |
| Test pass rate | 100% | 100% | asdf:test-system :clysm |
