# Data Model: Stage 1 Compiler Generation (Phase 13D-7)

**Date**: 2025-12-30
**Branch**: `040-stage1-compiler-gen`

## Overview

The Stage 1 compiler generation process uses a pipeline of data structures to track source code through compilation to binary output. This document describes the entities, their relationships, and state transitions.

## Entity Relationship Diagram

```
┌─────────────────┐      contains       ┌─────────────────┐
│  source-module  │──────(1:N)─────────▶│   source-form   │
│                 │                      │                 │
│  path           │                      │  id             │
│  module-index   │◀─────back-ref───────│  sexp           │
│  forms          │                      │  operator       │
│  status         │                      │  compilable-p   │
└─────────────────┘                      └────────┬────────┘
                                                  │
                                                  │ compiles to
                                                  ▼
                                         ┌─────────────────┐
                                         │ compilation-    │
                                         │    result       │
                                         │                 │
                                         │  success-p      │
                                         │  wasm-bytes     │
                                         │  error-message  │
                                         └────────┬────────┘
                                                  │
                         ┌────────────────────────┼───────────────────────┐
                         │ aggregates             │                       │
                         ▼                        ▼                       ▼
              ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
              │  module-stats   │     │ failure-group   │     │  Stage 1 Binary │
              │                 │     │                 │     │                 │
              │  compiled       │     │  operator       │     │  wasm bytes     │
              │  failed         │     │  count          │     │  100KB+         │
              │  failures       │     │  examples       │     │                 │
              └────────┬────────┘     └─────────────────┘     └─────────────────┘
                       │
                       │ summarizes
                       ▼
              ┌─────────────────┐
              │    summary      │
              │                 │
              │  total-forms    │
              │  compiled       │
              │  coverage-pct   │
              │  top-blockers   │
              └────────┬────────┘
                       │
                       │ wraps
                       ▼
              ┌─────────────────┐
              │ progress-report │
              │                 │
              │  timestamp      │
              │  modules        │
              │  summary        │
              └─────────────────┘
```

## Core Entities

### source-module

Represents a single `.lisp` file in the compiler codebase.

| Field | Type | Description |
|-------|------|-------------|
| path | string | Absolute file path |
| relative-path | string | Path relative to repository root |
| module-index | fixnum | Position in compilation order (1-N) |
| forms | list | List of source-form structs |
| dependency-group | string | Logical grouping (backend, reader, compiler) |
| status | keyword | Processing state |

**Status Values**: `:pending` → `:reading` → `:parsed` → `:compiling` → `:done`

### source-form

A single top-level S-expression from a source file.

| Field | Type | Description |
|-------|------|-------------|
| id | string | Unique identifier: `{module_index}:{form_index}` |
| sexp | any | The actual S-expression |
| operator | symbol | Head of the form (defun, defmacro, etc.) |
| name | any | Name being defined (if applicable) |
| source-text | string | Original source code for error reporting |
| module | source-module | Back-reference to containing module |
| index | fixnum | Position within module |
| compilable-p | boolean | Whether form is in blessed subset |

**Operators**: `defun`, `defmacro`, `defvar`, `defparameter`, `defstruct`, `defclass`, etc.

### compilation-result

Result of attempting to compile a single form.

| Field | Type | Description |
|-------|------|-------------|
| form | source-form | Reference to source form |
| form-id | string | Form identifier |
| success-p | boolean | Whether compilation succeeded |
| wasm-bytes | vector | Compiled output (if success) |
| error-type | symbol | Category of error (if failed) |
| error-message | string | Detailed error description |
| unsupported-feature | symbol | Specific CL feature causing failure |

### failure-group

Failures grouped by operator type for analysis.

| Field | Type | Description |
|-------|------|-------------|
| operator | symbol | The failing operator |
| count | fixnum | Number of failures |
| example | string | Representative failing form |
| examples | list | Up to 3 example forms |

### module-stats

Per-module compilation statistics.

| Field | Type | Description |
|-------|------|-------------|
| path | string | Module relative path |
| total-forms | fixnum | Number of forms in module |
| compiled | fixnum | Successfully compiled forms |
| failed | fixnum | Failed forms |
| skipped | fixnum | Non-compilable forms skipped |
| failures | list | List of failure-group structs |

### summary

Overall compilation summary.

| Field | Type | Description |
|-------|------|-------------|
| total-forms | fixnum | Forms across all modules |
| compiled | fixnum | Total successful compilations |
| failed | fixnum | Total failed compilations |
| skipped | fixnum | Total skipped forms |
| coverage-pct | float | Compilation success rate (0.0-100.0) |
| top-blockers | list | Top 5 blockers by impact |

### progress-report

Aggregated statistics for a compilation run.

| Field | Type | Description |
|-------|------|-------------|
| timestamp | string | ISO 8601 timestamp |
| stage0-version | string | Version/commit of Stage 0 used |
| modules | list | List of module-stats |
| summary | summary | Overall statistics |

### blocker-info

Information about a compilation blocker for prioritization.

| Field | Type | Description |
|-------|------|-------------|
| operator | symbol | Unsupported operator |
| affected-forms | fixnum | Forms affected by this blocker |
| impact-pct | float | Percentage of total forms |
| priority | string | HIGH, MEDIUM, LOW |
| recommendation | string | Suggested action |
| examples | list | Up to 3 example forms |

## State Transitions

### Module Processing Flow

```
                    ┌───────────┐
                    │  :pending │ Initial state
                    └─────┬─────┘
                          │ read-module
                          ▼
                    ┌───────────┐
                    │ :reading  │ File being read
                    └─────┬─────┘
                          │ parse-forms
                          ▼
                    ┌───────────┐
                    │  :parsed  │ Forms extracted
                    └─────┬─────┘
                          │ compile-all-forms
                          ▼
                    ┌───────────┐
                    │:compiling │ Compilation in progress
                    └─────┬─────┘
                          │ finish
                          ▼
                    ┌───────────┐
                    │   :done   │ Processing complete
                    └───────────┘
```

### Form Compilation Flow

```
   source-form              compilation-result
   ┌───────┐                ┌───────────┐
   │ sexp  │────compile────▶│ success-p │
   │       │                │ wasm-bytes│
   └───────┘                │ error-msg │
                            └───────────┘
                                  │
           ┌──────────────────────┼──────────────────────┐
           │ success              │ failure              │
           ▼                      ▼                      ▼
   ┌───────────────┐      ┌───────────────┐      ┌───────────────┐
   │ wasm-bytes    │      │ error-message │      │ unsupported-  │
   │ accumulated   │      │ logged        │      │ feature noted │
   └───────────────┘      └───────────────┘      └───────────────┘
```

## Validation Rules

### Form Validity

1. `form-id` must match pattern `\d+:\d+` (module:form)
2. `operator` must be a symbol
3. `compilable-p` is true only for blessed subset forms

### Statistics Invariants

1. `compiled + failed + skipped = total-forms`
2. `coverage-pct = 100 * (compiled / total-forms)`
3. Sum of module-stats must equal summary totals

### Output Constraints

1. Stage 1 binary must be >= 100KB
2. Stage 1 binary must pass `wasm-tools validate`
3. Coverage-pct must be >= 80% for success

## File Format: stage1-report.json

```json
{
  "timestamp": "2025-12-30T12:00:00Z",
  "stage0_version": "040-stage1-compiler-gen",
  "summary": {
    "total_forms": 5000,
    "compiled": 4000,
    "failed": 800,
    "skipped": 200,
    "coverage_pct": 80.0,
    "top_blockers": [
      {"operator": "FORMAT", "count": 150, "impact_pct": 3.0},
      {"operator": "DEFSTRUCT", "count": 100, "impact_pct": 2.0}
    ]
  },
  "modules": [
    {
      "path": "src/clysm/backend/leb128.lisp",
      "total_forms": 20,
      "compiled": 18,
      "failed": 2,
      "skipped": 0
    }
  ]
}
```
