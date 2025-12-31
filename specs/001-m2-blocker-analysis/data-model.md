# Data Model: Phase 13D Milestone M2 - Blocker Analysis

**Date**: 2025-12-31
**Related**: [plan.md](./plan.md), [spec.md](./spec.md)

## Overview

This document defines the data structures used for blocker analysis and Stage 1 compilation reporting.

## Entities

### Stage1Report

The main compilation report entity output by Stage 1 generation.

| Field | Type | Description |
|-------|------|-------------|
| timestamp | ISO8601 string | Report generation time |
| stage0_version | string | Version identifier for Stage 0 compiler |
| summary | Summary | Aggregated compilation statistics |
| modules | Module[] | Per-file compilation results |

### Summary

Aggregated compilation statistics.

| Field | Type | Description |
|-------|------|-------------|
| total_forms | integer | Total forms processed |
| compiled | integer | Successfully compiled forms |
| failed | integer | Forms that failed compilation |
| skipped | integer | Forms skipped (directives) |
| coverage_pct | float | Compilation rate (compiled / (total - skipped) * 100) |
| top_blockers | Blocker[] | Top 5 operators by failure count |

### Blocker

A compilation blocker - an operator that causes forms to fail.

| Field | Type | Description |
|-------|------|-------------|
| operator | string | The operator name (e.g., "DEFUN", "DEFSTRUCT") |
| count | integer | Number of forms that failed with this operator |
| priority | enum | "HIGH", "MEDIUM", "LOW" - based on count thresholds |

**Priority Rules**:
- HIGH: count >= 100
- MEDIUM: count >= 10 and count < 100
- LOW: count < 10

### Module

Per-file compilation statistics.

| Field | Type | Description |
|-------|------|-------------|
| path | string | Relative path to source file |
| compiled | integer | Successfully compiled forms in this file |
| failed | integer | Failed forms in this file |
| total | integer | Total forms in this file |

## Relationships

```
Stage1Report
├── summary: Summary (1:1)
│   └── top_blockers: Blocker[] (1:N)
└── modules: Module[] (1:N)
```

## State Transitions

### Blocker Lifecycle

```
[Unidentified] → [Detected] → [Analyzed] → [Fixing] → [Fixed] → [Verified]
```

| State | Description |
|-------|-------------|
| Unidentified | Blocker not yet found in report |
| Detected | Blocker appears in top_blockers list |
| Analyzed | Root cause determined, fix approach identified |
| Fixing | Fix implementation in progress |
| Fixed | Fix applied, awaiting verification |
| Verified | Re-run confirms blocker resolved |

### Compilation Form Lifecycle

```
[Pending] → [Parsing] → [Compiling] → [Compiled|Failed|Skipped]
```

| State | Description |
|-------|-------------|
| Pending | Form not yet processed |
| Parsing | AST parsing in progress |
| Compiling | Code generation in progress |
| Compiled | Successfully generated Wasm |
| Failed | Compilation error occurred |
| Skipped | Form intentionally skipped (directive) |

## Validation Rules

### Report Validation

1. `summary.total_forms == summary.compiled + summary.failed + summary.skipped`
2. `summary.coverage_pct == summary.compiled / (summary.total_forms - summary.skipped) * 100`
3. `sum(modules[*].total) == summary.total_forms`
4. `sum(modules[*].compiled) == summary.compiled`
5. `sum(modules[*].failed) == summary.failed`
6. `top_blockers.length <= 5`
7. `top_blockers` sorted by count descending

### Blocker Validation

1. `count > 0` (only non-zero blockers listed)
2. `priority` matches count thresholds
3. `operator` is valid Lisp special form or macro name

## Examples

### Sample Report

```json
{
  "timestamp": "2025-12-31T21:18:57Z",
  "stage0_version": "stage0-v1.0",
  "summary": {
    "total_forms": 26571,
    "compiled": 3631,
    "failed": 22482,
    "skipped": 458,
    "coverage_pct": 13.90,
    "top_blockers": [
      {"operator": "DEFUN", "count": 19084, "priority": "HIGH"},
      {"operator": "DEFSTRUCT", "count": 1953, "priority": "HIGH"}
    ]
  },
  "modules": [
    {"path": "src/clysm/backend/leb128.lisp", "compiled": 169, "failed": 1057, "total": 1238}
  ]
}
```

### Target Report (25%+ Goal)

```json
{
  "summary": {
    "total_forms": 26571,
    "compiled": 6643,
    "failed": 19470,
    "skipped": 458,
    "coverage_pct": 25.40,
    "top_blockers": [
      {"operator": "DEFUN", "count": 15000, "priority": "HIGH"}
    ]
  }
}
```
