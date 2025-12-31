# Data Model: Phase 13D M4 - DEFUN Blocker Analysis

**Date**: 2025-12-31
**Feature**: 001-m4-defun-blocker-analysis

## Entities

### Error Log Entry

Individual DEFUN compilation failure with full context for debugging.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `function_name` | string | Yes | Name of the DEFUN that failed (e.g., "MAKE-INSTANCE") |
| `module_path` | string | Yes | Source file path (e.g., "src/clysm/clos/mop.lisp") |
| `error_type` | enum | Yes | One of: `compile_error`, `validation_error` |
| `error_message` | string | Yes | Full error message from condition or wasm-tools |
| `pattern_id` | string | Yes | Normalized pattern identifier (e.g., "P001") |
| `lambda_list` | string | No | Original lambda-list for debugging |
| `failing_subform` | string | No | First subform that caused the error |
| `timestamp` | string | Yes | ISO 8601 timestamp of compilation attempt |

**Example**:
```json
{
  "function_name": "MAKE-INSTANCE",
  "module_path": "src/clysm/clos/instance.lisp",
  "error_type": "compile_error",
  "error_message": "Unknown function: ALLOCATE-INSTANCE",
  "pattern_id": "P003",
  "lambda_list": "(class &rest initargs &key &allow-other-keys)",
  "failing_subform": "(allocate-instance class)",
  "timestamp": "2025-12-31T12:00:00Z"
}
```

### Error Pattern Category

Classification of similar errors for batch analysis.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `pattern_id` | string | Yes | Unique identifier (e.g., "P001") |
| `pattern` | string | Yes | Normalized error pattern template |
| `count` | integer | Yes | Total occurrences of this pattern |
| `percentage` | float | Yes | Percentage of total DEFUN failures |
| `priority` | enum | Yes | One of: `HIGH`, `MEDIUM`, `LOW` |
| `examples` | array | Yes | First 3 example failures (function + module) |
| `affected_modules` | array | Yes | List of unique modules with this error |
| `suggested_fix` | string | No | Brief description of likely fix |

**Priority rules**:
- HIGH: count > 1000 OR percentage > 5%
- MEDIUM: count > 100 OR percentage > 1%
- LOW: everything else

**Example**:
```json
{
  "pattern_id": "P001",
  "pattern": "Unknown function: <NAME>",
  "count": 5432,
  "percentage": 28.6,
  "priority": "HIGH",
  "examples": [
    {"function": "MAKE-INSTANCE", "module": "src/clysm/clos/instance.lisp"},
    {"function": "SLOT-VALUE", "module": "src/clysm/clos/slot-access.lisp"},
    {"function": "FIND-CLASS", "module": "src/clysm/clos/mop.lisp"}
  ],
  "affected_modules": [
    "src/clysm/clos/instance.lisp",
    "src/clysm/clos/slot-access.lisp",
    "src/clysm/clos/mop.lisp"
  ],
  "suggested_fix": "Implement or stub the called function as a primitive"
}
```

### Compilation Report

Aggregate statistics for Stage 1 generation.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `timestamp` | string | Yes | ISO 8601 timestamp |
| `stage0_version` | string | Yes | Stage 0 version identifier |
| `summary` | object | Yes | Top-level compilation statistics |
| `modules` | array | Yes | Per-module compilation breakdown |
| `error_patterns` | array | Yes | **NEW**: Sorted list of error patterns |
| `top_blockers` | array | Yes | Operators causing most failures |

**Summary object**:
```json
{
  "total_forms": 26671,
  "compiled": 5612,
  "failed": 19653,
  "skipped": 1406,
  "coverage_pct": 22.21,
  "defun_failures": 18997,
  "defun_failure_reduction": 3997
}
```

### Lambda-List Descriptor

Parsed representation of [defun](resources/HyperSpec/Body/m_defun.htm) parameter list.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `required` | array | Yes | Required parameter names |
| `optional` | array | No | &optional parameters with defaults |
| `rest` | string | No | &rest parameter name |
| `keys` | array | No | &key parameters with defaults |
| `allow_other_keys` | boolean | Yes | Whether &allow-other-keys is present |
| `aux` | array | No | &aux parameters with init forms |

**Example**:
```json
{
  "required": ["class"],
  "optional": [],
  "rest": "initargs",
  "keys": [],
  "allow_other_keys": true,
  "aux": [
    {"name": "instance", "init": "(allocate-instance class)"}
  ]
}
```

## Relationships

```text
┌─────────────────────┐
│ Compilation Report  │
│ (1)                 │
└─────────────────────┘
         │
         │ contains
         ▼
┌─────────────────────┐     ┌─────────────────────┐
│ Error Pattern       │◄────│ Error Log Entry     │
│ (N patterns)        │     │ (M entries)         │
└─────────────────────┘     └─────────────────────┘
         │                           │
         │ classifies               │ describes
         ▼                           ▼
┌─────────────────────┐     ┌─────────────────────┐
│ Module Statistics   │     │ Lambda-List         │
│ (per source file)   │     │ Descriptor          │
└─────────────────────┘     └─────────────────────┘
```

## State Transitions

### Error Pattern Lifecycle

```text
[NEW] → [ANALYZED] → [FIXED] → [VERIFIED]
  │         │           │
  │         │           └── All examples now compile
  │         └── Root cause identified, fix implemented
  └── Pattern first detected in error log
```

### Compilation Status

```text
          ┌──────────────────┐
          │     PENDING      │
          └────────┬─────────┘
                   │ attempt compile
                   ▼
    ┌──────────────┴──────────────┐
    │                             │
    ▼                             ▼
┌───────────┐              ┌─────────────┐
│ COMPILED  │              │   FAILED    │
└───────────┘              └──────┬──────┘
                                  │
                    ┌─────────────┴─────────────┐
                    │                           │
                    ▼                           ▼
             ┌────────────┐             ┌───────────────┐
             │   SKIPPED  │             │ ERROR_LOGGED  │
             │ (intentional)            │               │
             └────────────┘             └───────────────┘
```

## Validation Rules

1. **Error Log Entry**:
   - `function_name` must be a valid Lisp symbol name
   - `module_path` must exist in the source tree
   - `pattern_id` must reference an existing pattern

2. **Error Pattern**:
   - `count` must equal the number of matching log entries
   - `percentage` must be `count / total_defun_failures * 100`
   - `examples` must contain at most 3 entries

3. **Compilation Report**:
   - `summary.compiled + summary.failed + summary.skipped == summary.total_forms`
   - `error_patterns` must be sorted by `count` descending
   - Top 10 patterns must cover ≥80% of failures (SC-005)

## Storage Format

All data is stored as JSON files in `dist/`:

| File | Purpose | Size Estimate |
|------|---------|---------------|
| `stage1-report.json` | Aggregate report with patterns | ~50KB |
| `defun-errors.json` | Detailed error log | ~5MB (18,997 entries) |

Files are overwritten on each Stage 1 generation run.
